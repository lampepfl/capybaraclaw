// The package sits under `tacit.library` so the plugin can build `Classified`
// values with the library's own `ClassifiedImpl`, which is `private[library]`.
package tacit.library.sheets

import language.experimental.captureChecking
import caps.*

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Paths}
import scala.util.{Try, Using}

import org.apache.poi.ss.usermodel.{
  Cell,
  CellType,
  DataFormatter,
  DateUtil,
  FormulaEvaluator,
  WorkbookFactory
}

import tacit.library.{Classified, ClassifiedImpl, FileSystem}

/** Load the first sheet of an `.xlsx` file. The first row is the header; a
  * header starting with `*` marks a confidential column, and the `*` is not
  * part of the column's name.
  *
  * The file must be under a classified path: otherwise agent code could read
  * its bytes itself and decode the confidential columns. Classified files
  * refuse plain reads, so the plugin, as trusted code, reads it here, after
  * `fs` has checked that the path is within the agent's bounds (`file.path`
  * is the path it resolved, so links are not followed again).
  *
  * The `*` markers are what turns the rest of a classified file public, so a
  * sheet without any is refused rather than shown in full.
  */
@assumeSafe
def loadSheet(path: String)(using fs: FileSystem): Sheet =
  val file = fs.access(path)
  if !file.isClassified then
    throw SecurityException(
      s"$path is not under a classified path, so its confidential columns would not be protected. " +
        "Ask the operator to add it to classified_paths."
    )
  val bytes = Using.resource(
    Files.newInputStream(Paths.get(file.path), LinkOption.NOFOLLOW_LINKS)
  )(_.readAllBytes())
  if Sheet.isText(bytes) then
    throw SecurityException(
      s"$path is text, not a workbook, so it is not shown."
    )
  val sheet = Sheet.parse(bytes)
  if sheet.confidentialColumns.isEmpty then
    throw SecurityException(
      s"$path marks no column as confidential (a header starting with *), so it is not shown: it is under a classified path."
    )
  sheet

/** A table whose confidential columns never leave it in the clear: public
  * columns can be read directly, while anything computed from the table as a
  * whole comes back as `Classified`. The agent can pass those values on (e.g.
  * to a private model) but cannot see them or branch on them.
  *
  * Aggregates are `Classified` even over a public column, since their type
  * cannot depend on which column they are given; compute public statistics
  * from `column` instead.
  */
@assumeSafe
final class Sheet private[sheets] (
    /** Column names in sheet order, without the `*` marker. */
    val columns: List[String],
    confidential: Set[String],
    data: Map[String, Vector[Value]],
    /** Number of data rows (the header is not counted). */
    val rowCount: Int
):

  def confidentialColumns: List[String] = columns.filter(confidential)

  def isConfidential(column: String): Boolean = confidential(known(column))

  /** The values of a public column, as the sheet displays them. */
  def column(name: String): List[String] = public(name).map(_.render).toList

  /** How many rows hold each value of a public column. */
  def countBy(name: String): Map[String, Int] =
    public(name).groupMapReduce(_.render)(_ => 1)(_ + _)

  // Aggregates look the column up before sealing, so that a wrong column name
  // is an ordinary error naming the columns rather than a sealed failure.

  /** Sum of the numeric cells of a column; other cells are skipped. */
  def sum(column: String): Classified[Double] =
    val xs = numbers(column)
    ClassifiedImpl.wrap(xs.sum)

  /** Mean of the numeric cells of a column, `None` if there are none. */
  def avg(column: String): Classified[Option[Double]] =
    val xs = numbers(column)
    ClassifiedImpl.wrap(Option.when(xs.nonEmpty)(xs.sum / xs.size))

  /** Mean of `column` per value of `group`, e.g. average salary per department. */
  def avgBy(group: String, column: String): Classified[Map[String, Double]] =
    val keys = cells(group).map(_.render)
    val values = cells(column).map(_.number)
    ClassifiedImpl.wrap:
      keys
        .zip(values)
        .collect:
          case (key, Some(v)) => key -> v
        .groupMap(_._1)(_._2)
        .map((key, vs) => key -> vs.sum / vs.size)

  /** How many numeric cells of `column` satisfy `pred` (0 for a text column).
    * `pred` must be pure, so it cannot send the values anywhere.
    */
  def countWhere(column: String)(pred: Double -> Boolean): Classified[Int] =
    val xs = numbers(column)
    ClassifiedImpl.wrap(xs.count(pred))

  /** Every row, column name to displayed value, for computations of your own
    * via a pure `map`.
    */
  def rows: Classified[List[Map[String, String]]] =
    val rendered = renderedRows
    ClassifiedImpl.wrap(rendered.toList)

  /** The whole table as CSV, e.g. to hand to a private model. */
  def asText: Classified[String] =
    val rendered = renderedRows
    ClassifiedImpl.wrap:
      (columns.map(Sheet.csv).mkString(",") +:
        rendered.map(r => columns.map(c => Sheet.csv(r(c))).mkString(",")))
        .mkString("\n")

  override def toString: String =
    val names = columns.map(c => if confidential(c) then s"*$c" else c)
    s"Sheet($rowCount rows, columns: ${names.mkString(", ")})"

  private def known(column: String): String =
    if data.contains(column) then column
    else
      throw IllegalArgumentException(
        s"No column '$column'. Columns: ${columns.mkString(", ")}."
      )

  private def cells(column: String): Vector[Value] = data(known(column))

  private def public(column: String): Vector[Value] =
    if confidential(known(column)) then
      throw IllegalArgumentException(
        s"Column '$column' is confidential: use sum, avg, avgBy, countWhere, rows or asText, which return Classified."
      )
    else data(column)

  private def numbers(column: String): Vector[Double] =
    cells(column).flatMap(_.number)

  private def renderedRows: Vector[Map[String, String]] =
    Vector.tabulate(rowCount)(i =>
      columns.map(c => c -> data(c)(i).render).toMap
    )

end Sheet

private[sheets] object Sheet:

  /** Agent code can write under a classified path only with `writeClassified`,
    * which always writes valid UTF-8 text, while a real workbook (a ZIP or OLE2
    * file) is not valid UTF-8. Refusing text keeps agent code from building a
    * workbook out of Classified data and loading it to see its "public" columns.
    */
  def isText(bytes: Array[Byte]): Boolean =
    Try(
      StandardCharsets.UTF_8.newDecoder().decode(ByteBuffer.wrap(bytes))
    ).isSuccess

  def csv(field: String): String =
    if field.exists(",\"\r\n".contains(_)) then
      "\"" + field.replace("\"", "\"\"") + "\""
    else field

  private final case class Header(
      index: Int,
      name: String,
      confidential: Boolean
  )

  /** First sheet of an `.xlsx`/`.xls` workbook. Blank header cells drop their
    * column. Rows without a public value are skipped (or, if every column is
    * confidential, rows without any value): deciding by confidential cells
    * would reveal through `rowCount` which of them are filled.
    */
  def parse(bytes: Array[Byte]): Sheet =
    Using.resource(WorkbookFactory.create(ByteArrayInputStream(bytes))):
      workbook =>
        val sheet = workbook.getSheetAt(0)
        val evaluator = workbook.getCreationHelper.createFormulaEvaluator()
        val formatter = DataFormatter()
        val top = Option(sheet.getRow(sheet.getFirstRowNum))
          .getOrElse(throw IllegalArgumentException("The sheet is empty."))
        val headers = (0 until math.max(top.getLastCellNum.toInt, 0)).toList
          .map(i =>
            i -> Option(top.getCell(i))
              .fold("")(c => shown(c, evaluator, formatter))
              .trim
          )
          .collect:
            case (i, h) if h.nonEmpty =>
              Header(i, h.stripPrefix("*").trim, h.startsWith("*"))
        val names = headers.map(_.name)
        if names.exists(_.isEmpty) then
          throw IllegalArgumentException(
            "A header is only '*': name the column."
          )
        if names.distinct.size != names.size then
          throw IllegalArgumentException(
            s"Column '${names.diff(names.distinct).head}' appears twice."
          )
        val publicColumns =
          headers.indices.filterNot(headers(_).confidential).toList
        val decisive = if publicColumns.isEmpty then headers.indices.toList
        else publicColumns
        val rows = (sheet.getFirstRowNum + 1 to sheet.getLastRowNum).toVector
          .flatMap(r => Option(sheet.getRow(r)))
          .map(row =>
            headers
              .map(h => value(row.getCell(h.index), evaluator, formatter))
              .toVector
          )
          .filter(row => decisive.exists(row(_) != Value.Empty))
        Sheet(
          names,
          headers.filter(_.confidential).map(_.name).toSet,
          headers.zipWithIndex.map((h, j) => h.name -> rows.map(_(j))).toMap,
          rows.size
        )

  private def value(
      cell: Cell,
      evaluator: FormulaEvaluator,
      formatter: DataFormatter
  ): Value =
    Option(cell).fold(Value.Empty): c =>
      // Formulas are evaluated: files written by other tools often store no
      // cached result. If evaluation fails, the cached result is used.
      val kind =
        if c.getCellType == CellType.FORMULA then
          Try(evaluator.evaluateFormulaCell(c))
            .getOrElse(c.getCachedFormulaResultType)
        else c.getCellType
      kind match
        case CellType.NUMERIC if DateUtil.isCellDateFormatted(c) =>
          Value.Text(shown(c, evaluator, formatter))
        case CellType.NUMERIC =>
          Value.Number(c.getNumericCellValue, shown(c, evaluator, formatter))
        case CellType.STRING  => Value.ofString(c.getStringCellValue)
        case CellType.BOOLEAN => Value.Text(c.getBooleanCellValue.toString)
        case _                => Value.Empty

  /** How the sheet displays `c`. Formatting re-evaluates formulas, so if a
    * formula cannot be evaluated (a function POI lacks), its cached result is
    * formatted instead.
    */
  private def shown(
      c: Cell,
      evaluator: FormulaEvaluator,
      formatter: DataFormatter
  ): String =
    Try(formatter.formatCellValue(c, evaluator)).getOrElse:
      c.getCachedFormulaResultType match
        case CellType.NUMERIC =>
          val style = c.getCellStyle
          formatter.formatRawCellContents(
            c.getNumericCellValue,
            style.getDataFormat,
            style.getDataFormatString
          )
        case CellType.STRING => c.getStringCellValue
        case _               => ""

/** One cell; numbers keep how the sheet displays them. */
private[sheets] enum Value:
  case Text(text: String)
  case Number(value: Double, shown: String)
  case Empty

  def render: String = this match
    case Text(t)      => t
    case Number(_, s) => s
    case Empty        => ""

  def number: Option[Double] = this match
    case Number(v, _) => Some(v)
    case Text(t) => Option.when(Value.Decimal.matches(t.trim))(t.trim.toDouble)
    case Empty   => None

private[sheets] object Value:
  /** Plain decimals only: `toDouble` would also take "12f", "NaN" or "Infinity". */
  val Decimal = """[-+]?(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?""".r

  def ofString(s: String): Value = if s.trim.isEmpty then Empty else Text(s)

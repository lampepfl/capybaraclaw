package safemode.compreview

import language.experimental.captureChecking
import scala.caps.assumeSafe
import safemode.lib.{Classified, PrivacyBudget, classify}

/** Public employee record. Department / Level / Location / TenureYears are
 *  visible by policy; salary, bonus, and rating are kept in parallel sealed
 *  arrays inside [[CompReview]] so they cannot be projected to a public
 *  Employee value. */
case class Employee(
    name: String,
    dept: String,
    level: String,
    location: String,
    tenureYears: Int,
)

/** Loaded compensation table. Methods come in two flavours:
 *  - Public aggregates (headcount-by-dept, tenure buckets) that reveal only
 *    distribution shape, not pay data.
 *  - Classified aggregates (avg salary, comp ratio, outliers, band violations,
 *    pay distribution) that operate on sealed pay data and return Classified
 *    values. The agent composes them with safemode.lib primitives —
 *    `writeConfidential`, `writePrivateAnswer`, `redact`, etc. — to actually
 *    surface results. */
class CompReview private[compreview] (
    private val employees: List[Employee],
    private val salaries: List[Double],
    private val bonuses: List[Double],
    private val ratings: List[Double],
    private val budget: PrivacyBudget,
):

  /** Number of employees per department. Public — the distribution shape
    * does not reveal individual or aggregate pay. */
  def headcountByDept: Map[String, Int] =
    employees.groupMapReduce(_.dept)(_ => 1)(_ + _)

  /** Headcount per tenure bucket: 0-2y, 3-5y, 6-10y, 11+y. Public. */
  def tenureBuckets: Map[String, Int] =
    val buckets = List("0-2y", "3-5y", "6-10y", "11+y")
    val zero = buckets.map(b => b -> 0).toMap
    employees.foldLeft(zero): (acc, e) =>
      val key = e.tenureYears match
        case n if n <= 2  => "0-2y"
        case n if n <= 5  => "3-5y"
        case n if n <= 10 => "6-10y"
        case _            => "11+y"
      acc.updated(key, acc(key) + 1)

  /** Average total compensation (salary + bonus) per department. Classified —
    * raw averages are derived from sealed pay data. */
  @assumeSafe
  def avgSalaryByDept: Classified[Map[String, Double]] =
    val perDept = employees.zip(salaries.zip(bonuses)).groupBy(_._1.dept)
    val avg = perDept.map: (dept, rows) =>
      val totals = rows.map((_, sb) => sb._1 + sb._2)
      dept -> (totals.sum / totals.size)
    classify(avg)

  /** Per-employee comp ratio (total comp / band midpoint) within a specific
    * level. Useful for spotting individuals priced far from the band's
    * midpoint. Classified — values are sealed pay. */
  @assumeSafe
  def compRatio(level: String, bandMidpoint: Double): Classified[Map[String, Double]] =
    require(bandMidpoint > 0, "bandMidpoint must be positive")
    val ratios = employees.zip(salaries.zip(bonuses))
      .filter((e, _) => e.level == level)
      .map: (e, sb) =>
        e.name -> ((sb._1 + sb._2) / bandMidpoint)
      .toMap
    classify(ratios)

  /** Employees whose total comp is more than `zScore` standard deviations
    * from the company mean. Classified — leaks even the *names* of outliers
    * combined with magnitude. */
  @assumeSafe
  def outliers(zScore: Double): Classified[List[(Employee, Double)]] =
    require(zScore >= 0, "zScore must be non-negative")
    val totals = salaries.lazyZip(bonuses).map(_ + _)
    if totals.isEmpty then classify(Nil)
    else
      val mean = totals.sum / totals.size
      val variance =
        totals.map(t => (t - mean) * (t - mean)).sum / math.max(1, totals.size - 1)
      val stdev = math.sqrt(variance)
      if stdev == 0.0 then classify(Nil)
      else
        val pairs = employees.zip(totals).flatMap: (emp, total) =>
          val z = (total - mean) / stdev
          if math.abs(z) >= zScore then Some(emp -> z) else None
        classify(pairs.sortBy(p => -math.abs(p._2)))

  /** Employees whose total comp falls outside the (low, high) band declared
    * for their level. Levels missing from `bands` are skipped. Classified. */
  @assumeSafe
  def bandViolations(bands: Map[String, (Double, Double)]): Classified[List[Employee]] =
    val violations = employees.zip(salaries.zip(bonuses)).flatMap: (e, sb) =>
      bands.get(e.level).flatMap: band =>
        val (low, high) = band
        val total = sb._1 + sb._2
        if total < low || total > high then Some(e) else None
    classify(violations)

  /** Raw total-comp values across the whole company. Classified — surface
    * only through summarize / redact / writePrivateAnswer; do not project
    * out via toString or similar. */
  @assumeSafe
  def payDistribution: Classified[List[Double]] =
    classify(salaries.lazyZip(bonuses).map(_ + _))

end CompReview

object CompReview:

  /** Load a payroll spreadsheet. Defaults to `PrivacyBudget(epsilon = 1.0,
    *  sensitivity = 1.0)` — a benign starting point for the demo, since
    *  per-row sensitivity of salary aggregates is 1 USD when normalised.
    *  Schema:
    *   Name | Department | Level | Location | TenureYears | *Salary | *Bonus | *PerfRating
    * Columns whose header starts with `*` are sealed at load time.
    * Relative paths are resolved against the `safemode.workdir` JVM property. */
  @assumeSafe
  def loadCompReview(path: String): CompReview =
    loadCompReview(path, PrivacyBudget(1.0, 1.0))

  @assumeSafe
  def loadCompReview(path: String, budget: PrivacyBudget): CompReview =
    import org.apache.poi.xssf.usermodel.XSSFWorkbook
    import org.apache.poi.ss.usermodel.{Cell, CellType, Row}

    val resolved = resolvePath(path)
    val workbook = new XSSFWorkbook(new java.io.FileInputStream(resolved))
    try
      val sheet = workbook.getSheetAt(0)
      val headerRow = sheet.getRow(0)
      val headers = (0 until headerRow.getLastCellNum.toInt).map: i =>
        Option(headerRow.getCell(i)).map(_.getStringCellValue.trim).getOrElse("")
      .toList

      val cleanHeaders = headers.map(h => if h.startsWith("*") then h.drop(1) else h)
      def colIndex(name: String): Int =
        val idx = cleanHeaders.indexOf(name)
        if idx < 0 then throw IllegalArgumentException(s"Column '$name' not found in $path")
        idx

      val iName     = colIndex("Name")
      val iDept     = colIndex("Department")
      val iLevel    = colIndex("Level")
      val iLocation = colIndex("Location")
      val iTenure   = colIndex("TenureYears")
      val iSalary   = colIndex("Salary")
      val iBonus    = colIndex("Bonus")
      val iRating   = colIndex("PerfRating")

      val empsBuilder = List.newBuilder[Employee]
      val salBuilder  = List.newBuilder[Double]
      val bonBuilder  = List.newBuilder[Double]
      val ratBuilder  = List.newBuilder[Double]

      val lastRow = sheet.getLastRowNum
      var r = 1
      while r <= lastRow do
        val row = sheet.getRow(r)
        if row != null && Option(row.getCell(iName)).exists(_.getCellType != CellType.BLANK) then
          empsBuilder += Employee(
            name        = cellString(row, iName),
            dept        = cellString(row, iDept),
            level       = cellString(row, iLevel),
            location    = cellString(row, iLocation),
            tenureYears = cellDouble(row, iTenure).toInt,
          )
          salBuilder += cellDouble(row, iSalary)
          bonBuilder += cellDouble(row, iBonus)
          ratBuilder += cellDouble(row, iRating)
        r += 1

      new CompReview(
        employees = empsBuilder.result(),
        salaries  = salBuilder.result(),
        bonuses   = bonBuilder.result(),
        ratings   = ratBuilder.result(),
        budget    = budget,
      )
    finally workbook.close()

  @assumeSafe
  private def resolvePath(path: String): String =
    val f = java.io.File(path)
    if f.isAbsolute then f.getAbsolutePath
    else
      val workdir = System.getProperty("safemode.workdir", ".")
      java.io.File(workdir, path).getAbsolutePath

  @assumeSafe
  private def cellString(row: org.apache.poi.ss.usermodel.Row, idx: Int): String =
    import org.apache.poi.ss.usermodel.CellType
    Option(row.getCell(idx)) match
      case None => ""
      case Some(c) => c.getCellType match
        case CellType.STRING  => c.getStringCellValue
        case CellType.NUMERIC => c.getNumericCellValue.toString
        case CellType.BOOLEAN => c.getBooleanCellValue.toString
        case _                => ""

  @assumeSafe
  private def cellDouble(row: org.apache.poi.ss.usermodel.Row, idx: Int): Double =
    import org.apache.poi.ss.usermodel.CellType
    Option(row.getCell(idx)) match
      case None => 0.0
      case Some(c) => c.getCellType match
        case CellType.NUMERIC => c.getNumericCellValue
        case CellType.STRING  => c.getStringCellValue.trim.toDoubleOption.getOrElse(0.0)
        case _                => 0.0

end CompReview

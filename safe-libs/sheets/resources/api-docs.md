## Confidential sheets (plugin `claw.sheets`)

Read `.xlsx` spreadsheets whose sensitive columns stay hidden from you. A header
that starts with `*` (e.g. `*Salary`) marks a confidential column; its name is
`Salary`, without the `*`. The file must be under a classified path (the
operator configures these); you cannot read it directly.

```scala
def loadSheet(path: String)(using FileSystem): Sheet
```
Loads the first sheet of the file. Call it inside `requestFileSystem`; the
`Sheet` it returns can be used after the block. Sheets without any `*` column
are refused. Rows without any public value are ignored.

```scala
class Sheet:
  val columns: List[String]                  // all column names, in order
  def confidentialColumns: List[String]
  def isConfidential(column: String): Boolean  // throws on an unknown column
  val rowCount: Int

  // Public columns only (throws on a confidential one):
  def column(name: String): List[String]     // values as the sheet shows them
  def countBy(name: String): Map[String, Int]

  // Any column; results are Classified, you cannot see them:
  def sum(column: String): Classified[Double]
  def avg(column: String): Classified[Option[Double]]
  def avgBy(group: String, column: String): Classified[Map[String, Double]]
  def countWhere(column: String)(pred: Double -> Boolean): Classified[Int]
  def rows: Classified[List[Map[String, String]]]   // column -> value, every row
  def asText: Classified[String]                     // the whole table as CSV
```
`sum`, `avg`, `avgBy` and `countWhere` use only the numeric cells of `column`
(on a text column they see nothing, e.g. `countWhere` gives 0). For other
questions, compute from `rows` with a pure `map`. Statistics of a public column
are Classified too; compute them from `column(...)` if you need to see them.

Turn a `Classified` result into text with a pure `map` (no printing or I/O
inside it), then hand it to something that can deliver it to the user, such as
the `claw.private-channel` plugin if it is loaded. Printing a `Classified`
shows only `Classified(***)`.

```scala
val payroll = requestFileSystem(".", FileAccess.ReadOnly, "to analyse the payroll"):
  loadSheet("payroll.xlsx")
payroll.countBy("Department")                      // public: Map(Engineering -> 5, ...)
val avgPay = payroll.avgBy("Department", "Salary") // Classified[Map[String, Double]]
val report = avgPay.map(_.map((d, v) => f"$d: $v%.0f").mkString("\n"))
val mean = payroll.avg("Salary").map(_.fold("no salaries")(v => f"Average salary: $v%.0f"))
val engineers = payroll.rows.map(_.filter(_("Department") == "Engineering").map(_("Name")))
```

package tacit.library.sheets

import java.io.ByteArrayOutputStream

import org.apache.poi.xssf.usermodel.XSSFWorkbook

import tacit.library.{Classified, ClassifiedImpl}

class SheetSuite extends munit.FunSuite:

  /** Cells: Double/Int numbers, String text, `Formula(f)` formulas without a
    * cached result (as other tools write them), anything else blank.
    */
  private final case class Formula(f: String)

  private def xlsx(
      rows: List[List[Any]],
      dateColumn: Option[Int] = None
  ): Array[Byte] =
    val wb = XSSFWorkbook()
    val sheet = wb.createSheet("Payroll")
    val dateStyle = wb.createCellStyle()
    dateStyle.setDataFormat(
      wb.getCreationHelper.createDataFormat().getFormat("yyyy-mm-dd")
    )
    rows.zipWithIndex.foreach: (cells, r) =>
      val row = sheet.createRow(r)
      cells.zipWithIndex.foreach: (v, c) =>
        val cell = row.createCell(c)
        if r > 0 && dateColumn.contains(c) then cell.setCellStyle(dateStyle)
        v match
          case d: Double  => cell.setCellValue(d)
          case i: Int     => cell.setCellValue(i.toDouble)
          case s: String  => cell.setCellValue(s)
          case Formula(f) => cell.setCellFormula(f)
          case _          => row.removeCell(cell)
    val out = ByteArrayOutputStream()
    wb.write(out)
    wb.close()
    out.toByteArray

  private def open[T](c: Classified[T]): T = ClassifiedImpl.unwrap(c).get

  private val payroll = Sheet.parse(
    xlsx(
      List(
        List("Name", "Department", "Tenure", "*Salary"),
        List("Ann", "Eng", 3, 100000.0),
        List("Bob", "Eng", 5, 120000.0),
        List(),
        List("Cid", "Ops", 1, 80000.0),
        List("Dee", "Ops", 2, "n/a")
      )
    )
  )

  test("header * marks confidential columns and is not part of the name"):
    assertEquals(
      payroll.columns,
      List("Name", "Department", "Tenure", "Salary")
    )
    assertEquals(payroll.confidentialColumns, List("Salary"))
    assertEquals(payroll.rowCount, 4) // the empty row is skipped
    assertEquals(
      payroll.toString,
      "Sheet(4 rows, columns: Name, Department, Tenure, *Salary)"
    )

  test("public columns are readable as the sheet displays them"):
    assertEquals(payroll.column("Tenure"), List("3", "5", "1", "2"))
    assertEquals(payroll.countBy("Department"), Map("Eng" -> 2, "Ops" -> 2))

  test("confidential columns cannot be read directly"):
    val e = intercept[IllegalArgumentException](payroll.column("Salary"))
    assert(e.getMessage.contains("confidential"))
    assert(!e.getMessage.contains("100000"))
    intercept[IllegalArgumentException](payroll.countBy("Salary"))

  test("aggregates are Classified and skip non-numeric cells"):
    val total = payroll.sum("Salary")
    assertEquals(total.toString, "Classified(***)")
    assertEquals(open(total), 300000.0)
    assertEquals(open(payroll.avg("Salary")), Some(100000.0))
    assertEquals(
      open(payroll.avgBy("Department", "Salary")),
      Map("Eng" -> 110000.0, "Ops" -> 80000.0)
    )
    assertEquals(open(payroll.countWhere("Salary")(_ > 90000)), 2)
    assertEquals(open(payroll.countWhere("Name")(_ => true)), 0)
    assertEquals(
      open(payroll.rows).map(_("Salary")),
      List("100000", "120000", "80000", "n/a")
    )

  test(
    "whatever a predicate throws, fatal errors included, stays inside the Classified"
  ):
    val e = InterruptedException("secret")
    assert(
      ClassifiedImpl
        .unwrap(payroll.countWhere("Salary")(_ => throw e))
        .isFailure
    )
    assert(
      ClassifiedImpl
        .unwrap(payroll.countWhere("Salary")(_ => throw StackOverflowError()))
        .isFailure
    )

  test("only plain decimals in text cells count as numbers"):
    val s = Sheet.parse(
      xlsx(
        List(
          List("K", "*V"),
          List("a", "12f"),
          List("b", "NaN"),
          List("c", " 7.5 "),
          List("d", "1e3")
        )
      )
    )
    assertEquals(open(s.sum("V")), 1007.5)

  test(
    "formulas without a cached result are evaluated, dates are shown as dates"
  ):
    val s = Sheet.parse(
      xlsx(
        List(
          List("Plus", "Start", "*V"),
          List(Formula("100+1"), Formula("DATE(2024,1,15)"), Formula("2*3"))
        ),
        dateColumn = Some(1)
      )
    )
    assertEquals(s.column("Plus"), List("101"))
    assertEquals(s.column("Start"), List("2024-01-15"))
    assertEquals(open(s.sum("V")), 6.0)

  test("a formula POI cannot evaluate falls back to its cached result"):
    val s = Sheet.parse(
      xlsx(List(List("Os", "*V"), List(Formula("INFO(\"osversion\")"), 1)))
    )
    assertEquals(s.column("Os"), List("0"))

  test("rows are kept or skipped by their public cells only"):
    val s =
      Sheet.parse(xlsx(List(List("Name", "*V"), List("Ann", 1), List((), 2))))
    assertEquals(s.rowCount, 1)
    val allConfidential =
      Sheet.parse(xlsx(List(List("*A", "*B"), List(1, ()), List((), 2))))
    assertEquals(allConfidential.rowCount, 2)

  test("asText is CSV with quoted headers and fields"):
    val s = Sheet.parse(xlsx(List(List("Comma, col", "*V"), List("a\r\nb", 1))))
    assertEquals(open(s.asText), "\"Comma, col\",V\n\"a\r\nb\",1")
    assertEquals(
      open(payroll.asText).linesIterator.toList,
      List(
        "Name,Department,Tenure,Salary",
        "Ann,Eng,3,100000",
        "Bob,Eng,5,120000",
        "Cid,Ops,1,80000",
        "Dee,Ops,2,n/a"
      )
    )

  test("unknown columns list the existing ones"):
    val e = intercept[IllegalArgumentException](payroll.sum("Bonus"))
    assert(e.getMessage.contains("Name, Department, Tenure, Salary"))

  test(
    "workbooks are binary; text, the only thing agent code can write under a classified path, is not one"
  ):
    assert(!Sheet.isText(xlsx(List(List("Name", "*V"), List("Ann", 1)))))
    assert(
      Sheet.isText(
        "PK\u0003\u0004 anything agent code could write".getBytes("UTF-8")
      )
    )

  test("duplicate column names are rejected"):
    intercept[IllegalArgumentException](
      Sheet.parse(xlsx(List(List("A", "*A"), List(1, 2))))
    )

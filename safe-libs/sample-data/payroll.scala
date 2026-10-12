//> using scala 3.8.4
//> using dep org.apache.poi:poi-ooxml:5.4.0
//> using dep org.apache.logging.log4j:log4j-core:2.24.3

// Writes a made-up payroll for demos: `*` marks the confidential columns.
//   scala-cli run safe-libs/sample-data -- capybaraclaw/examples/default/payroll.xlsx

import java.io.FileOutputStream
import scala.util.Using

import org.apache.poi.xssf.usermodel.XSSFWorkbook

@main def payroll(out: String): Unit =
  val header = List(
    "Name",
    "Department",
    "Level",
    "Location",
    "TenureYears",
    "*Salary",
    "*Bonus"
  )
  val people = List(
    ("Alice", "Engineering", "L5", "Lausanne", 7, 168000, 21000),
    ("Bob", "Engineering", "L4", "Lausanne", 4, 131000, 12000),
    ("Carol", "Engineering", "L4", "Geneva", 3, 104000, 8000),
    ("Dave", "Engineering", "L3", "Remote", 1, 92000, 5000),
    ("Erin", "Engineering", "L3", "Geneva", 2, 96000, 6000),
    ("Frank", "Sales", "L4", "Zurich", 6, 118000, 30000),
    ("Grace", "Sales", "L3", "Zurich", 2, 84000, 15000),
    ("Heidi", "Sales", "L5", "Geneva", 9, 155000, 40000),
    ("Ivan", "Operations", "L3", "Lausanne", 5, 81000, 4000),
    ("Judy", "Operations", "L4", "Lausanne", 8, 112000, 9000),
    ("Mallory", "Operations", "L5", "Remote", 11, 149000, 14000),
    ("Niaj", "Finance", "L4", "Zurich", 3, 121000, 10000)
  )
  Using.resource(XSSFWorkbook()): wb =>
    val sheet = wb.createSheet("Payroll")
    val top = sheet.createRow(0)
    header.zipWithIndex.foreach((h, i) => top.createCell(i).setCellValue(h))
    people.zipWithIndex.foreach: (p, r) =>
      val row = sheet.createRow(r + 1)
      val (name, dept, level, location, tenure, salary, bonus) = p
      List(name, dept, level, location).zipWithIndex.foreach((v, i) =>
        row.createCell(i).setCellValue(v)
      )
      List(tenure, salary, bonus).zipWithIndex.foreach((v, i) =>
        row.createCell(i + 4).setCellValue(v.toDouble)
      )
    header.indices.foreach(sheet.autoSizeColumn)
    Using.resource(FileOutputStream(out))(wb.write)
  println(s"wrote $out")

package capybaraclaw.gateway.port.cli

import scala.util.control.NonFatal

import org.jline.reader.{LineReader, Widget}

final class Widgets(reader: LineReader):
  private val table = reader.getWidgets.nn

  def afterWidget(name: String)(after: => Unit): Unit =
    Option(table.get(name)).foreach: original =>
      val wrapped: Widget = () =>
        val result = original.apply()
        try after
        catch case NonFatal(_) => ()
        result
      val _ = table.put(name, wrapped)

package capybaraclaw.gateway.port.cli

import org.jline.reader.LineReader

object HintWidgets:

  val Triggers: List[String] = List(
    "self-insert",
    "backward-delete-char",
    "delete-char",
    "kill-whole-line",
    "kill-line",
    "kill-word",
    "backward-kill-word",
    "yank",
    "up-line-or-history",
    "down-line-or-history",
    "begin-paste"
  )

  def install(reader: LineReader, onChange: String => Unit): Unit =
    val widgets = Widgets(reader)
    Triggers.foreach: name =>
      widgets.afterWidget(name):
        onChange(reader.getBuffer.nn.toString)
    widgets.afterWidget("accept-line"):
      onChange("")

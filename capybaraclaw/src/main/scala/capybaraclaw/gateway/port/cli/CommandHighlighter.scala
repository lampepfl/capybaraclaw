package capybaraclaw.gateway.port.cli

import org.jline.reader.{Highlighter, LineReader}
import org.jline.utils.{AttributedString, AttributedStyle}

object CommandHighlighter:
  import CliCommands.CommandStatus

  val instance: Highlighter = new Highlighter:
    override def highlight(
        reader: LineReader,
        buffer: String
    ): AttributedString =
      val style = CliCommands.commandStatus(buffer) match
        case CommandStatus.Known =>
          AttributedStyle.DEFAULT.bold.foreground(AttributedStyle.GREEN)
        case CommandStatus.InProgress =>
          AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN)
        case CommandStatus.Unknown =>
          AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
        case CommandStatus.Plain => AttributedStyle.DEFAULT
      AttributedString(buffer, style)
    override def setErrorPattern(pattern: java.util.regex.Pattern): Unit = ()
    override def setErrorIndex(errorIndex: Int): Unit = ()

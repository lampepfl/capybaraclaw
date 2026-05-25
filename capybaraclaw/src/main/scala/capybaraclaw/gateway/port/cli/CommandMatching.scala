package capybaraclaw.gateway.port.cli

import org.apache.commons.text.similarity.LevenshteinDistance

object CommandMatching:

  val HintLimit: Int = 5
  val MaxHintInputLength: Int = 32

  private val Levenshtein = LevenshteinDistance.getDefaultInstance.nn

  final case class HintResult(matches: List[String], truncated: Boolean)

  def topMatches(input: String, limit: Int): HintResult =
    val needle = input.trim.toLowerCase
    if !needle.startsWith("/") then HintResult(Nil, false)
    else if needle.length > MaxHintInputLength then HintResult(Nil, false)
    else
      val all = CliCommands.All.toList
      val (prefix, others) = all.partition(_.toLowerCase.startsWith(needle))
      val candidates =
        if prefix.nonEmpty then prefix.sorted
        else
          others
            .map(cmd => cmd -> Levenshtein(needle, cmd).nn.intValue)
            .sortBy((cmd, dist) => (dist, cmd))
            .map(_._1)
      HintResult(candidates.take(limit), truncated = candidates.sizeIs > limit)

  def formatHints(result: HintResult): String =
    if result.matches.isEmpty then ""
    else
      val base = "↳ " + result.matches.mkString("   ")
      if result.truncated then base + "   …" else base

  def suggestCommand(input: String): Option[String] =
    val needle = input.toLowerCase
    if needle.isEmpty then None
    else
      val ranked = CliCommands.All.toList
        .map(cmd => cmd -> Levenshtein(needle, cmd).nn.intValue)
        .sortBy((cmd, dist) => (dist, cmd))
      val threshold = math.max(2, needle.length / 2)
      ranked.headOption.collect:
        case (cmd, dist) if dist <= threshold => cmd

  def unknownCommandText(input: String): String =
    val base = s"Unknown command: $input."
    suggestCommand(input) match
      case Some(suggestion) => s"$base Did you mean: $suggestion?"
      case None             => base

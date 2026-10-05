package capybaraclaw.gateway.sqlite

/** Cuts the fragments shown for a matched message, like `grep -C`: each match
  * with `radius` characters on each side, edges moved out to the nearest word
  * boundary within `slack` characters, overlapping fragments merged.
  */
private[sqlite] object Fragments:
  /** Markers FTS5 `highlight()` puts around matches: private-use characters,
    * so they do not occur in normal text.
    */
  val Open: Char = ''
  val Close: Char = ''

  /** The `[start, end)` ranges of the matches in `text`, read from the
    * `highlight()` output of the same text. `None` if `text` contains the
    * markers itself, so they cannot be told apart.
    */
  def ranges(text: String, highlighted: String): Option[List[(Int, Int)]] =
    if text.exists(c => c == Open || c == Close) then None
    else
      val found = List.newBuilder[(Int, Int)]
      val plain = StringBuilder()
      var start = -1
      highlighted.foreach:
        case Open  => start = plain.length
        case Close =>
          if start >= 0 then found += (start -> plain.length)
          start = -1
        case c => plain += c
      Option.when(plain.toString == text)(found.result())

  /** One string per fragment, matches marked `«like this»`, with `…` where
    * the fragment does not reach the start or end of the text.
    */
  def render(
      text: String,
      matches: List[(Int, Int)],
      radius: Int,
      slack: Int
  ): List[String] =
    val blocks = matches
      .sortBy(_._1)
      .map: (s, e) =>
        (left(text, s - radius, slack), right(text, e + radius, slack))
      .foldLeft(List.empty[(Int, Int)]):
        case ((from, to) :: rest, (s, e)) if s <= to =>
          (from, math.max(to, e)) :: rest
        case (acc, block) => block :: acc
      .reverse
    blocks.map: (from, to) =>
      val sb = StringBuilder()
      if from > 0 then sb += '…'
      var pos = from
      matches
        .filter((s, e) => s >= from && e <= to)
        .sortBy(_._1)
        .foreach: (s, e) =>
          if s >= pos then
            sb ++= text.substring(pos, s)
            sb += '«' ++= text.substring(s, e) += '»'
            pos = e
      sb ++= text.substring(pos, to)
      if to < text.length then sb += '…'
      sb.toString

  private def left(text: String, pos: Int, slack: Int): Int =
    if pos <= 0 then 0
    else
      (pos to math.max(0, pos - slack) by -1)
        .find(i => i == 0 || text(i - 1).isWhitespace)
        .getOrElse:
          if Character.isLowSurrogate(text(pos)) then pos - 1 else pos

  private def right(text: String, pos: Int, slack: Int): Int =
    if pos >= text.length then text.length
    else
      (pos to math.min(text.length, pos + slack))
        .find(i => i == text.length || text(i).isWhitespace)
        .getOrElse:
          if Character.isHighSurrogate(text(pos - 1)) then pos + 1 else pos

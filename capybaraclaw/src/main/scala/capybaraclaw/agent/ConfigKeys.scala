package capybaraclaw.agent

object ConfigKeys:
  /** Fail on a key outside `known`: a typo such as `classifed_paths` would
    * otherwise silently drop that setting. Suggests the closest known key.
    * `where` names the file (and object) in the error.
    */
  def rejectUnknown(
      keys: Iterable[String],
      known: List[String],
      where: String
  ): Unit =
    keys.toList.sorted
      .find(!known.contains(_))
      .foreach: key =>
        val closest = known.minBy(editDistance(key, _))
        val hint =
          if editDistance(key, closest) <= 2 then s"did you mean '$closest'?"
          else s"known keys: ${known.mkString(", ")}"
        throw ConfigError(s"$where: unknown key '$key', $hint")

  private def editDistance(a: String, b: String): Int =
    var prev = Array.range(0, b.length + 1)
    for i <- 1 to a.length do
      val curr = Array.ofDim[Int](b.length + 1)
      curr(0) = i
      for j <- 1 to b.length do
        val cost = if a(i - 1) == b(j - 1) then 0 else 1
        curr(j) =
          math.min(math.min(curr(j - 1), prev(j)) + 1, prev(j - 1) + cost)
      prev = curr
    prev(b.length)

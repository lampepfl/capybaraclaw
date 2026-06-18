package capybaraclaw.gateway.sqlite

import capybaraclaw.gateway.SearchTerms

/** Compiles structured [[SearchTerms]] into a valid FTS5 MATCH expression. */
private[sqlite] object FtsQuery:

  private def quoteLiteral(term: String): String =
    "\"" + term.replace("\"", "\"\"") + "\""

  private def clean(terms: List[String]): List[String] =
    terms.map(_.trim).filter(t => t.nonEmpty && t.exists(_.isLetterOrDigit))

  def compile(terms: SearchTerms): Option[String] =
    def positive(term: String): String =
      if terms.prefix then quoteLiteral(term) + "*" else quoteLiteral(term)

    val all = clean(terms.allOf).map(positive)
    val any = clean(terms.anyOf).map(positive)
    val none = clean(terms.noneOf).map(quoteLiteral)

    val orGroup =
      if any.isEmpty then Nil else List("(" + any.mkString(" OR ") + ")")
    val components = all ++ orGroup

    if components.isEmpty then None
    else
      val positiveExpr = components.mkString(" AND ")
      if none.isEmpty then Some(positiveExpr)
      else
        val base =
          if components.sizeIs > 1 then "(" + positiveExpr + ")"
          else positiveExpr
        Some(none.foldLeft(base)((acc, term) => acc + " NOT " + term))

package capybaraclaw

object Throwables:
  def errorMessage(e: Throwable): String =
    Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)

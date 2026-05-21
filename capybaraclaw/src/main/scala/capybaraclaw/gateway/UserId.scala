package capybaraclaw.gateway

opaque type UserId <: String = String

object UserId:
  def apply(value: String): UserId =
    require(value.nonEmpty, "user id must not be empty")
    value

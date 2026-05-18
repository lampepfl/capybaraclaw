package capybaraclaw.gateway

opaque type PortId <: String = String

object PortId:
  def apply(value: String): PortId =
    require(value.nonEmpty, "port id must not be empty")
    value

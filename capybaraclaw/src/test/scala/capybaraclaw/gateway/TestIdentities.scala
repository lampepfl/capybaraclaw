package capybaraclaw.gateway

object TestIdentities:
  /** Everyone may be asked for anything, as before roles existed. */
  val everyoneMayAll: Identities = Identities.parse(
    """{"roles": {"all": {"may": ["*"]}}, "default_roles": ["all"]}""",
    "test identities"
  )

  def broker(identities: Identities = everyoneMayAll): ApprovalBroker =
    ApprovalBroker(() => identities)

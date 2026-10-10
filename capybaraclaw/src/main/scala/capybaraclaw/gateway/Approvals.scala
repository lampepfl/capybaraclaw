package capybaraclaw.gateway

import java.nio.file.Paths

/** What agent code asks for beyond its session's defaults. */
enum Permission:
  /** Files under a canonical directory. */
  case Files(root: String, access: Permission.FileAccess)

  /** Running these commands, with any arguments. */
  case Commands(names: Set[String])

  /** Connecting to these hosts. */
  case Hosts(names: Set[String], access: Permission.NetworkAccess)

  /** A permission a plugin defines, e.g. `"read payroll"` for some files. */
  case Plugin(plugin: String, permission: String, items: Set[String])

  /** For people and the agent; the path and names are JSON-quoted so
    * whatever they contain reads as data.
    */
  def describe: String = this match
    case Files(root, Permission.FileAccess.Read) =>
      s"read access to files under ${Permission.quote(root)}"
    case Files(root, Permission.FileAccess.ReadWrite) =>
      s"read and write access to files under ${Permission.quote(root)}"
    case Commands(names) =>
      s"running ${Permission.quoteAll(names)} with any arguments"
    case Hosts(hosts, Permission.NetworkAccess.Fetch) =>
      s"fetching from ${Permission.quoteAll(hosts)} (GET and HEAD only)"
    case Hosts(hosts, Permission.NetworkAccess.Send) =>
      s"sending data to ${Permission.quoteAll(hosts)}"
    case Plugin(plugin, permission, items) =>
      val forItems =
        if items.isEmpty then "" else s" for ${Permission.quoteAll(items)}"
      s"plugin ${Permission.quote(plugin)} permission ${Permission.quote(permission)}$forItems"

  def question(showPath: String => String = identity): String = this match
    case Files(root, Permission.FileAccess.Read) =>
      s"Allow capybara to read ${Permission.quote(showPath(root))}?"
    case Files(root, Permission.FileAccess.ReadWrite) =>
      s"Allow capybara to read and write ${Permission.quote(showPath(root))}?"
    case Commands(names) =>
      s"Allow capybara to run ${Permission.quoteAll(names)} with any arguments? " +
        "Commands run outside the sandbox, as you: they can read and change " +
        "any of your files and see the gateway's environment, API keys included."
    case Hosts(hosts, Permission.NetworkAccess.Fetch) =>
      s"Allow capybara to fetch from ${Permission.quoteAll(hosts)} (GET and HEAD only)?"
    case Hosts(hosts, Permission.NetworkAccess.Send) =>
      s"Allow capybara to send data to ${Permission.quoteAll(hosts)}?"
    case Plugin(plugin, name, items) =>
      val forItems =
        if items.isEmpty then "" else s" for ${Permission.quoteAll(items)}"
      s"Allow plugin ${Permission.quote(plugin)} to ${Permission.quote(name)}$forItems?"

object Permission:
  /** `ReadWrite` covers `Read`. */
  enum FileAccess:
    case Read, ReadWrite

  /** `Send` (any request) covers `Fetch` (GET and HEAD without a body). */
  enum NetworkAccess:
    case Fetch, Send

  def quote(text: String): String = ujson.Str(text).render()

  def quoteAll(texts: Set[String]): String =
    texts.toList.sorted.map(quote).mkString(", ")

/** `requester` is the [[Person.id]] whose turn asked; only they may answer.
  * `account` is the `"<port>:<user>"` they asked from. `reason` is the requester's own explanation (the agent's, or for
  * [[Permission.Plugin]] the plugin's), shown to the user as such.
  */
final case class ApprovalRequest(
    id: Int,
    sessionId: SessionId,
    requester: String,
    permission: Permission,
    reason: String = "",
    account: String = ""
):
  def reasonLine: Option[String] =
    val who = permission match
      case Permission.Plugin(_, _, _) => "the plugin's"
      case _                          => "capybara's"
    Option.when(reason.nonEmpty)(s"$who reason: ${Permission.quote(reason)}")

enum ApprovalDecision:
  case Approve, Deny

/** Whose grants: a person's, in one session. What Alice is granted in a shared
  * thread does not cover Bob's turns in it.
  */
final case class Grantee(sessionId: SessionId, person: String)

/** Pending requests and granted permissions of all sessions. Grants last until
  * the gateway stops.
  */
final case class Approvals(
    nextId: Int,
    pending: Map[Int, ApprovalRequest],
    grants: Map[Grantee, Set[Permission]]
):
  /** The part of `permission` the grantee has not been granted yet, if any. */
  def ungranted(
      grantee: Grantee,
      permission: Permission
  ): Option[Permission] =
    val granted = grants.getOrElse(grantee, Set.empty)
    permission match
      case Permission.Files(root, access) =>
        val covered = granted.exists:
          case Permission.Files(grantedRoot, grantedAccess) =>
            Paths.get(root).startsWith(Paths.get(grantedRoot)) &&
            (grantedAccess == Permission.FileAccess.ReadWrite ||
              access == Permission.FileAccess.Read)
          case _ => false
        Option.unless(covered)(permission)
      case Permission.Commands(names) =>
        val missing = names -- granted.flatMap:
          case Permission.Commands(grantedNames) => grantedNames
          case _                                 => Set.empty
        Option.when(missing.nonEmpty)(Permission.Commands(missing))
      case Permission.Hosts(hosts, access) =>
        val missing = hosts.filterNot: host =>
          granted.exists:
            case Permission.Hosts(grantedHosts, grantedAccess) =>
              grantedHosts.contains(host) &&
              (grantedAccess == Permission.NetworkAccess.Send ||
                access == Permission.NetworkAccess.Fetch)
            case _ => false
        Option.when(missing.nonEmpty)(Permission.Hosts(missing, access))
      case Permission.Plugin(plugin, name, items) =>
        val grantedItems = granted.collect:
          case Permission.Plugin(`plugin`, `name`, grantedItems) => grantedItems
        // No items asks for the permission as a whole, so only a grant given
        // as a whole covers it.
        val missing = items -- grantedItems.flatten
        val covered =
          if items.isEmpty then grantedItems.exists(_.isEmpty)
          else missing.isEmpty
        Option.unless(covered)(
          Permission.Plugin(
            plugin,
            name,
            if items.isEmpty then items else missing
          )
        )

  /** Reuses the grantee's pending request for the same permission, so an
    * agent that retries before the user answers does not pile up requests.
    * The flag tells whether the request is new, i.e. the user has not been
    * shown it yet.
    */
  def request(
      grantee: Grantee,
      permission: Permission,
      reason: String = "",
      account: String = ""
  ): (Approvals, ApprovalRequest, Boolean) =
    pending.values.find(r =>
      r.sessionId == grantee.sessionId && r.requester == grantee.person &&
        r.permission == permission
    ) match
      case Some(existing) => (this, existing, false)
      case None           =>
        val request = ApprovalRequest(
          nextId,
          grantee.sessionId,
          grantee.person,
          permission,
          reason,
          account
        )
        (
          copy(
            nextId = nextId + 1,
            pending = pending + (request.id -> request)
          ),
          request,
          true
        )

  /** Drops a pending request without a decision, e.g. one the user could not
    * be shown.
    */
  def withdraw(requestId: Int): Approvals =
    copy(pending = pending - requestId)

  /** The pending request `grantee` may answer: `requestId = None` is their
    * most recent one in the session. Only the requester answers a request,
    * and only in its own session.
    */
  def answerable(
      grantee: Grantee,
      requestId: Option[Int]
  ): Either[String, ApprovalRequest] =
    requestId match
      case Some(id) =>
        pending
          .get(id)
          .filter(_.sessionId == grantee.sessionId)
          .toRight(s"No pending permission request #$id in this session.")
          .filterOrElse(
            _.requester == grantee.person,
            s"Permission request #$id is not yours to answer."
          )
      case None =>
        pending.values
          .filter(r =>
            r.sessionId == grantee.sessionId && r.requester == grantee.person
          )
          .maxByOption(_.id)
          .toRight("You have no pending permission requests in this session.")

  /** Closes a pending request; approving grants it to its requester in its
    * session.
    */
  def resolve(request: ApprovalRequest, decision: ApprovalDecision): Approvals =
    val grantee = Grantee(request.sessionId, request.requester)
    val updatedGrants = decision match
      case ApprovalDecision.Approve =>
        grants.updated(
          grantee,
          grants.getOrElse(grantee, Set.empty) + request.permission
        )
      case ApprovalDecision.Deny => grants
    copy(pending = pending - request.id, grants = updatedGrants)

object Approvals:
  val empty: Approvals =
    Approvals(nextId = 1, pending = Map.empty, grants = Map.empty)

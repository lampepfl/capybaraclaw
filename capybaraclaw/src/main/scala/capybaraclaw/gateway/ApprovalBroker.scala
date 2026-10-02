package capybaraclaw.gateway

import capybaraclaw.gateway.port.Port

import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicReference

import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.util.Try
import scala.util.control.NonFatal

/** Answers TACIT's permission oracle for every session and records the users'
  * decisions. Agent code is never kept waiting: a request outside the granted
  * roots is denied right away with a message naming the pending request, the
  * user answers it on their port, and the agent retries once told.
  *
  * Called from REPL threads (the oracle) and from the gateway (turns and
  * replies), so the state is swapped atomically.
  */
final class ApprovalBroker:
  import ApprovalBroker.*

  private val logger = LoggerFactory.getLogger(classOf[ApprovalBroker])
  private val state = AtomicReference(BrokerState(Approvals.empty, Map.empty))

  /** Where the session's current turn came from: requests made by agent code
    * during the turn are shown on this port.
    */
  def beginTurn(sessionId: SessionId, port: Port, origin: Origin): Unit =
    update(s =>
      (s.copy(turns = s.turns.updated(sessionId, Turn(port, origin))), ())
    )

  def endTurn(sessionId: SessionId): Unit =
    update(s => (s.copy(turns = s.turns - sessionId), ()))

  def oracle(sessionId: SessionId): String => String =
    requestJson => decide(sessionId, requestJson)

  def resolve(
      sessionId: SessionId,
      requestId: Option[Int],
      decision: ApprovalDecision
  ): Either[String, ApprovalRequest] =
    update: s =>
      s.approvals.resolve(sessionId, requestId, decision) match
        case Right((approvals, request)) =>
          (s.copy(approvals = approvals), Right(request))
        case Left(error) => (s, Left(error))

  private def decide(sessionId: SessionId, requestJson: String): String =
    val outcome = parseRequest(requestJson) match
      case Left(rejection)             => Outcome.Rejected(rejection)
      case Right((permission, reason)) =>
        update(s => askOrAllow(s, sessionId, permission, reason))
    outcome match
      case Outcome.Allowed                   => allow
      case Outcome.Rejected(message)         => deny(message)
      case Outcome.Asked(request, newPrompt) =>
        val asked = newPrompt.forall: turn =>
          shown(request):
            turn.port.requestApproval(sessionId, turn.origin, request)
        if asked then
          deny(
            s"${request.permission.describe.capitalize} needs the user's approval (permission request #${request.id}). " +
              "The user has been asked. Stop and tell them you are waiting; " +
              "you will get a message once they decide."
          )
        else
          update(s => (s.copy(approvals = s.approvals.cancel(request.id)), ()))
          deny(
            s"Denied ${request.permission.describe}: the user could not be asked."
          )

  private def shown(request: ApprovalRequest)(show: => Unit): Boolean =
    try
      show
      true
    catch
      case NonFatal(e) =>
        logger.warn(s"could not show permission request #${request.id}", e)
        false

  private def update[A](f: BrokerState => (BrokerState, A)): A =
    @tailrec
    def loop(): A =
      val current = state.get()
      val (next, result) = f(current)
      if state.compareAndSet(current, next) then result else loop()
    loop()

object ApprovalBroker:
  private final case class Turn(port: Port, origin: Origin)

  private final case class BrokerState(
      approvals: Approvals,
      turns: Map[SessionId, Turn]
  )

  private enum Outcome:
    case Allowed
    case Rejected(message: String)

    /** `newPrompt` is the turn to show the request on, unless the user has
      * already been shown it.
      */
    case Asked(request: ApprovalRequest, newPrompt: Option[Turn])

  /** Grant check, channel lookup and request creation as one state change, so
    * an approval landing in between cannot be missed.
    */
  private def askOrAllow(
      s: BrokerState,
      sessionId: SessionId,
      permission: Permission,
      reason: String
  ): (BrokerState, Outcome) =
    s.approvals.ungranted(sessionId, permission) match
      case None          => (s, Outcome.Allowed)
      case Some(missing) =>
        s.turns.get(sessionId) match
          case None =>
            (
              s,
              Outcome.Rejected(
                s"Denied ${missing.describe}: it needs the user's approval and the user cannot be asked outside a turn."
              )
            )
          case Some(turn) if !turn.port.supportsApprovals =>
            (
              s,
              Outcome.Rejected(
                s"Denied ${missing.describe}: it needs the user's approval and this channel cannot ask the user."
              )
            )
          case Some(turn) =>
            val (approvals, request, isNew) =
              s.approvals.request(sessionId, missing, reason)
            (
              s.copy(approvals = approvals),
              Outcome.Asked(request, Option.when(isNew)(turn))
            )

  private val allow: String = ujson.write(ujson.Obj("allow" -> true))

  private def deny(message: String): String =
    ujson.write(ujson.Obj("allow" -> false, "message" -> message))

  private val unsupported = "Access denied: unsupported permission request."

  /** The permission asked for by TACIT's `{"kind": "filesystem", "root",
    * "resolved", "access"?}`, `{"kind": "exec", "items"}`, `{"kind":
    * "network", "items", "access"?}` or a plugin's `{"kind": "plugin",
    * "plugin", "permission", "items"?}` (all with an optional "reason"), or why
    * it is rejected without asking the user.
    */
  private def parseRequest(
      json: String
  ): Either[String, (Permission, String)] =
    Try(ujson.read(json).obj).toOption
      .toRight(unsupported)
      .flatMap: obj =>
        def text(key: String) = obj.get(key).flatMap(_.strOpt)
        def items = obj
          .get("items")
          .flatMap(v => Try(v.arr.map(_.str).toSet).toOption)
          .filter(_.nonEmpty)
        val reason = text("reason").getOrElse("").trim
        val permission = text("kind") match
          case Some("filesystem") =>
            (text("root"), text("resolved")) match
              case (Some(root), _) if !isAbsolute(root) =>
                Left(
                  s"Access to ${Permission.quote(root)} denied: request an absolute path. Relative paths are not resolved against the working directory."
                )
              case (Some(_), Some(resolved)) =>
                val access = text("access") match
                  case Some("read") => Permission.FileAccess.Read
                  case _            => Permission.FileAccess.ReadWrite
                Right(Permission.Files(resolved, access))
              case _ => Left(unsupported)
          case Some("exec") =>
            items.map(Permission.Commands(_)).toRight(unsupported)
          case Some("network") =>
            val access = text("access") match
              case Some("fetch") => Permission.NetworkAccess.Fetch
              case _             => Permission.NetworkAccess.Send
            items.map(Permission.Hosts(_, access)).toRight(unsupported)
          case Some("plugin") =>
            val pluginItems = obj.get("items") match
              case None        => Some(Set.empty[String])
              case Some(value) => Try(value.arr.map(_.str).toSet).toOption
            (text("plugin"), text("permission"), pluginItems) match
              case (Some(plugin), Some(permission), Some(items)) =>
                Right(Permission.Plugin(plugin, permission, items))
              case _ => Left(unsupported)
          case _ => Left(unsupported)
        permission.map((_, reason))
      .filterOrElse(
        (permission, reason) =>
          !(texts(permission) + reason).exists(hasNonPrintable),
        "Access denied: the request contains non-printable characters."
      )
      .filterOrElse(
        (permission, _) => !texts(permission).exists(_.isBlank),
        "Access denied: the request contains an empty name."
      )
      .filterOrElse(
        (permission, _) => exactNames(permission),
        "Access denied: wildcards are not allowed; request exact command names and host names."
      )
      .filterOrElse(
        (permission, _) => permission.question().length <= MaxQuestionLength,
        "Access denied: the request names too much to show the user at once; split it into smaller requests."
      )
      .filterOrElse(
        (_, reason) => reason.length <= MaxReasonLength,
        s"Access denied: keep the reason to at most $MaxReasonLength characters."
      )

  private val MaxReasonLength = 200

  private val MaxQuestionLength = 2900

  /** `*` is a wildcard in TACIT's host matching, so it would grant far more
    * than the prompt shows; command names are exact executables. `?` is
    * rejected too, as a precaution.
    */
  private def exactNames(permission: Permission): Boolean = permission match
    case Permission.Files(_, _)     => true
    case Permission.Commands(names) => !names.exists(_.exists("*?".contains(_)))
    case Permission.Hosts(hosts, _) => !hosts.exists(_.exists("*?".contains(_)))
    case Permission.Plugin(_, _, _) => true

  private def texts(permission: Permission): Set[String] = permission match
    case Permission.Files(root, _)                    => Set(root)
    case Permission.Commands(names)                   => names
    case Permission.Hosts(hosts, _)                   => hosts
    case Permission.Plugin(plugin, permission, items) =>
      items + plugin + permission

  /** Control, invisible formatting (e.g. bidi overrides, zero-width) and line
    * separator characters could make what is shown to the user differ from
    * what is granted.
    */
  private def hasNonPrintable(text: String): Boolean =
    text.codePoints.anyMatch: c =>
      Character.getType(c) match
        case Character.CONTROL | Character.FORMAT | Character.LINE_SEPARATOR |
            Character.PARAGRAPH_SEPARATOR =>
          true
        case _ => false

  private def isAbsolute(path: String): Boolean =
    Try(Paths.get(path).isAbsolute).getOrElse(false)

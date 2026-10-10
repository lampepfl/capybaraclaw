package capybaraclaw.gateway

import java.io.File
import java.nio.charset.StandardCharsets
import java.io.IOException
import java.nio.file.{Files, NoSuchFileException}
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.util.concurrent.atomic.AtomicReference

import org.slf4j.LoggerFactory

import scala.util.control.NonFatal

import capybaraclaw.agent.{ConfigError, ConfigKeys}

/** A human, possibly reachable through several ports. `id` is the name from
  * `identities.json`, or `"<port>:<user>"` for someone not listed there.
  * `operator` is set for the gateway's own terminal user, whom no role limits.
  */
final case class Person(
    id: String,
    roles: Set[String],
    operator: Boolean = false
)

/** Who is who across ports and what each role may approve, from the
  * operator-written `identities.json`:
  *
  * {{{
  * {
  *   "roles": {
  *     "admin": { "may": ["*"] },
  *     "dev": { "may": ["network:fetch", { "permission": "exec", "items": ["git"] }] }
  *   },
  *   "default_roles": [],
  *   "people": { "lukasz": { "ids": ["cli:lbialy", "slack:U0123"], "roles": ["admin"] } }
  * }
  * }}}
  *
  * A person may only be asked for permissions their roles allow; anything
  * else is denied without asking. People not listed, and listed people
  * without `roles`, get `default_roles`. Linking ids and giving roles only
  * ever comes from this file, never from the agent or from what a user claims
  * in a conversation.
  */
final class Identities private (
    byId: Map[String, Person],
    roles: Map[String, List[RoleRule]],
    defaultRoles: Set[String],
    operator: Option[String]
):
  def person(port: PortId, user: UserId): Person =
    val id = s"$port:$user"
    val p = byId.getOrElse(id, Person(id, defaultRoles))
    if operator.contains(id) then p.copy(operator = true) else p

  /** The part of `permission` that `person`'s roles do not allow, if any. */
  def disallowed(person: Person, permission: Permission): Option[Permission] =
    if person.operator then None
    else
      RoleRule.disallowedByAll(
        person.roles.toList.sorted.flatMap(roles.getOrElse(_, Nil)),
        permission
      )

  /** `id` (`"<port>:<user>"`) is the gateway's terminal user: always allowed,
    * whatever the file says.
    */
  def withOperator(id: String): Identities =
    Identities(byId, roles, defaultRoles, Some(id))

object Identities:
  val empty: Identities = Identities(Map.empty, Map.empty, Set.empty, None)

  private val TopKeys = List("roles", "default_roles", "people")
  private val RoleKeys = List("may")
  private val RuleKeys = List("permission", "items")
  private val PersonKeys = List("ids", "roles")

  def defaultFile: File =
    File(System.getProperty("user.home"), ".claw/identities.json")

  /** Nobody linked if `file` does not exist. Raises [[ConfigError]] naming the
    * file and the offending entry.
    */
  def load(file: File): Identities =
    if !file.exists() then empty
    else parse(Files.readString(file.toPath), file.getPath)

  def parse(raw: String, path: String): Identities =
    val obj =
      try ujson.read(raw).obj
      catch
        case NonFatal(e) =>
          throw ConfigError(
            s"$path is not a valid JSON object: ${e.getMessage}"
          )
    ConfigKeys.rejectUnknown(obj.keys, TopKeys, path)

    def objectAt(value: Option[ujson.Value], where: String) =
      value match
        case None    => Map.empty[String, ujson.Value]
        case Some(v) =>
          v.objOpt
            .map(_.toMap)
            .getOrElse(throw ConfigError(s"$where must be an object"))
    def strings(value: ujson.Value, where: String): List[String] =
      value.arrOpt
        .map(
          _.toList.map(
            _.strOpt
              .getOrElse(throw ConfigError(s"$where must hold only strings"))
          )
        )
        .getOrElse(throw ConfigError(s"$where must be an array of strings"))

    val roles = objectAt(obj.get("roles"), s"$path: 'roles'").map:
      (name, value) =>
        val where = s"$path: role '$name'"
        if name.isBlank then throw ConfigError(s"$path: a role name is blank")
        val fields = objectAt(Some(value), where)
        ConfigKeys.rejectUnknown(fields.keys, RoleKeys, where)
        val may = fields
          .get("may")
          .flatMap(_.arrOpt)
          .getOrElse(throw ConfigError(s"$where: 'may' must be an array"))
        name -> may.toList.map(rule(_, where))

    def roleNames(value: ujson.Value, where: String): Set[String] =
      val names = strings(value, where).toSet
      (names -- roles.keySet).toList.sorted.headOption.foreach: unknown =>
        throw ConfigError(s"$where: unknown role '$unknown'")
      names

    val defaultRoles = obj
      .get("default_roles")
      .map(roleNames(_, s"$path: 'default_roles'"))
      .getOrElse(Set.empty)

    val entries = objectAt(obj.get("people"), s"$path: 'people'").toList
      .flatMap: (name, value) =>
        val where = s"$path: person '$name'"
        if name.isBlank then throw ConfigError(s"$path: a person name is blank")
        // Ids of people not listed are "<port>:<user>"; a name with ':' could
        // take one, and with it that person's grants and private memory.
        if name.contains(':') then
          throw ConfigError(
            s"$where: a person name must not contain ':', which only ids of people not listed have"
          )
        val fields = objectAt(Some(value), where)
        ConfigKeys.rejectUnknown(fields.keys, PersonKeys, where)
        val personRoles = fields
          .get("roles")
          .map(roleNames(_, s"$where: 'roles'"))
          .getOrElse(defaultRoles)
        val ids = fields.get("ids").map(_.arrOpt) match
          case Some(Some(arr)) if arr.nonEmpty =>
            arr.toList.map: id =>
              id.strOpt
                .filter(validId)
                .getOrElse(
                  throw ConfigError(
                    s"$where: ids must look like \"<port>:<user>\", got ${id.render()}"
                  )
                )
          case _ =>
            throw ConfigError(s"$where: 'ids' must be a non-empty array")
        ids.map(_ -> Person(name, personRoles))
    entries
      .groupBy(_._1)
      .collectFirst:
        case (id, owners) if owners.map(_._2.id).distinct.size > 1 =>
          id -> owners.map(_._2.id).distinct.sorted
      .foreach: (id, owners) =>
        throw ConfigError(
          s"$path: id '$id' is listed for several people: ${owners.mkString(", ")}"
        )
    Identities(entries.toMap, roles, defaultRoles, None)

  /** A pattern, or `{"permission": pattern, "items": [...]}`. */
  private def rule(value: ujson.Value, where: String): RoleRule =
    value match
      case ujson.Str(text)   => RoleRule(PermissionPattern.parse(text, where))
      case ujson.Obj(fields) =>
        ConfigKeys.rejectUnknown(fields.keys, RuleKeys, where)
        val pattern = fields
          .get("permission")
          .flatMap(_.strOpt)
          .map(PermissionPattern.parse(_, where))
          .getOrElse(throw ConfigError(s"$where: a rule needs a 'permission'"))
        val items = fields
          .get("items")
          .map: v =>
            val list = v.arrOpt
              .filter(_.nonEmpty)
              .map(_.toList.map(_.strOpt.filter(!_.isBlank)))
              .filter(_.forall(_.isDefined))
              .map(_.flatten)
              .getOrElse(
                throw ConfigError(
                  s"$where: 'items' of $pattern must be a non-empty array of names"
                )
              )
            pattern.segments match
              case "files" :: _ => list.map(RoleRule.directory(_, where)).toSet
              case "network" :: _ => list.map(RoleRule.host).toSet
              case _              => list.toSet
        pattern.segments match
          case "*" :: _ if items.isDefined =>
            throw ConfigError(
              s"$where: 'items' needs a kind of permission, e.g. files:read, exec or plugin:<id>/<permission>, not *"
            )
          // Each plugin has its own items; the same names across all plugins
          // are unlikely to be meant.
          case ("plugin" :: Nil | "plugin" :: "*" :: _) if items.isDefined =>
            throw ConfigError(
              s"$where: 'items' of a plugin permission needs the plugin, e.g. plugin:<id>/<permission>, not $pattern"
            )
          case _ => ()
        RoleRule(pattern, items)
      case other =>
        throw ConfigError(
          s"$where: a rule must be a permission or an object, got ${other.render()}"
        )

  private def validId(id: String): Boolean =
    id.indexOf(':') match
      case -1 => false
      case i  => i > 0 && i < id.length - 1 && !id.exists(_.isWhitespace)

/** `identities.json`, re-read whenever its modification time or size
  * changes, so that roles and links apply without a restart. A change that
  * does not load, or a file that cannot be read, keeps the previous identities
  * in force and is logged; a deleted file links nobody. The file failing at
  * startup is an error, as with any configuration.
  */
final class IdentitiesFile(file: File, operator: Option[String] = None):
  private val logger = LoggerFactory.getLogger(classOf[IdentitiesFile])

  /** `None` stamp and text: the file does not exist. */
  private final case class Loaded(
      stamp: Option[(FileTime, Long)],
      raw: Option[String],
      identities: Identities
  )

  private val loaded =
    val stamp = stampNow()
    val raw = read(stamp)
    AtomicReference(Loaded(stamp, raw, load(raw)))

  def current(): Identities =
    val last = loaded.get()
    try
      val stamp = stampNow()
      if stamp == last.stamp then last.identities
      else
        val raw = read(stamp)
        val next =
          if raw == last.raw then last.copy(stamp = stamp)
          else
            try Loaded(stamp, raw, load(raw))
            catch
              case e: ConfigError =>
                logger.warn(s"keeping the previous identities: ${e.getMessage}")
                // Not parsed again, nor warned about, until the file changes.
                last.copy(stamp = stamp)
        val _ = loaded.compareAndSet(last, next)
        next.identities
    catch
      case e: IOException =>
        // Read again next time: the file may be readable once more.
        logger.warn(
          s"keeping the previous identities: cannot read ${file.getPath}: $e"
        )
        last.identities

  private def stampNow(): Option[(FileTime, Long)] =
    try
      val attributes =
        Files.readAttributes(file.toPath, classOf[BasicFileAttributes])
      Some((attributes.lastModifiedTime, attributes.size))
    catch case _: NoSuchFileException => None

  private def read(stamp: Option[(FileTime, Long)]): Option[String] =
    stamp.map(_ => Files.readString(file.toPath, StandardCharsets.UTF_8))

  private def load(raw: Option[String]): Identities =
    val identities =
      raw.fold(Identities.empty)(Identities.parse(_, file.getPath))
    operator.fold(identities)(identities.withOperator)

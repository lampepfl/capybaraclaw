package capybaraclaw.gateway

import java.io.{File, IOException}
import java.nio.file.{InvalidPathException, Paths}
import java.util.Locale

import capybaraclaw.agent.ConfigError

/** Which permissions a rule names, written `kind[:access]` or
  * `plugin[:<plugin>[/<permission>]]`, e.g. `files`, `files:read`, `exec`,
  * `network:fetch`, `plugin:*` or `plugin:sheets/read payroll`. `*` matches one
  * whole segment, and a pattern names everything below it: `files` covers
  * `files:read` and `files:write`, `plugin:sheets` every permission of plugin
  * `sheets`, and `*` everything.
  */
final case class PermissionPattern(segments: List[String]):
  def matches(permission: Permission): Boolean =
    PermissionPattern.names(permission).exists(covers(segments, _))

  private def covers(pattern: List[String], name: List[String]): Boolean =
    (pattern, name) match
      case (Nil, _)             => true
      case (_, Nil)             => false
      case ("*" :: pt, _ :: nt) => covers(pt, nt)
      case (p :: pt, n :: nt)   => p == n && covers(pt, nt)

  override def toString: String = segments match
    case "plugin" :: plugin :: rest =>
      s"plugin:$plugin${rest.map("/" + _).mkString}"
    case _ => segments.mkString(":")

object PermissionPattern:
  private val Kinds = List("files", "exec", "network", "plugin")

  /** The names a permission goes by: a wider access covers a narrower one,
    * as grants do (`files:write` covers reading, `network:send` fetching).
    */
  private def names(permission: Permission): List[List[String]] =
    permission match
      case Permission.Files(_, Permission.FileAccess.Read) =>
        List(List("files", "read"), List("files", "write"))
      case Permission.Files(_, Permission.FileAccess.ReadWrite) =>
        List(List("files", "write"))
      case Permission.Commands(_) => List(List("exec"))
      case Permission.Hosts(_, Permission.NetworkAccess.Fetch) =>
        List(List("network", "fetch"), List("network", "send"))
      case Permission.Hosts(_, Permission.NetworkAccess.Send) =>
        List(List("network", "send"))
      case Permission.Plugin(plugin, name, _) =>
        List(List("plugin", plugin, name))

  def parse(text: String, where: String): PermissionPattern =
    def fail(why: String): Nothing =
      throw ConfigError(s"$where: permission ${Permission.quote(text)} $why")
    val segments = text.split(":", 2).toList match
      case "plugin" :: rest :: Nil => "plugin" :: rest.split("/", 2).toList
      case other                   => other
    if segments.exists(_.isBlank) then fail("has an empty part")
    if segments.exists(s => s.contains('*') && s != "*") then
      fail("may only use * for a whole part, e.g. plugin:*")
    segments match
      case List("*")                          => ()
      case kind :: _ if !Kinds.contains(kind) =>
        fail(s"must start with ${Kinds.mkString(", ")} or be *")
      case List("files", access) if !Set("read", "write", "*")(access) =>
        fail("must be files, files:read or files:write")
      case "exec" :: _ :: _ =>
        fail("must be exec; name the commands in 'items'")
      case List("network", access) if !Set("fetch", "send", "*")(access) =>
        fail("must be network, network:fetch or network:send")
      case _ => ()
    PermissionPattern(segments)

/** One entry of a role's `may`: the permissions its pattern names, limited to
  * `items` if given. Items are command names for `exec`, host names for
  * `network`, the plugin's items for `plugin`, and directories (covering
  * everything below them) for `files`.
  */
final case class RoleRule(
    pattern: PermissionPattern,
    items: Option[Set[String]] = None
):
  /** The part of `permission` this rule does not allow, if any. Requests for a
    * plugin permission as a whole, without items, are only allowed by rules
    * without items.
    */
  def disallowed(permission: Permission): Option[Permission] =
    if !pattern.matches(permission) then Some(permission)
    else
      items match
        case None          => None
        case Some(allowed) =>
          permission match
            case Permission.Files(root, _) =>
              Option.unless(
                allowed.exists(dir =>
                  Paths.get(root).startsWith(Paths.get(dir))
                )
              )(permission)
            case Permission.Commands(names) =>
              val missing = names -- allowed
              Option.when(missing.nonEmpty)(Permission.Commands(missing))
            case Permission.Hosts(hosts, access) =>
              val missing = hosts.filterNot(h => allowed(RoleRule.host(h)))
              Option.when(missing.nonEmpty)(Permission.Hosts(missing, access))
            case Permission.Plugin(plugin, name, pluginItems) =>
              val missing = pluginItems -- allowed
              Option.when(pluginItems.isEmpty || missing.nonEmpty)(
                Permission.Plugin(plugin, name, missing)
              )

object RoleRule:
  /** The part of `permission` that none of `rules` allows, if any. Each item
    * may be allowed by a different rule.
    */
  def disallowedByAll(
      rules: List[RoleRule],
      permission: Permission
  ): Option[Permission] =
    rules.foldLeft(Option(permission)): (left, rule) =>
      left.flatMap(rule.disallowed)

  /** Host names are compared case-insensitively. */
  private[gateway] def host(name: String): String =
    name.toLowerCase(Locale.ROOT)

  /** Directory items as the canonical paths TACIT asks for: `~/` is the home
    * directory, and symlinks are resolved (e.g. `/tmp` on macOS).
    */
  private[gateway] def directory(path: String, where: String): String =
    val expanded =
      if path == "~" then System.getProperty("user.home")
      else if path.startsWith("~/") then
        System.getProperty("user.home") + path.drop(1)
      else path
    def invalid(why: String): Nothing =
      throw ConfigError(s"$where: directory ${Permission.quote(path)} $why")
    val absolute =
      try Paths.get(expanded).isAbsolute
      catch case _: InvalidPathException => invalid("is not a valid path")
    if !absolute then invalid("must be absolute or start with ~/")
    try File(expanded).getCanonicalPath
    catch case e: IOException => invalid(s"cannot be resolved: ${e.getMessage}")

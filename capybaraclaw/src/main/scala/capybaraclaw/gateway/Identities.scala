package capybaraclaw.gateway

import java.io.File

import scala.io.Source
import scala.util.control.NonFatal

import capybaraclaw.agent.{ConfigError, ConfigKeys}

enum Role:
  case Admin, Member

/** A human, possibly reachable through several ports. `id` is the name from
  * `identities.json`, or `"<port>:<user>"` for someone not listed there.
  */
final case class Person(id: String, role: Role)

/** Who is who across ports, from the operator-written `identities.json`:
  *
  * {{{
  * { "people": { "lukasz": { "ids": ["cli:lbialy", "slack:U0123"], "role": "admin" } } }
  * }}}
  *
  * Linking ids only ever comes from this file, never from the agent or from
  * what a user claims in a conversation.
  */
final class Identities private (byId: Map[String, Person]):
  def person(port: PortId, user: UserId): Person =
    val id = s"$port:$user"
    byId.getOrElse(id, Person(id, Role.Member))

object Identities:
  val empty: Identities = Identities(Map.empty)

  private val TopKeys = List("people")
  private val PersonKeys = List("ids", "role")

  /** `~/.claw/identities.json`, or nobody linked if it does not exist. */
  def default(): Identities =
    load(File(System.getProperty("user.home"), ".claw/identities.json"))

  /** Raises [[ConfigError]] naming the file and the offending entry. */
  def load(file: File): Identities =
    if !file.exists() then empty
    else
      val path = file.getPath
      val obj =
        try
          val raw =
            val source = Source.fromFile(file, "UTF-8")
            try source.mkString
            finally source.close()
          ujson.read(raw).obj
        catch
          case NonFatal(e) =>
            throw ConfigError(
              s"$path is not a valid JSON object: ${e.getMessage}"
            )
      ConfigKeys.rejectUnknown(obj.keys, TopKeys, path)
      val people =
        obj.get("people") match
          case None    => Map.empty[String, ujson.Value]
          case Some(v) =>
            v.objOpt.getOrElse(
              throw ConfigError(s"$path: 'people' must be an object")
            )
      val entries = people.toList.flatMap: (name, value) =>
        val where = s"$path: person '$name'"
        if name.isBlank then throw ConfigError(s"$path: a person name is blank")
        val fields = value.objOpt.getOrElse(
          throw ConfigError(s"$where must be an object")
        )
        ConfigKeys.rejectUnknown(fields.keys, PersonKeys, where)
        val role = fields.get("role").map(_.strOpt) match
          case None                 => Role.Member
          case Some(Some("admin"))  => Role.Admin
          case Some(Some("member")) => Role.Member
          case Some(_)              =>
            throw ConfigError(s"$where: 'role' must be \"admin\" or \"member\"")
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
        ids.map(_ -> Person(name, role))
      entries
        .groupBy(_._1)
        .collectFirst:
          case (id, owners) if owners.map(_._2.id).distinct.size > 1 =>
            id -> owners.map(_._2.id).distinct.sorted
        .foreach: (id, owners) =>
          throw ConfigError(
            s"$path: id '$id' is listed for several people: ${owners.mkString(", ")}"
          )
      Identities(entries.toMap)

  private def validId(id: String): Boolean =
    id.indexOf(':') match
      case -1 => false
      case i  => i > 0 && i < id.length - 1 && !id.exists(_.isWhitespace)

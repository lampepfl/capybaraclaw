package capybaraclaw.gateway.port.slack

import gears.async.ReadableChannel

trait SlackApi:
  def sendMessage(
      channel: String,
      text: String,
      threadTs: Option[String] = None
  ): String

  def readHistory(channel: String, limit: Int = 32): List[Message]
  def getChannel(id: String): Channel
  def getUser(id: String): User
  def messageChannel: ReadableChannel[Message]
  def shutdown(): Unit

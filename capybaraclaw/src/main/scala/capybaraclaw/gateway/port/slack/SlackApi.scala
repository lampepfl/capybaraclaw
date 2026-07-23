package capybaraclaw.gateway.port.slack

import gears.async.ReadableChannel

trait SlackApi:
  def sendMessage(
      channel: String,
      text: String,
      threadTs: Option[String] = None
  ): String

  def startStream(
      channel: String,
      threadTs: String,
      /** Required for channels, optional for DMs. */
      recipientUserId: Option[String],
      markdown: String
  ): String

  def appendStream(channel: String, ts: String, markdown: String): Unit
  def stopStream(channel: String, ts: String): Unit
  def readHistory(channel: String, limit: Int = 32): List[Message]
  def getChannel(id: String): Channel
  def getUser(id: String): User
  def messageChannel: ReadableChannel[Message]
  def shutdown(): Unit

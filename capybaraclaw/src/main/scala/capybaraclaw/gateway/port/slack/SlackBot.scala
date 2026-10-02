package capybaraclaw.gateway.port.slack
import language.experimental.captureChecking

import gears.async.ReadableChannel

class SlackBot(botToken: String, appToken: String) extends SlackApi:
  val client: SlackClient = SlackClient(botToken, appToken)

  def sendMessage(
      channel: String,
      text: String,
      threadTs: Option[String] = None
  ): String =
    client.sendMessage(channel, text, threadTs)

  def startStream(
      channel: String,
      threadTs: String,
      recipientUserId: Option[String],
      markdown: String
  ): String =
    client.startStream(channel, threadTs, recipientUserId, markdown)

  def appendStream(channel: String, ts: String, markdown: String): Unit =
    client.appendStream(channel, ts, markdown)

  def stopStream(channel: String, ts: String): Unit =
    client.stopStream(channel, ts)

  def readHistory(channel: String, limit: Int = 32): List[Message] =
    client.readHistory(channel, limit)

  def getChannel(id: String): Channel = client.getChannel(id)
  def getUser(id: String): User = client.getUser(id)

  def addReaction(channel: String, ts: String, name: String): Unit =
    client.addReaction(channel, ts, name)

  def removeReaction(channel: String, ts: String, name: String): Unit =
    client.removeReaction(channel, ts, name)

  def postApprovalPrompt(
      channel: String,
      threadTs: Option[String],
      prompt: ApprovalPrompt
  ): String =
    client.postApprovalPrompt(channel, threadTs, prompt)

  def closeApprovalPrompt(
      channel: String,
      ts: String,
      prompt: ApprovalPrompt,
      outcome: String
  ): Unit =
    client.closeApprovalPrompt(channel, ts, prompt, outcome)

  def postEphemeral(
      channel: String,
      threadTs: Option[String],
      userId: String,
      text: String
  ): Unit =
    client.postEphemeral(channel, threadTs, userId, text)

  def messageChannel: ReadableChannel[Message] = client.messageChannel
  def approvalClicks: ReadableChannel[ApprovalClick] = client.approvalClicks
  def shutdown(): Unit = client.shutdown()

object SlackBot:
  def fromEnv(): SlackBot =
    val botToken = sys.env.getOrElse(
      "SLACK_BOT_TOKEN",
      throw RuntimeException("SLACK_BOT_TOKEN not set")
    )
    val appToken = sys.env.getOrElse(
      "SLACK_APP_TOKEN",
      throw RuntimeException("SLACK_APP_TOKEN not set")
    )
    SlackBot(botToken, appToken)

  def usingBot[R](block: SlackBot^ => R): R =
    val bot = fromEnv()
    try block(bot)
    finally bot.shutdown()

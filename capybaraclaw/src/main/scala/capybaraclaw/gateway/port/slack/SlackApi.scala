package capybaraclaw.gateway.port.slack

import capybaraclaw.gateway.ApprovalDecision
import gears.async.ReadableChannel

final case class ApprovalPrompt(
    requestId: Int,
    question: String,
    reason: Option[String]
)

final case class ApprovalClick(
    userId: String,
    channel: String,
    messageTs: String,
    threadTs: Option[String],
    requestId: Int,
    decision: ApprovalDecision
)

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

  def addReaction(channel: String, ts: String, name: String): Unit
  def removeReaction(channel: String, ts: String, name: String): Unit

  def postApprovalPrompt(
      channel: String,
      threadTs: Option[String],
      prompt: ApprovalPrompt
  ): String

  def closeApprovalPrompt(
      channel: String,
      ts: String,
      prompt: ApprovalPrompt,
      outcome: String
  ): Unit

  def postEphemeral(
      channel: String,
      threadTs: Option[String],
      userId: String,
      text: String
  ): Unit

  def messageChannel: ReadableChannel[Message]
  def approvalClicks: ReadableChannel[ApprovalClick]
  def shutdown(): Unit

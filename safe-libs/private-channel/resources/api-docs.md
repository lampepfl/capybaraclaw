## Private channel (plugin `claw.private-channel`)

Deliver `Classified` results to the people allowed to see them: a private Slack
channel chosen by the operator, optionally through a private model. You never
see what is delivered, only that it was.

```scala
def askPrivately(question: String, data: Classified[Any])(using IOCapability): String
```
A private model answers `question` using `data` (its `toString`), and the
question and answer are posted to the private channel. Ask a complete question:
you will not see the answer, so you cannot follow up on it.

```scala
def postPrivately(message: Classified[Any])(using IOCapability): String
```
Posts `message` (its `toString`) to the private channel as it is.

Both return at once with a confirmation to pass on to the user, e.g. "Sent
to #hr-private ..."; the message appears in the channel shortly after. The
first call in a session asks the user for permission ("post any confidential
data capybara holds to the private channel"); until they approve it fails with
a message naming the permission request. Tell the user you are waiting, as
with any other permission request. Messages may arrive out of order; send one
complete result rather than many small ones.

```scala
val payroll = requestFileSystem(".", FileAccess.ReadOnly, "to analyse the payroll"):
  loadSheet("payroll.xlsx")
askPrivately("Who is paid furthest below their level's average?", payroll.asText)
val report = payroll.avgBy("Department", "Salary").map(_.map((d, v) => f"$d: $v%.0f").mkString("\n"))
postPrivately(report)
```

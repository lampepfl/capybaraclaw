val scala3Version = "3.10.0-RC1-bin-20260816-3adfcbd-NIGHTLY"
ThisBuild / resolvers += Resolver.scalaNightlyRepository

val stableScala3Version = "3.8.4"

val tacitVersion = "0.2.1-SNAPSHOT"
val tacitLibraryVersion = "0.2.1-SNAPSHOT"

lazy val clawCommand = Command.args("claw", "[<path>] [--flags...]") { (state, args) =>
  val quotedArgs = args.map { arg =>
    val escaped = arg.replace("\\", "\\\\").replace("\"", "\\\"")
    s""""$escaped""""
  }
  val runCommand =
    if (quotedArgs.isEmpty) "capybaraclaw/run"
    else s"capybaraclaw/run ${quotedArgs.mkString(" ")}"
  runCommand :: state
}

addCommandAlias("simple-agent", "agents/runMain tacit.agents.simpleAgentRepl")

val MUnitFramework = new TestFramework("munit.Framework")
val TestFull = config("testFull").extend(Test)

lazy val agents = project
  .in(file("agents"))
  .configs(TestFull)
  .settings(
    name := "tacit-agents",
    organization := "lampepfl",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := stableScala3Version,
    scalacOptions ++= Seq(
      "-deprecation", "-feature", "-unchecked",
      "-Yexplicit-nulls", "-Wsafe-init",
      "-language:experimental.modularity",
    ),
    libraryDependencies ++= Seq(
      "com.openai" % "openai-java" % "4.29.1",
      "com.anthropic" % "anthropic-java" % "2.18.0",
      "ch.epfl.lamp" %% "gears" % "0.2.0",
      "com.lihaoyi" %% "ujson" % "4.1.0",
      "org.scalameta" %% "munit" % "1.2.2" % Test,
    ),
    testFrameworks += MUnitFramework,
    Test / testOptions += Tests.Argument(MUnitFramework, "--exclude-tags=Network"),
    inConfig(TestFull)(Defaults.testTasks),
    TestFull / testOptions := Seq.empty,
  )

lazy val capybaraclaw = project
  .in(file("capybaraclaw"))
  .dependsOn(agents)
  .configs(TestFull)
  .settings(
    name := "capybaraclaw",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-deprecation", "-feature", "-unchecked",
      "-Yexplicit-nulls", "-Wsafe-init",
      "-language:experimental.modularity",
    ),
    libraryDependencies ++= Seq(
      "com.slack.api" % "bolt" % "1.48.0",
      "com.slack.api" % "bolt-socket-mode" % "1.48.0",
      "ch.qos.logback" % "logback-classic" % "1.5.32",
      "org.glassfish.tyrus.bundles" % "tyrus-standalone-client" % "1.21",
      "org.jline" % "jline-reader" % "4.0.12",
      "org.jline" % "jline-terminal-jni" % "4.0.12",
      "xyz.matthieucourt" %% "layoutz" % "0.7.0",
      "com.github.alexarchambault" %% "case-app" % "2.1.0",
      "org.xerial" % "sqlite-jdbc" % "3.53.0.0",
      "org.flywaydb" % "flyway-core" % "12.4.0",
      "org.apache.commons" % "commons-text" % "1.15.0",
      "lampepfl" %% "tacit" % tacitVersion,
      ("lampepfl" %% "tacit-library" % tacitLibraryVersion)
        .excludeAll(ExclusionRule(organization = "*", name = "*")),
      "org.scalameta" %% "munit" % "1.2.2" % Test,
    ),
    testFrameworks += MUnitFramework,
    Test / testOptions += Tests.Argument(MUnitFramework, "--exclude-tags=Network"),
    inConfig(TestFull)(Defaults.testTasks),
    TestFull / testOptions := Seq.empty,
    fork := true,
    run / fork := false,
    run / connectInput := true,
    Compile / mainClass := Some("capybaraclaw.main"),
  )

lazy val root = (project in file("."))
  .aggregate(agents, capybaraclaw)
  .settings(
    name := "capybaraclaw-root",
    publish / skip := true,
    commands += clawCommand,
  )

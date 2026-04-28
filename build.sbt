val scala3Version = {
  val fallback = "3.8.3-RC1"
  try {
    val url = "https://repo.scala-lang.org/artifactory/api/storage/local-maven-nightlies/org/scala-lang/scala3-compiler_3/"
    val content = scala.io.Source.fromURL(url, "UTF-8").mkString
    val pattern = """"uri"\s*:\s*"/(3\.[^"]*NIGHTLY)"""".r
    val versions = pattern.findAllMatchIn(content).map(_.group(1)).toList.sorted
    val latest = versions.last
    if (latest != fallback) println(s"[info] Use Scala 3 nightly: $latest")
    latest
  } catch { case _: Exception =>
    println(s"[warn] Failed to fetch latest nightly, using fallback: $fallback")
    fallback
  }
}
ThisBuild / resolvers += Resolver.scalaNightlyRepository

val stableScala3Version = "3.8.2"

val tacitVersion = "0.1.4-SNAPSHOT"
val tacitLibraryVersion = "0.1.4-SNAPSHOT"

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
      "ch.qos.logback" % "logback-classic" % "1.5.16",
      "org.glassfish.tyrus.bundles" % "tyrus-standalone-client" % "1.21",
      "org.jline" % "jline-reader" % "3.29.0",
      "org.jline" % "jline-terminal-jni" % "3.29.0",
      "xyz.matthieucourt" %% "layoutz" % "0.7.0",
      "com.github.alexarchambault" %% "case-app" % "2.1.0",
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
    javaOptions += {
      val cp = (Compile / dependencyClasspath).value
      val libJar = cp.map(_.data).find(_.getName.contains("tacit-library"))
        .map(_.getAbsolutePath)
        .getOrElse(sys.error(
          "tacit-library JAR not found on the dependency classpath. " +
          "Run `sbt lib/publishLocal` in the tacit repo first."
        ))
      s"-Dtacit.library.jar=$libJar"
    },
  )

lazy val root = (project in file("."))
  .aggregate(agents, capybaraclaw)
  .settings(
    name := "capybaraclaw-root",
    publish / skip := true,
    commands += clawCommand,
  )

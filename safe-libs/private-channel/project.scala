//> using scala 3.10.1-RC1-bin-20260930-0afa84a-NIGHTLY
//> using options -language:experimental.captureChecking -language:experimental.modularity
//> using repository ivy2Local
// The REPL's sandbox loads the host's tacit-library JAR ahead of plugin JARs
// (it also brings circe), so the plugin compiles against it but must not
// bundle it.
//> using compileOnly.dep lampepfl::tacit-library:0.2.2-SNAPSHOT
//> using resourceDir ./resources
// preamble.scala is a resource the REPL runs, not a source of this JAR.
//> using exclude "resources/**"
//> using test.dep lampepfl::tacit-library:0.2.2-SNAPSHOT
//> using test.dep org.scalameta::munit:1.3.5

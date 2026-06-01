//> using scala 3.8.nightly
//> using options -language:experimental.captureChecking -language:experimental.modularity -language:experimental.saferExceptions
//> using dep "com.example::safemode-capabilities:0.1.0"
//> using dep "org.apache.poi:poi-ooxml:5.4.0"
// POI 5.x uses Log4j2 API as a logging facade. Without a provider on the
// classpath, every load logs `Log4j API could not find a logging provider`
// to stderr, which clutters REPL output and confuses the agent's read of
// tool results. Bundle a provider + a silent config in resources/.
//> using dep "org.apache.logging.log4j:log4j-core:2.24.3"
//> using repository "ivy2Local"
//> using publish.organization "com.example"
//> using publish.name "safemode-compreview"
//> using publish.version "0.1.0"
//> using resourceDir ./resources

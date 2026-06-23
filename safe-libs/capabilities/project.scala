//> using scala 3.nightly
//> using options -language:experimental.captureChecking -language:experimental.modularity -language:experimental.saferExceptions
//> using publish.organization "com.example"
//> using publish.name "safemode-capabilities"
//> using publish.version "0.1.0"
// `resourceDir` is intentionally NOT set here: capabilities' plugin resources
// (tacit-plugin.json / preamble.scala / api-docs.md) are injected into the
// assembled JAR by `safe-libs/build-jars.sh` after packaging. This keeps the
// resources OUT of the ivy2Local-published library, so downstream plugins
// that depend on `safemode-capabilities` (compreview, private-llm) don't
// collide on the top-level resource names when they fat-assemble.

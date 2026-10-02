package capybaraclaw.agent

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

object TestPlugins:
  def write(
      dir: Path,
      jarName: String,
      apiMode: String,
      preamble: String = "// test preamble",
      id: String = "test.demo"
  ): Unit =
    val pluginsDir = dir.resolve("plugins")
    Files.createDirectories(pluginsDir)
    val manifest =
      s"""{"schemaVersion":1,"id":"$id","name":"Demo","version":"1.0","apiMode":"$apiMode"}"""
    val entries = List(
      "tacit-plugin.json" -> manifest,
      "preamble.scala" -> preamble,
      "api-docs.md" -> "demo docs"
    )
    val out = JarOutputStream(
      Files.newOutputStream(pluginsDir.resolve(jarName))
    )
    try
      entries.foreach: (name, content) =>
        out.putNextEntry(JarEntry(name))
        out.write(content.getBytes(StandardCharsets.UTF_8))
        out.closeEntry()
    finally out.close()

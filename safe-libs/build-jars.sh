#!/usr/bin/env bash
# Build fat (library) jars for every safe-lib under safe-libs/<name>/. Each lib
# must have a project.scala. Output: safe-libs/<name>/target/<name>-assembly.jar
# Tacit's REPL discovers these via Config.pluginScanDirs; capybaraclaw drops
# (or symlinks) them into ${workdir}/plugins/ so the agent picks them up at
# startup. JARs intended as plugins must include a top-level tacit-plugin.json
# manifest (alongside preamble.scala and api-docs.md).
#
# Each lib is also `scala-cli publish local`-ed before assembly so that
# downstream libs (e.g. compreview / private-llm that depend on
# safemode-capabilities) can resolve it from ~/.ivy2/local on the next
# iteration. Build order is the alphabetical order of subdirectories — keep
# foundation libs (capabilities) lexicographically first if you add new ones
# with cross-deps.
#
# Fat-assembly note: private-llm and compreview each bundle safemode-capabilities
# classes as a transitive dep, so the safemode.lib.* classes appear in three
# JARs. They are byte-identical because all three pull the SAME ivy2Local
# artifact. **Rebuild every plugin after touching capabilities** to avoid
# class-version drift between the bundled copies.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SAFE_LIBS_DIR="$REPO_ROOT/safe-libs"

for lib in "$SAFE_LIBS_DIR"/*/; do
  name="$(basename "$lib")"
  if [ ! -f "$lib/project.scala" ]; then
    echo "[build-jars] skipping $name (no project.scala)"
    continue
  fi
  out="$lib/target/${name}-assembly.jar"
  mkdir -p "$lib/target"
  echo "[build-jars] publishing $name to ivy2Local for downstream deps"
  cd "$lib"
  scala-cli --power publish local .
  echo "[build-jars] building $name → $out"
  scala-cli --power package --assembly --preamble=false . -o "$out" --force

  # `safe-libs/capabilities/` deliberately omits `using resourceDir ./resources`
  # so that its plugin metadata is NOT republished into ivy2Local — otherwise
  # downstream plugins that bundle capabilities as a dep would carry duplicate
  # tacit-plugin.json / preamble.scala / api-docs.md entries and the assembly
  # step would fail with a ZipException. We inject the resources directly into
  # the capabilities assembly after the build, so the runtime JAR still has
  # everything TACIT's plugin loader needs.
  if [ "$name" = "capabilities" ] && [ -d "$lib/resources" ]; then
    echo "[build-jars] injecting resources into $name assembly"
    (cd "$lib/resources" && zip -uq "$out" tacit-plugin.json preamble.scala api-docs.md)
  fi

  cd "$REPO_ROOT"
done

echo "[build-jars] done"

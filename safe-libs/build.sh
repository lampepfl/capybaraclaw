#!/usr/bin/env bash
# Builds the TACIT plugins in safe-libs/ into a workdir's plugins/ folder,
# where capybaraclaw loads them for every new session.
#
#   safe-libs/build.sh [workdir]    # default: capybaraclaw/examples/default
#
# Needs tacit-library 0.2.2-SNAPSHOT in ~/.ivy2/local, built from the TACIT
# commit capybaraclaw pins (see .github/workflows/test.yml): `sbt publishLocal`
# in that checkout.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
workdir="${1:-$here/../capybaraclaw/examples/default}"
mkdir -p "$workdir/plugins"

for lib in sheets private-channel; do
  # The host provides tacit-library (compile-only in project.scala) and the
  # Scala standard library, so neither goes into the plugin JAR.
  scala-cli --power package --assembly --preamble=false \
    --provided org.scala-lang:scala3-library_3 \
    --provided org.scala-lang:scala-library \
    "$here/$lib" -o "$workdir/plugins/$lib.jar" --force
done

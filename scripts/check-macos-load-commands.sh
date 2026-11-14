#!/usr/bin/env bash
# Checks that each given Mach-O binary loads only libraries in /usr/lib/ or /System/Library/. A library path from the
# build machine, such as Homebrew's libcrypto (issue #95), stops the binary at start on a Mac that does not have that
# file, before main runs. A test run on the build runner cannot find this, because the runner has Homebrew. Used by
# .github/workflows/release.yml and .github/workflows/build.yml and by hand.
#
# usage: scripts/check-macos-load-commands.sh <binary>...
set -euo pipefail

if [ "$#" -eq 0 ]; then
  echo "usage: $0 <binary>..." >&2
  exit 2
fi

FAILED=0

for BINARY in "$@"; do
  if [ ! -f "$BINARY" ]; then
    echo "error: file not found: $BINARY" >&2
    FAILED=1
    continue
  fi

  LOAD_COMMANDS="$(otool -L "$BINARY")"
  echo "$LOAD_COMMANDS"

  BINARY_FAILED=0
  # Library lines start with a tab. The file name line and the "(architecture ...)" lines of a fat binary do not.
  while IFS= read -r LIBRARY; do
    case "$LIBRARY" in
      /usr/lib/* | /System/Library/*) ;;
      *)
        echo "ERROR: $BINARY loads $LIBRARY, which is not in /usr/lib/ or /System/Library/"
        BINARY_FAILED=1
        ;;
    esac
  # The path can contain spaces, so remove the " (compatibility version ...)" suffix, not split on spaces.
  done < <(printf '%s\n' "$LOAD_COMMANDS" | awk '/^\t/ { line = substr($0, 2); sub(/ \(compatibility version.*$/, "", line); print line }')

  if [ "$BINARY_FAILED" -eq 0 ]; then
    echo "OK: $BINARY loads only system libraries"
  else
    FAILED=1
  fi
done

exit "$FAILED"

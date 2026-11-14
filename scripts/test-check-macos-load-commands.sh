#!/usr/bin/env bash
# Tests scripts/check-macos-load-commands.sh without Homebrew (issue #95). It builds small binaries with clang in a temp
# folder: one that loads only libSystem, a fat one (arm64 and x86_64), and one that loads a dylib from the temp folder,
# whose path is not a system path and has a space. Then it checks the exit code and the output of the check script for
# each case. Used by .github/workflows/build.yml and by hand. macOS only.
#
# usage: scripts/test-check-macos-load-commands.sh
set -euo pipefail

CHECK="$(cd "$(dirname "$0")" && pwd)/check-macos-load-commands.sh"

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
LIB_DIR="$TMP/with space"
mkdir -p "$LIB_DIR"

cat > "$TMP/main.c" <<'EOF'
int main(void) { return 0; }
EOF
cat > "$TMP/foo.c" <<'EOF'
int foo(void) { return 0; }
EOF
cat > "$TMP/uses-foo.c" <<'EOF'
int foo(void);
int main(void) { return foo(); }
EOF

clang "$TMP/main.c" -o "$TMP/system-only"
clang -arch arm64 -arch x86_64 "$TMP/main.c" -o "$TMP/universal"
clang -dynamiclib "$TMP/foo.c" -o "$LIB_DIR/libfoo.dylib"
clang "$TMP/uses-foo.c" "$LIB_DIR/libfoo.dylib" -o "$TMP/uses-foo"

FAILED=0

# expect <case> <expected exit code> <text that the output must have>... -- <check script arguments>...
expect() {
  local name="$1"
  local expected_exit="$2"
  shift 2
  local texts=()
  while [ "$1" != "--" ]; do
    texts+=("$1")
    shift
  done
  shift
  local output
  local actual_exit=0
  output="$("$CHECK" "$@" 2>&1)" || actual_exit=$?
  local ok=1
  if [ "$actual_exit" -ne "$expected_exit" ]; then
    echo "  expected exit $expected_exit, got $actual_exit"
    ok=0
  fi
  local text
  for text in "${texts[@]+"${texts[@]}"}"; do
    if ! grep -qF -- "$text" <<< "$output"; then
      echo "  the output has no: $text"
      ok=0
    fi
  done
  if [ "$ok" -eq 1 ]; then
    echo "PASS: $name"
  else
    local line
    while IFS= read -r line; do
      echo "  | $line"
    done <<< "$output"
    echo "FAIL: $name"
    FAILED=1
  fi
}

expect "a binary that loads only libSystem" 0 "OK: $TMP/system-only" -- "$TMP/system-only"
expect "a fat binary" 0 "OK: $TMP/universal" -- "$TMP/universal"
expect "a binary that loads a dylib outside the system folders" 1 \
  "ERROR: $TMP/uses-foo loads $LIB_DIR/libfoo.dylib, which is not in /usr/lib/ or /System/Library/" -- "$TMP/uses-foo"
expect "a good and a bad binary together" 1 "OK: $TMP/system-only" "ERROR: $TMP/uses-foo" -- \
  "$TMP/system-only" "$TMP/uses-foo"
expect "no arguments" 2 "usage:" --
expect "a file that does not exist" 1 "error: file not found: $TMP/nope" -- "$TMP/nope"

exit "$FAILED"

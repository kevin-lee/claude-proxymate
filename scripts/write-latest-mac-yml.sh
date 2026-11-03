#!/usr/bin/env bash
# Writes <dir>/latest-mac.yml, the electron-updater feed for macOS, from the arm64 and x64 zips of one release. Each build
# runner writes its own latest-mac.yml with only its own arch, so the release job writes the merged one instead. Both zips
# go in `files`. electron-updater on an Apple Silicon Mac (also under Rosetta) keeps the entries whose name contains
# "arm64", and on an Intel Mac the entries without it, so one feed serves both. The top-level `path` and `sha512` are only
# read by electron-updater versions before 2.15 and point at the arm64 zip. `sha512` is the base64 SHA-512 of the file,
# as electron-builder writes it. Used by .github/workflows/release.yml and by hand. Does not overwrite.
#
# usage: scripts/write-latest-mac-yml.sh <version> <dir>
set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "usage: $0 <version> <dir>" >&2
  exit 2
fi

VERSION="$1"
DIR="$2"

if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "error: version must be X.Y.Z: $VERSION" >&2
  exit 1
fi
if [ ! -d "$DIR" ]; then
  echo "error: directory not found: $DIR" >&2
  exit 1
fi

OUT="$DIR/latest-mac.yml"
if [ -e "$OUT" ]; then
  echo "error: already exists: $OUT" >&2
  exit 1
fi

ARM_ZIP="Claude-Proxy-$VERSION-arm64.zip"
X64_ZIP="Claude-Proxy-$VERSION-x64.zip"
for zip in "$ARM_ZIP" "$X64_ZIP"; do
  if [ ! -f "$DIR/$zip" ]; then
    echo "error: $DIR/$zip not found" >&2
    exit 1
  fi
done

# sha512_base64 <file>: the base64 SHA-512 of the file on one line.
sha512_base64() {
  openssl dgst -sha512 -binary "$1" | openssl base64 -A
}

# size_of <file>: the file size in bytes.
size_of() {
  wc -c < "$1" | tr -d '[:space:]'
}

ARM_SHA="$(sha512_base64 "$DIR/$ARM_ZIP")"
ARM_SIZE="$(size_of "$DIR/$ARM_ZIP")"
X64_SHA="$(sha512_base64 "$DIR/$X64_ZIP")"
X64_SIZE="$(size_of "$DIR/$X64_ZIP")"
RELEASE_DATE="$(date -u +%Y-%m-%dT%H:%M:%S.000Z)"

{
  echo "version: $VERSION"
  echo "files:"
  echo "  - url: $ARM_ZIP"
  echo "    sha512: $ARM_SHA"
  echo "    size: $ARM_SIZE"
  echo "  - url: $X64_ZIP"
  echo "    sha512: $X64_SHA"
  echo "    size: $X64_SIZE"
  echo "path: $ARM_ZIP"
  echo "sha512: $ARM_SHA"
  echo "releaseDate: '$RELEASE_DATE'"
} > "$OUT.tmp"
mv "$OUT.tmp" "$OUT"

echo "Done. Generated: $OUT"

#!/usr/bin/env bash
# Updates Casks/claude-proxymate.rb in a checkout of kevin-lee/homebrew-tap in place and keeps the previous release as
# Casks/claude-proxymate@<previous>.rb. The cask is edited, not regenerated, so content added by hand in the tap (such as
# preflight_steps) is kept. The pinned copy gets the tap's versioned-cask form: livecheck skipped, and preflight guards
# pointed at Caskroom/claude-proxymate/<v> under :homebrew_prefix, because a versioned cask's own caskroom_path only ever
# holds its own version. Used by .github/workflows/release.yml and by hand. Does not commit.
#
# usage: scripts/update-cask.sh <version> <arm64-sha256> <x64-sha256> <tap-checkout>
set -euo pipefail

if [ "$#" -ne 4 ]; then
  echo "usage: $0 <version> <arm64-sha256> <x64-sha256> <tap-checkout>" >&2
  exit 2
fi

VERSION="$1"
ARM_SHA="$2"
INTEL_SHA="$3"
TAP="$4"

if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "error: version must be X.Y.Z: $VERSION" >&2
  exit 1
fi
for sha in "$ARM_SHA" "$INTEL_SHA"; do
  if ! [[ "$sha" =~ ^[0-9a-f]{64}$ ]]; then
    echo "error: not a SHA-256 hex digest: $sha" >&2
    exit 1
  fi
done

CASKS="$TAP/Casks"
if [ ! -d "$CASKS" ]; then
  echo "error: Casks directory not found: $CASKS" >&2
  exit 1
fi
CURRENT="$CASKS/claude-proxymate.rb"
if [ ! -f "$CURRENT" ]; then
  echo "error: current cask not found: $CURRENT" >&2
  exit 1
fi

# parse_cask <file>: sets CASK_VERSION, CASK_ARM and CASK_INTEL from the cask's version and sha256 lines.
parse_cask() {
  CASK_VERSION="$(sed -n 's/^  version "\(.*\)"$/\1/p' "$1")"
  CASK_ARM="$(sed -n 's/^  sha256 arm:   "\(.*\)",$/\1/p' "$1")"
  CASK_INTEL="$(sed -n 's/^         intel: "\(.*\)"$/\1/p' "$1")"
}

parse_cask "$CURRENT"
PREV_VERSION="$CASK_VERSION"
PREV_ARM="$CASK_ARM"
PREV_INTEL="$CASK_INTEL"
if [ -z "$PREV_VERSION" ] || [ -z "$PREV_ARM" ] || [ -z "$PREV_INTEL" ]; then
  echo "error: could not parse the current cask: $CURRENT" >&2
  exit 1
fi

if [ "$PREV_VERSION" = "$VERSION" ]; then
  if [ "$PREV_ARM" = "$ARM_SHA" ] && [ "$PREV_INTEL" = "$INTEL_SHA" ]; then
    echo "Already at $VERSION with the same checksums. Nothing to do."
    exit 0
  fi
  echo "error: the cask is already at $VERSION with different checksums" >&2
  exit 1
fi

PINNED="$CASKS/claude-proxymate@$PREV_VERSION.rb"
if [ -e "$PINNED" ]; then
  echo "error: already exists: $PINNED" >&2
  exit 1
fi

# The pinned copy of the current cask. The cask header, the livecheck url and strategy lines must each appear exactly
# once. The preflight comment and guards are rewritten only when present.
if ! awk \
  -v prev="$PREV_VERSION" \
  -v note="  # The unversioned cask is checked because this cask's own caskroom_path only ever holds $PREV_VERSION." '
  $0 == "cask \"claude-proxymate\" do" {
    print "cask \"claude-proxymate@" prev "\" do"
    header++
    next
  }
  $0 == "    url :url" {
    print "    skip \"Versioned cask; pinned to " prev "\""
    url++
    next
  }
  $0 == "    strategy :github_latest" {
    strategy++
    next
  }
  $0 == "  # Remove the pre-0.3.0 route-mode.json as its format is incompatible with 0.3.0+." {
    print
    print note
    next
  }
  /^    if_path_exists "[^"]+", base: :caskroom_path do$/ {
    sub(/if_path_exists "/, "if_path_exists \"Caskroom/claude-proxymate/")
    sub(/base: :caskroom_path/, "base: :homebrew_prefix")
    print
    next
  }
  { print }
  END {
    if (header != 1 || url != 1 || strategy != 1) exit 1
  }
' "$CURRENT" > "$PINNED.tmp"; then
  rm -f "$PINNED.tmp"
  echo "error: the current cask does not have the expected shape: $CURRENT" >&2
  exit 1
fi
mv "$PINNED.tmp" "$PINNED"

sed \
  -e "s/^  version \"${PREV_VERSION//./\\.}\"\$/  version \"$VERSION\"/" \
  -e "s/^  sha256 arm:   \"$PREV_ARM\",\$/  sha256 arm:   \"$ARM_SHA\",/" \
  -e "s/^         intel: \"$PREV_INTEL\"\$/         intel: \"$INTEL_SHA\"/" \
  "$CURRENT" > "$CURRENT.tmp"
mv "$CURRENT.tmp" "$CURRENT"

parse_cask "$CURRENT"
if [ "$CASK_VERSION" != "$VERSION" ] || [ "$CASK_ARM" != "$ARM_SHA" ] || [ "$CASK_INTEL" != "$INTEL_SHA" ]; then
  echo "error: failed to update $CURRENT" >&2
  exit 1
fi

echo "Done. Generated:"
echo "  - $CURRENT"
echo "  - $PINNED"
echo "Previous: $PREV_VERSION"

#!/bin/zsh
# Render the whole site outside Dropbox, then copy the result into docs/.
#
# Why not `quarto render` in place: on 2026-10-28 Dropbox reverted a fresh
# in-place build to older files and left ~110 "conflicted copy" duplicates.
# The build copy also leaves out .redesign/ (experiments), _old/, and
# _blog-drafts/, so nothing from them can reach the published site.
#
# Usage: ./render.sh   (always a full render; a partial render here would
#                       wipe the pages it didn't build from docs/)
set -euo pipefail
ROOT=$(cd "$(dirname "$0")" && pwd)
BUILD=${TMPDIR:-/tmp}/carlislerainey-website-build

mkdir -p "$BUILD"
rsync -a --delete \
  --exclude docs/ --exclude .git/ --exclude .redesign/ --exclude _old/ \
  --exclude _blog-drafts/ --exclude .Rproj.user/ --exclude '*conflicted copy*' \
  "$ROOT"/ "$BUILD"/

(cd "$BUILD" && quarto render)

rsync -a --delete --checksum "$BUILD"/docs/ "$ROOT"/docs/
# keep any freeze results from posts that re-executed
rsync -a --checksum "$BUILD"/_freeze/ "$ROOT"/_freeze/

echo "docs/ updated. Dropbox can revert files a few minutes later; check with:"
echo "  find docs -name '*conflicted copy*'"

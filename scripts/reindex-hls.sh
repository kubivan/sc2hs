#!/usr/bin/env bash
# Rebuilds local packages and re-indexes their .hie files into the HLS hiedb.
# Run this after adding/modifying source files in sc2monad or sc2api.
# Usage: ./scripts/reindex-hls.sh

set -e

WORKSPACE="$(cd "$(dirname "$0")/.." && pwd)"
DB="$HOME/.cache/ghcide/$(echo -n "$WORKSPACE" | sha1sum | cut -d' ' -f1)-sc2hs-9.10.1-1.hiedb"
HIEDB="$HOME/.cabal/bin/hiedb"

if [ ! -f "$HIEDB" ]; then
  echo "hiedb not found at $HIEDB"
  echo "Install with: cabal install hiedb-0.6.0.0 --install-method=copy -w ~/.ghcup/ghc/9.10.1/bin/ghc"
  exit 1
fi

if [ ! -f "$DB" ]; then
  echo "HLS hiedb not found at $DB"
  echo "Open the project in VS Code first to let HLS create the database."
  exit 1
fi

echo "Rebuilding packages..."
cd "$WORKSPACE"
stack build --fast

echo "Indexing sc2monad .hie files..."
SC2MONAD_HIE=$(find "$WORKSPACE/sc2monad/.stack-work" -path "*/extra-compilation-artifacts/hie" -type d | head -1)
$HIEDB -D "$DB" --src-base-dir "$WORKSPACE/sc2monad" index "$SC2MONAD_HIE"

echo "Indexing sc2api .hie files..."
SC2API_HIE=$(find "$WORKSPACE/sc2api/.stack-work" -path "*/extra-compilation-artifacts/hie" -type d | head -1)
$HIEDB -D "$DB" --src-base-dir "$WORKSPACE/sc2api" index "$SC2API_HIE"

echo "Done. Restart HLS in VS Code (Ctrl+Shift+P → 'Haskell: Restart Haskell LSP Server') to pick up new entries."

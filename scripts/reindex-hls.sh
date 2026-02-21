#!/usr/bin/env bash
# Rebuilds local packages and re-indexes their .hie files into all HLS hiedbs
# that exist for this project — covering both sc2hs and sc2-workspace open roots.
# Run this after adding/modifying source files in sc2monad or sc2api.
# Usage: ./scripts/reindex-hls.sh

set -e

WORKSPACE="$(cd "$(dirname "$0")/.." && pwd)"
HIEDB="$HOME/.cabal/bin/hiedb"

if [ ! -f "$HIEDB" ]; then
  echo "hiedb not found at $HIEDB"
  echo "Install with: cabal install hiedb-0.6.0.0 --install-method=copy -w ~/.ghcup/ghc/9.10.1/bin/ghc"
  exit 1
fi

# Collect all hiedb paths to populate.
# HLS names the db: <sha1(project_root)>-<project_name>-9.10.1-1.hiedb
# We cover both "open from sc2hs" and "open from sc2-workspace (parent)".
declare -a DBS=()
for root in "$WORKSPACE" "$(dirname "$WORKSPACE")"; do
  hash="$(echo -n "$root" | sha1sum | cut -d' ' -f1)"
  name="$(basename "$root")"
  db="$HOME/.cache/ghcide/${hash}-${name}-9.10.1-1.hiedb"
  # Remove any corrupt 0-byte db (happens if created via 'touch' before hiedb runs)
  if [ -f "$db" ] && [ ! -s "$db" ]; then
    rm -f "$db" "$db-wal" "$db-shm"
  fi
  DBS+=("$db")
done

echo "Rebuilding packages..."
cd "$WORKSPACE"
stack build --fast

SC2MONAD_HIE=$(find "$WORKSPACE/sc2monad/.stack-work" -path "*/extra-compilation-artifacts/hie" -type d | head -1)
SC2API_HIE=$(find "$WORKSPACE/sc2api/.stack-work" -path "*/extra-compilation-artifacts/hie" -type d | head -1)

for DB in "${DBS[@]}"; do
  if [ ! -f "$DB" ]; then
    echo "Creating new hiedb at $DB"
    touch "$DB"
  fi
  echo "Indexing into $(basename "$DB")..."
  $HIEDB -D "$DB" --src-base-dir "$WORKSPACE/sc2monad" index "$SC2MONAD_HIE"
  $HIEDB -D "$DB" --src-base-dir "$WORKSPACE/sc2api"   index "$SC2API_HIE"
done

echo "Done. Restart HLS (VS Code: 'Haskell: Restart Haskell LSP Server') to pick up new entries."

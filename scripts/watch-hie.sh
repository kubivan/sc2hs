#!/usr/bin/env bash
# Watches for .hie file changes and re-indexes them into hiedb automatically
# Run once in a terminal alongside your development session

set -euo pipefail

WORKSPACE=$(pwd)
DB=$(find ~/.cache/ghcide -name "*sc2hs*9.10.1*.hiedb" | head -1)
HIEDB=~/.cabal/bin/hiedb

echo "Watching .hie files, indexing into: $DB"

find sc2monad sc2api -name "*.hie" | entr -n bash -c "
  echo '[hie-watch] Re-indexing sc2monad...'
  $HIEDB -D $DB --src-base-dir $WORKSPACE/sc2monad \
    index $WORKSPACE/sc2monad/.stack-work/dist/\$(ls sc2monad/.stack-work/dist/)/build/extra-compilation-artifacts/hie

  echo '[hie-watch] Re-indexing sc2api...'
  $HIEDB -D $DB --src-base-dir $WORKSPACE/sc2api \
    index $WORKSPACE/sc2api/.stack-work/dist/\$(ls sc2api/.stack-work/dist/)/build/extra-compilation-artifacts/hie

  echo '[hie-watch] Done.'
"
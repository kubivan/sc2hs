# HLS Cross-Package "Go to Definition" — Problem & Fix

## The Problem

In a multi-package Stack workspace (`sc2api`, `sc2monad`, `lambdarookie01`), hovering over a symbol from a sibling package in VS Code showed correct type information (e.g. *"defined in sc2monad"*), but **"Go to Definition" did nothing**.

### Why hover worked but jump didn't

HLS has two separate mechanisms:

| Feature | Mechanism |
|---|---|
| Hover / type info | Reads GHC's in-memory type-checked AST for the *current* component |
| Go to Definition | Queries a persistent **SQLite database** (`hiedb`) that maps symbol → source file |

The hover info came from the already-loaded module graph. Navigation requires a database entry with an **absolute path to the source file** — and that entry was missing for sibling packages.

---

## Root Cause: Three Compounding Issues

### 1. No `hie.yaml` — wrong cradle

Without a `hie.yaml` at the workspace root, HLS auto-discovers the project. With both `stack.yaml` and a `dist-newstyle/` directory present, the detection was ambiguous. More importantly, there was no explicit mapping of which Stack *component* handles each source directory.

**Fix:** Created `hie.yaml` at the workspace root:

```yaml
cradle:
  stack:
    - path: "./sc2api/src"
      component: "sc2api:lib"
    - path: "./sc2monad/src"
      component: "sc2monad:lib"
    - path: "./sc2monad/test"
      component: "sc2monad:test:sc2monad-test"
    - path: "./lambdarookie01/src"
      component: "lambdarookie01:exe:lambdarookie01"
```

This tells `hie-bios` (the cradle library HLS uses) exactly which GHC invocation to use for each source path.

### 2. No `.hie` files — GHC never wrote them

`.hie` files (Haskell Interface Extended) are binary files GHC writes alongside normal compilation output. They contain a full typed AST with source spans — the raw material `hiedb` indexes.

By default, GHC **does not** write `.hie` files. The flag `-fwrite-ide-info` must be explicitly passed.

**Fix:** Added to `stack.yaml`:

```yaml
ghc-options:
  "$everything": -fexternal-interpreter
  "$locals": -fwrite-ide-info   # ← added
```

`$locals` applies only to in-project packages (not external deps), avoiding unnecessary slowdown. After `stack clean && stack build`, 144 `.hie` files were generated across all three packages.

### 3. `.hie` files generated but not indexed into `hiedb`

HLS maintains a project-specific SQLite database at:

```
~/.cache/ghcide/<sha1(workspace_path)>-<project>-<ghc_version>-1.hiedb
```

HLS only indexes `.hie` files for components it **actively compiles** (i.e. the one containing the file currently open). It never scans dependency packages' `.hie` files automatically.

Querying the database before the fix:

```
sqlite3 <db> "SELECT count(*) FROM mods;"
-- 7  (only lambdarookie01 modules)
```

**Fix:** Manually indexed `sc2monad` and `sc2api` using the `hiedb` CLI tool, with `--src-base-dir` pointing to the package source root so relative paths in `.hie` files get resolved to absolute paths:

```sh
hiedb -D <db> --src-base-dir ./sc2monad \
  index ./sc2monad/.stack-work/.../extra-compilation-artifacts/hie

hiedb -D <db> --src-base-dir ./sc2api \
  index ./sc2api/.stack-work/.../extra-compilation-artifacts/hie
```

After indexing:

```
sqlite3 <db> "SELECT count(*) FROM mods;"
-- 76  (lambdarookie01 + sc2monad + sc2api)

sqlite3 <db> "SELECT mod, hs_src FROM mods WHERE unit LIKE 'sc2monad%' LIMIT 2;"
-- Footprint | /home/.../sc2monad/src/Footprint.hs
-- StepMonad | /home/.../sc2monad/src/StepMonad.hs
```

---

## Version Compatibility Gotcha

The standalone `hiedb` on Hackage (0.7.0.0) uses a **different schema version** (`99999067`) than the hiedb embedded in HLS 2.10.0.0 (`89999101`). Using the wrong version gives:

```
hiedb: IncompatibleSchemaVersion {expectedVersion = 99999067, gotVersion = 89999101}
```

**Fix:** Install `hiedb-0.6.0.0` specifically, compiled against GHC 9.10.1:

```sh
cabal install hiedb-0.6.0.0 --install-method=copy \
  -w ~/.ghcup/ghc/9.10.1/bin/ghc
```

---

## Why the Fix Is Persistent

The hiedb filename is `sha1(absolute_workspace_path)`. It is stable across HLS restarts as long as the workspace path doesn't change. The manually indexed entries survive indefinitely — HLS only *adds* to or queries the database, it does not wipe it on startup.

---

## Maintenance

The hiedb entries for `sc2monad` and `sc2api` become **stale** when those packages are rebuilt (new `.hie` files with updated content). Run the helper script after rebuilding:

```sh
./scripts/reindex-hls.sh
```

Then restart HLS: `Ctrl+Shift+P` → **Haskell: Restart Haskell LSP Server**.

---

## Summary of Files Changed

| File | Change |
|---|---|
| `hie.yaml` | Created — explicit Stack cradle mapping all source dirs to components |
| `stack.yaml` | Added `"$locals": -fwrite-ide-info` to emit `.hie` files on build |
| `scripts/reindex-hls.sh` | Created — maintenance script to re-index after rebuilds |
| `cabal.project.bak` | Renamed from `cabal.project` — not used; kept to avoid confusing HLS |

## Active Multi-Package Development Flow

### Day-to-day workflow

Run `stack build --file-watch` in a terminal. This rebuilds and regenerates
`.hie` files on every save. HLS picks up changes to existing modules automatically.

### When to run `reindex-hls.sh` + restart HLS

| Event | Action needed |
|---|---|
| Edit existing function/type | Nothing — HLS reloads automatically |
| Add new definition to existing module | Nothing |
| Add a **new `.hs` file** to sc2monad/sc2api | `reindex-hls.sh` + restart HLS |
| Add a new dependency in `.cabal` | `stack build` + restart HLS |
| After `stack clean && stack build` | `reindex-hls.sh` + restart HLS |

### Optional: auto-reindex watcher

Run `./scripts/watch-hie.sh` in a background terminal to automatically
re-index `.hie` files after each build. Still requires HLS restart for
truly new modules.
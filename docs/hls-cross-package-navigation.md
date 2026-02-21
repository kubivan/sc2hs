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

---

## Issue: "Go to Definition" Fails When Opening `sc2-workspace/` (Parent Dir)

### Symptom

Opening nvim from `sc2hs/` → go-to-definition works (even cross-package).
Opening nvim from `sc2-workspace/` → go-to-definition breaks for Haskell
(but still works for Rust files under `aiurgaze/`).

### Root Cause: Two Layers

#### Layer 1 — Rogue Mason HLS Instance (fixed first)

Mason's `haskell-language-server` package was installed alongside ghcup's,
and `nvim-lspconfig`'s built-in `lsp/hls.lua` was launching a second HLS
instance using Mason's binary (GHC 9.6.7, wrong version).

**Fix:**
- Uninstalled Mason's HLS
- Added `vim.lsp.config("hls", ...)` guard with empty cmd/filetypes
- Cleaned stale GHC 9.6.7 hiedbs

#### Layer 2 — Wrong Process CWD for HLS Wrapper (the real fix)

Even after removing the Mason conflict, go-to-definition still failed.
The `haskell-language-server-wrapper` determines which GHC-versioned
binary to launch **from its process CWD** — before it even reads the
LSP `rootUri` from the initialize request.

**What happened:**

1. `haskell-tools.nvim` correctly detects `root_dir = sc2hs/` and sends
   `rootUri = file:///...sc2hs` in the LSP initialize request.
2. But the wrapper process was started with **CWD = `sc2-workspace/`**
   (Neovim's working directory), not `sc2hs/`.
3. The wrapper looks for `hie.yaml` / `stack.yaml` in its CWD to pick
   the GHC version → finds nothing in `sc2-workspace/` → Default cradle
   → system GHC 9.6.7.
4. Launches `haskell-language-server-9.6.7` which then finds the Stack
   project needs 9.10.1 → fatal error:
   ```
   ghcide compiled against GHC 9.6.7 but currently using 9.10.1
   This is unsupported
   ```
5. All definition/hover requests return empty.

**Why:** The `haskell-tools.nvim` code set `cwd = project_root` in the
LSP client config, but Neovim's `vim.lsp.start()` uses `cmd_cwd` for
the process working directory. The `cwd` field is silently ignored.

The Neovim 0.11.5 call chain:
```
vim.lsp.start(config)
  → config.cmd_cwd                         # client.lua:461
  → vim.lsp.rpc.start(cmd, { cwd = ... })  # passes to transport
  → vim.system(cmd, { cwd = ... })          # _transport.lua:55
```

When `cmd_cwd` is nil, `vim.system()` inherits Neovim's CWD.

**Fix:** Changed `cwd` → `cmd_cwd` in `haskell-tools.nvim/lua/haskell-tools/lsp/init.lua`:

```lua
-- Before (silently ignored by Neovim):
cwd = project_root,

-- After (correctly sets process working directory):
cmd_cwd = project_root,
```

### Why Rust Worked from `sc2-workspace/`

rust-analyzer has a fundamentally different architecture:

- **Auto-discovers** `Cargo.toml` recursively — no root config needed
- **Single binary** — no GHC-version wrapper; same binary works for all editions
- **No conflicting installations** — Mason's rust-analyzer IS the one being used

HLS's `haskell-language-server-wrapper` architecture creates a
version-selection problem that rust-analyzer doesn't have.

### Fix Summary

1. **Uninstalled Mason's HLS** — removed the conflicting second instance.
2. **Added config guard** in mason.lua — prevents lspconfig's hls.lua from
   starting even if Mason HLS is re-installed.
3. **Cleaned stale GHC 9.6.7 hiedbs**.
4. **Changed `cwd` → `cmd_cwd`** in haskell-tools.nvim `lsp/init.lua` —
   ensures the wrapper process starts in the project root, not Neovim's CWD.

---

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


========

# 1. Verify hie-bios actually resolves the cradle correctly for a specific file
cd /home/ikubariev/src/sc2-workspace/sc2hs
hie-bios check sc2monad/src/StepMonad.hs

# 2. Check what flags GHC gets for that file
hie-bios flags sc2monad/src/StepMonad.hs

# 3. Check HLS logs for cradle errors (VS Code)
cat ~/.cache/hie-bios/hiedb.log 2>/dev/null || true
ls -la ~/.local/share/haskell-language-server/logs/ 2>/dev/null || true

# 4. Check Neovim HLS log
ls -la /tmp/hls-*.log 2>/dev/null || true

# 5. Verify hie.yaml syntax is valid
cat hie.yaml

# 6. Check if hie-bios is even installed and which version
hie-bios --version

# 7. Most importantly — check if HLS is finding the hie.yaml at the RIGHT root
# HLS looks for hie.yaml by walking UP from the open file
# If Neovim opens a file with cwd = sc2monad/ instead of sc2hs/, it misses the root hie.yaml
pwd
ls -la hie.yaml
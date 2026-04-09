# CLAUDE.md — Development Guide for R Dependency Analyzer

This file documents the architecture, known bugs, and desired output spec for this tool. Read it before making any changes.

---

## What this tool does

Scans a research project containing R and/or Stata scripts, extracts dependencies between files (who sources whom, who reads/writes which datasets), builds a directed dependency graph, detects issues, and outputs a single interactive HTML report. It never modifies the project files — read-only analysis only.

**Primary use case**: research projects with a setup file defining path variables (e.g. `build.dir <- "data/Build/"`), multiple build scripts referencing those paths, and a master orchestrator. The tool must correctly resolve path-variable references to identify the actual file relationships.

---

## Architecture

```
run_analysis.R            Entry point. Orchestrates steps 1–6 below.
R/
  parse_r_files.R         Step 2. Parse all R/Stata files; extract sources, reads, writes.
                          Detect setup file, extract path variables, resolve paths.
  inspect_data.R          Step 3. Inspect referenced data files for metadata (read-only).
  build_graph.R           Step 4. Build dependency graph nodes/edges from parsed data.
  detect_issues.R         Step 5. Detect circular deps, missing files, wrong deps, etc.
  generate_outputs.R      Step 6. Generate dependency_graph.html and optional master_summary.md.
config.yaml               (unused currently; reserved for future configuration)
```

### Data flow through `parse_r_project` (in `parse_r_files.R`)

```
list.files(project_path)
  → parse each .R/.Rmd/.do file → flat list: parsed[[rel]] = {sources, data_reads, data_writes, ...}
  → parse_setup_vars(parsed)         # scan setup/master files for path variable assignments
  → infer_stata_globals_from_r(...)  # infer Stata globals from R code that sets them
  → parse_stata_globals(parsed)      # collect Stata globals defined in .do files
  → resolve_paths_with_setup(parsed, setup_vars, project_path)  # substitute vars in all paths
  → discover_master_and_path_roots(...)
  → return list(files = parsed, setup_vars = ..., master_path = ..., ...)
```

After `parse_r_project`, the returned object is `parsed` with `parsed$files` = the flat list.
Inside `parse_r_project`, before the return, `parsed` IS the flat list (no `$files` key yet).

---

## Critical Bugs

### BUG 1 — Setup vars never extracted (root cause of most path resolution failures)

**File**: `R/parse_r_files.R`  
**Impact**: Paths like `build.dir/Countries/file.csv` stay as-is instead of resolving to `data/Build/Countries/file.csv`. All path-based edges in the graph are wrong. Execution order is wrong. Dataset index is incomplete.

**Root cause**: Inside `parse_r_project`, `parsed` is a flat named list keyed by relative file paths. The helper functions are called **before** the final `list(files = parsed, ...)` wrapping, so they receive the flat list. But all four helpers incorrectly access `parsed$files` which is `NULL` at that point:

| Function | Broken access | Consequence |
|---|---|---|
| `parse_setup_vars(parsed)` | `names(parsed$files)`, `vapply(parsed$files, ...)`, `parsed$files[[rel]]` | Returns empty `vars` → no setup vars |
| `infer_stata_globals_from_r(parsed, ...)` | `names(parsed$files)`, `parsed$files[[rel]]$path` | Returns empty → no Stata globals inferred |
| `parse_stata_globals(parsed)` | `names(parsed$files)`, `parsed$files[[rel]]$path` | Returns empty → no Stata globals |
| `resolve_paths_with_setup(parsed, ...)` | `parsed$project_path` (NULL), `names(parsed$files)` | No-op → paths stay unresolved |

Also, `discover_master_and_path_roots(project_path, names(parsed$files), setup_vars)` passes `NULL` as `rel_paths` so it detects nothing.

**Fix**:
1. In `parse_setup_vars`, `infer_stata_globals_from_r`, `parse_stata_globals`: replace every `parsed$files` with `parsed`.
2. In `resolve_paths_with_setup`: add `project_path` as a 3rd parameter (it currently tries `parsed$project_path` which is NULL). Replace `parsed$files` with `parsed`. Update call at line 78 to: `parsed <- resolve_paths_with_setup(parsed, setup_vars, project_path)`.
3. In `parse_r_project` line 81: change `names(parsed$files)` to `names(parsed)`.

### BUG 2 — Hardcoded debug log lines

**File**: `R/parse_r_files.R`, lines ~56–71 and ~246–261  
**Impact**: Writes to `c:/Users/iddo2/cursor_test/debug-c0a103.log` (fails silently on other machines, but is dead code).  
**Fix**: Remove both `#region agent log ... #endregion` blocks entirely.

### BUG 3 — Hardcoded default data path in entry point

**File**: `run_analysis.R`, line 18  
**Impact**: `default_data_path <- "C:\\Users\\iddo2\\Dropbox\\Migration Africa\\data"` — hardcoded to one machine/project.  
**Fix**: Change to `default_data_path <- ""` (empty string). Users should set `R_DEP_DATA_PATH` or be prompted.

### BUG 4 — Local path variables not reliably captured

**Context**: A build script may define `ctries_dir <- paste0(build.dir, "Countries/")` and then use `fread(paste0(ctries_dir, "file.csv"))`. The tool should resolve `ctries_dir` within that file's scope.  
**Current behaviour**: `local_path_vars` are extracted from simple assignments (`var <- "literal/path"`) but not from `paste0(setup_var, "suffix")` assignments. So `ctries_dir` is not in local_vars.  
**Fix**: In `parse_single_r_file`, also extract assignments of the form `var <- paste0(other_var, "suffix")` as local path vars. After setup_vars is resolved, substitute the rhs vars to build the full path value. Store as `local_path_vars[var]`.

---

## HTML Report — Desired Spec

The HTML report should be a clean, single-page document with a fixed sidebar for navigation.

### Sections (in order)

#### 1. Header
- Project name, date generated, quick stats line: "N scripts · M datasets referenced · K issues"
- No "project overview" prose section — it's not useful.

#### 2. Dependency Graph (main section)
- Full-width interactive vis-network graph (600px height minimum)
- **Show both script nodes AND data file nodes**. Currently only shows scripts; this makes the graph much less useful.
  - Script nodes: blue boxes
  - Data file nodes: green diamonds (or similar distinct shape)
  - Edges: script→script (sources/pipeline), script→data (writes), data→script (reads)
  - Red edges = wrong dependency (kept as-is)
- Hover tooltip shows full relative path
- Legend below the graph

#### 3. Execution Order
- Keep the current level-based layout (Step 1, Step 2, etc. with parallel items in same step)
- Show full relative path on hover (currently shows basename only)
- "Not used" section at the bottom (keep as details/summary collapsible)

#### 4. File Index
- Organize by **folder** (top-level subfolder of project, e.g. Build, Final)
- For each file show:
  - Full relative path
  - Role tag: `[setup]`, `[master]`, `[stata]`, or none
  - Sources: list of scripts it sources
  - Reads: resolved dataset paths (not raw variable expressions)
  - Writes: resolved dataset paths
  - Libraries (for setup file and unique libs only)
- Currently misses some files and shows unresolved paths like `build.dir/ctries_dir` — fixed by BUG 1

#### 5. Dataset Index
- Keep organized by folder
- Show: path, format, whether file exists, which scripts read it, which write it
- No raw data previews (data is not available to the tool in general)

#### 6. Issues
- Keep as a table: severity, type, message
- Group by severity (errors first, then warnings)

#### 7. Edges (collapsible)
- Full edge list as a table, collapsed by default (currently always visible and long)

### Design guidelines
- Clean sans-serif font (system-ui or Inter)
- White background, light gray card backgrounds (`#f8fafc`)
- Primary accent: `#2563eb` (blue)
- Sidebar nav: fixed left, `240px` wide, links to each section
- Main content: `calc(100% - 240px)` right of sidebar, max-width `1100px`
- Section headings: `h2` with bottom border
- Tables: striped rows, compact, sortable where possible (vanilla JS)
- No Bootstrap or heavy CSS frameworks — keep the file self-contained
- vis-network loaded from CDN (`unpkg.com/vis-network`) as currently

---

## Path Resolution Logic (important for correctness)

The tool resolves paths in two stages:

**Stage 1 — Setup vars** (global, from setup/master file):
```r
# SSA_env_SetUp.R
build.dir <- "data/Build/"
raw.dir   <- "data/Raw/"
out.dir   <- "data/Final/"
```
These become `setup_vars["build.dir"] = "data/Build/"` etc.

**Stage 2 — Local vars** (per-file, from local assignments):
```r
# In a Build script
ctries_dir <- paste0(build.dir, "Countries/")   # should resolve to "data/Build/Countries/"
fread(paste0(ctries_dir, "AGO.csv"))             # should resolve to "data/Build/Countries/AGO.csv"
```

`resolve_path_args` splits paths on `/` and substitutes each segment. For `ctries_dir/AGO.csv` to resolve, `ctries_dir` must be in `all_vars`. This only works after local_path_vars extraction is fixed (BUG 4).

**Stata globals**: Set by R via `paste0('global build "', normalizePath(build.dir, ...), '"')`. Resolved by `infer_stata_globals_from_r`. In .do files, paths use `"${build}/Countries/AGO.dta"` → substituted to `"data/Build/Countries/AGO.dta"` → made project-relative.

---

## Code Conventions

- All functions are plain R (no tidyverse, no packages required beyond base)
- `igraph` and `readxl` are optional dependencies (checked with `requireNamespace`)
- The tool must run on Windows (backslash paths) and Mac/Linux; always normalize with `gsub("\\\\", "/",...)`
- Never use `here()` or working directory assumptions inside the tool itself — all paths are passed explicitly
- HTML is built as string concatenation (no templating library) — keep this approach
- No user-facing changes without updating `README.md`
- No new R package dependencies — keep the tool dependency-free by default

---

## Test Projects

Three example projects (code-only, no data) are available in the parent folder:

| Project | Setup file | Notes |
|---|---|---|
| `migration/` | `code/SSA_env_SetUp.R` | R + Stata, most complex; main test case |
| `conflict/` | TBD | R only |
| `thesis/` | TBD | R only |

Run the tool against `migration/` to verify fixes:
```bash
Rscript run_analysis.R C:/Users/iddo2/dependency_analyzer/migration
```
After BUG 1 is fixed, `_dependency_analysis/path_setup.txt` should show `build.dir`, `raw.dir`, `out.dir`, etc. under `setup_path_vars`. Paths in the HTML file index should show `data/Build/...` not `build.dir/...`.

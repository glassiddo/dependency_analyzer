# CLAUDE.md — Development Guide for R Dependency Analyzer

This file documents the architecture, known issues, and desired output spec for this tool. Read it before making changes.

---

## Current product direction

The analyzer is a repeatable static auditor for messy R/Stata research projects, not an agentic codebase interpreter. Its core value is a trustworthy dependency graph plus structured outputs that can be handed to an AI agent or human reviewer.

Prioritize:

1. High-quality dependency graph.
2. Machine-readable `dependency_graph.json`, `nodes.csv`, and `edges.csv`.
3. Confidence/provenance for every edge.
4. Compact `agent_context.md` handoff.
5. Less noisy HTML and actionable issues only.

Core outputs in `<project>/_dependency_analysis/`:

- `dependency_graph.html`
- `dependency_graph.json`
- `nodes.csv`
- `edges.csv`
- `issues.csv`
- `agent_context.md`
- `path_setup.txt`

Normalized graph schema:

- Node `type`: `script`, `data`, `missing_script`, optionally `external_data`.
- Node `role`: `setup`, `master`, `script`, `stata`, `archived`, `unknown`.
- Edge `type`: `source_run`, `reads`, `writes`, `data_flow`.
- Edge `confidence`: `high`, `medium`, `low`.

Low-confidence edges must remain explicit, but should not dominate the high-confidence likely partial order. Noisy observations such as "dataset written but never read" belong in notes/agent context, not the main issue table.

Archive/old/backup folders are scanned, not deleted from analysis. Archived script nodes use `role = "archived"` and default to `included = false`; they are hidden from the active graph and headline issue counts unless active code explicitly references them or `graph.show_archived` is enabled. Archived issues use `scope = "archived"` and are grouped separately in HTML, JSON, CSV, and `agent_context.md`.

---

## What this tool does

Scans a research project containing R and/or Stata scripts, extracts dependencies between files (who sources whom, who reads/writes which datasets), builds a directed dependency graph, detects scoped issues, and outputs local HTML plus JSON/CSV artifacts. It never modifies project files except writing `_dependency_analysis`.

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
  generate_outputs.R      Step 6. Generate HTML, JSON, CSV, path_setup.txt, and agent_context.md.
  config.R                Config parsing and defaults.
  data_scan.R             Dataset scanning helpers.
  dataset_index.R         Dataset index construction.
config.yaml               Optional runtime config (`run_analysis.R --config <path>`).
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

After `parse_r_project`, the returned object includes the flat list as `parsed$files`.

---

## Known issues / notes

### Windows + portability guardrails (avoid regressions)

- Do not add machine-specific absolute paths in code, examples, or tests (use `C:/path/to/...` placeholders at most).
- Do not add hardcoded debug log file outputs.
- Keep `default_data_path` empty unless there is a strong reason (prefer env vars / config).

### Potential improvement — Local path variables not always captured

**Context**: A build script may define `ctries_dir <- paste0(build.dir, "Countries/")` and then use `fread(paste0(ctries_dir, "file.csv"))`. The tool should resolve `ctries_dir` within that file's scope.

**Current behaviour**: `local_path_vars` may miss some composed assignments (e.g. `paste0(setup_var, "suffix")`), so paths can remain partially unresolved.

**Possible fix**: In `parse_single_r_file`, also extract assignments of the form `var <- paste0(other_var, "suffix")` as local path vars. After setup vars are resolved, substitute rhs vars to build the full path value. Store as `local_path_vars[var]`.

---

## HTML Report — Desired Spec

The HTML report should be a clean, single-page document with a fixed sidebar for navigation.

### Sections (in order)

#### 1. Header
- Project name, date generated, quick stats line: "N scripts · M datasets referenced · K issues"
- No "project overview" prose section — it’s not useful.

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
ctries_dir <- paste0(build.dir, "Countries/")    # "data/Build/Countries/"
fread(paste0(ctries_dir, "AGO.csv"))             # "data/Build/Countries/AGO.csv"
```

**Stata globals**: Set by R via `paste0('global build "', normalizePath(build.dir, ...), '"')`. In .do files, paths can use `"${build}/Countries/AGO.dta"` which should resolve to `"data/Build/Countries/AGO.dta"` and then be made project-relative.

---

## Code Conventions

- Required runtime dependencies are R, `dplyr`, and `jsonlite`.
- `yaml`, `igraph`, and `readxl` are optional dependencies checked with `requireNamespace`.
- The tool must run on Windows (backslash paths) and Mac/Linux; always normalize with `gsub("\\\\", "/",...)`
- Never use `here()` or working directory assumptions inside the tool itself — all paths are passed explicitly
- HTML is built as string concatenation (no templating library) — keep this approach
- No user-facing changes without updating `README.md`
- Keep dependencies light and document any new required package in `README.md`.

---

## Git Sync (avoid local vs GitHub drift)

To keep your local checkout aligned with GitHub:

- Before starting work: `git pull --ff-only origin main`
- Before pushing: `git fetch origin` then `git status -sb` (ensure you are not behind `origin/main`)
- When you have local experiments you do not want to commit: `git stash push -u -m "wip"` (or commit on a feature branch)
- Quick equality check: `git rev-parse HEAD` and `git rev-parse origin/main` (they should match when synced)

If Git errors with `.git/index.lock` permission denied on Windows, reset the `.git` ACLs from an elevated PowerShell:

- `icacls .git /reset /T /C`
- Or run: `powershell -NoProfile -ExecutionPolicy Bypass -File .\\r_dep_analyzer\\tools\\reset-git-acl.ps1`

---

## Test Projects

Three example projects (code-only, no data) are available in the parent folder:

| Project | Setup file | Notes |
|---|---|---|
| `migration/` | `code/_Master.do` (detected root) | R + Stata, most complex; main test case |
| `conflict/` | (auto-detected roots) | R only |
| `thesis/` | TBD | R only |

Run the tool against `migration/` to verify fixes (from repo root):
```bash
Rscript .\\r_dep_analyzer\\run_analysis.R --config .\\r_dep_analyzer\\config.yaml .\\migration
```

Verify that the HTML file index shows resolved paths like `data/Build/...` rather than unresolved tokens like `build.dir/...`.

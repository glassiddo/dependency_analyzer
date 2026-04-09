# R Dependency Analyzer

A **read-only** tool that scans R and Stata research projects, extracts file dependencies, and generates an interactive HTML report showing the dependency graph, execution order, and file/dataset index.

## What it produces

A single `dependency_graph.html` inside `_dependency_analysis/` in your project folder:

- **Interactive graph** — scripts and datasets as nodes; edges show source relationships, data reads/writes. Zoom and pan. Red edges = wrong dependency.
- **Execution order** — topologically sorted list of scripts; same step = can run in parallel.
- **File index** — all R and Stata scripts organized by folder, with their resolved reads/writes/sources.
- **Dataset index** — all referenced datasets, which scripts produce and consume them.
- **Issues** — circular dependencies, missing source files, wrong dependency order.

## Supported patterns

**R / RMarkdown**:
- `source("path")` — script sourcing
- `read.csv`, `readRDS`, `fread`, `read_dta`, `read_excel`, etc. — data reads
- `saveRDS`, `fwrite`, `write_dta`, `write.csv`, etc. — data writes
- `here()` and `here::here()` — project-relative path resolution
- Path variables from setup files: `build.dir <- "data/Build/"` resolved throughout project

**Stata (.do)**:
- `use`, `save`, `merge using`, `import delimited`, `export` — data reads/writes
- `global` definitions — path variable resolution
- Stata globals set by R (e.g. `paste0('global build "', normalizePath(build.dir), '"')`) — inferred automatically

## Usage

**From terminal:**
```bash
Rscript run_analysis.R /path/to/your/research/project
# Exclude folders (e.g. Archive, LSMS):
Rscript run_analysis.R /path/to/project "Archive,LSMS"
```

**From R / RStudio:**
```r
setwd("/path/to/r_dep_analyzer")
Sys.setenv(R_DEP_PROJECT_PATH = "/path/to/your/project")
source("run_analysis.R")
```

**Environment variables:**

| Variable | Description |
|---|---|
| `R_DEP_PROJECT_PATH` | Path to project (alternative to command-line arg) |
| `R_DEP_EXCLUDE_FOLDERS` | Comma-separated folder names to skip (e.g. `Archive,LSMS`) |
| `R_DEP_PROJECT_NAME` | Display name for the report (defaults to folder name) |
| `R_DEP_DATA_PATH` | External data root (optional; used to check if files exist) |
| `R_DEP_MASTER_SUMMARY` | Set to `1` to also generate `master_summary.md` |
| `R_DEP_INCLUDE_ARCHIVE` | Set to `1` to include Archive/archive folders (excluded by default) |

## Output files

Written to `<project>/_dependency_analysis/`:

| File | Description |
|---|---|
| `dependency_graph.html` | Main report — open in any browser |
| `path_setup.txt` | Debug: detected setup vars and path roots (useful for troubleshooting) |
| `master_summary.md` | Optional Markdown summary (set `R_DEP_MASTER_SUMMARY=1`) |

## Dependencies

The tool runs with base R only. Optional packages improve some features:

- **igraph** — better cycle detection and topological sort: `install.packages("igraph")`
- **readxl** — Excel metadata: `install.packages("readxl")`

The interactive graph uses vis.js loaded from CDN (no R package needed).

## Project structure

```
r_dep_analyzer/
  run_analysis.R        Entry point
  R/
    parse_r_files.R     Parse R/Stata files, extract and resolve dependencies
    inspect_data.R      Inspect referenced datasets (metadata only, read-only)
    build_graph.R       Build dependency graph
    detect_issues.R     Detect issues (circular deps, missing files, etc.)
    generate_outputs.R  Generate HTML report and optional Markdown summary
  README.md             This file
  CLAUDE.md             Development guide (architecture, bugs, spec) — for AI/developers
```

## Tips

- **Path variables**: The tool automatically detects a setup file (any file named with "setup", "master", "config", or "init") and extracts path variable assignments like `build.dir <- "data/Build/"`. These are substituted throughout the project so edges show real paths, not variable names.
- **Stata projects**: If your R master script sets Stata globals (e.g. `paste0('global build "', normalizePath(build.dir), '"')`), the tool infers the global values from the R-side variables — no manual configuration needed.
- **Archive folders**: Excluded by default. Use `R_DEP_INCLUDE_ARCHIVE=1` to include them.
- **Troubleshooting paths**: Check `_dependency_analysis/path_setup.txt` to see which setup file and path variables were detected.

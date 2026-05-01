# R Dependency Analyzer

A **read-only** static dependency auditor for messy R and Stata research projects. It extracts script/data dependencies, assigns confidence and provenance to every edge, and writes structured artifacts that can be handed to a human reviewer or AI agent.

## What it produces

Files are written to `<project>/_dependency_analysis/`:

| File | Description |
|---|---|
| `dependency_graph.html` | Human report with interactive graph, handoff summary, actionable issues, setup/master/path detection, and collapsible details |
| `dependency_graph.json` | Main machine-readable artifact: project metadata, summary, detected hypotheses, nodes, edges, issues, and notes |
| `nodes.csv` | Flat node table |
| `edges.csv` | Flat edge table with confidence, provenance, source file, raw/resolved path, and certainty notes |
| `issues.csv` | Actionable issues only |
| `agent_context.md` | Compact AI-agent/human reviewer handoff |
| `path_setup.txt` | Debug view of detected setup/master candidates and path variables |
| `master_summary.md` | Optional legacy summary when `R_DEP_MASTER_SUMMARY=1` |

## Supported patterns

**R / RMarkdown**
- `source("path")`, `run_r(here(...))`, `run_stata(here(...))`
- `read.csv`, `readRDS`, `fread`, `read_dta`, `read_excel`, etc.
- `saveRDS`, `fwrite`, `write_dta`, `write.csv`, etc.
- `here()`, `here::here()`, `file.path()`, selected `paste0()` patterns
- Path variables from setup/master files and local script assignments

**Stata (`.do`)**
- `do`, `run`, `include`
- `use`, `save`, `merge using`, `import delimited`, `export`
- `global` definitions and globals inferred from R-side Stata setup

## Usage

```powershell
Rscript .\r_dep_analyzer\run_analysis.R --config .\r_dep_analyzer\config.yaml .\migration
Rscript .\r_dep_analyzer\run_analysis.R .\conflict
```

Legacy explicit folder exclusion still works:

```powershell
Rscript .\r_dep_analyzer\run_analysis.R .\migration "Archive,LSMS"
```

## Configuration

`config.yaml` supports both legacy `graph.roots` and the newer top-level fields:

```yaml
setup_files: []
master_files: []
exclude_dirs: [Archive, archive, archives, old, Old, backup, Backup]
include_dirs: []
roots: []
graph:
  show_archived: false
  show_low_confidence_edges: true
```

Archive/old/backup folders are scanned and represented in the structured outputs. Archived scripts get `role = "archived"` and `included = false` by default, so they are hidden from the default active graph and headline issues. If active code explicitly references an archived script, that reference is shown as an active warning. The HTML graph includes a **Show archived** toggle, and archived/stale issues are grouped separately.

## Confidence

- **High**: direct resolved `source`/Stata `do`; direct concrete read/write; exact normalized writer/read match.
- **Medium**: path variables or folder-level inference were involved.
- **Low**: unresolved variables, basename-only dataset matches, or otherwise ambiguous static inference.

Low-confidence edges remain in JSON/CSV and can be shown in HTML, but they do not dominate the high-confidence partial order.

## Issue scope

`issues.csv` and `dependency_graph.json` include a `scope` field:

- `active`: issue affects the default active graph.
- `archived`: issue comes from archived/old/backup code and is grouped as stale context.
- `global`: issue is project-wide uncertainty, such as duplicate dataset names.

## Dependencies

Required:

- R
- `dplyr`
- `jsonlite` for `dependency_graph.json`

Optional packages improve specific features:

- `igraph` for stronger cycle detection and topological sorting.
- `yaml` for more robust config parsing.
- `readxl` for Excel metadata when available.

The HTML graph is self-contained inline SVG/JavaScript and opens offline.

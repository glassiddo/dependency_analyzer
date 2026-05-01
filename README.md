# R Dependency Analyzer

Example graph screenshots for this project are from running the analyzer on the public thesis repository [`glassiddo/masters_thesis_thailand`](https://github.com/glassiddo/masters_thesis_thailand). GitHub's README editor can host an uploaded screenshot directly if you want the image embedded at the top.

`r_dep_analyzer` is a static, read-only dependency auditor for messy R and Stata research projects. It scans scripts, data reads and writes, `source()` and Stata `do` relationships, path/setup files, archived folders, and unresolved references, then writes a compact set of review artifacts.

Agentic AI can inspect a codebase, but messy research projects often contain hundreds of scripts, archived folders, duplicated dataset names, path variables, Stata globals, and partial pipelines. This tool gives the agent or reviewer a stable first pass: a read/write/source graph, explicit confidence levels, provenance for each edge, and a compact handoff file. It does not replace human or AI judgment; it reduces the initial search space and makes uncertainty visible instead of burying it in a long conversational code review.

## Setup

Use R 4.1 or newer. The analyzer uses the base pipe operator, `|>`, which was introduced in R 4.1.

Install required packages:

```r
install.packages(c("dplyr", "jsonlite"))
```

Optional packages improve specific features:

```r
install.packages(c("yaml", "igraph", "readxl"))
```

`yaml` enables more robust config parsing, `igraph` improves cycle detection and ordering, and `readxl` enables Excel metadata/sample inspection.

## Usage

From a clone of this repository:

```sh
Rscript path/to/r_dep_analyzer/run_analysis.R path/to/project
```

Example from the repo root:

```sh
Rscript r_dep_analyzer/run_analysis.R path/to/project
```

Outputs are written to:

```text
<project>/_dependency_analysis/
```

The analyzer does not modify the analyzed project except for creating or replacing files in `_dependency_analysis`. It may read referenced data files to collect metadata and small previews for CSV, RDS, RData, and Excel files. Stata `.dta` files are detected but not read.

If the project keeps data outside the project directory, pass a read-only data root:

```sh
Rscript r_dep_analyzer/run_analysis.R path/to/project --data-path path/to/data
```

The same value can be supplied with:

```sh
R_DEP_DATA_PATH=path/to/data Rscript r_dep_analyzer/run_analysis.R path/to/project
```

Useful options:

```sh
Rscript r_dep_analyzer/run_analysis.R --help
Rscript r_dep_analyzer/run_analysis.R path/to/project --roots scripts/master.R
Rscript r_dep_analyzer/run_analysis.R --config r_dep_analyzer/config.yaml path/to/project
```

## Outputs

Representative output files:

| File | Purpose |
|---|---|
| `dependency_graph.html` | Human-readable report and offline interactive graph |
| `dependency_graph.json` | Machine-readable graph, summary, detected setup/master files, issues, and notes |
| `nodes.csv` | Flat node table |
| `edges.csv` | Flat edge table with confidence, provenance, source file, raw path, and resolved path |
| `issues.csv` | Actionable issues, scoped as active, archived, or global |
| `agent_context.md` | Compact handoff for an AI agent or reviewer |
| `path_setup.txt` | Debug summary of setup/master/path-variable detection |

## Public Example

The intended public example is the external public thesis repository [`glassiddo/masters_thesis_thailand`](https://github.com/glassiddo/masters_thesis_thailand). Clone or check out that repository separately, then run:

```sh
Rscript r_dep_analyzer/run_analysis.R path/to/masters_thesis_thailand
```

Then inspect:

```text
path/to/masters_thesis_thailand/_dependency_analysis/dependency_graph.html
path/to/masters_thesis_thailand/_dependency_analysis/dependency_graph.json
path/to/masters_thesis_thailand/_dependency_analysis/nodes.csv
path/to/masters_thesis_thailand/_dependency_analysis/edges.csv
path/to/masters_thesis_thailand/_dependency_analysis/issues.csv
path/to/masters_thesis_thailand/_dependency_analysis/agent_context.md
```

This repository also includes a minimal built-in smoke-test fixture:

```sh
Rscript r_dep_analyzer/run_analysis.R r_dep_analyzer/tests/fixtures/minimal_project
```

No private `conflict/`, `migration/`, or local `thesis/` checkout is part of this repository.

## Tests

The tracked fixture project is under `r_dep_analyzer/tests/fixtures/minimal_project/`. Run:

```sh
Rscript r_dep_analyzer/tests/run_tests.R
```

The tests run the analyzer against the fixture and verify output files, JSON/CSV graph content, confidence/provenance fields, issue scoping, archived-folder handling, and the `agent_context.md` summary sections.

## Configuration

Configuration is optional. The active fields currently used by `r_dep_analyzer/config.yaml` are:

```yaml
setup_files: []
master_files: []
exclude_dirs: [Archive, archive, archives, old, Old, backup, Backup]
include_dirs: []
roots: []
graph:
  roots: []
  show_archived: false
  show_low_confidence_edges: true
  show_meta: false
  show_independent: false
  exclude: []
  meta_patterns: []
```

`setup_files` and `master_files` can pin known setup/orchestration files. `exclude_dirs` marks archive/old/backup folders as archived in the graph; those files are still scanned, but are hidden from the default active graph and scoped separately in issues. `include_dirs` can override archive classification for specific directories. `roots` or `graph.roots` provide hypothesized entry points. `graph.show_*`, `graph.exclude`, and `graph.meta_patterns` affect the rendered HTML graph.

The `scan`, `data`, and `output` sections in the bundled config are mostly defaults/legacy scaffolding. They are retained for compatibility but are not a full public API.

## Confidence And Issue Scope

High-confidence edges come from direct resolved `source()`/Stata `do` calls, direct concrete data reads/writes, or exact normalized writer/read matches. Medium confidence usually means path variables or folder-level inference were involved. Low confidence means unresolved variables, basename-only matches, dynamic paths, or otherwise ambiguous static inference.

`issues.csv` and `dependency_graph.json` include a `scope` field:

- `active`: affects the default active graph.
- `archived`: comes from archived/old/backup code.
- `global`: project-wide uncertainty, such as duplicate dataset names.

## Limitations

This is a static heuristic tool. It does not execute the project. Dynamic paths, complex wrappers, runtime-generated filenames, nonstandard Stata/R patterns, and custom pipeline frameworks can be missed or assigned low confidence. Execution order is a best-effort hypothesis, not proof of the runnable pipeline. Treat the output as a map for review, not as a build system.

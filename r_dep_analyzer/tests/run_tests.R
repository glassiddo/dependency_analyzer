#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (length(x) == 0 || is.null(x)) y else x

fail <- function(...) stop(paste0(...), call. = FALSE)
assert <- function(ok, msg) if (!isTRUE(ok)) fail(msg)

script_dir <- {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
}
analyzer_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
repo_root <- normalizePath(file.path(analyzer_dir, ".."), mustWork = TRUE)
fixture <- normalizePath(file.path(script_dir, "fixtures", "minimal_project"), mustWork = TRUE)
out_dir <- file.path(fixture, "_dependency_analysis")
if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE, force = TRUE)

rs <- Sys.which("Rscript")
if (!nzchar(rs)) fail("Rscript was not found on PATH.")

cmd_out <- character(0)
status <- tryCatch({
  cmd_out <- system2(rs, c(file.path(analyzer_dir, "run_analysis.R"), fixture), stdout = TRUE, stderr = TRUE)
  attr(cmd_out, "status") %||% 0L
}, error = function(e) {
  cmd_out <<- conditionMessage(e)
  1L
})
if (!identical(as.integer(status), 0L)) {
  writeLines(cmd_out)
  fail("Analyzer exited nonzero: ", status)
}

expected_files <- file.path(out_dir, c(
  "dependency_graph.html",
  "dependency_graph.json",
  "nodes.csv",
  "edges.csv",
  "issues.csv",
  "agent_context.md",
  "path_setup.txt"
))
missing_files <- expected_files[!file.exists(expected_files)]
assert(length(missing_files) == 0, paste("Missing expected output files:", paste(basename(missing_files), collapse = ", ")))

if (!requireNamespace("jsonlite", quietly = TRUE)) fail("jsonlite is required for tests. Install with install.packages(c(\"dplyr\", \"jsonlite\"))")
artifact <- jsonlite::fromJSON(file.path(out_dir, "dependency_graph.json"), simplifyVector = TRUE)
nodes <- artifact$nodes
edges <- artifact$edges
issues <- artifact$issues

node_ids <- nodes$id
assert("master.R" %in% node_ids, "master.R node not found")
assert("setup.R" %in% node_ids, "setup.R node not found")
assert("scripts/01_prepare.R" %in% node_ids, "scripts/01_prepare.R node not found")
assert("scripts/stata_step.do" %in% node_ids, "Stata script node not found")
assert("data/derived/clean.csv" %in% node_ids, "clean.csv data node not found")

prep_edge <- edges[edges$from == "scripts/01_prepare.R" & edges$to == "data/derived/clean.csv", , drop = FALSE]
assert(nrow(prep_edge) >= 1, "Expected write edge from 01_prepare.R to clean.csv")
model_edge <- edges[edges$from == "data/derived/clean.csv" & edges$to == "scripts/02_model.R", , drop = FALSE]
assert(nrow(model_edge) >= 1, "Expected read edge from clean.csv to 02_model.R")
assert("confidence" %in% names(edges) && "provenance" %in% names(edges), "edges missing confidence/provenance columns")
assert(any(nzchar(edges$confidence)), "No edge confidence values found")
assert(any(nzchar(edges$provenance)), "No edge provenance values found")

source_edge <- edges[edges$from == "master.R" & edges$to == "scripts/01_prepare.R", , drop = FALSE]
assert(nrow(source_edge) >= 1, "Expected source edge master.R -> scripts/01_prepare.R")

archived <- nodes[nodes$id == "archive/old_script.R", , drop = FALSE]
assert(nrow(archived) == 1 && identical(archived$role[[1]], "archived"), "archive/old_script.R should be marked archived")
assert(isFALSE(archived$included[[1]]), "archive/old_script.R should not be included in the active graph")

assert(nrow(issues) >= 1, "Expected at least one issue")
assert(any(issues$type == "missing_source" & issues$scope == "active" & grepl("missing_script", issues$to)), "Expected active missing_source issue")
assert(any(issues$type == "missing_source" & issues$scope == "archived" & grepl("missing_old", issues$to)), "Expected archived missing_source issue")

agent_context <- paste(readLines(file.path(out_dir, "agent_context.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (section in c("## Summary", "## Entry Points", "## Setup / Path Variables", "## Active Issues", "## Files For Downstream Use")) {
  assert(grepl(section, agent_context, fixed = TRUE), paste("agent_context.md missing section:", section))
}
master_pos <- regexpr("1. `master.R`", agent_context, fixed = TRUE)[[1]]
prepare_pos <- regexpr("`scripts/01_prepare.R`", agent_context, fixed = TRUE)[[1]]
model_pos <- regexpr("`scripts/02_model.R`", agent_context, fixed = TRUE)[[1]]
assert(master_pos > 0, "agent_context.md should list master.R first in active dependency shape")
assert(prepare_pos > master_pos, "agent_context.md should list sourced prepare script after master.R")
assert(model_pos > prepare_pos, "agent_context.md should list model script after prepare script")

path_setup <- paste(readLines(file.path(out_dir, "path_setup.txt"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
assert(grepl("setup_path_vars", path_setup, fixed = TRUE), "path_setup.txt missing setup path variables section")

message("All r_dep_analyzer fixture tests passed.")

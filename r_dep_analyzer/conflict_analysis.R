#!/usr/bin/env Rscript

# Minimal "conflict analysis" check for this repository.
#
# The repo includes a sample project under ../conflict. This script runs the analyzer
# against that project and sanity-checks that the rendered graph defaults match
# the intended UX (meta files + independent scripts hidden by default, but still present
# in the underlying data to toggle back in).

args <- commandArgs(trailingOnly = TRUE)
project_path <- if (length(args) >= 1 && nzchar(trimws(args[[1]]))) args[[1]] else NULL

script_dir <- {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  fileArg <- grep("^--file=", cmdArgs, value = TRUE)
  if (length(fileArg) > 0) dirname(normalizePath(sub("^--file=", "", fileArg))) else getwd()
}

repo_root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
if (is.null(project_path)) {
  project_path <- file.path(repo_root, "conflict")
}
project_path <- normalizePath(project_path, mustWork = FALSE)
if (!dir.exists(project_path)) stop("Project path does not exist: ", project_path)

run_script <- file.path(script_dir, "run_analysis.R")
if (!file.exists(run_script)) stop("Could not find run_analysis.R at: ", run_script)

message("Running analyzer on: ", project_path)
rs <- Sys.which("Rscript")
if (!nzchar(rs)) {
  # Fallback to standard Windows install locations.
  cands <- c(
    "C:/Program Files/R/R-4.5.1/bin/Rscript.exe",
    "C:/Program Files/R/R-4.4.0/bin/Rscript.exe"
  )
  rs <- cands[file.exists(cands)][1] %||% ""
}
if (!nzchar(rs)) stop("Could not find Rscript on PATH or in Program Files.")

cmd_out <- tryCatch(system2(rs, c(run_script, project_path), stdout = TRUE, stderr = TRUE), error = function(e) character(0))
if (length(cmd_out) > 0) {
  message(paste(cmd_out, collapse = "\n"))
}

out_html <- file.path(project_path, "_dependency_analysis", "dependency_graph.html")
if (!file.exists(out_html)) stop("Expected output not found: ", out_html)

html <- tryCatch(paste(readLines(out_html, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), error = function(e) "")
if (!nzchar(html)) stop("Could not read output HTML: ", out_html)

must_have <- c(
  "var hideMeta = true;",
  "var hideIndependent = true;"
)
missing <- must_have[!vapply(must_have, function(s) grepl(s, html, fixed = TRUE), logical(1))]
if (length(missing) > 0) {
  stop("Conflict analysis failed: expected defaults not found in HTML: ", paste(missing, collapse = ", "))
}

meta_count <- length(regmatches(html, gregexpr("\"meta\":true", html, fixed = TRUE))[[1]])
data_count <- length(regmatches(html, gregexpr("\"kind\":\"data\"", html, fixed = TRUE))[[1]])
indep_hint <- grepl("toggleIndependentScripts", html, fixed = TRUE)
meta_hint <- grepl("toggleMetaFiles", html, fixed = TRUE)
if (!indep_hint || !meta_hint) stop("Conflict analysis failed: expected toggle functions missing from HTML.")

message("Conflict analysis OK.")
message("  HTML: ", out_html)
message("  meta nodes flagged in data: ", meta_count)
message("  data nodes present in data: ", data_count)

#!/usr/bin/env Rscript
# R Dependency Analyzer - Main Entry Point
#
# From terminal: Rscript run_analysis.R /path/to/your/research/project
#
# From R/RStudio:
#   setwd("/path/to/r_dep_analyzer")
#   source("run_analysis.R")   # will prompt for project path
#   # Or set path first:
#   Sys.setenv(R_DEP_PROJECT_PATH = "C:/path/to/your/project")
#   source("run_analysis.R")

# -----------------------------------------------------------------------------
# Default data directory (used when R_DEP_DATA_PATH is not set).
# Change this to your project's data root so the scanner can find referenced
# datasets. Leave "" to use only the project's own data folder (e.g. project/data).
# -----------------------------------------------------------------------------
default_data_path <- ""

args <- commandArgs(trailingOnly = TRUE)
interactive_mode <- length(args) < 1
if (interactive_mode) {
  project_path <- trimws(Sys.getenv("R_DEP_PROJECT_PATH", ""))
  if (!nzchar(project_path)) {
    project_path <- trimws(readline("Project path: "))
  }
  if (!nzchar(project_path)) {
    message("No project path provided. Use: Rscript run_analysis.R /path/to/project [exclude_folders]")
    message("Or: Sys.setenv(R_DEP_PROJECT_PATH='C:/path/to/project'); source('run_analysis.R')")
    stop("Project path is required.")
  }
} else {
  project_path <- args[1]
}
exclude_folders <- if (length(args) >= 2 && nzchar(trimws(args[2]))) {
  trimws(strsplit(trimws(args[2]), "[,;]+")[[1]])
} else {
  env_excl <- trimws(Sys.getenv("R_DEP_EXCLUDE_FOLDERS", ""))
  if (nzchar(env_excl)) trimws(strsplit(env_excl, "[,;]+")[[1]]) else character(0)
}

project_path <- normalizePath(project_path, mustWork = FALSE)
if (!dir.exists(project_path)) {
  stop("Project path does not exist: ", project_path)
}

# Optional: prompt for folders to exclude when interactive
if (interactive() && interactive_mode && length(exclude_folders) == 0) {
  all_dirs <- list.dirs(project_path, recursive = TRUE, full.names = FALSE)
  all_dirs <- all_dirs[nzchar(all_dirs)]
  path_segments <- unique(unlist(strsplit(gsub("\\\\", "/", all_dirs), "/")))
  common_excl <- c("Archive", "archive", "archives", "LSMS", "Old", "Backup", "backup")
  suggested <- intersect(path_segments, common_excl)
  if (length(suggested) > 0) {
    message("Found folders you may want to exclude: ", paste(suggested, collapse = ", "))
  }
  excl_prompt <- trimws(readline("Folders to exclude (comma-separated, or Enter to skip): "))
  if (nzchar(excl_prompt)) {
    exclude_folders <- trimws(strsplit(excl_prompt, "[,;]+")[[1]])
    exclude_folders <- exclude_folders[nzchar(exclude_folders)]
  }
  project_name <- trimws(Sys.getenv("R_DEP_PROJECT_NAME", ""))
  data_path <- trimws(Sys.getenv("R_DEP_DATA_PATH", ""))
} else {
  project_name <- trimws(Sys.getenv("R_DEP_PROJECT_NAME", ""))
  data_path <- trimws(Sys.getenv("R_DEP_DATA_PATH", ""))
}
if (!nzchar(project_name)) project_name <- basename(project_path)
if (length(data_path) == 0 || !nzchar(data_path)) {
  data_path <- trimws(default_data_path)
  if (!nzchar(data_path)) data_path <- character(0) else data_path <- normalizePath(data_path, mustWork = FALSE)
} else {
  data_path <- normalizePath(data_path, mustWork = FALSE)
}
# Data path is read-only: used only to locate files and determine directory structure; the tool never modifies any data files.

# Tool lives in a different folder - find R scripts
cmdArgs <- commandArgs(trailingOnly = FALSE)
fileArg <- grep("^--file=", cmdArgs, value = TRUE)
if (length(fileArg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", fileArg)))
} else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  # Sourced from RStudio - use path of script being sourced
  ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  if (!is.null(ctx) && nzchar(ctx$path)) {
    script_dir <- dirname(ctx$path)
  } else {
    script_dir <- getwd()
  }
} else {
  script_dir <- getwd()
}
r_dir <- file.path(script_dir, "R")
if (!dir.exists(r_dir)) {
  r_dir <- file.path(script_dir, "r_dep_analyzer", "R")
}
if (!dir.exists(r_dir)) {
  r_dir <- file.path(getwd(), "r_dep_analyzer", "R")
}

# Source modules
source(file.path(r_dir, "parse_r_files.R"))
source(file.path(r_dir, "inspect_data.R"))
source(file.path(r_dir, "build_graph.R"))
source(file.path(r_dir, "detect_issues.R"))
source(file.path(r_dir, "generate_outputs.R"))

message("Scanning project: ", project_path)

# 1. Scan data directory (which folders/files exist; used to validate paths and plan dataset index only; read-only)
data_scan <- scan_data_directory(project_path, data_path)
message("  Data directory: ", length(data_scan$existing_paths_norm), " files in ", length(data_scan$top_level_folders), " top-level folder(s)")

# 2. Parse R files
parsed <- parse_r_project(project_path, exclude_folders = exclude_folders)
if (length(exclude_folders) > 0) {
  message("  Excluding folders: ", paste(exclude_folders, collapse = ", "))
}
n_r <- sum(vapply(parsed$files, function(f) is.null(f$file_type) || f$file_type != "stata", logical(1)))
n_stata <- sum(vapply(parsed$files, function(f) identical(f$file_type, "stata"), logical(1)))
if (n_stata > 0) {
  message("  Found ", n_r, " R file(s), ", n_stata, " Stata file(s)")
} else {
  message("  Found ", n_r, " R file(s)")
}

# 3. Inspect data files (metadata only; read-only, never modified)
data_info <- inspect_referenced_data(parsed, project_path, data_path = data_path, data_scan = data_scan)
message("  Referenced datasets: ", nrow(data_info$index))

# 4. Build dependency graph
graph <- build_dependency_graph(parsed, data_info, project_path, data_path = data_path)
message("  Graph: ", length(graph$nodes$id), " nodes, ", nrow(graph$edges), " edges")

# 5. Detect issues
issues <- detect_issues(graph, parsed, data_info, project_path)
message("  Issues found: ", nrow(issues))

# 6. Generate outputs (inside project's _dependency_analysis folder)
out_dir <- file.path(project_path, "_dependency_analysis")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

if (nzchar(Sys.getenv("R_DEP_MASTER_SUMMARY", "0")) && Sys.getenv("R_DEP_MASTER_SUMMARY") %in% c("1", "true", "TRUE")) {
  generate_master_summary(graph, issues, parsed, data_info, project_path, out_dir)
  message("  Wrote: ", file.path(out_dir, "master_summary.md"))
}

generate_visualization(graph, issues, parsed, data_info, project_path, out_dir, project_name = project_name)
message("  Wrote: ", file.path(out_dir, "dependency_graph.html"))

message("Done. Open ", file.path(out_dir, "dependency_graph.html"), " in a browser.")

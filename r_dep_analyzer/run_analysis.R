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

`%||%` <- function(x, y) if (length(x) == 0 || is.null(x)) y else x

args <- commandArgs(trailingOnly = TRUE)

print_usage <- function() {
  message("Usage:")
  message("  Rscript run_analysis.R <project_path> [exclude_folders]")
  message("  Rscript run_analysis.R --config <config.yaml> <project_path>")
  message("  Rscript run_analysis.R <project_path> --roots <file1,file2,...>")
  message("Options:")
  message("  --config <path>            Path to config.yaml (optional)")
  message("  --data-path <path>         Optional external data root to scan/read for metadata")
  message("  --roots <csv>              Comma-separated root files (relative to project)")
  message("  --root <path>              Root file (repeatable)")
  message("  --from <csv>               Alias for --roots")
  message("  --show-meta                Include meta/setup/master files in rendered graph")
  message("  --show-independent         Include independent scripts in rendered graph")
  message("  --exclude <csv>            Glob patterns to exclude from rendered graph")
  message("  --meta-pattern <glob>      Add a meta glob pattern (repeatable)")
  message("  --help                     Show this help")
  invisible(NULL)
}

parse_cli_args <- function(args) {
  out <- list(
    config_path = trimws(Sys.getenv("R_DEP_CONFIG_PATH", "")),
    project_path = NULL,
    data_path = trimws(Sys.getenv("R_DEP_DATA_PATH", "")),
    exclude_folders = NULL,
    roots = character(0),
    show_meta = NA,
    show_independent = NA,
    exclude_patterns = character(0),
    meta_patterns_add = character(0),
    help = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (a %in% c("--help", "-h")) { out$help <- TRUE; i <- i + 1L; next }
    if (a == "--config" && i + 1L <= length(args)) { out$config_path <- args[[i + 1L]]; i <- i + 2L; next }
    if (a == "--data-path" && i + 1L <= length(args)) { out$data_path <- args[[i + 1L]]; i <- i + 2L; next }
    if (a %in% c("--roots", "--from") && i + 1L <= length(args)) {
      out$roots <- c(out$roots, trimws(strsplit(args[[i + 1L]], "[,;]+")[[1]]))
      i <- i + 2L; next
    }
    if (a == "--root" && i + 1L <= length(args)) { out$roots <- c(out$roots, args[[i + 1L]]); i <- i + 2L; next }
    if (a == "--show-meta") { out$show_meta <- TRUE; i <- i + 1L; next }
    if (a == "--show-independent") { out$show_independent <- TRUE; i <- i + 1L; next }
    if (a == "--exclude" && i + 1L <= length(args)) {
      out$exclude_patterns <- c(out$exclude_patterns, trimws(strsplit(args[[i + 1L]], "[,;]+")[[1]]))
      i <- i + 2L; next
    }
    if (a == "--meta-pattern" && i + 1L <= length(args)) { out$meta_patterns_add <- c(out$meta_patterns_add, args[[i + 1L]]); i <- i + 2L; next }

    # Positional handling
    if (is.null(out$project_path) && !startsWith(a, "--")) {
      out$project_path <- a
      i <- i + 1L
      next
    }
    if (is.null(out$exclude_folders) && !startsWith(a, "--")) {
      out$exclude_folders <- a
      i <- i + 1L
      next
    }
    i <- i + 1L
  }

  # Env roots (only if none provided via args)
  env_roots <- trimws(Sys.getenv("R_DEP_ROOTS", ""))
  if (length(out$roots) == 0 && nzchar(env_roots)) out$roots <- trimws(strsplit(env_roots, "[,;]+")[[1]])
  env_show_meta <- trimws(Sys.getenv("R_DEP_SHOW_META", ""))
  if (is.na(out$show_meta) && nzchar(env_show_meta)) out$show_meta <- tolower(env_show_meta) %in% c("1", "true", "yes", "y", "on")
  env_show_indep <- trimws(Sys.getenv("R_DEP_SHOW_INDEPENDENT", ""))
  if (is.na(out$show_independent) && nzchar(env_show_indep)) out$show_independent <- tolower(env_show_indep) %in% c("1", "true", "yes", "y", "on")

  out$roots <- out$roots[nzchar(trimws(out$roots))]
  out$exclude_patterns <- out$exclude_patterns[nzchar(trimws(out$exclude_patterns))]
  out$meta_patterns_add <- out$meta_patterns_add[nzchar(trimws(out$meta_patterns_add))]

  out
}

cli <- parse_cli_args(args)
if (isTRUE(cli$help)) {
  print_usage()
  quit(save = "no", status = 0)
}

check_required_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) return(invisible(TRUE))
  stop(
    "Missing required R package(s): ", paste(missing, collapse = ", "), "\n",
    "Install them with:\n",
    "  install.packages(c(\"dplyr\", \"jsonlite\"))\n",
    call. = FALSE
  )
}

check_required_packages(c("dplyr", "jsonlite"))

interactive_mode <- is.null(cli$project_path) || !nzchar(trimws(cli$project_path))
if (interactive_mode) {
  project_path <- trimws(Sys.getenv("R_DEP_PROJECT_PATH", ""))
  if (!nzchar(project_path)) project_path <- trimws(readline("Project path: "))
  if (!nzchar(project_path)) {
    message("No project path provided.")
    print_usage()
    stop("Project path is required.")
  }
} else {
  project_path <- cli$project_path
}
exclude_folders <- if (!is.null(cli$exclude_folders) && nzchar(trimws(cli$exclude_folders))) {
  trimws(strsplit(trimws(cli$exclude_folders), "[,;]+")[[1]])
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
  data_path <- trimws(cli$data_path %||% "")
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
source(file.path(r_dir, "config.R"))

to_rel_path <- function(project_path, p) {
  if (!nzchar(p)) return("")
  pp <- gsub("\\\\", "/", normalizePath(project_path, mustWork = FALSE))
  p2 <- gsub("\\\\", "/", p)
  # If already looks relative, keep it.
  if (!dep_is_abs_path(p2)) return(gsub("^\\./", "", p2))
  full <- normalizePath(p2, mustWork = FALSE)
  full <- gsub("\\\\", "/", full)
  if (startsWith(tolower(full), tolower(pp))) {
    rel <- sub(paste0("^", gsub("([\\^\\$\\.|\\(\\)\\[\\]\\\\+\\*\\?\\{\\}])", "\\\\\\1", pp, perl = TRUE), "/?"), "", full, perl = TRUE)
    return(gsub("^\\./", "", rel))
  }
  # Outside project: cannot relativize safely; return as-is (won't match parsed rel paths).
  gsub("^\\./", "", p2)
}

detect_default_roots <- function(graph, parsed) {
  roots <- character(0)
  role_map <- if ("role" %in% names(graph$nodes)) setNames(graph$nodes$role, graph$nodes$id) else character(0)
  if (!is.null(parsed$master_path) && nzchar(parsed$master_path) && !identical(role_map[[parsed$master_path]], "archived")) roots <- c(roots, parsed$master_path)
  if (!is.null(parsed$stata_master_path) && nzchar(parsed$stata_master_path) && !identical(role_map[[parsed$stata_master_path]], "archived")) roots <- c(roots, parsed$stata_master_path)
  roots <- unique(roots[nzchar(roots)])
  if (length(roots) > 0) return(roots)

  included <- if ("included" %in% names(graph$nodes)) graph$nodes$included else rep(TRUE, nrow(graph$nodes))
  role <- if ("role" %in% names(graph$nodes)) graph$nodes$role else rep("unknown", nrow(graph$nodes))
  r_nodes <- graph$nodes$id[graph$nodes$type == "script" & included & role != "archived"]
  dep_edges <- graph$edges[graph$edges$type %in% c("source_run", "sources", "data_flow", "pipeline") &
                             graph$edges$from %in% r_nodes & graph$edges$to %in% r_nodes, , drop = FALSE]
  if (nrow(dep_edges) == 0) return(character(0))
  in_deg <- setNames(rep(0L, length(r_nodes)), r_nodes)
  out_deg <- setNames(rep(0L, length(r_nodes)), r_nodes)
  for (to in dep_edges$to) in_deg[to] <- in_deg[to] + 1L
  for (from in dep_edges$from) out_deg[from] <- out_deg[from] + 1L
  roots <- names(in_deg)[in_deg == 0L & out_deg > 0L]
  if (length(roots) == 0) names(in_deg)[in_deg == 0L] else roots
}

# Config (optional; loaded after we locate script_dir)
config_path <- trimws(cli$config_path %||% "")
if (!nzchar(config_path)) config_path <- file.path(script_dir, "config.yaml")
cfg_raw <- if (file.exists(config_path)) dep_yaml_read(config_path) else list()
migr <- dep_migrate_config_in_memory(cfg_raw)
cfg <- migr$config
if (length(migr$warnings) > 0) message("Config: ", paste(migr$warnings, collapse = " "))

if (length(cli$meta_patterns_add) > 0) cfg$graph$meta_patterns <- unique(c(cfg$graph$meta_patterns %||% character(0), cli$meta_patterns_add))
if (length(cli$exclude_patterns) > 0) cfg$graph$exclude <- unique(c(cfg$graph$exclude %||% character(0), cli$exclude_patterns))
if (!is.na(cli$show_meta)) cfg$graph$show_meta <- isTRUE(cli$show_meta)
if (!is.na(cli$show_independent)) cfg$graph$show_independent <- isTRUE(cli$show_independent)

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
graph <- build_dependency_graph(parsed, data_info, project_path, data_path = data_path, config = cfg)
parsed$setup_master_candidates <- graph$setup_master_candidates
message("  Graph: ", length(graph$nodes$id), " nodes, ", nrow(graph$edges), " edges")

# Roots (explicit > autodetect)
explicit_roots <- character(0)
if (length(cli$roots) > 0) {
  explicit_roots <- vapply(cli$roots, function(p) to_rel_path(project_path, p), character(1))
} else if (!is.null(cfg$graph$roots) && length(cfg$graph$roots) > 0) {
  explicit_roots <- vapply(cfg$graph$roots, function(p) to_rel_path(project_path, p), character(1))
} else if (!is.null(cfg$roots) && length(cfg$roots) > 0) {
  explicit_roots <- vapply(cfg$roots, function(p) to_rel_path(project_path, p), character(1))
}
explicit_roots <- unique(dep_path_norm(explicit_roots[nzchar(explicit_roots)]))
detected_roots <- character(0)
if (length(explicit_roots) > 0) {
  message("  Roots (explicit): ", paste(explicit_roots, collapse = ", "))
} else {
  detected_roots <- detect_default_roots(graph, parsed)
  detected_roots <- unique(dep_path_norm(detected_roots[nzchar(detected_roots)]))
  message("  Roots (detected): ", paste(detected_roots, collapse = ", "))
  message("    Override with: --roots <file1,file2,...> (or set graph.roots in config)")
}

# IMPORTANT UX: only user-provided roots are treated as "roots" for filtering logic.
# Autodetected roots are informational only (displayed in the report and logs).
user_roots <- explicit_roots
roots_display <- if (length(user_roots) > 0) user_roots else detected_roots

viz_options <- list(
  meta_patterns = cfg$graph$meta_patterns %||% character(0),
  exclude_patterns = cfg$graph$exclude %||% character(0),
  show_meta_default = isTRUE(cfg$graph$show_meta %||% FALSE),
  show_independent_default = isTRUE(cfg$graph$show_independent %||% FALSE),
  show_archived = isTRUE(cfg$graph$show_archived %||% FALSE),
  show_low_confidence_edges = isTRUE(cfg$graph$show_low_confidence_edges %||% TRUE)
)

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

write_structured_outputs(
  graph, issues, parsed, data_info, project_path, out_dir,
  project_name = project_name,
  roots = roots_display
)
message("  Wrote: ", file.path(out_dir, "dependency_graph.json"))
message("  Wrote: ", file.path(out_dir, "nodes.csv"))
message("  Wrote: ", file.path(out_dir, "edges.csv"))
message("  Wrote: ", file.path(out_dir, "issues.csv"))
message("  Wrote: ", file.path(out_dir, "agent_context.md"))

generate_visualization(
  graph, issues, parsed, data_info, project_path, out_dir,
  project_name = project_name,
  roots = user_roots,
  roots_display = roots_display,
  viz_options = viz_options
)
message("  Wrote: ", file.path(out_dir, "dependency_graph.html"))

message("Done. Open ", file.path(out_dir, "dependency_graph.html"), " in a browser.")

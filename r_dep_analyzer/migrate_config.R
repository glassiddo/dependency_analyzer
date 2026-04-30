#!/usr/bin/env Rscript

# Migrate an existing r_dep_analyzer config.yaml to the latest schema.
# This keeps old keys intact and adds a `version` plus `graph` section.
#
# Usage:
#   Rscript migrate_config.R path/to/config.yaml
#   Rscript migrate_config.R path/to/config.yaml --in-place
#
# Default behavior: write migrated YAML to stdout.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0 || any(args %in% c("--help", "-h"))) {
  message("Usage: Rscript migrate_config.R <config.yaml> [--in-place]")
  quit(save = "no", status = 2)
}

cfg_path <- args[[1]]
in_place <- any(args %in% "--in-place")
if (!file.exists(cfg_path)) stop("Config file does not exist: ", cfg_path)

script_dir <- {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  fileArg <- grep("^--file=", cmdArgs, value = TRUE)
  if (length(fileArg) > 0) dirname(normalizePath(sub("^--file=", "", fileArg))) else getwd()
}

r_dir <- file.path(script_dir, "R")
if (!dir.exists(r_dir)) r_dir <- file.path(script_dir, "r_dep_analyzer", "R")
if (!dir.exists(r_dir)) stop("Could not locate R/ directory for tool.")

source(file.path(r_dir, "config.R"))

orig <- dep_yaml_read(cfg_path)
migr <- dep_migrate_config_in_memory(orig)
cfg <- migr$config

has_before <- function(path) {
  cur <- orig
  parts <- strsplit(path, "\\.", fixed = FALSE)[[1]]
  for (p in parts) {
    if (is.null(cur) || !is.list(cur) || !(p %in% names(cur))) return(FALSE)
    cur <- cur[[p]]
  }
  TRUE
}

added <- character(0)
for (k in c("version", "graph", "graph.roots", "graph.show_meta", "graph.show_independent", "graph.exclude", "graph.meta_patterns")) {
  if (!has_before(k)) added <- c(added, k)
}

summary_lines <- c(
  paste0("Config migration: ", normalizePath(cfg_path, mustWork = FALSE)),
  if (length(added) > 0) paste0("Added keys: ", paste(added, collapse = ", ")) else "Added keys: (none)",
  if (length(migr$warnings) > 0) paste0("Notes: ", paste(migr$warnings, collapse = " ")) else "Notes: (none)"
)

if (in_place) {
  backup <- paste0(cfg_path, ".bak")
  file.copy(cfg_path, backup, overwrite = TRUE)
  dep_yaml_write(cfg, cfg_path)
  message(paste(summary_lines, collapse = "\n"))
  message("Wrote migrated config in-place; backup: ", backup)
} else {
  message(paste(summary_lines, collapse = "\n"))
  dep_yaml_write(cfg, stdout())
}


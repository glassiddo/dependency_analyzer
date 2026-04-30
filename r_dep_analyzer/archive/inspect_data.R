# Inspect referenced data files: metadata and sample rows. Read-only; never modifies any data files.
# Stata (.dta): detect only (exists, path) - do not read contents.
# If data_path is provided (user's data directory), we try it when resolving paths so we can find files correctly.
#
# NOTE (2026-04): This module is archived and no longer used by the analyzer.
# The tool no longer inspects/opens data files. It now builds a dataset index from
# file references in scripts only (see R/dataset_index.R).

# (Archived) legacy helper kept for reference only.
# Renamed to avoid colliding with the live `scan_data_directory()` in `R/data_scan.R`.
scan_data_directory_archived <- function(project_path, data_path = character(0)) {
  project_path <- gsub("\\\\", "/", normalizePath(project_path, mustWork = TRUE))
  existing_paths_norm <- character(0)
  top_level_folders <- character(0)
  roots_to_scan <- list()
  if (length(data_path) > 0 && nzchar(data_path) && dir.exists(data_path)) {
    data_path_norm <- gsub("\\\\", "/", normalizePath(data_path, mustWork = TRUE))
    roots_to_scan <- list(list(root = data_path_norm, prefix = "data"))
  } else if (dir.exists(file.path(project_path, "data"))) {
    roots_to_scan[[1L]] <- list(root = file.path(project_path, "data"), prefix = "data")
  }
  # Only list paths with data-like extensions (avoids scanning code/docs).
  # Use list.files only - do not call file.info() so we never trigger Dropbox/cloud to download;
  # we only see which paths exist from directory listing.
  data_ext_pat <- "\\.(dta|rds|csv|rda|rdata|xlsx|xls|shp|gpkg|dbf|prj|shx|cpg|txt|sav|parquet)(\\.(gz|zip))?$"
  for (r in roots_to_scan) {
    root <- r$root
    prefix <- r$prefix
    if (!dir.exists(root)) next
    f <- list.files(root, recursive = TRUE, full.names = TRUE, no.. = TRUE,
                    pattern = data_ext_pat, ignore.case = TRUE)
    for (full in f) {
      rel <- sub(paste0("^", gsub("\\\\", "/", root), "/?"), "", gsub("\\\\", "/", full))
      rel <- gsub("^/+", "", rel)
      if (nzchar(prefix)) rel <- paste0(prefix, "/", rel)
      existing_paths_norm <- c(existing_paths_norm, rel)
      segs <- strsplit(rel, "/")[[1]]
      if (length(segs) >= 2 && segs[1] == "data")
        top_level_folders <- c(top_level_folders, segs[2])
      else if (length(segs) >= 1 && nzchar(segs[1]))
        top_level_folders <- c(top_level_folders, segs[1])
    }
  }
  existing_paths_norm <- unique(existing_paths_norm)
  top_level_folders <- unique(tolower(trimws(top_level_folders)))
  list(existing_paths_norm = existing_paths_norm, top_level_folders = top_level_folders)
}

# Resolve a relative data path to a full path: try project_path, then data_path (and data_path + path with "data/" stripped).
# Returns the first path that exists, or project_path + p otherwise. Used only for reading metadata.
# Tries multiple variants to reduce false "missing" when layout differs (e.g. data_path is the data root).
data_file_resolve_path <- function(p, project_path, data_path) {
  p <- gsub("\\\\", "/", trimws(p))
  project_path <- gsub("\\\\", "/", project_path)
  cand <- file.path(project_path, p)
  if (file.exists(cand)) return(cand)
  if (length(data_path) > 0 && nzchar(data_path) && dir.exists(data_path)) {
    data_path <- gsub("\\\\", "/", data_path)
    candidates <- list(
      file.path(data_path, p),
      file.path(data_path, sub("^data/", "", p)),
      file.path(data_path, sub("^data\\\\", "", p))
    )
    # If path looks like data/Final/foo.dta, also try data_path/Final/foo.dta and data_path/foo.dta
    if (grepl("^data/", p, ignore.case = TRUE)) {
      rest <- sub("^data/", "", p, ignore.case = TRUE)
      candidates <- c(candidates, list(
        file.path(data_path, rest),
        file.path(data_path, basename(p))
      ))
    }
    for (cand_n in candidates) {
      if (length(cand_n) > 0 && nzchar(cand_n) && file.exists(cand_n))
        return(cand_n)
    }
  }
  cand
}

inspect_referenced_data <- function(parsed, project_path, data_path = character(0), data_scan = NULL) {
  stop("inspect_referenced_data() is archived and no longer supported. Use build_dataset_index_info() in R/dataset_index.R.")
}

try_inspect_one <- function(full_path, ext) {
  stop("try_inspect_one() is archived and no longer supported.")
}

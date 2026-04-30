# Shared helper: scan a data directory for existence (directory listing only; never opens files).
# This is intentionally opt-in: the scan runs only when the user explicitly provides `data_path`.

scan_data_directory <- function(project_path, data_path = character(0)) {
  project_path <- gsub("\\\\", "/", normalizePath(project_path, mustWork = TRUE))
  existing_paths_norm <- character(0)
  top_level_folders <- character(0)
  roots_to_scan <- list()
  scan_root_norm <- character(0)

  is_non_empty_scalar <- function(x) {
    if (is.null(x) || length(x) == 0) return(FALSE)
    if (is.na(x[1])) return(FALSE)
    nchar(trimws(x[1])) > 0
  }

  # Only scan when the user explicitly provides a data folder path.
  # Do not fall back to project_path/data: dataset indexing is opt-in.
  if (length(data_path) > 0 && is_non_empty_scalar(data_path) && dir.exists(data_path[1])) {
    data_path_norm <- gsub("\\\\", "/", normalizePath(data_path[1], mustWork = TRUE))
    roots_to_scan <- list(list(root = data_path_norm, prefix = "data"))
    scan_root_norm <- data_path_norm
  } else {
    return(list(existing_paths_norm = character(0), top_level_folders = character(0), root = character(0)))
  }

  # Only list paths with data-like extensions (avoids scanning code/docs).
  # Use list.files only - do not call file.info() so we never trigger Dropbox/cloud to download.
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
      if (!is.na(prefix) && nchar(prefix) > 0) rel <- paste0(prefix, "/", rel)
      existing_paths_norm <- c(existing_paths_norm, rel)
      segs <- strsplit(rel, "/")[[1]]
      if (length(segs) >= 2 && segs[1] == "data")
        top_level_folders <- c(top_level_folders, segs[2])
      else if (length(segs) >= 1 && !is.na(segs[1]) && nchar(segs[1]) > 0)
        top_level_folders <- c(top_level_folders, segs[1])
    }
  }

  existing_paths_norm <- unique(existing_paths_norm)
  top_level_folders <- unique(tolower(trimws(top_level_folders)))
  list(existing_paths_norm = existing_paths_norm, top_level_folders = top_level_folders, root = scan_root_norm)
}


# Inspect referenced data files: metadata and sample rows. Read-only; never modifies any data files.
# Stata (.dta): detect only (exists, path) - do not read contents.
# If data_path is provided (user's data directory), we try it when resolving paths so we can find files correctly.

# Scan the data directory to know which files and folders actually exist. Used so we never show
# made-up folders (e.g. "Other") in the dataset index. Read-only; never modifies any files.
# Returns list(existing_paths_norm = character(), top_level_folders = character()).
# existing_paths_norm: normalized project-relative paths (e.g. "data/Raw/foo.csv") for matching.
# top_level_folders: first-level folder names under the data root (e.g. Raw, Build, Final).
scan_data_directory <- function(project_path, data_path = character(0)) {
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
  project_path <- normalizePath(project_path, mustWork = TRUE)
  if (length(data_path) > 0 && nzchar(data_path) && dir.exists(data_path)) {
    data_path <- normalizePath(data_path, mustWork = TRUE)
    data_path <- gsub("\\\\", "/", data_path)
  } else {
    data_path <- character(0)
  }
  all_paths <- character(0)
  from_file <- character(0)
  
  for (rel in names(parsed$files)) {
    f <- parsed$files[[rel]]
    reads <- f$data_reads
    if (is.null(reads) || length(reads) == 0) reads <- character(0)
    writes <- f$data_writes
    if (is.null(writes) || length(writes) == 0) writes <- character(0)
    for (p in c(reads, writes)) {
      p_canon <- normalize_path_canonical(p)
      all_paths <- c(all_paths, p_canon)
      from_file <- c(from_file, rel)
    }
  }
  
  if (length(all_paths) == 0) {
    return(list(
      index = data.frame(
        path = character(0), rel_path = character(0), exists = logical(0),
        format = character(0), nrow = integer(0), ncol = integer(0),
        columns = character(0), sample_preview = character(0),
        from_files = character(0), stringsAsFactors = FALSE
      )
    ))
  }
  
  # Dedupe by path_norm_for_group (Raw/Countries = RAW/COUNTRIES); merge refs
  p_norm <- path_norm_for_group(all_paths)
  by_key <- split(seq_along(all_paths), p_norm)
  keep_idx <- vapply(by_key, function(ii) ii[1], integer(1))
  all_paths <- all_paths[keep_idx]
  refs <- setNames(lapply(by_key, function(ii) unique(from_file[ii])), all_paths)
  
  nrow_ <- integer(length(all_paths))
  ncol_ <- integer(length(all_paths))
  columns_ <- character(length(all_paths))
  sample_ <- character(length(all_paths))
  exists_ <- logical(length(all_paths))
  format_ <- character(length(all_paths))
  from_files_ <- character(length(all_paths))
  
  for (i in seq_along(all_paths)) {
    p <- all_paths[i]
    full_path <- data_file_resolve_path(p, project_path, data_path)
    exists_[i] <- file.exists(full_path)
    ext <- tolower(sub(".*\\.", "", p))
    format_[i] <- ext
    
    from_files_[i] <- paste(unique(refs[[p]]), collapse = ", ")
    
    if (!exists_[i]) {
      nrow_[i] <- NA_integer_
      ncol_[i] <- NA_integer_
      columns_[i] <- ""
      sample_[i] <- "(file not found)"
    } else if (ext %in% c("dta")) {
      # Stata: detect only, no read
      nrow_[i] <- NA_integer_
      ncol_[i] <- NA_integer_
      columns_[i] <- "(Stata file - not read)"
      sample_[i] <- "(Stata file - not read)"
    } else {
      info <- try_inspect_one(full_path, ext)
      nrow_[i] <- info$nrow
      ncol_[i] <- info$ncol
      columns_[i] <- info$columns
      sample_[i] <- info$sample
    }
  }
  
  out <- list(
    index = data.frame(
      path = all_paths,
      rel_path = all_paths,
      exists = exists_,
      format = format_,
      nrow = nrow_,
      ncol = ncol_,
      columns = columns_,
      sample_preview = sample_,
      from_files = from_files_,
      stringsAsFactors = FALSE
    )
  )
  if (!is.null(data_scan)) out$data_scan <- data_scan
  out
}

try_inspect_one <- function(full_path, ext) {
  out <- list(nrow = NA_integer_, ncol = NA_integer_, columns = "", sample = "")
  tryCatch({
    if (ext %in% c("csv", "txt")) {
      d <- utils::read.csv(full_path, nrows = 6, stringsAsFactors = FALSE)
      out$nrow <- tryCatch(nrow(utils::read.csv(full_path)), error = function(e) NA_integer_)
      out$ncol <- ncol(d)
      out$columns <- paste(names(d), collapse = ", ")
      out$sample <- paste(capture.output(print(head(d, 5))), collapse = "\n")
    } else if (ext == "rds") {
      d <- readRDS(full_path)
      if (is.data.frame(d)) {
        out$nrow <- nrow(d)
        out$ncol <- ncol(d)
        out$columns <- paste(names(d), collapse = ", ")
        out$sample <- paste(capture.output(print(head(d, 5))), collapse = "\n")
      } else {
        out$columns <- paste(class(d), collapse = ", ")
        out$sample <- "(not a data frame)"
      }
    } else if (ext %in% c("rda", "rdata")) {
      e <- new.env()
      load(full_path, envir = e)
      objs <- ls(e)
      if (length(objs) == 1) {
        d <- get(objs[1], e)
        if (is.data.frame(d)) {
          out$nrow <- nrow(d)
          out$ncol <- ncol(d)
          out$columns <- paste(names(d), collapse = ", ")
          out$sample <- paste(capture.output(print(head(d, 5))), collapse = "\n")
        }
      }
      out$columns <- if (nzchar(out$columns)) out$columns else paste(objs, collapse = ", ")
    } else if (ext %in% c("xlsx", "xls")) {
      if (requireNamespace("readxl", quietly = TRUE)) {
        d <- readxl::read_excel(full_path, n_max = 6)
        out$nrow <- nrow(readxl::read_excel(full_path))
        out$ncol <- ncol(d)
        out$columns <- paste(names(d), collapse = ", ")
        out$sample <- paste(capture.output(print(head(d, 5))), collapse = "\n")
      } else {
        out$columns <- "(install readxl for Excel support)"
      }
    } else {
      out$columns <- "(format not inspected)"
    }
  }, error = function(e) {
    out$columns <- paste0("(error: ", conditionMessage(e), ")")
  })
  out
}

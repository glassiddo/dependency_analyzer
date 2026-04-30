# Build a dataset index from script references only (no file reads).
# Optional: scan a data directory for existence (directory listing only; does not open files).

if (!exists("scan_data_directory", mode = "function")) {
  this_file <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
  helper <- if (nzchar(this_file)) file.path(dirname(this_file), "data_scan.R") else ""
  if (nzchar(helper) && file.exists(helper)) source(helper)
}

build_dataset_index_info <- function(parsed, project_path, data_path = character(0), scan_for_existence = FALSE) {
  norm <- function(p) path_norm_for_group(canonical_data_id(p))
  is_non_empty_vec <- function(x) !is.na(x) & nchar(x) > 0

  project_path <- normalizePath(project_path, mustWork = TRUE)
  scan <- NULL
  # Dataset indexing is opt-in: only scan if the user provided a data folder.
  has_data_path <- length(data_path) > 0 && !is.null(data_path) && !is.na(data_path[1]) && nzchar(trimws(data_path[1]))
  if (isTRUE(has_data_path) && dir.exists(data_path[1])) {
    scan <- scan_data_directory(project_path, data_path = data_path)
  }

  best_repr <- function(cands) {
    cands <- cands[!is.na(cands) & nzchar(cands)]
    if (length(cands) == 0) return(NA_character_)
    has_dir <- grepl("/", cands, fixed = TRUE)
    if (any(has_dir)) cands[which(has_dir)[1]] else cands[1]
  }

  refs <- dplyr::bind_rows(lapply(names(parsed$files), function(rel) {
    f <- parsed$files[[rel]]
    reads <- f$data_reads %||% character(0)
    writes <- f$data_writes %||% character(0)

    read_tbl <- if (length(reads) > 0) {
      k <- norm(reads)
      ok <- is_non_empty_vec(k)
      if (any(ok)) {
        data.frame(
          kind = rep("read", sum(ok)),
          k = k[ok],
          rel = rep(rel, sum(ok)),
          repr = vapply(reads[ok], function(p) canonical_data_id(p)[1], character(1)),
          stringsAsFactors = FALSE
        )
      } else NULL
    } else NULL

    write_tbl <- if (length(writes) > 0) {
      k <- norm(writes)
      ok <- is_non_empty_vec(k)
      if (any(ok)) {
        data.frame(
          kind = rep("write", sum(ok)),
          k = k[ok],
          rel = rep(rel, sum(ok)),
          repr = vapply(writes[ok], function(p) canonical_data_id(p)[1], character(1)),
          stringsAsFactors = FALSE
        )
      } else NULL
    } else NULL

    dplyr::bind_rows(read_tbl, write_tbl)
  })) |>
    dplyr::mutate(
      kind = as.character(.data$kind),
      k = as.character(.data$k),
      rel = as.character(.data$rel),
      repr = as.character(.data$repr)
    )

  if (nrow(refs) == 0) {
    return(list(
      index = data.frame(
        path = character(0),
        format = character(0),
        exists = logical(0),
        readers = character(0),
        writers = character(0),
        stringsAsFactors = FALSE
      ),
      basename_resolution = character(0),
      data_scan = scan
    ))
  }

  readers_map <- refs |>
    dplyr::filter(.data$kind == "read") |>
    dplyr::group_by(.data$k) |>
    dplyr::summarise(
      readers = paste(unique(.data$rel), collapse = ", "),
      .groups = "drop"
    )

  writers_map <- refs |>
    dplyr::filter(.data$kind == "write") |>
    dplyr::group_by(.data$k) |>
    dplyr::summarise(
      writers = paste(unique(.data$rel), collapse = ", "),
      .groups = "drop"
    )

  repr_map <- refs |>
    dplyr::group_by(.data$k) |>
    dplyr::summarise(repr = best_repr(.data$repr), .groups = "drop")

  idx_long <- dplyr::full_join(readers_map, writers_map, by = "k") |>
    dplyr::left_join(repr_map, by = "k") |>
    dplyr::mutate(
      readers = dplyr::coalesce(.data$readers, ""),
      writers = dplyr::coalesce(.data$writers, "")
    ) |>
    dplyr::select("k", "repr", "readers", "writers")

  idx_long <- idx_long[order(idx_long$k), , drop = FALSE]

  keys <- idx_long$k
  if (length(keys) == 0) {
    return(list(
      index = data.frame(
        path = character(0),
        format = character(0),
        exists = logical(0),
        readers = character(0),
        writers = character(0),
        stringsAsFactors = FALSE
      ),
      basename_resolution = character(0),
      data_scan = scan
    ))
  }

  paths <- idx_long$repr
  paths[is.na(paths)] <- vapply(keys[is.na(paths)], function(k) canonical_data_id(k)[1], character(1))
  formats <- tolower(sub(".*\\.", "", paths))

  exists <- rep(NA, length(paths))
  if (!is.null(scan) && length(scan$existing_paths_norm) > 0) {
    scan_norm <- unique(path_norm_for_group(scan$existing_paths_norm))
    exists <- norm(paths) %in% scan_norm
  }

  readers <- idx_long$readers
  writers <- idx_long$writers

  idx <- data.frame(
    path = paths,
    format = formats,
    exists = exists,
    readers = readers,
    writers = writers,
    stringsAsFactors = FALSE
  )

  # Basename resolution map:
  # - Prefer scanned data folder when available (prevents "hallucinated" linking).
  # - Fallback to referenced datasets when no scan is available.
  base_resolution <- character(0)
  if (!is.null(scan) && length(scan$existing_paths_norm) > 0) {
    scan_paths <- canonical_data_id(scan$existing_paths_norm)
    scan_paths <- unique(scan_paths[nzchar(scan_paths)])
    base <- tolower(basename(scan_paths))
    has_dir <- grepl("/", scan_paths, fixed = TRUE)
    base_to_full <- split(scan_paths[has_dir], base[has_dir])
    for (b in names(base_to_full)) {
      cands <- unique(base_to_full[[b]])
      if (length(cands) == 1 && !is.na(cands[1]) && nchar(cands[1]) > 0) base_resolution[b] <- cands
    }
  } else {
    base <- tolower(basename(idx$path))
    has_dir <- grepl("/", idx$path, fixed = TRUE)
    base_to_full <- split(idx$path[has_dir], base[has_dir])
    for (b in names(base_to_full)) {
      cands <- unique(base_to_full[[b]])
      if (length(cands) == 1 && !is.na(cands[1]) && nchar(cands[1]) > 0) base_resolution[b] <- cands
    }
  }

  # Scan-driven dataset index (physical directory structure); only produced when scan is available.
  dataset_index <- NULL
  if (!is.null(scan) && length(scan$existing_paths_norm) > 0) {
    scan_paths <- unique(canonical_data_id(scan$existing_paths_norm))
    scan_paths <- scan_paths[nzchar(scan_paths)]

    scan_norm <- unique(path_norm_for_group(scan_paths))
    scan_repr <- vapply(scan_norm, function(k) scan_paths[path_norm_for_group(scan_paths) == k][1], character(1))
    scan_norm <- names(scan_repr)
    scan_paths_u <- unname(scan_repr)

    base_key <- tolower(basename(scan_paths_u))
    base_to_norm <- split(scan_norm, base_key)
    base_to_norm <- base_to_norm[vapply(base_to_norm, length, integer(1)) == 1]
    base_to_norm <- vapply(base_to_norm, function(x) x[1], character(1))

    readers_by_scan <- setNames(vector("list", length(scan_norm)), scan_norm)
    writers_by_scan <- setNames(vector("list", length(scan_norm)), scan_norm)

    resolve_scan_key <- function(p) {
      p_norm <- path_norm_for_group(canonical_data_id(p))
      if (p_norm %in% scan_norm) return(p_norm)
      b <- tolower(basename(p_norm))
      if (b %in% names(base_to_norm)) return(base_to_norm[[b]])
      NA_character_
    }

    for (rel in names(parsed$files)) {
      f <- parsed$files[[rel]]
      reads <- f$data_reads %||% character(0)
      writes <- f$data_writes %||% character(0)

      for (p in reads) {
        k <- resolve_scan_key(p)
        if (!is.na(k) && nzchar(k)) readers_by_scan[[k]] <- unique(c(readers_by_scan[[k]], rel))
      }
      for (p in writes) {
        k <- resolve_scan_key(p)
        if (!is.na(k) && nzchar(k)) writers_by_scan[[k]] <- unique(c(writers_by_scan[[k]], rel))
      }
    }

    dataset_index <- data.frame(
      path = scan_paths_u,
      format = tolower(sub(".*\\.", "", scan_paths_u)),
      readers = vapply(scan_norm, function(k) paste(readers_by_scan[[k]] %||% character(0), collapse = ", "), character(1)),
      writers = vapply(scan_norm, function(k) paste(writers_by_scan[[k]] %||% character(0), collapse = ", "), character(1)),
      stringsAsFactors = FALSE
    )
    dataset_index <- dataset_index[order(tolower(dataset_index$path), dataset_index$path), , drop = FALSE]
  }

  list(index = idx, basename_resolution = base_resolution, data_scan = scan, dataset_index = dataset_index)
}

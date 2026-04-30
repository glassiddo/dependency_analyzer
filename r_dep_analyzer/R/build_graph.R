# Build dependency graph from parsed R files and data index.

# Display label for data paths. Bare filenames (no directory) are shown as-is; no project-specific path assumed.
data_path_display_label <- function(p) {
  if (!nzchar(p)) return(p)
  gsub("\\\\", "/", trimws(p))
}

# Returns TRUE when the first path segment looks like an unresolved variable name (e.g. int_dir, mines_dir).
# These paths cannot be reliably used for data-flow matching when no filesystem scan is available.
has_unresolved_prefix <- function(p) {
  if (!grepl("/", p, fixed = TRUE)) return(FALSE)
  first_seg <- strsplit(p, "/", fixed = TRUE)[[1]][1]
  grepl("^[a-z_]+_dir$|^[a-z]+\\.dir$", first_seg, ignore.case = TRUE)
}

build_dependency_graph <- function(parsed, data_info, project_path, data_path = character(0)) {
  project_path <- normalizePath(project_path, mustWork = TRUE)
  if (length(data_path) > 0 && nzchar(data_path)) data_path <- normalizePath(data_path, mustWork = FALSE)

  r_ids <- names(parsed$files)

  # If the user provided a data folder for scanning, only link scripts to datasets that
  # match a file found during that scan (no hallucinations).
  #
  # IMPORTANT: if the scan exists but found zero files, treat it as "no scan" for
  # graph linking, otherwise we would incorrectly drop all dataset nodes/edges.
  scan_norm_set <- character(0)
  has_scan_restrict <- !is.null(data_info$data_scan) && length(data_info$data_scan$existing_paths_norm) > 0
  has_scan <- has_scan_restrict
  if (isTRUE(has_scan_restrict)) {
    scan_norm_set <- unique(path_norm_for_group(canonical_data_id(data_info$data_scan$existing_paths_norm)))
    scan_norm_set <- scan_norm_set[nzchar(scan_norm_set)]
  }

  resolve_data_id <- function(p) {
    pid <- canonical_data_id(p)
    if (length(pid) == 0) return(pid)
    pid <- pid[1]
    if (!nzchar(pid)) return(pid)
    # Basename resolution is only safe when a filesystem scan is available AND the basename
    # uniquely maps to exactly one scanned path (guaranteed by build_dataset_index_info).
    # Without scan: skip basename resolution entirely to prevent merging distinct datasets.
    if (!grepl("/", pid, fixed = TRUE) && isTRUE(has_scan_restrict) &&
        !is.null(data_info$basename_resolution) && length(data_info$basename_resolution) > 0) {
      key <- tolower(pid)
      if (key %in% names(data_info$basename_resolution) && nzchar(data_info$basename_resolution[[key]])) {
        pid <- data_info$basename_resolution[[key]]
      }
    }
    # When no scan: paths with unresolved variable-like prefixes (e.g. int_dir/foo.rds) are
    # kept as-is for the dataset index but excluded from data-flow edges (see inference below).
    # We do NOT collapse to basename, which can incorrectly merge distinct datasets.
    if (isTRUE(has_scan_restrict)) {
      if (!(path_norm_for_group(pid) %in% scan_norm_set)) return("")
    }
    pid
  }

  nodes_r <- data.frame(
    id = r_ids,
    label = basename(r_ids),
    type = rep("r_file", length(r_ids)),
    stringsAsFactors = FALSE
  )

  data_paths_resolved <- vapply(
    data_info$index$path %||% character(0),
    resolve_data_id,
    character(1)
  )
  data_paths_resolved <- unique(data_paths_resolved[nzchar(data_paths_resolved)])
  nodes_data <- data.frame(
    id = data_paths_resolved,
    label = vapply(data_paths_resolved, data_path_display_label, character(1)),
    type = rep("data", length(data_paths_resolved)),
    stringsAsFactors = FALSE
  )

  nodes <- dplyr::bind_rows(nodes_r, nodes_data) |>
    dplyr::distinct(.data$id, .keep_all = TRUE) |>
    as.data.frame(stringsAsFactors = FALSE)

  resolve_source_id <- function(s, r_ids) {
    s_id <- s
    # Only append .R when path is not already a script extension (.R, .Rmd, .do)
    if (!grepl("\\.(R|r|Rmd|RMD|DO|do)$", s)) s_id <- paste0(s, ".R")
    if (s_id %in% r_ids) return(s_id)

    base_s <- basename(s_id)
    # Case-insensitive match so resolved paths (e.g. from setup vars) match actual filenames on any OS
    match_full <- r_ids[tolower(basename(r_ids)) == tolower(base_s)]
    if (length(match_full) == 1) return(match_full)
    if (length(match_full) > 1) {
      # Prefer same directory (case-insensitive) if possible
      dir_s <- dirname(s_id)
      same_dir <- match_full[tolower(dirname(match_full)) == tolower(dir_s)]
      return(if (length(same_dir) >= 1) same_dir[1] else match_full[1])
    }
    s_id
  }

  edge_rows <- lapply(r_ids, function(rel) {
    f <- parsed$files[[rel]]
    src <- f$sources %||% character(0)
    reads <- f$data_reads %||% character(0)
    writes <- f$data_writes %||% character(0)

    src_edges <- if (length(src) > 0) {
      to_ids <- vapply(src, resolve_source_id, character(1), r_ids = r_ids)
      src_reason <- if (!is.null(f$file_type) && identical(f$file_type, "stata")) {
        note_map <- f$stata_source_notes %||% character(0)
        vapply(seq_along(src), function(i) {
          s <- src[[i]]
          note <- if (length(note_map) > 0 && s %in% names(note_map) && nzchar(note_map[[s]])) paste0(" (", note_map[[s]], ")") else ""
          paste0("do/run/include: ", basename(s), note)
        }, character(1))
      } else {
        paste0("source(\"", basename(src), "\")")
      }
      data.frame(
        from = rep(rel, length(src)),
        to = to_ids,
        type = rep("sources", length(src)),
        reason = src_reason,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }

    read_ids <- if (length(reads) > 0) vapply(reads, resolve_data_id, character(1)) else character(0)
    read_ids <- read_ids[nzchar(read_ids)]
    read_edges <- if (length(read_ids) > 0) {
      r_notes <- f$data_read_notes %||% character(0)
      r_reason <- vapply(read_ids, function(pid) {
        note <- if (length(r_notes) > 0 && pid %in% names(r_notes) && nzchar(r_notes[[pid]])) paste0(" (", r_notes[[pid]], ")") else ""
        paste0("reads: ", data_path_display_label(pid), note)
      }, character(1))
      data.frame(
        from = read_ids,
        to = rep(rel, length(read_ids)),
        type = rep("reads", length(read_ids)),
        reason = r_reason,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }

    write_ids <- if (length(writes) > 0) vapply(writes, resolve_data_id, character(1)) else character(0)
    write_ids <- write_ids[nzchar(write_ids)]
    write_edges <- if (length(write_ids) > 0) {
      w_notes <- f$data_write_notes %||% character(0)
      w_reason <- vapply(write_ids, function(pid) {
        note <- if (length(w_notes) > 0 && pid %in% names(w_notes) && nzchar(w_notes[[pid]])) paste0(" (", w_notes[[pid]], ")") else ""
        paste0("writes: ", data_path_display_label(pid), note)
      }, character(1))
      data.frame(
        from = rep(rel, length(write_ids)),
        to = write_ids,
        type = rep("writes", length(write_ids)),
        reason = w_reason,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }

    dplyr::bind_rows(src_edges, read_edges, write_edges)
  })

  # reason column records why an edge exists (for provenance display in HTML)
  edges <- dplyr::bind_rows(edge_rows) |>
    dplyr::distinct() |>
    as.data.frame(stringsAsFactors = FALSE)

  # Add placeholder nodes for missing/ambiguous sourced scripts so provenance edges still show up.
  missing_scripts <- unique(edges$to[edges$type == "sources" & !(edges$to %in% r_ids)])
  missing_scripts <- missing_scripts[nzchar(missing_scripts)]
  if (length(missing_scripts) > 0) {
    nodes_missing <- data.frame(
      id = missing_scripts,
      label = basename(missing_scripts),
      type = rep("missing_script", length(missing_scripts)),
      stringsAsFactors = FALSE
    )
    nodes <- dplyr::bind_rows(nodes, nodes_missing) |>
      dplyr::distinct(.data$id, .keep_all = TRUE) |>
      as.data.frame(stringsAsFactors = FALSE)
  }

  # ── Data-flow: script that writes a dataset → script that reads it ────────────
  # Only creates script→script edges when the dataset match is reliable.
  # Reliability rules (no scan available):
  #   - Exact normalized-path matching only (no basename fallback)
  #   - Skip reads/writes with unresolved variable-like prefixes (e.g. int_dir/foo.rds)
  # Reliability rules (scan available):
  #   - Exact matching first, then basename matching (only within the scan's unique-basename map)
  #
  # Provenance: each data_flow edge records which dataset canonical ID(s) caused it.
  norm_path <- function(p) path_norm_for_group(canonical_data_id(p))

  # Build a precomputed index of readers so data-flow inference scales well.
  exact_keys <- character(0); exact_scripts <- character(0)
  folder_keys <- character(0); folder_scripts <- character(0)
  base_keys <- character(0); base_scripts <- character(0)

  for (script in r_ids) {
    f <- parsed$files[[script]]
    reads <- unique(f$data_reads %||% character(0))
    folders <- unique(f$data_read_folders %||% character(0))

    if (length(reads) > 0) {
      r_norm <- norm_path(reads)
      ok <- nzchar(r_norm)
      # When no scan: skip reads with unresolved variable prefixes (they can't be reliably matched)
      if (!isTRUE(has_scan)) {
        ok <- ok & !vapply(r_norm, has_unresolved_prefix, logical(1))
      }
      if (any(ok)) {
        r_norm_ok <- r_norm[ok]
        is_bare <- !grepl("/", r_norm_ok, fixed = TRUE)
        # Exact index: only for paths with a directory component
        if (any(!is_bare)) {
          exact_keys <- c(exact_keys, r_norm_ok[!is_bare])
          exact_scripts <- c(exact_scripts, rep(script, sum(!is_bare)))
        }
        # Basename index: only populate when scan is available (prevents false matches across dirs)
        if (isTRUE(has_scan_restrict)) {
          base_keys <- c(base_keys, basename(r_norm_ok))
          base_scripts <- c(base_scripts, rep(script, length(r_norm_ok)))
        }
      }
    }

    if (length(folders) > 0) {
      f_norm <- norm_path(folders)
      f_norm <- sub("/+$", "", f_norm)
      ok <- nzchar(f_norm)
      if (!isTRUE(has_scan)) {
        ok <- ok & !vapply(f_norm, has_unresolved_prefix, logical(1))
      }
      if (any(ok)) {
        folder_keys <- c(folder_keys, f_norm[ok])
        folder_scripts <- c(folder_scripts, rep(script, sum(ok)))
      }
    }
  }

  exact_index <- if (length(exact_keys) > 0) split(exact_scripts, exact_keys) else list()
  folder_index <- if (length(folder_keys) > 0) split(folder_scripts, folder_keys) else list()
  base_index <- if (length(base_keys) > 0) split(base_scripts, base_keys) else list()

  df_from <- character(0); df_to <- character(0)
  # Map (writer|||reader) → character vector of canonical dataset ids that caused the edge
  df_reasons_map <- new.env(parent = emptyenv())

  for (writer in r_ids) {
    f <- parsed$files[[writer]]
    writes <- unique(f$data_writes %||% character(0))
    if (length(writes) == 0) next

    # Skip writes with unresolved prefixes when no scan (can't reliably match)
    if (!isTRUE(has_scan)) {
      w_canon <- canonical_data_id(writes)
      ok_w <- !vapply(w_canon, has_unresolved_prefix, logical(1))
      writes <- writes[ok_w]
    }
    if (length(writes) == 0) next

    w_norms <- norm_path(writes)
    w_norms <- w_norms[nzchar(w_norms)]
    if (length(w_norms) == 0) next

    for (i in seq_along(w_norms)) {
      w_norm <- w_norms[i]
      readers <- character(0)

      ex <- exact_index[[w_norm]]
      if (!is.null(ex)) readers <- c(readers, ex)

      # Basename matching: only when scan is available (prevents merging distinct datasets)
      if (isTRUE(has_scan) && grepl("/", w_norm, fixed = TRUE)) {
        bkey <- basename(w_norm)
        bx <- base_index[[bkey]]
        if (!is.null(bx)) readers <- c(readers, bx)
      }

      # Folder prefix matching via ancestor walk (O(path depth), not O(#folders))
      dir_key <- dirname(w_norm)
      if (!nzchar(dir_key) || dir_key == ".") dir_key <- ""
      if (nzchar(dir_key)) {
        cur <- dir_key
        for (iter in 1:50) {
          fx <- folder_index[[cur]]
          if (!is.null(fx)) readers <- c(readers, fx)
          parent <- dirname(cur)
          if (!nzchar(parent) || parent == "." || parent == cur) break
          cur <- parent
        }
      }

      readers <- unique(readers)
      readers <- setdiff(readers, writer)
      if (length(readers) == 0) next

      for (reader in readers) {
        key <- paste(writer, reader, sep = "|||")
        cur <- if (exists(key, envir = df_reasons_map, inherits = FALSE))
          get(key, envir = df_reasons_map, inherits = FALSE) else character(0)
        assign(key, unique(c(cur, w_norm)), envir = df_reasons_map)
        df_from <- c(df_from, writer)
        df_to <- c(df_to, reader)
      }
    }
  }

  if (length(df_from) > 0) {
    # Deduplicate writer→reader pairs and build reason string from contributing datasets
    pair_key <- paste(df_from, df_to, sep = "|||")
    unique_idx <- !duplicated(pair_key)
    df_from_u <- df_from[unique_idx]
    df_to_u <- df_to[unique_idx]
    pair_key_u <- pair_key[unique_idx]
    reasons_u <- vapply(pair_key_u, function(k) {
      datasets <- if (exists(k, envir = df_reasons_map, inherits = FALSE))
        get(k, envir = df_reasons_map, inherits = FALSE) else character(0)
      if (length(datasets) > 0) paste0("data: ", paste(datasets, collapse = "; ")) else "data flow"
    }, character(1))
    edges <- rbind(edges, data.frame(
      from = unname(df_from_u), to = unname(df_to_u), type = "data_flow",
      reason = unname(reasons_u),
      stringsAsFactors = FALSE
    ))
  }

  edges <- unique(edges) |> as.data.frame(stringsAsFactors = FALSE)

  setup_file <- identify_setup_file(parsed, edges)

  list(nodes = nodes, edges = edges, project_path = project_path, setup_file = setup_file)
}


# Setup = file with no sources that is sourced by the most other files
identify_setup_file <- function(parsed, edges) {
  src_edges <- edges[edges$type == "sources", ]
  if (nrow(src_edges) == 0) return(NULL)
  sourced_count <- table(src_edges$to)
  setup_candidates <- names(parsed$files)[vapply(names(parsed$files), function(f) length(parsed$files[[f]]$sources) == 0, logical(1))]
  if (length(setup_candidates) == 0) return(NULL)
  counts <- vapply(setup_candidates, function(c) {
    v <- sourced_count[c]
    if (is.na(v) || is.null(v)) 0 else as.numeric(v)
  }, numeric(1))
  if (length(counts) == 0 || all(counts < 2, na.rm = TRUE)) return(NULL)
  setup_candidates[which.max(counts)]
}

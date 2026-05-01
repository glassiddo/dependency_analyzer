# Build dependency graph from parsed R/Stata files and data index.

data_path_display_label <- function(p) {
  if (!nzchar(p)) return(p)
  gsub("\\\\", "/", trimws(p))
}

has_unresolved_prefix <- function(p) {
  if (!grepl("/", p, fixed = TRUE)) return(FALSE)
  first_seg <- strsplit(p, "/", fixed = TRUE)[[1]][1]
  grepl("^[a-z_]+_dir$|^[a-z]+\\.dir$|^\\$|^\\$\\{", first_seg, ignore.case = TRUE)
}

archive_dir_defaults <- function() c("Archive", "archive", "archives", "old", "Old", "backup", "Backup")

path_has_segment <- function(path, segments) {
  if (length(segments) == 0) return(FALSE)
  parts <- strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1]]
  any(tolower(parts) %in% tolower(segments))
}

node_folder <- function(x, default = "root") {
  d <- gsub("\\\\", "/", dirname(x))
  if (!nzchar(d) || d == ".") default else d
}

detect_setup_master_candidates <- function(parsed, setup_file = NULL, config = list()) {
  rels <- names(parsed$files)
  archive_dirs <- config$exclude_dirs %||% archive_dir_defaults()
  rows <- list()
  add <- function(path, role, confidence, reason) {
    if (length(path) == 0 || is.null(path) || !nzchar(path)) return()
    rows[[length(rows) + 1L]] <<- data.frame(
      path = path, role = role, confidence = confidence, reason = reason,
      stringsAsFactors = FALSE
    )
  }

  for (p in config$setup_files %||% character(0)) add(p, "setup", "high", "configured setup_files override")
  for (p in config$master_files %||% character(0)) add(p, "master", "high", "configured master_files override")
  add(setup_file, "setup", "high", "sourced by multiple scripts and defines no sources")
  add(parsed$master_path %||% "", "master", "high", "detected master/orchestrator file")
  add(parsed$stata_master_path %||% "", "master", "high", "detected Stata master file")

  named <- parsed$setup_master_files %||% character(0)
  for (rel in named[named %in% rels]) {
    f <- parsed$files[[rel]]
    n_vars <- length(f$local_path_vars %||% character(0))
    n_calls <- length(f$sources %||% character(0))
    role <- if (grepl("master(?![a-z])", basename(rel), ignore.case = TRUE, perl = TRUE)) "master" else "setup"
    conf <- if (n_vars >= 2 || n_calls >= 2) "high" else if (n_vars >= 1 || n_calls >= 1) "medium" else "low"
    reason <- if (conf == "high") "filename plus path variables or orchestration calls" else if (conf == "medium") "filename plus one relevant static signal" else "filename match only"
    add(rel, role, conf, reason)
  }

  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) return(data.frame(
    path = character(0), role = character(0), confidence = character(0),
    reason = character(0), archived = logical(0), stringsAsFactors = FALSE
  ))
  out$archived <- vapply(out$path, path_has_segment, logical(1), segments = archive_dirs)
  hi_archived <- out$archived & out$confidence == "high"
  out$confidence[hi_archived] <- "medium"
  out$reason[out$archived & !grepl("archived path", out$reason, ignore.case = TRUE)] <-
    paste0(out$reason[out$archived & !grepl("archived path", out$reason, ignore.case = TRUE)], "; archived path")
  out |>
    dplyr::arrange(.data$archived, dplyr::desc(factor(.data$confidence, levels = c("low", "medium", "high"))), .data$path) |>
    dplyr::distinct(.data$path, .data$role, .keep_all = TRUE) |>
    as.data.frame(stringsAsFactors = FALSE)
}

script_role_for <- function(rel, parsed, setup_file = NULL, candidates = NULL, archive_dirs = archive_dir_defaults()) {
  if (path_has_segment(rel, archive_dirs)) return("archived")
  if (!is.null(candidates) && nrow(candidates) > 0) {
    hit <- candidates[candidates$path == rel, , drop = FALSE]
    if (nrow(hit) > 0) return(hit$role[1])
  }
  f <- parsed$files[[rel]]
  if (!is.null(f$file_type) && identical(f$file_type, "stata")) return("stata")
  if (grepl("setup|config|init|00_", basename(rel), ignore.case = TRUE)) return("setup")
  if (grepl("master(?![a-z])", basename(rel), ignore.case = TRUE, perl = TRUE)) return("master")
  "script"
}

edge_confidence_for_path <- function(path, dynamic = FALSE, basename_only = FALSE) {
  if (isTRUE(basename_only) || has_unresolved_prefix(path)) return("low")
  if (isTRUE(dynamic) || grepl("\\$|\\{|\\}|__p0__|^[A-Za-z_][A-Za-z0-9_.]*(/|$)", path)) return("medium")
  "high"
}

build_dependency_graph <- function(parsed, data_info, project_path, data_path = character(0), config = list()) {
  project_path <- normalizePath(project_path, mustWork = TRUE)
  if (length(data_path) > 0 && nzchar(data_path)) data_path <- normalizePath(data_path, mustWork = FALSE)

  r_ids <- names(parsed$files)
  archive_dirs <- config$exclude_dirs %||% archive_dir_defaults()
  include_dirs <- config$include_dirs %||% character(0)

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
    if (!grepl("/", pid, fixed = TRUE) &&
        !is.null(data_info$basename_resolution) && length(data_info$basename_resolution) > 0) {
      key <- tolower(pid)
      if (key %in% names(data_info$basename_resolution) && nzchar(data_info$basename_resolution[[key]])) {
        pid <- data_info$basename_resolution[[key]]
      }
    }
    if (isTRUE(has_scan_restrict) && !(path_norm_for_group(pid) %in% scan_norm_set)) return("")
    pid
  }

  resolve_data_records <- function(paths) {
    if (length(paths) == 0) return(data.frame(raw = character(0), resolved = character(0), confidence = character(0), certainty_notes = character(0), stringsAsFactors = FALSE))
    raw <- vapply(paths, function(p) canonical_data_id(p)[1], character(1))
    resolved <- vapply(raw, resolve_data_id, character(1))
    keep <- nzchar(resolved)
    raw <- raw[keep]
    resolved <- resolved[keep]
    by_basename <- nzchar(raw) & !grepl("/", raw, fixed = TRUE) & grepl("/", resolved, fixed = TRUE) & tolower(raw) == tolower(basename(resolved))
    conf <- vapply(raw, edge_confidence_for_path, character(1))
    conf[by_basename] <- "medium"
    notes <- ifelse(by_basename, "bare dataset name resolved to unique full path", "")
    data.frame(raw = raw, resolved = resolved, confidence = conf, certainty_notes = notes, stringsAsFactors = FALSE)
  }

  resolve_source_id <- function(s, r_ids) {
    s_id <- s
    if (!grepl("\\.(R|r|Rmd|RMD|DO|do)$", s)) s_id <- paste0(s, ".R")
    if (s_id %in% r_ids) return(s_id)
    base_s <- basename(s_id)
    match_full <- r_ids[tolower(basename(r_ids)) == tolower(base_s)]
    if (length(match_full) == 1) return(match_full)
    if (length(match_full) > 1) {
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
          paste0("Stata do/include \"", basename(s), "\"", note)
        }, character(1))
      } else {
        paste0("source(\"", basename(src), "\")")
      }
      data.frame(
        from = rel,
        to = to_ids,
        type = "source_run",
        confidence = ifelse(to_ids %in% r_ids, vapply(src, edge_confidence_for_path, character(1)), "high"),
        provenance = src_reason,
        source_file = rel,
        line = NA_integer_,
        raw_path = src,
        resolved_path = to_ids,
        certainty_notes = ifelse(to_ids %in% r_ids, "", "explicit source/run/include target could not be found"),
        stringsAsFactors = FALSE
      )
    } else NULL

    read_rec <- resolve_data_records(reads)
    read_edges <- if (nrow(read_rec) > 0) {
      read_ids <- read_rec$resolved
      r_notes <- f$data_read_notes %||% character(0)
      r_reason <- vapply(read_ids, function(pid) {
        note <- if (length(r_notes) > 0 && pid %in% names(r_notes) && nzchar(r_notes[[pid]])) paste0(" (", r_notes[[pid]], ")") else ""
        paste0("read call: ", data_path_display_label(pid), note)
      }, character(1))
      data.frame(
        from = read_ids,
        to = rel,
        type = "reads",
        confidence = read_rec$confidence,
        provenance = r_reason,
        source_file = rel,
        line = NA_integer_,
        raw_path = read_rec$raw,
        resolved_path = read_ids,
        certainty_notes = ifelse(nzchar(read_rec$certainty_notes), read_rec$certainty_notes,
                                 ifelse(vapply(read_rec$raw, has_unresolved_prefix, logical(1)), "unresolved path variable remains", "")),
        stringsAsFactors = FALSE
      )
    } else NULL

    write_rec <- resolve_data_records(writes)
    write_edges <- if (nrow(write_rec) > 0) {
      write_ids <- write_rec$resolved
      w_notes <- f$data_write_notes %||% character(0)
      w_reason <- vapply(write_ids, function(pid) {
        note <- if (length(w_notes) > 0 && pid %in% names(w_notes) && nzchar(w_notes[[pid]])) paste0(" (", w_notes[[pid]], ")") else ""
        paste0("write call: ", data_path_display_label(pid), note)
      }, character(1))
      data.frame(
        from = rel,
        to = write_ids,
        type = "writes",
        confidence = write_rec$confidence,
        provenance = w_reason,
        source_file = rel,
        line = NA_integer_,
        raw_path = write_rec$raw,
        resolved_path = write_ids,
        certainty_notes = ifelse(nzchar(write_rec$certainty_notes), write_rec$certainty_notes,
                                 ifelse(vapply(write_rec$raw, has_unresolved_prefix, logical(1)), "unresolved path variable remains", "")),
        stringsAsFactors = FALSE
      )
    } else NULL

    dplyr::bind_rows(src_edges, read_edges, write_edges)
  })

  edges <- dplyr::bind_rows(edge_rows) |>
    dplyr::distinct() |>
    as.data.frame(stringsAsFactors = FALSE)

  missing_scripts <- unique(edges$to[edges$type == "source_run" & !(edges$to %in% r_ids)])
  missing_scripts <- missing_scripts[nzchar(missing_scripts)]

  setup_file <- identify_setup_file(parsed, edges)
  candidates <- detect_setup_master_candidates(parsed, setup_file, config)
  roles <- vapply(r_ids, script_role_for, character(1), parsed = parsed, setup_file = setup_file,
                  candidates = candidates, archive_dirs = archive_dirs)
  folders <- vapply(r_ids, node_folder, character(1))
  archived <- roles == "archived"
  included <- !archived | vapply(r_ids, path_has_segment, logical(1), segments = include_dirs)

  nodes_r <- data.frame(
    id = r_ids,
    label = basename(r_ids),
    type = "script",
    path = r_ids,
    folder = folders,
    role = roles,
    included = included,
    confidence = NA_character_,
    stringsAsFactors = FALSE
  )

  data_paths_resolved <- vapply(data_info$index$path %||% character(0), resolve_data_id, character(1))
  data_paths_resolved <- unique(data_paths_resolved[nzchar(data_paths_resolved)])
  nodes_data <- data.frame(
    id = data_paths_resolved,
    label = vapply(data_paths_resolved, data_path_display_label, character(1)),
    type = "data",
    path = data_paths_resolved,
    folder = vapply(data_paths_resolved, node_folder, character(1), default = "data"),
    role = "unknown",
    included = TRUE,
    confidence = NA_character_,
    stringsAsFactors = FALSE
  )

  nodes_missing <- if (length(missing_scripts) > 0) {
    data.frame(
      id = missing_scripts,
      label = basename(missing_scripts),
      type = "missing_script",
      path = missing_scripts,
      folder = vapply(missing_scripts, node_folder, character(1)),
      role = "unknown",
      included = TRUE,
      confidence = "high",
      stringsAsFactors = FALSE
    )
  } else NULL

  nodes <- dplyr::bind_rows(nodes_r, nodes_data, nodes_missing) |>
    dplyr::distinct(.data$id, .keep_all = TRUE) |>
    as.data.frame(stringsAsFactors = FALSE)

  norm_path <- function(p) path_norm_for_group(vapply(canonical_data_id(p), resolve_data_id, character(1)))
  exact_keys <- character(0); exact_scripts <- character(0)
  folder_keys <- character(0); folder_scripts <- character(0)
  base_keys <- character(0); base_scripts <- character(0)

  for (script in r_ids) {
    f <- parsed$files[[script]]
    reads <- unique(f$data_reads %||% character(0))
    folders_read <- unique(f$data_read_folders %||% character(0))
    if (length(reads) > 0) {
      r_norm <- norm_path(reads)
      ok <- nzchar(r_norm)
      if (!isTRUE(has_scan)) ok <- ok & !vapply(r_norm, has_unresolved_prefix, logical(1))
      if (any(ok)) {
        r_norm_ok <- r_norm[ok]
        is_bare <- !grepl("/", r_norm_ok, fixed = TRUE)
        if (any(!is_bare)) {
          exact_keys <- c(exact_keys, r_norm_ok[!is_bare])
          exact_scripts <- c(exact_scripts, rep(script, sum(!is_bare)))
        }
        if (isTRUE(has_scan_restrict)) {
          base_keys <- c(base_keys, basename(r_norm_ok))
          base_scripts <- c(base_scripts, rep(script, length(r_norm_ok)))
        }
      }
    }
    if (length(folders_read) > 0) {
      f_norm <- sub("/+$", "", norm_path(folders_read))
      ok <- nzchar(f_norm)
      if (!isTRUE(has_scan)) ok <- ok & !vapply(f_norm, has_unresolved_prefix, logical(1))
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
  df_reasons_map <- new.env(parent = emptyenv())
  df_conf_map <- new.env(parent = emptyenv())

  add_flow_match <- function(writer, reader, dataset, confidence, note) {
    if (identical(writer, reader)) return()
    key <- paste(writer, reader, sep = "|||")
    cur <- if (exists(key, envir = df_reasons_map, inherits = FALSE)) get(key, envir = df_reasons_map, inherits = FALSE) else character(0)
    assign(key, unique(c(cur, dataset)), envir = df_reasons_map)
    ccur <- if (exists(key, envir = df_conf_map, inherits = FALSE)) get(key, envir = df_conf_map, inherits = FALSE) else character(0)
    assign(key, unique(c(ccur, paste(confidence, note, sep = "|||"))), envir = df_conf_map)
    df_from <<- c(df_from, writer)
    df_to <<- c(df_to, reader)
  }

  for (writer in r_ids) {
    writes <- unique(parsed$files[[writer]]$data_writes %||% character(0))
    if (length(writes) == 0) next
    if (!isTRUE(has_scan)) writes <- writes[!vapply(canonical_data_id(writes), has_unresolved_prefix, logical(1))]
    w_norms <- norm_path(writes)
    w_norms <- w_norms[nzchar(w_norms)]
    if (length(w_norms) == 0) next

    for (w_norm in w_norms) {
      ex <- exact_index[[w_norm]]
      if (!is.null(ex)) for (reader in unique(ex)) add_flow_match(writer, reader, w_norm, "high", "writer/read match on same normalized path")

      if (isTRUE(has_scan) && grepl("/", w_norm, fixed = TRUE)) {
        bkey <- basename(w_norm)
        bx <- base_index[[bkey]]
        if (!is.null(bx)) for (reader in unique(bx)) add_flow_match(writer, reader, w_norm, "low", paste0("basename-only match on ", bkey))
      }

      dir_key <- dirname(w_norm)
      if (!nzchar(dir_key) || dir_key == ".") dir_key <- ""
      if (nzchar(dir_key)) {
        cur <- dir_key
        for (iter in 1:50) {
          fx <- folder_index[[cur]]
          if (!is.null(fx)) for (reader in unique(fx)) add_flow_match(writer, reader, w_norm, "medium", paste0("reader watches folder ", cur))
          parent <- dirname(cur)
          if (!nzchar(parent) || parent == "." || parent == cur) break
          cur <- parent
        }
      }
    }
  }

  if (length(df_from) > 0) {
    pair_key <- paste(df_from, df_to, sep = "|||")
    unique_idx <- !duplicated(pair_key)
    pair_key_u <- pair_key[unique_idx]
    reasons_u <- vapply(pair_key_u, function(k) {
      datasets <- if (exists(k, envir = df_reasons_map, inherits = FALSE)) get(k, envir = df_reasons_map, inherits = FALSE) else character(0)
      if (length(datasets) > 0) paste0("writer/read match on ", paste(datasets, collapse = "; ")) else "data flow"
    }, character(1))
    conf_u <- vapply(pair_key_u, function(k) {
      vals <- if (exists(k, envir = df_conf_map, inherits = FALSE)) get(k, envir = df_conf_map, inherits = FALSE) else character(0)
      conf <- sub("\\|\\|\\|.*$", "", vals)
      if ("high" %in% conf) "high" else if ("medium" %in% conf) "medium" else "low"
    }, character(1))
    notes_u <- vapply(pair_key_u, function(k) {
      vals <- if (exists(k, envir = df_conf_map, inherits = FALSE)) get(k, envir = df_conf_map, inherits = FALSE) else character(0)
      notes <- sub("^[^|]+\\|\\|\\|", "", vals)
      paste(unique(notes[nzchar(notes)]), collapse = "; ")
    }, character(1))
    edges <- rbind(edges, data.frame(
      from = unname(df_from[unique_idx]),
      to = unname(df_to[unique_idx]),
      type = "data_flow",
      confidence = unname(conf_u),
      provenance = unname(reasons_u),
      source_file = NA_character_,
      line = NA_integer_,
      raw_path = unname(reasons_u),
      resolved_path = unname(reasons_u),
      certainty_notes = unname(notes_u),
      stringsAsFactors = FALSE
    ))
  }

  edges <- unique(edges) |> as.data.frame(stringsAsFactors = FALSE)
  if (nrow(edges) > 0) {
    edges$id <- paste0("e", seq_len(nrow(edges)))
    edges$reason <- edges$provenance
  } else {
    edges$id <- character(0)
    edges$reason <- character(0)
  }

  src_edges <- edges[edges$type %in% c("source_run", "sources"), , drop = FALSE]
  active_script_ids <- nodes$id[nodes$type == "script" & nodes$role != "archived" & nodes$included]
  active_archive_refs <- unique(src_edges$to[src_edges$from %in% active_script_ids & src_edges$to %in% nodes$id[nodes$role == "archived"]])
  nodes$included[nodes$role == "archived" & nodes$id %in% active_archive_refs] <- TRUE
  if (isTRUE(config$graph$show_archived %||% FALSE)) nodes$included[nodes$role == "archived"] <- TRUE

  list(
    nodes = nodes,
    edges = edges,
    project_path = project_path,
    setup_file = setup_file,
    setup_master_candidates = candidates,
    excluded_dirs = archive_dirs
  )
}

identify_setup_file <- function(parsed, edges) {
  src_edges <- edges[edges$type %in% c("source_run", "sources"), , drop = FALSE]
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

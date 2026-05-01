# Detect actionable issues and lower-priority notes from the normalized graph.

detect_issues <- function(graph, parsed, data_info, project_path) {
  project_path <- normalizePath(project_path, mustWork = TRUE)
  issue_cols <- c("severity", "scope", "type", "path", "from", "to", "message", "confidence", "provenance")
  issues <- data.frame(
    severity = character(0), scope = character(0), type = character(0), path = character(0),
    from = character(0), to = character(0), message = character(0),
    confidence = character(0), provenance = character(0),
    stringsAsFactors = FALSE
  )
  notes <- issues

  add_issue <- function(severity, scope, type, path = "", from = "", to = "", message = "",
                        confidence = "", provenance = "") {
    row <- data.frame(
      severity = severity, scope = scope, type = type, path = path, from = from, to = to,
      message = message, confidence = confidence, provenance = provenance,
      stringsAsFactors = FALSE
    )
    issues <<- rbind(issues, row)
  }
  add_note <- function(type, scope = "global", path = "", from = "", to = "", message = "",
                       confidence = "", provenance = "") {
    row <- data.frame(
      severity = "info", scope = scope, type = type, path = path, from = from, to = to,
      message = message, confidence = confidence, provenance = provenance,
      stringsAsFactors = FALSE
    )
    notes <<- rbind(notes, row)
  }

  script_nodes <- graph$nodes$id[graph$nodes$type == "script"]
  included <- if ("included" %in% names(graph$nodes)) graph$nodes$included else rep(TRUE, nrow(graph$nodes))
  role_map <- if ("role" %in% names(graph$nodes)) setNames(graph$nodes$role, graph$nodes$id) else character(0)
  archived_scripts <- names(role_map)[role_map == "archived"]
  active_scripts <- graph$nodes$id[graph$nodes$type == "script" & included & !(graph$nodes$id %in% archived_scripts)]
  edge_scope <- function(e) {
    ids <- unique(c(as.character(e$source_file %||% ""), as.character(e$from %||% ""), as.character(e$to %||% "")))
    ids <- ids[!is.na(ids) & nzchar(ids)]
    if (length(ids) > 0 && any(ids %in% archived_scripts)) "archived" else "active"
  }

  src_edges <- graph$edges[graph$edges$type %in% c("source_run", "sources"), , drop = FALSE]
  missing <- src_edges[!(src_edges$to %in% script_nodes), , drop = FALSE]
  if (nrow(missing) > 0) {
    for (i in seq_len(nrow(missing))) {
      e <- missing[i, ]
      scope <- edge_scope(e)
      add_issue(
        if (scope == "archived") "info" else "error",
        scope, "missing_source", path = e$source_file %||% e$from,
        from = e$from, to = e$to,
        message = paste0("Explicit source/run/include target could not be found: ", e$to),
        confidence = e$confidence %||% "high", provenance = e$provenance %||% ""
      )
    }
  }

  active_src <- src_edges[src_edges$from %in% active_scripts & src_edges$to %in% active_scripts, , drop = FALSE]
  if (nrow(active_src) > 0) {
    if (requireNamespace("igraph", quietly = TRUE)) {
      g <- igraph::graph_from_data_frame(active_src[, c("from", "to")], vertices = active_scripts)
      comps <- igraph::components(g, mode = "strong")
      cyc_ids <- names(comps$membership)[comps$csize[comps$membership] > 1]
      if (length(cyc_ids) > 0) {
        add_issue(
          "error", "active", "cycle", from = paste(cyc_ids, collapse = " -> "),
          message = paste0("Script-to-script dependency cycle among active scripts: ", paste(cyc_ids, collapse = " -> ")),
          confidence = "high", provenance = "source_run graph"
        )
      }
    } else {
      cycles <- simple_cycle_check(active_src)
      for (cyc in cycles) {
        add_issue(
          "error", "active", "cycle", from = paste(cyc, collapse = " -> "),
          message = paste0("Script-to-script dependency cycle among active scripts: ", paste(cyc, collapse = " -> ")),
          confidence = "high", provenance = "source_run graph"
        )
      }
    }
  }

  unresolved <- graph$edges[graph$edges$confidence == "low" &
                              grepl("unresolved path variable|unresolved|\\$|\\{", paste(graph$edges$raw_path, graph$edges$certainty_notes), ignore.case = TRUE),
                            , drop = FALSE]
  if (nrow(unresolved) > 0) {
    for (i in seq_len(nrow(unresolved))) {
      e <- unresolved[i, ]
      scope <- edge_scope(e)
      add_issue(
        if (scope == "archived") "info" else "warning",
        scope, "unresolved_path_variable", path = e$source_file %||% "",
        from = e$from, to = e$to,
        message = paste0("Path expression could not be fully resolved: ", e$raw_path),
        confidence = e$confidence, provenance = e$provenance
      )
    }
  }

  ambiguous <- graph$edges[graph$edges$type == "data_flow" & graph$edges$confidence == "low", , drop = FALSE]
  if (nrow(ambiguous) > 0) {
    for (i in seq_len(nrow(ambiguous))) {
      e <- ambiguous[i, ]
      scope <- if (e$from %in% active_scripts && e$to %in% active_scripts) "active" else if (e$from %in% archived_scripts || e$to %in% archived_scripts) "archived" else "global"
      add_issue(
        if (scope == "archived") "info" else "warning",
        scope, "ambiguous_dataset_match", from = e$from, to = e$to,
        message = paste0("Data-flow edge is low confidence: ", e$certainty_notes),
        confidence = "low", provenance = e$provenance
      )
    }
  }

  archived_refs <- src_edges[src_edges$from %in% active_scripts & src_edges$to %in% names(role_map)[role_map == "archived"], , drop = FALSE]
  if (nrow(archived_refs) > 0) {
    for (i in seq_len(nrow(archived_refs))) {
      e <- archived_refs[i, ]
      add_issue(
        "warning", "active", "excluded_but_referenced", path = e$source_file %||% e$from,
        from = e$from, to = e$to,
        message = paste0("Active code references archived/excluded script: ", e$to),
        confidence = e$confidence, provenance = e$provenance
      )
    }
  }

  data_nodes <- graph$nodes[graph$nodes$type == "data", , drop = FALSE]
  if (nrow(data_nodes) > 0) {
    key <- tolower(basename(data_nodes$id))
    dups <- unique(key[duplicated(key)])
    dup_issue_msgs <- character(0)
    dup_issue_paths <- character(0)
    for (k in dups[nzchar(dups)]) {
      paths <- data_nodes$id[key == k]
      has_bare <- any(!grepl("/", paths, fixed = TRUE))
      has_full <- any(grepl("/", paths, fixed = TRUE))
      if (has_bare && has_full) {
        dup_issue_msgs <- c(dup_issue_msgs, paste0(k, " (", paste(paths, collapse = "; "), ")"))
        dup_issue_paths <- c(dup_issue_paths, paste(paths, collapse = "; "))
      } else {
        add_note(
          "duplicate_dataset_basename", scope = "global", path = paste(paths, collapse = "; "),
          message = paste0("Multiple dataset references share basename '", k, "' across paths."),
          confidence = "low", provenance = "basename duplicate scan"
        )
      }
    }
    if (length(dup_issue_msgs) > 0) {
      sample_msg <- paste(head(dup_issue_msgs, 10), collapse = " | ")
      if (length(dup_issue_msgs) > 10) sample_msg <- paste0(sample_msg, " | ... ", length(dup_issue_msgs) - 10, " more")
      add_issue(
        "warning", "global", "duplicate_dataset_node",
        path = paste(head(dup_issue_paths, 20), collapse = " || "),
        message = paste0(length(dup_issue_msgs), " dataset basenames appear as both bare names and full paths. Examples: ", sample_msg),
        confidence = "low", provenance = "basename/full-path duplicate scan"
      )
    }
  }

  written <- unique(graph$edges$to[graph$edges$type == "writes"])
  read <- unique(graph$edges$from[graph$edges$type == "reads"])
  for (d in setdiff(written, read)) {
    add_note("dataset_written_not_read", scope = "global", path = d, to = d, message = paste0("Dataset is written but no matching read was detected: ", d), confidence = "medium")
  }
  for (d in setdiff(read, written)) {
    add_note("dataset_read_not_produced", scope = "global", path = d, from = d, message = paste0("Dataset is read but no matching writer was detected: ", d), confidence = "medium")
  }

  issues <- issues[, issue_cols, drop = FALSE]
  notes <- notes[, issue_cols, drop = FALSE]
  attr(issues, "notes") <- notes
  issues
}

`%||%` <- function(x, y) if (length(x) == 0 || is.null(x)) y else x

simple_cycle_check <- function(edges) {
  adj <- split(edges$to, edges$from)
  nodes <- unique(c(edges$from, edges$to))
  cycles <- list()
  for (start in nodes) {
    found <- find_cycle_dfs(start, start, adj, character(0), character(0))
    if (length(found) > 0) cycles <- c(cycles, list(found))
  }
  if (length(cycles) == 0) return(list())
  keys <- vapply(cycles, function(x) paste(sort(x), collapse = "|"), character(1))
  cycles[!duplicated(keys)]
}

find_cycle_dfs <- function(node, start, adj, path, path_set) {
  if (node %in% path_set) {
    if (node == start && length(path) > 0) return(path)
    return(character(0))
  }
  path <- c(path, node)
  path_set <- c(path_set, node)
  neighbors <- adj[[node]]
  if (is.null(neighbors)) return(character(0))
  for (n in neighbors) {
    res <- find_cycle_dfs(n, start, adj, path, path_set)
    if (length(res) > 0) return(res)
  }
  character(0)
}

# Detect issues: circular deps, missing files, wrong deps, unused files/datasets, etc.

detect_issues <- function(graph, parsed, data_info, project_path) {
  project_path <- normalizePath(project_path, mustWork = TRUE)
  issues <- data.frame(
    type = character(0), severity = character(0), from = character(0),
    to = character(0), message = character(0), stringsAsFactors = FALSE
  )
  
  r_nodes <- graph$nodes$id[graph$nodes$type == "r_file"]
  
  # 1. Missing source files (with case-insensitive fallback for cross-OS portability)
  for (rel in names(parsed$files)) {
    f <- parsed$files[[rel]]
    for (s in f$sources) {
      full <- file.path(project_path, s)
      if (!file.exists(full)) {
        # On case-sensitive filesystems, resolved path may differ by case; try same dir + same basename
        found_alt <- FALSE
        if (dir.exists(dirname(full))) {
          b <- basename(s)
          existing <- list.files(dirname(full), ignore.case = FALSE)
          if (any(tolower(existing) == tolower(b))) found_alt <- TRUE
        }
        if (!found_alt) {
          issues <- rbind(issues, data.frame(
            type = "missing_source",
            severity = "error",
            from = rel,
            to = s,
            message = paste0("R file '", rel, "' sources '", s, "' which does not exist"),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # 2. Missing data files - skipped (user may not have data in folder)
  
  # 3. Wrong dependencies: A sources B but pipeline/data-flow says B runs after A
  src_edges <- graph$edges[graph$edges$type == "sources" & graph$edges$from %in% r_nodes & graph$edges$to %in% r_nodes, ]
  ord_edges <- graph$edges[graph$edges$type %in% c("pipeline", "data_flow") & graph$edges$from %in% r_nodes & graph$edges$to %in% r_nodes, ]
  for (i in seq_len(nrow(src_edges))) {
    caller <- src_edges$from[i]
    callee <- src_edges$to[i]
    if (nrow(ord_edges) > 0) {
      rev_path <- any(ord_edges$from == callee & ord_edges$to == caller)
      if (rev_path) {
        issues <- rbind(issues, data.frame(
          type = "wrong_dependency",
          severity = "error",
          from = caller,
          to = callee,
          message = paste0("'", caller, "' sources '", callee, "' but execution order requires ", callee, " to run first (wrong order)"),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # 4. Circular dependencies (R files only)
  r_edges <- graph$edges[graph$edges$type == "sources" & graph$edges$from %in% r_nodes & graph$edges$to %in% r_nodes, ]
  if (nrow(r_edges) > 0) {
    if (requireNamespace("igraph", quietly = TRUE)) {
      g <- igraph::graph_from_data_frame(r_edges[, c("from", "to")], vertices = r_nodes)
      cyc <- tryCatch(igraph::girth(g), error = function(e) list(girth = Inf, circle = NULL))
      if (!is.null(cyc) && is.finite(cyc$girth) && cyc$girth > 0 && !is.null(cyc$circle) && length(cyc$circle) > 0) {
        cycle_nodes <- as.character(igraph::V(g)$name[cyc$circle])
        issues <- rbind(issues, data.frame(
          type = "circular_dependency",
          severity = "error",
          from = paste(cycle_nodes, collapse = " -> "),
          to = "",
          message = paste0("Circular dependency among R files: ", paste(cycle_nodes, collapse = " -> ")),
          stringsAsFactors = FALSE
        ))
      }
    } else {
      cycles <- simple_cycle_check(r_edges)
      if (length(cycles) > 0) {
        for (cyc in cycles) {
          issues <- rbind(issues, data.frame(
            type = "circular_dependency",
            severity = "error",
            from = paste(cyc, collapse = " -> "),
            to = "",
            message = paste0("Circular dependency: ", paste(cyc, collapse = " -> ")),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # 5. Files not used (never sourced by others, no pipeline/data consumer)
  has_incoming <- unique(graph$edges$to[graph$edges$type %in% c("sources", "pipeline", "data_flow")])
  has_outgoing_data <- unique(graph$edges$from[graph$edges$type == "data_flow"])
  setup_file <- graph$setup_file
  unused_files <- setdiff(r_nodes, has_incoming)
  unused_files <- setdiff(unused_files, has_outgoing_data)
  unused_files <- setdiff(unused_files, setup_file)
  if (length(unused_files) > 0) {
    for (o in unused_files) {
      issues <- rbind(issues, data.frame(
        type = "file_not_used",
        severity = "info",
        from = o,
        to = "",
        message = paste0("'", o, "' — not used"),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # 6. Datasets created but never read
  all_written <- unique(unlist(lapply(parsed$files, function(f) f$data_writes %||% character(0))))
  all_read <- unique(unlist(lapply(parsed$files, function(f) f$data_reads %||% character(0))))
  norm <- function(p) gsub("\\\\", "/", tolower(trimws(normalize_path_canonical(p))))
  written_norm <- norm(all_written)
  read_norm <- norm(all_read)
  unused_data <- all_written[!written_norm %in% read_norm]
  if (length(unused_data) > 0) {
    for (d in unused_data) {
      producer <- names(parsed$files)[vapply(parsed$files, function(f) d %in% (f$data_writes %||% character(0)), logical(1))]
      producer <- producer[1]
      issues <- rbind(issues, data.frame(
        type = "dataset_not_used",
        severity = "info",
        from = producer,
        to = d,
        message = paste0("'", d, "' created by '", producer, "' — never read"),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  issues
}

`%||%` <- function(x, y) if (length(x) == 0 || is.null(x)) y else x

simple_cycle_check <- function(edges) {
  adj <- split(edges$to, edges$from)
  nodes <- unique(c(edges$from, edges$to))
  cycles <- list()
  for (start in nodes) {
    found <- find_cycle_dfs(start, start, adj, character(0), character(0))
    if (length(found) > 0) {
      cycles <- c(cycles, list(found))
    }
  }
  # Dedupe by sorted node set
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

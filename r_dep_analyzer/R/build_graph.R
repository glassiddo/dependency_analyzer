# Build dependency graph from parsed R files and data index.

# Display label for data paths. Bare filenames (no directory) are shown as-is; no project-specific path assumed.
data_path_display_label <- function(p) {
  if (!nzchar(p)) return(p)
  gsub("\\\\", "/", trimws(p))
}

build_dependency_graph <- function(parsed, data_info, project_path, data_path = character(0)) {
  project_path <- normalizePath(project_path, mustWork = TRUE)
  if (length(data_path) > 0 && nzchar(data_path)) data_path <- normalizePath(data_path, mustWork = FALSE)
  nodes <- data.frame(id = character(0), label = character(0), type = character(0), stringsAsFactors = FALSE)
  edges <- data.frame(from = character(0), to = character(0), type = character(0), stringsAsFactors = FALSE)
  
  r_ids <- names(parsed$files)

  for (rel in r_ids) {
    nodes <- rbind(nodes, data.frame(
      id = rel,
      label = basename(rel),
      type = "r_file",
      stringsAsFactors = FALSE
    ))
  }
  
  for (i in seq_len(nrow(data_info$index))) {
    row <- data_info$index[i, ]
    pid <- row$path
    if (!pid %in% nodes$id) {
      nodes <- rbind(nodes, data.frame(
        id = pid,
        label = data_path_display_label(pid),
        type = "data",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  for (rel in r_ids) {
    f <- parsed$files[[rel]]
    for (s in f$sources) {
      s_id <- s
      # Only append .R when path is not already a script extension (.R, .Rmd, .do)
      if (!grepl("\\.(R|r|Rmd|RMD|DO|do)$", s)) s_id <- paste0(s, ".R")
      if (!s_id %in% r_ids) {
        base_s <- basename(s_id)
        # Case-insensitive match so resolved paths (e.g. from setup vars) match actual filenames on any OS
        match_full <- r_ids[tolower(basename(r_ids)) == tolower(base_s)]
        if (length(match_full) == 1) s_id <- match_full
        else if (length(match_full) > 1) {
          # Prefer same directory (case-insensitive) if possible
          dir_s <- dirname(s_id)
          same_dir <- match_full[tolower(dirname(match_full)) == tolower(dir_s)]
          s_id <- if (length(same_dir) >= 1) same_dir[1] else match_full[1]
        }
      }
      edges <- rbind(edges, data.frame(from = rel, to = s_id, type = "sources", stringsAsFactors = FALSE))
    }
    for (d in f$data_reads) {
      edges <- rbind(edges, data.frame(from = rel, to = d, type = "reads", stringsAsFactors = FALSE))
    }
  }
  
  pipeline_edges <- infer_pipeline_edges(r_ids)
  edges <- rbind(edges, pipeline_edges)
  
  # Data-flow: producer -> consumer (script that writes data -> script that reads it).
  # Reference-driven only: match each write to reads (exact or folder prefix). Paths are normalized
  # (path_norm_for_group) so R and Stata align (e.g. build.dir/Africa/foo.dta and data/Build/Africa/foo.dta).
  # The data scan is used only for the dataset index and to verify which paths exist; we do not iterate
  # over every scanned file here.
  norm_path <- function(p) path_norm_for_group(p)
  for (rel in r_ids) {
    f <- parsed$files[[rel]]
    writes <- f$data_writes
    if (is.null(writes)) writes <- character(0)
    for (w in writes) {
      w_norm <- norm_path(w)
      if (!nzchar(w_norm)) next
      for (other in r_ids) {
        if (other == rel) next
        reads <- parsed$files[[other]]$data_reads
        if (is.null(reads)) reads <- character(0)
        folders <- parsed$files[[other]]$data_read_folders
        if (is.null(folders)) folders <- character(0)
        matched <- FALSE
        for (r in c(reads, folders)) {
          r_norm <- norm_path(r)
          if (!nzchar(r_norm)) next
          if (r_norm == w_norm) {
            matched <- TRUE
            break
          }
          if (grepl("/", r_norm) && (startsWith(w_norm, paste0(r_norm, "/")) || w_norm == r_norm)) {
            matched <- TRUE
            break
          }
          # Bare filename (e.g. consistent14.dta from read_dta(here(..., cname, cens_dir, "consistent14.dta")))
          if (!grepl("/", r_norm) && (w_norm == r_norm || endsWith(w_norm, paste0("/", r_norm)))) {
            matched <- TRUE
            break
          }
        }
        if (matched) edges <- rbind(edges, data.frame(from = rel, to = other, type = "data_flow", stringsAsFactors = FALSE))
      }
    }
  }

  edges <- unique(edges)
  
  setup_file <- identify_setup_file(parsed, edges)
  
  list(nodes = nodes, edges = edges, project_path = project_path, setup_file = setup_file)
}

# Infer script dependencies from file naming: prepare/1, prepare/2a, ..., prepare/4, prepare/5
# Higher-numbered scripts depend on lower-numbered ones in same folder
infer_pipeline_edges <- function(r_ids) {
  edges <- data.frame(from = character(0), to = character(0), type = character(0), stringsAsFactors = FALSE)
  by_dir <- split(r_ids, dirname(r_ids))
  for (dir_files in by_dir) {
    if (length(dir_files) < 2) next
    ord <- pipeline_order(dir_files)
    levels <- pipeline_levels(ord)
    for (lev in unique(levels[levels > 0])) {
      from_idx <- which(levels > 0 & levels < lev)
      to_idx <- which(levels == lev)
      if (length(from_idx) > 0 && length(to_idx) > 0) {
        for (i in from_idx) {
          for (j in to_idx) {
            edges <- rbind(edges, data.frame(
              from = ord[i],
              to = ord[j],
              type = "pipeline",
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  unique(edges)
}

# Natural order: 1 < 2a < 2b < ... < 2o < 3a < ... < 4 < 5
pipeline_order <- function(paths) {
  basenames <- basename(paths)
  prefix <- sub("^([0-9]+[a-z]?)\\s*-.*", "\\1", basenames, ignore.case = TRUE)
  num_part <- suppressWarnings(as.numeric(gsub("[a-z]", "", prefix)))
  num_part[is.na(num_part)] <- 999
  alpha_part <- gsub("[0-9]", "", tolower(prefix))
  alpha_part[nchar(alpha_part) == 0] <- " "
  ord <- order(num_part, alpha_part)
  paths[ord]
}

# Level = numeric part for grouping (1, 2, 3, 4, 5); NA for non-pipeline files
pipeline_levels <- function(ordered_paths) {
  basenames <- basename(ordered_paths)
  prefix <- sub("^([0-9]+)[a-z]?\\s*-.*", "\\1", basenames, ignore.case = TRUE)
  lv <- suppressWarnings(as.numeric(prefix))
  lv[is.na(lv)] <- 0
  lv
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

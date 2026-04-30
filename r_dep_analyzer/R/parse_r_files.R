# Parse R files and extract dependencies (source, read.*, here())
# Handles: source(), read.csv, readRDS, read_excel, fread, haven::read_dta, etc.
# Resolves here() and here::here() paths relative to project root.

`%||%` <- function(x, y) if (length(x) == 0 || is.null(x)) y else x

parse_r_project <- function(project_path, exclude_folders = character(0)) {
  canonicalization_sanity_checks()
  r_ext <- c("\\.R$", "\\.r$", "\\.Rmd$", "\\.RMD$")
  do_ext <- c("\\.do$", "\\.DO$")
  exclude <- c("_dependency_analysis", ".git", ".Rproj.user", "renv", ".Rhistory")
  
  include_archive <- nzchar(Sys.getenv("R_DEP_INCLUDE_ARCHIVE", "")) && tolower(Sys.getenv("R_DEP_INCLUDE_ARCHIVE")) %in% c("1", "true", "yes")
  default_excl <- if (!include_archive) c("Archive", "archive", "Archives", "archives") else character(0)
  exclude_folders <- unique(c(default_excl, exclude_folders))
  
  all_files <- list.files(project_path, recursive = TRUE, full.names = TRUE)
  r_files <- all_files[grepl(paste(r_ext, collapse = "|"), all_files, ignore.case = TRUE)]
  do_files <- all_files[grepl(paste(do_ext, collapse = "|"), all_files, ignore.case = TRUE)]
  r_files <- c(r_files, do_files)
  r_files <- r_files[!grepl(paste(exclude, collapse = "|"), r_files)]
  r_files <- r_files[!is_readme_file(r_files, project_path)]
  
  if (length(exclude_folders) > 0) {
    exclude_folders <- trimws(exclude_folders[nzchar(exclude_folders)])
    if (length(exclude_folders) > 0) {
      path_segments <- strsplit(gsub("\\\\", "/", r_files), "/")
      keep <- vapply(path_segments, function(segs) {
        rel <- segs[seq_len(max(1, length(segs) - 1))]
        !any(tolower(rel) %in% tolower(exclude_folders))
      }, logical(1))
      r_files <- r_files[keep]
    }
  }
  
  excl_lower <- if (length(exclude_folders) > 0) tolower(trimws(exclude_folders[nzchar(exclude_folders)])) else character(0)
  path_in_excluded <- function(x, excl) {
    if (length(excl) == 0) return(logical(length(x)))
    segs_list <- strsplit(gsub("\\\\", "/", x), "/")
    vapply(segs_list, function(s) any(tolower(s) %in% excl), logical(1))
  }
  parsed <- list()
  for (f in r_files) {
    rel <- sub(paste0("^", gsub("\\\\", "/", project_path), "/?"), "", gsub("\\\\", "/", f))
    p <- if (grepl("\\.do$|\\.DO$", f, ignore.case = TRUE)) {
      parse_single_stata_file(f, project_path, rel)
    } else {
      parse_single_r_file(f, project_path, rel)
    }
    if (!is.null(p)) {
      if (length(excl_lower) > 0) {
        p$data_reads <- p$data_reads[!path_in_excluded(p$data_reads, excl_lower)]
        p$data_writes <- p$data_writes[!path_in_excluded(p$data_writes, excl_lower)]
      }
      parsed[[rel]] <- p
    }
  }

  setup_vars <- parse_setup_vars(parsed)

  # Stata globals: prefer extracting from Stata itself (master .do + propagation) so we do not
  # rely on R setup vars for macro resolution. Use legacy R->Stata inference and global scanning
  # only as a fallback for projects without a Stata master.
  inferred_stata <- infer_stata_globals_from_r(parsed, setup_vars, project_path)
  legacy_stata_globals <- parse_stata_globals(parsed)
  fallback_stata_globals <- c(inferred_stata, legacy_stata_globals)
  stata_proj <- analyze_stata_project(parsed, project_path, fallback_globals = fallback_stata_globals)
  parsed <- stata_proj$files

  # Keep a combined setup-var map for R path resolution and higher-order inference passes.
  # Stata master globals (when found) are appended last so they win on name collisions.
  setup_vars <- c(setup_vars, fallback_stata_globals, stata_proj$globals_union %||% character(0))
  parsed <- resolve_paths_with_setup(parsed, setup_vars, project_path)

  # Higher-order inference pass (static): list/c() -> lapply/purrr::map -> wrapper that reads files.
  # This never executes project code; it only parses ASTs and safe-evaluates string expressions.
  inferred <- infer_indirect_apply_reads(parsed, setup_vars, project_path)
  parsed <- inferred$files
  inference_warnings <- inferred$warnings

  # Discover master file (read-only) and path roots from master + setup for path matching
  master_info <- discover_master_and_path_roots(project_path, names(parsed), setup_vars)
  
  list(
    files = parsed,
    project_path = project_path,
    file_paths = r_files,
    setup_vars = setup_vars,
    master_path = master_info$master_path,
    stata_master_path = stata_proj$master_do %||% NULL,
    stata_global_debug = stata_proj$global_debug %||% NULL,
    path_roots_from_master = master_info$path_roots_from_master,
    path_roots_from_setup = master_info$path_roots_from_setup,
    setup_master_files = master_info$setup_master_files,
    inference_warnings = inference_warnings
  )
}

# Discover master file by name (e.g. contains "master") and path roots from master + setup files.
# Flexible: works with only setup, only master, or both. Does not modify any file.
discover_master_and_path_roots <- function(project_path, rel_paths, setup_vars) {
  project_path <- gsub("\\\\", "/", trimws(project_path))
  master_path <- NULL
  path_roots_from_master <- character(0)
  path_roots_from_setup <- character(0)
  r_rel <- rel_paths[grepl("\\.(R|r|Rmd|RMD)$", rel_paths)]

  # 1. Prefer master by filename, but avoid false matches like "masters_*".
  #    Use a negative lookahead so "master" is not immediately followed by a letter.
  master_name_pat <- "master(?![a-z])"
  master_cands <- r_rel[grepl(master_name_pat, basename(r_rel), ignore.case = TRUE, perl = TRUE)]
  if (length(master_cands) > 0) {
    # Prefer a candidate that actually orchestrates (run_r/run_stata); otherwise prefer shallowest path.
    has_orchestrator <- function(rel) {
      full <- file.path(project_path, rel)
      if (!file.exists(full)) return(FALSE)
      txt <- tryCatch(paste(readLines(full, warn = FALSE), collapse = "\n"), error = function(e) "")
      if (!nzchar(txt)) return(FALSE)
      grepl("run_r\\s*\\(|run_stata\\s*\\(", txt, ignore.case = TRUE)
    }
    orch <- master_cands[vapply(master_cands, has_orchestrator, logical(1))]
    pick_from <- if (length(orch) > 0) orch else master_cands
    depth <- vapply(strsplit(gsub("\\\\", "/", pick_from), "/"), length, integer(1))
    master_path <- pick_from[order(depth, pick_from)][1]
  }
  # 2. Fallback: first R file that contains run_r(here::here(...)) or run_stata(here::here(...))
  if (length(master_path) == 0 || !nzchar(master_path)) {
    for (rel in r_rel) {
      full <- file.path(project_path, rel)
      if (!file.exists(full)) next
      txt <- tryCatch(paste(readLines(full, warn = FALSE), collapse = "\n"), error = function(e) "")
      if (!nzchar(txt)) next
      mr <- gregexpr("run_r\\s*\\(\\s*(?:here::here|here)\\s*\\(", txt, ignore.case = TRUE)[[1]]
      ms <- gregexpr("run_stata\\s*\\(\\s*(?:here::here|here)\\s*\\(", txt, ignore.case = TRUE)[[1]]
      if (sum(mr > 0) + sum(ms > 0) > 0) {
        master_path <- rel
        break
      }
    }
  }

  # 3. Extract path segments from here() in run_r/run_stata in master and in any setup/master-named file
  scan_for_here_roots <- function(rel) {
    full <- file.path(project_path, rel)
    if (!file.exists(full)) return(character(0))
    txt <- tryCatch(paste(readLines(full, warn = FALSE), collapse = "\n"), error = function(e) "")
    if (!nzchar(txt)) return(character(0))
    pat <- "(?:run_r|run_stata)\\s*\\(\\s*(?:here::here|here)\\s*\\(([^)]+)\\)"
    m <- regmatches(txt, gregexpr(pat, txt, ignore.case = TRUE))[[1]]
    roots <- character(0)
    for (x in m) {
      args_str <- sub(pat, "\\1", x, ignore.case = TRUE)
      a <- gsub('["\']', "", gsub("\\s*,\\s*", "/", args_str))
      a <- trimws(gsub("\\s+", "", a))
      if (nzchar(a)) {
        segs <- strsplit(a, "/")[[1]]
        if (length(segs) >= 1 && nzchar(segs[1])) roots <- c(roots, segs[1])
      }
    }
    roots
  }
  for (rel in r_rel) {
    if (!is_setup_or_master_file(rel)) next
    path_roots_from_master <- c(path_roots_from_master, scan_for_here_roots(rel))
  }
  path_roots_from_master <- unique(path_roots_from_master)

  # 4. Path roots from setup vars (directory paths from setup/master file assignments)
  for (v in names(setup_vars)) {
    val <- trimws(setup_vars[v])
    if (!nzchar(val)) next
    val <- gsub("\\\\", "/", val)
    val <- sub("/+$", "", val)
    if (nzchar(val) && !grepl("\\.(rds|csv|dta|rda|xlsx|rdata|gpkg|shp)$", val, ignore.case = TRUE))
      path_roots_from_setup <- c(path_roots_from_setup, val)
  }
  path_roots_from_setup <- unique(path_roots_from_setup)

  setup_master_files <- rel_paths[vapply(rel_paths, is_setup_or_master_file, logical(1))]
  list(
    master_path = master_path,
    path_roots_from_master = path_roots_from_master,
    path_roots_from_setup = path_roots_from_setup,
    setup_master_files = setup_master_files
  )
}

# Normalize Stata global value for path resolution (slashes, trim only).
stata_global_value_to_canonical <- function(val) {
  if (!nzchar(val)) return(val)
  trimws(gsub("/+", "/", gsub("\\\\", "/", val)))
}

# Infer Stata global names from R code: paste0('global NAME "', normalizePath(R_VAR, winslash = "/"), '"')
# or normalizePath(getwd(), ...) for projdir. Returns stata_global_name -> resolved path.
infer_stata_globals_from_r <- function(parsed, setup_vars, project_path = NULL) {
  out <- character(0)
  proj_norm <- if (length(project_path) > 0 && nzchar(project_path))
    stata_global_value_to_canonical(gsub("\\\\", "/", trimws(project_path))) else character(0)
  r_files <- names(parsed)[vapply(parsed, function(f) is.null(f$file_type) || f$file_type != "stata", logical(1))]
  # Pattern: paste0('global NAME  "', normalizePath(R_VAR, winslash = "/"), '"') - flexible spacing
  pat_var <- "paste0\\s*\\(\\s*['\"]global\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*[\"']\\s*['\"]\\s*,\\s*normalizePath\\s*\\(\\s*([a-zA-Z_][a-zA-Z0-9_.]*)\\s*(?:,\\s*winslash\\s*=\\s*[\"']/[\"']\\s*)?\\)"
  pat_getwd <- "paste0\\s*\\(\\s*['\"]global\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*[\"']\\s*['\"]\\s*,\\s*normalizePath\\s*\\(\\s*getwd\\s*\\(\\s*\\)\\s*(?:,\\s*winslash\\s*=\\s*[\"']/[\"']\\s*)?\\)"
  for (rel in r_files) {
    txt <- tryCatch(readLines(parsed[[rel]]$path, warn = FALSE), error = function(e) character(0))
    if (length(txt) == 0) next
    full_txt <- paste(txt, collapse = "\n")
    m <- regmatches(full_txt, gregexpr(pat_var, full_txt))[[1]]
    for (x in m) {
      stata_name <- sub(pat_var, "\\1", x)
      r_var <- sub(pat_var, "\\2", x)
      if (!nzchar(stata_name) || !nzchar(r_var)) next
      if (r_var %in% names(setup_vars)) {
        val <- sub("/+$", "", setup_vars[r_var])
        if (nzchar(val)) out[stata_name] <- stata_global_value_to_canonical(val)
      }
    }
    m2 <- regmatches(full_txt, gregexpr(pat_getwd, full_txt))[[1]]
    for (x in m2) {
      stata_name <- sub(pat_getwd, "\\1", x)
      if (nzchar(stata_name) && nzchar(proj_norm)) out[stata_name] <- proj_norm
    }
  }
  out
}

# Collect Stata globals from .do files (global name = "value") for path resolution.
# Stata globals set by R at runtime are inferred via infer_stata_globals_from_r; this adds any defined inside .do files.
parse_stata_globals <- function(parsed) {
  globals <- character(0)
  do_files <- names(parsed)[vapply(parsed, function(f) identical(f$file_type, "stata"), logical(1))]
  for (rel in do_files) {
    txt <- tryCatch(readLines(parsed[[rel]]$path, warn = FALSE), error = function(e) character(0))
    if (length(txt) == 0) next
    full_txt <- paste(txt, collapse = "\n")
    pat <- 'global\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s+(?:=\\s*)?["\']([^"\']*)["\']'
    m <- regmatches(full_txt, gregexpr(pat, full_txt, ignore.case = TRUE))[[1]]
    for (x in m) {
      name <- sub(pat, "\\1", x, ignore.case = TRUE)
      val <- sub(pat, "\\2", x, ignore.case = TRUE)
      val <- trimws(gsub("\\\\", "/", val))
      if (!nzchar(name)) next
      if (!nzchar(val) || !is_path_like_value(val)) next
      val_canon <- stata_global_value_to_canonical(val)
      if (nzchar(val_canon)) globals[name] <- val_canon
    }
  }
  globals
}

# TRUE if basename looks like setup/master/config (flexible: SSA_env_SetUp, master.R, etc.)
# Used to choose which files to scan for path definitions (R and Stata).
is_setup_or_master_file <- function(rel_path) {
  base <- tolower(basename(rel_path))
  grepl("setup|master|set_up|set\\.?up|config|init|00_|env.*setup", base, ignore.case = TRUE)
}

# Scan setup/master-named files (R and Stata) for path variables. Works with only setup, only master, or both.
parse_setup_vars <- function(parsed) {
  all_rel <- names(parsed)
  # Include (1) files with no sources and setup-like name, (2) any file whose name suggests setup or master
  no_sources <- vapply(parsed, function(f) {
    (is.null(f$file_type) || f$file_type != "stata") && length(f$sources) == 0
  }, logical(1))
  by_name <- vapply(all_rel, is_setup_or_master_file, logical(1))
  setup_candidates <- unique(c(
    all_rel[no_sources & grepl("setup|config|00_|init", basename(all_rel), ignore.case = TRUE)],
    all_rel[by_name]
  ))
  if (length(setup_candidates) == 0) {
    setup_candidates <- names(parsed)[no_sources]
    if (is.null(setup_candidates)) setup_candidates <- character(0)
  }
  vars <- character(0)
  for (rel in setup_candidates) {
    f <- parsed[[rel]]
    if (is.null(f)) next
    # R/Rmd: path assignments (raw.dir <- "data/Raw", build.dir = "data/Build")
    if (is.null(f$file_type) || f$file_type != "stata") {
      txt <- tryCatch(readLines(f$path, warn = FALSE), error = function(e) character(0))
      if (length(txt) > 0) {
        full_txt <- paste(txt, collapse = "\n")
        pat_assign <- '([a-zA-Z][a-zA-Z0-9_.]*)\\s*(?:<-|=)\\s*["\']([^"\']+)["\']'
        m <- regmatches(full_txt, gregexpr(pat_assign, full_txt))[[1]]
        for (x in m) {
          var <- sub(pat_assign, "\\1", x)
          val <- trimws(gsub("\\\\", "/", sub(pat_assign, "\\2", x)))
          if (!nzchar(var) || !nzchar(val)) next
          if (is_path_like_value(val) || is_dir_segment_value(var, val)) vars[var] <- val
        }
        # Also capture lightweight, static path expressions commonly used in setup files:
        #   raw_dir <- here(data_dir, "raw")
        #   cln_dir <- here::here("data", "clean")
        #   cln_dir <- file.path(data_dir, "clean")
        # We do NOT execute code; we only join args to a project-relative path and then resolve identifiers
        # via other setup vars (e.g. data_dir).
        pat_here <- '([a-zA-Z][a-zA-Z0-9_.]*)\\s*(?:<-|=)\\s*(?:here::here|here)\\s*\\(([^)]*)\\)'
        m_here <- regmatches(full_txt, gregexpr(pat_here, full_txt, ignore.case = TRUE, perl = TRUE))[[1]]
        for (x in m_here) {
          var <- trimws(sub(pat_here, "\\1", x, ignore.case = TRUE, perl = TRUE))
          args <- sub(pat_here, "\\2", x, ignore.case = TRUE, perl = TRUE)
          # Only accept simple here()/here::here() arg lists (identifiers + string literals).
          # Skip if args include nested calls like paste0(...), normalizePath(...), etc.
          if (grepl("[A-Za-z_][A-Za-z0-9_.]*\\s*\\(", args, perl = TRUE)) next
          p <- paste_here_args(args, mark_identifiers = TRUE)
          if (nzchar(var) && nzchar(p)) vars[var] <- p
        }
        pat_fp <- '([a-zA-Z][a-zA-Z0-9_.]*)\\s*(?:<-|=)\\s*file\\.path\\s*\\(([^)]*)\\)'
        m_fp <- regmatches(full_txt, gregexpr(pat_fp, full_txt, ignore.case = TRUE, perl = TRUE))[[1]]
        for (x in m_fp) {
          var <- trimws(sub(pat_fp, "\\1", x, ignore.case = TRUE, perl = TRUE))
          args <- sub(pat_fp, "\\2", x, ignore.case = TRUE, perl = TRUE)
          if (grepl("[A-Za-z_][A-Za-z0-9_.]*\\s*\\(", args, perl = TRUE)) next
          p <- paste_here_args(args, mark_identifiers = TRUE)
          if (nzchar(var) && nzchar(p)) vars[var] <- p
        }
      }
    }
    # Stata .do: global name "value" (add to same setup_vars for consistent resolution)
    if (identical(f$file_type, "stata")) {
      txt <- tryCatch(readLines(f$path, warn = FALSE), error = function(e) character(0))
      if (length(txt) > 0) {
        full_txt <- paste(txt, collapse = "\n")
        pat <- 'global\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s+(?:=\\s*)?["\']([^"\']*)["\']'
        m <- regmatches(full_txt, gregexpr(pat, full_txt, ignore.case = TRUE))[[1]]
        for (x in m) {
          name <- sub(pat, "\\1", x, ignore.case = TRUE)
          val <- trimws(gsub("\\\\", "/", sub(pat, "\\2", x, ignore.case = TRUE)))
          if (!nzchar(name) || !nzchar(val)) next
          if (is_path_like_value(val) || is_dir_segment_value(name, val)) vars[name] <- val
        }
      }
    }
  }
  resolve_setup_var_values(vars)
}

resolve_setup_var_values <- function(vars) {
  if (length(vars) == 0) return(vars)
  out <- vars
  for (iter in 1:10) {
    changed <- FALSE
    all_vars <- out
    for (n in names(out)) {
      v <- out[n]
      if (!nzchar(v)) next
      # Resolve "${var}" tokens and other known identifiers inside the value (folder-safe).
      res <- resolve_path_args(v, all_vars, character(0), allow_folder = TRUE)
      if (length(res) > 0 && nzchar(res[1]) && !identical(res[1], v)) {
        out[n] <- res[1]
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  out
}

resolve_paths_with_setup <- function(parsed, setup_vars, project_path = NULL) {
  for (rel in names(parsed)) {
    f <- parsed[[rel]]
    if (!is.null(f$file_type) && f$file_type == "stata") {
      # If Stata has already been resolved via analyze_stata_project(), keep those results.
      if (!isTRUE(f$stata_resolved)) {
        rd <- resolve_stata_paths_with_notes(
          f$data_reads %||% character(0),
          setup_vars,
          project_path,
          assume_dta = f$data_reads_assume_dta %||% NULL
        )
        wr <- resolve_stata_paths_with_notes(
          f$data_writes %||% character(0),
          setup_vars,
          project_path,
          assume_dta = f$data_writes_assume_dta %||% NULL
        )
        f$data_reads <- rd$paths
        f$data_read_notes <- rd$notes
        f$data_writes <- wr$paths
        f$data_write_notes <- wr$notes
      }
    } else {
      local_vars <- if (length(f$local_path_vars) > 0) f$local_path_vars else character(0)
      local_vars <- resolve_local_path_vars(local_vars, setup_vars)
      f$sources <- resolve_source_paths(f$sources, setup_vars, local_vars)
      f$data_reads <- resolve_path_args(f$data_reads, setup_vars, local_vars, allow_folder = FALSE)
      f$data_writes <- resolve_path_args(f$data_writes, setup_vars, local_vars, allow_folder = FALSE)
      if (length(f$data_read_folders) > 0) {
        f$data_read_folders <- resolve_path_args(f$data_read_folders, setup_vars, local_vars, allow_folder = TRUE)
        f$data_read_folders <- unique(f$data_read_folders[nzchar(f$data_read_folders)])
      }
      # Normalize to deterministic, project-style dataset IDs (reduces full-path vs basename duplication).
      f$data_reads <- canonical_data_id(f$data_reads)
      f$data_writes <- canonical_data_id(f$data_writes)
      if (length(f$data_read_folders) > 0) f$data_read_folders <- canonical_data_id(f$data_read_folders)
    }
    f$data_reads <- unique(f$data_reads[nzchar(f$data_reads)])
    f$data_writes <- unique(f$data_writes[nzchar(f$data_writes)])
    f$sources <- unique(f$sources[nzchar(f$sources)])
    parsed[[rel]] <- f
  }
  parsed
}

# Resolve Stata paths: substitute ${var} and $var with setup_vars (Stata globals).
# Make project-relative and canonicalize to deterministic dataset IDs.
# Returns both resolved paths and (optional) per-path notes (e.g. assumed .dta).
resolve_stata_paths_with_notes <- function(paths, setup_vars, project_path = NULL, assume_dta = NULL) {
  if (length(paths) == 0) return(list(paths = character(0), notes = character(0)))
  out <- character(0)
  notes <- character(0)
  setup_l <- if (length(setup_vars) > 0) setNames(unname(setup_vars), tolower(names(setup_vars))) else character(0)
  if (is.null(assume_dta)) assume_dta <- rep(FALSE, length(paths))
  if (length(assume_dta) != length(paths)) assume_dta <- rep(FALSE, length(paths))
  for (i in seq_along(paths)) {
    p <- paths[[i]]
    p <- gsub("\\\\", "/", trimws(p))
    if (!nzchar(p)) next
    for (iter in 1:10) {
      changed <- FALSE
      m_brace <- regmatches(p, gregexpr("\\$\\{[a-zA-Z_][a-zA-Z0-9_]*\\}", p, perl = TRUE))[[1]]
      for (v in m_brace) {
        name <- gsub("^\\$\\{|\\}$", "", v)
        key <- tolower(name)
        repl <- if (length(setup_l) > 0 && key %in% names(setup_l)) sub("/+$", "", setup_l[[key]]) else ""
        p <- sub(v, repl, p, fixed = TRUE)
        changed <- TRUE
      }
      m_plain <- regmatches(p, gregexpr("\\$[a-zA-Z_][a-zA-Z0-9_]*", p, perl = TRUE))[[1]]
      for (v in m_plain) {
        name <- sub("^\\$", "", v)
        key <- tolower(name)
        repl <- if (length(setup_l) > 0 && key %in% names(setup_l)) sub("/+$", "", setup_l[[key]]) else ""
        p <- sub(v, repl, p, fixed = TRUE)
        changed <- TRUE
      }
      if (!changed) break
    }
    p <- gsub("/+", "/", p)
    p <- sub("/+$", "", p)
    if (!nzchar(p)) next

    # Stata convention: dataset paths may omit ".dta"
    assumed <- FALSE
    if (isTRUE(assume_dta[[i]])) {
      bn <- basename(p)
      has_ext <- grepl("\\.[A-Za-z0-9]+$", bn)
      if (!has_ext) {
        p <- paste0(p, ".dta")
        assumed <- TRUE
      }
    }

    p <- stata_path_to_project_relative(p, project_path)
    p <- stata_canonical_data_path(p)
    p <- canonical_data_id(p)
    if (nzchar(p) && is_valid_data_file_path(p)) {
      out <- c(out, p)
      if (assumed) notes[p] <- "assumed .dta"
    }
  }
  list(paths = unique(out), notes = notes)
}

# Resolve Stata paths: substitute ${var} and $var with setup_vars (Stata globals).
# Make project-relative so they match R paths (e.g. employment.dta from 3-Employment.R).
resolve_stata_paths <- function(paths, setup_vars, project_path = NULL) {
  resolve_stata_paths_with_notes(paths, setup_vars, project_path, assume_dta = NULL)$paths
}

# If path contains /FINAL/, /BUILD/, or /RAW/ (any case), normalize to data/Final/, data/Build/, data/Raw/
# so Stata paths match R (e.g. ${final}/employment.dta, ${projdir}/DATA/BUILD/Africa/file.dta).
# Handles any depth under raw/build/final so Stata-R cross-dependencies match.
# Normalize path for matching only (collapse repeated slashes). No project-specific folder names.
stata_canonical_data_path <- function(p) {
  p <- gsub("\\\\", "/", p)
  p <- gsub("/+", "/", p)
  p <- sub("/+$", "", p)
  if (!nzchar(p)) return(p)

  segs <- strsplit(p, "/", fixed = TRUE)[[1]]
  segs <- segs[nzchar(segs)]
  if (length(segs) == 0) return(p)
  segs_l <- tolower(segs)

  # If a path contains ".../data/<raw|build|final>/...", normalize the <raw|build|final> segment case.
  i_data <- which(segs_l == "data")
  if (length(i_data) > 0) {
    i <- i_data[1]
    if (length(segs) >= i + 1L && segs_l[i + 1L] %in% c("raw", "build", "final")) {
      tier <- tools::toTitleCase(segs_l[i + 1L])
      rest <- if (length(segs) > i + 1L) segs[(i + 2L):length(segs)] else character(0)
      return(paste(c("data", tier, rest), collapse = "/"))
    }
  }

  # Otherwise, if the path contains a RAW/BUILD/FINAL folder (anywhere), anchor from there and map to data/<Tier>/...
  i_tier <- which(segs_l %in% c("raw", "build", "final"))
  if (length(i_tier) > 0) {
    i <- tail(i_tier, 1)
    tier <- tools::toTitleCase(segs_l[i])
    rest <- if (length(segs) > i) segs[(i + 1L):length(segs)] else character(0)
    return(paste(c("data", tier, rest), collapse = "/"))
  }

  p
}

# If path is absolute and under project_path, return project-relative path so Stata and R match.
stata_path_to_project_relative <- function(p, project_path) {
  if (length(project_path) == 0 || !nzchar(project_path)) return(p)
  p <- gsub("\\\\", "/", p)
  proj_norm <- gsub("\\\\", "/", trimws(project_path))
  if (!nzchar(proj_norm)) return(p)
  is_abs <- grepl("^[A-Za-z]:", p) || startsWith(p, "/")
  if (!is_abs) return(p)

  # Try to normalize absolute paths (when possible) for reliable prefix stripping.
  p_abs <- if (grepl("^[A-Za-z]:", p)) {
    tryCatch(gsub("\\\\", "/", normalizePath(p, mustWork = FALSE)), error = function(e) p)
  } else {
    p
  }
  proj_abs <- tryCatch(normalizePath(proj_norm, mustWork = TRUE), error = function(e) proj_norm)
  proj_abs <- gsub("\\\\", "/", proj_abs)
  if (startsWith(p_abs, proj_abs)) {
    p <- sub(proj_abs, "", p_abs, fixed = TRUE)
    p <- sub("^/+", "", p)
    return(p)
  }

  # External absolute path: avoid creating junk dataset IDs like "C:/Users/...".
  # Prefer canonical project-style data paths if we can anchor on /data/ or a Raw/Build/Final tier.
  # Otherwise, keep only the basename as a match key.
  p2 <- normalize_path_canonical(p_abs)
  if (grepl("(^|/)data/", p2, ignore.case = TRUE) ||
      grepl("(^|/)(raw|build|final)(/|$)", p2, ignore.case = TRUE) ||
      grepl("(^|/)code(/|$)", p2, ignore.case = TRUE)) {
    return(p2)
  }
  basename(p2)
}

# Anchor any path containing a /code/ segment to a repo-style "code/..." prefix (case-normalized).
stata_canonical_code_path <- function(p) {
  p <- normalize_path_canonical(p)
  if (!nzchar(p)) return(p)
  segs <- strsplit(p, "/", fixed = TRUE)[[1]]
  segs <- segs[nzchar(segs)]
  if (length(segs) == 0) return(p)
  segs_l <- tolower(segs)
  i_code <- which(segs_l == "code")
  if (length(i_code) == 0) return(p)
  i <- tail(i_code, 1)
  rest <- if (length(segs) > i) segs[(i + 1L):length(segs)] else character(0)
  paste(c("code", rest), collapse = "/")
}

# --- Stata project pass: globals + do/run/include call graph -------------------
stata_is_truthy <- function(x) {
  if (length(x) == 0 || is.null(x)) return(FALSE)
  tolower(trimws(as.character(x[1]))) %in% c("1", "true", "yes", "y", "on")
}

stata_strip_comments <- function(txt) {
  if (!nzchar(txt)) return(txt)
  # Block comments: /* ... */
  txt <- gsub("(?s)/\\*.*?\\*/", "", txt, perl = TRUE)
  # Full-line comments: * ...
  txt <- gsub("(?m)^\\s*\\*.*$", "", txt, perl = TRUE)
  # Inline // comments (best-effort; Stata also uses /// for continuation)
  txt <- gsub("(?m)//[^/].*$", "", txt, perl = TRUE)
  txt
}

stata_split_semicolon_statements <- function(txt) {
  # Split by semicolons outside of single/double quotes.
  if (!nzchar(txt)) return(list(parts = character(0), rest = ""))
  parts <- character(0)
  cur <- character(0)
  in_sq <- FALSE
  in_dq <- FALSE
  chars <- strsplit(txt, "", fixed = TRUE)[[1]]
  for (ch in chars) {
    if (ch == "\"" && !in_sq) {
      in_dq <- !in_dq
      cur <- c(cur, ch)
      next
    }
    if (ch == "'" && !in_dq) {
      in_sq <- !in_sq
      cur <- c(cur, ch)
      next
    }
    if (ch == ";" && !in_sq && !in_dq) {
      parts <- c(parts, paste(cur, collapse = ""))
      cur <- character(0)
      next
    }
    cur <- c(cur, ch)
  }
  list(parts = parts, rest = paste(cur, collapse = ""))
}

stata_preprocess_for_parsing <- function(lines) {
  # Join /// continuations, handle #delimit ; ... #delimit cr, and normalize statement boundaries.
  # Output is newline-separated statements (best-effort), which makes downstream regex extraction robust.
  if (length(lines) == 0) return("")
  if (length(lines) == 1 && grepl("\n", lines, fixed = TRUE)) lines <- strsplit(lines, "\n", fixed = TRUE)[[1]]
  lines <- gsub("\r$", "", lines)

  delim <- "cr"
  semi_buf <- ""
  out <- character(0)
  i <- 1L
  while (i <= length(lines)) {
    ln0 <- lines[[i]]
    ln <- ln0

    if (grepl("(?i)^\\s*#delimit\\s+;\\b", ln, perl = TRUE)) {
      delim <- ";"
      i <- i + 1L
      next
    }
    if (grepl("(?i)^\\s*#delimit\\s+cr\\b", ln, perl = TRUE)) {
      delim <- "cr"
      if (nzchar(trimws(semi_buf))) out <- c(out, semi_buf)
      semi_buf <- ""
      i <- i + 1L
      next
    }

    # Join explicit continuations: line ending with /// (after trimming).
    repeat {
      ln_trim <- sub("\\s+$", "", ln)
      if (grepl("///\\s*$", ln_trim, perl = TRUE)) {
        ln_trim <- sub("///\\s*$", "", ln_trim, perl = TRUE)
        i <- i + 1L
        if (i <= length(lines)) {
          ln <- paste0(ln_trim, " ", trimws(lines[[i]]))
          next
        } else {
          ln <- ln_trim
          break
        }
      }
      ln <- ln_trim
      break
    }

    # Best-effort multiline join: "do" / "run" / "include" on its own line; or a line ending in "using".
    repeat {
      lnt <- trimws(ln)
      if (i < length(lines) &&
          (grepl("(?i)^(do|run|include)\\s*$", lnt, perl = TRUE) ||
             grepl("(?i)^(use|save|merge|append)\\s*$", lnt, perl = TRUE) ||
             grepl("(?i)\\busing\\s*$", lnt, perl = TRUE))) {
        i <- i + 1L
        ln <- paste0(lnt, " ", trimws(lines[[i]]))
        next
      }
      break
    }

    if (!nzchar(trimws(ln))) {
      i <- i + 1L
      next
    }

    if (identical(delim, ";")) {
      semi_buf <- trimws(paste(semi_buf, ln))
      sp <- stata_split_semicolon_statements(semi_buf)
      parts <- trimws(sp$parts)
      parts <- parts[nzchar(parts)]
      if (length(parts) > 0) out <- c(out, parts)
      semi_buf <- sp$rest
    } else {
      if (nzchar(trimws(semi_buf))) {
        out <- c(out, trimws(semi_buf))
        semi_buf <- ""
      }
      out <- c(out, ln)
    }

    i <- i + 1L
  }
  if (nzchar(trimws(semi_buf))) out <- c(out, trimws(semi_buf))
  paste(out, collapse = "\n")
}

stata_unquote_token <- function(x) {
  if (!nzchar(x)) return(x)
  x <- trimws(x)
  if ((startsWith(x, "\"") && endsWith(x, "\"")) || (startsWith(x, "'") && endsWith(x, "'"))) {
    return(substr(x, 2, nchar(x) - 1))
  }
  x
}

extract_stata_sources <- function(txt) {
  txt <- stata_strip_comments(txt)
  pat <- "(?im)\\b(do|run|include)\\b\\s+(\"[^\"]+\"|'[^']+'|[^\\s,;]+)"
  m <- regmatches(txt, gregexpr(pat, txt, perl = TRUE))[[1]]
  if (length(m) == 0) return(list(paths = character(0), kinds = character(0), assume_do = logical(0)))
  kinds <- tolower(sub(pat, "\\1", m, perl = TRUE))
  toks <- sub(pat, "\\2", m, perl = TRUE)
  paths <- vapply(toks, function(t) stata_unquote_token(t), character(1))
  paths <- gsub("\\\\", "/", paths)
  paths <- paths[nzchar(paths)]
  # Assume .do when no extension is present
  assume_do <- !grepl("\\.[A-Za-z0-9]+$", basename(paths))
  list(paths = paths, kinds = kinds, assume_do = assume_do)
}

is_stata_global_value_candidate <- function(val) {
  if (!nzchar(val)) return(FALSE)
  x <- trimws(gsub("\\\\", "/", val))
  x <- gsub("/+", "/", x)
  x <- sub("/+$", "", x)
  if (!nzchar(x)) return(FALSE)
  if (x %in% c("/", "\\", "_")) return(FALSE)
  if (grepl("`", x, fixed = TRUE)) return(FALSE)
  # Pure macro reference (e.g. $outdir, ${projdir}) is allowed; it will be resolved later.
  if (grepl("^\\$\\{?[A-Za-z_][A-Za-z0-9_]*\\}?$", x)) return(TRUE)
  # Allow Stata macro references ($x, ${x}) but require something path-like.
  if (grepl("\\s", x) && !grepl("/", x)) return(FALSE)
  grepl("/", x) || grepl("^[A-Za-z]:", x) || grepl("^(data|code)/", x, ignore.case = TRUE)
}

extract_stata_global_assignments <- function(txt) {
  txt <- stata_strip_comments(txt)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  out_names <- character(0)
  out_vals <- character(0)
  for (ln in lines) {
    m <- regmatches(ln, regexec("(?i)^\\s*global\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*(?:=\\s*)?(.*)$", ln, perl = TRUE))[[1]]
    if (length(m) < 3) next
    name <- m[2]
    rest <- trimws(m[3])
    if (!nzchar(name) || !nzchar(rest)) next
    # Extract first token; value with spaces must be quoted in Stata
    val <- if (startsWith(rest, "\"")) {
      sub("^\"([^\"]*)\".*$", "\\1", rest)
    } else if (startsWith(rest, "'")) {
      sub("^'([^']*)'.*$", "\\1", rest)
    } else {
      sub("^([^\\s,;]+).*$", "\\1", rest)
    }
    val <- gsub("\\\\", "/", trimws(val))
    if (!nzchar(val)) next
    if (!is_stata_global_value_candidate(val)) next
    out_names <- c(out_names, name)
    out_vals <- c(out_vals, val)
  }
  # Preserve order; last assignment wins at runtime, but order matters for within-file expansion.
  list(name = out_names, value = out_vals)
}

stata_expand_globals <- function(x, env) {
  if (!nzchar(x)) return(x)
  if (length(env) == 0) return(x)
  env_l <- setNames(unname(env), tolower(names(env)))
  out <- x
  for (iter in 1:10) {
    before <- out
    # ${name}
    m_brace <- regmatches(out, gregexpr("\\$\\{[A-Za-z_][A-Za-z0-9_]*\\}", out, perl = TRUE))[[1]]
    for (v in m_brace) {
      key <- tolower(gsub("^\\$\\{|\\}$", "", v))
      repl <- env_l[key]
      repl <- if (length(repl) == 0 || is.na(repl[[1]])) "" else repl[[1]]
      repl <- sub("/+$", "", repl)
      out <- sub(v, repl, out, fixed = TRUE)
    }
    # $name
    m_plain <- regmatches(out, gregexpr("\\$[A-Za-z_][A-Za-z0-9_]*", out, perl = TRUE))[[1]]
    for (v in m_plain) {
      key <- tolower(sub("^\\$", "", v))
      repl <- env_l[key]
      repl <- if (length(repl) == 0 || is.na(repl[[1]])) "" else repl[[1]]
      repl <- sub("/+$", "", repl)
      out <- sub(v, repl, out, fixed = TRUE)
    }
    if (identical(out, before)) break
  }
  out
}

stata_score_global_value <- function(val, project_path = NULL) {
  if (!nzchar(val)) return(-Inf)
  v <- stata_global_value_to_canonical(val)
  if (nzchar(project_path)) v <- stata_path_to_project_relative(v, project_path)
  v <- stata_canonical_data_path(v)
  v <- stata_canonical_code_path(v)
  score <- 0
  if (grepl("^data/(raw|build|final)(/|$)", v, ignore.case = TRUE)) score <- score + 80
  if (grepl("^code(/|$)", v, ignore.case = TRUE)) score <- score + 70
  if (grepl("(^|/)data/(raw|build|final)(/|$)", v, ignore.case = TRUE)) score <- score + 40
  if (grepl("(^|/)code(/|$)", v, ignore.case = TRUE)) score <- score + 35
  if (grepl("/", v, fixed = TRUE)) score <- score + 5
  if (grepl("^\\$\\{?[A-Za-z_][A-Za-z0-9_]*\\}?$", v)) score <- score - 15
  if (grepl("\\s", v)) score <- score - 2
  score - (nchar(v) / 10000)
}

stata_apply_global_assignments <- function(parent_env, assignments, project_path = NULL, debug_env = NULL) {
  env <- parent_env
  if (is.null(assignments) || length(assignments$name) == 0) return(env)
  seen_scores <- setNames(numeric(0), character(0))
  seen_vals <- setNames(character(0), character(0))
  pick_name_key <- function(nm, env) {
    if (length(env) == 0) return(nm)
    hit <- names(env)[tolower(names(env)) == tolower(nm)]
    if (length(hit) > 0) hit[1] else nm
  }
  for (i in seq_along(assignments$name)) {
    n <- assignments$name[[i]]
    v <- assignments$value[[i]]
    if (!nzchar(n) || !nzchar(v)) next
    v2 <- stata_expand_globals(v, env)
    v2 <- stata_global_value_to_canonical(v2)
    if (nzchar(project_path)) v2 <- stata_path_to_project_relative(v2, project_path)
    v2 <- stata_canonical_data_path(v2)
    v2 <- stata_canonical_code_path(v2)
    if (!nzchar(v2)) next
    key <- tolower(n)
    if (!is.null(debug_env)) {
      if (is.null(debug_env$candidates)) debug_env$candidates <- list()
      cur <- debug_env$candidates[[key]] %||% character(0)
      debug_env$candidates[[key]] <- unique(c(cur, v2))
    }
    score <- stata_score_global_value(v2, project_path)
    cur_score <- seen_scores[key]
    cur_score <- if (length(cur_score) == 0) NA_real_ else cur_score[[1]]
    cur_val <- seen_vals[key]
    cur_val <- if (length(cur_val) == 0) "" else cur_val[[1]]
    better <- is.na(cur_score) || score > cur_score ||
      (isTRUE(all.equal(score, cur_score)) && nzchar(cur_val) && nchar(v2) < nchar(cur_val))
    if (better) {
      n_key <- pick_name_key(n, env)
      env[n_key] <- v2
      seen_scores[key] <- score
      seen_vals[key] <- v2
      if (!is.null(debug_env)) {
        if (is.null(debug_env$chosen)) debug_env$chosen <- character(0)
        debug_env$chosen[key] <- v2
      }
    }
  }
  env
}

stata_match_do_file <- function(p, do_ids) {
  if (!nzchar(p)) return("")
  if (!grepl("\\.[A-Za-z0-9]+$", basename(p))) p <- paste0(p, ".do")
  # Exact match first
  if (p %in% do_ids) return(p)
  p_l <- tolower(p)
  do_l <- tolower(do_ids)
  hit <- do_ids[do_l == p_l]
  if (length(hit) == 1) return(hit)
  # Basename match if unique
  bn <- tolower(basename(p))
  hit2 <- do_ids[tolower(basename(do_ids)) == bn]
  if (length(hit2) == 1) return(hit2)
  p
}

stata_is_relative_target <- function(p_raw) {
  if (!nzchar(p_raw)) return(FALSE)
  p0 <- gsub("\\\\", "/", trimws(p_raw))
  if (!nzchar(p0)) return(FALSE)
  if (grepl("^[A-Za-z]:", p0) || startsWith(p0, "/")) return(FALSE)
  if (grepl("^\\$\\{?[A-Za-z_][A-Za-z0-9_]*\\}?", p0)) return(FALSE)
  if (grepl("^(data|code)/", p0, ignore.case = TRUE)) return(FALSE)
  TRUE
}

stata_disambiguate_do_match <- function(target, matches, caller_dir = NULL, raw_relative = FALSE) {
  if (length(matches) == 0) return(list(path = target, note = "unresolved", ambiguous = character(0)))
  if (length(matches) == 1) return(list(path = matches[[1]], note = "", ambiguous = character(0)))

  if (isTRUE(raw_relative) && nzchar(caller_dir)) {
    same_dir <- matches[tolower(dirname(matches)) == tolower(caller_dir)]
    if (length(same_dir) == 1) return(list(path = same_dir[[1]], note = "resolved relative to caller", ambiguous = character(0)))
  }

  lens <- nchar(matches)
  best_len <- min(lens)
  best <- matches[lens == best_len]
  if (length(best) == 1) {
    return(list(path = best[[1]], note = paste0("basename match (shortest of ", length(matches), ")"), ambiguous = setdiff(matches, best)))
  }

  amb_id <- paste0("ambiguous_do/", basename(target))
  list(path = amb_id, note = paste0("ambiguous: ", paste(basename(matches), collapse = ", ")), ambiguous = matches)
}

stata_resolve_source_targets <- function(raw_paths, assume_do, env, project_path, do_ids, caller_rel = NULL) {
  if (length(raw_paths) == 0) return(list(paths = character(0), notes = character(0)))
  out <- character(0)
  notes <- character(0)
  caller_dir <- if (!is.null(caller_rel) && nzchar(caller_rel)) dirname(gsub("\\\\", "/", caller_rel)) else ""
  if (caller_dir == ".") caller_dir <- ""

  do_l <- tolower(do_ids)
  for (i in seq_along(raw_paths)) {
    p_raw <- raw_paths[[i]]
    if (!nzchar(p_raw)) next
    p_raw <- gsub("\\\\", "/", trimws(p_raw))
    raw_relative <- stata_is_relative_target(p_raw)

    # Caller-relative disambiguation: Stata resolves unqualified targets relative to the caller's directory.
    if (isTRUE(raw_relative) && nzchar(caller_dir)) {
      p_rel <- sub("^\\./+", "", p_raw)
      if (!grepl("\\.[A-Za-z0-9]+$", basename(p_rel))) p_rel <- paste0(p_rel, ".do")
      cand <- normalize_path_canonical(paste(c(caller_dir, p_rel), collapse = "/"))
      hit <- do_ids[do_l == tolower(cand)]
      if (length(hit) == 1) {
        out <- c(out, hit[[1]])
        notes[hit[[1]]] <- "resolved relative to caller"
        next
      }
    }

    p <- stata_expand_globals(p_raw, env)
    p <- gsub("/+", "/", p)
    p <- sub("^\\./+", "", p)
    if (!nzchar(p)) next
    if (!is.null(assume_do) && length(assume_do) >= i && isTRUE(assume_do[[i]])) {
      if (!grepl("\\.[A-Za-z0-9]+$", basename(p))) p <- paste0(p, ".do")
    }
    p <- stata_path_to_project_relative(p, project_path)
    p <- stata_canonical_code_path(p)
    p <- sub("^/+", "", p)

    # Match against known .do files.
    if (p %in% do_ids) {
      out <- c(out, p)
      next
    }
    hit_ci <- do_ids[do_l == tolower(p)]
    if (length(hit_ci) == 1) {
      out <- c(out, hit_ci[[1]])
      next
    }

    # Basename matches (possibly ambiguous).
    p_do <- if (!grepl("\\.[A-Za-z0-9]+$", basename(p))) paste0(p, ".do") else p
    bn <- tolower(basename(p_do))
    hits <- do_ids[tolower(basename(do_ids)) == bn]
    dis <- stata_disambiguate_do_match(p_do, hits, caller_dir = caller_dir, raw_relative = raw_relative)
    out <- c(out, dis$path)
    if (nzchar(dis$note)) notes[dis$path] <- dis$note
  }
  key <- path_norm_for_group(out)
  u <- !duplicated(key)
  out_u <- out[u]
  notes_u <- notes[names(notes) %in% out_u]
  list(paths = out_u[nzchar(out_u)], notes = notes_u)
}

detect_stata_master_do <- function(parsed) {
  do_ids <- names(parsed)[vapply(parsed, function(f) identical(f$file_type, "stata"), logical(1))]
  if (length(do_ids) == 0) return(NULL)
  n_globals <- vapply(do_ids, function(rel) {
    a <- parsed[[rel]]$stata_global_assignments
    if (is.null(a) || length(a$name) == 0) 0L else length(a$name)
  }, integer(1))
  cand <- do_ids[n_globals > 0]
  if (length(cand) == 0) return(NULL)
  best <- cand[order(-n_globals[cand], nchar(dirname(cand)), tolower(cand))][1]
  best
}

analyze_stata_project <- function(parsed, project_path, fallback_globals = character(0)) {
  do_ids <- names(parsed)[vapply(parsed, function(f) identical(f$file_type, "stata"), logical(1))]
  if (length(do_ids) == 0) return(list(files = parsed, globals_union = character(0), master_do = NULL))

  master_do <- detect_stata_master_do(parsed)
  env_by <- setNames(vector("list", length(do_ids)), do_ids)
  sources_by <- setNames(vector("list", length(do_ids)), do_ids)
  source_notes_by <- setNames(vector("list", length(do_ids)), do_ids)
  master_debug <- new.env(parent = emptyenv())

  base_env <- fallback_globals
  if (!is.null(master_do) && master_do %in% do_ids) {
    env_by[[master_do]] <- stata_apply_global_assignments(
      base_env,
      parsed[[master_do]]$stata_global_assignments,
      project_path,
      debug_env = master_debug
    )
    queue <- master_do
  } else {
    # No master detected: fall back to a global env for macro expansion (best effort).
    queue <- character(0)
  }
  master_env <- if (!is.null(master_do) && master_do %in% do_ids) (env_by[[master_do]] %||% character(0)) else character(0)
  # Treat Stata master globals as session-wide defaults (many projects assume these are set externally).
  base_env_all <- c(base_env, master_env)

  # Propagate globals along do/run/include call graph.
  seen <- new.env(parent = emptyenv())
  push <- function(x) {
    for (v in x) if (nzchar(v)) assign(v, TRUE, envir = seen)
    x
  }
  queue <- push(queue)

  while (length(queue) > 0) {
    caller <- queue[1]; queue <- queue[-1]
    env <- env_by[[caller]] %||% base_env_all
    f <- parsed[[caller]]
    raw <- f$stata_sources_raw %||% character(0)
    assume_do <- f$stata_sources_assume_do %||% NULL
    res <- stata_resolve_source_targets(raw, assume_do, env, project_path, do_ids, caller_rel = caller)
    sources_by[[caller]] <- res$paths
    source_notes_by[[caller]] <- res$notes
    for (callee in (res$paths %||% character(0))) {
      if (!nzchar(callee) || !(callee %in% do_ids)) next
      env_new <- stata_apply_global_assignments(env, parsed[[callee]]$stata_global_assignments, project_path)
      env_old <- env_by[[callee]] %||% character(0)
      env_equal <- function(a, b) {
        if (length(a) != length(b)) return(FALSE)
        if (length(a) == 0) return(TRUE)
        na <- names(a) %||% character(0)
        nb <- names(b) %||% character(0)
        if (length(na) != length(nb)) return(FALSE)
        ord_a <- order(tolower(na)); ord_b <- order(tolower(nb))
        if (!identical(tolower(na[ord_a]), tolower(nb[ord_b]))) return(FALSE)
        identical(unname(a[ord_a]), unname(b[ord_b]))
      }
      if (length(env_old) == 0 || !env_equal(env_new, env_old)) {
        env_by[[callee]] <- env_new
        if (is.null(get0(callee, envir = seen, inherits = FALSE))) {
          queue <- c(queue, callee)
          assign(callee, TRUE, envir = seen)
        } else {
          queue <- c(queue, callee)
        }
      }
    }
  }

  globals_union <- base_env_all
  # Append in stable order; earlier wins on collisions.
  if (length(env_by) > 0) {
    for (rel in do_ids) {
      e <- env_by[[rel]] %||% character(0)
      for (n in names(e)) {
        cur <- globals_union[n]
        if (length(cur) == 0 || is.na(cur) || !nzchar(cur)) globals_union[n] <- e[n]
      }
    }
  }

  # Apply resolved sources and resolve dataset paths per-file using the in-scope env.
  for (rel in do_ids) {
    f <- parsed[[rel]]
    # Resolve sources even when the file is not reachable from the detected master.
    if (length(sources_by[[rel]] %||% character(0)) == 0) {
      env_tmp <- env_by[[rel]] %||% stata_apply_global_assignments(base_env_all, f$stata_global_assignments, project_path)
      res_tmp <- stata_resolve_source_targets(
        f$stata_sources_raw %||% character(0),
        f$stata_sources_assume_do %||% NULL,
        env_tmp,
        project_path,
        do_ids,
        caller_rel = rel
      )
      sources_by[[rel]] <- res_tmp$paths
      source_notes_by[[rel]] <- res_tmp$notes
    }
    f$sources <- sources_by[[rel]] %||% character(0)
    f$stata_source_notes <- source_notes_by[[rel]] %||% character(0)
    env <- env_by[[rel]] %||% stata_apply_global_assignments(base_env_all, f$stata_global_assignments, project_path)
    f$stata_globals_in_scope <- env

    rd <- resolve_stata_paths_with_notes(
      f$data_reads %||% character(0),
      env,
      project_path,
      assume_dta = f$data_reads_assume_dta %||% NULL
    )
    wr <- resolve_stata_paths_with_notes(
      f$data_writes %||% character(0),
      env,
      project_path,
      assume_dta = f$data_writes_assume_dta %||% NULL
    )
    f$data_reads <- rd$paths
    f$data_read_notes <- rd$notes
    f$data_writes <- wr$paths
    f$data_write_notes <- wr$notes
    f$stata_resolved <- TRUE
    parsed[[rel]] <- f
  }

  global_debug <- list(
    candidates = master_debug$candidates %||% list(),
    chosen = master_debug$chosen %||% character(0)
  )
  list(files = parsed, globals_union = globals_union, master_do = master_do, global_debug = global_debug)
}

# Resolve source paths (e.g. build.code.dir/2_Create_unit_IDs/0-Create_unit_IDs.R) so they match file paths.
resolve_source_paths <- function(paths, setup_vars, local_vars = character(0)) {
  if (length(paths) == 0) return(paths)
  out <- character(0)
  unwrap_var_token <- function(seg) {
    if (!nzchar(seg)) return(seg)
    m <- regmatches(seg, regexec("^\\$\\{([A-Za-z_][A-Za-z0-9_.]*)\\}$", seg))[[1]]
    if (length(m) >= 2) return(m[2])
    m2 <- regmatches(seg, regexec("^\\$([A-Za-z_][A-Za-z0-9_.]*)$", seg))[[1]]
    if (length(m2) >= 2) return(m2[2])
    seg
  }
  for (p in paths) {
    p <- gsub("\\\\", "/", trimws(p))
    if (!nzchar(p)) next
    parts <- strsplit(p, "/")[[1]]
    for (i in seq_along(parts)) {
      seg_raw <- parts[i]
      seg <- unwrap_var_token(seg_raw)
      if (seg %in% names(setup_vars)) parts[i] <- sub("/+$", "", setup_vars[seg])
      else if (seg %in% names(local_vars)) parts[i] <- local_vars[seg]
    }
    p <- paste(parts, collapse = "/")
    p <- gsub("/+", "/", p)
    p <- sub("^/+|/+$", "", p)
    if (nzchar(p)) out <- c(out, p)
  }
  out
}

# Parse a master/orchestrator script and return run_r/run_stata targets in execution order (for graph).
# Uses setup_vars from the project so paths like build.code.dir/... resolve to project-relative paths.
parse_master_ordered_calls <- function(master_path, setup_vars) {
  if (!file.exists(master_path)) return(character(0))
  txt <- tryCatch(paste(readLines(master_path, warn = FALSE), collapse = "\n"), error = function(e) "")
  if (!nzchar(txt)) return(character(0))
  pat_run_r   <- "run_r\\s*\\(\\s*(?:here::here|here)\\s*\\(([^)]+)\\)\\s*\\)"
  pat_run_stata <- "run_stata\\s*\\(\\s*(?:here::here|here)\\s*\\(([^)]+)\\)\\s*\\)"
  out <- character(0)
  pos <- integer(0)
  for (pat in list(pat_run_r, pat_run_stata)) {
    m <- gregexpr(pat, txt, ignore.case = TRUE)[[1]]
    if (length(m) > 0 && m[1] > 0) {
      len <- attr(m, "match.length")
      for (i in seq_along(m)) {
        full <- substr(txt, m[i], m[i] + len[i] - 1)
        args_str <- sub(pat, "\\1", full, ignore.case = TRUE)
        p <- paste_here_args(args_str)
        if (nzchar(p) && grepl("\\.(R|r|Rmd|rmd|DO|do)$", p)) {
          out <- c(out, p)
          pos <- c(pos, m[i])
        }
      }
    }
  }
  if (length(out) == 0) return(character(0))
  out <- out[order(pos)]
  resolve_source_paths(out, setup_vars, character(0))
}

# Resolve path strings: substitute setup vars (raw.dir, build.dir) and same-file local path vars (e.g. countries_dir <- "Countries").
# Paths can be "var,path" (comma-separated) or "var/segment/..." (slash-separated). Multi-pass so all segments resolve.
resolve_path_args <- function(paths, setup_vars, local_vars = character(0), allow_folder = FALSE) {
  if (length(paths) == 0) return(paths)
  all_vars <- c(setup_vars, local_vars)
  unwrap_var_token <- function(seg) {
    if (!nzchar(seg)) return(seg)
    m <- regmatches(seg, regexec("^\\$\\{([A-Za-z_][A-Za-z0-9_.]*)\\}$", seg))[[1]]
    if (length(m) >= 2) return(m[2])
    m2 <- regmatches(seg, regexec("^\\$([A-Za-z_][A-Za-z0-9_.]*)$", seg))[[1]]
    if (length(m2) >= 2) return(m2[2])
    seg
  }
  resolve_one <- function(p) {
    p <- trimws(p)
    if (!nzchar(p)) return(character(0))
    if (grepl("paste0\\s*\\(|\\b[a-z_]+\\([^)]*$", p, ignore.case = TRUE)) return(character(0))
    # Single identifier that is a path variable: e.g. path_var <- "Countries/file.dta"
    if (!grepl("[,/]", p)) {
      key <- unwrap_var_token(p)
      if (key %in% names(all_vars)) {
        res <- normalize_path_canonical(sub("/+$", "", all_vars[key]))
        if (nzchar(res) && is_valid_data_ref(res, allow_folder = allow_folder)) return(res)
        return(character(0))
      }
    }
    if (grepl("[,]", p) && !grepl("^[a-z_]+,[a-z]$", p, ignore.case = TRUE)) {
      parts <- strsplit(p, "\\s*,\\s*")[[1]]
      resolved <- character(length(parts))
      for (i in seq_along(parts)) {
        x <- trimws(gsub('^["\']|["\']$', "", parts[i]))
        x_key <- unwrap_var_token(x)
        if (x_key %in% names(all_vars)) resolved[i] <- sub("/+$", "", all_vars[x_key])
        else if (nzchar(x) && (!grepl("^[a-z_][a-z0-9_.]*$", x, ignore.case = TRUE) || grepl("/|\\.(csv|rds|dta|rda|gpkg|shp|xlsx)$", x, ignore.case = TRUE))) resolved[i] <- x
        else resolved[i] <- x
      }
      new_path <- paste(resolved[resolved != ""], collapse = "/")
      new_path <- gsub("/+", "/", new_path)
      new_path <- normalize_path_canonical(new_path)
      if (nzchar(new_path) && is_valid_data_ref(new_path, allow_folder = allow_folder)) return(new_path)
      return(character(0))
    }
    if (grepl("/", p)) {
      parts <- strsplit(p, "/")[[1]]
      parts <- trimws(parts)
      # Guard against common false positives from dynamic paste0() where a loop/index variable is mistaken for a folder.
      # Example: paste0(l, "/_centroids.rds") where `l` is a level indicator, not a directory.
      first_seg <- unwrap_var_token(parts[1])
      if (nzchar(first_seg) && nchar(first_seg) == 1L && grepl("^[A-Za-z]$", first_seg) && !(first_seg %in% names(all_vars))) {
        return(character(0))
      }
      guess_dir_segment <- function(seg) {
        if (!nzchar(seg)) return(seg)
        # Only for unresolved *_dir/.dir variables used as single path segments.
        seg_key <- unwrap_var_token(seg)
        if (seg_key %in% names(all_vars)) return(seg)
        if (!grepl("^[_A-Za-z0-9.]+$", seg)) return(seg)
        if (!grepl("(_dir|\\.dir)$", seg, ignore.case = TRUE)) return(seg)
        base <- sub("(_dir|\\.dir)$", "", seg, ignore.case = TRUE)
        base <- gsub("[_.]+", " ", base)
        base <- trimws(base)
        if (!nzchar(base)) return(seg)
        tools::toTitleCase(tolower(base))
      }
      # Multi-pass: keep substituting until no segment is a known var (handles countries_dir, ctries_dir, etc.)
      for (pass in 1:10) {
        changed <- FALSE
        for (j in seq_along(parts)) {
          seg_key <- unwrap_var_token(parts[j])
          if (seg_key %in% names(all_vars)) {
            parts[j] <- sub("/+$", "", all_vars[seg_key])
            changed <- TRUE
          }
        }
        if (!changed) break
      }
      # Heuristic fallback: map unresolved *_dir segments to TitleCase folder names (mines_dir -> Mines).
      parts <- vapply(parts, guess_dir_segment, character(1))
      new_path <- paste(parts, collapse = "/")
      new_path <- gsub("/+", "/", new_path)
      new_path <- sub("/+$", "", new_path)
      new_path <- normalize_path_canonical(new_path)
      if (nzchar(new_path) && is_valid_data_ref(new_path, allow_folder = allow_folder)) {
        # If the path still contains unresolved identifiers (e.g. mines_dir/foo.rds), keep it as-is.
        # Do not also add the basename: that creates duplicate dataset IDs and inflates "never read" counts.
        return(new_path)
      }
      return(character(0))
    }
    p2 <- normalize_path_canonical(p)
    if (is_valid_data_ref(p2, allow_folder = allow_folder)) return(p2)
    character(0)
  }
  out <- character(0)
  for (p in paths) {
    res <- resolve_one(p)
    out <- c(out, res)
  }
  out
}

parse_single_stata_file <- function(file_path, project_path, rel_path) {
  txt <- tryCatch(readLines(file_path, warn = FALSE), error = function(e) character(0))
  if (length(txt) == 0) return(NULL)
  full_txt <- stata_preprocess_for_parsing(txt)
  src_info <- extract_stata_sources(full_txt)
  glob_assign <- extract_stata_global_assignments(full_txt)
  rd <- extract_stata_reads(full_txt)
  wr <- extract_stata_writes(full_txt)
  list(
    path = file_path,
    rel_path = rel_path,
    sources = unique(src_info$paths %||% character(0)),
    stata_sources_raw = src_info$paths %||% character(0),
    stata_sources_kinds = src_info$kinds %||% character(0),
    stata_sources_assume_do = src_info$assume_do %||% logical(0),
    stata_source_notes = character(0),
    stata_global_assignments = glob_assign,
    data_reads = unique(rd$paths %||% character(0)),
    data_reads_assume_dta = rd$assume_dta %||% logical(0),
    data_read_folders = character(0),
    data_writes = unique(wr$paths %||% character(0)),
    data_writes_assume_dta = wr$assume_dta %||% logical(0),
    libraries = character(0),
    file_type = "stata",
    stata_resolved = FALSE
  )
}

extract_stata_reads <- function(txt) {
  txt2 <- stata_strip_comments(txt)
  tok_pat <- "(\"[^\"]+\"|'[^']+'|[^\\s,;]+)"
  # Dataset reads: use/merge using/append using
  pat_use_using <- paste0("(?im)^\\s*use\\b[^\\n]*?\\busing\\b\\s+(", tok_pat, ")")
  pat_use_plain <- paste0("(?im)^\\s*use\\b(?![^\\n]*\\busing\\b)\\s+(", tok_pat, ")")
  pat_merge <- paste0("(?im)\\bmerge\\b[^\\n]*?\\busing\\b\\s+(", tok_pat, ")")
  pat_append <- paste0("(?im)\\bappend\\b[^\\n]*?\\busing\\b\\s+(", tok_pat, ")")
  cap <- function(pat) {
    m <- regmatches(txt2, gregexpr(pat, txt2, perl = TRUE))[[1]]
    if (length(m) == 0) return(character(0))
    sub(pat, "\\1", m, perl = TRUE)
  }
  p_ds <- c(cap(pat_use_using), cap(pat_use_plain), cap(pat_merge), cap(pat_append))
  p_ds <- vapply(p_ds, stata_unquote_token, character(1))
  p_ds <- gsub("\\\\", "/", p_ds)
  assume_dta <- !grepl("\\.[A-Za-z0-9]+$", basename(p_ds))

  # Other reads (do not assume .dta)
  pat_imp <- paste0("(?im)\\b(import\\s+delimited|import\\s+excel|insheet)\\b[^\\n]*?(?:using\\b\\s+)?(", tok_pat, ")")
  m2 <- regmatches(txt2, gregexpr(pat_imp, txt2, perl = TRUE))[[1]]
  p_other <- if (length(m2) > 0) sub(pat_imp, "\\2", m2, perl = TRUE) else character(0)
  p_other <- vapply(p_other, stata_unquote_token, character(1))
  p_other <- gsub("\\\\", "/", p_other)

  paths0 <- c(p_ds, p_other)
  assume0 <- c(assume_dta, rep(FALSE, length(p_other)))
  keep <- nzchar(paths0)
  paths0 <- paths0[keep]
  assume0 <- assume0[keep]
  # Drop tempfile refs: path is only backtick-identifier (use `origin')
  keep2 <- !grepl("^`[a-zA-Z_][a-zA-Z0-9_]*'$", paths0)
  paths0 <- paths0[keep2]
  assume0 <- assume0[keep2]

  expand_with_flags <- function(txt, paths, flags) {
    if (length(paths) == 0) return(list(paths = character(0), flags = logical(0)))
    out_p <- character(0)
    out_f <- logical(0)
    for (i in seq_along(paths)) {
      p <- paths[[i]]
      ex <- expand_stata_loop_paths(txt, p)
      if (length(ex) == 0) next
      out_p <- c(out_p, ex)
      out_f <- c(out_f, rep(isTRUE(flags[[i]]), length(ex)))
    }
    list(paths = out_p, flags = out_f)
  }
  ex <- expand_with_flags(txt2, paths0, assume0)
  paths <- ex$paths
  flags <- ex$flags
  if (length(paths) == 0) return(list(paths = character(0), assume_dta = logical(0)))
  # Deduplicate while preserving "assumed" if any duplicate was assumed.
  key <- path_norm_for_group(paths)
  u <- !duplicated(key)
  paths_u <- paths[u]
  flags_u <- vapply(unique(key), function(k) any(flags[key == k]), logical(1))
  list(paths = paths_u, assume_dta = flags_u)
}

extract_stata_writes <- function(txt) {
  txt2 <- stata_strip_comments(txt)
  tok_pat <- "(\"[^\"]+\"|'[^']+'|[^\\s,;]+)"
  # Dataset writes: save
  pat_save <- paste0("(?im)\\bsave\\b[^\\n]*?(?:using\\b\\s+)?(", tok_pat, ")")
  m1 <- regmatches(txt2, gregexpr(pat_save, txt2, perl = TRUE))[[1]]
  p_save <- if (length(m1) > 0) sub(pat_save, "\\1", m1, perl = TRUE) else character(0)
  p_save <- vapply(p_save, stata_unquote_token, character(1))
  p_save <- gsub("\\\\", "/", p_save)
  assume_dta <- !grepl("\\.[A-Za-z0-9]+$", basename(p_save))

  # Other writes (do not assume .dta)
  pat_exp <- paste0("(?im)\\b(export\\s+delimited|export\\s+excel|export)\\b[^\\n]*?(?:using\\b\\s+)?(", tok_pat, ")")
  m2 <- regmatches(txt2, gregexpr(pat_exp, txt2, perl = TRUE))[[1]]
  p_other <- if (length(m2) > 0) sub(pat_exp, "\\2", m2, perl = TRUE) else character(0)
  p_other <- vapply(p_other, stata_unquote_token, character(1))
  p_other <- gsub("\\\\", "/", p_other)

  paths0 <- c(p_save, p_other)
  assume0 <- c(assume_dta, rep(FALSE, length(p_other)))
  keep <- nzchar(paths0)
  paths0 <- paths0[keep]
  assume0 <- assume0[keep]

  expand_with_flags <- function(txt, paths, flags) {
    if (length(paths) == 0) return(list(paths = character(0), flags = logical(0)))
    out_p <- character(0)
    out_f <- logical(0)
    for (i in seq_along(paths)) {
      p <- paths[[i]]
      ex <- expand_stata_loop_paths(txt, p)
      if (length(ex) == 0) next
      out_p <- c(out_p, ex)
      out_f <- c(out_f, rep(isTRUE(flags[[i]]), length(ex)))
    }
    list(paths = out_p, flags = out_f)
  }
  ex <- expand_with_flags(txt2, paths0, assume0)
  paths <- ex$paths
  flags <- ex$flags
  if (length(paths) == 0) return(list(paths = character(0), assume_dta = logical(0)))
  key <- path_norm_for_group(paths)
  u <- !duplicated(key)
  paths_u <- paths[u]
  flags_u <- vapply(unique(key), function(k) any(flags[key == k]), logical(1))
  list(paths = paths_u, assume_dta = flags_u)
}

# Expand paths containing Stata locals like `wave' or `w' by finding forval/foreach in the file
expand_stata_loop_paths <- function(txt, paths) {
  if (length(paths) == 0) return(paths)
  out <- character(0)
  backtick <- "\x60"
  for (p in paths) {
    if (!grepl(paste0(backtick, "[a-zA-Z_][a-zA-Z0-9_]*\x27"), p)) {
      out <- c(out, p)
      next
    }
    # Find forval x = 1/2 or foreach x in 2 1 (Stata local in backticks)
    local_pat <- paste0("\x60", "([a-zA-Z_][a-zA-Z0-9_]*)\x27")
    loop_m <- regmatches(p, regexpr(local_pat, p))
    loop_var <- if (length(loop_m) > 0) loop_m[[1L]] else character(0)
    if (length(loop_var) == 0) {
      out <- c(out, p)
      next
    }
    loop_var <- sub("^\x60|\x27$", "", loop_var)
    forval_pat <- paste0("forval\\s+", loop_var, "\\s*=\\s*([0-9]+)\\s*/\\s*([0-9]+)")
    foreach_pat <- paste0("foreach\\s+", loop_var, "\\s+in\\s+([^\\n{]+)")
    repl_from <- seq(1L, 1L)
    m_forval <- regmatches(txt, regexpr(forval_pat, txt, ignore.case = TRUE))
    if (length(m_forval) > 0) {
      nums <- as.integer(c(sub(forval_pat, "\\1", m_forval[1], ignore.case = TRUE), sub(forval_pat, "\\2", m_forval[1], ignore.case = TRUE)))
      repl_from <- seq(min(nums), max(nums))
    } else {
      m_foreach <- regmatches(txt, regexpr(foreach_pat, txt, ignore.case = TRUE))
      if (length(m_foreach) > 0) {
        rest <- sub(foreach_pat, "\\1", m_foreach[1], ignore.case = TRUE)
        rest <- trimws(strsplit(rest, "\\s+")[[1]])
        repl_from <- rest[nzchar(rest)]
      }
    }
    if (length(repl_from) > 0 && length(repl_from) <= 20L) {
      for (val in repl_from) {
        p_exp <- gsub(paste0("\x60", loop_var, "\x27"), val, p, fixed = TRUE)
        if (nzchar(p_exp) && is_valid_data_file_path(p_exp)) out <- c(out, p_exp)
      }
    } else {
      out <- c(out, p)
    }
  }
  out
}

# Exclude readme-like files: read me, read_me, readme, read me - code
is_readme_file <- function(full_paths, project_path) {
  base <- basename(full_paths)
  name_no_ext <- tolower(trimws(sub("\\.[Rr](md)?$", "", base, ignore.case = TRUE)))
  name_norm <- gsub("[^a-z0-9]", "", name_no_ext)
  name_norm %in% c("readme", "readmecode")
}

parse_single_r_file <- function(file_path, project_path, rel_path) {
  txt <- tryCatch(readLines(file_path, warn = FALSE), error = function(e) character(0))
  if (length(txt) == 0) return(NULL)
  
  # For Rmd, extract only R chunks
  if (grepl("\\.Rmd$|\\.RMD$", file_path, ignore.case = TRUE)) {
    txt <- extract_r_chunks(txt)
  }
  
  full_txt <- paste(txt, collapse = "\n")
  local_path_vars <- extract_local_path_vars(full_txt)
  sources <- extract_sources(full_txt)
  dr <- extract_data_reads(full_txt)
  data_reads <- dr$paths[vapply(dr$paths, is_valid_data_file_path, logical(1))]
  data_read_folders <- if (!is.null(dr$folder_paths)) dr$folder_paths else character(0)
  data_writes <- extract_data_writes(full_txt)
  data_writes <- data_writes[vapply(data_writes, is_valid_data_file_path, logical(1))]

  # Dynamic path resolution (variables/loops/paste0/here/file.path) without executing code.
  # Only link to a dataset later if the resolved path matches a scanned data file.
  data_reads_dyn <- extract_data_paths_from_ast(full_txt, mode = "read")
  data_writes_dyn <- extract_data_paths_from_ast(full_txt, mode = "write")
  if (length(data_reads_dyn) > 0) data_reads <- unique(c(data_reads, data_reads_dyn))
  if (length(data_writes_dyn) > 0) data_writes <- unique(c(data_writes, data_writes_dyn))
  libraries <- extract_libraries(full_txt)
  
  # Resolve here() paths (file existence only; setup/local vars applied later in resolve_paths_with_setup)
  project_root <- normalizePath(project_path, mustWork = TRUE)
  sources <- resolve_here_paths(sources, project_root, dirname(file_path))
  data_reads <- resolve_here_paths(data_reads, project_root, dirname(file_path))
  data_reads <- data_reads[vapply(data_reads, is_valid_data_file_path, logical(1))]
  data_writes <- resolve_here_paths(data_writes, project_root, dirname(file_path))
  data_writes <- data_writes[vapply(data_writes, is_valid_data_file_path, logical(1))]
  
  list(
    path = file_path,
    rel_path = rel_path,
    sources = unique(sources),
    data_reads = unique(data_reads),
    data_read_folders = unique(data_read_folders),
    data_writes = unique(data_writes),
    libraries = unique(libraries),
    local_path_vars = local_path_vars
  )
}

# Same-file path segment assignments, e.g. ctries_dir <- "Countries", countries_dir = "countries"
# Used to resolve paths like here(raw.dir, countries_dir, ...) or read_dta(here(countries_dir, "x.dta"))
extract_local_path_vars <- function(txt) {
  # Match var <- "value", var = "value", var<-'value', var = 'value' (flexible spacing and quotes)
  pat <- '([a-zA-Z][a-zA-Z0-9_.]*)\\s*(?:<-|=)\\s*["\']([^"\']*)["\']'
  m <- regmatches(txt, gregexpr(pat, txt))[[1]]
  vars <- character(0)
  for (x in m) {
    var <- trimws(sub(pat, "\\1", x))
    val <- trimws(gsub("\\\\", "/", sub(pat, "\\2", x)))
    if (!nzchar(var) || !nzchar(val)) next
    # Keep values that look like path segments: folder names (Countries, countries), or paths with /
    if (nchar(val) <= 200 && (grepl("^[A-Za-z0-9_/. -]+$", val) || grepl("/", val)))
      vars[var] <- val
  }
  # Also capture: var <- paste0(other_var, "suffix") where suffix is a string literal.
  # Store a placeholder and resolve later once setup_vars are known.
  pat_p0 <- '([a-zA-Z][a-zA-Z0-9_.]*)\\s*(?:<-|=)\\s*paste0\\s*\\(\\s*([a-zA-Z][a-zA-Z0-9_.]*)\\s*,\\s*["\']([^"\']+)["\']\\s*\\)'
  m2 <- regmatches(txt, gregexpr(pat_p0, txt, ignore.case = TRUE))[[1]]
  for (x in m2) {
    var <- trimws(sub(pat_p0, "\\1", x, ignore.case = TRUE))
    base <- trimws(sub(pat_p0, "\\2", x, ignore.case = TRUE))
    suf <- trimws(gsub("\\\\", "/", sub(pat_p0, "\\3", x, ignore.case = TRUE)))
    if (!nzchar(var) || !nzchar(base) || !nzchar(suf)) next
    if (nchar(suf) > 200) next
    if (!grepl("^[A-Za-z0-9_/. -]+$", suf) && !grepl("/", suf)) next
    vars[var] <- paste0("__p0__:", base, ":", suf)
  }
  # Also capture: var <- file.path(other_var, "literal") — same __p0__ placeholder convention.
  pat_fp0 <- '([a-zA-Z][a-zA-Z0-9_.]*)\\s*(?:<-|=)\\s*file\\.path\\s*\\(\\s*([a-zA-Z][a-zA-Z0-9_.]*)\\s*,\\s*["\']([^"\']+)["\']\\s*\\)'
  m3 <- regmatches(txt, gregexpr(pat_fp0, txt, ignore.case = TRUE))[[1]]
  for (x in m3) {
    var <- trimws(sub(pat_fp0, "\\1", x, ignore.case = TRUE))
    base <- trimws(sub(pat_fp0, "\\2", x, ignore.case = TRUE))
    suf <- trimws(gsub("\\\\", "/", sub(pat_fp0, "\\3", x, ignore.case = TRUE)))
    if (!nzchar(var) || !nzchar(base) || !nzchar(suf)) next
    if (nchar(suf) > 200) next
    if (!grepl("^[A-Za-z0-9_/. -]+$", suf) && !grepl("/", suf)) next
    if (var %in% names(vars)) next  # paste0 already captured it; prefer that
    vars[var] <- paste0("__p0__:", base, ":", suf)
  }
  vars
}

extract_r_chunks <- function(lines) {
  in_chunk <- FALSE
  out <- character(0)
  for (line in lines) {
    if (grepl("^```\\s*\\{r", line, ignore.case = TRUE)) {
      in_chunk <- TRUE
      next
    }
    if (in_chunk && grepl("^```", line)) {
      in_chunk <- FALSE
      next
    }
    if (in_chunk) out <- c(out, line)
  }
  out
}

# source("x.R"), source(here("x.R")), run_r(here(...)), run_stata(here(...)), etc.
extract_sources <- function(txt) {
  pat1 <- 'source\\s*\\(\\s*["\']([^"\']+\\.R(?:md)?)["\']'
  pat2 <- 'source\\s*\\(\\s*here::here\\s*\\(([^)]+)\\)\\s*\\)'
  pat3 <- 'source\\s*\\(\\s*here\\s*\\(([^)]+)\\)\\s*\\)'
  pat_fp <- 'source\\s*\\(\\s*file\\.path\\s*\\(([^)]+)\\)\\s*\\)'
  pat_paste0 <- 'source\\s*\\(\\s*paste0\\s*\\(([^)]+)\\)\\s*\\)'
  # run_r(here(build.code.dir, "2_Create_unit_IDs", "0-Create_unit_IDs.R")) and run_stata(here(...))
  pat_run_r_here <- 'run_r\\s*\\(\\s*here::here\\s*\\(([^)]+)\\)\\s*\\)'
  pat_run_r_here2 <- 'run_r\\s*\\(\\s*here\\s*\\(([^)]+)\\)\\s*\\)'
  pat_run_stata_here <- 'run_stata\\s*\\(\\s*here::here\\s*\\(([^)]+)\\)\\s*\\)'
  pat_run_stata_here2 <- 'run_stata\\s*\\(\\s*here\\s*\\(([^)]+)\\)\\s*\\)'
  
  p1 <- gsub(pat1, "\\1", regmatches(txt, gregexpr(pat1, txt, ignore.case = TRUE))[[1]], ignore.case = TRUE)
  p2 <- parse_here_args(regmatches(txt, gregexpr(pat2, txt, ignore.case = TRUE))[[1]], pat2, 1L)
  p3 <- parse_here_args(regmatches(txt, gregexpr(pat3, txt, ignore.case = TRUE))[[1]], pat3, 1L)
  p_fp <- parse_here_args(regmatches(txt, gregexpr(pat_fp, txt, ignore.case = TRUE))[[1]], pat_fp, 1L)
  
  p_run_r <- c(
    parse_here_args(regmatches(txt, gregexpr(pat_run_r_here, txt, ignore.case = TRUE))[[1]], pat_run_r_here, 1L),
    parse_here_args(regmatches(txt, gregexpr(pat_run_r_here2, txt, ignore.case = TRUE))[[1]], pat_run_r_here2, 1L)
  )
  p_run_stata <- c(
    parse_here_args(regmatches(txt, gregexpr(pat_run_stata_here, txt, ignore.case = TRUE))[[1]], pat_run_stata_here, 1L),
    parse_here_args(regmatches(txt, gregexpr(pat_run_stata_here2, txt, ignore.case = TRUE))[[1]], pat_run_stata_here2, 1L)
  )
  p_run_r <- p_run_r[nzchar(p_run_r) & grepl("\\.(R|r|Rmd|rmd|DO|do)$", p_run_r)]
  p_run_stata <- p_run_stata[nzchar(p_run_stata) & grepl("\\.(DO|do)$", p_run_stata)]
  p_paste0 <- character(0)
  m <- regmatches(txt, gregexpr(pat_paste0, txt, ignore.case = TRUE))[[1]]
  for (x in m) {
    args <- gsub(pat_paste0, "\\1", x, ignore.case = TRUE)
    parts <- strsplit(args, "\\s*,\\s*")[[1]]
    for (a in parts) {
      a <- gsub('^["\']|["\']$', "", trimws(a))
      if (grepl("\\.R(?:md)?$", a, ignore.case = TRUE)) p_paste0 <- c(p_paste0, a)
    }
  }
  # Keep only run_r/run_stata paths that look like script paths (.R, .Rmd, .do)
  p_run_r <- p_run_r[grepl("\\.(R|r|Rmd|rmd|DO|do)$", p_run_r)]
  p_run_stata <- p_run_stata[grepl("\\.(DO|do)$", p_run_stata)]
  
  unique(c(p1, p2, p3, p_fp, p_paste0, p_run_r, p_run_stata))
}

# read.csv, readRDS, read_excel, fread, haven::read_dta, read.dta13, etc.
# Returns list(paths = file paths, folder_paths = folder paths for data-flow only, e.g. from paste0(cens_dir, ...))
extract_data_reads <- function(txt) {
  folder_paths <- character(0)
  # read.csv("x.csv"), read.csv(here("x.csv")), etc.
  read_funs <- c(
    "read\\.csv", "read\\.csv2", "read\\.table", "readRDS", "load",
    "read_excel", "read_xlsx", "read_xls", "fread",
    "read_dta", "read\\.dta", "read\\.dta13",
    "st_read", "read_sf"
  )
  pat_fun <- paste(read_funs, collapse = "|")
  
  # First arg: "path" or 'path' or here(...) or here::here(...)
  pat_lit <- paste0('(', pat_fun, ')\\s*\\(\\s*["\']([^"\']+)["\']')
  pat_here <- paste0('(', pat_fun, ')\\s*\\(\\s*here::here\\s*\\(([^)]+)\\)')
  pat_here2 <- paste0('(', pat_fun, ')\\s*\\(\\s*here\\s*\\(([^)]+)\\)')
  pat_filepath <- paste0('(', pat_fun, ')\\s*\\(\\s*file\\.path\\s*\\(([^)]+)\\)')
  
  paths <- character(0)
  
  m_lit <- regmatches(txt, gregexpr(pat_lit, txt, ignore.case = TRUE))[[1]]
  for (x in m_lit) {
    p <- sub(pat_lit, "\\2", x, ignore.case = TRUE)
    paths <- c(paths, p)
  }

  # Named first-arg: read.csv(file="path"), fread(input="path"), st_read(dsn="path"), etc.
  pat_named_first <- paste0('(', pat_fun, ')\\s*\\(\\s*(?:file|path|dsn|input|con|url)\\s*=\\s*["\']([^"\']+)["\']')
  m_named <- regmatches(txt, gregexpr(pat_named_first, txt, ignore.case = TRUE))[[1]]
  for (x in m_named) {
    p <- sub(pat_named_first, "\\2", x, ignore.case = TRUE)
    paths <- c(paths, p)
  }

  m_here <- regmatches(txt, gregexpr(pat_here, txt, ignore.case = TRUE))[[1]]
  paths <- c(paths, parse_here_args(m_here, pat_here, 2L, mark_identifiers = TRUE))

  m_here2 <- regmatches(txt, gregexpr(pat_here2, txt, ignore.case = TRUE))[[1]]
  paths <- c(paths, parse_here_args(m_here2, pat_here2, 2L, mark_identifiers = TRUE))

  pat_dta <- 'read_dta\\s*\\(\\s*here::here\\s*\\(([^)]+)\\)'
  pat_dta2 <- 'read\\.dta13?\\s*\\(\\s*here::here\\s*\\(([^)]+)\\)'
  m_dta <- regmatches(txt, gregexpr(pat_dta, txt, ignore.case = TRUE))[[1]]
  m_dta2 <- regmatches(txt, gregexpr(pat_dta2, txt, ignore.case = TRUE))[[1]]
  paths <- c(
    paths,
    parse_here_args(m_dta, pat_dta, 1L, mark_identifiers = TRUE),
    parse_here_args(m_dta2, pat_dta2, 1L, mark_identifiers = TRUE)
  )

  # Literal filenames inside here(...) for read_* - e.g. read_dta(here(raw.dir, ctries_dir, cname, cens_dir, "consistent_10.dta"))
  # so we capture "consistent_10.dta" and can match writers that write that file under any subpath (e.g. Countries/AGO/Census/).
  # Allow newlines inside here() so multi-line calls are captured.
  read_here_pat <- paste0('(', pat_fun, ')\\s*\\(\\s*(?:here::here|here)\\s*\\(([^)]+)\\)')
  read_here_m <- regmatches(txt, gregexpr(read_here_pat, txt, ignore.case = TRUE))[[1]]
  for (hm in read_here_m) {
    inner <- sub(read_here_pat, "\\2", hm, ignore.case = TRUE)
    quoted <- regmatches(inner, gregexpr('["\']([^"\']*\\.(dta|rds|csv|rda|rdata|xlsx|xls|gpkg|shp|sav|parquet))["\']', inner, ignore.case = TRUE))[[1]]
    for (q in quoted) {
      fname <- gsub('^["\']|["\']$', "", q)
      if (nzchar(fname) && is_valid_data_file_path(fname)) paths <- c(paths, fname)
    }
  }
  m_fp <- regmatches(txt, gregexpr(pat_filepath, txt, ignore.case = TRUE))[[1]]
  for (x in m_fp) {
    args <- gsub(pat_filepath, "\\2", x, ignore.case = TRUE)
    p <- paste_here_args(args, mark_identifiers = TRUE)
    if (nzchar(p)) paths <- c(paths, p)
  }
  
  pat_paste0a <- 'paste0\\s*\\(\\s*([a-z_.]+)\\s*,\\s*["\']/([^"\']*)["\']\\s*\\)'
  pat_paste0b <- 'paste0\\s*\\(\\s*([a-z_.]+)\\s*,\\s*["\']/["\']\\s*,\\s*["\']([^"\']+)["\']'
  for (pat in list(pat_paste0a, pat_paste0b)) {
    m <- regmatches(txt, gregexpr(pat, txt, ignore.case = TRUE))[[1]]
    for (x in m) {
      var <- sub(pat, "\\1", x, ignore.case = TRUE)
      rest <- sub(pat, "\\2", x, ignore.case = TRUE)
      if (nzchar(var) && nzchar(rest)) paths <- c(paths, paste0(var, "/", rest))
    }
  }
  
  # Only extract path from paste0() when ALL args are string literals (no variables like yr, w, lvl).
  # Otherwise we get junk like "census/yr/.rds", "fl_nl_yrs_w/w/.rds", "t/lvl/00" from paste0("census", yr, ".rds"), etc.
  pat_paste0_multi <- 'paste0\\s*\\(([^)]+)\\)'
  m_multi <- regmatches(txt, gregexpr(pat_paste0_multi, txt, ignore.case = TRUE))[[1]]
  for (x in unique(m_multi)) {
    args_str <- gsub(pat_paste0_multi, "\\1", x, ignore.case = TRUE)
    args <- strsplit(args_str, "\\s*,\\s*")[[1]]
    args <- trimws(args)
    has_variable <- FALSE
    path_parts <- character(0)
    for (a in args) {
      is_quoted <- grepl('^["\'][^"\']*["\']$', a)
      a_clean <- gsub('^["\']|["\']$', "", a)
      if (!is_quoted && nzchar(a) && grepl("^[a-zA-Z_][a-zA-Z0-9_.]*$", a)) {
        # Unquoted identifier = variable (yr, w, lvl, ctry_str, etc.) - do not treat as path segment
        has_variable <- TRUE
        break
      }
      if (nzchar(a_clean) && (grepl("\\.(csv|rds|dta|xlsx|rda|gpkg)$", a_clean, ignore.case = TRUE) || (is_quoted && grepl("^[a-zA-Z0-9_/. -]+$", a_clean)) || grepl("^[a-zA-Z_][a-zA-Z0-9_.]*$", a_clean))) path_parts <- c(path_parts, a_clean)
    }
    if (!has_variable && length(path_parts) > 0) {
      p_combined <- paste(path_parts, collapse = "/")
      if (nzchar(p_combined) && is_valid_data_file_path(p_combined)) paths <- c(paths, p_combined)
    }
    # When paste0 has variables (e.g. level `l`, yr) but first arg is a known dir (cens_dir, raw.dir),
    # record folder path for data-flow matching only (not added to paths so data index stays file-only)
    if (has_variable && length(path_parts) > 0) {
      folder_path <- paste(path_parts, collapse = "/")
      if (nzchar(folder_path)) folder_paths <- c(folder_paths, folder_path)
    }
  }
  
  list(paths = unique(paths), folder_paths = unique(folder_paths))
}

# Normalize path for grouping: slash/trim/collapse only. Paths are resolved via setup_vars
# (from setup files and master); matching is case-insensitive via path_norm_for_group.
normalize_path_canonical <- function(p) {
  if (length(p) == 0) return(p)
  out <- character(length(p))
  for (i in seq_along(p)) {
    x <- p[i]
    if (is.na(x)) {
      out[i] <- ""
      next
    }
    if (!nzchar(x)) {
      out[i] <- x
      next
    }
    x <- gsub("\\\\", "/", trimws(x))
    x <- gsub("/+", "/", x)
    # If an absolute or "externalish" path contains a /data/... segment, normalize to that project-style relative form.
    # This reduces noisy dataset IDs like "/Migration Africa/data/Build/..." -> "data/Build/...".
    # IMPORTANT: do NOT strip a legitimate project-relative prefix like "africa/artisanal/data/Raw/...".
    is_abs <- grepl("^[A-Za-z]:", x) || startsWith(x, "/")
    is_externalish_rel <- grepl("(^|/)\\.\\.?/", x) # ./ or ../ anywhere
    m_tier <- regexpr("(^|/)data/(raw|build|final)(/|$)", x, ignore.case = TRUE)
    has_tiered_data <- length(m_tier) > 0 && !is.na(m_tier[1]) && m_tier[1] > 1
    if (has_tiered_data) {
      prefix <- substr(x, 1, m_tier[1] - 1)
      if (is_abs || is_externalish_rel || grepl("\\s", prefix)) {
        start <- m_tier[1]
        if (substr(x, start, start) == "/") start <- start + 1
        x <- substr(x, start, nchar(x))
      }
    } else if (is_abs || is_externalish_rel) {
      m <- regexpr("(^|/)data/", x, ignore.case = TRUE)
      if (length(m) > 0 && !is.na(m[1]) && m[1] > 1) {
        start <- m[1]
        if (substr(x, start, start) == "/") start <- start + 1
        x <- substr(x, start, nchar(x))
      }
    }
    out[i] <- sub("/+$", "", x)
  }
  out
}

canonicalization_sanity_checks <- function() {
  # Guardrails for path canonicalization; keep lightweight and deterministic.
  a <- normalize_path_canonical("Migration Africa/data/Final/x.dta")
  if (!identical(a, "data/Final/x.dta")) stop("canonicalization sanity check failed: expected 'data/Final/x.dta', got: ", a)
  b <- normalize_path_canonical("africa/artisanal/data/Raw/x.dta")
  if (!identical(b, "africa/artisanal/data/Raw/x.dta")) stop("canonicalization sanity check failed for repo-relative nested data path: ", b)
  c <- canonical_data_id("C:/Users/me/Dropbox/Migration Africa/data/BUILD/x.dta")
  if (!identical(c, "data/Build/x.dta")) stop("canonicalization sanity check failed for absolute data tier: ", c)
  invisible(TRUE)
}

# Deterministic dataset ID strategy:
# - Prefer a canonical project-relative "data/<Raw|Build|Final>/..." form when paths indicate a tier.
# - Strip external absolute prefixes (normalize_path_canonical) so "C:/.../data/Build/x.dta" becomes "data/Build/x.dta".
# - Keep a bare filename only as a match key (to be resolved later when possible).
canonical_data_id <- function(p) {
  if (length(p) == 0) return(p)
  x <- normalize_path_canonical(p)
  out <- character(length(x))
  for (i in seq_along(x)) {
    xi <- x[i]
    if (!nzchar(xi)) {
      out[i] <- xi
      next
    }

    # Avoid propagating external absolute roots into dataset IDs.
    # If we couldn't anchor it to data/... via normalize_path_canonical, keep only the basename as a match key.
    if ((grepl("^[A-Za-z]:", xi) || startsWith(xi, "/")) && !grepl("(^|/)data/", xi, ignore.case = TRUE)) {
      xi <- basename(xi)
    }

    segs <- strsplit(xi, "/", fixed = TRUE)[[1]]
    segs <- segs[nzchar(segs)]
    if (length(segs) == 0) {
      out[i] <- xi
      next
    }
    segs_l <- tolower(segs)

    # raw/... -> data/Raw/...
    if (segs_l[1] %in% c("raw", "build", "final")) {
      tier <- tools::toTitleCase(segs_l[1])
      segs <- c("data", tier, segs[-1])
      out[i] <- paste(segs, collapse = "/")
      next
    }

    # data/raw/... -> data/Raw/...
    if (length(segs) >= 2L && segs_l[1] == "data" && segs_l[2] %in% c("raw", "build", "final")) {
      segs[1] <- "data"
      segs[2] <- tools::toTitleCase(segs_l[2])
      out[i] <- paste(segs, collapse = "/")
      next
    }

    out[i] <- xi
  }
  out
}

# For grouping: same key for paths that differ only by case/slashes (e.g. data/Build vs data/build).
path_norm_for_group <- function(p) tolower(normalize_path_canonical(p))

# saveRDS, fwrite, write_dta, write.csv, st_write - path is typically 2nd arg
extract_data_writes <- function(txt) {
  write_funs <- c(
    "saveRDS", "save", "fwrite", "write\\.csv", "write\\.csv2", "write\\.table",
    "write_dta", "write\\.dta", "st_write", "write_sf"
  )
  pat_fun <- paste(write_funs, collapse = "|")
  pat_lit <- paste0('(', pat_fun, ')\\s*\\([^,]+,\\s*["\']([^"\']+)["\']')
  pat_here <- paste0('(', pat_fun, ')\\s*\\([^,]+,\\s*here::here\\s*\\(([^)]+)\\)')
  pat_here2 <- paste0('(', pat_fun, ')\\s*\\([^,]+,\\s*here\\s*\\(([^)]+)\\)')
  pat_filepath <- paste0('(', pat_fun, ')\\s*\\([^,]+,\\s*file\\.path\\s*\\(([^)]+)\\)')
  paths <- character(0)
  m_lit <- regmatches(txt, gregexpr(pat_lit, txt, ignore.case = TRUE))[[1]]
  for (x in m_lit) {
    p <- sub(pat_lit, "\\2", x, ignore.case = TRUE)
    paths <- c(paths, p)
  }
  # Named second-arg: saveRDS(obj, file="path"), write.csv(df, file="path"), etc.
  pat_named_second <- paste0('(', pat_fun, ')\\s*\\([^,\n]+,\\s*(?:file|path|dsn|output)\\s*=\\s*["\']([^"\']+)["\']')
  m_named <- regmatches(txt, gregexpr(pat_named_second, txt, ignore.case = TRUE))[[1]]
  for (x in m_named) {
    p <- sub(pat_named_second, "\\2", x, ignore.case = TRUE)
    paths <- c(paths, p)
  }
  m_here <- regmatches(txt, gregexpr(pat_here, txt, ignore.case = TRUE))[[1]]
  paths <- c(paths, parse_here_args(m_here, pat_here, 2L, mark_identifiers = TRUE))
  m_here2 <- regmatches(txt, gregexpr(pat_here2, txt, ignore.case = TRUE))[[1]]
  paths <- c(paths, parse_here_args(m_here2, pat_here2, 2L, mark_identifiers = TRUE))
  m_fp <- regmatches(txt, gregexpr(pat_filepath, txt, ignore.case = TRUE))[[1]]
  for (x in m_fp) {
    args <- gsub(pat_filepath, "\\2", x, ignore.case = TRUE)
    p <- paste_here_args(args, mark_identifiers = TRUE)
    if (nzchar(p)) paths <- c(paths, p)
  }
  unique(paths)
}

safe_eval_string_expr <- function(expr, vars, max_out = 200L) {
  # Evaluate a limited subset of R expressions into character vectors.
  # This is NOT code execution: only string/numeric literals, identifiers mapped in `vars`,
  # and a small whitelist of string-building functions are supported.
  if (is.null(expr)) return(character(0))

  to_chr <- function(x) {
    if (length(x) == 0) return(character(0))
    if (is.character(x)) return(x)
    if (is.numeric(x) || is.integer(x) || is.logical(x)) return(as.character(x))
    character(0)
  }

  cap_vec <- function(x) {
    x <- to_chr(x)
    x <- x[!is.na(x)]
    x <- x[nzchar(x)]
    if (length(x) > max_out) x <- x[seq_len(max_out)]
    x
  }

  cross_join <- function(parts, sep) {
    if (length(parts) == 0) return(character(0))
    out <- ""
    for (v in parts) {
      v <- cap_vec(v)
      if (length(v) == 0) return(character(0))
      next_out <- character(0)
      for (o in out) {
        for (vv in v) {
          if (length(next_out) >= max_out) break
          if (!nzchar(o)) next_out <- c(next_out, vv)
          else if (!nzchar(vv)) next_out <- c(next_out, o)
          else next_out <- c(next_out, paste0(o, sep, vv))
        }
        if (length(next_out) >= max_out) break
      }
      out <- next_out
      if (length(out) == 0) return(character(0))
    }
    out
  }

  call_fun_end <- function(call) {
    if (!is.call(call) || length(call) == 0) return("")
    head <- call[[1]]
    if (is.name(head)) return(as.character(head))
    if (is.call(head) && length(head) >= 3 && as.character(head[[1]]) %in% c("::", ":::")) {
      return(as.character(head[[3]]))
    }
    ""
  }

  # Literals
  if (is.character(expr)) return(cap_vec(expr))
  if (is.numeric(expr) || is.integer(expr) || is.logical(expr)) return(cap_vec(expr))

  # Identifiers
  if (is.name(expr)) {
    nm <- as.character(expr)
    if (!nzchar(nm)) return(character(0))
    if (!is.null(vars[[nm]])) return(cap_vec(vars[[nm]]))
    return(character(0))
  }

  if (!is.call(expr)) return(character(0))

  fn <- call_fun_end(expr)
  if (!nzchar(fn)) return(character(0))

  fn_l <- tolower(fn)

  # One-arg transforms
  if (fn_l %in% c("tolower", "toupper", "as.character")) {
    if (length(expr) < 2) return(character(0))
    v <- safe_eval_string_expr(expr[[2]], vars, max_out = max_out)
    if (length(v) == 0) return(character(0))
    if (fn_l == "tolower") return(tolower(v))
    if (fn_l == "toupper") return(toupper(v))
    return(v)
  }

  # c(...) concatenation
  if (fn_l == "c") {
    vals <- character(0)
    if (length(expr) < 2) return(character(0))
    for (i in 2:length(expr)) {
      vals <- c(vals, safe_eval_string_expr(expr[[i]], vars, max_out = max_out))
      if (length(vals) >= max_out) break
    }
    return(unique(cap_vec(vals)))
  }

  # list(...) - treat as a character vector of the elements (names ignored)
  if (fn_l == "list") {
    vals <- character(0)
    if (length(expr) < 2) return(character(0))
    for (i in 2:length(expr)) {
      vals <- c(vals, safe_eval_string_expr(expr[[i]], vars, max_out = max_out))
      if (length(vals) >= max_out) break
    }
    return(unique(cap_vec(vals)))
  }

  # Path-building
  if (fn_l %in% c("file.path", "here")) {
    if (length(expr) < 2) return(character(0))
    parts <- list()
    for (i in 2:length(expr)) parts[[length(parts) + 1L]] <- safe_eval_string_expr(expr[[i]], vars, max_out = max_out)
    v <- cross_join(parts, "/")
    v <- gsub("/+", "/", v)
    v <- gsub("\\\\", "/", v)
    v <- sub("^/+", "", v)
    return(unique(cap_vec(v)))
  }

  if (fn_l %in% c("paste0", "paste")) {
    if (length(expr) < 2) return(character(0))
    # For paste(), respect sep if it's a literal (otherwise default to " ").
    sep <- if (fn_l == "paste0") "" else " "
    if (fn_l == "paste") {
      nms <- names(expr)
      if (!is.null(nms) && "sep" %in% nms) {
        i_sep <- which(nms == "sep")[1]
        if (!is.na(i_sep) && length(expr) >= i_sep) {
          sep_val <- safe_eval_string_expr(expr[[i_sep]], vars, max_out = max_out)
          if (length(sep_val) >= 1) sep <- sep_val[1]
        }
      }
    }
    parts <- list()
    nms <- names(expr)
    for (i in 2:length(expr)) {
      # Skip named control args for paste()
      if (fn_l == "paste" && !is.null(nms) && nzchar(nms[i]) && nms[i] %in% c("sep", "collapse")) next
      parts[[length(parts) + 1L]] <- safe_eval_string_expr(expr[[i]], vars, max_out = max_out)
    }
    v <- cross_join(parts, sep)
    v <- gsub("\\\\", "/", v)
    v <- gsub("/+", "/", v)
    v <- gsub("\\s+", "", v)
    return(unique(cap_vec(v)))
  }

  character(0)
}

# --- Indirect list -> apply/map -> wrapper inference (static) -----------------

infer_indirect_apply_reads <- function(files, setup_vars, project_path, max_enum = 500L) {
  # Returns list(files=..., warnings=character())
  warnings <- character(0)
  if (length(files) == 0) return(list(files = files, warnings = warnings))

  # Read/parsing helper (Rmd -> only chunks, matches parse_single_r_file behavior)
  read_file_for_ast <- function(path) {
    txt <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
    if (length(txt) == 0) return(character(0))
    if (grepl("\\.Rmd$|\\.RMD$", path, ignore.case = TRUE)) txt <- extract_r_chunks(txt)
    txt
  }

  call_fun_end <- function(call) {
    if (!is.call(call) || length(call) == 0) return("")
    head <- call[[1]]
    if (is.name(head)) return(as.character(head))
    if (is.call(head) && length(head) >= 3 && as.character(head[[1]]) %in% c("::", ":::")) {
      return(as.character(head[[3]]))
    }
    ""
  }

  is_assign_call <- function(e) is.call(e) && as.character(e[[1]])[1L] %in% c("<-", "=") && length(e) >= 3

  # Parse all files once so we can resolve wrapper functions across files.
  ast_cache <- list()
  for (rel in names(files)) {
    txt <- read_file_for_ast(files[[rel]]$path)
    exprs <- tryCatch(parse(text = txt), error = function(e) NULL)
    ast_cache[[rel]] <- exprs
  }

  # Build a function table: rel_path -> list(fn_name -> list(params, body))
  fun_table <- list()
  for (rel in names(files)) {
    exprs <- ast_cache[[rel]]
    if (is.null(exprs) || length(exprs) == 0) next
    tbl <- list()
    for (e in as.list(exprs)) {
      if (!is_assign_call(e)) next
      lhs <- e[[2]]
      rhs <- e[[3]]
      if (!is.name(lhs)) next
      nm <- as.character(lhs)
      if (!nzchar(nm)) next
      if (is.call(rhs) && as.character(rhs[[1]])[1L] == "function") {
        params <- as.list(rhs[[2]])
        param_names <- names(params)
        if (is.null(param_names)) param_names <- character(0)
        # Prefer explicit name list; fall back to symbols where possible
        if (length(param_names) == 0 && length(params) > 0) {
          param_names <- vapply(params, function(p) if (is.name(p)) as.character(p) else "", character(1))
        }
        param_names <- param_names[nzchar(param_names)]
        body <- rhs[[3]]
        tbl[[nm]] <- list(params = param_names, body = body)
      }
    }
    fun_table[[rel]] <- tbl
  }

  # Build a simple string-valued symbol table for each file (list/c and safe string exprs).
  # This is used to resolve lapply/map inputs like `files <- list(a="x.rds", ...)`.
  value_table <- list()
  for (rel in names(files)) {
    exprs <- ast_cache[[rel]]
    if (is.null(exprs) || length(exprs) == 0) next
    vars <- list()
    for (e in as.list(exprs)) {
      if (!is_assign_call(e)) next
      lhs <- e[[2]]
      rhs <- e[[3]]
      if (!is.name(lhs)) next
      nm <- as.character(lhs)
      if (!nzchar(nm)) next
      vals <- safe_eval_string_expr(rhs, vars, max_out = as.integer(max_enum))
      # Keep only plausible filenames/path-ish tokens (not arbitrary strings)
      vals <- vals[nchar(vals) <= 300L]
      if (length(vals) > 0) vars[[nm]] <- unique(vals)
      if (!is.null(vars[[nm]]) && length(vars[[nm]]) > max_enum) vars[[nm]] <- vars[[nm]][seq_len(max_enum)]
    }
    value_table[[rel]] <- vars
  }

  # Resolve available wrapper definitions for a file: same-file + sourced files (transitively, within parsed set).
  get_available_fun_defs <- function(rel) {
    seen <- character(0)
    stack <- rel
    out <- list()
    while (length(stack) > 0) {
      cur <- stack[1]; stack <- stack[-1]
      if (!nzchar(cur) || cur %in% seen) next
      seen <- c(seen, cur)
      tbl <- fun_table[[cur]]
      if (!is.null(tbl) && length(tbl) > 0) {
        for (nm in names(tbl)) if (is.null(out[[nm]])) out[[nm]] <- tbl[[nm]]
      }
      srcs <- files[[cur]]$sources %||% character(0)
      srcs <- srcs[nzchar(srcs)]
      # Only follow sources that are part of the parsed project set (read-only).
      srcs <- srcs[srcs %in% names(files)]
      if (length(srcs) > 0) stack <- c(stack, srcs)
      if (length(seen) > 200L) break
    }
    out
  }

  # Wrapper executor: given a function body and its single parameter, statically infer read paths.
  infer_reads_from_wrapper <- function(body, param, arg_vals, env_vars) {
    if (length(arg_vals) == 0) return(character(0))

    read_funs <- c(
      "read.csv", "read.csv2", "read.table", "readrds", "load",
      "read_excel", "read_xlsx", "read_xls", "fread",
      "read_dta", "read.dta", "read.dta13",
      "st_read", "read_sf"
    )

    get_named_or_pos <- function(call, name_candidates, pos) {
      if (!is.call(call)) return(NULL)
      nms <- names(call)
      if (!is.null(nms)) {
        for (nm in name_candidates) {
          i <- which(nms == nm)
          if (length(i) > 0) return(call[[i[1]]])
        }
      }
      if (length(call) >= (pos + 1L)) return(call[[pos + 1L]])
      NULL
    }

    add_var <- function(vars, name, vals) {
      if (is.null(name) || !nzchar(name)) return(vars)
      vals <- vals[!is.na(vals)]
      vals <- vals[nzchar(vals)]
      vals <- vals[nchar(vals) <= 300L]
      if (length(vals) == 0) return(vars)
      cur <- vars[[name]]
      if (is.null(cur)) cur <- character(0)
      vars[[name]] <- unique(c(cur, vals))
      if (length(vars[[name]]) > 50L) vars[[name]] <- vars[[name]][seq_len(50L)]
      vars
    }

    walk_one <- function(e, vars) {
      paths <- character(0)
      if (missing(e) || is.null(e)) return(list(paths = character(0), vars = vars))

      if (is.call(e) && as.character(e[[1]])[1L] == "{") {
        inner <- as.list(e)[-1]
        for (x in inner) {
          res <- walk_one(x, vars)
          vars <- res$vars
          paths <- c(paths, res$paths)
          if (length(paths) >= max_enum) break
        }
        return(list(paths = paths, vars = vars))
      }

      if (is_assign_call(e)) {
        lhs <- e[[2]]
        rhs <- e[[3]]
        if (is.name(lhs)) {
          nm <- as.character(lhs)
          vals <- safe_eval_string_expr(rhs, vars, max_out = 200L)
          vars <- add_var(vars, nm, vals)
        }
      }

      if (is.call(e) && as.character(e[[1]])[1L] == "for" && length(e) >= 4) {
        loop_var <- e[[2]]
        seq_expr <- e[[3]]
        body2 <- e[[4]]
        if (is.name(loop_var)) {
          nm <- as.character(loop_var)
          seq_vals <- safe_eval_string_expr(seq_expr, vars, max_out = 50L)
          if (length(seq_vals) > 0) {
            child_vars <- vars
            child_vars[[nm]] <- seq_vals
            res_body <- walk_one(body2, child_vars)
            paths <- c(paths, res_body$paths)
          } else {
            res_body <- walk_one(body2, vars)
            paths <- c(paths, res_body$paths)
          }
        } else {
          res_body <- walk_one(body2, vars)
          paths <- c(paths, res_body$paths)
        }
        return(list(paths = paths, vars = vars))
      }

      if (is.call(e)) {
        fn <- tolower(call_fun_end(e))
        if (nzchar(fn) && fn %in% read_funs) {
          path_expr <- get_named_or_pos(e, c("file", "path", "dsn", "input", "con", "url"), 1L)
          vals <- safe_eval_string_expr(path_expr, vars, max_out = 200L)
          if (length(vals) > 0) paths <- c(paths, vals)
        }
        args <- as.list(e)[-1]
        for (a in args) {
          res <- walk_one(a, vars)
          # Note: vars changes from nested contexts can matter for sibling reads only in sequential blocks;
          # for simplicity, keep current vars here (conservative) and only aggregate paths.
          paths <- c(paths, res$paths)
          if (length(paths) >= max_enum) break
        }
      }

      list(paths = paths, vars = vars)
    }

    out <- character(0)
    for (v in arg_vals) {
      vars <- env_vars
      vars[[param]] <- v
      res <- walk_one(body, vars)
      out <- c(out, res$paths)
      if (length(out) >= max_enum) break
    }
    unique(out)
  }

  # Apply-site detection + inference
  apply_funs <- c(
    "lapply",
    "map", "map_chr", "map_dfr", "map_df", "map_dfc", "map_dbl", "map_int", "map_lgl"
  )

  is_apply_call <- function(e) {
    if (!is.call(e)) return(FALSE)
    fn <- tolower(call_fun_end(e))
    nzchar(fn) && fn %in% apply_funs
  }

  get_apply_args <- function(call) {
    # returns list(x_expr=..., f_expr=..., kind=...)
    fn <- tolower(call_fun_end(call))
    if (fn == "lapply") {
      x_expr <- if (length(call) >= 2) call[[2]] else NULL
      f_expr <- if (length(call) >= 3) call[[3]] else NULL
      return(list(x_expr = x_expr, f_expr = f_expr, kind = "lapply"))
    }
    # purrr::map(.x, .f)
    nms <- names(call)
    x_expr <- NULL; f_expr <- NULL
    if (!is.null(nms) && ".x" %in% nms) x_expr <- call[[which(nms == ".x")[1]]]
    if (!is.null(nms) && ".f" %in% nms) f_expr <- call[[which(nms == ".f")[1]]]
    if (is.null(x_expr) && length(call) >= 2) x_expr <- call[[2]]
    if (is.null(f_expr) && length(call) >= 3) f_expr <- call[[3]]
    list(x_expr = x_expr, f_expr = f_expr, kind = "purrr_map")
  }

  resolve_fun_expr <- function(f_expr, fun_defs) {
    # Supports: symbol, function(...) { ... }, purrr formula ~ ...
    if (is.null(f_expr)) return(NULL)
    if (is.name(f_expr)) {
      nm <- as.character(f_expr)
      if (nzchar(nm) && !is.null(fun_defs[[nm]])) return(c(list(name = nm), fun_defs[[nm]]))
      return(NULL)
    }
    if (is.call(f_expr) && as.character(f_expr[[1]])[1L] == "function") {
      params <- as.list(f_expr[[2]])
      param_names <- names(params)
      if (is.null(param_names) || length(param_names) == 0) {
        param_names <- vapply(params, function(p) if (is.name(p)) as.character(p) else "", character(1))
      }
      param_names <- param_names[nzchar(param_names)]
      body <- f_expr[[3]]
      return(list(name = "<inline>", params = param_names, body = body))
    }
    if (is.call(f_expr) && as.character(f_expr[[1]])[1L] == "~" && length(f_expr) >= 2) {
      # Formula shorthand: ~ read_int(.x)  OR ~ { ... }
      rhs <- f_expr[[2]]
      if (is.name(rhs)) {
        nm <- as.character(rhs)
        if (nzchar(nm) && !is.null(fun_defs[[nm]])) return(c(list(name = nm), fun_defs[[nm]]))
      }
      if (is.call(rhs) && is.name(rhs[[1]])) {
        nm <- as.character(rhs[[1]])
        if (nzchar(nm) && !is.null(fun_defs[[nm]])) return(c(list(name = nm), fun_defs[[nm]]))
      }
      return(NULL)
    }
    NULL
  }

  # Walk arbitrary expressions and collect apply callsites.
  collect_apply_calls <- function(exprs) {
    out <- list()
    is_missing_arg <- function(x) {
      # In parsed calls, a missing argument is represented by an "empty symbol" that can trigger
      # "argument 'x' is missing" errors if you try to inspect it directly. Treat any inspection error
      # as a missing-arg sentinel.
      ok <- TRUE
      nm <- ""
      tryCatch({
        nm <- as.character(x)
      }, error = function(e) {
        ok <<- FALSE
      })
      (!ok) || (is.character(nm) && length(nm) >= 1L && !nzchar(nm[1]))
    }
    walk <- function(a) {
      if (missing(a) || is.null(a)) return()
      if (is_apply_call(a)) out[[length(out) + 1L]] <<- a
      if (is.call(a)) {
        parts <- as.list(a)
        for (i in seq_along(parts)) {
          x <- parts[[i]]
          if (is_missing_arg(x)) next
          walk(x)
        }
      } else if (is.expression(a)) {
        parts <- as.list(a)
        for (i in seq_along(parts)) {
          x <- parts[[i]]
          if (is_missing_arg(x)) next
          walk(x)
        }
      } else if (is.list(a)) {
        for (i in seq_along(a)) {
          x <- a[[i]]
          if (is_missing_arg(x)) next
          walk(x)
        }
      }
    }
    walk(exprs)
    out
  }

  for (rel in names(files)) {
    exprs <- ast_cache[[rel]]
    if (is.null(exprs) || length(exprs) == 0) next

    fun_defs <- get_available_fun_defs(rel)
    vars0 <- value_table[[rel]] %||% list()

    apply_calls <- collect_apply_calls(exprs)
    if (length(apply_calls) == 0) next

    local_vars <- files[[rel]]$local_path_vars %||% character(0)
    local_vars <- resolve_local_path_vars(local_vars, setup_vars)
    env_vars <- c(as.list(setup_vars), as.list(local_vars), vars0)

    inferred_paths <- character(0)

    for (call in apply_calls) {
      aa <- get_apply_args(call)
      x_vals <- safe_eval_string_expr(aa$x_expr, env_vars, max_out = as.integer(max_enum) + 1L)
      if (length(x_vals) == 0) next

      if (length(x_vals) > max_enum) {
        warnings <- c(
          warnings,
          paste0("[indirect-map] ", rel, ": skipped enumeration (", length(x_vals), " > ", max_enum, ") for an apply/map call; inferred paths would be too large.")
        )
        next
      }

      fun_def <- resolve_fun_expr(aa$f_expr, fun_defs)
      if (is.null(fun_def) || is.null(fun_def$params) || length(fun_def$params) == 0) next

      # Only support single-argument wrappers for now (common case for map/lapply over a list of filenames).
      param <- fun_def$params[1]
      if (!nzchar(param)) next

      reads <- infer_reads_from_wrapper(fun_def$body, param, x_vals, env_vars)
      if (length(reads) == 0) next

      reads <- resolve_here_paths(reads, normalizePath(project_path, mustWork = TRUE), dirname(files[[rel]]$path))
      reads <- canonical_data_id(reads)
      reads <- reads[nzchar(reads)]
      reads <- reads[vapply(reads, is_valid_data_file_path, logical(1))]

      if (length(reads) > 0) inferred_paths <- c(inferred_paths, reads)
      if (length(inferred_paths) >= max_enum) break
    }

    if (length(inferred_paths) > 0) {
      files[[rel]]$data_reads <- unique(c(files[[rel]]$data_reads %||% character(0), inferred_paths))
      # Keep deterministic IDs; filter again just in case.
      files[[rel]]$data_reads <- unique(files[[rel]]$data_reads[vapply(files[[rel]]$data_reads, is_valid_data_file_path, logical(1))])
    }
  }

  list(files = files, warnings = unique(warnings))
}

extract_data_paths_from_ast <- function(txt, mode = c("read", "write"), max_paths = 500L) {
  mode <- match.arg(mode)
  exprs <- tryCatch(parse(text = txt), error = function(e) NULL)
  if (is.null(exprs) || length(exprs) == 0) return(character(0))

  read_funs <- c(
    "read.csv", "read.csv2", "read.table", "readrds", "load",
    "read_excel", "read_xlsx", "read_xls", "fread",
    "read_dta", "read.dta", "read.dta13",
    "st_read", "read_sf"
  )
  write_funs <- c(
    "saverds", "save", "fwrite", "write.csv", "write.csv2", "write.table",
    "write_dta", "write.dta", "st_write", "write_sf"
  )

  call_fun_end <- function(call) {
    if (!is.call(call) || length(call) == 0) return("")
    head <- call[[1]]
    if (is.name(head)) return(as.character(head))
    if (is.call(head) && length(head) >= 3 && as.character(head[[1]]) %in% c("::", ":::")) {
      return(as.character(head[[3]]))
    }
    ""
  }

  get_named_or_pos <- function(call, name_candidates, pos) {
    if (!is.call(call)) return(NULL)
    nms <- names(call)
    if (!is.null(nms)) {
      for (nm in name_candidates) {
        i <- which(nms == nm)
        if (length(i) > 0) return(call[[i[1]]])
      }
    }
    if (length(call) >= (pos + 1L)) return(call[[pos + 1L]])
    NULL
  }

  add_var <- function(vars, name, vals) {
    if (is.null(name) || !nzchar(name)) return(vars)
    vals <- vals[!is.na(vals)]
    vals <- vals[nzchar(vals)]
    vals <- vals[nchar(vals) <= 300L]
    if (length(vals) == 0) return(vars)
    cur <- vars[[name]]
    if (is.null(cur)) cur <- character(0)
    vars[[name]] <- unique(c(cur, vals))
    if (length(vars[[name]]) > 50L) vars[[name]] <- vars[[name]][seq_len(50L)]
    vars
  }

  walk_block <- function(expr_list, vars) {
    out <- character(0)
    for (e in expr_list) {
      res <- walk_one(e, vars)
      vars <- res$vars
      if (length(res$paths) > 0) out <- c(out, res$paths)
      if (length(out) >= max_paths) break
    }
    list(paths = out, vars = vars)
  }

  walk_one <- function(e, vars) {
    paths <- character(0)
    if (missing(e) || is.null(e)) return(list(paths = character(0), vars = vars))

    # Sequential blocks
    if (is.call(e) && as.character(e[[1]])[1L] == "{") {
      inner <- as.list(e)[-1]
      res <- walk_block(inner, vars)
      return(list(paths = res$paths, vars = res$vars))
    }

    # Assignments
    if (is.call(e) && as.character(e[[1]])[1L] %in% c("<-", "=") && length(e) >= 3) {
      lhs <- e[[2]]
      rhs <- e[[3]]
      if (is.name(lhs)) {
        nm <- as.character(lhs)
        vals <- safe_eval_string_expr(rhs, vars, max_out = 200L)
        vars <- add_var(vars, nm, vals)
      }
    }

    # for (x in ...) { ... }
    if (is.call(e) && as.character(e[[1]])[1L] == "for" && length(e) >= 4) {
      loop_var <- e[[2]]
      seq_expr <- e[[3]]
      body <- e[[4]]
      if (is.name(loop_var)) {
        nm <- as.character(loop_var)
        seq_vals <- safe_eval_string_expr(seq_expr, vars, max_out = 50L)
        if (length(seq_vals) > 0) {
          child_vars <- vars
          child_vars[[nm]] <- seq_vals
          res_body <- walk_one(body, child_vars)
          paths <- c(paths, res_body$paths)
        } else {
          res_body <- walk_one(body, vars)
          paths <- c(paths, res_body$paths)
        }
      } else {
        res_body <- walk_one(body, vars)
        paths <- c(paths, res_body$paths)
      }
      return(list(paths = paths, vars = vars))
    }

    # Read/write calls
    if (is.call(e)) {
      fn <- tolower(call_fun_end(e))
      if (nzchar(fn)) {
        if (mode == "read" && fn %in% read_funs) {
          path_expr <- get_named_or_pos(e, c("file", "path", "dsn", "input"), 1L)
          vals <- safe_eval_string_expr(path_expr, vars, max_out = 200L)
          if (length(vals) > 0) paths <- c(paths, vals)
        }
        if (mode == "write" && fn %in% write_funs) {
          path_expr <- get_named_or_pos(e, c("file", "path", "dsn", "output"), 2L)
          vals <- safe_eval_string_expr(path_expr, vars, max_out = 200L)
          if (length(vals) > 0) paths <- c(paths, vals)
        }
      }

      # Recurse into call arguments to catch nested calls in if/with/etc.
      args <- as.list(e)[-1]
      for (a in args) {
        res <- walk_one(a, vars)
        if (length(res$paths) > 0) paths <- c(paths, res$paths)
        if (length(paths) >= max_paths) break
      }
    }

    list(paths = paths, vars = vars)
  }

  res <- walk_block(as.list(exprs), list())
  out <- unique(res$paths)
  out <- gsub("\\\\", "/", out)
  out <- gsub("/+", "/", out)
  out <- out[nzchar(out)]
  out <- out[vapply(out, is_valid_data_file_path, logical(1))]
  if (length(out) > max_paths) out <- out[seq_len(max_paths)]
  out
}

# Join here()/file.path() args with "/". When mark_identifiers=TRUE, unquoted identifiers
# are encoded as ${var} so unresolved variables don't get treated as literal folders (e.g. l/_centroids.rds).
paste_here_args <- function(args, mark_identifiers = FALSE) {
  if (!nzchar(args)) return("")
  parts <- strsplit(args, "\\s*,\\s*")[[1]]
  parts <- trimws(parts)
  if (length(parts) == 0) return("")

  out <- character(0)
  for (p in parts) {
    if (!nzchar(p)) next
    is_quoted <- grepl('^["\'][^"\']*["\']$', p)
    p_clean <- gsub('^["\']|["\']$', "", p)
    if (!nzchar(p_clean)) next
    if (!is_quoted && grepl("^[a-zA-Z_][a-zA-Z0-9_.]*$", p_clean)) {
      if (isTRUE(mark_identifiers)) {
        out <- c(out, paste0("${", p_clean, "}"))
      } else {
        out <- c(out, p_clean)
      }
    } else {
      out <- c(out, p_clean)
    }
  }
  if (length(out) == 0) return("")
  a <- paste(out, collapse = "/")
  a <- gsub("\\s+", "", a)
  a <- gsub("/+", "/", a)
  a
}

# Parse here(...) / here::here(...) inner args: join with "/".
parse_here_args <- function(matches, pattern, group = 1L, mark_identifiers = FALSE) {
  if (length(matches) == 0) return(character(0))
  args <- gsub(pattern, paste0("\\", group), matches, ignore.case = TRUE)
  out <- character(length(args))
  for (i in seq_along(args)) {
    out[i] <- paste_here_args(args[i], mark_identifiers = mark_identifiers)
  }
  out
}

is_path_like_value <- function(val) {
  if (!nzchar(val)) return(FALSE)
  x <- trimws(gsub("\\\\", "/", val))
  x <- gsub("/+", "/", x)
  x <- sub("/+$", "", x)
  if (!nzchar(x)) return(FALSE)
  if (x %in% c("/", "\\", "_")) return(FALSE)
  # Avoid unresolved placeholders / shell-ish expansions
  if (grepl("\\$\\{|\\$[A-Za-z_]|`", x)) return(FALSE)
  # If it contains whitespace, require a slash to look like a path.
  if (grepl("\\s", x) && !grepl("/", x)) return(FALSE)
  if (grepl("/", x)) return(TRUE)
  if (grepl("^[A-Za-z]:", x)) return(TRUE)
  if (grepl("^(data|code)/", x, ignore.case = TRUE)) return(TRUE)
  # File-like: require at least one alnum before the dot (exclude ".do")
  grepl("^[A-Za-z0-9].*\\.(r|rmd|do|dta|csv|rds|rda|rdata|xlsx|xls|txt|gpkg|shp|sav|parquet)$", x, ignore.case = TRUE)
}

is_dir_segment_value <- function(var_name, val) {
  if (!nzchar(var_name) || !nzchar(val)) return(FALSE)
  vn <- tolower(trimws(var_name))
  if (!grepl("(_dir|\\.dir)$", vn)) return(FALSE)
  x <- trimws(gsub("\\\\", "/", val))
  x <- gsub("/+", "/", x)
  x <- sub("/+$", "", x)
  if (!nzchar(x)) return(FALSE)
  # Single path segment folder names (e.g. "Mines", "Countries", "Protected Areas").
  if (grepl("/", x, fixed = TRUE)) return(FALSE)
  if (grepl("\\$\\{|\\$[A-Za-z_]|`", x)) return(FALSE)
  if (nchar(x) > 200) return(FALSE)
  grepl("^[A-Za-z0-9][A-Za-z0-9_ .-]*$", x)
}

is_valid_data_file_path <- function(p) {
  if (!nzchar(p)) return(FALSE)
  x <- normalize_path_canonical(p)
  if (!nzchar(x)) return(FALSE)
  bn <- basename(x)
  # Reject extension-only tokens that come from dynamic paste0() like ".rds"
  if (grepl("^\\.[A-Za-z0-9]+$", bn)) return(FALSE)
  # Reject obvious code fragments / dynamic constructs
  if (grepl("`", x, fixed = TRUE)) return(FALSE)
  if (grepl("paste0\\s*\\(|\\b[a-z_]+\\s*\\([^)]*$", x, ignore.case = TRUE)) return(FALSE)
  if (grepl("^[a-z_]+,[a-z]$", x, ignore.case = TRUE)) return(FALSE)
  # Require a file extension
  grepl("^[A-Za-z0-9].*\\.(dta|rds|csv|rda|rdata|xlsx|xls|txt|gpkg|shp|sav|parquet|dbf|prj|shx|cpg)$", bn, ignore.case = TRUE) ||
    grepl("/[^/]+\\.(dta|rds|csv|rda|rdata|xlsx|xls|txt|gpkg|shp|sav|parquet|dbf|prj|shx|cpg)$", x, ignore.case = TRUE)
}

is_valid_data_folder_ref <- function(p) {
  if (!nzchar(p)) return(FALSE)
  x <- normalize_path_canonical(p)
  if (!nzchar(x)) return(FALSE)
  if (grepl("\\$\\{|\\$[A-Za-z_]|`", x)) return(FALSE)
  if (grepl("paste0\\s*\\(|\\b[a-z_]+\\s*\\([^)]*$", x, ignore.case = TRUE)) return(FALSE)
  grepl("/", x) && nchar(x) > 2
}

is_valid_data_ref <- function(p, allow_folder = FALSE) {
  if (is_valid_data_file_path(p)) return(TRUE)
  if (allow_folder && is_valid_data_folder_ref(p)) return(TRUE)
  FALSE
}

resolve_local_path_vars <- function(local_vars, setup_vars) {
  if (length(local_vars) == 0) return(character(0))
  vars <- local_vars
  # Resolve placeholders from extract_local_path_vars(): "__p0__:BASE:SUF"
  for (iter in 1:10) {
    changed <- FALSE
    all_vars <- c(setup_vars, vars)
    for (n in names(vars)) {
      v <- vars[n]
      if (!nzchar(v) || !startsWith(v, "__p0__:")) next
      parts <- strsplit(sub("^__p0__:", "", v), ":", fixed = TRUE)[[1]]
      if (length(parts) < 2) next
      base <- parts[1]
      suf <- paste(parts[-1], collapse = ":")
      base_val <- all_vars[base]
      if (length(base_val) == 0 || !nzchar(base_val) || startsWith(base_val, "__p0__:")) next
      base_val <- sub("/+$", "", normalize_path_canonical(base_val))
      suf <- normalize_path_canonical(suf)
      joined <- normalize_path_canonical(paste0(base_val, "/", suf))
      if (nzchar(joined)) {
        vars[n] <- joined
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  vars
}

extract_libraries <- function(txt) {
  pat <- "(?:library|require)\\s*\\(\\s*[\"']?([^\"')\\s,]+)[\"']?\\s*\\)"
  m <- regmatches(txt, gregexpr(pat, txt, ignore.case = TRUE))[[1]]
  libs <- gsub(pat, "\\1", m, ignore.case = TRUE)
  pac <- extract_pacman_packages(txt)
  unique(c(libs, pac))
}

# pacman::p_load(dplyr, haven, ...) - ignore comments and commented-out packages
extract_pacman_packages <- function(txt) {
  pat <- "pacman::p_load\\s*\\(([^)]+)\\)"
  m <- regmatches(txt, gregexpr(pat, txt, ignore.case = TRUE))[[1]]
  if (length(m) == 0) return(character(0))
  args <- gsub(pat, "\\1", m, ignore.case = TRUE)
  pkgs <- character(0)
  for (a in args) {
    parts <- strsplit(a, ",\\s*")[[1]]
    for (p in parts) {
      p <- sub("#.*", "", p)
      p <- gsub("^[\"']|[\"']$", "", trimws(p))
      if (!nzchar(p) || grepl("^#", p)) next
      if (grepl("^[a-zA-Z0-9._]+$", p)) pkgs <- c(pkgs, p)
    }
  }
  pkgs
}

# Resolve paths to project-relative form (for consistent node IDs)
resolve_here_paths <- function(paths, project_root, file_dir) {
  if (length(paths) == 0) return(character(0))
  project_root <- normalizePath(project_root, mustWork = TRUE)
  project_root_norm <- gsub("\\\\", "/", project_root)
  out <- character(length(paths))
  for (i in seq_along(paths)) {
    p <- paths[i]
    p <- gsub("\\\\", "/", p)
    cand1 <- file.path(project_root, p)
    cand2 <- file.path(file_dir, p)
    found <- FALSE
    if (file.exists(cand1)) {
      abs <- normalizePath(cand1)
      abs_norm <- gsub("\\\\", "/", abs)
      out[i] <- sub(project_root_norm, "", abs_norm, fixed = TRUE)
      out[i] <- sub("^/", "", out[i])
      found <- TRUE
    } else if (file.exists(cand2)) {
      abs <- normalizePath(cand2)
      abs_norm <- gsub("\\\\", "/", abs)
      out[i] <- sub(project_root_norm, "", abs_norm, fixed = TRUE)
      out[i] <- sub("^/", "", out[i])
      found <- TRUE
    }
    if (!found && grepl("^do/", p)) {
      # Fallback: do/X.R -> X.R (common when setup is at root)
      alt <- sub("^do/", "", p)
      cand_alt <- file.path(project_root, alt)
      if (file.exists(cand_alt)) {
        out[i] <- alt
        found <- TRUE
      }
    }
    if (!found) out[i] <- p
  }
  sub("^/", "", out)
}

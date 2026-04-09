# Parse R files and extract dependencies (source, read.*, here())
# Handles: source(), read.csv, readRDS, read_excel, fread, haven::read_dta, etc.
# Resolves here() and here::here() paths relative to project root.

parse_r_project <- function(project_path, exclude_folders = character(0)) {
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
  inferred_stata <- infer_stata_globals_from_r(parsed, setup_vars, project_path)
  setup_vars <- c(setup_vars, inferred_stata)
  stata_globals <- parse_stata_globals(parsed)
  setup_vars <- c(setup_vars, stata_globals)
  parsed <- resolve_paths_with_setup(parsed, setup_vars, project_path)

  # Discover master file (read-only) and path roots from master + setup for path matching
  master_info <- discover_master_and_path_roots(project_path, names(parsed), setup_vars)
  
  list(
    files = parsed,
    project_path = project_path,
    file_paths = r_files,
    setup_vars = setup_vars,
    master_path = master_info$master_path,
    path_roots_from_master = master_info$path_roots_from_master,
    path_roots_from_setup = master_info$path_roots_from_setup,
    setup_master_files = master_info$setup_master_files
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

  # 1. Prefer master by filename: any R file whose basename contains "master"
  for (rel in r_rel) {
    if (grepl("master", basename(rel), ignore.case = TRUE)) {
      master_path <- rel
      break
    }
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
          val <- sub(pat_assign, "\\2", x)
          if (nzchar(var) && nzchar(val)) vars[var] <- val
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
          if (nzchar(name) && nzchar(val)) vars[name] <- val
        }
      }
    }
  }
  vars
}

resolve_paths_with_setup <- function(parsed, setup_vars, project_path = NULL) {
  for (rel in names(parsed)) {
    f <- parsed[[rel]]
    if (!is.null(f$file_type) && f$file_type == "stata") {
      f$data_reads <- resolve_stata_paths(f$data_reads, setup_vars, project_path)
      f$data_writes <- resolve_stata_paths(f$data_writes, setup_vars, project_path)
    } else {
      local_vars <- if (length(f$local_path_vars) > 0) f$local_path_vars else character(0)
      f$sources <- resolve_source_paths(f$sources, setup_vars, local_vars)
      f$data_reads <- resolve_path_args(f$data_reads, setup_vars, local_vars)
      f$data_writes <- resolve_path_args(f$data_writes, setup_vars, local_vars)
      if (length(f$data_read_folders) > 0) {
        f$data_read_folders <- resolve_path_args(f$data_read_folders, setup_vars, local_vars)
        f$data_read_folders <- unique(f$data_read_folders[nzchar(f$data_read_folders)])
      }
    }
    f$data_reads <- unique(f$data_reads[nzchar(f$data_reads)])
    f$data_writes <- unique(f$data_writes[nzchar(f$data_writes)])
    f$sources <- unique(f$sources[nzchar(f$sources)])
    parsed[[rel]] <- f
  }
  parsed
}

# Resolve Stata paths: substitute ${var} and $var with setup_vars (Stata globals).
# Make project-relative so they match R paths (e.g. employment.dta from 3-Employment.R).
resolve_stata_paths <- function(paths, setup_vars, project_path = NULL) {
  if (length(paths) == 0) return(paths)
  out <- character(0)
  for (p in paths) {
    p <- gsub("\\\\", "/", trimws(p))
    if (!nzchar(p)) next
    for (iter in 1:10) {
      changed <- FALSE
      m_brace <- regmatches(p, regexpr("\\$\\{[a-zA-Z_][a-zA-Z0-9_]*\\}", p))
      for (v in m_brace) {
        name <- sub("^\\$\\{|\\}$", "", v)
        repl <- if (name %in% names(setup_vars)) sub("/+$", "", setup_vars[name]) else ""
        p <- sub(v, repl, p, fixed = TRUE)
        changed <- TRUE
      }
      m_plain <- regmatches(p, regexpr("\\$[a-zA-Z_][a-zA-Z0-9_]*", p))
      for (v in m_plain) {
        name <- sub("^\\$", "", v)
        repl <- if (name %in% names(setup_vars)) sub("/+$", "", setup_vars[name]) else ""
        p <- sub(v, repl, p, fixed = TRUE)
        changed <- TRUE
      }
      if (!changed) break
    }
    p <- gsub("/+", "/", p)
    p <- sub("/+$", "", p)
    if (!nzchar(p)) next
    p <- stata_path_to_project_relative(p, project_path)
    p <- stata_canonical_data_path(p)
    p <- normalize_path_canonical(p)
    if (nzchar(p) && is_valid_data_path(p)) out <- c(out, p)
  }
  out
}

# If path contains /FINAL/, /BUILD/, or /RAW/ (any case), normalize to data/Final/, data/Build/, data/Raw/
# so Stata paths match R (e.g. ${final}/employment.dta, ${projdir}/DATA/BUILD/Africa/file.dta).
# Handles any depth under raw/build/final so Stata-R cross-dependencies match.
# Normalize path for matching only (collapse repeated slashes). No project-specific folder names.
stata_canonical_data_path <- function(p) {
  p <- gsub("\\\\", "/", p)
  p <- gsub("/+", "/", p)
  sub("/+$", "", p)
}

# If path is absolute and under project_path, return project-relative path so Stata and R match.
stata_path_to_project_relative <- function(p, project_path) {
  if (length(project_path) == 0 || !nzchar(project_path)) return(p)
  p <- gsub("\\\\", "/", p)
  proj_norm <- gsub("\\\\", "/", trimws(project_path))
  if (!nzchar(proj_norm)) return(p)
  if (grepl("^[A-Za-z]:", p)) {
    p_abs <- tryCatch(normalizePath(p, mustWork = FALSE), error = function(e) p)
  } else if (startsWith(p, "/")) {
    p_abs <- p
  } else {
    return(p)
  }
  proj_abs <- tryCatch(normalizePath(proj_norm, mustWork = TRUE), error = function(e) proj_norm)
  proj_abs <- gsub("\\\\", "/", proj_abs)
  if (startsWith(p_abs, proj_abs)) {
    p <- sub(proj_abs, "", p_abs, fixed = TRUE)
    p <- sub("^/+", "", p)
  }
  p
}

# Resolve source paths (e.g. build.code.dir/2_Create_unit_IDs/0-Create_unit_IDs.R) so they match file paths.
resolve_source_paths <- function(paths, setup_vars, local_vars = character(0)) {
  if (length(paths) == 0) return(paths)
  out <- character(0)
  for (p in paths) {
    p <- gsub("\\\\", "/", trimws(p))
    if (!nzchar(p)) next
    parts <- strsplit(p, "/")[[1]]
    for (i in seq_along(parts)) {
      seg <- parts[i]
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
resolve_path_args <- function(paths, setup_vars, local_vars = character(0)) {
  if (length(paths) == 0) return(paths)
  all_vars <- c(setup_vars, local_vars)
  resolve_one <- function(p) {
    p <- trimws(p)
    if (!nzchar(p)) return(character(0))
    if (grepl("paste0\\s*\\(|\\b[a-z_]+\\([^)]*$", p, ignore.case = TRUE)) return(character(0))
    # Single identifier that is a path variable: e.g. path_var <- "Countries/file.dta"
    if (!grepl("[,/]", p) && p %in% names(all_vars)) {
      res <- sub("/+$", "", all_vars[p])
      if (nzchar(res) && is_valid_data_path(res)) return(res)
      return(character(0))
    }
    if (grepl("[,]", p) && !grepl("^[a-z_]+,[a-z]$", p, ignore.case = TRUE)) {
      parts <- strsplit(p, "\\s*,\\s*")[[1]]
      resolved <- character(length(parts))
      for (i in seq_along(parts)) {
        x <- trimws(gsub('^["\']|["\']$', "", parts[i]))
        if (x %in% names(all_vars)) resolved[i] <- sub("/+$", "", all_vars[x])
        else if (nzchar(x) && (!grepl("^[a-z_][a-z0-9_.]*$", x, ignore.case = TRUE) || grepl("/|\\.(csv|rds|dta|rda|gpkg|shp|xlsx)$", x, ignore.case = TRUE))) resolved[i] <- x
        else resolved[i] <- x
      }
      new_path <- paste(resolved[resolved != ""], collapse = "/")
      new_path <- gsub("/+", "/", new_path)
      if (nzchar(new_path) && is_valid_data_path(new_path)) return(new_path)
      return(character(0))
    }
    if (grepl("/", p)) {
      parts <- strsplit(p, "/")[[1]]
      parts <- trimws(parts)
      # Multi-pass: keep substituting until no segment is a known var (handles countries_dir, ctries_dir, etc.)
      for (pass in 1:10) {
        changed <- FALSE
        for (j in seq_along(parts)) {
          if (parts[j] %in% names(all_vars)) {
            parts[j] <- sub("/+$", "", all_vars[parts[j]])
            changed <- TRUE
          }
        }
        if (!changed) break
      }
      new_path <- paste(parts, collapse = "/")
      new_path <- gsub("/+", "/", new_path)
      new_path <- sub("/+$", "", new_path)
      if (nzchar(new_path) && is_valid_data_path(new_path)) {
        out_paths <- new_path
        last_seg <- parts[length(parts)]
        has_unresolved <- any(vapply(parts, function(s) nzchar(s) && grepl("^[a-zA-Z_][a-zA-Z0-9_.]*$", s) && !(s %in% names(all_vars)), logical(1)))
        if (has_unresolved && nzchar(last_seg) && grepl("\\.(dta|rds|csv|rda|rdata|xlsx|xls|gpkg|shp|sav)$", last_seg, ignore.case = TRUE) && is_valid_data_path(last_seg))
          out_paths <- c(out_paths, last_seg)
        return(out_paths)
      }
      return(character(0))
    }
    if (is_valid_data_path(p)) return(p)
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
  full_txt <- paste(txt, collapse = "\n")
  data_reads <- extract_stata_reads(full_txt)
  data_writes <- extract_stata_writes(full_txt)
  data_reads <- data_reads[vapply(data_reads, is_valid_data_path, logical(1))]
  data_writes <- data_writes[vapply(data_writes, is_valid_data_path, logical(1))]
  list(
    path = file_path,
    rel_path = rel_path,
    sources = character(0),
    data_reads = unique(data_reads),
    data_read_folders = character(0),
    data_writes = unique(data_writes),
    libraries = character(0),
    file_type = "stata"
  )
}

extract_stata_reads <- function(txt) {
  # use "path", merge using "path", import delimited "path"
  pat <- '(?:use|merge|append)\\s+(?:[^"\']*?\\s+)?(?:using\\s+)?["\']([^"\']+?)["\']'
  m <- regmatches(txt, gregexpr(pat, txt, ignore.case = TRUE))[[1]]
  p1 <- gsub(pat, "\\1", m, ignore.case = TRUE)
  pat2 <- '(?:import\\s+delimited|import\\s+excel|insheet)\\s+(?:using\\s+)?["\']([^"\']+?)["\']'
  m2 <- regmatches(txt, gregexpr(pat2, txt, ignore.case = TRUE))[[1]]
  p2 <- gsub(pat2, "\\1", m2, ignore.case = TRUE)
  paths <- c(p1, p2)
  paths <- gsub("\\\\", "/", paths)
  # Drop tempfile refs: path is only backtick-identifier (use `origin')
  paths <- paths[!grepl("^`[a-zA-Z_][a-zA-Z0-9_]*'$", paths)]
  # Expand paths with Stata locals, e.g. "${final}/full_migration_census`w'.dta" -> full_migration_census1.dta, full_migration_census2.dta
  paths <- expand_stata_loop_paths(txt, paths)
  unique(paths)
}

extract_stata_writes <- function(txt) {
  pat <- '(?:save|export|export\\s+delimited|export\\s+excel)\\s+(?:using\\s+)?["\']([^"\']+?)["\']'
  m <- regmatches(txt, gregexpr(pat, txt, ignore.case = TRUE))[[1]]
  paths <- gsub(pat, "\\1", m, ignore.case = TRUE)
  paths <- gsub("\\\\", "/", paths)
  paths <- expand_stata_loop_paths(txt, paths)
  unique(paths)
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
        if (nzchar(p_exp) && is_valid_data_path(p_exp)) out <- c(out, p_exp)
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
  data_reads <- dr$paths[vapply(dr$paths, is_valid_data_path, logical(1))]
  data_read_folders <- if (!is.null(dr$folder_paths)) dr$folder_paths else character(0)
  data_writes <- extract_data_writes(full_txt)
  data_writes <- data_writes[vapply(data_writes, is_valid_data_path, logical(1))]
  libraries <- extract_libraries(full_txt)
  
  # Resolve here() paths (file existence only; setup/local vars applied later in resolve_paths_with_setup)
  project_root <- normalizePath(project_path, mustWork = TRUE)
  sources <- resolve_here_paths(sources, project_root, dirname(file_path))
  data_reads <- resolve_here_paths(data_reads, project_root, dirname(file_path))
  data_reads <- data_reads[vapply(data_reads, is_valid_data_path, logical(1))]
  data_writes <- resolve_here_paths(data_writes, project_root, dirname(file_path))
  data_writes <- data_writes[vapply(data_writes, is_valid_data_path, logical(1))]
  
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
  
  m_here <- regmatches(txt, gregexpr(pat_here, txt, ignore.case = TRUE))[[1]]
  paths <- c(paths, parse_here_args(m_here, pat_here, 2L))
  
  m_here2 <- regmatches(txt, gregexpr(pat_here2, txt, ignore.case = TRUE))[[1]]
  paths <- c(paths, parse_here_args(m_here2, pat_here2, 2L))
  
  pat_dta <- 'read_dta\\s*\\(\\s*here::here\\s*\\(([^)]+)\\)'
  pat_dta2 <- 'read\\.dta13?\\s*\\(\\s*here::here\\s*\\(([^)]+)\\)'
  m_dta <- regmatches(txt, gregexpr(pat_dta, txt, ignore.case = TRUE))[[1]]
  m_dta2 <- regmatches(txt, gregexpr(pat_dta2, txt, ignore.case = TRUE))[[1]]
  paths <- c(paths, parse_here_args(m_dta, pat_dta, 1L), parse_here_args(m_dta2, pat_dta2, 1L))

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
      if (nzchar(fname) && is_valid_data_path(fname)) paths <- c(paths, fname)
    }
  }
  m_fp <- regmatches(txt, gregexpr(pat_filepath, txt, ignore.case = TRUE))[[1]]
  for (x in m_fp) {
    args <- gsub(pat_filepath, "\\2", x, ignore.case = TRUE)
    p <- paste_here_args(args)
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
      if (nzchar(p_combined) && is_valid_data_path(p_combined)) paths <- c(paths, p_combined)
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
    if (!nzchar(x)) {
      out[i] <- x
      next
    }
    x <- gsub("\\\\", "/", trimws(x))
    x <- gsub("/+", "/", x)
    out[i] <- sub("/+$", "", x)
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
  m_here <- regmatches(txt, gregexpr(pat_here, txt, ignore.case = TRUE))[[1]]
  paths <- c(paths, parse_here_args(m_here, pat_here, 2L))
  m_here2 <- regmatches(txt, gregexpr(pat_here2, txt, ignore.case = TRUE))[[1]]
  paths <- c(paths, parse_here_args(m_here2, pat_here2, 2L))
  m_fp <- regmatches(txt, gregexpr(pat_filepath, txt, ignore.case = TRUE))[[1]]
  for (x in m_fp) {
    args <- gsub(pat_filepath, "\\2", x, ignore.case = TRUE)
    p <- paste_here_args(args)
    if (nzchar(p)) paths <- c(paths, p)
  }
  unique(paths)
}

# Join here()/file.path() args with "/"; keep variable names (raw.dir, build.dir) for resolve_path_args to substitute from setup_vars
paste_here_args <- function(args) {
  a <- gsub('["\']\\s*,\\s*["\']', "/", args)
  a <- gsub('["\']', "", a)
  a <- gsub("\\s+", "", a)
  # Replace comma between args with / so we get "raw.dir/Countries/ctry_code/..."
  a <- gsub("\\s*,\\s*", "/", a)
  a
}

# Parse here(...) / here::here(...) inner args: join with "/", keep var names for later setup resolution
parse_here_args <- function(matches, pattern, group = 1L) {
  if (length(matches) == 0) return(character(0))
  args <- gsub(pattern, paste0("\\", group), matches, ignore.case = TRUE)
  out <- character(length(args))
  for (i in seq_along(args)) {
    a <- gsub('["\']\\s*,\\s*["\']', "/", args[i])
    a <- gsub('["\']', "", a)
    a <- gsub("\\s+", "", a)
    # Join comma-separated args with / (preserve variable names like raw.dir, build.dir for resolve_path_args)
    a <- gsub("\\s*,\\s*", "/", a)
    out[i] <- a
  }
  out
}

# Filter out bogus paths (variable references, R code fragments, nested paste0, dynamic paste0 fragments)
is_valid_data_path <- function(p) {
  if (!nzchar(p)) return(FALSE)
  # Backtick is R variable syntax (e.g. level `i, level `l in paste0) - never part of a dataset path
  if (grepl("`", p, fixed = TRUE)) return(FALSE)
  if (grepl("paste0\\s*\\(|\\b[a-z_]+\\s*\\([^)]*$", p, ignore.case = TRUE)) return(FALSE)
  # Reject paths that look like R code / variable segments (e.g. "geo/lvl", "ctry_str/_adm", "t/lvl/00", "census/yr/.rds")
  if (grepl("^[a-z_][a-z0-9_]*/[a-z_][a-z0-9_]*(/[a-z_][a-z0-9_]*)*$", p, ignore.case = TRUE) && !grepl("\\.(csv|rds|dta|rda|gpkg|shp|xlsx|txt)$", p, ignore.case = TRUE))
    return(FALSE)
  # Reject paths with a 1-2 char segment that looks like a variable (e.g. "fl_nl_yrs_w/w/.rds" has "w", "census/yr/.rds" has "yr")
  segs <- strsplit(p, "/")[[1]]
  allow_short <- c("r", "R", "id", "do")  # allow e.g. output/R/id.rds
  for (s in segs) {
    if (nzchar(s) && nchar(s) <= 2 && grepl("^[a-z_][a-z0-9_]*$", s, ignore.case = TRUE) && !(tolower(s) %in% tolower(allow_short)))
      return(FALSE)
  }
  has_ext <- grepl("\\.(csv|rds|rda|rdata|xlsx|xls|dta|txt|gpkg|shp)$", p, ignore.case = TRUE)
  has_slash <- grepl("/", p)
  if (grepl("^[a-z_]+,[a-z]$", p, ignore.case = TRUE)) return(FALSE)
  has_ext || (has_slash && nchar(p) > 5)
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

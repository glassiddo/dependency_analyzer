# Configuration helpers for r_dep_analyzer.
#
# Goal: keep config optional and dependency-light. We support a small YAML subset
# (enough for the repo's config.yaml) without requiring external packages.
#
# Supported YAML subset:
# - comments with '#'
# - nested maps via indentation
# - scalars: strings (quoted/unquoted), booleans, numbers
# - inline lists: [a, b, "c"]

`%||%` <- function(x, y) if (length(x) == 0 || is.null(x)) y else x

dep_default_config <- function() {
  list(
    version = 2,
    scan = list(
      r_extensions = c(".R", ".r", ".Rmd", ".RMD"),
      data_extensions = c(".csv", ".CSV", ".rds", ".RDS", ".rda", ".RData", ".xlsx", ".xls", ".dta"),
      exclude_dirs = c("_dependency_analysis", ".git", ".Rproj.user", "renv", ".Rhistory")
    ),
    data = list(
      sample_rows = 5,
      stata_read_contents = FALSE
    ),
    output = list(
      dir_name = "_dependency_analysis",
      master_file = "master_summary.md",
      viz_file = "dependency_graph.html"
    ),
    graph = list(
      roots = character(0),
      show_meta = FALSE,
      show_independent = FALSE,
      exclude = character(0),
      # Meta files/patterns to hide from the rendered graph by default.
      # Patterns are glob-like against both the full rel path and the basename.
      meta_patterns = c(
        # Python packaging / orchestration
        "setup.py",
        "pyproject.toml",
        "requirements*.txt",
        "Pipfile",
        "poetry.lock",
        # Node / TS tooling
        "package.json",
        "package-lock.json",
        "pnpm-lock.yaml",
        "yarn.lock",
        "tsconfig*.json",
        "webpack*.js",
        "vite.config.*",
        "rollup.config.*",
        # Build / infra
        "Makefile",
        "CMakeLists.txt",
        "Dockerfile",
        ".github/workflows/*.yml",
        ".github/workflows/*.yaml",
        # Common research orchestrators in this repo
        "*[Mm]aster*.R",
        "*[Ss]etup*.R",
        "*[Cc]onfig*.R",
        "*[Ii]nit*.R",
        "*_Master.do",
        "*master*.do",
        "*setup*.do"
      )
    )
  )
}

dep_is_abs_path <- function(p) {
  if (!nzchar(p)) return(FALSE)
  grepl("^[A-Za-z]:[/\\\\]", p) || grepl("^[/\\\\]{2}[^/\\\\]+[/\\\\]+[^/\\\\]+", p) || grepl("^/", p)
}

dep_path_norm <- function(p) {
  gsub("\\\\", "/", trimws(p))
}

dep_glob_to_regex <- function(glob) {
  g <- dep_path_norm(glob)
  # Escape regex metacharacters, then reintroduce glob tokens.
  g <- gsub("([.\\+\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1", g, perl = TRUE)
  g <- gsub("\\*", ".*", g, fixed = FALSE)
  g <- gsub("\\?", ".", g, fixed = TRUE)
  paste0("^", g, "$")
}

dep_match_any_glob <- function(path, patterns) {
  if (length(patterns) == 0) return(FALSE)
  p <- dep_path_norm(path)
  b <- basename(p)
  pats <- patterns[nzchar(patterns)]
  if (length(pats) == 0) return(FALSE)
  regs <- vapply(pats, dep_glob_to_regex, character(1))
  any(vapply(regs, function(r) grepl(r, p, perl = TRUE) || grepl(r, b, perl = TRUE), logical(1)))
}

dep_deep_merge <- function(base, override) {
  if (is.null(override)) return(base)
  if (!is.list(base) || !is.list(override)) return(override)
  out <- base
  for (nm in names(override)) {
    if (nm %in% names(out)) {
      out[[nm]] <- dep_deep_merge(out[[nm]], override[[nm]])
    } else {
      out[[nm]] <- override[[nm]]
    }
  }
  out
}

dep_yaml_strip_comments <- function(lines) {
  # Strip comments outside quotes (simple, conservative: if line starts with # after ws, drop).
  out <- character(0)
  for (ln in lines) {
    if (!nzchar(trimws(ln))) next
    if (grepl("^\\s*#", ln)) next
    # Remove trailing comments if there is at least one space before '#'.
    ln2 <- sub("\\s+#.*$", "", ln)
    out <- c(out, ln2)
  }
  out
}

dep_yaml_parse_scalar <- function(v) {
  v <- trimws(v)
  if (!nzchar(v)) return("")
  if (v %in% c("true", "TRUE", "yes", "YES")) return(TRUE)
  if (v %in% c("false", "FALSE", "no", "NO")) return(FALSE)
  if (grepl("^[0-9]+$", v)) return(as.integer(v))
  if (grepl("^[0-9]+\\.[0-9]+$", v)) return(as.numeric(v))
  # quoted string
  if ((startsWith(v, "\"") && endsWith(v, "\"")) || (startsWith(v, "'") && endsWith(v, "'"))) {
    return(substr(v, 2, nchar(v) - 1))
  }
  v
}

dep_yaml_parse_inline_list <- function(v) {
  v <- trimws(v)
  if (!startsWith(v, "[") || !endsWith(v, "]")) return(NULL)
  inner <- trimws(substr(v, 2, nchar(v) - 1))
  if (!nzchar(inner)) return(character(0))
  # Split on commas not inside quotes (simple scanner).
  parts <- character(0)
  cur <- ""
  in_q <- FALSE
  q_char <- ""
  chars <- strsplit(inner, "", fixed = TRUE)[[1]]
  for (ch in chars) {
    if (!in_q && (ch == "\"" || ch == "'")) {
      in_q <- TRUE
      q_char <- ch
      cur <- paste0(cur, ch)
      next
    }
    if (in_q && ch == q_char) {
      in_q <- FALSE
      q_char <- ""
      cur <- paste0(cur, ch)
      next
    }
    if (!in_q && ch == ",") {
      parts <- c(parts, trimws(cur))
      cur <- ""
      next
    }
    cur <- paste0(cur, ch)
  }
  parts <- c(parts, trimws(cur))
  vapply(parts, dep_yaml_parse_scalar, character(1))
}

dep_yaml_read <- function(path) {
  if (!file.exists(path)) return(list())
  # Prefer yaml package if available (more robust).
  if (requireNamespace("yaml", quietly = TRUE)) {
    y <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
    if (!is.null(y) && is.list(y)) return(y)
  }

  raw <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  lines <- dep_yaml_strip_comments(raw)
  if (length(lines) == 0) return(list())

  set_nested <- function(lst, parts, value) {
    if (length(parts) == 0) return(lst)
    k <- parts[1]
    if (length(parts) == 1) {
      lst[[k]] <- value
      return(lst)
    }
    child <- lst[[k]]
    if (is.null(child) || !is.list(child)) child <- list()
    lst[[k]] <- set_nested(child, parts[-1], value)
    lst
  }
  get_nested <- function(lst, parts) {
    cur <- lst
    for (p in parts) {
      if (is.null(cur) || !is.list(cur) || !(p %in% names(cur))) return(NULL)
      cur <- cur[[p]]
    }
    cur
  }

  out <- list()
  key_stack <- character(0)
  indent_stack <- integer(0)

  for (ln in lines) {
    if (!nzchar(trimws(ln))) next
    indent <- attr(regexpr("^\\s*", ln), "match.length")
    while (length(indent_stack) > 0 && indent_stack[length(indent_stack)] >= indent) {
      indent_stack <- indent_stack[-length(indent_stack)]
      key_stack <- key_stack[-length(key_stack)]
    }

    kv <- strsplit(trimws(ln), ":", fixed = TRUE)[[1]]
    key <- trimws(kv[1])
    rest <- if (length(kv) > 1) trimws(paste(kv[-1], collapse = ":")) else ""
    if (!nzchar(key)) next

    if (!nzchar(rest)) {
      key_stack <- c(key_stack, key)
      indent_stack <- c(indent_stack, indent)
      # Ensure the map exists
      existing <- get_nested(out, key_stack)
      if (is.null(existing) || !is.list(existing)) {
        out <- set_nested(out, key_stack, list())
      }
      next
    }

    lst <- dep_yaml_parse_inline_list(rest)
    val <- if (!is.null(lst)) lst else dep_yaml_parse_scalar(rest)
    out <- set_nested(out, c(key_stack, key), val)
  }

  out
}

dep_migrate_config_in_memory <- function(cfg) {
  defaults <- dep_default_config()
  merged <- dep_deep_merge(defaults, cfg %||% list())
  warnings <- character(0)
  # If the config looks like an older one (no version / no graph), note it.
  if (is.null(cfg$version) || is.null(cfg$graph)) {
    warnings <- c(warnings, "Config upgraded in-memory to include graph settings (version 2). Run migrate_config.R to update the file on disk.")
  }
  list(config = merged, warnings = warnings)
}

dep_yaml_write <- function(cfg, target) {
  scalar_to_yaml <- function(x) {
    if (is.logical(x)) return(if (isTRUE(x)) "true" else "false")
    if (is.numeric(x) && length(x) == 1) return(as.character(x))
    # Quote strings that contain special chars or are empty.
    s <- as.character(x)
    if (!nzchar(s)) return("\"\"")
    if (grepl("[:#\\[\\],\\s]", s)) return(paste0("\"", gsub("\"", "\\\\\"", s, fixed = TRUE), "\""))
    s
  }
  list_to_inline <- function(xs) {
    paste0("[", paste(vapply(xs, scalar_to_yaml, character(1)), collapse = ", "), "]")
  }
  write_lines <- character(0)
  emit <- function(lines) write_lines <<- c(write_lines, lines)
  emit_map <- function(obj, indent = 0L) {
    nms <- names(obj)
    if (length(nms) == 0) return()
    for (nm in nms) {
      val <- obj[[nm]]
      pref <- paste0(strrep(" ", indent), nm, ":")
      if (is.list(val) && !is.null(names(val))) {
        emit(pref)
        emit_map(val, indent + 2L)
      } else if (is.atomic(val) && length(val) == 0) {
        emit(paste0(pref, " []"))
      } else if (is.atomic(val) && length(val) > 1) {
        emit(paste0(pref, " ", list_to_inline(val)))
      } else if (is.atomic(val)) {
        emit(paste0(pref, " ", scalar_to_yaml(val)))
      } else if (is.null(val)) {
        emit(paste0(pref, " \"\""))
      } else {
        # fallback
        emit(paste0(pref, " ", scalar_to_yaml(as.character(val))))
      }
    }
  }
  emit("# r_dep_analyzer config (YAML)")
  emit_map(cfg, indent = 0L)
  if (inherits(target, "connection")) {
    writeLines(write_lines, target)
    return(invisible(NULL))
  }
  con <- file(target, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(write_lines, con)
  invisible(target)
}

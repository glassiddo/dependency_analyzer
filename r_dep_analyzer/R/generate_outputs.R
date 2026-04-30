# Generate interactive HTML visualization and optional Markdown summary.

`%|nz|%` <- function(x, y) {
  if (length(x) == 0 || is.null(x)) return(y)
  x1 <- x[1]
  if (is.na(x1)) return(y)
  if (is.character(x1) && nzchar(x1)) x else y
}

write_path_setup_file <- function(parsed, project_path, out_dir) {
  master_path <- parsed$master_path %|nz|% NULL
  stata_master_path <- parsed$stata_master_path %|nz|% NULL
  path_roots_master <- parsed$path_roots_from_master %||% character(0)
  path_roots_setup  <- parsed$path_roots_from_setup  %||% character(0)
  setup_vars        <- parsed$setup_vars             %||% character(0)
  stata_global_debug <- parsed$stata_global_debug    %||% NULL
  setup_master_files <- parsed$setup_master_files    %||% character(0)
  inference_warnings <- parsed$inference_warnings    %||% character(0)
  out_path <- file.path(out_dir, "path_setup.txt")
  con <- file(out_path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines("# Path detection (generated, do not edit)", con)
  writeLines(paste0("# Project: ", project_path), con)
  writeLines("", con)
  writeLines("master_file:", con)
  writeLines(if (length(master_path) > 0 && nzchar(master_path)) master_path else "(none detected)", con)
  writeLines("", con)
  writeLines("stata_master_file:", con)
  writeLines(if (length(stata_master_path) > 0 && nzchar(stata_master_path)) stata_master_path else "(none detected)", con)
  writeLines("", con)
  writeLines("setup_or_master_files_scanned:", con)
  if (length(setup_master_files) > 0) writeLines(paste0("  - ", setup_master_files), con) else writeLines("  (none)", con)
  writeLines("", con)
  writeLines("path_roots_from_master:", con)
  if (length(path_roots_master) > 0) writeLines(paste0("  - ", path_roots_master), con) else writeLines("  (none)", con)
  writeLines("", con)
  writeLines("path_roots_from_setup:", con)
  if (length(path_roots_setup) > 0) writeLines(paste0("  - ", path_roots_setup), con) else writeLines("  (none)", con)
  writeLines("", con)
  writeLines("setup_path_vars:", con)
  if (length(setup_vars) > 0) {
    for (n in names(setup_vars)) if (nzchar(setup_vars[n])) writeLines(paste0("  ", n, ": ", setup_vars[n]), con)
  } else writeLines("  (none)", con)
  writeLines("", con)
  writeLines("stata_global_candidates_from_master:", con)
  if (!is.null(stata_global_debug) && length(stata_global_debug$candidates %||% list()) > 0) {
    cand <- stata_global_debug$candidates %||% list()
    chosen <- stata_global_debug$chosen %||% character(0)
    for (nm in sort(names(cand))) {
      vals <- unique(cand[[nm]] %||% character(0))
      vals <- vals[nzchar(vals)]
      if (length(vals) == 0) next
      writeLines(paste0("  ", nm, ":"), con)
      ch <- chosen[[nm]]
      if (length(ch) > 0 && nzchar(ch)) writeLines(paste0("    chosen: ", ch), con)
      others <- setdiff(vals, ch)
      if (length(others) > 0) writeLines(paste0("    other_candidates:"), con)
      if (length(others) > 0) writeLines(paste0("      - ", others), con)
    }
  } else writeLines("  (none)", con)
  writeLines("", con)
  writeLines("inference_warnings:", con)
  if (length(inference_warnings) > 0) writeLines(paste0("  - ", inference_warnings), con) else writeLines("  (none)", con)
  invisible(out_path)
}

generate_master_summary <- function(graph, issues, parsed, data_info, project_path, out_dir) {
  out_path <- file.path(out_dir, "master_summary.md")
  con <- file(out_path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  wl <- function(...) writeLines(paste(...), con)
  wl("# R Project Dependency Summary"); wl("")
  n_r  <- sum(graph$nodes$type == "r_file")
  n_d  <- sum(graph$nodes$type == "data")
  wl("## Overview"); wl(paste("Scripts:", n_r)); wl(paste("Datasets:", n_d))
  wl(paste("Errors:", sum(issues$severity == "error")))
  wl(paste("Notes:",  sum(issues$severity == "info"))); wl("")
  wl("## Execution Order"); wl("")
  ord <- try_topological_order(graph)
  for (i in seq_along(ord)) wl(paste0(i, ". ", ord[i]))
  invisible(out_path)
}

try_topological_order <- function(graph) {
  r_edges <- graph$edges[graph$edges$type %in% c("sources", "data_flow"), ]
  r_nodes <- graph$nodes$id[graph$nodes$type == "r_file"]
  if (nrow(r_edges) == 0) return(r_nodes)
  sort_edges <- r_edges
  src_idx <- r_edges$type == "sources"
  sort_edges[src_idx, c("from", "to")] <- r_edges[src_idx, c("to", "from")]
  # Defensive: drop edges that reference non-script nodes (e.g. missing sourced files).
  # This keeps igraph happy and matches the fallback behavior which only orders known scripts.
  sort_edges <- sort_edges[sort_edges$from %in% r_nodes & sort_edges$to %in% r_nodes, , drop = FALSE]
  if (nrow(sort_edges) == 0) return(r_nodes)
  if (requireNamespace("igraph", quietly = TRUE)) {
    g <- igraph::graph_from_data_frame(sort_edges[, c("from", "to")], vertices = r_nodes)
    if (igraph::is_dag(g)) {
      topo_fn <- if (existsMethod("topological_sort", "igraph") ||
                     exists("topological_sort", asNamespace("igraph"), inherits = FALSE))
        igraph::topological_sort else igraph::topo_sort
      topo_result <- topo_fn(g, mode = "out")
      return(igraph::V(g)$name[as.integer(topo_result)])
    }
  }
  adj <- split(sort_edges$to, sort_edges$from)
  in_deg <- setNames(rep(0L, length(r_nodes)), r_nodes)
  for (to in sort_edges$to) in_deg[to] <- in_deg[to] + 1L
  queue <- r_nodes[in_deg[r_nodes] == 0]
  ord <- character(0)
  while (length(queue) > 0) {
    n <- queue[1]; queue <- queue[-1]; ord <- c(ord, n)
    for (nb in adj[[n]] %||% character(0)) {
      in_deg[nb] <- in_deg[nb] - 1L
      if (in_deg[nb] == 0) queue <- c(queue, nb)
    }
  }
  if (length(ord) == length(r_nodes)) ord else character(0)
}

get_used_unused_scripts <- function(graph, parsed) {
  r_nodes <- graph$nodes$id[graph$nodes$type == "r_file"]
  ord <- try_topological_order(graph)
  if (length(ord) == 0) ord <- r_nodes
  # Include all scanned scripts in both the graph and the execution order, even if
  # they have zero detected dependencies.
  list(ord = ord, used = r_nodes, unused = character(0))
}

# ---- Execution order ----
build_execution_order_display <- function(graph, parsed, uu) {
  ord <- uu$ord; used <- uu$used; unused <- uu$unused
  if (length(ord) == 0) return('<p>Could not determine order (possible cycles).</p>')
  r_edges <- graph$edges[graph$edges$type %in% c("sources", "data_flow"), ]
  sort_edges <- r_edges
  sort_edges[r_edges$type == "sources", c("from", "to")] <- r_edges[r_edges$type == "sources", c("to", "from")]
  preds_of <- split(sort_edges$from, sort_edges$to)
  used_ord <- ord[ord %in% used]
  levels <- setNames(rep(NA_integer_, length(used)), used)
  for (n in used_ord) {
    preds <- preds_of[[n]]
    if (is.null(preds) || length(preds) == 0) { levels[n] <- 0L } else {
      pl <- levels[preds]; pl <- pl[!is.na(pl) & is.finite(pl)]
      levels[n] <- if (length(pl) > 0) max(pl) + 1L else 0L
    }
  }
  by_level <- split(used, levels[used])
  levs <- sort(unique(levels[is.finite(levels)]))
  pal_bg     <- c("#dbeafe","#d1fae5","#fef3c7","#e0e7ff","#fce7f3","#e0f2fe","#dcfce7","#fff7ed")
  pal_border <- c("#2563eb","#059669","#d97706","#6d28d9","#db2777","#0284c7","#16a34a","#ea580c")
  global_num <- 0L
  out <- '<div class="exec-order"><p class="viz-desc">Files are grouped by step &mdash; same step = no dependencies between them (can run in parallel).</p>'
  for (lev in levs) {
    items  <- by_level[[as.character(lev)]]
    idx    <- (as.integer(lev)) %% length(pal_bg) + 1L
    bg     <- pal_bg[idx]; border <- pal_border[idx]
    step   <- as.integer(lev) + 1L
    can_par <- length(items) > 1
    par_badge <- if (can_par) paste0(' <span class="exec-par-badge">',length(items),' files &mdash; parallel</span>') else ''
    out <- paste0(out, sprintf(
      '<div class="exec-step-group" style="border-left:3px solid %s;background:%s20">',
      border, substr(bg, 1, 7)))
    out <- paste0(out, sprintf('<div class="exec-step-header" style="color:%s">Step %d%s</div><ol class="exec-file-list">', border, step, par_badge))
    for (it in items) {
      global_num <- global_num + 1L
      folder <- gsub("\\\\", "/", dirname(it))
      if (!nzchar(folder) || folder == ".") folder <- "root"
      out <- paste0(out, sprintf(
        '<li data-folder="%s" title="%s"><span class="exec-num">%d</span> %s</li>',
        html_esc(folder), html_esc(it), global_num, html_esc(basename(it))))
    }
    out <- paste0(out, '</ol></div>')
  }
  if (length(unused) > 0) {
    out <- paste0(out, '<details class="exec-unused-details"><summary>Not in execution chain (',
      length(unused), ' file', if (length(unused) != 1) 's' else '', ')</summary><ul class="exec-unused-list">')
    for (u in unused) {
      folder <- gsub("\\\\", "/", dirname(u))
      if (!nzchar(folder) || folder == ".") folder <- "root"
      out <- paste0(out, sprintf('<li data-folder="%s" title="%s">%s</li>', html_esc(folder), html_esc(u), html_esc(basename(u))))
    }
    out <- paste0(out, '</ul></details>')
  }
  paste0(out, '</div>')
}

# ---- Main visualization entry point ----
generate_visualization <- function(graph, issues, parsed, data_info, project_path, out_dir,
                                   project_name = NULL,
                                   roots = character(0),
                                   roots_display = roots,
                                   viz_options = list()) {
  out_path <- file.path(out_dir, "dependency_graph.html")
  if (is.null(project_name) || !nzchar(project_name)) project_name <- basename(project_path)
  write_path_setup_file(parsed, project_path, out_dir)
  if (nrow(graph$nodes) == 0) { message("No nodes to visualize."); return(invisible(NULL)) }

  rev_src <- graph$edges$type == "sources"
  flow_edges <- graph$edges
  flow_edges[rev_src, c("from","to")] <- graph$edges[rev_src, c("to","from")]

  uu <- get_used_unused_scripts(graph, parsed)

  setup_file <- graph$setup_file %|nz|% NULL

  viz_html    <- build_svg_dependency_graph_div(
    graph, issues, flow_edges, uu$used, parsed,
    setup_file = graph$setup_file,
    roots = roots,
    viz_options = viz_options
  )
  exec_display <- build_execution_order_display(graph, parsed, uu)
  html <- build_full_html(parsed, data_info, graph, issues, uu, exec_display, viz_html,
    project_path = project_path, project_name = project_name,
    setup_file = setup_file,
    roots = roots_display)
  con <- file(out_path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(html, con)
  invisible(out_path)
}

# ---- File index ----
# Standalone dependency graph (SVG + vanilla JS). No external CDN dependencies.
build_svg_dependency_graph_div <- function(graph, issues, flow_edges, used, parsed,
                                           setup_file = NULL,
                                           roots = character(0),
                                           viz_options = list()) {
  missing_nodes <- graph$nodes$id[graph$nodes$type == "missing_script"]
  r_nodes <- unique(c(used, missing_nodes))
  if (length(r_nodes) == 0) return('<div class="empty-graph">No scripts in the execution chain to display.</div>')

  r_edges <- flow_edges[
    flow_edges$from %in% r_nodes & flow_edges$to %in% r_nodes &
      flow_edges$type %in% c("sources", "data_flow"), ]

  data_nodes_all <- graph$nodes$id[graph$nodes$type == "data"]
  data_edges <- flow_edges[
    flow_edges$type %in% c("reads", "writes") &
      ((flow_edges$from %in% r_nodes & flow_edges$to %in% data_nodes_all) |
         (flow_edges$to %in% r_nodes & flow_edges$from %in% data_nodes_all)), ]
  data_nodes <- unique(c(
    data_edges$from[data_edges$from %in% data_nodes_all],
    data_edges$to[data_edges$to %in% data_nodes_all]
  ))

  wrong_pairs <- character(0)
  if (nrow(issues) > 0) {
    wd <- issues[issues$type == "wrong_dependency", ]
    if (nrow(wd) > 0) wrong_pairs <- paste(wd$from, wd$to, sep = "|")
  }

  safe_id <- function(x) {
    o <- gsub("[^a-zA-Z0-9]", "_", x)
    if (grepl("^[0-9]", o)) o <- paste0("n", o)
    o
  }
  all_ids <- c(r_nodes, data_nodes)
  id_map <- setNames(make.unique(vapply(all_ids, safe_id, character(1)), sep = "_"), all_ids)

  get_folder <- function(x) { d <- gsub("\\\\", "/", dirname(x)); if (!nzchar(d) || d == ".") "root" else d }
  type_map <- setNames(graph$nodes$type, graph$nodes$id)

  meta_patterns <- viz_options$meta_patterns %||% character(0)
  exclude_patterns <- viz_options$exclude_patterns %||% character(0)
  show_meta_default <- isTRUE(viz_options$show_meta_default %||% FALSE)
  show_indep_default <- isTRUE(viz_options$show_independent_default %||% FALSE)

  match_any <- if (exists("dep_match_any_glob", mode = "function")) dep_match_any_glob else function(path, patterns) FALSE
  roots <- unique(roots[nzchar(roots)])
  meta_by_rule <- function(rel) {
    is_named_meta <- match_any(rel, meta_patterns)
    is_detected_meta <- (!is.null(parsed$setup_master_files) && rel %in% (parsed$setup_master_files %||% character(0))) ||
      (!is.null(parsed$master_path) && nzchar(parsed$master_path) && identical(rel, parsed$master_path)) ||
      (!is.null(parsed$stata_master_path) && nzchar(parsed$stata_master_path) && identical(rel, parsed$stata_master_path)) ||
      (!is.null(setup_file) && nzchar(setup_file) && identical(rel, setup_file))
    is_named_meta || is_detected_meta
  }
  excluded_by_rule <- function(rel) match_any(rel, exclude_patterns)

  # Build JS data via robust JSON serialization (avoid hand-built JSON/escaping).
  nodes <- c(
    lapply(seq_along(r_nodes), function(i) {
      rel <- r_nodes[i]
      id <- id_map[[rel]]
      is_setup_node <- !is.null(setup_file) && identical(rel, setup_file)
      is_missing_node <- !is.null(type_map[[rel]]) && identical(type_map[[rel]], "missing_script")
      is_root <- rel %in% roots
      is_meta <- meta_by_rule(rel)
      is_excluded <- excluded_by_rule(rel)
      list(
        id = id,
        kind = "script",
        label = basename(rel),
        rel = rel,
        folder = get_folder(rel),
        root = is_root,
        meta = is_meta,
        excluded = is_excluded,
        bg = if (is_setup_node) "#fef3c7" else if (is_missing_node) "#f1f5f9" else "#dbeafe",
        border = if (is_setup_node) "#d97706" else if (is_missing_node) "#64748b" else "#3b82f6"
      )
    }),
    lapply(seq_along(data_nodes), function(i) {
      rel <- data_nodes[i]
      id <- id_map[[rel]]
      list(
        id = id,
        kind = "data",
        label = basename(rel),
        rel = rel,
        folder = "data",
        bg = "#dcfce7",
        border = "#16a34a"
      )
    })
  )

  edges <- list()
  edge_i <- 0L
  add_edge <- function(from, to, kind, wrong = FALSE, reason = "") {
    fid <- id_map[[from]]
    tid <- id_map[[to]]
    if (is.na(fid) || !nzchar(fid) || is.na(tid) || !nzchar(tid)) return(invisible(NULL))
    edge_i <<- edge_i + 1L
    edges[[edge_i]] <<- list(
      id = paste0("e", edge_i),
      from = fid,
      to = tid,
      kind = kind,
      wrong = isTRUE(wrong),
      reason = if (is.na(reason) || !nzchar(reason)) "" else as.character(reason)
    )
    invisible(NULL)
  }
  has_reason <- "reason" %in% colnames(r_edges)
  for (i in seq_len(nrow(r_edges))) {
    e <- r_edges[i, ]
    is_wrong <- paste(e$from, e$to, sep = "|") %in% wrong_pairs
    reason_str <- if (has_reason && !is.na(e$reason) && nzchar(e$reason)) e$reason else e$type
    add_edge(e$from, e$to, e$type, is_wrong, reason_str)
  }
  has_reason_d <- "reason" %in% colnames(data_edges)
  for (i in seq_len(nrow(data_edges))) {
    e <- data_edges[i, ]
    reason_str <- if (has_reason_d && !is.na(e$reason) && nzchar(e$reason)) e$reason else e$type
    add_edge(e$from, e$to, e$type, FALSE, reason_str)
  }
  nodes_json <- jsonlite::toJSON(nodes, auto_unbox = TRUE)
  edges_json <- jsonlite::toJSON(edges, auto_unbox = TRUE)
  # Extra defense: ensure literal "</script" cannot terminate the script tag.
  nodes_json <- gsub("</script", "<\\\\/script", nodes_json, ignore.case = TRUE)
  edges_json <- gsub("</script", "<\\\\/script", edges_json, ignore.case = TRUE)

  # Dataset nodes are hidden by default for all projects.
  # Override: set R_DEP_SHOW_DATASETS_DEFAULT=1 (or true/yes/on) to show datasets by default.
  show_datasets_env <- trimws(Sys.getenv("R_DEP_SHOW_DATASETS_DEFAULT", ""))
  show_datasets_default <- nzchar(show_datasets_env) &&
    tolower(show_datasets_env) %in% c("1", "true", "yes", "y", "on")

  js_template <- '(function(){
    var allNodesData = __NODES__;
    var allEdgesData = __EDGES__;
    var hideMeta = __HIDE_META__;
    var hideIndependent = __HIDE_INDEPENDENT__;
    var showDatasets = __SHOW_DATASETS__;
    var selectedId = null;
    var isFs = false;
    var hasRenderedOnce = false;

   function escHtml(s) {
     return String(s).replace(/&/g,\"&amp;\").replace(/</g,\"&lt;\").replace(/>/g,\"&gt;\").replace(/\\\"/g,\"&quot;\");
   }

   function computeScriptDegree(nodes, edges) {
     function includedForDegree(n) {
       if (!n || n.kind !== \"script\") return false;
       if (n.excluded) return false;
       if (hideMeta && n.meta) return false;
       return true;
     }
     var scriptIds = new Set(nodes.filter(includedForDegree).map(function(n){ return n.id; }));
     // "Independent" means: no other script depends on it (out-degree == 0)
     // in the currently in-scope graph (respects hideMeta).
     var outdeg = {};
     nodes.forEach(function(n){ if (includedForDegree(n)) outdeg[n.id] = 0; });
     edges.forEach(function(e){
       // Only count script-to-script edges that represent dependencies between scripts.
       // Ignore reads/writes because those connect scripts to datasets.
       if ((e.kind === \"sources\" || e.kind === \"data_flow\") && scriptIds.has(e.from) && scriptIds.has(e.to)) {
         outdeg[e.from] = (outdeg[e.from] || 0) + 1;
       }
     });
     return outdeg;
   }

   function isIndependentScript(n, degree) {
     if (!n || n.kind !== \"script\") return false;
     if (n.excluded) return false;
     if (n.root) return false;
     if (hideMeta && n.meta) return false;
     return ((degree[n.id] || 0) === 0);
   }

   function computeIndependentHiddenSet(nodes, edges) {
     // Hide "independent" scripts (out-degree == 0) relative to the current graph
     // after applying meta/excluded filtering. To avoid peeling the *entire* DAG,
     // we apply a small bounded cascade (2 rounds).
     var scripts = nodes.filter(function(n){
       if (!n || n.kind !== \"script\") return false;
       if (n.excluded) return false;
       if (hideMeta && n.meta) return false;
       return true;
     });
     var isRoot = {};
     scripts.forEach(function(n){ isRoot[n.id] = !!n.root; });

     var removed = new Set();
     var maxRounds = 2;

     function buildOutdeg(active) {
       var outdeg = {};
       scripts.forEach(function(n){ if (active.has(n.id)) outdeg[n.id] = 0; });
       edges.forEach(function(e){
         if (!(e.kind === \"sources\" || e.kind === \"data_flow\")) return;
         if (!active.has(e.from) || !active.has(e.to)) return;
         outdeg[e.from] = (outdeg[e.from] || 0) + 1;
       });
       return outdeg;
     }

     for (var round = 0; round < maxRounds; round++) {
       var active = new Set(scripts.map(function(n){ return n.id; }).filter(function(id){ return !removed.has(id); }));
       if (active.size === 0) break;
       var outdeg = buildOutdeg(active);
       var newly = [];
       active.forEach(function(id){
         if (!isRoot[id] && (outdeg[id] || 0) === 0) newly.push(id);
       });
       if (newly.length === 0) break;
       newly.forEach(function(id){ removed.add(id); });
     }
     return removed;
   }

   function isVisible(n, degree, indepHidden) {
     if (n && n.excluded) return false;
     if (!showDatasets && n && n.kind === \"data\") return false;
     if (n && n.root) return true;
     if (hideMeta && n && n.meta) return false;
     if (hideIndependent && indepHidden && indepHidden.has(n.id)) return false;
     return true;
   }

   function styleEdge(e) {
     var color = \"#94a3b8\";
     var dashes = false;
     if (e.kind === \"reads\")  { color = \"#16a34a\"; dashes = [5,3]; }
     else if (e.kind === \"writes\") { color = \"#ea580c\"; dashes = [5,3]; }
     else if (e.kind === \"data_flow\") { color = \"#475569\"; dashes = false; }
      return {color: color, dashes: dashes};
    }

   function buildVisibleData() {
     var degree = computeScriptDegree(allNodesData, allEdgesData);
     var indepHidden = hideIndependent ? computeIndependentHiddenSet(allNodesData, allEdgesData) : null;
     var vn = allNodesData.filter(function(n){ return isVisible(n, degree, indepHidden); });
     var vi = new Set(vn.map(function(n){ return n.id; }));
     var ve = allEdgesData.filter(function(e){ return vi.has(e.from) && vi.has(e.to); });

     var nodes = vn.map(function(n){
       return {
         id: n.id,
         label: n.label,
         title: n.rel,
         shape: \"box\",
         margin: 10,
         font: {face: \"Segoe UI,system-ui,sans-serif\", size: 12, color: \"#0f172a\"},
         color: {background: n.bg, border: n.border},
         borderWidth: (selectedId && n.id===selectedId) ? 3 : 1.5
       };
     });

     var edges = ve.map(function(e){
       var st = styleEdge(e);
       return {
         id: e.id,
         from: e.from,
         to: e.to,
         arrows: {to: {enabled: true, scaleFactor: 0.7}},
         color: {color: st.color},
         dashes: st.dashes,
         width: (selectedId && (e.from===selectedId || e.to===selectedId)) ? 2.6 : 1.2,
         opacity: (selectedId && !(e.from===selectedId || e.to===selectedId)) ? 0.15 : 1.0,
         title: e.reason || e.kind || \"edge\"
       };
     });

      return {nodes: nodes, edges: edges};
    }

    var container = document.getElementById(\"dep-network\");
    if (!container) return;

   // NOTE: The report previously used vis-network via a CDN loader.
   // The graph is now rendered exclusively via the inline SVG renderer below so the
   // report is fully offline-capable and has no external script dependencies.

   function updateIndependentButton() {
     var btn = document.getElementById(\"indep-btn\");
     if (!btn) return;
     btn.textContent = hideIndependent ? \"Show independent scripts\" : \"Hide independent scripts\";
   }
   function updateMetaButton() {
     var btn = document.getElementById(\"meta-btn\");
     if (!btn) return;
     btn.textContent = hideMeta ? \"Show meta files\" : \"Hide meta files\";
   }
   function updateDatasetsButton() {
     var btn = document.getElementById(\"datasets-btn\");
     if (!btn) return;
     btn.textContent = showDatasets ? \"Hide datasets\" : \"Show datasets\";
   }

   window.toggleDatasets = function() {
     showDatasets = !showDatasets;
     if (!showDatasets && selectedId) {
       var sel = allNodesData.filter(function(n){ return n.id === selectedId; })[0];
       if (sel && sel.kind === \"data\") selectedId = null;
     }
     updateDatasetsButton();
     rebuildGraph();
   };
   window.toggleIndependentScripts = function() {
     hideIndependent = !hideIndependent;
     if (hideIndependent && selectedId) {
       var sel = allNodesData.filter(function(n){ return n.id === selectedId; })[0];
       var indepHidden = computeIndependentHiddenSet(allNodesData, allEdgesData);
       if (sel && indepHidden && indepHidden.has(sel.id)) selectedId = null;
     }
     updateIndependentButton();
     rebuildGraph();
   };
   window.toggleMetaFiles = function() {
     hideMeta = !hideMeta;
     if (hideMeta && selectedId) {
       var degree = computeScriptDegree(allNodesData, allEdgesData);
       var sel = allNodesData.filter(function(n){ return n.id === selectedId; })[0];
       if (sel && sel.meta && !sel.root) selectedId = null;
     }
     updateMetaButton();
     rebuildGraph();
   };
   window.showAllScripts = function() {
     hideIndependent = false;
     hideMeta = false;
     showDatasets = false;
     selectedId = null;
     updateIndependentButton();
     updateMetaButton();
     updateDatasetsButton();
     rebuildGraph();
   };
   // The SVG renderer will set up initial render later in this script.

  function svgEl(name, attrs) {
    var el = document.createElementNS("http://www.w3.org/2000/svg", name);
    if (attrs) Object.keys(attrs).forEach(function(k){ el.setAttribute(k, attrs[k]); });
    return el;
  }

  function layoutNodes(nodes, edges) {
    var scriptNodes = nodes.filter(function(n){ return n.kind !== \"data\"; });
    var dataNodes   = nodes.filter(function(n){ return n.kind === \"data\"; });
    var scriptIdSet = new Set(scriptNodes.map(function(n){ return n.id; }));

    // Topological sort on script nodes only (ignore data nodes and data edges)
    var scriptEdges = edges.filter(function(e){
      return scriptIdSet.has(e.from) && scriptIdSet.has(e.to);
    });
    var out = {}; var indeg = {};
    scriptNodes.forEach(function(n){ out[n.id]=[]; indeg[n.id]=0; });
    scriptEdges.forEach(function(e){
      out[e.from].push(e.to);
      indeg[e.to] = (indeg[e.to]||0) + 1;
    });
    var q = scriptNodes.map(function(n){ return n.id; }).filter(function(id){ return (indeg[id]||0)===0; });
    var order = [];
    while (q.length) {
      var n = q.shift(); order.push(n);
      (out[n]||[]).forEach(function(m){ indeg[m]--; if (indeg[m]===0) q.push(m); });
    }
    if (order.length !== scriptNodes.length) order = scriptNodes.map(function(n){ return n.id; });

    var level = {};
    order.forEach(function(id){ level[id]=0; });
    order.forEach(function(id){
      (out[id]||[]).forEach(function(m){
        level[m] = Math.max(level[m]||0, (level[id]||0)+1);
      });
    });

    // Assign fractional levels to data nodes:
    //   level = max(writer levels) + 0.5  (output: sits below its writers)
    //   level = min(reader levels) - 0.5  (input-only: sits above its readers)
    var dataLevel = {};
    dataNodes.forEach(function(dn){
      var did = dn.id;
      var writerLevels = [], readerLevels = [];
      edges.forEach(function(e){
        if (e.kind === \"writes\" && e.to === did && level[e.from] !== undefined)
          writerLevels.push(level[e.from]);
        if (e.kind === \"reads\"  && e.from === did && level[e.to] !== undefined)
          readerLevels.push(level[e.to]);
      });
      if (writerLevels.length > 0)
        dataLevel[did] = Math.max.apply(null, writerLevels) + 0.5;
      else if (readerLevels.length > 0)
        dataLevel[did] = Math.min.apply(null, readerLevels) - 0.5;
      // nodes with neither are skipped (no placement)
    });

    // Collect and sort all unique level values
    var allLevelVals = [];
    Object.keys(level).forEach(function(id){
      if (allLevelVals.indexOf(level[id]) < 0) allLevelVals.push(level[id]);
    });
    Object.keys(dataLevel).forEach(function(id){
      if (allLevelVals.indexOf(dataLevel[id]) < 0) allLevelVals.push(dataLevel[id]);
    });
    allLevelVals.sort(function(a,b){ return a-b; });

    // Group nodes by their level key
    var levelToNodes = {};
    allLevelVals.forEach(function(l){ levelToNodes[String(l)] = []; });
    scriptNodes.forEach(function(n){
      var l = level[n.id];
      if (l !== undefined) levelToNodes[String(l)].push(n);
    });
    dataNodes.forEach(function(n){
      var l = dataLevel[n.id];
      if (l !== undefined) levelToNodes[String(l)].push(n);
    });
    allLevelVals.forEach(function(l){
      levelToNodes[String(l)].sort(function(a,b){ return (a.label||\"\").localeCompare(b.label||\"\"); });
    });

    var nodeW = 170, nodeH = 30, xGap = 200, yGap = 90, margin = 28;
    var maxScriptCols = 1;
    allLevelVals.forEach(function(l){
      var sn = levelToNodes[String(l)].filter(function(n){ return n.kind !== \"data\"; });
      if (sn.length > maxScriptCols) maxScriptCols = sn.length;
    });
    var width  = margin*2 + Math.max(1, maxScriptCols-1)*xGap + nodeW;
    var height = margin*2 + Math.max(0, allLevelVals.length-1)*yGap + nodeH;

    var pos = {};
    allLevelVals.forEach(function(l, rowIdx){
      var rowNodes = levelToNodes[String(l)];
      var isDataRow = (l !== Math.floor(l));
      var nW = nodeW;
      var nH = nodeH;
      var y  = margin + rowIdx * yGap;
      var sx;
      if (isDataRow) {
        var totalW = rowNodes.length * nW + Math.max(0, rowNodes.length-1) * 20;
        sx = (width - totalW) / 2;
        rowNodes.forEach(function(n, idx){
          pos[n.id] = {x: sx + idx*(nW+20), y: y, w: nW, h: nH};
        });
      } else {
        sx = margin + ((maxScriptCols - rowNodes.length) * xGap) / 2;
        rowNodes.forEach(function(n, idx){
          pos[n.id] = {x: sx + idx*xGap, y: y, w: nW, h: nH};
        });
      }
    });
    return {pos:pos, width:width, height:height};
  }

  function renderSvg(nodes, edges, preserveViewBox) {
    var svg = document.getElementById(\"dep-svg\");
    if (!svg) return;
    var prevViewBox = svg.getAttribute(\"viewBox\");
    while (svg.firstChild) svg.removeChild(svg.firstChild);

    if (!nodes || nodes.length===0) {
      var t = svgEl(\"text\", {x:10, y:20, fill:\"#64748b\"}); t.textContent = \"No nodes to display.\";
      svg.appendChild(t);
      return;
    }

    var layout = layoutNodes(nodes, edges);
    var fitView = \"0 0 \" + layout.width + \" \" + layout.height;
    svg.setAttribute(\"data-fit-view\", fitView);
    if (!preserveViewBox || !prevViewBox) svg.setAttribute(\"viewBox\", fitView);
    else svg.setAttribute(\"viewBox\", prevViewBox);

    var providers = new Set();
    var consumers = new Set();
    if (selectedId) {
      edges.forEach(function(e){
        if (e.to===selectedId) providers.add(e.from);
        if (e.from===selectedId) consumers.add(e.to);
      });
    }

    edges.forEach(function(e){
      var a = layout.pos[e.from], b = layout.pos[e.to];
      if (!a || !b) return;
      var x1 = a.x + a.w/2, y1 = a.y + a.h;
      var x2 = b.x + b.w/2, y2 = b.y;
      var dy = y2 - y1;
      var c1y = y1 + Math.min(70, Math.max(20, dy/2));
      var c2y = y2 - Math.min(70, Math.max(20, dy/2));
      var d = \"M \" + x1 + \" \" + y1 + \" C \" + x1 + \" \" + c1y + \" \" + x2 + \" \" + c2y + \" \" + x2 + \" \" + y2;
      var stroke = \"#94a3b8\";
      var dash = null;
      if (e.kind === \"reads\")  { stroke = \"#16a34a\"; dash = \"5,3\"; }
      else if (e.kind === \"writes\") { stroke = \"#ea580c\"; dash = \"5,3\"; }
      else if (e.kind === \"data_flow\") { stroke = \"#475569\"; }
      var width = 1.2;
      var opacity = 1.0;
      if (selectedId) {
        var connected = (e.from===selectedId)||(e.to===selectedId);
        opacity = connected ? 1.0 : 0.12;
        width = connected ? 2.2 : 1.0;
      }
      var attrs = {d:d, fill:\"none\", stroke:stroke, \"stroke-width\":width, \"stroke-opacity\":opacity};
      if (dash) attrs[\"stroke-dasharray\"] = dash;
      svg.appendChild(svgEl(\"path\", attrs));
    });

    nodes.forEach(function(n){
      var p = layout.pos[n.id];
      if (!p) return;
      var isSel = selectedId && n.id===selectedId;
      var isProv = selectedId && providers.has(n.id);
      var isCons = selectedId && consumers.has(n.id);
      var bg = n.bg, brd = n.border, txt = \"#1e293b\", bw = 1.5;
      if (selectedId) {
        if (isSel)       { bg=\"#1e40af\"; brd=\"#1e3a8a\"; txt=\"#ffffff\"; bw=3; }
        else if (isProv) { bg=\"#14532d\"; brd=\"#16a34a\"; txt=\"#ffffff\"; bw=2; }
        else if (isCons) { bg=\"#7c2d12\"; brd=\"#c2410c\"; txt=\"#ffffff\"; bw=2; }
        else             { bg=\"#f1f5f9\"; brd=\"#e2e8f0\"; txt=\"#94a3b8\"; bw=1; }
      }
      var g = svgEl(\"g\", {\"data-id\":n.id, cursor:\"pointer\"});
      var title = svgEl(\"title\"); title.textContent = n.rel;
      var shapeEl = svgEl(\"rect\", {x:p.x, y:p.y, width:p.w, height:p.h, rx:6, ry:6, fill:bg, stroke:brd, \"stroke-width\":bw});
      var text = svgEl(\"text\", {x:p.x + p.w/2, y:p.y + p.h/2 + 4, \"text-anchor\":\"middle\", fill:txt, \"font-size\":\"12\", \"font-family\":\"Segoe UI,system-ui,sans-serif\"});
      text.textContent = n.label;
      g.appendChild(title); g.appendChild(shapeEl); g.appendChild(text);
      g.addEventListener(\"click\", function(){
        selectedId = (selectedId===n.id) ? null : n.id;
        rebuildGraph();
      });
      svg.appendChild(g);
    });
  }

  function escHtml(s) {
    return String(s).replace(/&/g,\"&amp;\").replace(/</g,\"&lt;\").replace(/>/g,\"&gt;\").replace(/\"/g,\"&quot;\");
  }

  function rebuildGraph() {
    var degree = computeScriptDegree(allNodesData, allEdgesData);
    var indepHidden = hideIndependent ? computeIndependentHiddenSet(allNodesData, allEdgesData) : null;
    if (hideIndependent && indepHidden) {
      var visibleScriptCount = allNodesData.filter(function(n){
        return n && n.kind === \"script\" && isVisible(n, degree, indepHidden);
      }).length;
      if (visibleScriptCount === 0) {
        hideIndependent = false;
        indepHidden = null;
        updateIndependentButton();
      }
    }
    var vn = allNodesData.filter(function(n){ return isVisible(n, degree, indepHidden); });
    var vi = new Set(vn.map(function(n){return n.id;}));
    var ve = allEdgesData.filter(function(e){return vi.has(e.from) && vi.has(e.to);});
    // First render should always fit-to-view; subsequent renders preserve pan/zoom.
    renderSvg(vn, ve, hasRenderedOnce);
    hasRenderedOnce = true;
  }

  function fitGraph() {
    var svg = document.getElementById(\"dep-svg\");
    if (!svg) return;
    var vb = svg.getAttribute(\"data-fit-view\");
    if (vb) svg.setAttribute(\"viewBox\", vb);
  }

  function getViewBox(svg) {
    var vb = (svg.getAttribute(\"viewBox\") || svg.getAttribute(\"data-fit-view\") || \"0 0 100 100\").trim();
    var parts = vb.split(/\\s+/).map(function(x){ return parseFloat(x); });
    return {x: parts[0]||0, y: parts[1]||0, w: parts[2]||100, h: parts[3]||100};
  }
  function setViewBox(svg, vb) {
    svg.setAttribute(\"viewBox\", [vb.x, vb.y, vb.w, vb.h].join(\" \"));
  }
  function enablePanZoom() {
    var svg = document.getElementById(\"dep-svg\");
    if (!svg) return;
    if (svg.dataset && svg.dataset.panzoom === \"1\") return;
    if (svg.dataset) svg.dataset.panzoom = \"1\";

    var isPanning = false;
    var start = {x:0, y:0};
    var vb0 = null;

    svg.addEventListener(\"wheel\", function(ev){
      ev.preventDefault();
      var rect = svg.getBoundingClientRect();
      if (!rect.width || !rect.height) return;
      var vb = getViewBox(svg);
      var fit = getViewBox({ getAttribute: function(){ return svg.getAttribute(\"data-fit-view\"); } });
      var mx = (ev.clientX - rect.left) / rect.width;
      var my = (ev.clientY - rect.top) / rect.height;
      var z = ev.deltaY < 0 ? 0.9 : 1.1;
      var newW = vb.w * z;
      var minW = fit.w * 0.08;
      var maxW = fit.w * 6.0;
      newW = Math.max(minW, Math.min(maxW, newW));
      var newH = vb.h * (newW / vb.w);
      var newX = vb.x + (vb.w - newW) * mx;
      var newY = vb.y + (vb.h - newH) * my;
      setViewBox(svg, {x:newX, y:newY, w:newW, h:newH});
    }, {passive:false});

    svg.addEventListener(\"pointerdown\", function(ev){
      if (ev.pointerType === \"mouse\" && ev.button !== 0) return;
      if (ev.target && ev.target.closest && ev.target.closest(\"g[data-id]\")) return;
      isPanning = true;
      start = {x: ev.clientX, y: ev.clientY};
      vb0 = getViewBox(svg);
      try { svg.setPointerCapture(ev.pointerId); } catch(e) {}
    });
    svg.addEventListener(\"pointermove\", function(ev){
      if (!isPanning || !vb0) return;
      var rect = svg.getBoundingClientRect();
      var dx = ev.clientX - start.x;
      var dy = ev.clientY - start.y;
      var sx = vb0.w / rect.width;
      var sy = vb0.h / rect.height;
      setViewBox(svg, {x: vb0.x - dx*sx, y: vb0.y - dy*sy, w: vb0.w, h: vb0.h});
    });
    function endPan(ev) {
      if (!isPanning) return;
      isPanning = false;
      vb0 = null;
      try { svg.releasePointerCapture(ev.pointerId); } catch(e) {}
    }
    svg.addEventListener(\"pointerup\", endPan);
    svg.addEventListener(\"pointercancel\", endPan);

  }

  function updateIndependentButton() {
    var btn = document.getElementById(\"indep-btn\");
    if (!btn) return;
    btn.textContent = hideIndependent ? \"Show independent scripts\" : \"Hide independent scripts\";
  }
  function updateMetaButton() {
    var btn = document.getElementById(\"meta-btn\");
    if (!btn) return;
    btn.textContent = hideMeta ? \"Show meta files\" : \"Hide meta files\";
  }
  function updateDatasetsButton() {
    var btn = document.getElementById(\"datasets-btn\");
    if (!btn) return;
    btn.textContent = showDatasets ? \"Hide datasets\" : \"Show datasets\";
  }
  window.toggleDatasets = function() {
    showDatasets = !showDatasets;
    if (!showDatasets && selectedId) {
      var sel = allNodesData.filter(function(n){ return n.id === selectedId; })[0];
      if (sel && sel.kind === \"data\") selectedId = null;
    }
    updateDatasetsButton();
    rebuildGraph();
  };
  window.toggleIndependentScripts = function() {
    hideIndependent = !hideIndependent;
    if (hideIndependent && selectedId) {
      var sel = allNodesData.filter(function(n){ return n.id === selectedId; })[0];
      var indepHidden = computeIndependentHiddenSet(allNodesData, allEdgesData);
      if (sel && indepHidden && indepHidden.has(sel.id)) selectedId = null;
    }
    updateIndependentButton();
    rebuildGraph();
  };
  window.toggleMetaFiles = function() {
    hideMeta = !hideMeta;
    if (hideMeta && selectedId) {
      var sel = allNodesData.filter(function(n){ return n.id === selectedId; })[0];
      if (sel && sel.meta && !sel.root) selectedId = null;
    }
    updateMetaButton();
    rebuildGraph();
  };
  window.showAllScripts = function() {
    hideIndependent = false;
    hideMeta = false;
    showDatasets = false;
    selectedId = null;
    updateIndependentButton();
    updateMetaButton();
    updateDatasetsButton();
    rebuildGraph();
  };
  window.resetGraphLayout = function() { fitGraph(); };

  var isFs = false;
  window.toggleFullscreen = function() {
    var wrap = document.querySelector(\".graph-wrap\");
    var el = document.getElementById(\"dep-network\");
    var btn = document.getElementById(\"fs-btn\");
    isFs = !isFs;
    if (wrap) wrap.classList.toggle(\"graph-fullscreen\", isFs);
    el.style.height = isFs ? \"calc(100vh - 140px)\" : \"600px\";
    btn.textContent = isFs ? \"Exit full screen\" : \"Full screen\";
  };

  // Render with a defensive error surface: if anything goes wrong, show it in the SVG.
  try {
    rebuildGraph();
    enablePanZoom();
    updateIndependentButton();
    updateMetaButton();
    updateDatasetsButton();
  } catch (err) {
    try { console.error(err); } catch (e) {}
    try {
      var svg = document.getElementById("dep-svg");
      if (svg) {
        while (svg.firstChild) svg.removeChild(svg.firstChild);
        svg.setAttribute("viewBox", "0 0 900 60");
        var t = svgEl("text", {x: 10, y: 24, fill: "#dc2626", "font-size": "14", "font-family": "Segoe UI,system-ui,sans-serif"});
        t.textContent = "Dependency graph render error: " + (err && err.message ? err.message : String(err));
        svg.appendChild(t);
      }
    } catch (e2) {}
  }
})();'

  js_code <- gsub("__NODES__", nodes_json, js_template, fixed = TRUE)
  js_code <- gsub("__EDGES__", edges_json, js_code, fixed = TRUE)
  js_code <- gsub("__SHOW_DATASETS__", if (isTRUE(show_datasets_default)) "true" else "false", js_code, fixed = TRUE)
  js_code <- gsub("__HIDE_META__", if (isTRUE(!show_meta_default)) "true" else "false", js_code, fixed = TRUE)
  js_code <- gsub("__HIDE_INDEPENDENT__", if (isTRUE(!show_indep_default)) "true" else "false", js_code, fixed = TRUE)
  js_code <- trimws(js_code)
  if (!startsWith(js_code, "(function(){") || !endsWith(js_code, "})();")) {
    stop("Internal error: generated dependency graph JS did not validate (expected to start with '(function(){' and end with '})();').")
  }

  datasets_btn_label <- if (isTRUE(show_datasets_default)) "Hide datasets" else "Show datasets"
  meta_btn_label <- if (isTRUE(show_meta_default)) "Hide meta files" else "Show meta files"
  indep_btn_label <- if (isTRUE(!show_indep_default)) "Show independent scripts" else "Hide independent scripts"
  paste0(
    '<div class="graph-wrap">',
    '<div class="graph-main">',
    '<div class="graph-toolbar">',
      '<button id="fs-btn" class="btn-sm" onclick="toggleFullscreen()">Full screen</button>',
      '<button id="datasets-btn" class="btn-sm" onclick="toggleDatasets()">', datasets_btn_label, '</button>',
      '<button id="meta-btn" class="btn-sm" onclick="toggleMetaFiles()">', meta_btn_label, '</button>',
      '<button id="indep-btn" class="btn-sm" onclick="toggleIndependentScripts()">', indep_btn_label, '</button>',
      '<button class="btn-sm" onclick="showAllScripts()">Reset view</button>',
      '</div>',
    '<div id="dep-network" style="width:100%;height:600px;border:1px solid #e2e8f0;border-radius:6px;background:#fafafa;">',
    '<svg id="dep-svg" xmlns="http://www.w3.org/2000/svg" preserveAspectRatio="xMidYMid meet" style="width:100%;height:100%;display:block"></svg>',
    '</div>',
    '</div>',
    '</div>',
    '<div class="graph-legend">',
    '<span class="leg-item"><span class="leg-dot" style="background:#dbeafe;border:2px solid #3b82f6"></span>Script</span>',
    '<span class="leg-item"><span class="leg-dot" style="background:#fef3c7;border:2px solid #d97706"></span>Setup</span>',
    '<span class="leg-item"><span class="leg-dot" style="background:#dcfce7;border:2px solid #16a34a"></span>Dataset</span>',
    '<span class="leg-item"><span class="leg-line" style="border-top-color:#16a34a;border-top-style:dashed"></span>Reads</span>',
    '<span class="leg-item"><span class="leg-line" style="border-top-color:#ea580c;border-top-style:dashed"></span>Writes</span>',
    '<span class="leg-item"><span class="leg-line" style="border-top-color:#475569"></span>Data flow</span>',
    '<span class="leg-item"><span class="leg-line" style="border-top-color:#94a3b8"></span>Sources</span>',
    '</div>',
    '<p class="viz-desc">Click a node to highlight its connected edges. Drag to pan, scroll to zoom. Hover edges for provenance.</p>',
    '<script>', js_code, '</script>'
  )
}

build_r_file_index <- function(parsed, graph, setup_file, project_path = NULL) {
  r_files <- names(parsed$files)
  ord <- try_topological_order(graph)
  if (length(ord) == 0) ord <- r_files

  # Build folder tree (include parent folders so the index is grouped "parent first").
  get_folder <- function(x) { d <- gsub("\\\\", "/", dirname(x)); if (!nzchar(d) || d == ".") "root" else d }
  file_folders <- unique(vapply(ord, get_folder, character(1)))
  folders <- unique(c("root", file_folders))
  for (f in file_folders[file_folders != "root"]) {
    parts <- strsplit(f, "/", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    if (length(parts) <= 1) next
    for (i in 1:(length(parts) - 1)) folders <- c(folders, paste(parts[1:i], collapse = "/"))
  }
  folders <- unique(folders)

  priority <- c("root", "code", "prepare", "build", "final", "raw", "analysis", "do", "output", "data")
  sort_children <- function(paths) {
    if (length(paths) <= 1) return(paths)
    base <- ifelse(paths == "root", "root", tolower(basename(paths)))
    paths[order(match(base, priority, nomatch = 999L), base, paths)]
  }
  parent_of <- function(f) {
    if (f == "root") return(NA_character_)
    if (!grepl("/", f, fixed = TRUE)) return("root")
    sub("/[^/]+$", "", f)
  }
  parents <- vapply(folders, parent_of, character(1))
  kids_map <- split(folders[!is.na(parents)], parents[!is.na(parents)])

  data_refs <- build_data_reads_display(parsed)
  out <- '<h2 id="files">File Index</h2><p class="viz-desc">Organized by folder (parent-first), with files in execution order.</p>'
  if (nzchar(data_refs$legend)) out <- paste0(out, '<div class="data-legend">', data_refs$legend, '</div>')

  row_html <- function(rel, folder) {
    f    <- parsed$files[[rel]]
    role_tags <- character(0)
    if (!is.null(setup_file) && identical(rel, setup_file)) role_tags <- c(role_tags, "setup")
    if (!is.null(parsed$master_path) && nzchar(parsed$master_path) && identical(rel, parsed$master_path)) role_tags <- c(role_tags, "master")
    if (!is.null(f$file_type) && f$file_type == "stata") role_tags <- c(role_tags, "stata")
    role_html <- if (length(role_tags) > 0) {
      paste(vapply(role_tags, function(r) sprintf('<span class="role-tag role-%s">%s</span>', r, r), character(1)), collapse = " ")
    } else ""
    reads_html <- data_refs$by_file[[rel]] %|nz|% "<span class='dim'>(none)</span>"
    writes <- f$data_writes %||% character(0)
    writes_html <- if (length(writes) == 0) {
      "<span class='dim'>(none)</span>"
    } else {
      wn <- path_norm_for_group(writes)
      writes <- writes[!duplicated(wn)]
      parts <- vapply(seq_along(writes), function(j) {
        p <- writes[j]
        disp <- data_path_display_label(p)
        short <- if (grepl("/", disp)) basename(disp) else disp
        sprintf('<span class="data-ref" title="%s">%s</span>', html_esc(disp), html_esc(short))
      }, character(1))
      paste(parts, collapse = " ")
    }
    src_list <- unique(f$sources %||% character(0))
    src_html  <- if (length(src_list) == 0) "<span class='dim'>(none)</span>" else
      paste(vapply(src_list, function(s) sprintf('<span class="src-tag" title="%s">%s</span>', html_esc(s), html_esc(basename(s))), character(1)), collapse = " ")
    sprintf('<tr data-folder="%s" title="%s"><td title="%s">%s</td><td>%s</td><td class="src-cell">%s</td><td class="data-reads">%s</td><td class="data-reads">%s</td></tr>',
      html_esc(folder), html_esc(rel), html_esc(rel), html_esc(basename(rel)), role_html, src_html, reads_html, writes_html)
  }

  render_folder <- function(folder, depth = 0L) {
    kids <- sort_children(kids_map[[folder]] %||% character(0))
    files <- ord[vapply(ord, function(x) identical(get_folder(x), folder), logical(1))]
    display <- if (folder == "root") "(root)" else basename(folder)
    title <- if (folder == "root") "(root)" else folder
    n_total <- sum(vapply(ord, function(x) {
      f <- get_folder(x)
      if (folder == "root") identical(f, "root")
      else identical(f, folder) || startsWith(f, paste0(folder, "/"))
    }, logical(1)))
    open_attr <- if (depth <= 1L) " open" else ""

    block <- paste0(
      '<details class="fi-folder-block file-folder" data-folder="', html_esc(folder), '"', open_attr, '>',
      '<summary><strong>', html_esc(display), '</strong> <span class="count">', n_total, ' file', if (n_total != 1) 's' else '', '</span></summary>'
    )

    if (length(files) > 0) {
      block <- paste0(block, '<table><tr><th>File</th><th>Role</th><th>Sources</th><th>Data reads</th><th>Data writes</th></tr>',
        paste(vapply(files, function(rel) row_html(rel, folder), character(1)), collapse = ""),
        '</table>')
    }
    if (length(kids) > 0) {
      block <- paste0(block, '<div class="fi-children">',
        paste(vapply(kids, function(k) render_folder(k, depth + 1L), character(1)), collapse = ""),
        '</div>')
    }
    paste0(block, '</details>')
  }

  top <- sort_children(kids_map[["root"]] %||% character(0))
  out <- paste0(out, paste(vapply(top, function(t) render_folder(t, 0L), character(1)), collapse = ""))
  out
}

order_folder_names <- function(folders) {
  priority <- c("root","prepare","build","final","empirical","do","analysis","output")
  fb <- ifelse(folders == "root", "root", tolower(basename(folders)))
  folders[order(match(fb, priority, nomatch = 999L), folders)]
}

build_data_reads_display <- function(parsed) {
  # Deduplicate within each file by normalized path
  all_reads_deduped <- unlist(lapply(parsed$files, function(f) {
    r <- f$data_reads %||% character(0); if (length(r) == 0) return(character(0))
    unique(path_norm_for_group(r))
  }))
  if (length(all_reads_deduped) == 0) return(list(legend = "", by_file = list()))
  counts <- table(all_reads_deduped)
  common_norm <- names(counts)[counts >= 3]

  by_file <- list()
  for (rel in names(parsed$files)) {
    reads <- parsed$files[[rel]]$data_reads %||% character(0)
    if (length(reads) == 0) { by_file[[rel]] <- "<span class='dim'>(none)</span>"; next }
    # Deduplicate by normalized path, keep first representative raw path
    reads_norm <- path_norm_for_group(reads)
    unique_idx  <- !duplicated(reads_norm)
    reads      <- reads[unique_idx]
    reads_norm <- reads_norm[unique_idx]
    parts <- vapply(seq_along(reads), function(j) {
      p <- reads[j]; pn <- reads_norm[j]
      disp <- data_path_display_label(p)
      short <- if (grepl("/", disp)) basename(disp) else disp
      cls   <- if (pn %in% common_norm) "data-ref common" else "data-ref"
      sprintf('<span class="%s" title="%s">%s</span>', cls, html_esc(disp), html_esc(short))
    }, character(1))
    by_file[[rel]] <- paste(parts, collapse = " ")
  }
  legend_parts <- if (length(common_norm) > 0) {
    vapply(common_norm, function(p) {
      repr <- reads[path_norm_for_group(reads) == p]
      # Use a representative display
      disp <- data_path_display_label(if (length(repr) > 0) repr[1] else p)
      sprintf('<span class="data-ref common" title="%s">%s</span>', html_esc(disp), html_esc(basename(disp)))
    }, character(1))
  } else character(0)
  legend <- if (length(legend_parts) > 0)
    paste0('<strong>Frequently used:</strong> ', paste(unique(legend_parts), collapse = " "))
  else ""
  list(legend = legend, by_file = by_file)
}

# ---- Dataset index ----
build_dataset_index <- function(data_info, parsed) {
  idx <- data_info$dataset_index
  if (is.null(idx)) return("")
  if (nrow(idx) == 0) {
    return('<h2 id="datasets">Dataset Index</h2><p class="viz-desc">No data files found in the provided data folder.</p>')
  }

  # Build directory tree (parent-first)
  dir_key_of <- function(p) {
    d <- dirname(normalize_path_canonical(p))
    if (!nzchar(d) || d == ".") "root" else tolower(d)
  }
  dir_disp_of <- function(p) {
    d <- dirname(normalize_path_canonical(p))
    if (!nzchar(d) || d == ".") "root" else d
  }
  dir_disp_env <- new.env(parent = emptyenv())
  set_dir_disp_if_better <- function(k, cand) {
    if (!nzchar(k) || k == "root") return(invisible(NULL))
    if (!nzchar(cand) || cand == "root") return(invisible(NULL))
    cur <- get0(k, envir = dir_disp_env, inherits = FALSE, ifnotfound = "")
    if (!nzchar(cur)) {
      assign(k, cand, envir = dir_disp_env)
      return(invisible(NULL))
    }
    # Prefer one that is not all-uppercase (reduces duplicate trees like UGANDA vs Uganda).
    cur_all_upper <- identical(cur, toupper(cur))
    cand_all_upper <- identical(cand, toupper(cand))
    if (cur_all_upper && !cand_all_upper) assign(k, cand, envir = dir_disp_env)
    invisible(NULL)
  }
  idx_dir_key <- vapply(idx$path, dir_key_of, character(1))
  for (j in seq_along(idx$path)) set_dir_disp_if_better(idx_dir_key[j], dir_disp_of(idx$path[j]))

  dirs_exact <- unique(idx_dir_key)
  dirs <- unique(c("root", dirs_exact))
  for (d in dirs_exact[dirs_exact != "root"]) {
    parts <- strsplit(d, "/", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    if (length(parts) <= 1) next
    for (i in 1:(length(parts) - 1)) dirs <- c(dirs, paste(parts[1:i], collapse = "/"))
  }
  dirs <- unique(dirs)

  priority <- c("root", "data", "raw", "build", "final", "output")
  sort_children <- function(paths) {
    if (length(paths) <= 1) return(paths)
    base <- ifelse(paths == "root", "root", tolower(basename(paths)))
    paths[order(match(base, priority, nomatch = 999L), base, paths)]
  }
  parent_of <- function(f) {
    if (f == "root") return(NA_character_)
    if (!grepl("/", f, fixed = TRUE)) return("root")
    sub("/[^/]+$", "", f)
  }
  parents <- vapply(dirs, parent_of, character(1))
  kids_map <- split(dirs[!is.na(parents)], parents[!is.na(parents)])

  fmt_scripts <- function(x) {
    x <- x %||% ""
    if (length(x) == 0 || is.na(x[1])) x <- ""
    x <- trimws(x[1])
    if (!nzchar(x)) return("-")
    parts <- trimws(strsplit(x, "\\s*,\\s*")[[1]])
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0) return("-")
    disp <- paste(unique(basename(parts)), collapse = ", ")
    sprintf('<span class="refs" title="%s">%s</span>', html_esc(paste(parts, collapse = ", ")), html_esc(disp))
  }

  out <- '<h2 id="datasets">Dataset Index</h2><p class="viz-desc">Datasets found in the provided data folder (existence scan only), grouped by directory. Hover filename for full path.</p>'

  render_dir <- function(d, depth = 0L) {
    kids <- sort_children(kids_map[[d]] %||% character(0))
    rows <- idx[idx_dir_key == d, , drop = FALSE]
    n_direct <- nrow(rows)
    n_total <- if (d == "root") sum(idx_dir_key == "root") else sum(idx_dir_key == d | startsWith(idx_dir_key, paste0(d, "/")))

    disp_path <- if (d == "root") "(root)" else get0(d, envir = dir_disp_env, inherits = FALSE, ifnotfound = d)
    disp <- if (d == "root") "(root)" else basename(disp_path)
    title <- if (d == "root") "(root)" else disp_path
    open_attr <- if (depth <= 1L) " open" else ""
    outd <- paste0('<details class="dataset-folder"', open_attr, '><summary><strong>',
      html_esc(disp), '</strong> <span class="count">', n_total, ' file', if (n_total != 1) 's' else '', '</span></summary>')

    if (n_direct > 0) {
      rows <- rows[order(tolower(basename(rows$path)), rows$path), , drop = FALSE]
      outd <- paste0(outd, '<table><tr><th>File</th><th>Created by</th><th>Read by</th></tr>')
      for (i in seq_len(n_direct)) {
        row <- rows[i, ]
        outd <- paste0(outd, sprintf(
          '<tr data-folder="%s"><td title="%s">%s</td><td>%s</td><td>%s</td></tr>',
          html_esc(d),
          html_esc(row$path), html_esc(basename(row$path)),
          fmt_scripts(row$writers), fmt_scripts(row$readers)))
      }
      outd <- paste0(outd, '</table>')
    }

    if (length(kids) > 0) {
      outd <- paste0(outd, paste(vapply(kids, function(k) render_dir(k, depth + 1L), character(1)), collapse = ""))
    }
    paste0(outd, '</details>')
  }

  top <- sort_children(kids_map[["root"]] %||% character(0))
  out <- paste0(out, paste(vapply(top, function(t) render_dir(t, 0L), character(1)), collapse = ""))
  out
}

# ---- Issues ----
format_issue_message <- function(iss) {
  if (iss$type == "dataset_not_used")
    sprintf('<span class="iss-ds">%s</span> created by <span class="iss-sc">%s</span> &mdash; never read', html_esc(iss$to), html_esc(basename(iss$from)))
  else if (iss$type == "file_not_used")
    sprintf('<span class="iss-sc">%s</span> &mdash; not in execution chain', html_esc(iss$from))
  else html_esc(iss$message)
}

build_issues_section <- function(issues) {
  if (nrow(issues) == 0) return('<h2 id="issues">Issues</h2><p class="ok">No issues detected.</p>')
  n_err  <- sum(issues$severity == "error")
  n_info <- sum(issues$severity == "info")
  type_labels <- c(
    wrong_dependency    = "Wrong dependencies",
    missing_source      = "Missing source files",
    circular_dependency = "Circular dependencies",
    file_not_used       = "Files not in execution chain",
    dataset_not_used    = "Datasets created but never read"
  )
  by_type    <- split(seq_len(nrow(issues)), issues$type)
  type_order <- c("wrong_dependency","missing_source","circular_dependency","file_not_used","dataset_not_used")
  out <- '<h2 id="issues">Issues</h2>'
  if (n_err  > 0) out <- paste0(out, sprintf('<p class="iss-sum err">%d error%s &mdash; review before running</p>', n_err,  if (n_err  != 1) "s" else ""))
  if (n_info > 0) out <- paste0(out, sprintf('<p class="iss-sum info">%d info note%s</p>',                   n_info, if (n_info != 1) "s" else ""))
  for (t in c(intersect(type_order, names(by_type)), setdiff(names(by_type), type_order))) {
    idx <- by_type[[t]]; n <- length(idx)
    label <- if (!is.na(type_labels[t]) && nzchar(type_labels[t])) type_labels[t] else t
    is_err <- issues$severity[idx[1]] == "error"
    cls    <- if (is_err) "iss-group err" else "iss-group info"
    out <- paste0(out, sprintf('<details class="%s"><summary>%s (%d)</summary><ul>', cls, html_esc(label), n))
    for (i in idx) out <- paste0(out, sprintf('<li>%s</li>', format_issue_message(issues[i, ])))
    out <- paste0(out, '</ul></details>')
  }
  out
}

# ---- Folder filter panel ----
build_folder_filter_panel <- function(all_folders, r_files) {
  if (length(all_folders) == 0) return("")
  get_folder <- function(x) { d <- gsub("\\\\", "/", dirname(x)); if (!nzchar(d) || d == ".") "root" else d }
  folder_counts_exact <- table(vapply(r_files, get_folder, character(1)))

  folders <- unique(all_folders)
  if (!("root" %in% folders)) folders <- c("root", folders)

  # Aggregate counts for parent folders (include all descendants)
  agg_count <- function(folder) {
    if (folder == "root") return(as.integer(folder_counts_exact["root"] %||% 0L))
    nms <- names(folder_counts_exact)
    sum(as.integer(folder_counts_exact[nms == folder | startsWith(nms, paste0(folder, "/"))] %||% 0L))
  }

  default_off <- folders[grepl("old|archive|archives|backup", basename(folders), ignore.case = TRUE)]
  priority <- c("root", "code", "prepare", "build", "final", "raw", "analysis", "do", "output", "data")
  sort_children <- function(paths) {
    if (length(paths) <= 1) return(paths)
    base <- ifelse(paths == "root", "root", tolower(basename(paths)))
    paths[order(match(base, priority, nomatch = 999L), base, paths)]
  }

  parent_of <- function(f) {
    if (f == "root") return(NA_character_)
    if (!grepl("/", f, fixed = TRUE)) return("root")
    sub("/[^/]+$", "", f)
  }
  parents <- vapply(folders, parent_of, character(1))
  kids_map <- split(folders[!is.na(parents)], parents[!is.na(parents)])

  render_node <- function(folder, depth = 0L) {
    kids <- sort_children(kids_map[[folder]] %||% character(0))
    checked <- if (folder %in% default_off) "" else " checked"
    disp <- if (folder == "root") "(root)" else basename(folder)
    full_disp <- if (folder == "root") "(root)" else folder
    n <- agg_count(folder)

    label <- sprintf(
      '<label class="folder-tree-label" title="%s"><input type="checkbox" class="folder-cb" value="%s"%s><span class="fn">%s</span><span class="fc">%d</span></label>',
      html_esc(full_disp), html_esc(folder), checked, html_esc(disp), n
    )
    if (length(kids) == 0) {
      return(paste0('<div class="folder-tree-leaf">', label, '</div>'))
    }
    inner <- paste(vapply(kids, function(k) render_node(k, depth + 1L), character(1)), collapse = "")
    open_attr <- if (depth <= 1L) " open" else ""
    paste0(
      '<details class="folder-tree"', open_attr, '>',
      '<summary>', label, '</summary>',
      '<div class="folder-tree-children">', inner, '</div>',
      '</details>'
    )
  }

  top <- sort_children(kids_map[["root"]] %||% character(0))
  cb_html <- paste(vapply(top, function(t) render_node(t, 0L), character(1)), collapse = "")

  paste0(
    '<div class="filter-panel" id="filter-panel">',
    '<span class="filter-title">Folder filter</span>',
    '<div class="filter-cbs folder-tree-wrap">', cb_html, '</div>',
    '<button class="btn-sm" onclick="showAllFolders()">Show all</button>',
    '</div>'
  )
}

# ---- Full HTML assembly ----
build_full_html <- function(parsed, data_info, graph, issues, uu, exec_display, viz_html,
                             project_path = NULL, project_name = NULL,
                             setup_file = NULL,
                             roots = character(0)) {
  if (is.null(project_name) || !nzchar(project_name)) project_name <- "Project"
  n_r     <- sum(graph$nodes$type == "r_file")
  n_d     <- sum(graph$nodes$type == "data")
  n_edges <- nrow(graph$edges)
  n_err   <- sum(issues$severity == "error")
  n_note  <- sum(issues$severity == "info")

  r_index    <- build_r_file_index(parsed, graph, setup_file, project_path)
  show_dataset_index <- !is.null(data_info$data_scan)
  data_index <- if (isTRUE(show_dataset_index)) build_dataset_index(data_info, parsed) else ""
  issues_html <- build_issues_section(issues)

  # Data existence note (the tool never opens/reads data files)
  data_path_note <- ""
  if (isTRUE(show_dataset_index)) {
    n_scanned <- length(data_info$data_scan$existing_paths_norm)
    if (n_scanned > 0) {
      data_path_note <- sprintf('<span class="stat-note">%d data files listed (existence only)</span>', n_scanned)
    } else {
      data_path_note <- '<span class="stat-note dim">No data files found in the provided data folder</span>'
    }
  } else {
    data_path_note <- '<span class="stat-note dim">Dataset index not generated (set R_DEP_DATA_PATH to your data folder)</span>'
  }

  roots_note <- ""
  if (length(roots) > 0) {
    roots_note <- sprintf('<span class="stat-note">Roots: %s</span>', html_esc(paste(roots, collapse = ", ")))
  }

  overview <- sprintf(
    paste0(
      '<div class="overview-grid">',
      '<div class="stat"><span class="num">%d</span><span class="lbl">Scripts</span></div>',
      '<div class="stat"><span class="num">%d</span><span class="lbl">Datasets ref.</span></div>',
      '<div class="stat %s"><span class="num">%d</span><span class="lbl">Errors</span></div>',
      '<div class="stat"><span class="num">%d</span><span class="lbl">Notes</span></div>',
      '</div>%s%s'
    ),
    n_r, n_d, if (n_err > 0) "err" else "", n_err, n_note, data_path_note, roots_note)

  css <- '
*{box-sizing:border-box;margin:0;padding:0}
body{font-family:"Segoe UI",system-ui,-apple-system,sans-serif;background:#f1f5f9;color:#1e293b;font-size:14px;line-height:1.5}
nav{position:sticky;top:0;z-index:200;background:#1e293b;padding:10px 28px;display:flex;gap:4px;align-items:center;flex-wrap:wrap}
nav .title{color:#f8fafc;font-weight:600;font-size:15px;margin-right:16px;white-space:nowrap}
nav a{color:#94a3b8;text-decoration:none;font-size:13px;padding:5px 10px;border-radius:5px;transition:all .15s}
nav a:hover{color:#f8fafc;background:rgba(255,255,255,.1)}
main{max-width:1280px;margin:0 auto;padding:20px 24px}
.card{background:#fff;border-radius:10px;box-shadow:0 1px 4px rgba(0,0,0,.07);padding:24px 28px;margin-bottom:18px}
h2{font-size:1.05rem;font-weight:600;color:#0f172a;margin-bottom:16px;padding-bottom:10px;border-bottom:2px solid #e2e8f0}
h3{font-size:.9rem;font-weight:600;color:#475569;margin:18px 0 10px}
table{border-collapse:collapse;width:100%;font-size:13px}
th{background:#334155;color:#f8fafc;font-weight:500;font-size:11px;text-transform:uppercase;letter-spacing:.04em;padding:9px 12px;text-align:left}
td{border-bottom:1px solid #f1f5f9;padding:8px 12px;vertical-align:top}
tr:hover td{background:#f8fafc}
.overview-grid{display:flex;gap:16px;flex-wrap:wrap;margin-bottom:12px}
.stat{padding:14px 22px;background:#f8fafc;border-radius:8px;text-align:center;border:1px solid #e2e8f0;min-width:90px}
.stat .num{display:block;font-size:1.8rem;font-weight:700;color:#0f172a}
.stat .lbl{font-size:11px;color:#64748b;text-transform:uppercase;letter-spacing:.04em}
.stat.err .num{color:#dc2626}
.stat-note{display:block;font-size:12px;color:#64748b;margin-top:10px}
.stat-note.dim{color:#94a3b8}
.btn-sm{padding:5px 12px;background:#f1f5f9;border:1px solid #e2e8f0;border-radius:5px;font-size:12px;cursor:pointer;color:#475569;transition:all .15s}
.btn-sm:hover{background:#e2e8f0;color:#0f172a}
.graph-wrap{display:flex;gap:12px;align-items:flex-start}
.graph-wrap.graph-fullscreen{position:fixed;inset:0;z-index:999;background:#f1f5f9;padding:18px;overflow:hidden}
.graph-wrap.graph-fullscreen .graph-main{height:100%}
.graph-wrap.graph-fullscreen #dep-network{height:calc(100vh - 140px) !important}
.graph-main{flex:1;min-width:0}
.graph-toolbar{display:flex;gap:8px;margin-bottom:8px;flex-wrap:wrap}
#dep-network{border-radius:6px}
#dep-svg{touch-action:none;cursor:grab}
.graph-legend{display:flex;flex-wrap:wrap;gap:12px;margin-top:10px;padding:8px 0}
.leg-item{display:flex;align-items:center;gap:6px;font-size:12px;color:#64748b}
.leg-dot{display:inline-block;width:14px;height:14px;border-radius:3px}
.leg-line{display:inline-block;width:20px;height:0;border-top:3px solid #94a3b8}
.exec-order{margin-top:4px}
.exec-step-group{margin:10px 0;border-radius:6px;padding:12px 14px 8px}
.exec-step-header{font-size:13px;font-weight:600;margin-bottom:8px}
.exec-par-badge{font-size:11px;font-weight:400;color:#64748b;margin-left:6px}
.exec-file-list{list-style:none;padding:0;display:flex;flex-wrap:wrap;gap:6px}
.exec-file-list li{display:flex;align-items:center;gap:5px;padding:4px 10px;background:#fff;border-radius:5px;border:1px solid #e2e8f0;font-size:12px;cursor:default}
.exec-num{background:#e2e8f0;color:#64748b;border-radius:10px;padding:0 5px;font-size:10px;font-weight:600}
.exec-unused-details{margin-top:10px;border:1px solid #e2e8f0;border-radius:6px}
.exec-unused-details summary{padding:8px 12px;cursor:pointer;font-size:13px;color:#64748b}
.exec-unused-list{margin:6px 0 8px 20px;font-size:12px;color:#94a3b8}
.fi-folder-block{margin-bottom:20px}
.file-folder{border:1px solid #e2e8f0;border-radius:8px;overflow:hidden}
.file-folder summary{padding:12px 16px;cursor:pointer;font-size:13px;background:#f8fafc;display:flex;justify-content:space-between;align-items:center}
.file-folder[open] summary{border-bottom:1px solid #e2e8f0}
.file-folder table{margin:0}
.file-folder .file-folder{margin-left:18px}
.role-tag{display:inline-block;padding:1px 7px;border-radius:10px;font-size:11px;font-weight:600}
.role-setup{background:#fef3c7;color:#92400e}
.role-master{background:#dbeafe;color:#1e40af}
.role-stata{background:#e0e7ff;color:#3730a3}
.src-cell{font-size:12px}
.src-tag{display:inline-block;padding:2px 6px;margin:1px;background:#f1f5f9;border-radius:4px;font-size:11px;color:#334155;cursor:default}
.data-reads{font-size:12px}
.data-ref{display:inline-block;padding:2px 6px;margin:1px 2px 1px 0;background:#f1f5f9;border-radius:4px;font-size:11px;cursor:help}
.data-ref.common{background:#dbeafe;color:#1e40af}
.data-legend{font-size:12px;color:#475569;margin-bottom:14px;padding:10px 14px;background:#eff6ff;border-radius:6px;border-left:3px solid #3b82f6}
.dataset-folder{margin:8px 0;border:1px solid #e2e8f0;border-radius:8px}
.dataset-folder summary{padding:12px 16px;cursor:pointer;font-size:13px;background:#f8fafc;border-radius:8px 8px 0 0;display:flex;justify-content:space-between;align-items:center}
.dataset-folder[open] summary{border-radius:8px 8px 0 0;border-bottom:1px solid #e2e8f0}
.dataset-folder table{margin:0}
.dataset-folder .dataset-folder{margin-left:18px}
.fi-children{margin-left:18px;padding-left:8px;border-left:1px solid #e2e8f0}
.dataset-folder td,.dataset-folder th{padding:7px 12px}
.count{color:#64748b;font-weight:400;font-size:12px}
.exists.yes{color:#059669;font-weight:500}
.exists.missing{color:#dc2626;font-weight:500}
.exists.unknown{color:#64748b;font-weight:500}
.refs{font-size:12px;color:#64748b}
.creator{font-size:12px}
.iss-group{margin:8px 0;border-radius:6px;overflow:hidden;border:1px solid #e2e8f0}
.iss-group summary{cursor:pointer;padding:10px 14px;font-size:13px;font-weight:500}
.iss-group.err summary{background:#fef2f2;border-left:3px solid #dc2626;color:#7f1d1d}
.iss-group.info summary{background:#f0f9ff;border-left:3px solid #0ea5e9;color:#0c4a6e}
.iss-group ul{margin:8px 0 8px 20px;font-size:13px;padding-bottom:8px}
.iss-sum{font-size:13px;margin-bottom:10px;font-weight:500}
.iss-sum.err{color:#dc2626}
.iss-sum.info{color:#0284c7}
.iss-ds{color:#7c3aed;font-weight:500}
.iss-sc{color:#0d9488;font-weight:500}
.ok{color:#059669;font-weight:500}
.viz-desc{font-size:12px;color:#64748b;margin:6px 0 14px;line-height:1.5}
.dim{color:#94a3b8}
details summary::-webkit-details-marker{color:#94a3b8}
'

  datasets_nav <- if (isTRUE(show_dataset_index)) '  <a href="#datasets">Datasets</a>\n' else ""
  datasets_card <- if (isTRUE(show_dataset_index)) {
    paste0(
      '  <div class="card" id="datasets">\n',
      '    ', data_index, '\n',
      '  </div>\n'
    )
  } else ""

  # Debug section: edge counts by type and full edge table with provenance
  debug_html <- build_debug_section(graph)

  sprintf('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>%s &mdash; Dependency Analysis</title>
<style>%s</style>
</head>
<body>
<nav>
  <span class="title">%s</span>
  <a href="#overview-card">Overview</a>
  <a href="#graph">Graph</a>
  <a href="#execution">Execution</a>
  <a href="#files">Files</a>
%s
  <a href="#issues">Issues</a>
  <a href="#debug">Debug</a>
</nav>
<main>
  <div class="card" id="overview-card">
    <h2>Overview &mdash; %s</h2>
    %s
  </div>
  <div class="card" id="graph">
    <h2>Dependency Graph</h2>
<p class="viz-desc">Interactive: scroll to zoom, drag to pan. Arrows = data/execution flow (provider &rarr; consumer). Click a node to see edge provenance.</p>
    %s
  </div>
  <div class="card" id="execution">
    <h2>Execution Order</h2>
    %s
  </div>
  <div class="card" id="files">
    %s
  </div>
%s
  <div class="card" id="issues">
    %s
  </div>
  <div class="card" id="debug">
    %s
  </div>
</main>
</body>
</html>',
    html_esc(project_name), css,
    html_esc(project_name),
    datasets_nav,
    html_esc(project_name), overview,
    viz_html,
    exec_display,
    r_index,
    datasets_card,
    issues_html,
    debug_html
  )
}

build_debug_section <- function(graph) {
  edges <- graph$edges
  # Count by type
  type_counts <- if (nrow(edges) > 0) table(edges$type) else table(character(0))
  count_rows <- vapply(sort(names(type_counts)), function(t) {
    sprintf('<tr><td>%s</td><td>%d</td></tr>', html_esc(t), type_counts[[t]])
  }, character(1))
  count_html <- paste0(
    '<table style="width:auto;margin-bottom:14px"><tr><th>Edge type</th><th>Count</th></tr>',
    paste(count_rows, collapse = ""),
    '</table>'
  )

  # Full edge table (capped at 500 rows to avoid huge HTML)
  has_reason <- "reason" %in% colnames(edges)
  max_rows <- min(nrow(edges), 500L)
  edge_rows <- character(0)
  if (nrow(edges) > 0) {
    for (i in seq_len(max_rows)) {
      e <- edges[i, ]
      reason_str <- if (has_reason && !is.na(e$reason) && nzchar(e$reason)) e$reason else ""
      edge_rows <- c(edge_rows, sprintf(
        '<tr><td title="%s">%s</td><td title="%s">%s</td><td>%s</td><td style="color:#64748b;font-size:11px">%s</td></tr>',
        html_esc(e$from), html_esc(basename(e$from)),
        html_esc(e$to), html_esc(basename(e$to)),
        html_esc(e$type),
        html_esc(reason_str)
      ))
    }
  }
  trunc_note <- if (nrow(edges) > 500L) sprintf('<p style="color:#64748b;font-size:12px">Showing first 500 of %d edges.</p>', nrow(edges)) else ""
  edge_table_html <- paste0(
    '<table><tr><th>From</th><th>To</th><th>Type</th><th>Reason / Provenance</th></tr>',
    paste(edge_rows, collapse = ""),
    '</table>', trunc_note
  )

  paste0(
    '<details><summary><h2 style="display:inline;font-size:1.05rem">Debug &mdash; Edges &amp; Provenance</h2></summary>',
    '<p class="viz-desc">All graph edges with type and provenance. Use this to understand why a script&rarr;script edge exists.</p>',
    count_html,
    edge_table_html,
    '</details>'
  )
}

html_esc <- function(x) {
  if (length(x) == 0 || (length(x) == 1 && !nzchar(as.character(x)))) return("")
  x <- as.character(x)
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub('"',  "&quot;", x, fixed = TRUE)
  x
}

html_esc_js <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub('"',    '\\\\"',    x, fixed = TRUE)
  x <- gsub("\n",   "\\\\n",    x, fixed = TRUE)
  x <- gsub("\r",   "",         x, fixed = TRUE)
  x
}


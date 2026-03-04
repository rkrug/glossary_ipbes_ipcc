# Shiny module: directed term hierarchy graph
# =============================================================================

#' UI for the hierarchy graph module
#'
#' @param id Shiny module namespace id.
#' @return UI tags.
#' @keywords internal
mod_graph_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::sliderInput(
          inputId = ns("min_score"),
          label = "Minimum subsumption score",
          min = 0.45,
          max = 0.95,
          value = 0.65,
          step = 0.01
        )
      ),
      shiny::column(
        width = 3,
        shiny::checkboxInput(
          inputId = ns("best_parent_only"),
          label = "Keep only best parent per child",
          value = TRUE
        )
      ),
      shiny::column(
        width = 3,
        shiny::numericInput(
          inputId = ns("max_edges"),
          label = "Max edges to display",
          value = 200,
          min = 20,
          max = 1000,
          step = 10
        )
      ),
      shiny::column(
        width = 2,
        shiny::div(
          style = "margin-top:25px;",
          shiny::actionButton(
            inputId = ns("zoom_tree"),
            label = "Focus Selected Tree",
            icon = shiny::icon("bullseye"),
            class = "btn btn-primary"
          ),
          htmltools::div(
            class = "graph-focus-help",
            "Click a node, then use this button."
          )
        )
      )
    ),
    htmltools::div(
      class = "graph-summary",
      shiny::textOutput(ns("graph_status"))
    ),
    shiny::uiOutput(ns("graph_widget")),
    htmltools::h4("Top Directed Edges"),
    reactable::reactableOutput(ns("hierarchy_table"))
  )
}

# =============================================================================

#' Server for the hierarchy graph module
#'
#' @param id Shiny module namespace id.
#' @param merged_rv ReactiveVal with merged glossary data.
#' @param cache_dir Path to app cache directory.
#' @param highlight_terms_rv Optional [shiny::reactiveVal()] used to push graph
#'   selection highlights to other modules.
#' @keywords internal
mod_graph_server <- function(id, merged_rv, cache_dir, highlight_terms_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    full_edges_rv <- shiny::reactiveVal(NULL)
    cache_meta_rv <- shiny::reactiveVal(NULL)
    def_lookup_rv <- shiny::reactiveVal(character(0))
    selected_node_rv <- shiny::reactiveVal(NULL)

    shiny::observeEvent(merged_rv(), {
      data <- merged_rv()
      if (is.null(data) || nrow(data) == 0) {
        cache_meta_rv(NULL)
        full_edges_rv(.empty_hierarchy_edges())
        def_lookup_rv(character(0))
        selected_node_rv(NULL)
        if (!is.null(highlight_terms_rv)) highlight_terms_rv(character(0))
        return(invisible(NULL))
      }

      def_lookup_rv(.build_definition_lookup(data))

      meta <- .hierarchy_cache_meta(data)
      cache_meta_rv(meta)
      cached <- .load_hierarchy_cache(cache_dir, meta)
      if (!is.null(cached)) {
        full_edges_rv(cached)
      } else {
        full_edges_rv(NULL)
      }
    }, ignoreNULL = FALSE)

    full_hierarchy_edges <- shiny::reactive({
      edges <- full_edges_rv()
      if (!is.null(edges)) return(edges)

      data <- merged_rv()
      if (is.null(data) || nrow(data) == 0) return(.empty_hierarchy_edges())

      withCallingHandlers({
        shiny::withProgress(message = "Computing hierarchy graph cache...", value = 0.1, {
          edges <- compute_term_hierarchy(
            merged_data = data,
            min_score = 0,
            best_parent_only = FALSE
          )
          shiny::incProgress(0.8)
          full_edges_rv(edges)
          meta <- cache_meta_rv()
          if (!is.null(meta)) {
            .save_hierarchy_cache(cache_dir, meta, edges)
          }
          shiny::incProgress(0.1)
          edges
        })
      }, warning = function(w) {
        # keep graph computation resilient if cache writes fail
        invokeRestart("muffleWarning")
      })
    })

    hierarchy_edges <- shiny::reactive({
      edges <- full_hierarchy_edges()
      if (is.null(edges) || nrow(edges) == 0) return(.empty_hierarchy_edges())

      edges <- edges[edges$score >= input$min_score, , drop = FALSE]
      if (nrow(edges) == 0) return(edges)
      if (isTRUE(input$best_parent_only)) {
        edges <- .select_best_parent_per_child(edges)
      }
      edges[order(-edges$score, edges$child_term), , drop = FALSE]
    })

    plot_edges <- shiny::reactive({
      edges <- hierarchy_edges()
      if (nrow(edges) == 0) return(edges)

      max_edges <- as.integer(input$max_edges)
      if (is.na(max_edges) || max_edges < 1L) max_edges <- 200L
      max_edges <- min(max_edges, nrow(edges))
      edges[order(-edges$score), , drop = FALSE][seq_len(max_edges), , drop = FALSE]
    })

    shiny::observeEvent(input$node_click, {
      node <- input$node_click
      if (is.null(node) || !nzchar(node)) {
        selected_node_rv(NULL)
      } else {
        selected_node_rv(as.character(node))
      }
    }, ignoreNULL = FALSE)

    shiny::observe({
      edges <- plot_edges()
      selected <- selected_node_rv()
      valid_nodes <- unique(c(edges$parent_term, edges$child_term))
      if (!is.null(selected) && nzchar(selected) && !(selected %in% valid_nodes)) {
        selected_node_rv(NULL)
      }
    })

    selected_tree_terms <- shiny::reactive({
      edges <- plot_edges()
      selected <- selected_node_rv()
      if (is.null(selected) || !nzchar(selected) || nrow(edges) == 0) return(character(0))
      .hierarchy_connected_terms(edges, selected)
    })

    shiny::observe({
      if (is.null(highlight_terms_rv)) return(invisible(NULL))
      highlight_terms_rv(selected_tree_terms())
    })

    output$graph_status <- shiny::renderText({
      all_edges <- full_hierarchy_edges()
      shown_edges <- hierarchy_edges()
      if (is.null(all_edges) || nrow(all_edges) == 0) {
        return("No directed hierarchy edges available.")
      }
      if (nrow(shown_edges) == 0) {
        return(paste0(
          "No edges pass the current threshold (", sprintf("%.2f", input$min_score),
          "). Cached total edges: ", nrow(all_edges)
        ))
      }
      selected <- selected_node_rv()
      selected_suffix <- if (is.null(selected) || !nzchar(selected)) {
        ""
      } else {
        paste0(" | Selected node: ", selected)
      }
      nodes_n <- length(unique(c(shown_edges$parent_term, shown_edges$child_term)))
      paste0(
        "Displayed edges: ", nrow(shown_edges),
        " (cached total: ", nrow(all_edges), ")",
        " | Connected terms: ", nodes_n,
        " | Current threshold: ", sprintf("%.2f", input$min_score),
        selected_suffix
      )
    })

    output$graph_widget <- shiny::renderUI({
      if (requireNamespace("visNetwork", quietly = TRUE)) {
        visNetwork::visNetworkOutput(session$ns("hierarchy_graph"), height = "760px")
      } else {
        shiny::plotOutput(session$ns("hierarchy_plot"), height = "760px")
      }
    })

    if (requireNamespace("visNetwork", quietly = TRUE)) {
      graph_data <- shiny::reactive({
        .build_vis_graph_data(plot_edges(), def_lookup_rv())
      })

      output$hierarchy_graph <- visNetwork::renderVisNetwork({
        .build_vis_hierarchy_network(
          graph_data = graph_data(),
          click_input_id = session$ns("node_click")
        )
      })

      shiny::observe({
        gd <- graph_data()
        style <- .vis_style_for_selection(gd, selected_node_rv())
        proxy <- visNetwork::visNetworkProxy(session$ns("hierarchy_graph"), session = session)
        proxy <- visNetwork::visUpdateNodes(proxy, nodes = style$nodes)
        proxy <- visNetwork::visUpdateEdges(proxy, edges = style$edges)
      })

      shiny::observeEvent(input$zoom_tree, {
        gd <- graph_data()
        if (nrow(gd$nodes) == 0) return(invisible(NULL))
        linked <- selected_tree_terms()
        proxy <- visNetwork::visNetworkProxy(session$ns("hierarchy_graph"), session = session)
        if (length(linked) > 0) {
          visNetwork::visFit(
            proxy,
            nodes = linked,
            animation = list(duration = 500, easingFunction = "easeInOutQuad")
          )
        } else {
          visNetwork::visFit(
            proxy,
            animation = list(duration = 500, easingFunction = "easeInOutQuad")
          )
        }
      })
    } else {
      output$hierarchy_plot <- shiny::renderPlot({
        edges <- plot_edges()
        .plot_hierarchy_network(edges)
      }, res = 110)
    }

    output$hierarchy_table <- reactable::renderReactable({
      edges <- hierarchy_edges()
      selected <- selected_node_rv()
      linked <- selected_tree_terms()
      if (nrow(edges) == 0) {
        return(reactable::reactable(
          data.frame(Message = "No edges available at the current threshold."),
          bordered = TRUE
        ))
      }
      edges <- edges[order(-edges$score), , drop = FALSE]
      k <- min(50L, nrow(edges))
      out <- edges[seq_len(k), c(
        "parent_term", "child_term", "score", "lex_sub", "def_contain", "def_sim"
      )]
      reactable::reactable(
        out,
        striped = TRUE,
        bordered = TRUE,
        defaultPageSize = 10,
        rowStyle = function(index) {
          row <- out[index, , drop = FALSE]
          if (!is.null(selected) && nzchar(selected) &&
              (row$parent_term == selected || row$child_term == selected)) {
            return(list(background = "#ffe8b3", boxShadow = "inset 4px 0 0 #f59e0b"))
          }
          if (length(linked) > 0 &&
              row$parent_term %in% linked && row$child_term %in% linked) {
            return(list(background = "#fff7d6"))
          }
          NULL
        },
        columns = list(
          parent_term = reactable::colDef(name = "Parent"),
          child_term = reactable::colDef(name = "Child"),
          score = reactable::colDef(name = "Score", format = reactable::colFormat(digits = 3)),
          lex_sub = reactable::colDef(name = "LexSub", format = reactable::colFormat(digits = 3)),
          def_contain = reactable::colDef(name = "DefContain", format = reactable::colFormat(digits = 3)),
          def_sim = reactable::colDef(name = "DefSim", format = reactable::colFormat(digits = 3))
        )
      )
    })
  })
}

# ---- Plot helpers -----------------------------------------------------------

.plot_hierarchy_network <- function(edges) {
  graphics::plot.new()

  if (is.null(edges) || nrow(edges) == 0) {
    graphics::text(0.5, 0.5, "No hierarchy edges to display.", cex = 1.1, col = "#666666")
    return(invisible(NULL))
  }

  nodes <- .layout_hierarchy_nodes(edges)
  if (nrow(nodes) == 0) {
    graphics::text(0.5, 0.5, "No hierarchy nodes to display.", cex = 1.1, col = "#666666")
    return(invisible(NULL))
  }

  edge_idx <- cbind(
    match(edges$parent_term, nodes$term),
    match(edges$child_term, nodes$term)
  )

  xlim <- range(nodes$x) + c(-0.08, 0.08)
  ylim <- range(nodes$y) + c(-0.12, 0.12)
  graphics::plot.window(xlim = xlim, ylim = ylim)
  graphics::axis(1, labels = FALSE, tick = FALSE)
  graphics::axis(2, labels = FALSE, tick = FALSE)
  graphics::box()

  # Draw direction edges (parent -> child).
  score_rng <- range(edges$score, na.rm = TRUE)
  denom <- if (diff(score_rng) == 0) 1 else diff(score_rng)
  edge_lwd <- 0.7 + 2.2 * ((edges$score - score_rng[1]) / denom)
  edge_col <- grDevices::adjustcolor("#6c757d", alpha.f = 0.55)

  for (i in seq_len(nrow(edges))) {
    p <- edge_idx[i, 1]
    c <- edge_idx[i, 2]
    if (is.na(p) || is.na(c)) next
    graphics::arrows(
      x0 = nodes$x[p], y0 = nodes$y[p],
      x1 = nodes$x[c], y1 = nodes$y[c],
      length = 0.065, angle = 22,
      lwd = edge_lwd[i],
      col = edge_col
    )
  }

  level_vals <- sort(unique(nodes$level))
  level_col <- stats::setNames(
    grDevices::hcl.colors(length(level_vals), "Teal"),
    as.character(level_vals)
  )
  node_col <- unname(level_col[as.character(nodes$level)])

  graphics::points(
    nodes$x, nodes$y,
    pch = 21, bg = node_col, col = "#1f2937", cex = 1.15, lwd = 0.8
  )

  label_cex <- if (nrow(nodes) > 120) 0.45 else 0.62
  labels <- .shorten_term_label(nodes$term)
  graphics::text(nodes$x, nodes$y, labels = labels, pos = 3, cex = label_cex, col = "#1f2937")

  graphics::title(
    main = "Directed Term Hierarchy (parent \u2192 child)",
    sub = "Edges ranked by subsumption score. Layers follow term token count."
  )
}

.layout_hierarchy_nodes <- function(edges) {
  if (is.null(edges) || nrow(edges) == 0) {
    return(data.frame(term = character(), level = integer(), x = numeric(), y = numeric()))
  }

  parent_levels <- data.frame(
    term = edges$parent_term,
    level = edges$parent_token_n,
    stringsAsFactors = FALSE
  )
  child_levels <- data.frame(
    term = edges$child_term,
    level = edges$child_token_n,
    stringsAsFactors = FALSE
  )
  node_levels <- rbind(parent_levels, child_levels)
  node_levels <- stats::aggregate(level ~ term, data = node_levels, FUN = min)
  node_levels <- node_levels[order(node_levels$level, node_levels$term), , drop = FALSE]
  if (nrow(node_levels) == 0) return(node_levels)

  level_values <- sort(unique(node_levels$level))
  y_map <- stats::setNames(
    seq(from = 0.95, to = 0.05, length.out = length(level_values)),
    as.character(level_values)
  )

  node_levels$x <- NA_real_
  node_levels$y <- NA_real_
  for (lv in level_values) {
    idx <- which(node_levels$level == lv)
    k <- length(idx)
    xs <- if (k == 1L) 0.5 else seq(from = 0.06, to = 0.94, length.out = k)
    node_levels$x[idx] <- xs
    node_levels$y[idx] <- y_map[[as.character(lv)]]
  }

  node_levels
}

.shorten_term_label <- function(x, max_nchar = 32L) {
  ifelse(
    nchar(x) <= max_nchar,
    x,
    paste0(substr(x, 1, max_nchar - 1L), "\u2026")
  )
}

.build_definition_lookup <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(character(0))

  terms <- as.character(data$matched_term)
  terms <- terms[!is.na(terms)]
  terms <- trimws(terms)
  terms <- terms[nzchar(terms)]
  terms <- unique(terms)
  if (length(terms) == 0) return(character(0))

  defs <- setNames(rep("", length(terms)), terms)
  for (term in terms) {
    idx <- which(data$matched_term == term)[1]
    row <- data[idx, , drop = FALSE]
    defs[[term]] <- .collect_term_definition_text(row)
  }
  defs
}

.build_vis_graph_data <- function(edges, definition_lookup) {
  if (is.null(edges) || nrow(edges) == 0) {
    return(list(
      nodes = data.frame(
        id = character(),
        label = character(),
        title = character(),
        level = integer(),
        value = numeric(),
        color.background = character(),
        color.border = character(),
        font.color = character(),
        stringsAsFactors = FALSE
      ),
      edges = data.frame(
        id = character(),
        from = character(),
        to = character(),
        width = numeric(),
        title = character(),
        color.color = character(),
        color.opacity = numeric(),
        stringsAsFactors = FALSE
      ),
      edge_source = edges
    ))
  }

  node_terms <- unique(c(edges$parent_term, edges$child_term))

  parent_levels <- data.frame(term = edges$parent_term, level = edges$parent_token_n, stringsAsFactors = FALSE)
  child_levels <- data.frame(term = edges$child_term, level = edges$child_token_n, stringsAsFactors = FALSE)
  levels_df <- stats::aggregate(level ~ term, data = rbind(parent_levels, child_levels), FUN = min)
  levels_df <- levels_df[match(node_terms, levels_df$term), , drop = FALSE]

  deg_tab <- table(c(edges$parent_term, edges$child_term))
  node_degree <- as.numeric(deg_tab[node_terms])
  node_degree[is.na(node_degree)] <- 1

  level_vals <- sort(unique(levels_df$level))
  level_cols <- stats::setNames(
    grDevices::hcl.colors(length(level_vals), "Teal"),
    as.character(level_vals)
  )

  node_title <- vapply(node_terms, function(term) {
    definition <- if (!is.null(definition_lookup) && term %in% names(definition_lookup)) {
      definition_lookup[[term]]
    } else {
      ""
    }
    definition <- gsub("\\s+", " ", definition)
    definition <- trimws(definition)
    if (!nzchar(definition)) definition <- "No definition available."
    if (nchar(definition) > 1000) {
      definition <- paste0(substr(definition, 1, 997), "...")
    }
    paste0(
      "<div style='max-width:460px; white-space:normal;'>",
      "<strong>", htmltools::htmlEscape(term), "</strong><br/>",
      "<span>", htmltools::htmlEscape(definition), "</span>",
      "</div>"
    )
  }, character(1))

  nodes <- data.frame(
    id = node_terms,
    label = node_terms,
    title = node_title,
    level = levels_df$level,
    value = pmin(40, 6 + node_degree),
    color.background = unname(level_cols[as.character(levels_df$level)]),
    color.border = "#334155",
    font.color = "#111827",
    stringsAsFactors = FALSE
  )

  width_scale <- {
    rng <- range(edges$score, na.rm = TRUE)
    if (diff(rng) == 0) rep(2.2, nrow(edges)) else 1 + 5 * ((edges$score - rng[1]) / diff(rng))
  }

  edge_title <- paste0(
    "<b>", htmltools::htmlEscape(edges$parent_term), " \u2192 ",
    htmltools::htmlEscape(edges$child_term), "</b><br/>",
    "Score: ", sprintf("%.3f", edges$score),
    "<br/>LexSub: ", sprintf("%.3f", edges$lex_sub),
    "<br/>DefContain: ", sprintf("%.3f", edges$def_contain),
    "<br/>DefSim: ", sprintf("%.3f", edges$def_sim)
  )

  edge_df <- data.frame(
    id = paste0("e", seq_len(nrow(edges))),
    from = edges$parent_term,
    to = edges$child_term,
    width = width_scale,
    title = edge_title,
    color.color = "#6b7280",
    color.opacity = 0.60,
    stringsAsFactors = FALSE
  )

  list(
    nodes = nodes,
    edges = edge_df,
    edge_source = edges
  )
}

.build_vis_hierarchy_network <- function(graph_data, click_input_id = "node_click") {
  if (is.null(graph_data) || nrow(graph_data$nodes) == 0) {
    nodes <- data.frame(
      id = "none",
      label = "No edges at current threshold",
      title = "No hierarchy edges pass current filters.",
      stringsAsFactors = FALSE
    )
    return(
      visNetwork::visNetwork(nodes, data.frame(), width = "100%", height = "760px") |>
        visNetwork::visNodes(shape = "box", color = list(background = "#f3f4f6", border = "#9ca3af")) |>
        visNetwork::visPhysics(enabled = FALSE)
    )
  }

  visNetwork::visNetwork(
    nodes = graph_data$nodes,
    edges = graph_data$edges,
    width = "100%",
    height = "760px"
  ) |>
    visNetwork::visHierarchicalLayout(
      direction = "UD",
      sortMethod = "directed",
      levelSeparation = 190,
      nodeSpacing = 320,
      treeSpacing = 380,
      blockShifting = TRUE,
      edgeMinimization = TRUE,
      parentCentralization = TRUE
    ) |>
    visNetwork::visNodes(
      shape = "dot",
      font = list(size = 14, face = "Segoe UI")
    ) |>
    visNetwork::visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
      smooth = FALSE
    ) |>
    visNetwork::visInteraction(
      hover = TRUE,
      navigationButtons = TRUE,
      zoomView = TRUE,
      dragView = TRUE,
      dragNodes = TRUE
    ) |>
    visNetwork::visPhysics(enabled = FALSE) |>
    visNetwork::visEvents(
      click = sprintf(
        "function(params) {
           if (params.nodes && params.nodes.length > 0) {
             Shiny.setInputValue('%s', params.nodes[0], {priority: 'event'});
           } else {
             Shiny.setInputValue('%s', '', {priority: 'event'});
           }
         }",
        click_input_id,
        click_input_id
      )
    )
}

.vis_style_for_selection <- function(graph_data, selected_node = NULL) {
  if (is.null(graph_data) || nrow(graph_data$nodes) == 0) {
    return(list(
      nodes = data.frame(id = character(), stringsAsFactors = FALSE),
      edges = data.frame(id = character(), stringsAsFactors = FALSE)
    ))
  }

  nodes <- graph_data$nodes[, c("id", "value", "color.background", "color.border", "font.color"), drop = FALSE]
  edges <- graph_data$edges[, c("id", "width", "color.color", "color.opacity"), drop = FALSE]

  if (is.null(selected_node) || !nzchar(selected_node) || !(selected_node %in% nodes$id)) {
    return(list(nodes = nodes, edges = edges))
  }

  linked_terms <- .hierarchy_connected_terms(graph_data$edge_source, selected_node)
  linked_edge <- graph_data$edges$from %in% linked_terms & graph_data$edges$to %in% linked_terms

  non_linked <- !(nodes$id %in% linked_terms)
  nodes$color.background[non_linked] <- "#e5e7eb"
  nodes$color.border[non_linked] <- "#9ca3af"
  nodes$font.color[non_linked] <- "#9ca3af"

  sel <- nodes$id == selected_node
  nodes$color.background[sel] <- "#f59e0b"
  nodes$color.border[sel] <- "#b45309"
  nodes$font.color[sel] <- "#111827"
  nodes$value[sel] <- pmax(nodes$value[sel], 20)

  edges$color.color <- ifelse(linked_edge, "#2563eb", "#c7cbd1")
  edges$color.opacity <- ifelse(linked_edge, 0.95, 0.18)

  list(nodes = nodes, edges = edges)
}

.hierarchy_connected_terms <- function(edges, root_term) {
  if (is.null(edges) || nrow(edges) == 0 || is.null(root_term) || !nzchar(root_term)) {
    return(character(0))
  }
  all_nodes <- unique(c(edges$parent_term, edges$child_term))
  if (!(root_term %in% all_nodes)) return(character(0))

  child_of <- split(edges$child_term, edges$parent_term)
  parent_of <- split(edges$parent_term, edges$child_term)

  descendants <- .graph_reachable(root_term, child_of)
  ancestors <- .graph_reachable(root_term, parent_of)
  unique(c(root_term, descendants, ancestors))
}

.graph_reachable <- function(start_node, adjacency) {
  visited <- character(0)
  queue <- start_node
  while (length(queue) > 0) {
    node <- queue[[1]]
    queue <- queue[-1]
    neighbors <- adjacency[[node]]
    if (is.null(neighbors) || length(neighbors) == 0) next
    neighbors <- unique(as.character(neighbors))
    new_nodes <- setdiff(neighbors, c(visited, start_node))
    if (length(new_nodes) > 0) {
      visited <- c(visited, new_nodes)
      queue <- c(queue, new_nodes)
    }
  }
  unique(visited)
}

# ---- Cache helpers ----------------------------------------------------------

.hierarchy_cache_path <- function(cache_dir) {
  file.path(cache_dir, "hierarchy_edges_cache.rds")
}

.hierarchy_cache_meta <- function(data) {
  list(
    schema = 1L,
    fingerprint = .hierarchy_fingerprint(data)
  )
}

.hierarchy_fingerprint <- function(data) {
  if (is.null(data) || nrow(data) == 0) return("empty")

  payload <- list(
    matched_term = as.character(data$matched_term),
    ipbes_defs = lapply(data$ipbes_data, function(df) {
      if (is.null(df) || !("definition" %in% names(df))) return(character(0))
      as.character(df$definition)
    }),
    ipcc_defs = lapply(data$ipcc_data, function(df) {
      if (is.null(df) || !("definition" %in% names(df))) return(character(0))
      as.character(df$definition)
    })
  )

  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(payload, tf)
  unname(as.character(tools::md5sum(tf)[[1]]))
}

.load_hierarchy_cache <- function(cache_dir, expected_meta) {
  path <- .hierarchy_cache_path(cache_dir)
  if (!file.exists(path)) return(NULL)
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj) || !is.list(obj) || is.null(obj$meta) || is.null(obj$edges)) return(NULL)
  if (!identical(obj$meta, expected_meta)) return(NULL)
  obj$edges
}

.save_hierarchy_cache <- function(cache_dir, meta, edges) {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  path <- .hierarchy_cache_path(cache_dir)
  tryCatch(
    saveRDS(list(meta = meta, edges = edges), path),
    error = function(e) invisible(NULL)
  )
  invisible(TRUE)
}

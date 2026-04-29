# ============================================================
# server.R — 26C Network Analytics Dashboard
# Stage 3: All FAKEDATA stubs replaced with real data calls.
# g, gco, gmem are loaded once in app.R at startup.
# ============================================================

server <- function(input, output, session) {

  # ==========================================================
  # REACTIVES — all now backed by real data objects
  # ==========================================================

  # DATA SOURCE: g (REAL — FAKE_001 replaced Stage 3)
  # g is loaded in app.R from data/committees/graph_bipartite.rds
  bipartite_summary <- reactive({
    nodes_tbl <- g |>
      tidygraph::activate("nodes") |>
      dplyr::as_tibble()

    list(
      total_nodes  = igraph::vcount(g),
      n_committees = nodes_tbl |> dplyr::filter(type == TRUE)  |> nrow(),
      n_members    = nodes_tbl |> dplyr::filter(type == FALSE) |> nrow(),
      total_edges  = igraph::ecount(g),
      geo_counts   = nodes_tbl |>
        dplyr::filter(type == TRUE) |>           # committees only
        dplyr::count(geography, name = "count") |>
        dplyr::arrange(dplyr::desc(count)),
      type_counts  = nodes_tbl |>
        dplyr::filter(type == TRUE) |>
        dplyr::count(committee_type, name = "count") |>
        dplyr::arrange(dplyr::desc(count))
    )
  })

  # DATA SOURCE: gco (REAL — FAKE_002 replaced Stage 3)
  # gco is built in app.R via coaffiliate(g, type=FALSE, ...)
  coaff_edges <- reactive({
    gco |>
      tidygraph::activate("edges") |>
      dplyr::as_tibble() |>
      dplyr::filter(weight > 1) |>
      # join geography from the FROM node
      dplyr::left_join(
        gco |>
          tidygraph::activate("nodes") |>
          dplyr::as_tibble() |>
          dplyr::select(id = name, geography) |>   # name col → id
          dplyr::mutate(row_id = dplyr::row_number()),
        by = c("from" = "row_id")
      ) |>
      dplyr::select(from = .data$from_name %||% from,
                    to   = .data$to_name   %||% to,
                    weight, geography) |>
      dplyr::arrange(dplyr::desc(weight))
  })

  # Helper: pull named edges from gco cleanly
  coaff_edges_named <- reactive({
    edges_tbl <- gco |>
      tidygraph::activate("edges") |>
      dplyr::as_tibble()
    nodes_tbl <- gco |>
      tidygraph::activate("nodes") |>
      dplyr::as_tibble() |>
      dplyr::mutate(.row = dplyr::row_number())

    edges_tbl |>
      dplyr::filter(weight > 1) |>
      dplyr::left_join(nodes_tbl |> dplyr::select(.row, from_name = name, geography),
                       by = c("from" = ".row")) |>
      dplyr::left_join(nodes_tbl |> dplyr::select(.row, to_name   = name),
                       by = c("to"   = ".row")) |>
      dplyr::select(from = from_name, to = to_name, weight, geography) |>
      dplyr::arrange(dplyr::desc(weight))
  })

  # DATA SOURCE: gco isolates (REAL — FAKE_002 replaced Stage 3)
  isolate_info <- reactive({
    deg_tbl <- gco |>
      tidygraph::activate("nodes") |>
      dplyr::as_tibble() |>
      dplyr::mutate(degree = .data$deg)

    n_total    <- nrow(deg_tbl)
    n_isolates <- sum(deg_tbl$degree == 0)
    n_conn     <- n_total - n_isolates

    deg_levels = c(as.character(0:6), "7+")
    deg_dist = deg_tbl |>
      dplyr::mutate(
        degree_bucket = dplyr::if_else(.data$degree >= 7, "7+", as.character(.data$degree))
      ) |>
      dplyr::count(degree_bucket, name = "count") |>
      dplyr::rename(label = degree_bucket) |>
      dplyr::right_join(tibble::tibble(label = deg_levels), by = "label") |>
      dplyr::mutate(count = tidyr::replace_na(count, 0L))

    list(
      total_committees = n_total,
      n_isolates       = n_isolates,
      n_connected      = n_conn,
      degree_dist      = deg_dist
    )
  })

  # DATA SOURCE: gco centrality (REAL — FAKE_003 replaced Stage 3)
  # gco already has deg, wdeg, betw mutated in app.R
  centrality_data <- reactive({
    gco |>
      tidygraph::activate("nodes") |>
      dplyr::as_tibble()
  })

  # DATA SOURCE: shortest path distances from focal node (REAL — FAKE_003 replaced Stage 3)
  distance_data <- reactive({
    focal <- input$focalNode %||% "committee_23"
    nodes_tbl <- gco |>
      tidygraph::activate("nodes") |>
      dplyr::as_tibble()

    focal_idx <- which(nodes_tbl$name == focal)
    if (length(focal_idx) == 0) {
      # focal node not in gco (may be isolate in bipartite); use first non-isolate
      focal_idx <- which(nodes_tbl$deg > 0)[1]
    }

    # igraph shortest paths from focal to all others
    dists <- igraph::distances(igraph::as.igraph(gco),
                               v     = focal_idx,
                               mode  = "all") |>
             as.vector()
    dists[is.infinite(dists)] <- NA_real_

    max_steps <- min(max(dists, na.rm = TRUE), 8)
    steps_tbl <- tibble::tibble(steps_val = 0:max_steps) |>
      dplyr::mutate(
        label = dplyr::case_when(
          steps_val == 0 ~ "0 steps",
          steps_val == 1 ~ "1 step",
          TRUE           ~ paste0(steps_val, " steps")
        ),
        count = purrr::map_int(steps_val, ~sum(dists == .x, na.rm = TRUE))
      )

    steps_tbl
  })

  # Add steps_to_focal column to centrality data for table display
  centrality_with_steps <- reactive({
    focal <- input$focalNode %||% "committee_23"
    nodes_tbl <- gco |>
      tidygraph::activate("nodes") |>
      dplyr::as_tibble() |>
      dplyr::mutate(.row = dplyr::row_number())

    focal_idx <- which(nodes_tbl$name == focal)
    if (length(focal_idx) == 0) focal_idx <- which(nodes_tbl$deg > 0)[1]

    dists <- igraph::distances(igraph::as.igraph(gco),
                               v = focal_idx, mode = "all") |>
             as.vector()
    dists[is.infinite(dists)] <- NA_integer_

    nodes_tbl |>
      dplyr::mutate(steps_to_focal = as.integer(dists))
  })

  # DATA SOURCE: community detection (REAL — FAKE_004 replaced Stage 3)
  community_data <- reactive({
    algo <- input$communityAlgo %||% "infomap"
    gco_conn <- gco |> tidygraph::filter(!tidygraph::node_is_isolated())

    infomap_tbl <- gco_conn |>
      tidygraph::mutate(community = tidygraph::group_infomap() |> factor()) |>
      tidygraph::activate("nodes") |>
      dplyr::as_tibble() |>
      dplyr::count(community, name = "count") |>
      dplyr::mutate(community = paste("Community", community)) |>
      dplyr::arrange(dplyr::desc(count))

    fg_tbl <- gco_conn |>
      tidygraph::mutate(
        community = tidygraph::group_fast_greedy(
                      weights = .E()$weight, n_groups = 3) |> factor()
      ) |>
      tidygraph::activate("nodes") |>
      dplyr::as_tibble() |>
      dplyr::group_by(community) |>
      dplyr::summarise(
        count        = dplyr::n(),
        mean_wdeg    = round(mean(wdeg, na.rm = TRUE), 1),
        sd_wdeg      = round(sd(wdeg, na.rm = TRUE), 1),
        dominant_geo = dplyr::first(
          names(sort(table(geography), decreasing = TRUE))
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(community = paste("Community", community)) |>
      dplyr::arrange(dplyr::desc(count))

    list(infomap = infomap_tbl, fast_greedy = fg_tbl)
  })

  # DATA SOURCE: gmem geography sub-graphs (REAL — FAKE_005 replaced Stage 3)
  # gmem built in app.R; falls back to gco-based summary if gmem failed
  geo_stats <- reactive({
    geo_filter <- input$geoFilter %||% "all"
    gmem_nodes_tbl <- if (!is.null(gmem)) gmem |> tidygraph::activate("nodes") |> dplyr::as_tibble() else NULL
    gmem_edges_tbl <- if (!is.null(gmem)) gmem |> tidygraph::activate("edges") |> dplyr::as_tibble() else NULL
    has_gmem_geo = !is.null(gmem_nodes_tbl) && "geography" %in% names(gmem_nodes_tbl)
    has_gmem_type = !is.null(gmem_nodes_tbl) && "type" %in% names(gmem_nodes_tbl)
    has_gmem_edge_geo = !is.null(gmem_edges_tbl) && "geography" %in% names(gmem_edges_tbl)

    if (has_gmem_geo && has_gmem_type && has_gmem_edge_geo) {
      # Pull from gmem — summarise edge-based shared seats per geography group
      edges_tbl <- gmem_edges_tbl
      nodes_tbl <- gmem_nodes_tbl

      df <- nodes_tbl |>
        dplyr::group_by(geography) |>
        dplyr::summarise(
          n_members    = dplyr::n(),
          n_committees = sum(type == TRUE, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::left_join(
          edges_tbl |>
            dplyr::group_by(geography) |>
            dplyr::summarise(local_seats = sum(weight, na.rm = TRUE), .groups = "drop"),
          by = "geography"
        ) |>
        dplyr::mutate(
          local_seats  = tidyr::replace_na(local_seats, 0L),
          global_total = sum(local_seats)
        )
    } else {
      # Fallback: summarise from committee projection (gco)
      nodes_tbl <- gco |>
        tidygraph::activate("nodes") |>
        dplyr::as_tibble()
      edges_tbl <- gco |>
        tidygraph::activate("edges") |>
        dplyr::as_tibble()

      df <- nodes_tbl |>
        dplyr::filter(!is.na(geography)) |>
        dplyr::group_by(geography) |>
        dplyr::summarise(
          n_members    = dplyr::n(),
          n_committees = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::left_join(
          edges_tbl |>
            dplyr::mutate(
              geography = nodes_tbl$geography[from]
            ) |>
            dplyr::group_by(geography) |>
            dplyr::summarise(local_seats = sum(weight, na.rm = TRUE), .groups = "drop"),
          by = "geography"
        ) |>
        dplyr::mutate(
          local_seats  = tidyr::replace_na(local_seats, 0L),
          global_total = sum(local_seats, na.rm = TRUE)
        )
    }

    if (geo_filter != "all") {
      df <- dplyr::filter(df, tolower(geography) == tolower(geo_filter))
    }
    df
  })

  # ==========================================================
  # TAB 1 — BIPARTITE  (FAKE_001 → REAL)
  # ==========================================================

  output$bipartiteStats <- renderUI({
    s <- bipartite_summary()
    tags$div(class = "stat-grid",
      stat_card("Total Nodes",  s$total_nodes,  "committees + members"),
      stat_card("Committees",   s$n_committees, "type == TRUE",  modifier = "accent"),
      stat_card("Members",      s$n_members,    "type == FALSE", modifier = "accent2"),
      stat_card("Total Edges",  s$total_edges,  "memberships")
    )
  })

  observe({
    s <- bipartite_summary()
    session$sendCustomMessage("updateGeoChart", list(
      labels = as.list(s$geo_counts$geography),
      values = as.list(s$geo_counts$count)
    ))
  })

  observe({
    s <- bipartite_summary()
    session$sendCustomMessage("updateTypeChart", list(
      labels = as.list(s$type_counts$committee_type),
      values = as.list(s$type_counts$count)
    ))
  })

  # PLACEHOLDER_001 → REAL: bipartite network (ggraph FR layout)
  output$bipartiteNet <- renderPlot({
    ggraph::ggraph(g, layout = "fr") +
      ggraph::geom_edge_link(alpha = 0.25, colour = "#2e6da4") +
      ggraph::geom_node_point(
        ggplot2::aes(colour = type, size = type),
        show.legend = TRUE
      ) +
      ggplot2::scale_colour_manual(
        values = c("TRUE" = "#e8a020", "FALSE" = "#2e6da4"),
        labels = c("TRUE" = "Committee", "FALSE" = "Member")
      ) +
      ggplot2::scale_size_manual(
        values = c("TRUE" = 3.5, "FALSE" = 1.5),
        guide  = "none"
      ) +
      ggplot2::labs(colour = "Node type") +
      ggraph::theme_graph(base_family = "sans") +
      ggplot2::theme(legend.position = "bottom",
                     legend.text     = ggplot2::element_text(size = 9))
  }, height = 300)

  # PLACEHOLDER_002 → REAL: FR layout ggplot2 (geom_segment + geom_point)
  output$layoutVis <- renderPlot({
    layout_coords <- ggraph::create_layout(g, layout = "fr")
    edges_tbl <- g |>
      tidygraph::activate("edges") |>
      dplyr::as_tibble() |>
      dplyr::left_join(layout_coords |>
                         dplyr::select(from_id = name,
                                       from_x = x, from_y = y) |>
                         dplyr::mutate(.row = dplyr::row_number()),
                       by = c("from" = ".row")) |>
      dplyr::left_join(layout_coords |>
                         dplyr::select(to_id = name,
                                       to_x = x, to_y = y) |>
                         dplyr::mutate(.row = dplyr::row_number()),
                       by = c("to" = ".row"))

    ggplot2::ggplot() +
      ggplot2::geom_segment(
        data = edges_tbl,
        ggplot2::aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
        alpha = 0.18, colour = "#2e6da4", linewidth = 0.3
      ) +
      ggplot2::geom_point(
        data = layout_coords,
        ggplot2::aes(x = x, y = y, fill = type),
        shape = 21, colour = "white", size = 2,
        show.legend = TRUE
      ) +
      ggplot2::scale_fill_manual(
        values = c("TRUE" = "#e8a020", "FALSE" = "#2e6da4"),
        labels = c("TRUE" = "Committee", "FALSE" = "Member")
      ) +
      ggplot2::labs(fill = "Node type") +
      ggplot2::theme_void(base_family = "sans") +
      ggplot2::theme(legend.position = "bottom",
                     legend.text     = ggplot2::element_text(size = 9))
  }, height = 300)

  # ==========================================================
  # TAB 2 — COAFFILIATION  (FAKE_002 → REAL, PLACEHOLDER_003 → REAL)
  # ==========================================================

  # PLACEHOLDER_003 → REAL: coaffiliation network (ggraph)
  output$coaffNet <- renderPlot({
    gco |>
      tidygraph::filter(!tidygraph::node_is_isolated()) |>
      ggraph::ggraph(layout = "fr") +
        ggraph::geom_edge_link(
          ggplot2::aes(width = weight, alpha = weight),
          colour = "#2e6da4", show.legend = TRUE
        ) +
        ggraph::geom_node_point(
          ggplot2::aes(size = wdeg),
          colour = "#1a3a5c", fill = "#2e6da4", shape = 21
        ) +
        ggraph::scale_edge_width(range = c(0.3, 2.5), guide = "none") +
        ggraph::scale_edge_alpha(range = c(0.2, 0.7), guide = "none") +
        ggplot2::scale_size(range = c(1.5, 7), name = "Wtd. degree") +
        ggraph::theme_graph(base_family = "sans") +
        ggplot2::theme(legend.position = "bottom",
                       legend.text     = ggplot2::element_text(size = 9))
  }, height = 300)

  output$isolateStats <- renderUI({
    iso <- isolate_info()
    tags$div(class = "stat-grid",
      style = "grid-template-columns: 1fr 1fr 1fr; margin-bottom: 0;",
      stat_card("Total Committees", iso$total_committees, "in gco"),
      stat_card("Isolates",         iso$n_isolates,       "degree == 0",  modifier = "accent"),
      stat_card("Connected",        iso$n_connected,      "for analysis", modifier = "accent2")
    )
  })

  observe({
    iso <- isolate_info()
    session$sendCustomMessage("updateIsolateDegChart", list(
      labels = as.list(iso$degree_dist$label),
      values = as.list(iso$degree_dist$count)
    ))
  })

  output$sharedMembersTable <- renderTable({
    coaff_edges_named() |>
      dplyr::select(
        `From Committee` = from,
        `To Committee`   = to,
        `Shared Members` = weight,
        Geography        = geography
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s")

  # ==========================================================
  # TAB 3 — CENTRALITY  (FAKE_003 → REAL)
  # ==========================================================

  output$centralityStats <- renderUI({
    cd   <- centrality_data()
    dd   <- distance_data()
    focal <- input$focalNode %||% "committee_23"

    # Weighted mean distance (exclude self, exclude Inf)
    step_vals  <- dd$steps_val * dd$count
    total_n    <- sum(dd$count) - dd$count[dd$steps_val == 0]
    avg_dist   <- if (total_n > 0) round(sum(step_vals) / (total_n + 1e-9), 1) else NA

    tags$div(class = "stat-grid",
      stat_card("Median Degree",
                median(cd$deg,  na.rm = TRUE),
                "committees connected to"),
      stat_card("Max Weighted Degree",
                max(cd$wdeg, na.rm = TRUE),
                "shared member-seats", modifier = "accent"),
      stat_card("Avg Dist. from Focal",
                avg_dist,
                paste0("steps to ", focal), modifier = "accent2"),
      stat_card("Max Betweenness",
                round(max(cd$betw, na.rm = TRUE), 1),
                "top bridging committee")
    )
  })

  output$focalLabel <- renderText({ input$focalNode %||% "committee_23" })

  output$degDensityPlot <- plotly::renderPlotly({
    cd <- centrality_data() |>
      dplyr::filter(is.finite(deg), is.finite(wdeg))
    if (nrow(cd) == 0) {
      return(plotly::plot_ly() |>
        plotly::layout(
          title = "No centrality data available",
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        ))
    }

    deg_df = cd |>
      dplyr::transmute(value = deg, metric = "Degree")
    wdeg_df = cd |>
      dplyr::transmute(value = wdeg, metric = "Weighted degree")
    plot_df = dplyr::bind_rows(deg_df, wdeg_df)

    p = ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = value, colour = metric, fill = metric)
    ) +
      ggplot2::geom_density(alpha = 0.20, linewidth = 1.0, adjust = 1.1, na.rm = TRUE) +
      ggplot2::geom_rug(data = deg_df, ggplot2::aes(x = value), inherit.aes = FALSE,
                        sides = "b", alpha = 0.12, colour = "#2e6da4") +
      ggplot2::geom_vline(
        xintercept = stats::median(cd$deg, na.rm = TRUE),
        linetype = "dashed", linewidth = 0.5, colour = "#6b7a91"
      ) +
      ggplot2::scale_colour_manual(values = c("Degree" = "#2e6da4", "Weighted degree" = "#3cb4c8")) +
      ggplot2::scale_fill_manual(values = c("Degree" = "#2e6da4", "Weighted degree" = "#3cb4c8")) +
      ggplot2::labs(x = "Centrality value", y = "Density", colour = NULL, fill = NULL) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        legend.position = "top",
        panel.grid.minor = ggplot2::element_blank()
      )

    plotly::ggplotly(p, tooltip = c("x", "y", "colour")) |>
      plotly::layout(margin = list(l = 40, r = 10, b = 35, t = 20))
  })

  observe({
    dd    <- distance_data()
    focal <- input$focalNode %||% "committee_23"
    session$sendCustomMessage("updateDistanceChart", list(
      focal  = focal,
      labels = as.list(dd$label),
      values = as.list(dd$count)
    ))
  })

  output$centralityTable <- renderTable({
    centrality_with_steps() |>
      dplyr::arrange(dplyr::desc(wdeg)) |>
      dplyr::slice_head(n = 10) |>
      dplyr::select(
        Committee        = name,
        Geography        = geography,
        Degree           = deg,
        `Wtd. Degree`    = wdeg,
        Betweenness      = betw,
        `Dist. to Focal` = steps_to_focal
      ) |>
      dplyr::mutate(Betweenness = round(Betweenness, 1))
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s")

  # ==========================================================
  # TAB 4 — CLUSTERING  (FAKE_004 → REAL, PLACEHOLDER_004 → REAL)
  # ==========================================================

  output$algoLabel <- renderText({
    if ((input$communityAlgo %||% "infomap") == "infomap") "(Infomap)" else "(Fast-Greedy k=3)"
  })

  observe({
    cd   <- community_data()
    algo <- input$communityAlgo %||% "infomap"
    df   <- if (algo == "infomap") cd$infomap else cd$fast_greedy
    session$sendCustomMessage("updateCommunityBarChart", list(
      labels = as.list(df$community),
      values = as.list(df$count)
    ))
  })

  # PLACEHOLDER_004 → REAL: community network (ggraph, colored by community)
  output$communityNet <- renderPlot({
    gco |>
      tidygraph::filter(!tidygraph::node_is_isolated()) |>
      tidygraph::mutate(
        community = tidygraph::group_fast_greedy(
          weights = .E()$weight, n_groups = 3
        ) |> factor()
      ) |>
      ggraph::ggraph(layout = "fr") +
        ggraph::geom_edge_link(alpha = 0.22, colour = "#adb5bd") +
        ggraph::geom_node_point(
          ggplot2::aes(colour = community, size = wdeg),
          show.legend = TRUE
        ) +
        ggplot2::scale_colour_manual(
          values = c("1" = "#2e6da4", "2" = "#3cb4c8", "3" = "#e8a020"),
          name   = "Community"
        ) +
        ggplot2::scale_size(range = c(2, 7), name = "Wtd. degree") +
        ggraph::theme_graph(base_family = "sans") +
        ggplot2::theme(legend.position = "bottom",
                       legend.text     = ggplot2::element_text(size = 9))
  }, height = 250)

  output$clusterStatsTable <- renderTable({
    community_data()$fast_greedy |>
      dplyr::select(
        Community          = community,
        `N Committees`     = count,
        `Mean Wtd. Degree` = mean_wdeg,
        `SD`               = sd_wdeg,
        `Dominant Geo`     = dominant_geo
      ) |>
      dplyr::mutate(
        `Mean Wtd. Degree` = round(`Mean Wtd. Degree`, 1),
        `SD`               = round(`SD`, 1)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s")

  # ==========================================================
  # TAB 5 — ITERATION  (FAKE_005 → REAL)
  # ==========================================================

  observe({
    gs <- geo_stats()
    session$sendCustomMessage("updateGeoSubgraphChart", list(
      labels      = as.list(gs$geography),
      members     = as.list(gs$n_members),
      local_seats = as.list(gs$local_seats)
    ))
  })

  output$localCoaffTable <- renderTable({
    gs <- geo_stats()
    df <- gs |>
      dplyr::mutate(
        pct = dplyr::if_else(
          !is.na(global_total) & global_total > 0,
          round(local_seats / global_total * 100, 1),
          NA_real_
        )
      ) |>
      dplyr::select(
        Geography            = geography,
        `N Members`          = n_members,
        `N Committees`       = n_committees,
        `Local Shared Seats` = local_seats,
        `% of Global Total`  = pct
      )
    df
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s", na = "—")

  # Custom JS tabs can keep panels hidden from Shiny's default visibility tracker.
  # Force key outputs to keep rendering so plots/tables are ready when tab is shown.
  always_render = c(
    "coaffNet", "sharedMembersTable",
    "degDensityPlot", "centralityTable",
    "communityNet", "clusterStatsTable",
    "localCoaffTable"
  )
  purrr::walk(always_render, ~shiny::outputOptions(output, .x, suspendWhenHidden = FALSE))

}

# Null-coalescing helper
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && nchar(as.character(x)) > 0) x else y

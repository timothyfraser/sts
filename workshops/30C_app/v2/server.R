# ============================================================
# server.R — 26C Network Analytics Dashboard
# Tier A/B: wired to fake_data.R stubs (replace in Stage 3)
# Tier C:   placeholder UI divs (replace in Stage 3)
# ============================================================

server <- function(input, output, session) {

  # ==========================================================
  # REACTIVES — all currently backed by fake data
  # ==========================================================

  # DATA SOURCE: FAKE_001 (fake) — replace in Stage 3 with:
  #   g %>% activate("nodes") %>% as_tibble() %>% count(type, geography, committee_type)
  bipartite_summary <- reactive({ get_fake_bipartite_summary() })

  # DATA SOURCE: FAKE_002 (fake) — replace in Stage 3 with:
  #   gco %>% activate("edges") %>% as_tibble() %>% filter(weight > 1) %>% ...
  coaff_edges  <- reactive({ get_fake_coaff_edges() })
  isolate_info <- reactive({ get_fake_isolate_summary() })

  # DATA SOURCE: FAKE_003 (fake) — replace in Stage 3 with:
  #   gco %>% mutate(deg, wdeg, betw, steps = node_distance_to(...)) %>% as_tibble()
  centrality_data <- reactive({ get_fake_centrality() })

  distance_data <- reactive({
    get_fake_distance_by_focal(input$focalNode %||% "committee_23")
  })

  # DATA SOURCE: FAKE_004 (fake) — replace in Stage 3 with group_infomap() / group_fast_greedy()
  community_data <- reactive({ get_fake_communities() })

  # DATA SOURCE: FAKE_005 (fake) — replace in Stage 3 with gmem summarized by geography
  geo_stats <- reactive({
    get_fake_geo_stats(input$geoFilter %||% "all")
  })

  # ==========================================================
  # TAB 1 — BIPARTITE
  # ==========================================================

  # Tier A: stat cards
  output$bipartiteStats <- renderUI({
    s <- bipartite_summary()
    tags$div(class = "stat-grid",
      stat_card("Total Nodes",  s$total_nodes,  "committees + members", "FAKE_001"),
      stat_card("Committees",   s$n_committees, "type == TRUE",         "FAKE_001", "accent"),
      stat_card("Members",      s$n_members,    "type == FALSE",        "FAKE_001", "accent2"),
      stat_card("Total Edges",  s$total_edges,  "memberships",          "FAKE_001")
    )
  })

  # Tier A: geo chart — send data to Chart.js via custom message
  observe({
    s <- bipartite_summary()
    session$sendCustomMessage("updateGeoChart", list(
      labels = s$geo_counts$geography,
      values = as.list(s$geo_counts$count)
    ))
  })

  # Tier A: type donut chart
  observe({
    s <- bipartite_summary()
    session$sendCustomMessage("updateTypeChart", list(
      labels = s$type_counts$committee_type,
      values = as.list(s$type_counts$count)
    ))
  })

  # Tier C: bipartite network placeholder
  output$bipartiteNetPlaceholder <- renderUI({
    # PLACEHOLDER_001 — Stage 3: replace entire renderUI with:
    #   output$bipartiteNet <- renderPlot({ ggraph(g, "fr") + geom_edge_link() + ... })
    #   And in ui.R: plotOutput("bipartiteNet", height = "300px")
    placeholder_panel_ui(
      label = "igraph / tidygraph render",
      note  = "Network visualization — available in Shiny (Stage 3)\nplot(g) · ggraph layout",
      placeholder_id = "PLACEHOLDER_001"
    )
  })

  # Tier C: FR layout placeholder
  output$layoutVisPlaceholder <- renderUI({
    # PLACEHOLDER_002 — Stage 3: replace with renderPlot({ ggplot() + geom_segment() + geom_point() })
    placeholder_panel_ui(
      label = "ggraph FR layout + ggplot2",
      note  = "geom_segment (edges) + geom_point (nodes) colored by node type",
      placeholder_id = "PLACEHOLDER_002"
    )
  })

  # ==========================================================
  # TAB 2 — COAFFILIATION
  # ==========================================================

  # Tier C: coaff network placeholder
  output$coaffNetPlaceholder <- renderUI({
    # PLACEHOLDER_003 — Stage 3: replace with renderPlot({ ggraph(gco) + ... })
    placeholder_panel_ui(
      label = "ggraph render of gco",
      note  = "Node size ∝ degree; edge width ∝ shared members\nAvailable in Shiny (Stage 3)",
      placeholder_id = "PLACEHOLDER_003"
    )
  })

  # Tier A: isolate stat cards
  output$isolateStats <- renderUI({
    iso <- isolate_info()
    tags$div(class = "stat-grid",
      style = "grid-template-columns: 1fr 1fr 1fr; margin-bottom: 0;",
      stat_card("Total Committees", iso$total_committees, "in gco"),
      stat_card("Isolates",         iso$n_isolates,       "degree == 0",  modifier = "accent"),
      stat_card("Connected",        iso$n_connected,      "for analysis", modifier = "accent2")
    )
  })

  # Tier A: isolate degree distribution chart
  observe({
    iso <- isolate_info()
    session$sendCustomMessage("updateIsolateDegChart", list(
      labels = as.list(iso$degree_dist$label),
      values = as.list(iso$degree_dist$count)
    ))
  })

  # Tier A: shared members table
  output$sharedMembersTable <- renderTable({
    coaff_edges() |>
      dplyr::select(
        `From Committee` = from,
        `To Committee`   = to,
        `Shared Members` = weight,
        Geography        = geography
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s")

  # ==========================================================
  # TAB 3 — CENTRALITY
  # ==========================================================

  # Tier B: centrality stat cards
  output$centralityStats <- renderUI({
    cd <- centrality_data()
    tags$div(class = "stat-grid",
      stat_card("Median Degree",
                median(cd$deg, na.rm = TRUE),
                "committees connected to",      "FAKE_003"),
      stat_card("Max Weighted Degree",
                max(cd$wdeg, na.rm = TRUE),
                "shared member-seats",          "FAKE_003", "accent"),
      stat_card("Avg Dist. from Focal",
                round(mean(distance_data()$count * 0:5) / sum(distance_data()$count), 1),
                paste0("steps to ", input$focalNode %||% "committee_23"), "FAKE_003", "accent2"),
      stat_card("Max Betweenness",
                round(max(cd$betw, na.rm = TRUE), 1),
                "top bridging committee",       "FAKE_003")
    )
  })

  # Tier B: focal node label
  output$focalLabel <- renderText({ input$focalNode %||% "committee_23" })

  # Tier B: degree density chart (reactive on centrality_data)
  observe({
    cd <- centrality_data()
    # Build approximate density by binning (no server-side density estimation needed —
    # pass raw sorted values; JS does the smoothed line)
    session$sendCustomMessage("updateDegDensityChart", list(
      deg_values  = as.list(sort(cd$deg)),
      wdeg_values = as.list(sort(cd$wdeg)),
      median_deg  = median(cd$deg, na.rm = TRUE)
    ))
  })

  # Tier B: distance chart — reactive on focal node selection
  observe({
    dd <- distance_data()
    session$sendCustomMessage("updateDistanceChart", list(
      focal  = input$focalNode %||% "committee_23",
      labels = as.list(dd$steps),
      values = as.list(dd$count)
    ))
  })

  # Tier B: centrality table (top 10 by wdeg)
  output$centralityTable <- renderTable({
    centrality_data() |>
      dplyr::arrange(dplyr::desc(wdeg)) |>
      dplyr::slice_head(n = 10) |>
      dplyr::select(
        Committee     = name,
        Geography     = geography,
        Degree        = deg,
        `Wtd. Degree` = wdeg,
        Betweenness   = betw,
        `Dist. to Focal` = steps_to_23
      ) |>
      dplyr::mutate(Betweenness = round(Betweenness, 1))
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s")

  # ==========================================================
  # TAB 4 — CLUSTERING
  # ==========================================================

  # Tier A: algorithm label in panel header
  output$algoLabel <- renderText({
    if ((input$communityAlgo %||% "infomap") == "infomap") "(Infomap)" else "(Fast-Greedy k=3)"
  })

  # Tier A: community bar chart — reactive on input$communityAlgo
  observe({
    cd   <- community_data()
    algo <- input$communityAlgo %||% "infomap"
    df   <- if (algo == "infomap") cd$infomap else cd$fast_greedy
    session$sendCustomMessage("updateCommunityBarChart", list(
      labels = as.list(df$community),
      values = as.list(df$count)
    ))
  })

  # Tier C: community network placeholder
  output$communityNetPlaceholder <- renderUI({
    # PLACEHOLDER_004 — Stage 3: replace with renderPlot({ ggraph(gco) + geom_node_point(aes(color=community)) })
    placeholder_panel_ui(
      label = "ggraph colored by community",
      note  = "Nodes colored by group_fast_greedy assignment\nAvailable in Shiny (Stage 3)",
      placeholder_id = "PLACEHOLDER_004"
    )
  })

  # Tier B: cluster stats table (fast-greedy only — infomap doesn't produce cluster stats here)
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
  # TAB 5 — ITERATION
  # ==========================================================

  # Tier B: geo subgraph chart — reactive on input$geoFilter (set via JS hidden input)
  observe({
    gs <- geo_stats()
    session$sendCustomMessage("updateGeoSubgraphChart", list(
      labels       = as.list(gs$geography),
      members      = as.list(gs$n_members),
      local_seats  = as.list(gs$local_seats)
    ))
  })

  # Tier B: local coaffiliation table
  output$localCoaffTable <- renderTable({
    geo_stats() |>
      dplyr::mutate(pct = round(local_seats / global_total * 100, 1)) |>
      dplyr::select(
        Geography            = geography,
        `N Members`          = n_members,
        `N Committees`       = n_committees,
        `Local Shared Seats` = local_seats,
        `% of Global Total`  = pct
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s")

}

# Null-coalescing helper (base R doesn't have %||%)
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && x != "") x else y

# ============================================================
# ui.R — 26C Network Analytics Dashboard
# Raw HTML/JS UI — NO fluidPage, NO bslib, NO shinydashboard
# Design tokens injected from design_metadata.json at runtime
# ============================================================

ui <- tagList(

  tags$head(
    # Google Fonts
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Source+Serif+4:wght@400;600;700&family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@300;400;500;600&display=swap"),
    # Chart.js CDN
    tags$script(src = "https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js"),
    # App stylesheet (extracted from mockup)
    # Use includeCSS so local file always loads in Shiny.
    includeCSS("www/styles.css"),
    # Inject CSS variables from design_metadata.json (baked in Stage 2)
    tags$style(HTML(build_css_vars(metadata))),
    # App JS (Chart.js helpers, tab switching, filter pills)
    # Use includeScript so local file always loads in Shiny.
    includeScript("www/app.js")
  ),

  # ============================================================
  # APP SHELL
  # ============================================================
  tags$div(id = "app",

    # ---- TOP HEADER ----
    tags$div(id = "topbar",
      tags$div(
        tags$div(class = "logo", "Network Analytics"),
        tags$div(class = "subtitle", "26C — Disaster Recovery Committees")
      ),
      tags$div(class = "spacer"),
      tags$nav(class = "tab-nav",
        tags$button(class = "tab-btn active", `data-tab` = "tab-bipartite",
          onclick = "switchTab('tab-bipartite', this)", "1 · Bipartite Graph"),
        tags$button(class = "tab-btn", `data-tab` = "tab-coaffiliation",
          onclick = "switchTab('tab-coaffiliation', this)", "2 · Coaffiliation"),
        tags$button(class = "tab-btn", `data-tab` = "tab-centrality",
          onclick = "switchTab('tab-centrality', this)", "3 · Centrality"),
        tags$button(class = "tab-btn", `data-tab` = "tab-clustering",
          onclick = "switchTab('tab-clustering', this)", "4 · Clustering"),
        tags$button(class = "tab-btn", `data-tab` = "tab-iteration",
          onclick = "switchTab('tab-iteration', this)", "5 · Iteration")
      )
    ), # /topbar

    # ---- BODY ROW ----
    tags$div(id = "body-row",

      # ============================================================
      # LEFT SIDEBAR — filter controls (replaces design panel)
      # ============================================================
      tags$div(id = "sidebar",
        tags$div(class = "sb-section",
          tags$div(class = "sb-title", "Filters"),
          tags$div(class = "sb-meta",
            "Controls apply to Centrality and Iteration tabs.")
        ),

        tags$div(class = "sb-section",
          tags$div(class = "sb-label", "Focal Node (Centrality)"),
          tags$select(
            id = "focalNode", class = "sb-select",
            # Shiny picks this up as input$focalNode
            tags$option(value = "committee_23", "committee_23"),
            tags$option(value = "committee_1",  "committee_1"),
            tags$option(value = "committee_7",  "committee_7"),
            tags$option(value = "committee_15", "committee_15")
          )
        ),

        tags$div(class = "sb-section",
          tags$div(class = "sb-label", "Geography (Iteration)"),
          tags$div(class = "filter-bar", id = "geo-filters",
            tags$div(class = "filter-pill active",
              onclick = "setGeoFilter('all', this)",    "All"),
            tags$div(class = "filter-pill",
              onclick = "setGeoFilter('iwate', this)",  "Iwate"),
            tags$div(class = "filter-pill",
              onclick = "setGeoFilter('miyagi', this)", "Miyagi"),
            tags$div(class = "filter-pill",
              onclick = "setGeoFilter('fukushima', this)", "Fukushima"),
            tags$div(class = "filter-pill",
              onclick = "setGeoFilter('national', this)", "National")
          ),
          # Hidden input that Shiny reads as input$geoFilter
          tags$input(type = "hidden", id = "geoFilter", value = "all",
                     class = "shiny-bound-input")
        ),

        tags$div(class = "sb-section",
          tags$div(class = "sb-label", "Community Algorithm"),
          tags$select(
            id = "communityAlgo", class = "sb-select",
            tags$option(value = "infomap",     "Infomap (auto k)"),
            tags$option(value = "fast_greedy", "Fast-Greedy (k=3)")
          )
        ),

        tags$div(class = "sb-section sb-legend",
          tags$div(class = "sb-label", "Legend"),
          tags$div(class = "legend-row",
            tags$span(class = "fake-badge", "FAKE"), " = synthetic data"
          ),
          tags$div(class = "legend-row",
            tags$span(class = "placeholder-badge", "PLACEHOLDER"), " = Stage 3"
          )
        )
      ), # /sidebar

      # ============================================================
      # MAIN CONTENT AREA
      # ============================================================
      tags$div(id = "main-content",

        # ==================== TAB 1: BIPARTITE ====================
        tags$div(id = "tab-bipartite", class = "tab-pane active",

          tags$div(class = "section-header",
            tags$h2("Bipartite Graph — Committees & Members"),
            tags$span(class = "step-tag", "Section 0 · Setup")
          ),
          tags$p(class = "section-desc",
            "A bipartite (two-mode) network connects two distinct node types — ",
            tags$strong("committees"), " and ", tags$strong("members"),
            ". An edge means a person holds membership on a committee."
          ),

          # Concept box
          tags$div(class = "concept-box",
            tags$span(class = "comment", "# Load the bipartite tidygraph object"), tags$br(),
            tags$span(class = "keyword", "g"), " = ",
            tags$span(class = "fn", "read_rds"), "(",
            tags$span(class = "str", '"data/committees/graph_bipartite.rds"'), ")", tags$br(), tags$br(),
            tags$span(class = "comment", "# type == TRUE  → committee nodes"), tags$br(),
            tags$span(class = "comment", "# type == FALSE → member nodes"), tags$br(),
            "g ", tags$span(class = "comment", "# 695 nodes, 749 edges")
          ),

          # Stat cards — Tier A: wired to output$bipartiteStats
          uiOutput("bipartiteStats"),

          # Network plots — Tier C: placeholders
          tags$div(class = "panel-grid cols-2",
            tags$div(class = "panel", id = "panel_bipartite_net",
              tags$div(class = "panel-header",
                tags$h3("Bipartite Network")
                # PLACEHOLDER_001 replaced Stage 3 ✅
              ),
              tags$div(class = "panel-body", style = "padding: 0;",
                plotOutput("bipartiteNet", height = "300px")
              )
            ),
            tags$div(class = "panel", id = "panel_layout_vis",
              tags$div(class = "panel-header",
                tags$h3("FR Layout Preview")
                # PLACEHOLDER_002 replaced Stage 3 ✅
              ),
              tags$div(class = "panel-body", style = "padding: 0;",
                plotOutput("layoutVis", height = "300px")
              )
            )
          ),

          # Geography breakdown — Tier A: wired to JS charts via Shiny messages
          tags$div(class = "panel",
            tags$div(class = "panel-header",
              tags$h3("Committee Geography Breakdown")
              # FAKE_001 replaced Stage 3 ✅
            ),
            tags$div(class = "panel-body",
              tags$div(class = "panel-grid cols-2",
                tags$div(
                  tags$div(class = "chart-sub-label",
                    "Committees by geographic scope"),
                  tags$div(class = "chart-wrap", style = "height:200px",
                    tags$canvas(id = "geoChart")
                  )
                ),
                tags$div(
                  tags$div(class = "chart-sub-label",
                    "Committee type distribution"),
                  tags$div(class = "chart-wrap", style = "height:200px",
                    tags$canvas(id = "typeChart")
                  )
                )
              )
            )
          )

        ), # /tab-bipartite

        # ==================== TAB 2: COAFFILIATION ====================
        tags$div(id = "tab-coaffiliation", class = "tab-pane",

          tags$div(class = "section-header",
            tags$h2("Coaffiliation Network"),
            tags$span(class = "step-tag", "Section 1 · Transformation")
          ),
          tags$p(class = "section-desc",
            "Coaffiliation projects a bipartite graph into a unipartite graph. ",
            "Here: committee–committee edges where weight = number of shared members."
          ),

          tags$div(class = "concept-box",
            tags$span(class = "comment", "# Load helper function"), tags$br(),
            tags$span(class = "fn", "source"), "(",
            tags$span(class = "str", '"functions/coaffiliate.R"'), ")", tags$br(), tags$br(),
            tags$span(class = "keyword", "gco"), " = ",
            tags$span(class = "fn", "coaffiliate"),
            "(graph = g, type = FALSE, names = TRUE,", tags$br(),
            HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
            "weight = ", tags$span(class = "str", '"weight"'), ", diag = FALSE)"
          ),

          callout_box(
            tags$strong("How it works: "),
            "The bipartite adjacency matrix B is used to compute ",
            tags$em(HTML("B<sup>T</sup>·B")), " (committee projection). ",
            "The diagonal is zeroed so committees don't count sharing with themselves."
          ),

          tags$div(class = "panel-grid cols-2",
            # PLACEHOLDER_003 replaced Stage 3 ✅
            tags$div(class = "panel", id = "panel_coaff_net",
              tags$div(class = "panel-header",
                tags$h3("Coaffiliation Network (Committees)")
              ),
              tags$div(class = "panel-body", style = "padding: 0;",
                plotOutput("coaffNet", height = "300px")
              )
            ),

            # Isolates analysis — Tier A: wired
            tags$div(class = "panel", id = "panel_isolates",
              tags$div(class = "panel-header",
                tags$h3("Isolates Analysis")
                # FAKE_002 replaced Stage 3 ✅
              ),
              tags$div(class = "panel-body",
                callout_box(
                  tags$strong("Isolates"), " are committees with no shared members.",
                  " Removing them reduces graph size before analysis.",
                  type = "warn"
                ),
                tags$div(style = "margin-top:0.75rem;"),
                uiOutput("isolateStats"),
                tags$div(class = "chart-wrap", style = "height:160px; margin-top:0.75rem",
                  tags$canvas(id = "isolateDegChart")
                )
              )
            )
          ),

          # Shared members table — Tier A: wired
          tags$div(class = "panel",
            tags$div(class = "panel-header",
              tags$h3("Committees Sharing > 1 Member")
              # FAKE_002 replaced Stage 3 ✅
            ),
            tags$div(class = "panel-body",
              tableOutput("sharedMembersTable")
            )
          )

        ), # /tab-coaffiliation

        # ==================== TAB 3: CENTRALITY ====================
        tags$div(id = "tab-centrality", class = "tab-pane",

          tags$div(class = "section-header",
            tags$h2("Centrality Metrics"),
            tags$span(class = "step-tag", "Section 2.2 · Quantities of Interest")
          ),
          tags$p(class = "section-desc",
            "How central is each committee? Three measures: ",
            tags$strong("degree"), " (raw connections), ",
            tags$strong("weighted degree"), " (sum of shared members), and ",
            tags$strong("betweenness"), " (bridging capacity)."
          ),

          # Centrality stat cards — Tier B
          uiOutput("centralityStats"),

          # Degree density — Tier B: Chart.js via message handler
          tags$div(class = "panel", id = "panel_centrality_dist",
            tags$div(class = "panel-header",
              tags$h3("Degree Distribution")
              # FAKE_003 replaced Stage 3 ✅
            ),
            tags$div(class = "panel-body",
              callout_box(
                tags$strong("Interpretation: "),
                "Density curves show degree distribution across all committees. ",
                "Most are low-degree; a few are highly connected hubs. ",
                "Dashed line marks the median degree."
              ),
              tags$div(style = "margin-top:0.85rem;"),
              tags$div(class = "chart-wrap", style = "height:240px",
                plotly::plotlyOutput("degDensityPlot", height = "240px")
              )
            )
          ),

          # Centrality table — Tier B: tableOutput
          tags$div(class = "panel", id = "panel_centrality_table",
            tags$div(class = "panel-header",
              tags$h3("Top 10 Committees by Centrality")
              # FAKE_003 replaced Stage 3 ✅
            ),
            tags$div(class = "panel-body",
              tableOutput("centralityTable")
            )
          ),

          # Distance to focal — Tier B: reactive on input$focalNode
          tags$div(class = "panel",
            tags$div(class = "panel-header",
              tags$h3("Distance to Focal Node")
              # FAKE_003 replaced Stage 3 ✅
            ),
            tags$div(class = "panel-body",
              callout_box(
                "Focal node is selected in the left sidebar. ",
                "Chart shows distribution of steps from every other committee."
              ),
              tags$div(style = "margin-top:0.75rem; font-size:0.76rem; color:var(--color-muted)",
                "Focal: ", tags$strong(textOutput("focalLabel", inline = TRUE))
              ),
              tags$div(style = "margin-top:0.5rem;"),
              tags$div(class = "chart-wrap", style = "height:200px",
                tags$canvas(id = "distanceChart")
              )
            )
          )

        ), # /tab-centrality

        # ==================== TAB 4: CLUSTERING ====================
        tags$div(id = "tab-clustering", class = "tab-pane",

          tags$div(class = "section-header",
            tags$h2("Community Detection"),
            tags$span(class = "step-tag", "Section 2.3 · Clustering")
          ),
          tags$p(class = "section-desc",
            "Community detection groups nodes that are more densely connected to each other ",
            "than to the rest of the graph. Two algorithms: ",
            tags$strong("fast-greedy"), " (k=3) and ", tags$strong("infomap"), "."
          ),

          tags$div(class = "concept-box",
            tags$span(class = "comment", "# Fast-greedy: specify k groups"), tags$br(),
            "gco %>% ", tags$span(class = "fn", "mutate"),
            "(community = ", tags$span(class = "fn", "group_fast_greedy"),
            "(weights = .E()$weight, n_groups = 3))", tags$br(), tags$br(),
            tags$span(class = "comment", "# Infomap: detects its own number of communities"), tags$br(),
            "gco %>% ", tags$span(class = "fn", "mutate"),
            "(community = ", tags$span(class = "fn", "group_infomap"),
            "() %>% ", tags$span(class = "fn", "factor"), "())"
          ),

          callout_box(
            tags$strong("Caution: "),
            "Use community detection only with clear theoretical justification. ",
            "An algorithm without a reason is not analytically useful.",
            type = "warn"
          ),

          tags$div(class = "panel-grid cols-2",
            # Community bar — Tier A: reactive on input$communityAlgo
            tags$div(class = "panel", id = "panel_community_bar",
              tags$div(class = "panel-header",
                tags$h3("Community Sizes"),
                # FAKE_004 replaced Stage 3 ✅
                tags$span(class = "algo-badge", textOutput("algoLabel", inline = TRUE))
              ),
              tags$div(class = "panel-body",
                tags$div(class = "chart-wrap", style = "height:220px",
                  tags$canvas(id = "communityBarChart")
                )
              )
            ),

            # PLACEHOLDER_004 replaced Stage 3 ✅
            tags$div(class = "panel",
              tags$div(class = "panel-header",
                tags$h3("Community Network")
              ),
              tags$div(class = "panel-body", style = "padding: 0;",
                plotOutput("communityNet", height = "250px")
              )
            )
          ),

          # Cluster stats table — Tier B: tableOutput
          tags$div(class = "panel", id = "panel_community_stats",
            tags$div(class = "panel-header",
              tags$h3("Cluster Statistics (Fast-Greedy, k=3)")
              # FAKE_004 replaced Stage 3 ✅
            ),
            tags$div(class = "panel-body",
              tags$p(style = "font-size:0.78rem; color:var(--color-muted); margin-bottom:0.75rem",
                "Mean and SD of weighted degree centrality within each community"),
              tableOutput("clusterStatsTable")
            )
          )

        ), # /tab-clustering

        # ==================== TAB 5: ITERATION ====================
        tags$div(id = "tab-iteration", class = "tab-pane",

          tags$div(class = "section-header",
            tags$h2("Iterating with purrr & morph()"),
            tags$span(class = "step-tag", "Section 3 · Scalable Analytics")
          ),
          tags$p(class = "section-desc",
            "When a graph is too large to analyze at once, split it into sub-graphs, ",
            "apply analysis per piece with ",
            tags$code("purrr::map()"), ", then reassemble."
          ),

          # Flow diagram
          tags$div(class = "flow-steps",
            tags$div(class = "flow-step active-step",
              tags$div(class = "flow-step-title", "morph()"),
              tags$div(class = "flow-step-desc",
                "Split graph by ", tags$em("from_geo"), " into sub-graphs")
            ),
            tags$div(class = "flow-step active-step",
              tags$div(class = "flow-step-title", "map()"),
              tags$div(class = "flow-step-desc", "Apply function to each sub-graph")
            ),
            tags$div(class = "flow-step active-step",
              tags$div(class = "flow-step-title", "bind / join"),
              tags$div(class = "flow-step-desc", "Reassemble into one graph")
            ),
            tags$div(class = "flow-step",
              tags$div(class = "flow-step-title", "analyze"),
              tags$div(class = "flow-step-desc", "Run centrality, clustering, etc.")
            )
          ),

          tags$div(class = "concept-box",
            tags$span(class = "comment", "# Split, sample, reassemble"), tags$br(),
            tags$span(class = "keyword", "glist"), " = g %>%", tags$br(),
            HTML("&nbsp;&nbsp;"), tags$span(class = "fn", "activate"),
            "(", tags$span(class = "str", '"edges"'), ") %>%", tags$br(),
            HTML("&nbsp;&nbsp;"), tags$span(class = "fn", "mutate"),
            "(from_geo = .N()$geography[ .E()$from ]) %>%", tags$br(),
            HTML("&nbsp;&nbsp;"), tags$span(class = "fn", "morph"),
            "(to_split, from_geo, split_by = ",
            tags$span(class = "str", '"edges"'), ") %>%", tags$br(),
            HTML("&nbsp;&nbsp;"), tags$span(class = "fn", "map"),
            "(~.x %>% ", tags$span(class = "fn", "activate"),
            "(", tags$span(class = "str", '"edges"'), ") %>% ",
            tags$span(class = "fn", "sample_n"), "(size = 30))", tags$br(), tags$br(),
            tags$span(class = "comment", "# Two ways to reassemble:"), tags$br(),
            "glist %>% ", tags$span(class = "fn", "bind_graphs_list"),
            "(.id = ", tags$span(class = "str", '"group"'), ")",
            tags$span(class = "comment", "    # allows duplicate nodes"), tags$br(),
            "glist %>% ", tags$span(class = "fn", "graph_join_list"),
            "(by = ", tags$span(class = "str", '"name"'), ", .id = ",
            tags$span(class = "str", '"group"'), ")",
            tags$span(class = "comment", "  # no duplicate nodes")
          ),

          # Geo subgraph chart — Tier B: reactive on input$geoFilter (via JS hidden input)
          tags$div(class = "panel",
            tags$div(class = "panel-header",
              tags$h3("Sub-graph Statistics by Geography")
              # FAKE_005 replaced Stage 3 ✅
            ),
            tags$div(class = "panel-body",
              tags$p(style = "font-size:0.76rem; color:var(--color-muted); margin-bottom:0.5rem",
                "Geography filter is set in the left sidebar."),
              tags$div(class = "chart-wrap", style = "height:220px",
                tags$canvas(id = "geoSubgraphChart")
              )
            )
          ),

          # Local coaff table — Tier B: tableOutput
          tags$div(class = "panel",
            tags$div(class = "panel-header",
              tags$h3("Local Coaffiliation by Geography")
              # FAKE_005 replaced Stage 3 ✅
            ),
            tags$div(class = "panel-body",
              callout_box(
                tags$strong("gmem"), ": iterative coaffiliation — ",
                tags$code("coaffiliate(type=TRUE)"),
                " applied per geography subgraph, then joined via ",
                tags$code("graph_join_list()"), ". ",
                "Compares local vs. global total shared member-seats."
              ),
              tags$div(style = "margin-top:0.75rem;"),
              tableOutput("localCoaffTable")
            )
          )

        ) # /tab-iteration

      ) # /main-content
    ) # /body-row
  ) # /app
) # tagList

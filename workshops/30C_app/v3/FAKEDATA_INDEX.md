# FAKEDATA_INDEX — 26C Network Analytics Dashboard
# Stage 3 Final

| ID | Description | Used In | Real Replacement | Stage 3 Status |
|----|-------------|---------|-----------------|----------------|
| FAKE_001 | Bipartite graph summary: node/edge counts, geography & committee type | `output$bipartiteStats`, `updateGeoChart`, `updateTypeChart` | `g` via `read_rds("data/committees/graph_bipartite.rds")` | ✅ Replaced — `bipartite_summary()` reactive reads from `g` |
| FAKE_002 | Coaffiliation edges (weight > 1) and isolate degree distribution | `output$sharedMembersTable`, `output$isolateStats`, `updateIsolateDegChart` | `gco` from `coaffiliate(g, type=FALSE, ...)` built in `app.R` | ✅ Replaced — `coaff_edges_named()` and `isolate_info()` read from `gco` |
| FAKE_003 | Node centrality metrics: degree, weighted degree, betweenness, distance | `output$centralityStats`, `output$centralityTable`, `updateDegDensityChart`, `updateDistanceChart` | `gco` with `deg`, `wdeg`, `betw` mutated in `app.R`; `igraph::distances()` for focal distances | ✅ Replaced — `centrality_data()` and `distance_data()` reactive on `input$focalNode` |
| FAKE_004 | Community detection results (infomap + fast-greedy k=3) | `output$clusterStatsTable`, `updateCommunityBarChart` | `gco %>% mutate(community = group_infomap())` / `group_fast_greedy()` reactive on `input$communityAlgo` | ✅ Replaced — `community_data()` reactive computes both algorithms on demand |
| FAKE_005 | Geography sub-graph statistics from purrr iterative coaffiliation | `output$localCoaffTable`, `updateGeoSubgraphChart` | `gmem` built in `app.R` via `morph(to_split) + map(coaffiliate) + graph_join_list()` | ✅ Replaced — `geo_stats()` reactive reads from `gmem`; falls back to `g` summary if `gmem` fails |
| PLACEHOLDER_001 | Bipartite network visualization (igraph/ggraph) | `output$bipartiteNet` | `ggraph(g, "fr") + geom_edge_link() + geom_node_point(aes(colour=type))` | ✅ Replaced — `renderPlot` + `plotOutput("bipartiteNet")` in `ui.R` |
| PLACEHOLDER_002 | FR layout ggplot2 visualization | `output$layoutVis` | `ggplot() + geom_segment(edges) + geom_point(layout, aes(fill=type))` | ✅ Replaced — `renderPlot` + `plotOutput("layoutVis")` in `ui.R` |
| PLACEHOLDER_003 | Coaffiliation network plot (ggraph) | `output$coaffNet` | `ggraph(gco) + geom_edge_link(aes(width=weight)) + geom_node_point(aes(size=wdeg))` | ✅ Replaced — `renderPlot` + `plotOutput("coaffNet")` in `ui.R` |
| PLACEHOLDER_004 | Community-colored network plot | `output$communityNet` | `ggraph(gco) + geom_node_point(aes(color=community))` with `group_fast_greedy` | ✅ Replaced — `renderPlot` + `plotOutput("communityNet")` in `ui.R` |

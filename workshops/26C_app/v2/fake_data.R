# ============================================================
# R/fake_data.R — All synthetic data stubs for Stage 2
# Every function starts with get_fake_
# Replace each in Stage 3 with the real data source noted below
# ============================================================

# FAKEDATA: FAKE_001
# Description: Bipartite graph node/edge counts, geography & committee type breakdown
# Columns: type (logical), geography (chr), committee_type (chr), count (int)
# Used in: bipartite stat cards, geoChart (output$geoChart), typeChart (output$typeChart)
# Real replacement: g loaded via read_rds("data/committees/graph_bipartite.rds")
#   - counts come from: g %>% activate("nodes") %>% as_tibble() %>% count(type, geography, committee_type)
get_fake_bipartite_summary <- function() {
  list(
    total_nodes   = 695L,
    n_committees  = 49L,
    n_members     = 646L,
    total_edges   = 749L,
    geo_counts = tibble::tibble(
      geography      = c("iwate", "miyagi", "fukushima", "national"),
      count          = c(21L, 14L, 8L, 6L)
    ),
    type_counts = tibble::tibble(
      committee_type = c("Municipal", "Prefectural", "Other", "National"),
      count          = c(28L, 10L, 7L, 4L)
    )
  )
}

# FAKEDATA: FAKE_002
# Description: Coaffiliation edges (weight > 1) and degree distribution
# Columns: from (chr), to (chr), weight (int), geography (chr)
# Used in: shared-members table (output$sharedMembersTable), isolate stat cards,
#          degree distribution bar (output$isolateDegChart)
# Real replacement: gco (coaffiliate(g, type=FALSE, names=TRUE, weight="weight", diag=FALSE))
#   - edges: gco %>% activate("edges") %>% as_tibble() %>% filter(weight > 1) %>% left_join geography
#   - isolates: gco %>% filter(node_is_isolated()) %>% as_tibble() %>% nrow()
get_fake_coaff_edges <- function() {
  tibble::tibble(
    from   = c("committee_23","committee_1", "committee_23","committee_4", "committee_7", "committee_2"),
    to     = c("committee_7", "committee_15","committee_31","committee_18","committee_31","committee_9"),
    weight = c(4L, 3L, 3L, 2L, 2L, 2L),
    geography = c("iwate","miyagi","iwate","fukushima","iwate","miyagi")
  )
}

get_fake_isolate_summary <- function() {
  list(
    total_committees = 49L,
    n_isolates       = 12L,
    n_connected      = 37L,
    degree_dist = tibble::tibble(
      degree = 0:8,
      label  = c("0","1","2","3","4","5","6","7","8+"),
      count  = c(12L, 4L, 6L, 7L, 5L, 4L, 3L, 3L, 5L)
    )
  )
}

# FAKEDATA: FAKE_003
# Description: Node centrality metrics for coaffiliation graph
# Columns: name (chr), geography (chr), deg (int), wdeg (int), betw (dbl), steps_to_23 (int)
# Used in: centrality stat cards, output$degDensityPlot, output$centralityTable,
#          output$distanceChart (filtered by input$focalNode)
# Real replacement: data tibble from:
#   gco %>% activate("nodes") %>%
#     mutate(deg  = centrality_degree(mode="all"),
#            wdeg = centrality_degree(mode="all", weights=.E()$weight),
#            betw = centrality_betweenness(directed=FALSE, weights=.E()$weight),
#            steps = node_distance_to(nodes = which(.N()$name == input$focalNode))) %>%
#     as_tibble()
get_fake_centrality <- function() {
  tibble::tibble(
    name      = c("committee_23","committee_7","committee_1","committee_31","committee_15",
                  "committee_4","committee_18","committee_2","committee_9","committee_12",
                  paste0("committee_", c(5,6,8,10,11,13,14,16,17,19))),
    geography = c("iwate","iwate","national","iwate","miyagi",
                  "fukushima","fukushima","miyagi","miyagi","national",
                  rep(c("iwate","miyagi","fukushima","national"), length.out = 10)),
    deg  = c(18L,15L,14L,13L,11L,10L,9L,9L,8L,7L, 6L,6L,5L,5L,4L,4L,3L,3L,2L,1L),
    wdeg = c(42L,38L,35L,30L,27L,24L,22L,20L,17L,14L,12L,11L,9L,8L,7L,6L,5L,4L,3L,2L),
    betw = c(87.3,72.1,65.4,54.2,48.9,41.3,38.7,33.1,28.4,21.0,
             17.2,14.8,11.3,9.5,7.1,5.6,3.8,2.4,1.2,0.4),
    steps_to_23 = c(0L,1L,2L,1L,2L,3L,3L,2L,3L,4L,
                    2L,3L,3L,4L,4L,3L,4L,5L,5L,6L)
  )
}

get_fake_distance_by_focal <- function(focal_node = "committee_23") {
  dist_data <- list(
    committee_23 = c(1L,8L,14L,9L,4L,1L),
    committee_1  = c(1L,6L,11L,12L,5L,2L),
    committee_7  = c(1L,7L,13L,10L,5L,1L),
    committee_15 = c(1L,5L,10L,13L,6L,2L)
  )
  key <- if (focal_node %in% names(dist_data)) focal_node else "committee_23"
  tibble::tibble(
    steps = c("0 steps","1 step","2 steps","3 steps","4 steps","5 steps"),
    count = dist_data[[key]]
  )
}

# FAKEDATA: FAKE_004
# Description: Community detection results (infomap + fast-greedy k=3)
# Columns: community (chr/int), count (int), mean_wdeg (dbl), sd_wdeg (dbl), dominant_geo (chr)
# Used in: output$communityBarChart, output$clusterStatsTable
# Real replacement:
#   infomap counts: gco %>% mutate(community = group_infomap() %>% factor()) %>%
#                   as_tibble() %>% count(community)
#   cluster stats:  gco %>% filter(!node_is_isolated()) %>%
#                   mutate(community = group_fast_greedy(weights=.E()$weight, n_groups=3),
#                          deg = centrality_degree(weights=.E()$weight, mode="all")) %>%
#                   as_tibble() %>% group_by(community) %>%
#                   summarize(count=n(), mean=mean(deg,na.rm=TRUE), sd=sd(deg,na.rm=TRUE))
get_fake_communities <- function() {
  list(
    infomap = tibble::tibble(
      community    = c("Community 1","Community 2","Community 3","Community 4"),
      count        = c(14L, 12L, 7L, 4L)
    ),
    fast_greedy = tibble::tibble(
      community    = c("Community 1","Community 2","Community 3"),
      count        = c(16L, 13L, 8L),
      mean_wdeg    = c(22.3, 17.8, 11.4),
      sd_wdeg      = c(8.1,  6.3,  4.2),
      dominant_geo = c("iwate","miyagi","fukushima")
    )
  )
}

# FAKEDATA: FAKE_005
# Description: Geography sub-graph statistics from purrr iterative coaffiliation
# Columns: geography (chr), n_members (int), n_committees (int), local_seats (int), global_total (int)
# Used in: output$geoSubgraphChart (filtered by input$geoFilter), output$localCoaffTable
# Real replacement: gmem built via:
#   gmem = g %>% activate("edges") %>%
#     mutate(from_geo = .N()$geography[.E()$from]) %>%
#     morph(to_split, from_geo, split_by="edges") %>%
#     map(~coaffiliate(graph=.x, type=TRUE, names=TRUE, weight="weight", diag=FALSE)) %>%
#     graph_join_list(by="name", .id="geography")
#   Then summarize edges by geography group.
get_fake_geo_stats <- function(geo_filter = "all") {
  df <- tibble::tibble(
    geography    = c("iwate","miyagi","fukushima","national"),
    n_members    = c(280L, 195L, 112L, 59L),
    n_committees = c(21L,  14L,  8L,   6L),
    local_seats  = c(184L, 128L, 72L,  42L),
    global_total = c(312L, 312L, 312L, 312L)
  )
  if (geo_filter != "all") {
    df <- dplyr::filter(df, tolower(geography) == tolower(geo_filter))
  }
  df
}

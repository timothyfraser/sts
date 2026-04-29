# ============================================================
# R/fake_data.R — STAGE 3: All stubs replaced with real data.
# Functions below are COMMENTED OUT and kept for reference only.
# None of these are called from server.R any more.
# ============================================================

# FAKEDATA REPLACED: FAKE_001 (Stage 3)
# Real: g %>% activate("nodes") %>% as_tibble() %>% count(type, geography, committee_type)
# get_fake_bipartite_summary <- function() { ... }

# FAKEDATA REPLACED: FAKE_002 (Stage 3)
# Real: gco %>% activate("edges") %>% as_tibble() %>% filter(weight > 1) %>% ...
# get_fake_coaff_edges <- function() { ... }
# get_fake_isolate_summary <- function() { ... }

# FAKEDATA REPLACED: FAKE_003 (Stage 3)
# Real: gco (with deg, wdeg, betw mutated in app.R) %>%
#         mutate(steps = igraph::distances(focal_idx)) %>% as_tibble()
# get_fake_centrality <- function() { ... }
# get_fake_distance_by_focal <- function(focal_node = "committee_23") { ... }

# FAKEDATA REPLACED: FAKE_004 (Stage 3)
# Real: gco %>% filter(!node_is_isolated()) %>%
#         mutate(community = group_infomap() %>% factor()) %>% ...
# get_fake_communities <- function() { ... }

# FAKEDATA REPLACED: FAKE_005 (Stage 3)
# Real: gmem built in app.R via morph(to_split) + map(coaffiliate) + graph_join_list()
# get_fake_geo_stats <- function(geo_filter = "all") { ... }

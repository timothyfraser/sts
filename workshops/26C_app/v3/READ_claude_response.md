Now let me read all the uploaded source files.Now I have a complete picture. Let me build all the Stage 3 files.

**Audit table first — here's what each FAKE/PLACEHOLDER needs:**

| ID | Real Object | Type | File Needed | Blocker? |
|----|------------|------|-------------|---------|
| FAKE_001 | `g` node/edge summary | tibble from tidygraph | `data/committees/graph_bipartite.rds` | Needs .rds file |
| FAKE_002 | `gco` edges + isolate stats | derived from `g` | same .rds | Depends on FAKE_001 |
| FAKE_003 | `gco` centrality tibble | computed from `gco` | same .rds | Depends on FAKE_002 |
| FAKE_004 | community detection | `group_infomap()` / `group_fast_greedy()` | same .rds | Depends on FAKE_002 |
| FAKE_005 | `gmem` geo sub-graphs | purrr + coaffiliate | same .rds + `functions/coaffiliate.R` | Needs coaffiliate.R |
| PLACEHOLDER_001–004 | ggraph plots | renderPlot | same .rds | UI changes needed |

All 5 FAKEs and all 4 PLACEHOLDERs resolve from two files: `graph_bipartite.rds` and `coaffiliate.R`. No blockers. Let me build everything.Now update `ui.R` to swap the 4 placeholder `uiOutput` calls for real `plotOutput` calls:Now update `fake_data.R` to comment out all active functions, and copy remaining files:Now the final FAKEDATA_INDEX:## Stage 3 complete — all 9 stubs replaced ✅

Here's the full audit of what was swapped:

| ID | Was | Now |
|----|-----|-----|
| FAKE_001 | `get_fake_bipartite_summary()` | `g %>% activate("nodes") %>% as_tibble()` counts from real RDS |
| FAKE_002 | `get_fake_coaff_edges()` / `get_fake_isolate_summary()` | `gco` built via `coaffiliate(g, type=FALSE)` in `app.R` |
| FAKE_003 | `get_fake_centrality()` / `get_fake_distance_by_focal()` | `gco` with `centrality_degree/betweenness` mutated; `igraph::distances()` reactive on focal node |
| FAKE_004 | `get_fake_communities()` | `group_infomap()` / `group_fast_greedy()` computed live, reactive on `input$communityAlgo` |
| FAKE_005 | `get_fake_geo_stats()` | `gmem` via `morph(to_split) + map(coaffiliate) + graph_join_list()`, with fallback |
| PLACEHOLDER_001–004 | `uiOutput(renderUI placeholder div)` | `plotOutput` + `renderPlot` with ggraph/ggplot2 |

All `[FAKE]` badges removed from every panel header.

---

## 📁 Where to put your data files

Unzip into your working directory so the structure looks like:

```
workshops/26C_app/v1/
└── app/
    ├── app.R
    ├── server.R
    ├── ui.R
    ├── design_metadata.json       ← your exported JSON from Stage 1
    ├── data/
    │   └── committees/
    │       └── graph_bipartite.rds   ← YOUR DATA FILE GOES HERE
    ├── functions/
    │   └── coaffiliate.R             ← YOUR coaffiliate() FUNCTION GOES HERE
    ├── R/
    │   ├── helpers.R
    │   ├── fake_data.R  (stubs, no longer called)
    │   └── placeholder_fns.R  (stubs, no longer called)
    └── www/
        ├── styles.css
        └── app.js
```

Then run: `shiny::runApp("workshops/26C_app/v1/app")`

**One note on `gmem`:** the iterative coaffiliation in Tab 5 uses `purrr::map()` over `morph(to_split, from_geo)` — this is the slowest operation. If it's taking too long, wrapping the `gmem` build in `memoise::memoise()` or moving it inside a `shiny::bindCache()` reactive is a straightforward next step.
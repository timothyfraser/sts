# v14a.R - prep for the architecture-builder lab (chapter 14).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v14a.R
# It writes:
#   docs-v3/labs/data/v14a.json        three stakeholder briefs sized from course data
#   tests/labs/golden/v14a.json        named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
#
# Built from _template.R. The R pane is evaluated below to get every readout. The SQL pane is a
# generated Supabase schema skeleton (views, pg_cron): it is shown, not executed, and the page
# says so under the code panel.

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)

# ---- 0. REPO ROOT [KEEP] -------------------------------------------------------------------
find_root = function(dir = getwd()) {
  dir = normalizePath(dir, mustWork = TRUE)
  while (!(dir.exists(file.path(dir, "data")) && dir.exists(file.path(dir, "docs-v3")))) {
    parent = dirname(dir)
    if (parent == dir) stop("prep: could not find the repo root (a folder with data/ and docs-v3/)")
    dir = parent
  }
  dir
}
root = find_root()

# ---- 1. CONFIG -----------------------------------------------------------------------------
lab_id     = "v14a"
page       = "docs-v3/labs/v14a-architecture-builder.html"
seed       = 5460
data_out   = paste0("docs-v3/labs/data/", lab_id, ".json")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. SIZE THE THREE BRIEFS FROM COURSE DATA ---------------------------------------------
# Each brief is a fictional stakeholder need. Its row counts come from the course datasets:
#   rows_window = rows the source holds for the window the stakeholder looks at
#   groups      = rows left after the aggregation the stakeholder actually needs
set.seed(seed)

# air_quality: hourly PM2.5 readings per monitor; the last 30 days in the file.
aq = read_csv(file.path(root, "data/air_quality/air_quality.csv"), show_col_types = FALSE)
aq_last = aq %>% filter(datetime > max(datetime) - as.difftime(30, units = "days"))
aq_rows = nrow(aq_last)
aq_groups = aq_last %>% mutate(day = as.Date(datetime)) %>% distinct(aqs_id_full, day) %>% nrow()

# bluebikes: one row per station (with its block group); a month of hourly station counts.
bb = readRDS(file.path(root, "data/bluebikes/stationbg_dataset.rds"))
bb_stations = nrow(bb)
bb_rows = bb_stations * 24 * 30
bb_groups = bb_stations

# social infrastructure: one row per block group per year, rolled up to city by year.
si = readRDS(file.path(root, "data/social_infra/bg_data.rds"))
si_rows = nrow(si)
si_groups = si %>% as.data.frame() %>% distinct(name, year) %>% nrow()

briefs = tibble(
  brief         = c("air_quality", "bluebikes", "social_infra"),
  stakeholder   = c("County air-quality officer", "Bikeshare rebalancing lead", "City planning analyst"),
  need          = c("Daily mean PM2.5 per monitor, on a public dashboard",
                    "Rides per station, so vans can rebalance docks",
                    "Social infrastructure per resident, city by year"),
  source        = c("EPA AirNow feed", "Bluebikes trip feed", "Annual site census"),
  rows_window   = as.numeric(c(aq_rows, bb_rows, si_rows)),
  groups        = as.numeric(c(aq_groups, bb_groups, si_groups)),
  bytes_per_row = c(64, 48, 96),
  cadence_min   = c(60, 15, 525600),
  max_stale_min = c(120, 30, 527040),
  max_payload_kb = c(250, 250, 250),
  budget_usd    = c(40, 40, 40),
  requests_month = c(30000, 90000, 3000)
)

dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_json(list(briefs = briefs), file.path(root, data_out), digits = NA, pretty = TRUE)
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)
briefs = as_tibble(fromJSON(file.path(root, data_out))$briefs) %>%   # exactly the bytes the page sees
  mutate(across(where(is.numeric), as.numeric))                    # JS numbers are doubles

# ---- 3. STATES -----------------------------------------------------------------------------
# compute: where the aggregation runs. client = React sums raw rows; endpoint = Plumber sums on
# every request; matview = a materialised view in Supabase, refreshed by pg_cron.
states = list(
  baseline   = list(brief = "air_quality", compute = "client",  refresh_min = 60, grain = "aggregate", rls = FALSE),
  aq_matview = list(brief = "air_quality", compute = "matview", refresh_min = 60, grain = "aggregate", rls = FALSE),
  aq_person  = list(brief = "air_quality", compute = "matview", refresh_min = 60, grain = "person",    rls = FALSE),
  aq_rls     = list(brief = "air_quality", compute = "matview", refresh_min = 60, grain = "person",    rls = TRUE),
  bikes      = list(brief = "bluebikes",   compute = "endpoint", refresh_min = 15, grain = "aggregate", rls = FALSE),
  social     = list(brief = "social_infra", compute = "matview", refresh_min = 1440, grain = "aggregate", rls = FALSE)
)

# ---- 4. PANEL CODE -------------------------------------------------------------------------
code_r = function(s) {
  paste0(
    "check = briefs %>%\n",
    "  filter(brief == \"", s$brief, "\") %>%\n",
    "  mutate(\n",
    "    compute = \"", s$compute, "\", refresh_min = ", s$refresh_min,
    ", grain = \"", s$grain, "\", rls = ", toupper(s$rls), ",\n",
    "    rows_sent = if_else(compute == \"client\", rows_window, groups),\n",
    "    rows_scanned = if_else(compute == \"matview\", groups, rows_window),\n",
    "    payload_kb = rows_sent * bytes_per_row / 1024,\n",
    "    latency_ms = 80 + rows_scanned / 500,\n",
    "    stale_min = if_else(compute == \"matview\", pmax(refresh_min, cadence_min), cadence_min),\n",
    "    cron_usd = if_else(compute == \"matview\", 43200 / refresh_min * rows_window * 3e-7, 0),\n",
    "    api_usd = if_else(compute == \"endpoint\", requests_month * rows_window * 5e-10, 0),\n",
    "    egress_usd = requests_month * payload_kb / 1024^2 * 0.09,\n",
    "    cost_usd = 25 + cron_usd + api_usd + egress_usd,\n",
    "    pass_fresh = stale_min + latency_ms / 60000 <= max_stale_min,\n",
    "    pass_payload = payload_kb <= max_payload_kb,\n",
    "    pass_privacy = grain == \"aggregate\" | rls,\n",
    "    pass_cost = cost_usd <= budget_usd\n",
    "  )\n\n",
    "check %>%\n",
    "  select(payload_kb, latency_ms, stale_min, cost_usd,\n",
    "         pass_fresh, pass_payload, pass_privacy, pass_cost)"
  )
}
code_sql = function(s) {
  src = s$brief
  lines = c(
    paste0("-- Supabase schema skeleton for the ", src, " brief"),
    paste0("CREATE TABLE ", src, "_raw ("),
    "  id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,",
    "  unit_id text NOT NULL,",
    if (s$grain == "person") "  person_id uuid NOT NULL," else NULL,
    "  observed_at timestamptz NOT NULL,",
    "  value double precision",
    ");"
  )
  if (s$compute == "matview") {
    lines = c(lines, "",
      paste0("CREATE MATERIALIZED VIEW ", src, "_daily AS"),
      paste0("SELECT unit_id, ", if (s$grain == "person") "person_id, " else "",
             "date_trunc('day', observed_at) AS day, avg(value) AS mean_value"),
      paste0("FROM ", src, "_raw GROUP BY 1, 2", if (s$grain == "person") ", 3" else "", ";"),
      "",
      paste0("SELECT cron.schedule('refresh_", src, "', '",
             if (s$refresh_min < 60) paste0("*/", s$refresh_min, " * * * *")
             else if (s$refresh_min < 1440) paste0("0 */", s$refresh_min / 60, " * * *")
             else "0 3 * * *",
             "',"),
      paste0("  'REFRESH MATERIALIZED VIEW ", src, "_daily');"))
  }
  if (isTRUE(s$rls)) {
    lines = c(lines, "",
      paste0("ALTER TABLE ", src, "_raw ENABLE ROW LEVEL SECURITY;"),
      paste0("CREATE POLICY own_rows ON ", src, "_raw"),
      "  FOR SELECT USING (person_id = auth.uid());")
  }
  paste(lines, collapse = "\n")
}

readouts_from = function(result) {
  r = as.list(result)
  lapply(r, function(v) if (is.logical(v)) ifelse(v, "pass", "fail") else v)
}

# ---- 5. RUN EACH STATE (the R pane, evaluated for real) ------------------------------------
golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$briefs = briefs
  result = eval(parse(text = code_r(s)), envir = env)
  stopifnot(nrow(result) == 1)
  list(name = name, state = s, readouts = readouts_from(result),
       code = list(r = code_r(s), sql = code_sql(s)))
})

# ---- 6. WRITE GOLDEN [KEEP] ----------------------------------------------------------------
golden = list(
  lab          = lab_id,
  page         = page,
  generated_by = paste0("tools/labs/prep/", lab_id, ".R"),
  tolerance    = 1e-6,
  states       = unname(golden_states),
  ack          = ack
)
dir.create(dirname(file.path(root, golden_out)), recursive = TRUE, showWarnings = FALSE)
write_json(golden, file.path(root, golden_out), auto_unbox = TRUE, digits = NA, pretty = TRUE)
message("prep: wrote ", data_out, " (", file.size(file.path(root, data_out)), " B) and ", golden_out)

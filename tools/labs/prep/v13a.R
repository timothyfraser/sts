# v13a.R - prep for the lab "A pipeline on a clock: cron, pg_cron and freshness".
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v13a.R
# It writes:
#   docs-v3/labs/data/v13a.csv       one simulated day of the hourly PM2.5 feed (38 sites x 24 hours)
#   tests/labs/golden/v13a.json      named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).

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

# ---- 1. CONFIG [CHANGE] --------------------------------------------------------------------
lab_id    = "v13a"
page      = "docs-v3/labs/v13a-pipeline-on-a-clock.html"
source    = "data/air_quality/air_quality.csv"
seed      = 5460
sim_day   = "2024-08-15"                                  # a day where 38 sites report all 24 hours
data_out  = paste0("docs-v3/labs/data/", lab_id, ".csv")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + REDUCE ------------------------------------------------------------------------
set.seed(seed)
feed_df = read_csv(file.path(root, source), show_col_types = FALSE, col_types = "cccdc") %>%
  filter(pollutant == "PM2.5", substr(datetime, 1, 10) == sim_day) %>%
  distinct(aqs_id_full, datetime, .keep_all = TRUE) %>%
  group_by(aqs_id_full) %>%
  filter(n() == 24) %>%
  ungroup() %>%
  mutate(minute = as.integer(substr(datetime, 12, 13)) * 60L) %>%
  arrange(minute, aqs_id_full) %>%
  select(aqs_id_full, datetime, minute, value)

dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_csv(feed_df, file.path(root, data_out), na = "")
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

# Re-read the exact bytes the page fetches. The panel code calls this data `feed`.
feed = read_csv(file.path(root, data_out), show_col_types = FALSE, col_types = "ccid")

# ---- 3. STATES -----------------------------------------------------------------------------
states = list(
  baseline    = list(ingest = "0 * * * *",    refresh = "15 */3 * * *", fail = "none",  policy = "upsert"),
  fail_append = list(ingest = "0 * * * *",    refresh = "15 */3 * * *", fail = "13:00", policy = "append"),
  fail_upsert = list(ingest = "0 * * * *",    refresh = "15 */3 * * *", fail = "13:00", policy = "upsert"),
  slow_ingest = list(ingest = "0 */6 * * *",  refresh = "0 * * * *",    fail = "none",  policy = "upsert")
)

# ---- 4. PANEL CODE -------------------------------------------------------------------------
code_r = function(s) {
  failing = s$fail != "none"
  paste0(
    "# .github/workflows/ingest.yml  (GitHub Actions reads cron in UTC)\n",
    "# on:\n",
    "#   schedule:\n",
    "#     - cron: '", s$ingest, "'   # ingest\n",
    "#     - cron: '", s$refresh, "'   # refresh the API view\n",
    "\n",
    "# minutes after midnight when a cron string fires (minute + hour fields)\n",
    "cron_runs = function(cron) {\n",
    "  f = strsplit(cron, \" \")[[1]]\n",
    "  field = function(x, n) {\n",
    "    if (x == \"*\") return(0:(n - 1))\n",
    "    if (startsWith(x, \"*/\")) return(seq(0, n - 1, by = as.integer(substring(x, 3))))\n",
    "    as.integer(strsplit(x, \",\")[[1]])\n",
    "  }\n",
    "  sort(as.vector(outer(field(f[1], 60), field(f[2], 24) * 60, `+`)))\n",
    "}\n",
    "\n",
    "ingest = function(table, batch) {\n",
    if (s$policy == "upsert")
      "  rows_upsert(table, batch, by = c(\"aqs_id_full\", \"datetime\"))   # idempotent\n"
    else
      "  bind_rows(table, batch)   # append: a rerun adds the batch again\n",
    "}\n",
    "\n",
    "runs = cron_runs(\"", s$ingest, "\")\n",
    if (failing) paste0(
      "fail_at = min(runs[runs >= ", hm_to_min(s$fail), "])   # first run at or after ", s$fail, " crashes\n",
      "runs = sort(c(runs, fail_at + 5))                 # the scheduler retries 5 minutes later\n") else "",
    "refreshes = cron_runs(\"", s$refresh, "\")\n",
    "events = bind_rows(tibble(t = runs, job = \"ingest\"), tibble(t = refreshes, job = \"refresh\")) %>%\n",
    "  arrange(t, job)\n",
    "\n",
    "table = feed[0, ]\n",
    "watermark = -1       # last minute safely loaded\n",
    "served = NA          # newest reading the API view can see\n",
    "served_at_18 = NA\n",
    "for (i in seq_len(nrow(events))) {\n",
    "  t = events$t[i]\n",
    "  if (events$job[i] == \"ingest\") {\n",
    "    batch = feed %>% filter(minute > watermark, minute <= t)\n",
    "    table = ingest(table, batch)\n",
    if (failing)
      "    if (t != fail_at) watermark = t   # the crash comes after the write, before the watermark\n"
    else
      "    watermark = t\n",
    "  } else {\n",
    "    served = if (nrow(table) > 0) max(table$minute) else NA\n",
    "  }\n",
    "  if (t <= 18 * 60) served_at_18 = served\n",
    "}\n",
    "\n",
    "tibble(\n",
    "  ingest_runs   = length(runs),\n",
    "  rows_in_table = nrow(table),\n",
    "  duplicate_rows = nrow(table) - nrow(distinct(table, aqs_id_full, datetime)),\n",
    "  freshness_min = 18 * 60 - served_at_18,     # API staleness at 18:00\n",
    "  next_refresh  = sprintf(\"%02d:%02d\", min(refreshes[refreshes > 600]) %/% 60, min(refreshes[refreshes > 600]) %% 60)\n",
    ")"
  )
}
hm_to_min = function(x) as.integer(substr(x, 1, 2)) * 60L + as.integer(substr(x, 4, 5))

code_sql = function(s) {
  paste0(
    "-- pg_cron: the database runs its own clock (times in UTC)\n",
    "SELECT cron.schedule('ingest-air', '", s$ingest, "', $$CALL ingest_air()$$);\n",
    "SELECT cron.schedule('refresh-api', '", s$refresh, "',\n",
    "  $$REFRESH MATERIALIZED VIEW CONCURRENTLY api_latest$$);\n",
    "\n",
    "-- inside ingest_air(): load the batch staged since the watermark\n",
    "INSERT INTO readings (aqs_id_full, datetime, value)\n",
    "SELECT aqs_id_full, datetime, value FROM staging",
    if (s$policy == "upsert")
      "\nON CONFLICT (aqs_id_full, datetime)\n  DO UPDATE SET value = EXCLUDED.value;   -- upsert: a rerun is a no-op"
    else
      ";   -- append: a rerun inserts the same rows again",
    "\n\n",
    "-- next 5 runs: SELECT jobname, schedule FROM cron.job;"
  )
}

readouts_from = function(result) as.list(result)

# ---- 5. RUN EACH STATE (the panel's R, run for real) ---------------------------------------
golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$feed = feed
  result = eval(parse(text = code_r(s)), envir = env)
  list(name = name, state = s, readouts = readouts_from(result), code = list(r = code_r(s), sql = code_sql(s)))
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

# v1b.R - prep for lab v1b "Connect, query, collect: where the work happens".
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v1b.R
# It writes:
#   docs-v3/labs/data/v1b.json          per-state rows moved, SQL sent, result rows (from the full table)
#   docs-v3/labs/data/v1b_sample.csv    a seeded 2,000-row sample of flights (the rows on the wire)
#   tests/labs/golden/v1b.json          named states, readouts, code, acks
# Running it twice must leave `git status` unchanged (byte-for-byte output).
#
# The class database is Postgres (Supabase), reached with dbConnect() + env vars. This script
# runs the SAME dplyr/dbplyr pipeline against data/nycflights.sqlite, so the rows moved and the
# SQL sent are real. The connection lines in the panel are shown, not evaluated here.

library(dplyr)
library(dbplyr)
library(readr)
library(purrr)
library(jsonlite)
library(DBI)
library(RSQLite)

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
lab_id     = "v1b"
page       = "docs-v3/labs/v1b-connect-and-query.html"
source     = "data/nycflights.sqlite"
seed       = 5460
n_sample   = 2000
data_out   = "docs-v3/labs/data/v1b.json"
sample_out = "docs-v3/labs/data/v1b_sample.csv"
golden_out = "tests/labs/golden/v1b.json"
bytes_per_value = 8          # rough wire cost of one value
wire_bytes_per_s = 1.25e6    # a 10 Mbps home connection
ack = list()

con = dbConnect(SQLite(), file.path(root, source))
table_rows = dbGetQuery(con, "SELECT COUNT(*) AS n FROM flights")$n
table_cols = length(dbListFields(con, "flights"))

# ---- 2. SAMPLE (the rows on the wire) ------------------------------------------------------
set.seed(seed)
sample_df = tbl(con, "flights") %>%
  select(month, day, carrier, flight, origin, dest, dep_delay) %>%
  collect() %>%
  slice_sample(n = n_sample) %>%
  arrange(month, day, carrier, flight)
dir.create(dirname(file.path(root, sample_out)), recursive = TRUE, showWarnings = FALSE)
write_csv(sample_df, file.path(root, sample_out), na = "")

# ---- 3. STATES -----------------------------------------------------------------------------
states = list(
  baseline    = list(collect = "after_summarise", creds = "env"),
  after_filter = list(collect = "after_filter",   creds = "env"),
  first_line  = list(collect = "first",            creds = "env"),
  hard_coded  = list(collect = "after_summarise", creds = "code")
)

# ---- 4. PANEL CODE -------------------------------------------------------------------------
connect_r = function(s) {
  if (s$creds == "env") paste0(
    "readRenviron(\".env\")\n",
    "con = dbConnect(RPostgres::Postgres(),\n",
    "  host = Sys.getenv(\"SUPABASE_HOST\"), port = Sys.getenv(\"SUPABASE_PORT\"),\n",
    "  dbname = Sys.getenv(\"SUPABASE_DB\"), user = Sys.getenv(\"SUPABASE_USER\"),\n",
    "  password = Sys.getenv(\"SUPABASE_PASSWORD\"))\n"
  ) else paste0(
    "con = dbConnect(RPostgres::Postgres(),\n",
    "  host = \"db.abcdefgh.supabase.co\", port = 5432,\n",
    "  dbname = \"postgres\", user = \"postgres\",\n",
    "  password = \"hunter2-SuperSecret!\")\n"
  )
}
pipeline_r = function(s) {
  c1 = if (s$collect == "first") " %>%\n  collect()" else ""
  c2 = if (s$collect == "after_filter") " %>%\n  collect()" else ""
  c3 = if (s$collect == "after_summarise") " %>%\n  collect()" else ""
  paste0(
    "result = tbl(con, \"flights\")", c1, " %>%\n",
    "  filter(origin == \"JFK\", dest == \"BOS\")", c2, " %>%\n",
    "  group_by(carrier) %>%\n",
    "  summarize(flights = n(), mean_delay = mean(dep_delay, na.rm = TRUE))", c3
  )
}
code_r = function(s) {
  paste0(
    "library(DBI)\nlibrary(dplyr)\nlibrary(dbplyr)\n\n",
    connect_r(s),
    "dbListTables(con)\n\n",
    pipeline_r(s), "\n\n",
    "dbDisconnect(con)"
  )
}

# ---- 5. RUN + CHECK EACH STATE -------------------------------------------------------------
# The pipeline text is evaluated for real; collect() is wrapped only to record the SQL it sent
# and how many rows and columns crossed the connection.
run_state = function(s) {
  env = new.env(parent = globalenv())
  env$con = con
  env$sent = NULL
  env$collect = function(x, ...) {
    env$sent = as.character(sql_render(x))
    out = dplyr::collect(x, ...)
    env$rows_moved = nrow(out); env$cols_moved = ncol(out)
    out
  }
  eval(parse(text = pipeline_r(s)), envir = env)
  bytes = env$rows_moved * env$cols_moved * bytes_per_value
  list(
    rows_moved   = env$rows_moved,
    cols_moved   = env$cols_moved,
    transfer_ms  = round(bytes / wire_bytes_per_s * 1000, 2),
    result_rows  = nrow(env$result),
    secret_in_code = if (s$creds == "code") 1 else 0,
    sql = env$sent,
    result = env$result
  )
}
runs = map(states, run_state)
positions = c("first", "after_filter", "after_summarise")
by_pos = map(set_names(positions), function(p) run_state(list(collect = p, creds = "env")))
dbDisconnect(con)

readout_keys = c("rows_moved", "cols_moved", "transfer_ms", "result_rows", "secret_in_code")
golden_states = imap(states, function(s, name) {
  r = runs[[name]]
  list(name = name, state = s,
       readouts = c(r[readout_keys], list(table_rows = table_rows)),
       code = list(r = code_r(s), sql = r$sql))
})

# ---- 6. WRITE PAGE DATA + GOLDEN -----------------------------------------------------------
page_data = list(
  table_rows = table_rows, table_cols = table_cols,
  bytes_per_value = bytes_per_value, wire_bytes_per_s = wire_bytes_per_s,
  positions = map(by_pos, function(r) list(
    rows_moved = r$rows_moved, cols_moved = r$cols_moved, transfer_ms = r$transfer_ms,
    sql = r$sql, result = r$result %>% arrange(desc(flights)) %>% mutate(mean_delay = round(mean_delay, 2))
  ))
)
write_json(page_data, file.path(root, data_out), auto_unbox = TRUE, digits = NA, pretty = TRUE)
total = file.size(file.path(root, data_out)) + file.size(file.path(root, sample_out))
stopifnot(total <= 500 * 1024)

golden = list(
  lab = lab_id, page = page, generated_by = "tools/labs/prep/v1b.R", tolerance = 1e-6,
  states = unname(golden_states), ack = ack
)
dir.create(dirname(file.path(root, golden_out)), recursive = TRUE, showWarnings = FALSE)
write_json(golden, file.path(root, golden_out), auto_unbox = TRUE, digits = NA, pretty = TRUE)
message("prep: wrote ", data_out, " + ", sample_out, " (", total, " B) and ", golden_out)

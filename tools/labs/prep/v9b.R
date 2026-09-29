# v9b.R - prep for the lab "Indexes, partitions and pre-aggregation for big bundles".
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v9b.R
# It writes:
#   docs-v3/labs/data/v9b.json       table sizes the page's scan model needs (a few KB)
#   tests/labs/golden/v9b.json       named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
#
# Source: the bluebikes SQLite (data/bluebikes/bluebikes.zip), table tally_rush_edges, the
# 5M+ station-pair table the datacom workshop queries. The zip is unpacked into a temp dir,
# never into data/.
#
# The duckdb package is not installed on the build machine, so no Postgres or DuckDB engine is
# run here. The row counts ARE real (counted in SQLite); the pages and bytes each engine reads
# are computed from those counts with the storage model below, and the page runs the same model.

library(dplyr)
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
lab_id     = "v9b"
page       = "docs-v3/labs/v9b-indexes-and-bundles.html"
source     = "data/bluebikes/bluebikes.zip"
data_out   = paste0("docs-v3/labs/data/", lab_id, ".json")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# Storage model (Postgres defaults; DuckDB row groups and column widths).
page_bytes   = 8192     # one Postgres heap page
page_header  = 24
row_bytes    = 60       # tuple header + item pointer + start_code, end_code, day, rush, count
mv_row_bytes = 40       # tuple header + item pointer + start_code, month, trips
index_depth  = 3        # B-tree levels walked before the first heap page
row_group    = 122880   # DuckDB rows per row group
col_bytes    = list(start_code = 8, day = 4, count = 4)   # DuckDB bytes per value, per column

# ---- 2. LOAD: count what each query touches ------------------------------------------------
tmp = file.path(tempdir(), "v9b")
dir.create(tmp, showWarnings = FALSE)
unzip(file.path(root, source), exdir = tmp)
db = dbConnect(SQLite(), file.path(tmp, "our_data", "bluebikes.sqlite"))

n_rows = dbGetQuery(db, "SELECT count(*) AS n FROM tally_rush_edges")$n

by_station = dbGetQuery(db, "SELECT start_code, count(*) AS n, sum(count) AS trips
                             FROM tally_rush_edges GROUP BY start_code") %>%
  arrange(desc(n), start_code)
by_month = dbGetQuery(db, "SELECT substr(day, 1, 7) AS month, count(*) AS n, sum(count) AS trips
                           FROM tally_rush_edges GROUP BY month") %>%
  arrange(month)
# Where the busiest station's rows sit in the heap (rowid order = load order, roughly time):
# 200 buckets, one per storage block on the page, so the index scan lights the right blocks.
n_blocks = 200
station_hist = dbGetQuery(db, paste0("SELECT CAST((rowid - 1) * ", n_blocks, " / ", n_rows, " AS INTEGER) AS b,
                                  count(*) AS n FROM tally_rush_edges
                                  WHERE start_code = '", by_station$start_code[1], "' GROUP BY b")) %>%
  right_join(tibble(b = 0:(n_blocks - 1)), by = "b") %>%
  mutate(n = coalesce(n, 0L)) %>%
  arrange(b) %>%
  pull(n)
mv_rows = dbGetQuery(db, "SELECT count(*) AS n FROM (SELECT DISTINCT start_code, substr(day, 1, 7)
                          FROM tally_rush_edges)")$n
station = by_station$start_code[1]                                  # the busiest start station
month   = by_month %>% arrange(desc(n), month) %>% slice(1) %>% pull(month)   # the busiest month

sizes = list(
  table = "tally_rush_edges", n_rows = n_rows, mv_rows = mv_rows,
  station = station, station_rows = by_station$n[1], station_trips = by_station$trips[1],
  station_hist = station_hist,
  month = month, month_rows = by_month$n[by_month$month == month],
  month_trips = by_month$trips[by_month$month == month],
  total_trips = sum(by_month$trips),
  months = by_month %>% select(month, n),
  page_bytes = page_bytes, page_header = page_header, row_bytes = row_bytes,
  mv_row_bytes = mv_row_bytes, index_depth = index_depth, row_group = row_group,
  col_bytes = col_bytes
)
dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_json(sizes, file.path(root, data_out), auto_unbox = TRUE, digits = NA)
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)
sizes = read_json(file.path(root, data_out), simplifyVector = TRUE)

# ---- 3. STATES -----------------------------------------------------------------------------
st = function(query, index = FALSE, partition = FALSE, mview = FALSE, engine = "postgres")
  list(query = query, index = index, partition = partition, mview = mview, engine = engine)
states = list(
  baseline        = st("station"),
  station_index   = st("station", index = TRUE),
  month_scan      = st("month"),
  month_index     = st("month", index = TRUE),
  month_partition = st("month", partition = TRUE),
  total_postgres  = st("total"),
  total_duckdb    = st("total", engine = "duckdb"),
  total_mview     = st("total", mview = TRUE)
)

# ---- 4. SCAN MODEL (the page's model() is the same function) -------------------------------
model = function(s, z) {
  rpp    = floor((z$page_bytes - z$page_header) / z$row_bytes)
  rpp_mv = floor((z$page_bytes - z$page_header) / z$mv_row_bytes)
  trips  = switch(s$query, station = z$station_trips, month = z$month_trips, total = z$total_trips)
  cols   = switch(s$query, station = c("start_code", "count"), month = c("day", "count"), total = "count")
  if (s$mview) {
    rows = z$mv_rows
    pg_pages = ceiling(rows / rpp_mv)
  } else {
    rows = z$n_rows
    if (s$query == "month" && s$partition) rows = z$month_rows
    if (s$query == "station" && s$index && s$engine == "postgres") rows = z$station_rows
    pg_pages = ceiling(rows / rpp) + if (s$query == "station" && s$index) z$index_depth else 0
  }
  row_store = pg_pages * z$page_bytes
  col_store = rows * sum(unlist(z$col_bytes[cols]))
  list(
    rows_scanned  = rows,
    pages_scanned = if (s$engine == "postgres") pg_pages else ceiling(rows / z$row_group),
    bytes_read    = if (s$engine == "postgres") row_store else col_store,
    bytes_row_store = row_store,
    bytes_col_store = col_store,
    trips = trips
  )
}

# ---- 5. PANEL CODE -------------------------------------------------------------------------
where_of = function(s) switch(s$query,
  station = paste0("start_code = '", sizes$station, "'"),
  month   = paste0("day >= '", sizes$month, "-01' AND day < '", next_month(sizes$month), "-01'"),
  total   = "")
next_month = function(m) {
  y = as.integer(substr(m, 1, 4)); mo = as.integer(substr(m, 6, 7)) + 1
  if (mo == 13) { y = y + 1; mo = 1 }
  sprintf("%04d-%02d", y, mo)
}
query_sql = function(s) {
  from = if (s$mview) "trips_by_station_month" else "tally_rush_edges"
  col  = if (s$mview) "trips" else "count"
  w = where_of(s)
  if (s$mview && s$query == "month") w = paste0("month = '", sizes$month, "'")
  paste0("SELECT sum(", col, ") AS trips\nFROM ", from, if (w != "") paste0("\nWHERE ", w) else "", ";")
}
code_sql = function(s) {
  ddl = character(0)
  if (s$engine == "postgres") {
    if (s$index) ddl = c(ddl, "CREATE INDEX edges_start_idx\n  ON tally_rush_edges (start_code);")
    if (s$partition) ddl = c(ddl, paste0(
      "CREATE TABLE edges_by_month (LIKE tally_rush_edges)\n  PARTITION BY RANGE (day);\n",
      "CREATE TABLE edges_", gsub("-", "_", sizes$month), " PARTITION OF edges_by_month\n",
      "  FOR VALUES FROM ('", sizes$month, "-01') TO ('", next_month(sizes$month), "-01');"))
    if (s$mview) ddl = c(ddl, paste0(
      "CREATE MATERIALIZED VIEW trips_by_station_month AS\n",
      "  SELECT start_code, substr(day, 1, 7) AS month, sum(count) AS trips\n",
      "  FROM tally_rush_edges\n  GROUP BY start_code, month;"))
  }
  q = query_sql(s)
  if (s$partition && !s$mview && s$engine == "postgres") q = sub("tally_rush_edges", "edges_by_month", q)
  paste(c(ddl, paste0(if (s$engine == "postgres") "EXPLAIN ANALYZE\n" else "", q)), collapse = "\n\n")
}
code_r = function(s) {
  filt = switch(s$query,
    station = paste0("  filter(start_code == \"", sizes$station, "\") %>%\n"),
    month   = paste0("  filter(day >= \"", sizes$month, "-01\", day < \"", next_month(sizes$month), "-01\") %>%\n"),
    total   = "")
  con = if (s$engine == "duckdb")
    "library(DBI)\nlibrary(duckdb)\nlibrary(dplyr)\n\ncon = dbConnect(duckdb::duckdb())\n"
  else
    "library(DBI)\nlibrary(RPostgres)\nlibrary(dplyr)\n\ncon = dbConnect(RPostgres::Postgres(), dbname = \"bluebikes\")\n"
  src = if (s$mview) "trips_by_station_month" else if (s$partition && s$engine == "postgres") "edges_by_month" else "tally_rush_edges"
  sumcol = if (s$mview) "trips" else "count"
  if (s$mview && s$query == "month") filt = paste0("  filter(month == \"", sizes$month, "\") %>%\n")
  paste0(con, "\n",
    "con %>%\n  tbl(\"", src, "\") %>%\n", filt,
    "  summarize(trips = sum(", sumcol, ", na.rm = TRUE)) %>%\n  collect()")
}

# ---- 5b. CROSS-CHECK: run each state's panel SQL on the SQLite copy -------------------------
# The panel's query text is executed here (EXPLAIN ANALYZE dropped; the materialised view is
# built as a plain table, and the partitioned parent is a view over the whole table). The count
# of rows the access path touches must equal the model's rows_scanned, and the sum must equal
# the model's trips. Pages and bytes stay modelled.
mv_ddl = sub("CREATE MATERIALIZED VIEW", "CREATE TABLE", strsplit(code_sql(states$total_mview), "\n\n")[[1]][1])
invisible(dbExecute(db, mv_ddl))
invisible(dbExecute(db, "CREATE VIEW edges_by_month AS SELECT * FROM tally_rush_edges"))
iwalk(states, function(s, name) {
  q = sub("^EXPLAIN ANALYZE\n", "", tail(strsplit(code_sql(s), "\n\n")[[1]], 1))
  m = model(s, sizes)
  pruned = !s$mview && ((s$query == "station" && s$index && s$engine == "postgres") || (s$query == "month" && s$partition))
  count_q = if (pruned) sub("sum\\([a-z]+\\) AS trips", "count(*) AS n", q) else sub("SELECT sum\\([a-z]+\\) AS trips\n(FROM [a-z_]+).*$", "SELECT count(*) AS n \\1;", q)
  got_rows  = dbGetQuery(db, count_q)$n
  got_trips = dbGetQuery(db, q)$trips
  if (got_rows != m$rows_scanned || got_trips != m$trips)
    stop("prep cross-check FAILED for state ", name, ": SQLite rows ", got_rows, " vs model ", m$rows_scanned,
         "; trips ", got_trips, " vs model ", m$trips)
  message("prep cross-check ok: ", name, " rows ", got_rows, " trips ", got_trips)
})
dbDisconnect(db)
unlink(tmp, recursive = TRUE)

# ---- 6. RUN EACH STATE + WRITE GOLDEN ------------------------------------------------------
golden_states = imap(states, function(s, name) {
  list(name = name, state = s, readouts = model(s, sizes), code = list(r = code_r(s), sql = code_sql(s)))
})
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

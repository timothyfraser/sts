# v5a.R - prep for the lab "Normalise it" (chapter 5: relational design).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v5a.R
# It writes:
#   docs-v3/labs/data/v5a.json        the four base tables and the wide (denormalised) join
#   tests/labs/golden/v5a.json        named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).

library(dplyr)
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
lab_id     = "v5a"
page       = "docs-v3/labs/v5a-normalize-it.html"
seed       = 5460
n_sample   = 40
data_out   = paste0("docs-v3/labs/data/", lab_id, ".json")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + SAMPLE ----------------------------------------------------------------------
# A seeded sample of flights, kept only where every key has a match, so the four base tables
# are a valid relational database before the student touches anything.
planes_all   = read_csv(file.path(root, "data/planes.csv"), show_col_types = FALSE)
airports_all = read_csv(file.path(root, "data/airports.csv"), show_col_types = FALSE)
airlines_all = read_csv(file.path(root, "data/airlines.csv"), show_col_types = FALSE)

set.seed(seed)
flights = read_csv(file.path(root, "data/flights.csv"), show_col_types = FALSE) %>%
  filter(!is.na(dep_delay), tailnum %in% planes_all$tailnum, dest %in% airports_all$faa) %>%
  slice_sample(n = n_sample) %>%
  arrange(time_hour, carrier, flight) %>%
  mutate(id = sprintf("f%02d", row_number()), dep_delay = as.integer(dep_delay)) %>%
  select(id, carrier, tailnum, origin, dest, dep_delay)

airlines = airlines_all %>% filter(carrier %in% flights$carrier) %>% arrange(carrier)
airports = airports_all %>% filter(faa %in% c(flights$origin, flights$dest)) %>% select(faa, name) %>% arrange(faa)
planes   = planes_all %>% filter(tailnum %in% flights$tailnum) %>%
  mutate(seats = as.integer(seats)) %>% select(tailnum, manufacturer, model, seats) %>% arrange(tailnum)

# The wide table: the join a dashboard would use, stored as ONE table.
flights_wide = flights %>%
  left_join(airlines %>% rename(airline = name), by = "carrier") %>%
  left_join(planes, by = "tailnum") %>%
  left_join(airports %>% rename(origin_name = name), by = c("origin" = "faa")) %>%
  left_join(airports %>% rename(dest_name = name), by = c("dest" = "faa")) %>%
  select(id, carrier, airline, tailnum, manufacturer, model, seats, origin, origin_name, dest, dest_name, dep_delay)

# The edit the lab makes: rename the airline that appears most often in the sample.
top = flights %>% count(carrier, sort = TRUE) %>% slice(1)
stopifnot(top$carrier == "EV")
rename_carrier = "EV"
rename_from    = airlines$name[airlines$carrier == rename_carrier]
rename_to      = "ExpressJet Airlines LLC"
new_flight = list(id = "f41", carrier = "ZZ", tailnum = flights$tailnum[1], origin = "JFK", dest = "LAX", dep_delay = 0L)

lab_data = list(
  n_source = nrow(read_csv(file.path(root, "data/flights.csv"), show_col_types = FALSE)),
  rename = list(carrier = rename_carrier, from = rename_from, to = rename_to),
  new_flight = new_flight,
  airlines = airlines, airports = airports, planes = planes, flights = flights,
  flights_wide = flights_wide
)
dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_json(lab_data, file.path(root, data_out), auto_unbox = TRUE, digits = NA, dataframe = "rows")
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

# Re-read the file the page fetches, so R computes on exactly the bytes the browser sees.
d = read_json(file.path(root, data_out), simplifyVector = TRUE)
airlines = as_tibble(d$airlines); airports = as_tibble(d$airports)
planes = as_tibble(d$planes); flights = as_tibble(d$flights); flights_wide = as_tibble(d$flights_wide)

# ---- 3. STATES -----------------------------------------------------------------------------
# view: "normalized" (four tables, keys) or "denormalized" (one wide table).
states = list(
  baseline     = list(view = "denormalized", rename = FALSE, add = FALSE, fk = FALSE),
  renamed_wide = list(view = "denormalized", rename = TRUE,  add = FALSE, fk = FALSE),
  normalized   = list(view = "normalized",   rename = FALSE, add = FALSE, fk = FALSE),
  renamed_norm = list(view = "normalized",   rename = TRUE,  add = FALSE, fk = FALSE),
  orphan       = list(view = "normalized",   rename = FALSE, add = TRUE,  fk = FALSE),
  fk_reject    = list(view = "normalized",   rename = FALSE, add = TRUE,  fk = TRUE),
  wide_add     = list(view = "denormalized", rename = FALSE, add = TRUE,  fk = TRUE)
)

# ---- 4. PANEL CODE -------------------------------------------------------------------------
q = function(x) paste0("'", x, "'")
nf = new_flight
ins_cols = "(id, carrier, tailnum, origin, dest, dep_delay)"
ins_vals = paste0("(", paste(q(nf$id), q(nf$carrier), q(nf$tailnum), q(nf$origin), q(nf$dest), nf$dep_delay, sep = ", "), ")")

code_r = function(s) {
  wide = s$view == "denormalized"
  tbl_name = if (wide) "flights_wide" else "flights"
  col = if (wide) "airline" else "name"
  target = if (wide) "flights_wide" else "airlines"
  paste0(
    "db = dbConnect(RSQLite::SQLite(), \":memory:\")\n",
    "dbExecute(db, \"PRAGMA foreign_keys = ", if (s$fk) "ON" else "OFF", "\")\n",
    if (wide) paste0(
      "dbWriteTable(db, \"flights_wide\", flights_wide)\n"
    ) else paste0(
      "dbExecute(db, \"CREATE TABLE airlines (carrier TEXT PRIMARY KEY, name TEXT)\")\n",
      "dbExecute(db, \"CREATE TABLE airports (faa TEXT PRIMARY KEY, name TEXT)\")\n",
      "dbExecute(db, \"CREATE TABLE planes (tailnum TEXT PRIMARY KEY, manufacturer TEXT, model TEXT, seats INTEGER)\")\n",
      "dbExecute(db, \"CREATE TABLE flights (id TEXT PRIMARY KEY,\n",
      "  carrier TEXT REFERENCES airlines (carrier), tailnum TEXT REFERENCES planes (tailnum),\n",
      "  origin TEXT REFERENCES airports (faa), dest TEXT REFERENCES airports (faa), dep_delay INTEGER)\")\n",
      "dbWriteTable(db, \"airlines\", airlines, append = TRUE)\n",
      "dbWriteTable(db, \"airports\", airports, append = TRUE)\n",
      "dbWriteTable(db, \"planes\", planes, append = TRUE)\n",
      "dbWriteTable(db, \"flights\", flights, append = TRUE)\n"
    ),
    "\n",
    "changed = ", if (s$rename) paste0(
      "dbExecute(db, \"UPDATE ", target, " SET ", col, " = ", q(rename_to), " WHERE carrier = ", q(rename_carrier), "\")"
    ) else "0", "\n",
    "added = ", if (s$add) paste0(
      "tryCatch(dbExecute(db, \"INSERT INTO ", tbl_name, " ", ins_cols, " VALUES ", ins_vals, "\"),\n",
      "  error = function(e) -1)"
    ) else "0", "\n",
    "\n",
    if (wide) "wide = tbl(db, \"flights_wide\") %>% collect()" else paste0(
      "# rebuild the wide table for a dashboard\n",
      "wide = tbl(db, \"flights\") %>%\n",
      "  left_join(tbl(db, \"airlines\") %>% rename(airline = name), by = \"carrier\") %>%\n",
      "  left_join(tbl(db, \"planes\"), by = \"tailnum\") %>%\n",
      "  collect()"
    )
  )
}
code_sql = function(s) {
  wide = s$view == "denormalized"
  paste0(
    "-- Postgres enforces REFERENCES always; SQLite only with PRAGMA foreign_keys = ON\n",
    if (wide) paste0(
      "CREATE TABLE flights_wide (id TEXT, carrier TEXT, airline TEXT, tailnum TEXT,\n",
      "  manufacturer TEXT, model TEXT, seats INTEGER, origin TEXT, origin_name TEXT,\n",
      "  dest TEXT, dest_name TEXT, dep_delay INTEGER);\n"
    ) else paste0(
      "CREATE TABLE airlines (carrier TEXT PRIMARY KEY, name TEXT);\n",
      "CREATE TABLE flights (\n",
      "  id TEXT PRIMARY KEY,\n",
      "  carrier TEXT REFERENCES airlines (carrier),\n",
      "  tailnum TEXT REFERENCES planes (tailnum),\n",
      "  origin TEXT REFERENCES airports (faa),\n",
      "  dest TEXT REFERENCES airports (faa),\n",
      "  dep_delay INTEGER);\n"
    ),
    if (s$rename) paste0("\nUPDATE ", if (wide) "flights_wide SET airline" else "airlines SET name",
                         " = ", q(rename_to), " WHERE carrier = ", q(rename_carrier), ";\n") else "",
    if (s$add) paste0("\nINSERT INTO ", if (wide) "flights_wide" else "flights", " ", ins_cols, "\nVALUES ", ins_vals, ";\n") else "",
    if (wide) "" else paste0(
      "\n-- the join that rebuilds the wide table\n",
      "SELECT f.*, a.name AS airline, p.manufacturer, p.model, p.seats\n",
      "FROM flights f\n",
      "LEFT JOIN airlines a ON a.carrier = f.carrier\n",
      "LEFT JOIN planes p ON p.tailnum = f.tailnum;\n"
    )
  )
}

# ---- 5. RUN + CHECK EACH STATE -------------------------------------------------------------
cell_bytes = function(df) sum(vapply(df, function(v) sum(nchar(ifelse(is.na(v), "", as.character(v)), type = "bytes")), numeric(1)))

golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$airlines = airlines; env$airports = airports; env$planes = planes
  env$flights = flights; env$flights_wide = flights_wide
  eval(parse(text = code_r(s)), envir = env)                  # the panel's R, run for real
  db = env$db
  tabs = if (s$view == "denormalized") "flights_wide" else c("airlines", "airports", "planes", "flights")
  dfs = map(tabs, function(t) dbReadTable(db, t))
  main = dfs[[length(dfs)]]
  orphans = if (s$view == "denormalized") sum(is.na(main$airline)) else
    dbGetQuery(db, "SELECT COUNT(*) AS n FROM flights f LEFT JOIN airlines a ON a.carrier = f.carrier WHERE a.carrier IS NULL")$n
  dbDisconnect(db)
  readouts = list(
    tables  = length(tabs),
    rows    = nrow(main),
    cells   = sum(map_dbl(dfs, function(x) nrow(x) * ncol(x))),
    bytes   = sum(map_dbl(dfs, cell_bytes)),
    changed = env$changed,
    orphans = orphans,
    insert  = if (!s$add) "none" else if (env$added == 1) "inserted" else "rejected"
  )
  list(name = name, state = s, readouts = readouts, code = list(r = code_r(s), sql = code_sql(s)))
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

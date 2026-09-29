# _template.R - the lab prep template AND the reference lab's prep script.
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/_template.R
# It writes:
#   docs-v3/labs/data/_reference.csv     the page's data (seeded sample, <= 500 KB)
#   tests/labs/golden/_reference.json    named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
#
# To build a lab: copy this file to tools/labs/prep/<id>.R and change ONLY the blocks marked
# [CHANGE]. Blocks marked [KEEP] are the machinery that keeps the page, the panel code and the
# golden values from drifting apart. See tools/labs/README.md "How to build a lab".

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(DBI)
library(RSQLite)

# ---- 0. REPO ROOT [KEEP] -------------------------------------------------------------------
# Walk up from the working directory to the folder that holds data/ and docs-v3/.
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
lab_id    = "_reference"                                  # golden id; also the data file stem
page      = "docs-v3/labs/_reference.html"                # the page this golden checks
source    = "data/weather.csv"                            # course data, repo-relative
seed      = 5460                                          # never change after first publish
n_sample  = 240                                           # rows kept for the page
data_out  = paste0("docs-v3/labs/data/", lab_id, ".csv")  # <= 500 KB in total per lab
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()                                              # e.g. list(list(rule = "gradient-text", reason = "..."))

# ---- 2. LOAD + SAMPLE [CHANGE the pipeline, KEEP set.seed + write + re-read] ---------------
set.seed(seed)
sample_df = read_csv(file.path(root, source), show_col_types = FALSE) %>%
  filter(!is.na(temp), !is.na(wind_speed)) %>%
  slice_sample(n = n_sample) %>%
  arrange(time_hour) %>%
  mutate(id = sprintf("w%03d", row_number())) %>%
  select(id, origin, month, day, hour, temp, wind_speed)

dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_csv(sample_df, file.path(root, data_out), na = "")
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

# Re-read the file the page will fetch, so R computes on exactly the bytes the browser sees.
# The panel code calls this data `weather`; name it whatever your panel code names it.
weather = read_csv(file.path(root, data_out), show_col_types = FALSE)

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
# Same names and fields as `states` in the page's Lab.create(...). baseline comes first.
states = list(
  baseline = list(min_temp = 0,  sort = "time"),
  warm     = list(min_temp = 70, sort = "time"),
  sorted   = list(min_temp = 0,  sort = "temp")
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
# The EXACT text the page's code panel shows for a state. The page's code.r / code.sql
# functions must build the same strings. The R text is evaluated below to get the readouts,
# so the numbers can never disagree with the code a student copies.
code_r = function(s) {
  paste0(
    "kept = weather %>%\n",
    "  filter(temp >= ", s$min_temp, ")",
    if (s$sort == "temp") " %>%\n  arrange(desc(temp))" else "",
    "\n\n",
    "kept %>%\n",
    "  summarize(rows_out = n(), mean_temp = mean(temp), mean_wind = mean(wind_speed))"
  )
}
code_sql = function(s) {   # set to NULL if the lab has no SQL twin
  paste0(
    "SELECT * FROM weather\n",
    "WHERE temp >= ", s$min_temp,
    if (s$sort == "temp") "\nORDER BY temp DESC" else "",
    ";"
  )
}

# The readouts the gate compares: a flat named list of numbers/strings with the SAME keys as
# the page's readouts(). Here the summarize() row IS the readouts.
readouts_from = function(result) {
  as.list(result)
}

# ---- 5. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
con = dbConnect(SQLite(), ":memory:")
dbWriteTable(con, "weather", as.data.frame(weather))

golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$weather = weather
  result = eval(parse(text = code_r(s)), envir = env)       # the panel's R, run for real
  code = list(r = code_r(s))
  if (!is.null(code_sql)) {
    code$sql = code_sql(s)
    via_sql = dbGetQuery(con, code$sql)                       # the panel's SQL, run for real
    kept = env$kept
    if (nrow(via_sql) != nrow(kept) || !isTRUE(all.equal(via_sql$temp, kept$temp)))
      stop("prep: SQL and R disagree for state '", name, "'")
  }
  list(name = name, state = s, readouts = readouts_from(result), code = code)
})
dbDisconnect(con)

# ---- 6. WRITE GOLDEN [KEEP] ----------------------------------------------------------------
golden = list(
  lab          = lab_id,
  page         = page,
  generated_by = paste0("tools/labs/prep/", if (lab_id == "_reference") "_template" else lab_id, ".R"),
  tolerance    = 1e-6,
  states       = unname(golden_states),
  ack          = ack
)
dir.create(dirname(file.path(root, golden_out)), recursive = TRUE, showWarnings = FALSE)
write_json(golden, file.path(root, golden_out), auto_unbox = TRUE, digits = NA, pretty = TRUE)
message("prep: wrote ", data_out, " (", file.size(file.path(root, data_out)), " B) and ", golden_out)

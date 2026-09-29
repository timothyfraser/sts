# v3c.R - prep for lab v3c: one model per group (betas, iterated).
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
library(broom)
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
lab_id    = "v3c"
page      = "docs-v3/labs/v3c-betas-by-group.html"
source    = "data/jp_solar.csv"
seed      = 5460
data_out  = paste0("docs-v3/labs/data/", lab_id, ".csv")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + REDUCE [CHANGE the pipeline, KEEP set.seed + write + re-read] ---------------
# All 147 municipalities, every month. time = months since the first month in the data.
set.seed(seed)
raw = read_csv(file.path(root, source), show_col_types = FALSE, col_types = cols(muni_code = col_character()))
first = min(raw$date)
sample_df = raw %>%
  mutate(time = (as.integer(format(date, "%Y")) - as.integer(format(first, "%Y"))) * 12 +
                as.integer(format(date, "%m")) - as.integer(format(first, "%m")),
         solar_rate = round(solar_rate, 5)) %>%
  arrange(muni_code, time) %>%
  select(muni_code, time, solar, solar_rate, disaster)

dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_csv(sample_df, file.path(root, data_out), na = "")
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

jp = read_csv(file.path(root, data_out), show_col_types = FALSE, col_types = cols(muni_code = col_character()))

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
# form: "time" or "time_disaster"; min_months: minimum months with new installs per group;
# view / n_show / highlight change the picture, not the model.
states = list(
  baseline = list(form = "time", view = "per_group", min_months = 3, n_show = 24, highlight = "02204"),
  min30    = list(form = "time", view = "per_group", min_months = 30, n_show = 24, highlight = "02204"),
  disaster = list(form = "time_disaster", view = "per_group", min_months = 3, n_show = 24, highlight = "02204"),
  pooled   = list(form = "time", view = "pooled", min_months = 3, n_show = 24, highlight = "02204")
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
code_r = function(s) {
  f = if (s$form == "time") "solar_rate ~ time" else "solar_rate ~ time + disaster"
  keep = paste0(
    "  filter(solar > 0) %>%\n",
    "  group_by(muni_code) %>%\n",
    "  filter(n() >= ", s$min_months, ") %>%\n")
  paste0(
    "# one model per municipality\n",
    "slopes = jp %>%\n", keep,
    "  reframe( lm(formula = ", f, ") %>% tidy() ) %>%\n",
    "  filter(term == \"time\")\n\n",
    "# one pooled model for everyone\n",
    "pooled = jp %>%\n", keep,
    "  ungroup() %>%\n",
    "  lm(formula = ", f, ") %>%\n",
    "  tidy() %>%\n",
    "  filter(term == \"time\")\n\n",
    "slopes %>%\n",
    "  summarize(pooled_slope = pooled$estimate, n_groups = n(),\n",
    "            n_above = sum(estimate > pooled$estimate), slope_sd = sd(estimate))"
  )
}
code_sql = NULL

readouts_from = function(result, env, s) {
  sel = env$slopes %>% filter(muni_code == s$highlight)
  c(as.list(result),
    list(sel_slope = if (nrow(sel)) sel$estimate else "none",
         sel_se    = if (nrow(sel)) sel$std.error else "none"))
}

# ---- 5. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$jp = jp
  result = eval(parse(text = code_r(s)), envir = env)       # the panel's R, run for real
  list(name = name, state = s, readouts = readouts_from(result, env, s), code = list(r = code_r(s)))
})

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

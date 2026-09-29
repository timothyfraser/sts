# v5c.R - prep for lab v5c: simulating quantities of interest from betas.
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v5c.R
# It writes:
#   docs-v3/labs/data/v5c.json      model fits (coef, vcov) and seeded coefficient draws
#   tests/labs/golden/v5c.json      named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
# Built from tools/labs/prep/_template.R: only the [CHANGE] blocks differ.

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
setwd(root)   # the panel code reads data/jp_emissions.csv relative to the repo root

# ---- 1. CONFIG [CHANGE] --------------------------------------------------------------------
lab_id     = "v5c"
page       = "docs-v3/labs/v5c-simulate-qoi.html"
source     = "data/jp_emissions.csv"
seed       = 5460
fracs      = c(1, 0.5, 0.2)          # share of municipalities used to fit the model
n_sims_all = c(100, 250, 1000)       # number of simulated coefficient vectors
data_out   = paste0("docs-v3/labs/data/", lab_id, ".json")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

num = function(x) sprintf("%.0f", x)     # integers written plainly (no 1e+05), same as JS String()
frac_txt = function(f) as.character(f)   # 1, 0.5, 0.2 as JS String() writes them

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
# The EXACT text the page's code panel shows for a state. It is evaluated below for real.
code_r = function(s) {
  xb = function(pop) paste0("b0 + b_pop * log(", num(pop), ") + b_income * log(", num(s$income), ")")
  paste0(
    "emissions = read_csv(\"data/jp_emissions.csv\", show_col_types = FALSE) %>%\n",
    "  filter(year == 2017, emissions > 0, pop > 0)\n",
    "\n",
    if (s$frac < 1) paste0("set.seed(5460)\nfit_data = emissions %>% slice_sample(prop = ", frac_txt(s$frac), ")\n")
    else "fit_data = emissions\n",
    "m = fit_data %>% lm(formula = log(emissions) ~ log(pop) + log(income_per_capita))\n",
    "\n",
    "set.seed(5460)\n",
    "sims = MASS::mvrnorm(n = ", s$n, ", mu = coef(m), Sigma = vcov(m)) %>%\n",
    "  as_tibble() %>%\n",
    "  setNames(c(\"b0\", \"b_pop\", \"b_income\"))\n",
    "\n",
    if (s$qoi == "ev") paste0(
      "qoi = sims %>%\n",
      "  mutate(q = exp(", xb(s$pop), "))\n",
      "\n",
      "analytic = predict(m, newdata = tibble(pop = ", num(s$pop), ", income_per_capita = ", num(s$income), "),\n",
      "  interval = \"confidence\") %>% exp()\n")
    else paste0(
      "qoi = sims %>%\n",
      "  mutate(q = exp(", xb(2 * s$pop), ") -\n",
      "             exp(", xb(s$pop), "))\n",
      "\n",
      "analytic = NULL  # predict() has no interval for a difference of two predictions\n"),
    "\n",
    "qoi %>%\n",
    "  summarize(n_fit = nrow(fit_data), n_sims = n(), sim_mean = mean(q),\n",
    "            sim_lower = quantile(q, 0.025), sim_upper = quantile(q, 0.975))"
  )
}

readouts_from = function(result, env) {
  r = as.list(result)
  r = lapply(r, function(v) unname(as.numeric(v)))
  r$sim_width = r$sim_upper - r$sim_lower
  if (is.null(env$analytic)) {
    r$an_lower = "none"; r$an_upper = "none"; r$an_width = "none"
  } else {
    a = env$analytic
    r$an_lower = unname(a[1, "lwr"]); r$an_upper = unname(a[1, "upr"]); r$an_width = r$an_upper - r$an_lower
  }
  r
}

run_state = function(s) {
  env = new.env(parent = globalenv())
  result = eval(parse(text = code_r(s)), envir = env)
  list(result = result, env = env)
}

# ---- 2. DATA FOR THE PAGE: every fit x every n_sims [CHANGE] --------------------------------
base = list(qoi = "ev", n = 1000, frac = 1, pop = 50000, income = 1200)
fits = map(fracs, function(f) {
  draws = map(n_sims_all, function(n) {
    out = run_state(modifyList(base, list(n = n, frac = f)))
    sims = out$env$sims
    list(b0 = sims$b0, b_pop = sims$b_pop, b_income = sims$b_income)
  })
  names(draws) = num(n_sims_all)
  m = run_state(modifyList(base, list(n = 100, frac = f)))$env$m
  list(frac = f, n_fit = nobs(m), df = m$df.residual, tq = qt(0.975, m$df.residual),
       coef = unname(coef(m)), vcov = unname(split(vcov(m), row(vcov(m)))), draws = draws)
})
names(fits) = frac_txt(fracs)
page_data = list(source = source, year = 2017, fits = fits)
dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_json(page_data, file.path(root, data_out), auto_unbox = TRUE, digits = NA)
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
states = list(
  baseline = base,
  sub20    = modifyList(base, list(frac = 0.2)),
  few      = modifyList(base, list(n = 100)),
  fd       = modifyList(base, list(qoi = "fd"))
)

# ---- 5. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
golden_states = imap(states, function(s, name) {
  out = run_state(s)
  list(name = name, state = s, readouts = readouts_from(out$result, out$env), code = list(r = code_r(s)))
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

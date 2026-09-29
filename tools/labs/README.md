# Labs: how to build one

A lab is one page that makes one data concept move on real course data. The pattern, the
`window.__lab` contract and the golden format are in `specs/LABS.md`; the gate is
`tools/verify/verify_lab.mjs` (checks a-i, `tests/labs/README.md`). This file is the recipe.

The **reference lab** is the worked example, and every lab starts as a copy of it:

| Reference file | What it is |
|---|---|
| `tools/labs/prep/_template.R` | the prep template AND the reference lab's prep script |
| `docs-v3/labs/_reference.html` | the reference page (kit-based, real data) |
| `docs-v3/labs/data/_reference.csv` | its data, written by `_template.R` |
| `tests/labs/golden/_reference.json` | its golden, written by `_template.R` |
| `tests/labs/shots/_reference/` | the gate's shots and reports, written by the gate |

## 1. The files your lab owns

For a lab `<id>` (for example `v1a`) and a short `<slug>`:

| File | Written by |
|---|---|
| `tools/labs/prep/<id>.R` | you (copy of `_template.R`) |
| `docs-v3/labs/<id>-<slug>.html` | you (copy of `_reference.html`) |
| `docs-v3/labs/data/<id>*.csv\|json` | your prep script, never by hand |
| `tests/labs/golden/<id>.json` | your prep script, never by hand (only `ack` comes from the CONFIG block) |
| `tests/labs/shots/<id>/` | the gate; commit it |

Touch nothing else. The kit (`docs-v3/labs/kit/`) and the gate are shared: if they need a
change, report it instead of editing them.

## 2. The prep script: copy `_template.R` to `tools/labs/prep/<id>.R`

The template is in numbered blocks. Each block header says `[CHANGE]` or `[KEEP]`.

| Block | Change? | What you change |
|---|---|---|
| 0. REPO ROOT | KEEP | nothing |
| library() lines | add only | add `library(tidyr)`, `library(sf)`, `library(igraph)` etc. one per line if you need them. Never `library(tidyverse)`. |
| 1. CONFIG | CHANGE | `lab_id` = `"<id>"`, `page` = `"docs-v3/labs/<id>-<slug>.html"`, `source` = your `data/...` file, `n_sample`, `ack`. Keep `seed` once published. `data_out` and `golden_out` follow `lab_id`. |
| 2. LOAD + SAMPLE | CHANGE the pipeline | the `read_csv(...) %>% ... %>% select(...)` pipeline: drop NA rows the page should not see, sample, add a stable `id`, keep only the columns the page uses. Keep `set.seed(seed)`, the `write_csv`, the 500 KB `stopifnot`, and the re-read into the name your panel code uses (`weather` in the reference). |
| 3. STATES | CHANGE | the named states. `baseline` first. Field names and values must equal `states` in the page's `Lab.create`. |
| 4. PANEL CODE | CHANGE | `code_r(s)` and `code_sql(s)` (the exact panel text; set `code_sql = NULL` if there is no SQL twin) and `readouts_from(result)` (turns the value of the last R expression into the flat readouts list, same keys as the page's `readouts()`). |
| 5. RUN + CHECK | KEEP | nothing. It evaluates the panel's R text for every state, runs the SQL text on an in-memory SQLite copy and stops if the kept rows disagree. If your SQL is not a row filter, change only the comparison line inside `if (!is.null(code_sql))`. |
| 6. WRITE GOLDEN | KEEP | nothing (`write_json(..., auto_unbox = TRUE, digits = NA, pretty = TRUE)`). |

The rule that makes the whole thing work: **the R code a student copies is the R code that
computed the golden numbers** (`eval(parse(text = code_r(s)))`). Never compute readouts with
separate R code.

House style: `=` for assignment, `%>%`, packages loaded one by one.

## 3. The page: copy `_reference.html` to `docs-v3/labs/<id>-<slug>.html`

Every part to change carries a `[CHANGE]` comment:

1. `<title>` and `<body data-lab="<id>">`.
2. The header: eyebrow (chapter section), `h1` (the concept), one-line outcome.
3. The sidebar controls (one control per state field), the metric labels, the legend, the table columns.
4. The three learning checks: LC 01 observe (a value read off the baseline), LC 02 intervene
   (change a control), LC 03 transfer (a new case). Exactly 3 options each, exactly one
   `data-correct="true"`, a Hint and a Reveal. Take the numbers in them from your golden.
5. The script: `kept()`/the computation (mirror your R line by line), `id`, `initial`, `states`
   (same as block 3), `load` (fetch `data/<id>...`), `render`, `readouts` (same keys as the
   golden), `metrics.spec`, `statusText`, `code.r` / `code.sql` (build the SAME strings as
   `code_r()` / `code_sql()`), `onState` and the control listeners.

Keep: the `sts.css` + `kit/lab.css` links, D3 7.9.0 from cdnjs, `kit/lab.js`, the bench
structure, `Lab.create` (never hand-roll `window.__lab`). Colours are tokens only
(`var(--y)`, `var(--bench-line)`, ...), never hex. Size SVG viewBoxes to the stage width
(as the reference does) so text stays legible at 390 px.

**Reduced motion:** when `Lab.motion.reduced()` is true, render every mark in its end state with
duration 0 and resolve `render` immediately (the reference sets `ms = 0`). The kit helpers do
this for you; a D3 transition you write yourself must do it too.

## 4. Run order

```bash
Rscript tools/labs/prep/<id>.R            # writes data + golden
git status --porcelain > /tmp/a
Rscript tools/labs/prep/<id>.R            # run it again
git status --porcelain > /tmp/b
diff /tmp/a /tmp/b && git diff --stat -- docs-v3/labs/data tests/labs/golden   # both must be empty
node tools/verify/verify_lab.mjs <id>     # every line PASS, exit 0
```

(First time in a checkout: `cd tools/verify && npm ci && cd ../..`.) Before your files are
tracked, compare `md5sum` of the data and golden between the two runs instead of `git diff`.

## 5. Fit check (Playwright)

The gate writes `tests/labs/shots/<id>/`: `390-light.png`, `390-dark.png`, `1280-light.png`,
`1280-dark.png`, `390-light-reduced.png`, plus `gate.json`, `impeccable.json`, `network.json`.
**Commit this folder.** Open at least `390-light.png` and `1280-dark.png` and check these moments:

- **First paint at 390:** header, visual and the key metric are readable without zooming;
  axis text is not tiny; nothing is cut off; no horizontal scroll.
- **Bench at 1280:** the visual fills its column without giant text; the sidebar is not taller
  than the visual by a screen; the status bar reads `ready` plus your summary.
- **After a state change** (drive it with a Playwright script calling `__lab.setState(...)` and
  screenshot): the moved metric shows its delta; dropped marks read as dropped.
- **Reduced motion** (`390-light-reduced.png`): everything is in its end position.

## 6. Budgets

- Data from `docs-v3/labs/data/`: at most **500 KB** per lab (the prep script stops above it).
- Lab JS (the kit plus your inline script): at most **150 KB**. D3 and motion from the CDNs do not count.

## 7. The `ack` field

If the impeccable detector (check i) flags something that is intentional for your lab, add it
in CONFIG, not by hand in the golden:

```r
ack = list(list(rule = "gradient-text", reason = "the bench title uses the course gradient on purpose"))
```

`rule` is the finding's `antipattern` id from `tests/labs/shots/<id>/impeccable.json`; `reason`
is required by review. Advisory findings never fail the gate and need no ack. Fix the page
before you reach for `ack`.

# Lab gate

`tools/verify/verify_lab.mjs` is the scripted definition of done for every interactive
concept lab under `docs-v3/labs/`. A lab is done when the gate prints no `FAIL` line for it.

## Run it

```bash
cd tools/verify && npm ci && cd ../..          # once (CI also runs: npx playwright install --with-deps chromium)

node tools/verify/verify_lab.mjs v1a           # one lab, by golden id
node tools/verify/verify_lab.mjs --all         # every golden whose page is under docs-v3/labs/
node tools/verify/verify_lab.mjs --self-test   # the fixtures below: proves the gate fails when it should
```

Options: `--golden-dir DIR` (default `tests/labs/golden`), `--site-root DIR` (default `docs-v3`),
`--shots-dir DIR` (default `tests/labs/shots`). `--self-test` defaults these to
`tests/labs/fixtures/golden`, `tests/labs/fixtures/site` and `tests/labs/shots/_fixtures`.
Set `CHROMIUM_PATH` to use a specific Chromium binary.

Exit 0 only if every check of every lab passes. With `--all`, a page under `labs/` whose
`<body>` carries `data-lab` but that no golden file points at also fails.

Output is one line per result:

```
PASS|FAIL <id> <check> <state|shot|-> <expected ... vs got ...>
```

## The checks

| | Check | Fails when |
|---|---|---|
| a | load | any console error or page error; `__lab.ready` missing or rejects; no `performance.mark('lab:first-render')`, or it is at 1500 ms or later |
| b | readouts | for a golden state, `__lab.setState(state)` then `__lab.readouts()` differs from the golden value: numbers by more than `tolerance` (absolute, default 1e-6), strings at all |
| c | code panel | the text of `pre[data-code-pane=<lang>] > code`, or `__lab.code()[lang]`, differs from the golden code (line endings normalised, trailing whitespace trimmed per line) |
| d | learning checks | not exactly three `section.lab-lc[data-lc]`; an LC without exactly 3 options and exactly one `data-correct="true"`; choosing a wrong then the right option does not add `.is-chosen` + `.is-wrong` / `.is-correct` (re-choosing must work); Hint or Reveal does not toggle `hidden` and `aria-expanded` both ways |
| e | screenshots | any of the five shots is not written |
| f | axe | any serious or critical axe-core violation at 390 or 1280, light or dark |
| g | h-scroll | `scrollWidth > clientWidth` at 390, light or dark |
| h | budget | responses from `labs/data/` total more than 500 KB, or lab JS (script responses plus inline `<script>`) more than 150 KB; D3 from cdnjs and motion from jsdelivr do not count |
| i | impeccable | `impeccable@4.1.0 detect --json <page>` reports a finding whose rule (`antipattern`) is not in the golden `ack` list (project ignores in `.impeccable/config.json` are honoured by the tool itself; advisory findings do not count) |

Checks a-d and h run at 1280 light with a network log; e-g reload the page once per shot.

## Golden file: `tests/labs/golden/<id>.json`

Written by the lab's prep script (`tools/labs/prep/<id>.R`), which computes every readout by
running the exact code the panel shows. Do not hand-edit a real lab's golden values.

```json
{
  "lab": "v1a",
  "page": "docs-v3/labs/v1a-query-builder.html",
  "generated_by": "tools/labs/prep/v1a.R",
  "tolerance": 1e-6,
  "states": [
    { "name": "baseline",
      "state": { "limit": 10 },
      "readouts": { "rows_out": 2000, "label": "all rows" },
      "code": { "r": "flights %>% ...", "sql": "SELECT ..." } }
  ],
  "ack": [ { "rule": "gradient-text", "reason": "why this finding is intentional" } ]
}
```

- `page` is repo-relative (`docs-v3/labs/...`) or site-relative (`labs/...`), and may carry a `?query`.
- `states[].name` matches a key of `__lab.states`; `state` is passed to `__lab.setState` as is.
- Only the readout keys listed are compared; extra keys from `__lab.readouts()` are ignored.
- `ack[].rule` is an impeccable rule id (the `antipattern` field of its JSON); `reason` is required by review, not by the gate.
- Fixture goldens also carry `expect_fail` (see below); real goldens do not.

## Shots layout: `tests/labs/shots/<id>/`

| File | What |
|---|---|
| `390-light.png`, `390-dark.png`, `1280-light.png`, `1280-dark.png` | full-page screenshots after `__lab.ready` |
| `390-light-reduced.png` | 390 light with `prefers-reduced-motion: reduce` |
| `impeccable.json` | `{ tool, page, ack, findings[] }`, the raw detector findings plus the ack list applied |
| `network.json` | every response `{ url, type, status, bytes }` the budget check (h) summed |
| `gate.json` | `{ lab, ok, checked_at, results[{ ok, check, state, msg }] }` |

These are build outputs; CI uploads the folder as the `lab-shots` artifact.

## Fixtures: `tests/labs/fixtures/`

A self-contained fixture lab (`site/labs/fx-lab.html`, plain HTML and inline JS that implements
`window.__lab` and the LC DOM by hand, no kit) and one golden per case. `?fault=<name>` breaks
exactly one thing. Each fixture golden's `expect_fail` lists the checks that must fail; the
self-test fails if any other check fails, or if an expected one passes.

| Golden | Page | Must fail |
|---|---|---|
| `fx` | `fx-lab.html` | nothing |
| `fx-readout` | `?fault=readout` (mean off by 1e-3) | b |
| `fx-code` | `?fault=code` (R pane differs from the template) | c |
| `fx-lc` | `?fault=lc` (LC2 Reveal does nothing) | d |
| `fx-payload` | `?fault=payload` (31 x 20 KB from `labs/data/`, about 606 KB) | h |
| `fx-axe` | `?fault=axe` (a button with no name) | f |
| `fx-scroll` | `?fault=scroll` (a 900 px wide block) | g |
| `fx-console` | `?fault=console` | a |
| `fx-slow` | `?fault=slow` (first render after 1.7 s) | a |
| `fx-impeccable` | `fx-slop.html` (gradient text) | i |
| `fx-impeccable-acked` | `fx-slop.html`, `gradient-text` acknowledged | nothing |

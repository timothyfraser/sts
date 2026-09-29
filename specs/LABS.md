# LABS: the interactive concept lab pattern

A lab is one standalone page that makes ONE data concept move on real course data.
A student opens it from a chapter section, touches a control, and sees three things
change together: the visual, the numbers, and the exact code that produced them.
It borrows the shape of the NetSci concept labs and re-tokens it for STS (indigo to
purple bench, yellow for what you touch, light blue for the number that moved).

Every lab is built from the shared kit in `docs-v3/labs/kit/` (`lab.js`, `lab.css`).
`docs-v3/labs/kit/demo.html` uses every component on toy data and is the reference.

## 1. Anatomy (top to bottom)

1. **Header**: eyebrow (chapter section), `h1` naming the concept, one-line learning outcome
   in `p.lab-lo`. The line starts with exactly one lead-in, `Learning objective:` (capital L,
   colon, plain text or wrapped in `<strong>`), then the objective as a sentence:
   `<p class="lab-lo"><strong>Learning objective:</strong> Explain what ...</p>`.
   Not `Learning objective.`, not `Objective:`, not lower case.
2. **Bench** (`section.lab-bench`): a dark indigo-to-purple surface in BOTH themes.
   - **Visual** (left, fluid): the D3 figure (map, chart, table view) in `.lab-stage`.
     The stage is as tall as its figure: the visual column does not stretch to the sidebar's
     height, and the stage reserves 320px only while it is still empty (loading). Give the figure
     its own height (an SVG viewBox, or an explicit height); never size it to 100% of the stage.
   - **Status bar** under the visual: `loading`, `applying`, `ready` + a one-line summary, or `error: ...`.
   - **Sidebar** (right, 340px): Controls, Metrics, Legend, and optionally a linked Rows table.
     Metrics show the raw value in mono tabular numerals plus a **delta against baseline**
     (green up, red down) that flashes briefly when it changes.
   - At 720px and below the bench is one column: sidebar under the visual, metrics in a
     two-column grid so the key number stays near the fold. No horizontal page scroll at 390px.
3. **Code panel** (under the bench): R | SQL tabs and a Copy button. The text is the exact
   code for the current state, regenerated on every change.
4. **Three learning checks** on the ordinary page surface:
   - LC 01 **observe**: read a value off the bench at baseline.
   - LC 02 **intervene**: change a control, predict or explain what moved.
   - LC 03 **transfer**: apply the idea to a new case.
   Each has exactly 3 options (exactly one correct), a Hint, and Reveal answer.
   The LC heading may be `h2` or `h3` (pick whichever keeps the page's heading order); the kit
   styles `.lab-lc h2` and `.lab-lc h3` identically (small mono caps), so every lab looks the same.
5. **Radii** come only from the DESIGN.md `rounded` scale: 6px for controls (select, segmented
   buttons, Copy, LC options and buttons, legend swatches), 8px for cards (LC cards, code panel),
   10px for the bench. No other radius literals in the kit or a lab page.

## 2. Real data, prepared in R

- Each lab has a prep script `tools/labs/prep/<id>.R`. It reads course data, reduces it to
  what the page needs, and writes `docs-v3/labs/data/<id>*.json|csv` (at most 500 KB per lab).
- The same script computes the expected readouts and code for every named state and writes the
  golden file `tests/labs/golden/<id>.json` with
  `jsonlite::write_json(..., auto_unbox = TRUE, digits = NA, pretty = TRUE)` so a rerun is byte-identical.
- The page computes its readouts from the data in the browser; the gate checks they match the golden.

## 3. Motion

- D3 v7.9.0 from cdnjs: `https://cdnjs.cloudflare.com/ajax/libs/d3/7.9.0/d3.min.js` (a page `<script>`).
- motion.dev 11.18.2 from jsDelivr ESM: `https://cdn.jsdelivr.net/npm/motion@11.18.2/+esm`,
  imported lazily by the kit and guarded: if it fails, or takes over 3 s, every helper jumps to its end state.
- Motion shows the data operation, nothing else: filtered points shrink out (exits faster than
  entrances), sorted rows slide to their new place (FLIP), a changed metric flashes. Routine
  changes run 150 to 350 ms with a decelerating ease; no bounce, no decorative loops.
- **Reduced motion** (`prefers-reduced-motion: reduce`): no spatial movement, every row and point
  is rendered in its end position, and `setState()` resolves immediately after the render. The
  delta text and status bar still carry the change.

## 4. The `window.__lab` contract

Set by the kit's `Lab.create(...)`; a page never hand-rolls it. The page sets `<body data-lab="<id>">`.

```js
window.__lab = {
  id: 'v1a',
  ready,              // Promise: resolves after data load + first render
  states,             // { <name>: <stateObject>, ... } the named states (same names as golden)
  setState(obj),      // Promise: applies controls, resolves when the DOM has settled (motion finished or skipped)
  getState(),         // current state object (JSON-serialisable)
  readouts(),         // flat object { key: number|string } of the sidebar metrics, raw (unformatted) values
  code(),             // { r: string, sql?: string } exact code-panel text for the current state
}
```

- The kit calls `performance.mark('lab:first-render')` once, after the first render.
- Failures never hang the contract: if D3 or the data does not load, or a render throws, the
  message goes to the status bar, `__lab.error` is set, and `ready` / `setState` still resolve
  (with `{ error }`). The visual keeps its last good render.

### Golden file `tests/labs/golden/<id>.json`

```json
{
  "lab": "v1a",
  "page": "docs-v3/labs/v1a-query-builder.html",
  "generated_by": "tools/labs/prep/v1a.R",
  "tolerance": 1e-6,
  "states": [
    { "name": "baseline", "state": { }, "readouts": { "rows_out": 2000 }, "code": { "r": "...", "sql": "..." } }
  ],
  "ack": [ { "rule": "<impeccable rule id>", "reason": "why this finding is intentional" } ]
}
```

- Numeric readouts compare with `tolerance` (absolute); strings compare exactly.
- `code.r` / `code.sql` compare exactly after normalising line endings and trimming trailing whitespace per line.
- `ack` lists impeccable detector findings that are intentional for this lab.

### Required DOM

Code panel: `div.lab-code[data-lab-code]` with tabs `button[data-code-tab="r"|"sql"]`, panes
`pre[data-code-pane="r"|"sql"] > code` (pane text equals `__lab.code()[lang]`), and `button[data-code-copy]`.
The kit builds this for you.

Learning checks (exactly three, written in the page):

```html
<section class="lab-lc" data-lc="1">            <!-- 1 observe, 2 intervene, 3 transfer -->
  <h3>LC 01 · observe</h3><p class="lc-q">...</p>
  <div class="lc-options" role="group" aria-label="LC 01 options">
    <button class="lc-option" data-correct="false">...</button>
    <button class="lc-option" data-correct="true">...</button>
    <button class="lc-option" data-correct="false">...</button>
  </div>
  <button class="lc-hint-btn" aria-expanded="false">Hint</button>
  <div class="lc-hint" hidden>...</div>
  <button class="lc-reveal-btn" aria-expanded="false">Reveal answer</button>
  <div class="lc-answer" hidden>...</div>
</section>
```

Choosing an option adds `.is-chosen` and `.is-correct` / `.is-wrong`; Hint and Reveal toggle
`hidden` and `aria-expanded`.

## 5. Kit API (`window.Lab`)

| Call | Does |
|---|---|
| `Lab.create(cfg)` | Wires everything and sets `window.__lab`. `cfg = { id, states, initial, load?, render(state, data), readouts(state, data), code: { r(state), sql?(state) }, metrics?: { el, spec }, status?, codeEl?, statusText?(state, readouts), onState?(state), needsD3? }` |
| `Lab.metrics(el, spec)` | Sidebar metric list; `spec = [{ key, label, fmt? }]`; deltas are against the first values shown |
| `Lab.status(el)` | Live-region status bar (`role="status"`) |
| `Lab.codePanel(el, { r, sql })` | The R/SQL panel; templates are functions of state |
| `Lab.table(el, rows, cols, opts)` | Linked table in a labelled, keyboard-scrollable region; `update(rows)` animates reorders |
| `Lab.map(el, geojson, opts)` | D3 Mercator map; `render({ points, highlight })` resolves when transitions end |
| `Lab.lcs(root)` | Wires the learning checks (called by `create`) |
| `Lab.motion` | `{ reduced(), load(), animate(el, keyframes, opts), flip(els, mutate) }`; all resolve in the end state |
| `Lab.fmt(v, digits)` | Number formatting for display only (readouts stay raw) |

Minimal page script:

```js
var tb = Lab.table(document.getElementById('table'), [], [{ key: 'id', label: 'id' }, { key: 'value', label: 'value', num: true }]);
Lab.create({
  id: 'v1a', initial: { min: 0 },
  states: { baseline: { min: 0 }, min50: { min: 50 } },
  load: function () { return fetch('data/v1a.json').then(function (r) { return r.json(); }); },
  render: function (s, d) { return tb.update(d.filter(function (r) { return r.value >= s.min; })); },
  readouts: function (s, d) { return { rows_out: d.filter(function (r) { return r.value >= s.min; }).length }; },
  metrics: { el: document.getElementById('metrics'), spec: [{ key: 'rows_out', label: 'rows kept', fmt: 0 }] },
  status: document.getElementById('status'), codeEl: document.getElementById('code'),
  code: { r: function (s) { return 'data %>%\n  filter(value >= ' + s.min + ')'; },
          sql: function (s) { return 'SELECT * FROM data\nWHERE value >= ' + s.min + ';'; } }
});
```

Controls call `lab.setState({...})`; `onState` syncs the controls when the harness sets a state.

## 6. Tokens

The kit uses `sts.css` tokens (`--y`, `--b`, `--indigo`, `--plum`, `--mono`, `--sans`, `--ink`, `--line`, ...)
and adds bench tokens, because the bench is dark in both themes and cannot follow `--ink`:

`--bench-bg` (indigo to plum gradient), `--bench-panel`, `--bench-line`, `--bench-ink`,
`--bench-muted`, `--bench-up`, `--bench-down`. Pages use tokens only; no hex colours in a lab page.

## 7. Files per lab `<id>`

| File | What |
|---|---|
| `docs-v3/labs/<id>-<slug>.html` | the page (loads `labs/kit/*`) |
| `docs-v3/labs/data/<id>*.json\|csv` | prepared data, at most 500 KB per lab |
| `tools/labs/prep/<id>.R` | writes the data and the golden |
| `tests/labs/golden/<id>.json` | named states, readouts, code, acks |
| `tests/labs/shots/<id>/` | gate output: `390-light.png`, `390-dark.png`, `1280-light.png`, `1280-dark.png`, `390-light-reduced.png`, `impeccable.json`, `gate.json` |

## 8. Done for a lab

axe: 0 serious or critical at 390 and 1280 in light and dark; every named state matches the
golden; code pane text equals `__lab.code()`; reduced motion ends in place; the impeccable
detector has no unacknowledged findings; kit JS stays under 60 KB.

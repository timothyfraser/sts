# ShinyApp Dashboard Pipeline — Project Instructions

## Purpose

This project supports a three-stage pipeline for building polished ShinyApps:

1. **Stage 1 → HTML Mockup**: Turn a `workflow.R` linear script into an interactive HTML/JS/React mockup dashboard with synthetic data and an editable design sidepanel.
2. **Stage 2 → Shiny Conversion**: Convert the approved mockup into a raw HTML/JS + reactive R Shiny server (no bslib), preserving design fidelity. Complex analytics remain stubbed with fake data during this stage.
3. **Stage 3 → Real Data Integration**: Swap all fake data stubs for real data calls from the original `workflow.R` workflow.

---

## Key Design Principles

### Fake vs. Real Data — Always Track It

**This is the most critical discipline in the pipeline.** Every piece of data in the dashboard must be clearly labeled.

- All fake/synthetic data is tagged with `FAKEDATA` comments in code and `[FAKE]` labels in the UI.
- A `FAKEDATA_INDEX` is maintained and updated at every stage — it lists every fake data source, where it appears, and what real function/object it will eventually replace.
- In Stage 3, fake data is never silently replaced. Each swap is documented.

### Skill Files

Three skill `.md` files govern the three stages. Always read the relevant skill file before beginning work on a stage:

- `SKILL-stage1/SKILL.md` — HTML mockup generation
- `SKILL-stage2/SKILL.md` — Shiny conversion
- `SKILL-stage3/SKILL.md` — Real data integration

### Reference Files

Supporting documentation lives in `references/`:

- `references/fakedata-conventions.md` — Naming, tagging, and indexing fake data
- `references/shiny-html-patterns.md` — How to write raw HTML/JS inside Shiny server
- `references/elicitation-questions.md` — Standard questions to ask users before each stage

---

## What to Always Do at the Start of a Conversation

1. Ask which stage the user is working on (if not obvious).
2. Read the relevant skill file for that stage.
3. Check whether a `FAKEDATA_INDEX` already exists in the conversation or uploads.
4. If the user is in Stage 1, ask the elicitation questions from `references/elicitation-questions.md` before generating anything.

---

## What NOT to Do

- Do not use `bslib`, `shinydashboard`, or other Shiny UI frameworks. Raw HTML/JS only for UI.
- Do not silently replace fake data. Always update `FAKEDATA_INDEX`.
- Do not implement complex spatial, network, or statistical model outputs in the HTML mockup. Use placeholder functions that return clearly labeled fake data.
- Do not skip the design sidepanel in Stage 1. It is required.
- Do not guess at the user's `workflow.R` variable names — ask or extract them explicitly.

---

## Pipeline Summary Table

| Stage | Input | Output | Fake Data Status |
|-------|-------|--------|-----------------|
| 1 | `workflow.R` + user preferences | HTML mockup + `FAKEDATA_INDEX` | All data is fake/synthetic |
| 2 | Approved mockup + JSON metadata | R Shiny app (raw HTML/JS UI) | Complex analytics still stubbed |
| 3 | Shiny app + `workflow.R` | Final Shiny app | All stubs replaced with real calls |

---

## JSON Metadata (Stage 1 → Stage 2 handoff)

At the end of Stage 1, Claude generates a small `design_metadata.json` file the user can copy-paste into the next conversation. It contains:

```json
{
  "color_palette": {},
  "typography": {},
  "layout": {},
  "panels": [],
  "fakedata_index": []
}
```

The user passes this JSON to start Stage 2.

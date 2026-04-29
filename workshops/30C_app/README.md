# 30C App Workshop

This folder captures an iterative Shiny app build/debug workflow across versions `v0` to `v3`, with Claude producing staged outputs and Cursor used between stages (`v1`, `v2`, `v3`) to debug and implement fixes.

Claude conversation (full lineage for `v0`/`v1`/`v2`/`v3`):
- [Claude chat: staged app build and debugging](https://claude.ai/share/6ee31ec5-730d-4e5f-9998-7df077db14cc)

## Folder Index

### `v0/`
Conceptual origin stage in the Claude chat flow. Note: a `v0/` directory is not currently present in this checked-in folder; use the chat link above for the initial baseline context.

### `v1/`
First working Shiny conversion pass.

Included files:
- `app.R`, `ui.R`, `server.R`
- `fake_data.R`, `helpers.R`, `placeholder_fns.R`
- `styles.css`, `app.js`
- `FAKEDATA_INDEX.md`
- `runme.R`

Teaching focus:
- Stage-2 style Shiny structure with fake data stubs
- Early app startup/path/debug fixes and frontend asset loading adjustments

### `v2/`
Second iteration with additional debugging and app hardening.

Included files:
- `app.R`, `ui.R`, `server.R`
- `fake_data.R`, `helpers.R`, `placeholder_fns.R`
- `styles.css`, `app.js`
- `FAKEDATA_INDEX.md`
- `READ_chat_cursor_debugging.md`
- `runme.R`

Teaching focus:
- Structured debugging workflow in Cursor
- Improved reliability and clearer handoff documentation

### `v3/`
Most complete version in this sequence, including modular functions and bundled committee data.

Included files:
- `app.R`, `ui.R`, `server.R`, `runme.R`
- `R/` (`fake_data.R`, `helpers.R`, `placeholder_fns.R`)
- `functions/` (`coaffiliate.R`, `graph_join_list.R`, `bind_graphs_list.R`, `add_layout.R`)
- `www/` (`styles.css`, `app.js`)
- `data/committees/` (`graph_bipartite.rds`, `committees.csv`, `members.csv`, `edgelist.csv`, `README.md`)
- `FAKEDATA_INDEX.md`
- `READ_claude_response.md`, `READ_cursor_chat_debugging.md`

Teaching focus:
- Stage-3 integration mindset (real data + network workflow)
- Iterative runtime debugging for Shiny/tidygraph/ggraph + frontend rendering issues

### `.claude/`
Local skill/reference pack used to guide students through the staged app pipeline.

## `.claude` Skills and References (Student Index)

### `.claude/PROJECT_INSTRUCTIONS.md`
Master guide for the 3-stage pipeline: purpose, required inputs/outputs, fake-data discipline, and stage-by-stage expectations.

### `.claude/SKILL-stage1/SKILL.md`
How to convert `workflow.R` into an interactive HTML/JS dashboard mockup with synthetic data, design controls, placeholders, and export metadata.

### `.claude/SKILL-stage2/SKILL.md`
How to convert the approved mockup to a Shiny app using raw HTML/JS patterns (not dashboard UI frameworks), while keeping complex outputs stubbed.

### `.claude/SKILL-stage3/SKILL.md`
How to replace fake data and placeholders with real workflow outputs in a controlled, auditable way (no silent swaps).

### `.claude/fakedata-conventions.md`
Rules for fake-data IDs, code tags, `[FAKE]` badges, placeholder IDs, and `FAKEDATA_INDEX` table format used across all stages.

### `.claude/shiny-html-patterns.md`
Practical implementation patterns for high-fidelity Shiny UIs using raw tags/CSS/JS (including Chart.js messaging and common pitfalls).

### `.claude/elicitation-questions.md`
Question sets to gather requirements before Stage 1/2/3 so generated artifacts match audience, layout, interactivity, and deployment constraints.

## Suggested Student Workflow

1. Read `.claude/PROJECT_INSTRUCTIONS.md` first.
2. If you need the full origin story (including `v0`), open the Claude chat link.
3. Start hands-on from `v1/`, then compare your changes against `v2/` and `v3/`.
4. Use the stage skills in order: Stage 1 -> Stage 2 -> Stage 3.
5. Keep `FAKEDATA_INDEX.md` updated as you move from stubbed to real data.

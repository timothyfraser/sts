# Scaffold for SYSEN 5460 (sts)

Generated 2026-09-29 by the new-course scaffolder. Every page is a
placeholder archetype (`<!-- ARCHETYPE: ... -->`, "Placeholder — replace via ledger task").
Course facts render from `contract/site.json` via `data-site-text`; never type one into a page.

## Generated
- `contract/chapters.json`
- `contract/site.json`
- `docs-v3/activities/placeholder.html`
- `docs-v3/assets/contract.js`
- `docs-v3/assets/site-text.js`
- `docs-v3/assets/sts.css`
- `docs-v3/course.html`
- `docs-v3/index.html`
- `docs-v3/modules/01-placeholder.html`
- `docs-v3/modules/02-placeholder.html`
- `docs-v3/modules/03-placeholder.html`
- `docs-v3/oral/challenge.html`
- `docs-v3/projects/brief.html`
- `docs-v3/readings.html`
- `docs-v3/slides/_template.html`
- `docs-v3/workshops/placeholder.html`

Deck template source: sigma docs-v3/slides/_template.html (SYSEN 5300 -> SYSEN 5460, #B31B1B -> #6b21a8).
Residual sigma literals left in the deck template to re-brand by hand: SIGMA, Sigma, sigma.

## Colour tokens
- `--brand`: #6b21a8
- `--brand-dark`: #561a86
- `--brand-tint`: #f3edf8
- `--brand-line`: #dac8e9
- `--ink`: #20112c
- `--muted`: #685478
- `--gray`: #fbf8fc
- `--gray2`: #f6f2fa
- `--line`: #e9def2
- `--code-bg`: #180e20
- `--nav-ink`: #ffffff
- nav ink on brand contrast 8.72:1 (chips need >= 4.5)
- body ink on white contrast 17.83:1 (body needs >= 7)

## Next
1. Fill `contract/site.json`: `external.canvas`, `readings.canvas_url`, `site_base_url`, `term`.
2. Port the sigma runtime verbatim (sigma-shell.js, site-nav.js, release-gate.js, slides.js/.css,
   slide-playground.js, playground-live.js, tools/verify/*, deck_audit.py, bump_cachebust.py,
   ledger.py, tests/*), then retire `assets/site-text.js` in favour of site-nav.js.
3. Parameterize every hard-coded course literal in the ported files (course number, colour,
   site URL, localStorage/global prefixes, ledger forbidden list, firewall test, deck presenter).
4. Replace each placeholder through a ledger task; a page is done when its gates pass.
5. Wire the Posit Connect deploy action for `docs-v3/` and bump `?v=` on every include change.

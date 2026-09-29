# Course site spec (v2027)

The v2027 course website is a static site with no build step. This file is the
contract for what the site is and how a page is judged done.

## 1. Root

- The site root is `docs-v3/`. Everything under it is published as-is to a public
  URL with no login, so everything under it is public.
- Vanilla HTML, CSS and JS only. No framework, no bundler. Every include carries a
  `?v=<token>` cache-bust; bump it whenever the included file changes.

## 2. Identity contract

- Every course fact (number, title, institution, term, colour, book title and
  subtitle, repo, site URL, Canvas links, nav, footer) lives once, in
  `contract/site.json`.
- `docs-v3/assets/contract.js` is generated from `contract/site.json`
  (`window.COURSE_CONTRACT`); never hand-edit it.
- Pages render facts through `data-site-text="<dotted.path>"` and links through
  `data-site-href="<dotted.path>"`. A course fact is never typed into a page.
- `contract/chapters.json` is the page registry (modules, then units).
- A contract field that is still empty is an open fact: the gate reports it as
  `OPEN` and the page keeps its own fallback href.

## 3. Archetypes

Each page opens with `<!-- ARCHETYPE: ... -->` naming its purpose and slots.

| Archetype | File |
|---|---|
| home | `index.html` |
| course (logistics, policy anchors) | `course.html` |
| module | `modules/NN-*.html` |
| workshop tutorial (R \| Python toggle, R first) | `workshops/*.html` |
| deck (lesson / workshop / recitation) | `slides/_template.html` and copies |
| activity (one Canvas submit link) | `activities/*.html` |
| project brief | `projects/*.html` |
| oral challenge (format and preparation only) | `oral/*.html` |
| readings (release-gated links into Canvas) | `readings.html` |

## 4. Tokens

- The course colour is one value in `contract/site.json` and one token set in
  `docs-v3/assets/sts.css` (`--brand*`, neutrals, highlights, dark theme).
- Pages, decks and activities use `var(--*)` only. No hex colour in any page.
  The only files that carry hex values are `assets/sts.css` and the deliberate
  brand copy at the top of `assets/slides.css`, both derived from the one colour.
- Light and dark themes: the OS preference, overridable with
  `<html data-theme="light|dark">`.

## 5. Dates

Dates and deadlines live in Canvas, never typed on the site. Readings are linked
through Canvas and never hosted here.

## 6. Gates

Run from the repo root after `npm ci --prefix tools/verify`. Chromium comes from
the environment (`CHROMIUM_PATH`, default the preinstalled Playwright build).

```bash
node tools/verify/verify_page.mjs --all      # or one or more docs-v3 pages
node tools/verify/verify_visual.mjs --all --widths 390,1280 --themes light,dark
grep -rn "#B31B1B" docs-v3/                  # must print nothing
```

- `verify_page.mjs`: page loads (200), no console or page error, no failed
  same-origin request, ARCHETYPE comment present, every `data-site-text` slot
  rendered, no hex colour or course number typed in the page, non-empty title.
- `verify_visual.mjs`: screenshot per page, width and theme (to
  `tools/verify/shots/`, git-ignored), axe-core with zero serious or critical
  violations, and no horizontal scroll at 390px.

A page is done only when every gate passes.

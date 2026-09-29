---
name: SYSEN 5460 STS
description: Purple, techy, friendly course site for data communication and geospatial systems at scale (variant C dual highlight)
colors:
  purple: "#6B21A8"
  purple-dark: "#3B0764"
  indigo: "#1E1B4B"
  purple-tint: "#F3E8FF"
  purple-line: "#E9D5FF"
  ink: "#14121A"
  muted: "#4B5563"
  white: "#FFFFFF"
  pink: "#FCE7F3"
  pink-line: "#FBCFE8"
  pink-ink: "#831843"
  gray: "#F3F4F6"
  gray-2: "#E5E7EB"
  line: "#D1D5DB"
  code-bg: "#1A1033"
  code-ink: "#EDE9FE"
  code-comment: "#A78BFA"
  highlight-interactive: "#F5D90A"
  highlight-interactive-soft: "#FEF9C3"
  highlight-data: "#38BDF8"
  highlight-data-soft: "#E0F2FE"
typography:
  body:
    fontFamily: "system-ui sans (see --sans)"
    fontSize: "16px"
    lineHeight: 1.6
  h1-site:
    fontFamily: "system-ui sans (see --sans)"
    fontSize: "clamp(28px, 7vw, 44px)"
  code:
    fontFamily: "ui-monospace, SFMono-Regular, Menlo, Consolas, monospace"
  deck-body:
    fontFamily: "Lato (vendored)"
    fontSize: "30px"
rounded:
  control: "6px"
  card: "8px"
spacing:
  page-pad: "16px"
  card-pad: "14px"
components:
  button-primary:
    backgroundColor: "{colors.purple}"
    textColor: "{colors.white}"
    rounded: "{rounded.control}"
    padding: "8px 14px"
  card:
    backgroundColor: "{colors.white}"
    textColor: "{colors.ink}"
    rounded: "{rounded.card}"
    padding: "14px"
  code-block:
    backgroundColor: "{colors.code-bg}"
    textColor: "{colors.code-ink}"
    rounded: "{rounded.control}"
    padding: "12px"
---
# Design System: SYSEN 5460 STS

## Overview

**Creative North Star: "The purple control room"**

A bright, friendly, techy purple course site: white pages and near-black ink, a purple focal colour, light pink and light grey panels, and two highlight roles. Dense where the data is (labs, code, maps), calm where the prose is. Motion only ever shows a data operation. Source of truth for tokens: `docs-v3/assets/sts.css` (variant C dual, light and dark themes); the deck system shares token names in `docs-v3/assets/slides.css`.

**Key characteristics:** purple focal; indigo to purple gradient chrome; yellow = interactive, light blue = data; dark purple code blocks; dark indigo/purple lab bench in both themes.

## Intentional decisions (instructor rulings, not anti-patterns)

- **The indigo to purple gradient is the instructor's explicit ruling.** It is used on the navbar (`--nav-bg: linear-gradient(135deg, var(--indigo), var(--purple))`), the home banner, and the gradient feature card (including its flip variant). It is an intentional brand decision after the sister project's navbar, not a generic "purple gradient" to be fixed. If the detector reports it, acknowledge it; do not redesign it away. `--nav-bg-solid` exists where a flat colour is required.
- **Fonts follow a split:** system sans for the site, system mono for code and data, vendored Lato in the slide decks. No web-font download for the site.
- **Code blocks are dark purple** (`--code-bg #1A1033`, ink `#EDE9FE`, comments `#A78BFA`), by ruling, in both themes.
- **Highlight roles (variant C dual):** yellow `#F5D90A` = interactive (hover, active tab, selected control, focus on dark grounds); light blue `#38BDF8` = data (the annotated point, info chips, KPI deltas).
- **The lab "bench"** (visual + sidebar) sits on the dark purple to indigo feature surface in both light and dark themes; page prose stays on the light surface.

## Colors

A purple-anchored palette on white, with pink and grey as quiet grounds and two highlight roles.

### Primary
- **Course Purple** (#6B21A8): buttons, links, accent bar, rail, focal chart series. White on it 8.72:1.
- **Deep Purple** (#3B0764): headings, footer, annotation ink, highlight text ink.
- **Indigo** (#1E1B4B): gradient start for navbar, banner and feature cards.

### Secondary
- **Interactive Yellow** (#F5D90A, soft #FEF9C3): interaction states only.
- **Data Blue** (#38BDF8, soft #E0F2FE, lighter #7DD3FC): the one thing to notice in a chart.

### Neutral
- **Ink** (#14121A) body text; **Muted** (#4B5563) captions; **White** surface; **Grey** (#F3F4F6 / #E5E7EB / #D1D5DB) panels, gridlines, rules; **Pink** (#FCE7F3, line #FBCFE8, ink #831843) callouts.

### Named Rules
**The Fill-Not-Text Rule.** Highlights are fills, washes and underlines, never text on white (yellow on white is 1.42:1).
**The Stroke Rule.** Data-highlight dots carry a deep-purple stroke so the mark clears 3:1.
**The No-Orange Rule.** Orange is out by ruling; plasma orange survives only inside the sequential ramp.
**Chart ramps.** Categorical (C) `#6B21A8 #38BDF8 #CC4778 #E0B800 #1E1B4B #9CA3AF`; sequential plasma `#0D0887 #5B02A3 #9A179B #CB4678 #EB7852 #FBB32F`. Purple is the series the message is about; grey is context.

## Typography

**Body Font:** system sans stack (`--sans`)
**Label/Mono Font:** system mono (`ui-monospace, SFMono-Regular, Menlo, Consolas, monospace`) for code, data tables and lab readouts
**Deck Font:** vendored Lato
**Character:** plain, fast, legible system type; personality comes from colour and data, not the typeface.

### Hierarchy
- **Site H1** (clamp(28px, 7vw, 44px)): page titles.
- **Body** (16px / 1.6): prose.
- **Deck** (body 30px, lead 34px, sub 44px, title 62px, h1 96px, floor 26px): slides on a 1280 x 720 stage.

## Layout

Single reading column (`.page` max-width 880px, 16px gutter) that works at phone width. Labs use a two-column bench (visual left, 340px sidebar right) that stacks on narrow screens.

## Elevation & Depth

Mostly flat with a soft two-layer shadow (`--shadow: 0 1px 2px rgba(20,18,26,.06), 0 4px 16px rgba(20,18,26,.08)`). Depth for emphasis comes from the dark feature surfaces, not heavier shadows.

## Shapes

Small radii: 6px for controls and code blocks, 8px for cards. 1px `--line` borders on cards.

## Components

### Buttons
- **Primary:** purple fill, white text, 6px radius, 8px 14px padding. Hover lifts 1px; focus ring purple (yellow on dark grounds).

### Cards / Containers
- **Card:** white, 1px line border, 8px radius, 14px padding.
- **Gradient feature card:** indigo to purple background, white type, yellow interactive highlight; flip variant (front title + glyph, back prose or stat) with reduced-motion fallback that stacks both faces.
- **Dark feature card:** dark purple to indigo; for callouts that must not be missed and "the question this week".
- **Callout / panel:** pink callouts, light grey panels (AI prompt, formula).

### Code blocks
Dark purple, 6px radius, 12px padding, horizontal scroll.

### Labs
Dark bench, sidebar with live metrics, status bar, three learning-check cards, R | SQL code panel.

## Motion

Tokens `--dur-fast 150ms`, `--dur-base 280ms`, `--dur-slow 600ms`, `--stagger 80ms`, `--rise 12px`. Cards rise in, bars grow, one pulse travels the one path the chart is about. `prefers-reduced-motion` removes all animation.

## Do's and Don'ts

- **Do** keep the indigo to purple gradient on navbar, banner and feature cards (ruling).
- **Do** use yellow only for interaction and light blue only for data.
- **Do** keep one message per chart, direct labels, light gridlines.
- **Don't** put highlight colours as text on white.
- **Don't** add orange, 3D, dual axes, or decorative motion.
- **Don't** introduce a web font on the site.

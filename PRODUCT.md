# Product

<!-- impeccable:product-schema 1 -->

> Written without a live interview: every fact below is taken from the instructor's written brief and the course plan. Anything those sources do not state is marked "Not yet decided".

## Platform

web

## Stack

Static HTML/CSS/JS site, like the sister course site (sigma): its shell and verification gates carry over. React appears only in student builds and exemplars, never in the course site. Content deploys through Posit Connect.

## Users

- **Students of SYSEN 5460**, *Data Science for Socio-Technical Systems: Decision-Making and Data Communication at Scale*. No prerequisites; beginner R in week 1 is real, so anything in week 1 must be zero-setup. Hybrid delivery (on-campus and distance).
- **The teaching team**, who build and maintain chapters, labs and decks with AI agents.

## Product Purpose

The course website is the textbook: one chapter per week, R first, with the same step shown in Supabase/PostgreSQL where both make sense. It teaches data communication and visual design systems at scale, with geospatial analysis as the thread through every module (temporal, spatial, networks, geospatial networks). Success: students build a well-designed React front end powered by a Plumber (or FastAPI) back end on a Supabase database, and can defend their design and data-architecture choices.

## Positioning

The course's demo contrast: "what AI generates for you right away" versus a thoughtfully designed, AI-led analytical dashboard. The workshops are the content; the best material is geospatial and database work.

## Operating Context

- Each chapter opens with a learning check on last week's chapter, then the week's question, sections with R chunks (and an R | SQL tab set where a database twin exists), a helper-prompt card per section, a playground, activities, gated readings and a lecture deck.
- Activities are the pedagogy: draw it, explain it, read it, code it, review it, critique it.
- Every chapter carries one or more interactive concept labs (visual + sidebar + three learning checks + R | SQL code panel) on real course data.
- Canvas owns dates, grades, submission and extra readings; the website links to readings, gated.
- Oral challenges with simulated stakeholders are their own assignments.

## Capabilities and Constraints

- R first everywhere; a Python pass may come much later.
- Shiny is retired; Plumber + React replaces it.
- Labs teach data wrangling, data engineering, data science, statistics and GIS concepts, not dataviz; each maps to an existing learning objective; nothing is added for niftiness.
- Motion depicts a data operation, never decoration; everything works with reduced motion.
- AI grading: not now (plan only).
- Final site URL: Not yet decided.
- Final grading scheme: Not yet decided.

## Brand Commitments

- The course colour is purple (it always has been). Palette: purple focal; white and black; light pink and light grey; a highlight colour. Yellow and light blue, not orange (instructor ruling).
- The navbar, banner and feature cards use an indigo to purple gradient, by the instructor's explicit ruling.
- The existing purple monochrome banner illustration heads the home page.

## Evidence on Hand

- Workshop scripts and documented course datasets in this repository (`workshops/`, `data/`).
- Last year's activities and decks, to be imported rather than replaced by invented filler.
- The machine-learning ("GeOAI") example has no surviving notes; it is designed fresh. Do not fabricate prior material for it.
- No testimonials, outcomes data or enrollment figures are supplied; do not invent any.

## Product Principles

1. The workshops are the content; the site presents them, it does not replace them.
2. Show both routes: R and SQL side by side where both make sense.
3. Every visual carries one message; motion and colour mean something or are removed.
4. Real course data, toggleable, over synthetic examples.
5. Activities over lecture: students draw, explain, critique and build.

## Accessibility & Inclusion

WCAG AA on every token pair in use, visible focus rings, contrast of highlight-on-gradient checked, colour-blind check of the categorical ramp, and a reduced-motion audit. Body text holds at least 7:1 contrast. Pages must work at phone width.

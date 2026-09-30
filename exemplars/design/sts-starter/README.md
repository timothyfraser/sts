# STS Starter Kit

A small, neutral design system for the course's React + Plumber template:
warm-grey neutrals, one green accent, one blue for data, loading, empty and
error states, in light and dark themes.

- `tokens.css`: the tokens (colours, fonts, spacing, radius).
- `components.css`: the components, built from tokens only.
- `preview/index.html`: every token and component on one page.
- `DESIGN.md`: what each token is for, the seven rules, and how to copy the
  kit into your own app. **Read this before you use the kit.**

## Viewing the preview

The preview is a plain HTML file with no build step.

**Quickest:** open `preview/index.html` in your browser (double-click it, or
drag it onto a browser window).

**From a local server** (useful if your browser is strict about local files):

```bash
# from the repository root
python3 -m http.server 8000
# then open http://localhost:8000/exemplars/design/sts-starter/preview/
```

In R, `servr::httd(".")` from the repository root does the same.

Use the **Switch to dark** button at the top right to change themes. The page
also follows your system's light/dark setting until you press the button. To
see the phone layout, narrow the window, or open your browser's device toolbar
(DevTools) at 375 px wide.

The fonts (IBM Plex Sans and Mono) load from Google Fonts. Offline, the page
falls back to your system fonts and still works.

## Using it in an app

Copy `tokens.css` (and `components.css` if you want it) into your app's
`src/styles/`. Never import them from this folder. `DESIGN.md` explains why and
walks through the steps.

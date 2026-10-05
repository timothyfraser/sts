# STS Starter Kit: design notes for students

Kit version 1.0. This is the small, neutral design system your React + Plumber
app starts from. It has warm-grey neutrals, one green accent, one blue for data,
three semantic colours, seven spacing steps, one corner radius and one type
family (IBM Plex). It comes in a light theme and a dark theme.

You don't have to love it. It is there so that your first working app looks
tidy without you making fifty design decisions on day one. Once the app works,
change the accent (see the section on changing the accent below) and make it
yours.

Open `preview/index.html` to see every token and component in use (the README
says how).

## What is in this folder

| File | What it is |
|---|---|
| `tokens.css` | Every colour, font, spacing step and radius, as CSS custom properties. Light theme plus two identical dark blocks. |
| `components.css` | Buttons, chip, form fields, card, KPI tile, table, the three panel states, legend and the app shell. It uses tokens only. |
| `preview/index.html` | A standalone page that shows the kit as a working dashboard, with a light/dark switch. |
| `preview/shots/` | Screenshots of the preview at phone and laptop width, in both themes. |

## The tokens and what each one is for

A **token** is a named value, such as `--accent: #0F6E56`. Components say
`var(--accent)` instead of the hex code, so changing one line in `tokens.css`
changes every button, chip and highlighted bar at once.

### Colour

| Token | Light | Dark | Use it for |
|---|---|---|---|
| `--bg` | `#F7F7F4` | `#141513` | The page background behind every panel. |
| `--surface` | `#FFFFFF` | `#1D1F1C` | Cards, tables and inputs: anything that sits on the page. |
| `--fg` | `#1C1C1A` | `#ECEDE8` | Body text and headings. |
| `--muted` | `#5F615C` | `#A3A69E` | Labels, notes and axis text: the secondary words. |
| `--line` | `#DCDDD7` | `#33362F` | Borders, table rules and chart gridlines. |
| `--accent` | `#0F6E56` | `#4FCBA6` | Actions and "the selected thing": primary button, selected chip, highlighted bar. |
| `--accent-ink` | `#FFFFFF` | `#0B2A21` | Text placed *on* the accent (the primary button's label). |
| `--accent-soft` | `#DDF0E9` | `#163A30` | A quiet fill for a selected chip or row. |
| `--data` | `#2A52E6` | `#8FA7FF` | Chart marks (bars, lines, points) and the info button's text. |
| `--data-soft` | `#E4EAFF` | `#1E2A5C` | The info button's fill and light data backgrounds. |
| `--ok` | `#0F6E56` | `#4FCBA6` | A number that got better (`+8.4%`). |
| `--warn` / `--warn-soft` | `#B4520A` / `#FBE8D5` | `#F0A05C` / `#3F2A14` | The empty state: the query worked but returned no rows. |
| `--bad` / `--bad-soft` | `#B42318` / `#FBE3E0` | `#F08A80` / `#44201C` | The error state, and a number that got worse. |

Keep two ideas apart. **Accent** means "you can act on this" or "this is
selected". **Data** means "this is a measurement". A bar is blue because it is
data. It turns green only when the reader has selected it. Semantic colours
(`ok`, `warn`, `bad`) carry meaning and are never decoration. Don't paint a
header red because red looks nice.

### Type

| Token | Value | Use it for |
|---|---|---|
| `--sans` | IBM Plex Sans, then system fonts | Everything people read. |
| `--mono` | IBM Plex Mono, then system monospace | Code, query strings such as `GET /trend?year=2016`, numbers that must line up. |

The type scale is h1 32 to 40 px, h2 22 px, h3 16 px, body 16 px, labels 13 px.
Headings use weight 600 and body text 400.

The fonts load from Google Fonts. If they fail to load, the fallback fonts in
the stack take over and nothing breaks. Add this line to your `index.html`
`<head>`:

```html
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600&family=IBM+Plex+Mono:wght@400;500&display=swap">
```

### Space and shape

| Token | Value | Typical use |
|---|---|---|
| `--s1` | 4px | Inside a KPI tile, label to value. |
| `--s2` | 8px | A heading and the sentence under it. |
| `--s3` | 12px | Controls in a row; small card padding. |
| `--s4` | 16px | Card padding, the gap between cards, the page gutter on a phone. |
| `--s5` | 24px | Between groups inside a section. |
| `--s6` | 32px | Page top and bottom padding. |
| `--s7` | 48px | Between page sections. |
| `--r` | 8px | The one corner radius, for cards, buttons and inputs. |

## The seven rules

1. **TOKENS.** Components use `var(--token)` only. A raw hex value such as
   `#2A52E6` inside a component is a bug. Put it in `tokens.css` and give it a
   name.
2. **SPACING.** Use the steps above, 4 to 48 px. Space between siblings comes
   from `gap` on the parent (flex or grid), not from margins on the children.
3. **RADIUS.** One radius, 8 px. The fully rounded "pill" shape is for chips
   only.
4. **NUMBERS.** Every table, KPI tile and chart axis uses tabular numerals
   (`font-variant-numeric: tabular-nums`) so the digits line up. Right-align
   number columns.
5. **STATES.** Every panel that fetches data has three states besides
   "done": **loading** (a bar and the request it is waiting on), **empty** (the
   query worked but found no rows, plus what to try) and **error** (the
   request failed, what probably went wrong, and a Retry button). A panel is
   never just blank.
6. **PHONE.** At 400 px wide, the page is one column, nothing scrolls
   sideways, and every button or input is at least 44 px tall. A wide table
   scrolls inside its own card (`.tw`), never the whole page.
7. **DARK.** Both themes come from the same token names. Never write a
   dark-only colour into a component. If something looks wrong in dark mode,
   fix the token.

## How to use the kit in your app

**Copy the files. Don't import them from this folder.**

1. Copy `tokens.css` into your app as `src/styles/tokens.css`. Copy
   `components.css` too if you want the ready-made components, for example as
   `src/styles/components.css`.
2. Put a header comment at the top of your copy that names the kit and the
   version you copied, so you can tell later where the values came from:

   ```css
   /* Copied from the STS Starter Kit, tokens.css v1.0. Edit here, not in the kit. */
   ```

3. Import the files once, from your entry file (for a Vite + React app that is
   usually `src/main.jsx`):

   ```js
   import './styles/tokens.css';
   import './styles/components.css';
   ```

4. **Never** write an import like `../../exemplars/design/sts-starter/tokens.css`.
   Your app has to build and deploy on its own, and the deployed app won't have
   this folder next to it. A copy also means you can change your accent without
   changing anybody else's app.

5. **Fake data never ships.** The preview uses example figures so you can see
   the layout. In your app, every number on screen comes from your API. Before
   you deploy, search your code for any made-up rows you used while building
   and remove them.

## How to change the accent (and check it still reads)

The accent is the easiest way to make the kit yours. You change three tokens
in each theme:

1. In the light `:root` block, set `--accent` to your colour, `--accent-ink`
   to the text colour that sits on it (usually white or near-black) and
   `--accent-soft` to a very pale tint of it.
2. In **both** dark blocks (the `@media` one and the `[data-theme="dark"]`
   one), set a lighter, brighter version of the same colour for `--accent`, a
   very dark `--accent-ink` and a deep, dim `--accent-soft`. The two dark
   blocks must match.
3. If your accent used to double as "good news", also update `--ok`, or leave
   `--ok` green on purpose.
4. **Check the contrast.** Use any WCAG contrast checker (browser DevTools
   show it when you inspect a text element's colour) and confirm these pairs:

   | Pair | Where you see it | Needs at least |
   |---|---|---|
   | `--accent-ink` on `--accent` | Primary button label | 4.5 : 1 |
   | `--accent` on `--bg` | Rule labels, accent text on the page | 4.5 : 1 |
   | `--accent` on `--surface` | A highlighted bar next to a grey gridline | 3 : 1 |

   Check both themes. For reference, the kit's own green gives 6.2 : 1
   (button, light), 7.6 : 1 (button, dark), 5.8 : 1 (on the light page) and
   9.1 : 1 (on the dark page).

   The **info button** (`--data` on `--data-soft`) is the tightest pair:
   5.1 : 1 in the light theme and 5.9 : 1 in dark. It is 15 px, weight 500
   text, so 4.5 : 1 is the target. If you change the data colours, re-check it.
5. Reload the preview (or your app) in both themes, then look at the primary
   button, a selected chip and the highlighted bar in the chart.

Keep the accent and the data colour clearly different. If you pick a blue
accent, readers can no longer tell "selected" from "data". Pick a data colour
that is far from it too.

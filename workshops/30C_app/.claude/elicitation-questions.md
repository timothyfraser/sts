# Elicitation Questions by Stage

## Stage 1 — Before Generating the HTML Mockup

Ask these questions before building anything. Present as a numbered list. Options are suggestions — the user can write in their own answer.

---

### 1. Dashboard Purpose
> What is the primary audience for this dashboard?
- A) Just me (developer/researcher) exploring my own data
- B) Students or classmates
- C) External stakeholders / clients / public
- D) Other: ___

---

### 2. Layout Preference
> What overall layout do you have in mind?
- A) Single scrolling page with stacked panels
- B) 2-column grid (charts side by side)
- C) Tabbed layout (different views per tab)
- D) Sidebar + main content area
- E) Not sure — show me 2 options and I'll pick

---

### 3. Primary Chart Types
> Which of these chart types does your workflow produce? (select all that apply)
- A) Bar / column chart
- B) Line / time series
- C) Scatter plot
- D) Histogram / density
- E) Map / choropleth (will be a placeholder in Stage 1)
- F) Network diagram (will be a placeholder in Stage 1)
- G) Table / data grid
- H) Summary stat cards (big numbers)
- I) Other: ___

---

### 4. Interactivity Needs
> What filters or controls do you want users to have?
- A) Year / time period slider or dropdown
- B) Geographic filter (county, state, region)
- C) Category / group filter
- D) A numeric threshold slider
- E) No filters — dashboard is static
- F) Other: ___

---

### 5. Color Style
> What's your preferred visual style?
- A) Clean / minimal (whites, light grays, one accent color)
- B) Dark mode
- C) Institutional (blues, navy — academic / government feel)
- D) Warm / earthy tones
- E) I'll customize it myself in the sidepanel
- F) Match a specific brand — here are the hex codes: ___

---

### 6. Complexity Check
> Does your `workflow.R` produce any of the following? (select all that apply — these will be placeholders in Stage 1)
- A) Spatial / map objects (`sf`, `terra`, `tmap`, `leaflet`)
- B) Network / graph objects (`igraph`, `tidygraph`)
- C) Fitted statistical models (`lm`, `glm`, `lmer`, `brm`, etc.)
- D) Machine learning model outputs
- E) None of the above — all outputs are data frames and ggplots

---

### 7. Synthetic Data Realism
> How realistic should the fake data be in the mockup?
- A) Just plausible ranges and column names — I mostly care about layout
- B) Fairly realistic — match the distributions and magnitudes from my real data
- C) Very realistic — I'll paste some summary stats for you to match

---

## Stage 2 — Before Converting to Shiny

Ask these if not already answered:

### 1. R Package Constraints
> Are there any R packages you can't use (e.g., server doesn't have them, student machines, etc.)?

### 2. Deployment Target
> Where will this Shiny app run?
- A) Locally (`shiny::runApp`)
- B) shinyapps.io
- C) Posit Connect
- D) Other: ___

### 3. Data Loading
> How does your `workflow.R` load data?
- A) Reads local CSV/RDS files (I'll bundle them with the app)
- B) Queries a database (I'll handle credentials separately)
- C) Calls an API
- D) The processed data objects are already created earlier in the script — just source it

---

## Stage 3 — Before Real Data Integration

### 1. Data Availability
> Which data files / objects from `workflow.R` are ready to use?
- A) All of them — everything is local and accessible
- B) Some are missing — I'll note which ones
- C) Some require credentials or API keys I'll configure separately

### 2. Performance
> Are any of your real data operations slow (> 5 seconds)?
- A) No — everything is fast
- B) Yes — I need caching (I'm open to `memoise` or `bindCache`)
- C) Not sure — let's see what happens

![Banner Image](../../docs/images/title.png)

# 🗳️ README: Boston Voting Dataset Codebook ('/boston_voting')

> This folder contains spatial and tabular data on voter behavior, polling locations, precinct geography, and transit infrastructure for Boston, MA.

---

## 📁 Files Overview

| File | Description |
|------|-------------|
| `boston_votes.csv` | Precinct-level voting and turnout data |
| `polling_places.rds` | Geolocated polling place points |
| `precincts.rds` | Spatial boundaries of voting precincts |
| `train_lines_boston.rds` | Subway and light rail lines (MBTA) |
| `train_stops_boston.rds` | Subway and light rail station points (MBTA) |

---

## 📊 `boston_votes.csv`

**Description**: Voter behavior and turnout by ward-precinct in the 2020 presidential election.

| Variable        | Description |
|------------------|-------------|
| `ward`           | Ward number (string format, e.g. `"01"`) |
| `precinct`       | Precinct number within ward (string format, e.g. `"01"`) |
| `ward_precinct`  | Concatenated ward and precinct code (e.g. `"0101"`) |
| `voted_biden`    | Percent of votes cast for Joe Biden |
| `voted_trump`    | Percent of votes cast for Donald Trump |
| `voter_turnout`  | Percent of registered voters who turned out to vote |

🧠 **Note**: Percentages may not sum to 100% due to third-party or write-in votes.

---

## 📍 `polling_places.rds`

**Description**: Spatial point data of Boston polling place locations.

| Variable         | Description |
|------------------|-------------|
| `id`             | Unique ID for polling place |
| `ward_precinct`  | Matching precinct code (e.g. `"0101"`) |
| `geometry`       | Spatial point location in projected coordinates (meters) |

---

## 🗺️ `precincts.rds`

**Description**: Spatial boundaries of voting precincts.

| Variable         | Description |
|------------------|-------------|
| `ward_precinct`  | Ward-precinct code (e.g. `"0205"`) |
| `geometry`       | `MULTIPOLYGON` geometry of precinct boundaries |

---

## 🚇 `train_lines_boston.rds`

**Description**: MBTA subway/light rail lines in Boston.

| Variable | Description |
|----------|-------------|
| `line`   | MBTA line name (e.g. `"RED"`, `"ORANGE"`, `"GREEN"`, `"BLUE"`) |
| `geometry` | `LINESTRING` geometry tracing transit route path |

---

## 🚉 `train_stops_boston.rds`

**Description**: Station point data for MBTA train lines.

| Variable | Description |
|----------|-------------|
| `station` | Station name (e.g. `"Ashmont"`, `"Harvard"`) |
| `line`    | MBTA line name (e.g. `"RED"`, `"GREEN"`) |
| `geometry` | Spatial point location in projected coordinates (meters) |

---

## 📦 Example Analysis Use Cases

- Analyze voting patterns relative to MBTA station proximity
- Visualize voter turnout across precincts using `precincts.rds`
- Map polling place accessibility using `polling_places.rds`
- Integrate transit lines with voting patterns to explore urban infrastructure and civic behavior

---

## 🗂️ Example Join Key

- Join `boston_votes.csv` with `polling_places.rds` or `precincts.rds` using `ward_precinct`

```r
votes <- read_csv("data/boston_voting/boston_votes.csv")
precincts <- read_rds("data/boston_voting/precincts.rds")

# Join example
votes_geo <- precincts %>% left_join(votes, by = "ward_precinct")
```

---

![Footer Image](../../docs/images/footer.png)

---

← 🏠 [Back to Top](#README-boston_voting)

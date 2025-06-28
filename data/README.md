# README `/data`

A collection of additional READMEs for files located in the main directory of the data folder.

## `jp_solar.csv` 🇯🇵☀️

This dataset includes **monthly observations of rooftop solar adoption** across a matched sample of 147 Japanese municipalities, collected over **43 months** surrounding the 2011 Tōhoku earthquake and tsunami.

Each row represents a **city-month** observation, totaling **6,321 observations**.

### Variables

| Column            | Type    | Description                                                                 |
|-------------------|---------|-----------------------------------------------------------------------------|
| `muni_code`       | `chr`   | Municipality code (5-digit Japanese city code)                             |
| `date`            | `date`  | Observation date (typically end-of-month)                                  |
| `year`            | `dbl`   | Calendar year of the observation                                           |
| `solar_under_10kw`| `dbl`   | rooftop solar systems <10kW installed in the municipality       |
| `solar`           | `dbl`   | Number of new rooftop solar units added during the month                   |
| `solar_rate`      | `dbl`   | New rooftop solar installations per 1,000 residents                        |
| `disaster`        | `dbl`   | `1` if the municipality experienced deaths or damage from the 2011 disaster; `0` otherwise |
| `pop`             | `dbl`   | Population of the municipality from the most recent 5-year census          |

### Notes

- The variable `solar_rate` normalizes adoption by population for comparability across municipalities.

- ### Sample Data from `jp_solar.csv`

| muni_code | date       | year | solar_under_10kw | disaster | pop   | solar | solar_rate           |
|-----------|------------|------|------------------|----------|-------|--------|------------------------|
| 02204     | 2019-06-25 | 2019 | 214              | 0        | 34284 | 6      | 0.17500875043752187    |
| 02204     | 2019-03-25 | 2019 | 208              | 0        | 34284 | 10     | 0.29168125072920315    |
| 02204     | 2018-12-25 | 2018 | 198              | 0        | 34284 | 9      | 0.2625131256562828     |

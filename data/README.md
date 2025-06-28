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



## Codebook: `jp_solar_farms_2018.csv`

This dataset includes municipality-level covariates used in a matching experiment to evaluate the relationship between solar adoption and disaster vulnerability in Japan. Each row represents a single municipality observed at baseline, prior to intervention or outcome tracking. Based on Fraser (2019) in Social Science Quarterly.

### Columns

| Variable                        | Description                                                                                   | Example                |
|----------------------------------|-----------------------------------------------------------------------------------------------|------------------------|
| `muni_code`                     | Unique identifier for the municipality (Japanese official code).                             | `01100`                |
| `muni`                          | Municipality name (English transliteration).                                                  | `Sapporo`              |
| `muni_type`                     | Type of municipality (e.g., city, town, village).                                             | `city`                 |
| `pref`                          | Name of the prefecture.                                                                      | `Hokkaido`             |
| `pref_code`                     | Japanese prefecture code (2-digit numeric string).                                            | `01`                   |
| `region`                        | Broader geographic region grouping (e.g., Kanto, Tohoku).                                    | `Hokkaido`             |
| `fukushima_exclusion_zone`      | Binary indicator for whether the municipality overlaps with the 2011 nuclear exclusion zone. | `0` (No), `1` (Yes)    |
| `coast`                         | Binary indicator for whether the municipality is coastal.                                    | `0` (No), `1` (Yes)    |
| `pv_output_2018`                | Estimated average photovoltaic (solar) output in 2018 (kWh/m²/day).                           | `1218`                 |
| `windspeed_2018`                | Average wind speed in 2018 (m/s).                                                             | `6.56`                 |
| `solarkw`                       | Total installed solar capacity across all system sizes (in kilowatts).                       | `66442.8`              |
| `sp`                            | Total number of solar power systems installed.                                                | `217`                  |
| `sp_10_49kw`                    | Count of systems in the 10–49 kW range.                                                       | `162`                  |
| `sp_50_499kw`                   | Count of systems in the 50–499 kW range.                                                      | `15`                   |
| `sp_500_1999kw`                 | Count of systems in the 500–1999 kW range.                                                    | `40`                   |
| `sp_2000kw_plus`                | Count of systems larger than 2,000 kW.                                                        | `0`                    |
| `land_price_commercial_2009`    | Commercial land price in 2009 (¥/m²).                                                        | `257000`               |
| `land_price_residential_2009`   | Residential land price in 2009 (¥/m²).                                                       | `65000`                |
| `area_2010`                     | Land area in square kilometers.                                                              | `112112`               |
| `population_2010`               | Population count from the 2010 census.                                                       | `1913545`              |
| `unemployment_2010`             | Unemployment rate (%) in 2010.                                                               | `7.7`                  |
| `voter_turnout_2012`            | Voter turnout percentage in the 2012 national election.                                      | `55.367`               |
| `crime_rate_2008`               | Reported crimes per 1,000 residents in 2008.                                                  | `14.32`                |
| `income_taxable_per_capita_2010`| Average taxable income per capita in 2010 (¥).                                               | `1256912.95`           |
| `financial_str_index_2010`      | Financial strength index (higher = stronger fiscal standing).                                | `69`                   |
| `ratio_revs_exp_2010`           | Ratio of municipal revenues to expenditures.                                                  | `0.6`                  |
| `disaster_deaths_2011`          | Number of disaster-related deaths in 2011.                                                   | `0`                    |
| `disaster_damage_2011`          | Estimated number of buildings damaged or destroyed from the 2011 disaster                         | `0`                    |

---

### Notes

- Units are generally as reported in national statistics.
- Data sources include Japan's e-Stat Portal, METI, and NPA.
- `pv_output_2018` and `windspeed_2018` are modeled estimates from METI’s solar and wind databases.
- Solar system counts are disaggregated by capacity band for understanding scale of installation.
- The dataset was designed for analyzing municipalities based on socioeconomic, geographic, and disaster-exposure covariates.

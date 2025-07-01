# README `/data`

A collection of additional READMEs for files located in the main directory of the data folder.

---

## `jp_solar.csv` ☀️

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


---

## `jp_solar_farms_2018.csv`

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

- Citation: https://scholar.google.com/citations?view_op=view_citation&hl=en&user=Ty7f6yAAAAAJ&cstart=20&pagesize=80&authuser=1&citation_for_view=Ty7f6yAAAAAJ:R3hNpaxXUhUC


### Notes

- Units are generally as reported in national statistics.
- Data sources include Japan's e-Stat Portal, METI, and NPA.
- `pv_output_2018` and `windspeed_2018` are modeled estimates from METI’s solar and wind databases.
- Solar system counts are disaggregated by capacity band for understanding scale of installation.
- The dataset was designed for analyzing municipalities based on socioeconomic, geographic, and disaster-exposure covariates.

---

## `jp_matching_experiment.csv`

This dataset contains municipality-level covariates for examining the social, economic, and demographic variation in exposure to the 2011 Tōhoku earthquake and tsunami in Japan. Each row represents a municipality in the year 2011 to 2018 (?).

### Columns

| Variable                      | Description                                                                                     | Example     |
|------------------------------|-------------------------------------------------------------------------------------------------|-------------|
| `muni_code`                  | Unique municipality identifier (Japanese official code).                                        | `02201`     |
| `muni`                       | Name of the municipality (English transliteration).                                             | `Aomori`    |
| `pref`                       | Name of the prefecture.                                                                         | `Aomori`    |
| `year`                       | Year of observation (always 2011 for this dataset).                                             | `2011`      |
| `by_tsunami`                 | Categorical indicator of tsunami impact. `"Hit"` if municipality experienced tsunami damage or deaths, `"Not Hit"` otherwise. | `Hit` |
| `social_capital`             | Composite index measuring social capital (e.g. community participation, trust), where 0 = worst possible and 1 = best possible.               | `0.393`     |
| `damage_rate`                | Rate of damaged or destroyed buildings per 1000 residents (0 if no impact).                           | `0.001`     |
| `pop_density`                | Population density (persons per km²).                                                           | `1137.5`    |
| `exp_dis_relief_per_capita` | Per-capita government expenditure on disaster relief in fiscal 2011 (in 10,000s of yen).        | `2.44`      |
| `income_per_capita`         | Per-capita income (millions of yen).                                                            | `1.13`      |
| `unemployment`              | Unemployment rate (%) in 2011.                                                                  | `5.9`       |
| `pop_women`                 | Share of population that is female (%).                                                         | `53.6`      |
| `pop_over_age_65`           | Share of population age 65 and older (%).                                                       | `27.9`      |

---

- Citation: https://scholar.google.com/citations?view_op=view_citation&hl=en&user=Ty7f6yAAAAAJ&cstart=20&pagesize=80&authuser=1&citation_for_view=Ty7f6yAAAAAJ:e5wmG9Sq2KIC


### Notes

- All data reflect pre- or post-disaster conditions as measured in or around 2011.
- `social_capital` is based on survey or aggregate indicators (source-dependent).
- The dataset can be used for matching designs or causal inference related to tsunami exposure and resilience.
- `damage_rate` and `exp_dis_relief_per_capita` are zero for municipalities not affected by the disaster.

---

## `jp_emissions.csv`

This dataset contains annual greenhouse gas emissions estimates and associated socioeconomic indicators for municipalities across Japan, spanning approximately 2005–2020. Each row represents a municipality-year observation and includes detailed emissions breakdowns, population statistics, government expenditures, and social indicators.

- `jp_emissions.csv` — 20,892 rows × 59 columns
- Format: CSV (UTF-8 encoded)
- Units: Emissions in metric tons CO₂ equivalent unless otherwise stated

### 📅 Time and Geography

| Variable         | Description |
|------------------|-------------|
| `year`           | Calendar year |
| `pref_code`      | Japanese prefecture code (2-digit) |
| `pref`           | Prefecture name (romanized) |
| `pref_jp`        | Prefecture name (Japanese) |
| `muni_code`      | 5-digit municipality code |
| `muni`           | Municipality name (romanized) |
| `muni_jp`        | Municipality name (Japanese) |

### ♻️ Emissions Totals

| Variable                       | Description |
|--------------------------------|-------------|
| `emissions`                   | Total CO₂ emissions (metric tons) |
| `emissions_industrial_subtotal` | Industrial emissions (sum of categories below) |
| `emissions_manufacturing`     | Manufacturing sector emissions |
| `emissions_construction_mining` | Construction & mining emissions |
| `emissions_agr_forest_fish`   | Agriculture, forestry, and fishing emissions |
| `emissions_consumer_subtotal` | Consumer emissions (sum of households and business) |
| `emissions_business`          | Commercial/business sector emissions |
| `emissions_households`        | Household emissions |
| `emissions_transport_subtotal` | Transportation emissions (sum below) |
| `emissions_automobiles`       | Private vehicles |
| `emissions_trucks`            | Commercial trucks |
| `emissions_railroad`          | Rail transport |
| `emissions_boats`             | Maritime emissions |
| `emissions_waste`             | Waste treatment emissions |
| `emissions_1990_2005`         | Reduction required vs. 1990/2005 baseline |

### 👥 Demographics & Socioeconomics

| Variable         | Description |
|------------------|-------------|
| `pop`            | Total population |
| `pop_women`      | % women |
| `pop_age_65_plus`| % age 65+ |
| `income_per_capita` | Annual income per capita (1000s of yen) |
| `unemployment`   | % unemployed |
| `pop_college`    | % with college education |
| `area_inhabitable` | km² of habitable area |
| `land_price_res` | Average residential land price (¥/m²) |

### 👷 Employment by Sector (%)

| Variable             | Description |
|----------------------|-------------|
| `employees_primary`   | Agriculture, forestry, fishing |
| `employees_secondary` | Manufacturing and industry |
| `employees_tertiary`  | Services, retail, etc. |
| `employees_muni`      | Public sector (municipality-employed) |

### 💰 Fiscal Indicators

| Variable             | Description |
|----------------------|-------------|
| `fin_str_index`      | Financial strength index |
| `ratio_rev_exp`      | Revenue-to-expenditure ratio |
| `exp_social_welfare` | Municipal spending on social welfare |
| `exp_health`         | Spending on health |
| `exp_public_works`   | Public works budget |
| `exp_fire`           | Firefighting services budget |
| `exp_education`      | Education spending |

### 💼 Economic Output (million yen)

| Variable             | Description |
|----------------------|-------------|
| `value_manuf_mill`   | Manufacturing value |
| `value_agr_mill`     | Agriculture value |
| `value_commerce_mill`| Commercial output value |

### 🌱 Environment & Migration

| Variable             | Description |
|----------------------|-------------|
| `renewables_kw_rate` | Installed renewable energy per capita (kW/person) |
| `social_capital`     | Composite social capital score |
| `bonding` / `bridging` / `linking` | Subcomponents of social capital |
| `vulnerability`      | Composite vulnerability index |
| `total_migration_rate` | Net migration per 1000 people |
| `inmigrants` / `outmigrants` | Annual in-/out-migration count |

### 🧭 Disaster Impact Flags (2011)

| Variable             | Description |
|----------------------|-------------|
| `fukushima`          | Flag for Fukushima-affected municipalities (1 = affected) |
| `tsunami`            | Flag for tsunami-impacted areas |
| `death_2011`         | Deaths reported from 2011 disaster |
| `destroy_2011`       | Structures destroyed in 2011 |
| `damage_2011`        | Structures damaged in 2011 |
| `exclusion_zone`     | Within nuclear exclusion zone (1 = yes) |

---

## 🧠 Example Uses

- Modeling the impact of social capital on emissions reductions  
- Identifying high-need areas for disaster recovery funding  
- Longitudinal tracking of municipal climate progress  

---

## 📌 Notes

- Emissions estimates derived from inventory methods; units are metric tons CO₂e  
- Fiscal and employment values are standardized per capita or per municipality where noted  
- Some indicators (e.g., population) are interpolated or repeated for years with missing data  

## 📚 Source

https://scholar.google.com/citations?view_op=view_citation&hl=en&user=Ty7f6yAAAAAJ&cstart=20&pagesize=80&authuser=1&citation_for_view=Ty7f6yAAAAAJ:u5HHmVD_uO8C

---


## `environmental_health.csv` - Environmental Health Dataset

### Load
```r
library(readr)
env_health <- read_csv("data/environmental_health.csv", show_col_types = FALSE)
```
| county         | fips  | state | air\_pollution | pop\_black | poc | urban          | party      | wealth       |
| -------------- | ----- | ----- | -------------- | ---------- | --- | -------------- | ---------- | ------------ |
| Autauga County | 01001 | AL    | 11.7           | 0.19       | Low | Urbanized Area | Republican | Above Median |

### Variables

- county: County name
- fips: 5-digit FIPS code
- state: State abbreviation
- air_pollution: Numeric air pollution measure (micrograms per cubic meter of PM2.5)
- pop_black: Proportion of Black residents (0-1)
- poc: People of Color category (e.g. "Low", "High") - above a threshold of 30%.
- urban: Urbanization status (e.g. "Urbanized Area", "Rural Area")
- party: Dominant political party in 2016 election.
- wealth: Wealth category relative to median ("Above Median", "Below Median")

---

# README `/bluebikes`

## Bluebikes Data Folder

This folder contains comprehensive data and materials for analyzing the Boston Bluebikes bikesharing system (formerly Hubway), covering about a decade of rides.

## Citation & Paper

Please cite if you use this data:

Fraser, T., Van Woert, K., Olivieri, S., Baron, J., Buckley, K., & Lalli, P. (2025). Cycling cities: Measuring urban mobility mixing in bikeshare networks. *Journal of Transport Geography*, 126, 104223.  
[Published Version](http://dx.doi.org/10.2139/ssrn.4076776)

## Setup

Unzip the main raw data archive:

```r
unzip("data/bluebikes/bluebikes.zip")  # creates bluebikes.sqlite
```

The `bluebikes.sqlite` SQLite database contains detailed bikeshare trip data. Using SQLite allows efficient querying without loading millions of rows directly into R. You can use dplyr syntax to query and then `collect()` results in manageable chunks.

We're going to use this SQLite database to access a refined version of our data. SQLite is really handy because we can ask it to do lots of number-crunching without loading all the millions of rows into our R environment, which would normally cause it to crash. Instead, we can feed SQLite basic dplyr functions, like ```select()```, ```mutate()```, ```filter()```, ```group_by()```, and ```summarize()```, and then ask the SQLite database to ```collect()``` the resulting data and give it to us in R. This output (should) be much, much, much smaller, at a size R can handle.


## Key Data Files

### 1. `bluebikes.sqlite`
- Main database containing multiple large tables:
  - `tally_rush_edges`: daily ride counts aggregated by rush hour periods.
  - `tally_rush`: daily ride counts by start and end stations during rush hours (very large).

### 2. `stationbg_dataset.rds`
- `sf` POINT dataset of Bluebikes stations with geographic location and linked census block group (`geoid`).
- Contains demographic and socioeconomic traits of each block group (population by race, age, density, employment, education, etc.).
- 416 rows, 47 columns.

### 3. `bgdataset.rds`
- `sf` MULTIPOLYGON dataset of all census block groups in Boston area.
- Contains rich demographic/socioeconomic traits and spatial geometry.
- 2,252 rows, 46 columns.

### 4. `dates.rds`
- Simple dataset of dates from 2011 onward with year and weekday labels.
- 3,500 rows, 3 columns.

### Common Identifiers

Here's a quick summary of all variable names you will run into across datasets.

- ```code```: unique ID for each bluebikes station.
- ```geoid```: unique ID for each census block group.
- ```count```: total rides occuring during that time, between those places.

### Variable Highlights (from `stationbg_dataset.rds` & `bgdataset.rds`)

- `geoid`: Census block group identifier  
- `geometry`: Spatial data (`POINT` for stations, `MULTIPOLYGON` for block groups)  
- `pop_0_40000_2019`, `pop_40001_60000_2019`, `pop_60001_100000_2019`, `pop_100000_plus_2019`: Population by income bracket  
- `pop_asian_2020`, `pop_black_2020`, `pop_white_2020`, `pop_hisplat_2020`, `pop_natam_2020`: Population by race/ethnicity proportions  
- `pop_density_2020`: Population density  
- `pop_employed_2019`: Employment rate  
- `pop_over_65_2019`: Elderly population proportion  
- Variables with suffixes `_smooth5` and `_smooth10` indicate missing data imputed by median of nearest 5 or 10 neighbors.

Example Demographics Variable Set:

- ```pop_density_2020```: population density per square kilometer in 2020. Some places are missing data.
- ```pop_density_2020_smooth5```: population density, with missing data filled in by taking the median of their 5 nearest neighboring census block groups.
- ```pop_density_2020_smooth10```: population density, with missing data filled in by taking the median of their 10 nearest neighboring census block groups. Either is fine to use.

I generated many other variables too, saved in ```data/bluebikes/stationbg_dataset.rds.``` Let's look at them real quick.

```{r, message = FALSE, warning = FALSE}
read_rds("data/bluebikes/stationbg_dataset.rds") %>% 
  select(-contains("smooth")) %>%
  names()
```


## How to Load

Example to load station block group dataset:

```r
library(readr)
library(dplyr)
library(sf)

station_bg <- read_rds("data/bluebikes/stationbg_dataset.rds")
glimpse(station_bg)
```

Example to load census block group polygons with demographics:

```r
bgdata <- read_rds("data/bluebikes/bgdataset.rds")
glimpse(bgdata)
```

Example to load dates data:

```r
dates <- read_rds("data/bluebikes/dates.rds")
glimpse(dates)
```

## Notes

- The raw data in the SQLite file is very large; use SQLite queries to subset or aggregate before loading into R.
- Demographic variables are standardized to 2019 or 2020 years depending on source.
- Smoothing applied to fill missing demographic values for spatial continuity.
- `geoid` is the key spatial identifier linking stations to census areas.

---

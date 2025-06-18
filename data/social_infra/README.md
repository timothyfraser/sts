# README: Social Infrastructure Dataset

**Author**: Tim Fraser\
**File**: `social_infra/example.R`\
**⚠️ Note**: This data is not yet released — please do **not share widely**.

## 🧱 Overview

This data comes from a new study, describing **25 cities' social infrastructure rates for every city block**.

The **social infrastructure database** is a massive database recording social infrastructure sites, broken into 4 major types:

-   🌳 **Parks** (e.g., green space, community gardens, bike paths)

-   🏛️ **Community Spaces** (e.g., libraries, community centers)

-   ☕ **Social Businesses** (e.g., coffee shops)

-   ⛪ **Places of Worship** (e.g., mosques, synagogues, and churches)

------------------------------------------------------------------------

## 📚 Citation

For data collection methods, see:\
Fraser, T., Cherdchaiyapong, N., Tekle, W., Thomas, E., Zayas, J., Page-Tan, C., & Aldrich, D. P. (2022).\
*Trust but verify: Validating new measures for mapping social infrastructure in cities.*\
**Urban Climate, 46**, 101287.\
🔗 [Main Article](https://drive.google.com/file/d/1zYtZNVE-J6Eb-Zsqg1icpnce6m65e96x/view?usp=sharing)\
📎 [Appendix](https://drive.google.com/file/d/10jEc2ZAunfo9C_NSUL6FgcpcsQc13iqu/view?usp=sharing)

------------------------------------------------------------------------

## 📦 Required Packages

``` r
library(dplyr) 
library(readr) 
library(ggplot2) 
library(sf) 
library(viridis) 
```

## 🧹 Memory Tip

Clear your local environment and frees memory. Recommended before loading large files.

``` r
rm(list = ls()); gc()
```

------------------------------------------------------------------------

## 🗺️ Data Resources


### 🧭 `bounds.geojson` – City Boundaries

> Spatial polygons for city boundaries covering the 25 most populous U.S. cities.

``` r
read_sf("data/social_infra/bounds.geojson") %>% head() 
```

| Variable   | Description                                    |
|------------|------------------------------------------------|
| `name`     | Name of city, for joining with other databases |
| `geometry` | Spatial geometry of city boundaries            |

------------------------------------------------------------------------

### 🧮 `bg.geojson` – Block Group Geometries

> Spatial polygons for every block group in the study region.

``` r
read_sf("data/social_infra/bg.geojson") %>% head()
```

| Variable    | Description              |
|-------------|--------------------------|
| `geoid`     | Census block group ID    |
| `area_land` | Land area (in square km) |
| `name`      | Name of city             |
| `geometry`  | Block group geometry     |

------------------------------------------------------------------------

### 📈 `bg_data.rds` – Block Group Socioeconomic Data

> Includes population, race, income, education, etc., by block group and year.

``` r
read_rds("data/social_infra/bg_data.rds") %>% glimpse()
```

Includes variables like:

-   `year`, `pop`, `median_household_income`, `unemployment`

-   `% women`, `% over_65`, `% black`, `% hisplat`, etc.

------------------------------------------------------------------------

### 🧊 `tally1km.geojson` – Gridded 1km Rates

> 1km² grid data with rates of social infrastructure and 2020 census estimates.

``` r
read_sf("data/social_infra/tally1km.geojson") %>% head()
```

Includes:

-   📍 `cell`, `geometry`
-   🏙️ `name`, `pop_density`, `units_occupied`
-   🧱 Infrastructure: `community_space`, `place_of_worship`, `social_business`, `park`
-   🧬 Demographics: `% white`, `% black`, `% hisplat`, etc.

------------------------------------------------------------------------

### 📌 `sites.geojson` – Site Locations

> Point data for each site in the social infrastructure database.

``` r
read_sf("data/social_infra/sites.geojson") %>% head()
```

Includes:

-   🆔 `place_id`, `name`

-   🏷️ `type`, `term`

-   📍 `geometry`

------------------------------------------------------------------------

## ⚠️ Other Datasets

Other datasets are available, but **not recommended** for dashboard use at this time.\
Their processing methods require further documentation.

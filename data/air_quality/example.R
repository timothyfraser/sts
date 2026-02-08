# example.R

# An example script showing 
# some ways to work with the air_quality.csv and related files

library(dplyr) # for data wrangling functions
library(readr) # for reading in data
library(lubridate) # for dates
library(sf) # for spatial data
library(stringr) # for string detection

# Really big file, so get used to it first using just the first 1000 rows...
preview = read_csv("data/air_quality/air_quality.csv", n_max = 1000)
preview %>% glimpse()

# Need some data on the air quality monitors? Here's a good query.
monitors = read_rds("data/air_quality/sites.rds") %>%
  st_transform(crs = 4326) %>%
  # Read in census block group IDs
  st_join(y = read_sf("data/air_quality/bg.geojson") %>% st_transform(crs = 4326), left = FALSE) %>%
  as_tibble() %>%
  select(aqs_id_full, site_name, dist, county, geoid, area_land) %>%
  # Join in county names
  left_join(y = read_rds("data/air_quality/metro.rds") %>% rename(county_name = county), by = c("county" = "geoid") ) %>%
  # Filter to just monitors in the 5 borough area!
  filter(county_name %in% c("New York County", "Bronx County", "Queens County", "Kings County", "Richmond County"))

# Ignore the warning message about CPL_transform. Doesn't matter.

# Want to work with the air quality dataset? Get read for some filters!
read_csv("data/air_quality/air_quality.csv") %>%
  # Filter to 1 pollutant metric (either PM2.5 or AQI)
  filter(pollutant == "PM2.5") %>%
  # Extract a particular piece of information from the datetime,
  # eg. year
  mutate(year = lubridate::year(datetime)) %>%
  filter(year %in% c(2024, 2025)) %>%
  # OR Filter to a specific year with string-detection
  # filter(str_detect(datetime, "(2024|2025)")) %>%
  # drop unit and pollutant - don't need them anymore
  select(-unit, -pollutant) %>%
#   mutate(month = lubridate::month(datetime)) 
#  mutate(wday = lubridate::wday(datetime)) 
#  mutate(hour = lubridate::hour(datetime))
  # Filter to just the monitors we selected above, using inner_join!
  inner_join(by = c("aqs_id_full"), y = monitors %>% select(aqs_id_full)) %>%
  # and write yourself a smaller version of the file, preferrably as .rds,
  # a compressed file type
  saveRDS("data/air_quality/data.rds")

# Remember that all values are in Greenwich mean time (GMT+0),
# so they'll need to be converted to EST if you want to do hour-specific filters.

# Much easier to read in now; and much smaller.
read_rds("data/air_quality/data.rds")


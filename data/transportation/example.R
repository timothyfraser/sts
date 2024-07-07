#' @name transportation/example.R
#' @description
#' Example script for assessing this database

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

emissions = read_rds("data/transportation/emissions.rds")

# Please read data/transportation/README.md first before proceeding.

# This database is intentionally structured to show vehicle emissions and vehicle activity data,
# at multiple levels of aggregation, for every county-year-pollutant set in New York State.

# `by` is the aggregation identifier.
# `by == 16` - overall for that county-year-pollutant set
# `by == 8` - estimates for each sourcetype for that county-year-pollutant set
# `by == 14` - estimates for each fueltype for that county-year-pollutant set
# `by == 12` - estimates for each regclass for that county-year-pollutant set
# `by == 15` - estimates for each roadtype for that county-year-pollutant set

# So, always always always filter by `by`. Otherwise, the data is a meaningless mess.
# If you're not sure, just filter by == 16 to get the overall totals.

# Every county, year, and pollutant, overall
emissions %>%
  filter(by == 16)


# One county (geoid = "36109"), by sourcetype (by = 8), for just passenger vehicles (sourcetype = 21), 
# for CO2 equivalent emissions (pollutant = 98)
emissions %>%
  filter(geoid == "36109", by == 8, sourcetype == 21, pollutant == 98)

# Every county in 2020 for CO2 equivalent emissions overall
emissions %>%
  filter(by == 16, pollutant == 98, year == 2020)


# An example visual
data = emissions %>%
  filter(by == 16, pollutant == 98, year == 2020)

ggplot() +
  geom_col(data = data, mapping = aes(x = geoid, y = emissions)) +
  coord_flip()





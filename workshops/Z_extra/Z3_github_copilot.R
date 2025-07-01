# Z3_github_copilot.R

# This script is a test for how to use github copilot.


library(dplyr)
library(readr)
library(lubridate) # date conversions with lubridate

data = read_csv("data/air_quality/air_quality.csv") %>% head(1000)

# I want to aggregate this dataset by day
data %>%
  # Convert dttm to day with lubridate
  # then aggregate per pollutant for units = UG/M3
  mutate(day = lubridate::date(datetime)) %>%
  # group by day, filter to pollutant is PM2.5 and unit is UG/M3
  # calculate mean value
  group_by(day) %>%
  filter(pollutant == "PM2.5", unit == "UG/M3") %>%
  summarize(mean_value = mean(value, na.rm = TRUE)) 

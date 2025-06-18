# download_data.R
# A script for downloading all necessary data for this database
# Note: for instructor use only.

library(dplyr)
library(readr)
library(downloader)
library(sf)
library(readxl)
setwd(rstudioapi::getActiveProject())

# Stop message
stop("Don't run this script.\nThis is just for the instructor to download the data. \nSee data/electric_school_buses/README.md for how to use the data.")

# Download the Electric School bus price tracker
read_excel("data/electric_school_buses/raw_file_jan_13_2023.xlsx", sheet = "Price Tracker") %>%
  setNames(nm = names(.) %>% tolower() %>% stringr::str_replace_all(c(" "= "_", 'listed_as_"special_needs"_bus[?]' = "special_needs_bus"))) %>%
  mutate(
    across(.cols = c("bus_manufacturer", "bus_model", "seating_capacity", "special_needs_bus", "vehicle_dealer"),
           .fn = ~if_else(.x == "N/A", true = NA_character_, false = .x))
  ) %>%
  mutate(seating_capacity = as.numeric(seating_capacity)) %>%
  write_csv("data/electric_school_buses/buses.csv")

read_csv("data/electric_school_buses/buses.csv") %>% head(2) %>% glimpse()




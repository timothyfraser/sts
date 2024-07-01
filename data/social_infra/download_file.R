# Basic replication code
# for instructor use only


library(dplyr)
library(readr)
library(ggplot2)
library(sf)

data = read_sf("data/social_infra/tally1kmbg.geojson") %>%
  as_tibble() %>%
  select(-geometry) 

data %>%
  saveRDS("data/social_infra/traits.rds")
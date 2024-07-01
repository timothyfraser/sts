#' @name evacuation/example.rds
#' @title Evacuation Dataset for Hurricane Dorian
#' @description
#' Sourced from Fraser 2022 in Sustainability Science
#' Edges represent Facebook users going from City A to City B


library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(tidygraph)

data = read_rds("data/evacuation/dorian.rds")

edges = data %>%
  activate("edges") %>%
  as_tibble() %>%
  st_as_sf() %>%
  # Sample 1000
  sample_n(size = 1000)

nodes = data %>%
  activate("nodes") %>%
  as_tibble() %>%
  st_as_sf()

ggplot() +
  geom_sf(data = nodes, mapping = aes(fill = social_capital),
          shape = 21, color = "white", size = 4) +
  geom_sf(data = edges, mapping = aes(alpha = evacuation))

rm(list = ls())

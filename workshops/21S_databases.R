#' @name 21S_databases.R
#' @title Solutions for Learning Checks on Network Joins
#' @author Tim Fraser

### LC 1 ###########################################

# How many rides during PM rush hour in 2017 started from a station in a majority Hispanic/Latino block group?
# Use your skills developed in Single Node Join to test it out.
# Hint: use pop_hisplat_2020_smooth5.

# See solutions in 21S_databases.R.

# Load packages
library(dplyr)
library(readr)
library(DBI)
library(RSQLite)
library(stringr)

# Connect to database
db = dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")

# Build a query for your edges
q_edges = db %>%
  # Get edges tallied during rush hour periods
  tbl("tally_rush_edges") %>%
  # zoom into just 2017
  filter(str_sub(day, 1,4) == "2017") %>%
  # zoom into just evening
  filter(rush == "pm") 

q_edges # preview it

# Build a query for your nodes
q_nodes = db %>%
  # Get station dataset
  tbl("stationbg_dataset") %>%
  # Narrow into requried variables
  select(code, pop_hisplat_2020_smooth5) %>%
  # above 50% or below 50%
  mutate(majority = if_else(pop_hisplat_2020_smooth5 > 0.5, "yes", "no")) %>%
  # Narrow into just core variables
  select(code, majority)

q_nodes # preview it

# For each station-pair over time...
q_data = q_edges %>%
  # Classify the edge by the start_code station's status
  # as being in a majority Hispanic/Latino neighborhood or not
  left_join(
    by = c("start_code" = "code"),
    y = q_nodes %>% select(code, start_hisplat = majority))

q_data # preview it

# Aggregate!
# Count up the total number of trips for each classification of the start_code station
q_data %>%
  group_by(start_hisplat) %>%
  summarize(trips = sum(count, na.rm = TRUE))

# Looks like about 2432 trips started in predominantly Hispanic/Latino neighborhoods

# Disconnect
dbDisconnect(db)

# Cleanup!
rm(list = ls())





### LC 2 ###############################################

# During PM Rush Hour in 2017, 
# how many trips went from a majority Hispanic/Latino neighborhood 
# to a majority Non-Hispanic/Non-Latino neighborhood?

# Load packages
library(dplyr)
library(readr)
library(DBI)
library(RSQLite)
library(stringr)

# Connect to database
db = dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")

# Build a query for your edges
q_edges = db %>%
  # Get edges tallied during rush hour periods
  tbl("tally_rush_edges") %>%
  # zoom into just 2017
  filter(str_sub(day, 1,4) == "2017") %>%
  # zoom into just evening
  filter(rush == "pm") 

q_edges # preview it

# Build a query for your nodes
q_nodes = db %>%
  # Get station dataset
  tbl("stationbg_dataset") %>%
  # Narrow into requried variables
  select(code, pop_hisplat_2020_smooth5) %>%
  # above 50% or below 50%
  mutate(majority = if_else(pop_hisplat_2020_smooth5 > 0.5, "yes", "no")) %>%
  # Narrow into just core variables
  select(code, majority)

q_nodes # preview it

# For each station-pair over time...
q_data = q_edges %>%
  # Classify the edge by the start_code station's status
  # as being in a majority Hispanic/Latino neighborhood or not
  left_join(
    by = c("start_code" = "code"),
    y = q_nodes %>% select(code, start_hisplat = majority)) %>%
  # Classify the edge by the end_code station's status
  # as being in a majority Hispanic/Latino neighborhood or not
  left_join(
    by = c("end_code" = "code"),
    y = q_nodes %>% select(code, end_hisplat = majority)) %>%
  # filter out edges without classifications
  filter(start_hisplat != "NA" & end_hisplat != "NA")

q_data # preview it

# Aggregate and collect
q_stat = q_data %>%
  group_by(start_hisplat, end_hisplat) %>%
  summarize(trips = sum(count, na.rm = TRUE))  %>%
  collect()

# Filter result to relevant one
q_stat %>%
  filter(start_hisplat == "yes" & end_hisplat == "no")

# Looks like 525 trips, by my count.

# Remember to disconnect!
dbDisconnect(db)

# Cleanup
rm(list = ls())


### LC 3 ####################################################

# Get the total number of trips during PM rush hour in 2017.
# Using system.time({}), find out how long it takes to collect() that query.
# Compare this against getting the total number of trips during PM rush hour between 2017 and 2021.


# Load packages
library(dplyr)
library(readr)
library(DBI)
library(RSQLite)
library(stringr)

# Connect to database
db = dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")


# Build a query for your edges
q1 = db %>%
  # Get edges tallied during rush hour periods
  tbl("tally_rush_edges") %>%
  # zoom into just evening
  filter(rush == "pm") %>%
  # zoom into just 2017
  filter(str_sub(day, 1,4) %in% c("2017") ) %>%
  # Get total number of trips
  summarize(count = n())

# How long does it take to collect that query?
t1 = system.time({ q1 %>% collect() })



# Build a second query for your edges
q2 = db %>%
  # Get edges tallied during rush hour periods
  tbl("tally_rush_edges") %>%
  # zoom into just evening
  filter(rush == "pm") %>%
  # zoom into just 2017
  filter(str_sub(day, 1,4) %in% c("2017", "2018", "2019", "2020", "2021") ) %>%
  # Get total number of trips
  summarize(count = n())

# How long does it take to collect that query?
t2 = system.time({ q2 %>% collect() })

# Looks like the second query takes 0.43 seconds longer,
# at least on my computer. What about on yours?
t2 - t1


# Remember to disconnect!
dbDisconnect(db)

# Cleanup
rm(list = ls())

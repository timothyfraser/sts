# download_data.R
# A script for downloading all necessary data for this database
# from Cornell's CAT database
# Note: for instructor use only.

library(DBI)
library(RMySQL)
library(dplyr)
library(stringr)
library(readr)

Sys.setenv("CATSERVER_USERNAME"="XXXXXXXXXXXXXX")
Sys.setenv("CATSERVER_PASSWORD"="XXXXXXXXXXXXXXX")
Sys.setenv("CATSERVER_HOST"="XXXXXXXXXXXXXXX")
Sys.setenv("CATSERVER_PORT"="XXXXXXXXXXX")

# Connect to covariates
db = dbConnect(
  drv = RMySQL::MySQL(),
  username = Sys.getenv("CATSERVER_USERNAME"),
  password = Sys.getenv("CATSERVER_PASSWORD"),
  host = Sys.getenv("CATSERVER_HOST"),
  port = as.integer(Sys.getenv("CATSERVER_PORT")),
  dbname = "cov")

# Get the area metadata
db %>% tbl("areas") %>%
  filter(level == "county") %>%
  filter(state == "NY") %>%
  collect() %>%
  saveRDS("data/transportation/areas.rds")

db %>% tbl("metrics") %>% 
  collect() %>%
  saveRDS("data/transportation/metrics.rds")

db %>% 
  tbl("projections") %>%
  filter(str_sub(geoid, 1,2) == "36") %>%
  saveRDS("data/transportation/projections.rds")

db %>% 
  tbl("types") %>%
  saveRDS("data/transportation/types.rds")

db %>% 
  tbl("pollutants") %>%
  saveRDS("data/transportation/pollutants.rds")

db %>% 
  tbl("bys") %>%
  saveRDS("data/transportation/bys.rds")

dbDisconect(db); remove(db)

# Download emissions data
db = dbConnect(
  drv = RMySQL::MySQL(),
  username = Sys.getenv("CATSERVER_USERNAME"),
  password = Sys.getenv("CATSERVER_PASSWORD"),
  host = Sys.getenv("CATSERVER_HOST"),
  port = as.integer(Sys.getenv("CATSERVER_PORT")),
  dbname = "granddata")

# Get NY county table names
t = tibble(table = db %>% dbListTables()) %>%
  filter(str_detect(table, "d36") & table != "d36")

# Get a starter object
bundle = db %>% tbl(t$table[1]) %>% head(0)

# For each county...
for(i in 1:nrow(t)){
  bundle = db %>% tbl(t$table[i]) %>% 
    union(bundle, .)
  print(i)
}

# Save to file
bundle %>% collect() %>%
  saveRDS("data/transportation/emissions.rds")

read_rds("data/transportation/emissions.rds") %>%
  select(geoid) %>%
  distinct()

# Disconnect
dbDisconnect(db)

# Cleanup
rm(list = ls())

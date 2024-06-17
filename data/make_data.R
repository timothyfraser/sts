#' @name make_data.R
#' @author Tim Fraser

# Quick script for writing these tables to file, 
# so we can interact with them more normally.



#' @name make_nycflights
#' @description
#' Save these tables to file and also make a sqlite database
library(dplyr)
library(readr)
library(nycflights13)

nycflights13::airlines %>% write_csv("data/airlines.csv")
nycflights13::airports %>% write_csv("data/airports.csv")
nycflights13::flights %>% write_csv("data/flights.csv")
nycflights13::planes %>% write_csv("data/planes.csv")
nycflights13::weather %>% write_csv("data/weather.csv")


library(RSQLite)
library(DBI)
library(dplyr)

db = dbConnect(drv = RSQLite::SQLite(), "data/nycflights.sqlite")
dbWriteTable(conn = db, name = "airlines", value = read_csv("data/airlines.csv"), row.names = FALSE, overwrite = TRUE) 
dbWriteTable(conn = db, name = "airports", value = read_csv("data/airports.csv"), row.names = FALSE, overwrite = TRUE) 
dbWriteTable(conn = db, name = "flights", value = read_csv("data/flights.csv"), row.names = FALSE, overwrite = TRUE) 
dbWriteTable(conn = db, name = "planes", value = read_csv("data/planes.csv"), row.names = FALSE, overwrite = TRUE) 
dbWriteTable(conn = db, name = "weather", value = read_csv("data/weather.csv"), row.names = FALSE, overwrite = TRUE) 
dbDisconnect(db); remove(db)

#' @name make_gapminder
#' Quick script for making a sqlite database of gapminder data.

library(RSQLite)
library(DBI)
library(dplyr)

db = dbConnect(drv = RSQLite::SQLite(), "data/gapminder.sqlite")

data = gapminder::gapminder
# Write 
dbWriteTable(conn = db, name = "gapminder", value = data, row.names = FALSE, overwrite = TRUE, append = FALSE)

dbDisconnect(db)
rm(list = ls())

# 8C_databases_3.R
# Topic: Make Your First Database!

# In this script, we're going to build your first database!

# We'll use SQLite to build a *local* database, stored in a .sqlite file.
# SQLite is great for small-to-medium size databases, 
# where if you had to interact with a 100 MB csv file all the time, 
# it would be hard, but where it's still small enough to live on your computer. 
# Eg. < 1 GB.

# 0. Setup ##################################################

# Load helper packages
library(readr) # for reading in csv files
library(dplyr) # for %>%, glimpse(), etc.
library(DBI) # for database connections
library(RSQLite) # for using SQLite

# If there's a sqlite database there already, just delete it with unlink()
unlink("data/jp_solar.sqlite")

# 1. Your First Database ###########################################################


library(DBI) # for database connections
library(RSQLite) # for using SQLite

# Connect to database, 
# using DBI's dbConnect() function, with SQLite() driver.
# SQLite doesn't have any passwords or extra settings - 
# it's just a file.
# When no SQLITE database yet exists, 
# dbConnect() will create it.
db = dbConnect(drv = RSQLite::SQLite(), "data/jp_solar.sqlite")

# View the connection object
db

# Get vector of any tables in database (should be empty)
dbListTables(db)

# Disconnect from the database - ALWAYS disconnect when done - 
# otherwise it uses up computer resources.
dbDisconnect(db)


# 2a. Your First Table - Simple ########################################

library(DBI)
library(RSQLite)
library(dplyr)
library(readr)

# Import your data - a dataset where each row describes a city-year.
# (good convention to name tables after what 1 row means.)
cityyears = read_csv("data/jp_solar.csv")

# Let's view our dataset
cityyears %>% glimpse()

# Connect to database, using DBI's dbConnect() function, with SQLite()
db = dbConnect(drv = RSQLite::SQLite(), "data/jp_solar.sqlite")

# Let's write our table to file.
dbWriteTable(conn = db, name = "cityyears", value = units)

# List tables in database
dbListTables(db)

# Make a query, but don't collect() yet - means this is just a snapshot.
q = db %>% tbl("cityyears") %>%
  filter(year == 2019) %>%
  filter(muni_code == "02204")

# View the query
q

# Collect and process the query, returning a table of data
data = q %>% collect()

# View the data
data

# Let's remove the table
dbRemoveTable(conn = db, name = "cityyears")

# Disconnect from the database
dbDisconnect(db)

# Clear environment
rm(list = ls())

# 2b. Your First Table - Extended ###############################################################

# Import your data - a dataset where each row describes a city-year.
# (good convention to name tables after what 1 row means.)
cityyears = read_csv("data/jp_solar.csv")

# Let's view our dataset
cityyears %>% glimpse()

# Connect to database, using DBI's dbConnect() function, with SQLite() driver.
# SQLite doesn't have any passwords or extra settings - it's just a file.
db = dbConnect(drv = RSQLite::SQLite(), "data/jp_solar.sqlite")

# View the connection object
db

# Get a vector of any existing tables (should be none)
dbListTables(db)

# Let's write our table to file.
dbWriteTable(conn = db, name = "cityyears", value = units)

# Check tables!
dbListTables(db)

# View a snapshot of our 'cityyears' table
db %>% tbl("cityyears")

# Make a query, but don't collect() yet - means this is just a snapshot, not a table
db %>% tbl("cityyears") %>% 
  filter(year == 2019) %>%
  filter(muni_code == "02204")

# Make a query and collect() it - run the query and get the actual table.
data = db %>% tbl("cityyears") %>% 
  filter(year == 2019) %>%
  filter(muni_code == "02204") %>%
  collect()
# Tada! Here's your queried table!
data

# But this table has some issues, so let's actually drop it. 
# Specifically, the date field wasn't encoded correctly, 
# so we ended up with numbers rather than dates in the date column.
# Let's remove the table
dbRemoveTable(conn = db, name = "cityyears")

# Confirm - no more table 'cityyears'!
dbListTables(db)

# Disconnect from the database
dbDisconnect(db)

# Clear environment
rm(list = ls())

# 3. Using Field Types ##############################################################

# Let's make a new table, but this time, we'll use fieldtypes!

# We want to use as little storage space as possible with databases,
# so we often provide 'fieldtypes' = a list of column names, types, and lengths.
# This means that SQL-systems don't overallocate space to, say, an integer that only goes from 0 to 9.

# All SQL-related platforms have field type documentation online.
# You can read about SQLite's field types here: 
# SQLite Field Types
# https://www.sqlite.org/datatype3.html

# Import our cityyears dataset
cityyears = read_csv("data/jp_solar.csv") %>%
  # but THIS time, make date include the hours:minutes:seconds, as a character vector
  # SQLite recognizes it as date-time
  mutate(date = paste0(date, " 00:00:00"))

# Connect to database, using DBI's dbConnect() function, with SQLite() driver.
db = dbConnect(drv = RSQLite::SQLite(), "data/jp_solar.sqlite")

# Add the table - use overwrite = TRUE if you're concerned there's a previous version of the table
dbWriteTable(conn = db, name = "cityyears", value = cityyears, overwrite = TRUE)

# Try filtering by date - it should work!
db %>% tbl("cityyears") %>% filter(date > "2019-01-01")


# Next, let's try to make actual fieldtypes, to be more specific.

# We'll write ourselves a named vector called 'fieldtypes'
# It's named, in that each value (eg. "CHAR(5)" has a name eg. "muni_code")
# "muni_code" = "CHAR(5)" means make the "muni_code" column a text/character field with a max of 5 spaces.
# CHAR, INT, and BLOB are very common across SQL platforms, and SQLite recognizes them.

# We can check the max length of each field with nchar() and max()
cityyears$date %>% nchar() %>% max()
cityyears$muni_code %>% nchar() %>% max()
cityyears$year %>% nchar() %>% max()
cityyears$solar_under_10kw %>% range()
cityyears$disaster %>% range()
cityyears$pop %>% range()
cityyears$solar %>% range()
cityyears$solar_rate %>% range()

fieldtypes = c(
  "muni_code" = "CHAR(5)", "date" = "CHAR", "year" = "INT(4)",
  "solar_under_10kw" = "INT(5)", "disaster" = "INT(1)",
  "pop" = "INT(6)", "solar" = "5", "solar_rate" = "REAL")


# Need to overwrite that table? use overwrite = TRUE
dbWriteTable(conn = db, name = "cityyears", value = cityyears, fieldtypes = fieldtypes, overwrite = TRUE)

# Test it - should work!
db %>% tbl("cityyears") %>% filter(date > "2019-01-01")

# And you should be able to filter with it as a data.frame too.
db %>% tbl("cityyears") %>% filter(date > "2019-01-01") %>% head(2) %>% 
  collect() %>%
  filter(date > "2019-06-01")

# Disconnect
dbDisconnect(db)

# Clear Environment
rm(list = ls())


# 4. Adding a Second Table ###########################################################

# Databases are collections of tables. 
# It's very advantageous for us to make multiple tables if it will save us time.

# Let's create some extra tables that might be of interest to us.

# Connect to database using SQLite()
db = dbConnect(drv = RSQLite::SQLite(), "data/jp_solar.sqlite")

# Let's write a query to get a table of all unique cities, 
# tallying up the total number of solar panels
query = db %>% tbl("cityyears") %>%
  # Group by city
  group_by(muni_code) %>%
  # For each city, return 1 row containing...
  summarize(
    solar_under_10kw  = sum(solar_under_10kw, na.rm = TRUE),
    # total solar farms
    solar = sum(solar, na.rm = TRUE),
    # Population - there's only 1 population per city, so it works out
    pop = pop,
    # Disaster - there's only 1 disaster status per city, so it works out
    disaster = disaster)

# Get the data
data = query %>% collect()

# Write the data to a new table, called 'cities'
dbWriteTable(conn = db, name = "cities", value = data, overwrite = TRUE)

# If we were being really precise, we could add some basic fieldtypes
fieldtypes = c("muni_code" = "CHAR(5)", "solar_under_10kw" = "INT(5)",  "solar" = "5", 
               "disaster" = "INT(1)", "pop" = "INT(6)")
# Write the data to a new table, called 'cities'
dbWriteTable(conn = db, name = "cities", value = data, fieldtypes = fieldtypes, overwrite = TRUE)


# Check existing tables
dbListTables(db)

# Check out your new cities table!
db %>% tbl("cities")

# Disconnect from database
dbDisconnect(db)

# Clean up environment
rm(list = ls())


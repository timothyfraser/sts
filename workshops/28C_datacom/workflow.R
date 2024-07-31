# workflow.R
# A script for testing out our workflow.

# Set working directory
setwd("/cloud/project/workshops/28C_datacom")

# Test 1 ################################

# Can we connect to the bluebikes database?
db = dbConnect(drv = RSQLite::SQLite(), "bluebikes.sqlite")

db %>% dbListTables()

# How long does it take to query it?
system.time({
  db %>%
    tbl("tally_rush_edges") %>%
    filter(rush == "pm", day == "2011-09-30") %>%
    collect()
})

# Test 2 ################################

# How can we handle BIG queries for Shiny Apps?

# Option 1: Filter It and Deal with It in realtime
db %>%
  tbl("tally_rush_edges") %>%
  filter(rush == "pm", day == "2011-09-30") %>%
  mutate(year = stringr::str_sub(day, 1,4)) %>%
  group_by(year, start_code, end_code) %>%
  summarize(trip = n()) 

# Option 2: Preprocess Your Data and Load it in.
db %>%
  tbl("tally_rush_edges") %>%
  filter(rush == "pm", day == "2011-09-30") %>%
  mutate(year = stringr::str_sub(day, 1,4)) %>%
  group_by(year, start_code, end_code) %>%
  summarize(trip = n()) %>%
  collect() %>%
  saveRDS("28C_datacom/helper_data.rds")


dbDisconnect(db)


# Test 3 ################################

# First Debugging Tip: Check - can we connect to the database?

# Connect to database
db = dbConnect(drv = RSQLite::SQLite(), "bluebikes.sqlite")
# List tables
db %>% dbListTables()
# Disconnect
dbDisconnect(db)

# Test 4 ################################

# Second Debugging Tip: Can we reference an input and get the desired result?

# Can we successfully run a query with a test input?

# Testing input list values...
input = list(year= "2020")
# Connect to database
db = dbConnect(drv = RSQLite::SQLite(), "bluebikes.sqlite")

data = db %>%
  tbl("tally_rush_edges") %>%
  filter(rush == "pm") %>%
  mutate(year = stringr::str_sub(day, 1,4)) %>%
  filter(year == input$year) %>%
  head(1) %>%
  collect()

# Disconnect
dbDisconnect(db)
# View
data
# Return a single value for a test output
data$year[1]





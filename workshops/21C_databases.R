#' @name 21C_databases.R
#' @author Tim Fraser
#' @title Network Joins with Micromobility Networks



# 0. Background ##############################

# We're going to use this SQLite database to access 
# a refined version of our data. SQLite is really handy because 
# we can ask it to do lots of number-crunching 
# without loading all the millions of rows into our R environment, 
# which would normally cause it to crash. 
# Instead, we can feed SQLite basic dplyr functions, 
# like ```select()```, ```mutate()```, ```filter()```, 
# ```group_by()```, and ```summarize()```, 
# and then ask the SQLite database to ```collect()``` 
# the resulting data and give it to us in R. 
# This output (should) be much, much, much smaller, 
# at a size R can handle.

# Our data is saved in `data/bluebikes`


## Unzip ##########################

# First, remember to unzip the bluebikes dataset,
# using the code shared in setup_bluebikes.R, reproduced below.
# unzip("data/bluebikes/bluebikes.zip", junkpaths = TRUE, exdir = "data/bluebikes")


# Here's our data:

## Datasets ############################

# - `data/bluebikes/bluebikes.sqlite`: a HUGE compendium of datasets.
# These files are a little too big to access on their own, 
# so we will access them via the SQLite database. Contains:

# --- `tally_rush_edges` (in `bluebikes.sqlite`): a dataset tallying 
# number of rides each day during morning and evening rushhour.

# --- `tally_rush` (in `bluebikes.sqlite`): a dataset tallying number 
# of rides each day during morning and evening rushhour, for EACH START AND END STATION. That's a LOT!

#  - `data/bluebikes/stationbg_dataset.rds`: an `sf` dataset of 
# geolocated points for all bluebike stations present in our data. Contains the `geoid` of the census block group each station is located in. Also contains all the traits of that block group!


#  Others You might run into, but don't need to think about as much.

# - `data/bluebikes/dates.rds`: a dataset of dates and days of the week
# for the past 10 years. Useful for filtering, but not strictly necessary.

# - `data/bluebikes/bgdataset.rds`: an `sf` dataset of all census block group
# polygons in Boston. Contains all the traits of each block group.


## Codebook ##########################

# Here's a quick summary of all variable names you will run into.

# - `code`: unique ID for each bluebikes station.
# 
# - `geoid`: unique ID for each census block group.
# 
# - `count`: total rides occuring during that time, between those places.
# 
# Example Demographics Variable Set:
#   
#   - `pop_density_2020`: population density per square kilometer in 2020. Some places are missing data.
# 
# - `pop_density_2020_smooth5`: population density, with missing data filled in by taking the median of their 5 nearest neighboring census block groups.
# 
# - `pop_density_2020_smooth10`: population density, with missing data filled in by taking the median of their 10 nearest neighboring census block groups. Either is fine to use.


### Demographics ###############################
#
# - `pop_white_2020`: % of white residents (you can make pop_nonwhite_2020 by taking 1 - pop_white_2020)
#
# - `pop_black_2020`: % of Black residents
#
# - `pop_hisplat_2020`: % of Hispanic/Laitno residents
# 
# - `pop_asian_2020`: % of Asian residents
# 
# - `pop_natam_2020`: % of Native American residents
# 
# 

### Socioeconomics ################################
# 
# - `pop_0_40000_2019`: % of families earning 0-40K a year.
# 
# - `pop_40001_60000_2019`: % of families earning 40-60K a year.
# 
# - `pop_60001_100000_2019`: % of families earning 60-100K a year.
# 
# - `pop_100000_plus_2019`: % of families earning 100K+ a year.
# 
# - `pop_some_college`: % of residents with some college education or more.
# 
# - `pop_employed_2019`: % of residents employed, out of total population.


# I generated many other variables too, 
# saved in `data/bluebikes/stationbg_dataset.rds.`
# Let's look at them real quick.
read_rds("data/bluebikes/stationbg_dataset.rds") %>% 
  select(-contains("smooth")) %>%
  names()


# 0. Setup #################################

## 0.1 Load Packages ##################################

library(dpylr) # data wrangling
library(readr) # reading data
library(ggplot2) # visualizing data
library(DBI) # for databases
library(dbplyr) # data wrangling for databases
library(RSQLite) # for SQLite
library(stringr) # for string manipulation

## 0.2 Loading bluebikes ##############################

# Let's investigate our data.

# Tell R to hire a 'SQL translator' object, which we'll name 'db',
# sourced from our bluebikes data
db <- dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")

# What tables are in our database?
db %>% dbListTables()

# Let's investigate them.

### stationbg_dataset ######################################

# stationbg_dataset is a table of bluebikes stations 
# and the traits of the block group they are located in.
db %>% tbl("stationbg_dataset")

# code - bluebikes station unique ID
# geoid - census block group unique ID, in which bluebikes station is located
# x - longitude of station in WGS projection
# y - latitute of station in WGS projection
# pop_2020 - population in 2020
# pop_density_2020 - population density per square kilometer in 2020.
# pop_white_2020 - % White residents in 2020
# pop_black_2020 - % Black residents in 2020
# pop_asian_2020 - % Asian residents in 2020
# pop_natam_2020 - % Native American residents in 2020
# pop_hisplat_2020 - % Hispanic/Latino residents in 2020
# pop_women_2019 - % women in 2019
# pop_over_65_2019 - % residents over age 65 in 2019
# pop_some_college - % residents with some college education in 2019
# pop_0_40000_2019 - % households earning 0-40K in 2019
# pop_40001_60000 - % households earning 40-60K in 2019
# pop_60001_100000_2019 - % households earning 60-100K in 2019
# pop_100000_plus - % households earning >100K in 2019
db %>% tbl("stationbg_dataset") %>%
  glimpse()

### tally_rush_edges ##############################

# `tally_rush_edges` is a table tallying the number of riders 
# traveling between bluebikes stations
# during a specific day and rush hour period (AM or PM)
db %>% tbl("tally_rush_edges")

# start_code - unique ID of bluebikes station where the ride started
# end_code - unique ID of bluebikes station where the ride ended
# day - day of ride
# rush - AM or PM rush hour
# count - total riders


### tally_rush #####################################

# `tally_rush` is a table tallying the number of bluebikes riders (count)
# during AM or PM rush hour (rush) per day (day)
db %>% tbl("tally_rush")

# It is an aggregation of `tally_rush_edges`



### dates ##########################################

# `dates` is a table of helper data corresponding to each date, 
# including year and weekday.
db %>% tbl("dates")



## 0.3 Example Query #####################

db %>%
  # Tell R to look in the tally_rush dataset in our SQLite database
  # It will return the first 1000 rows
  tbl("tally_rush") %>%
  # Grab first 6 rows
  head()

## 0.4 Collecting Data #####################

mine <- db %>%
  # Tell R to look in the tally_rush dataset in our SQLite database
  # It will return the first 1000 rows
  tbl("tally_rush") %>%
  # Grab first 6 rows
  head() %>%
  # extract those five rows to be used
  collect()

# Check it out!
mine

## 0.6 Handling Dates ####################################

### Using ```str_sub()``` ####################

# `str_sub()`, also known as string-sub, 
# is an amazing function loaded within the `stringr` package. 
# It allows you to extract part of your entry, 
# based on the number of characters.
# It's great for extracting the year, month, day, census code, etc. 
# out of structured text. 

# Here's a quick example.

# Let's make an example dataset
mydates <- data.frame(day = c("2020-10-11", "2021-05-02", "2021-02-03"))

mydates


# We can extract the first four letters
# by setting `start = 1` and `end = 4`.

mydates$day %>% str_sub(start = 1, end = 4)
# Though you could also write it like this
# str_sub(mydates$day, start = 1, end = 4)

# You could also write it like this, 
# within `mutate()`.

mydates %>%
  mutate(day = str_sub(day, start = 1, end = 4))

# Finally, you could systematize a bunch of info 
# in multiple columns!

mydates %>%
  mutate(
    # let's get the year
    y = str_sub(day, start = 1, end = 4),
    # then the month
    m = str_sub(day, start = 6, end = 7),
    # then the day!
    d = str_sub(day, start = 9, end = 10))

#Handy, right?

### Summarizing Data with Dates ########################
db %>%
  tbl("tally_rush") %>%
  # Zoom into October, using the 6th and 7th characters in the day vector
  filter(str_sub(day, start = 6, end = 7) == "10")  %>%
  # Zoom into just am rush hour traffic
  filter(rush == "am") %>%
  # Count how many rows (days) is in that?
  summarize(count = n()) %>%
  # collect response - 243 days!
  collect()


### Visualize Collected Data by Date ##################
myviz <- db %>%
  tbl("tally_rush") %>%
  # Zoom into October, using the 6th and 7th characters in the day vector
  filter(str_sub(day, start = 6, end = 7) == "10")  %>%
  # Zoom into just am rush hour traffic
  filter(rush == "am") %>%
  # collect response - 243 days!
  collect() %>%
  # If you ever work with date data, 
  # you WILL have to transform it from character into date format
  mutate(day = as.Date(day))



myviz %>% 
  ggplot(mapping = aes(x = day, y = count)) +
  geom_jitter(size = 3, color = "purple", alpha = 0.5) +
  labs(subtitle = "Yay October!")

dbDisconnect(db)


# 1. Querying Network Data ############################

## 1.1 Querying Edges #################################

# The big one we need to worry about file size for
# with is `tally_rush_edges`. 
# It's also the coolest!

# Connect to db
db = dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")


### Build your Query ###############

# When working with REALLY BIG DATASETS,
# it's a good idea to wait until you've finalized
# your query before using collect() to collect the data.

# Step by step, construct your query of a table, but don't collect it yet.

db %>%
  tbl("tally_rush_edges") %>%
  # zoom into just 2021
  filter(str_sub(day, 1,4) == "2021") %>%
  # zoom into just morning
  filter(rush == "am")

### Check Query Size ###########################################

# It's much easier to summarize data and collect a small number of rows 
# than to collect a ton of rows at once.
# Even easier is to summarize data and preview the results, without collecting

# This means that it's very easy to check sample size.

# How many edges in this dataset?
db %>%
  tbl("tally_rush_edges") %>%
  summarize(count = n())
# >5 million!!!

# How many edges in 2021?
db %>%
  tbl("tally_rush_edges") %>%
  filter(str_sub(day, 1,4) == "2021")  %>%
  summarize(count = n())
# >400,000!!!

# How many edges in 2021 am rush hour periods?
db %>%
  tbl("tally_rush_edges") %>%
  filter(str_sub(day, 1,4) == "2021")  %>%
  filter(rush == "am") %>%
  summarize(count = n())
# >27,000!!!


# It's just ~20K rows, so let's collect it. I'd avoid going above 50K.
edges = db %>%
  tbl("tally_rush_edges") %>%
  # zoom into just 2021
  filter(str_sub(day, 1,4) == "2021") %>%
  # zoom into just morning
  filter(rush == "am") %>%
  collect() 

### Time Benchmarking ##################

# How long will your query take?

# Might be worth checking if you're doing this query for a dashboard.
# Look at the 'elapsed' cell
system.time({
  db %>%
    tbl("tally_rush_edges") %>%
    # zoom into just 2021
    filter(str_sub(day, 1,4) == "2021") %>%
    # zoom into just morning
    filter(rush == "am") %>%
    collect() 
})
# On my computer, it's about 1.14 seconds. How about on your computer?
# This will depend on your computer's / posit.cloud project's RAM.


# Disconnect
dbDisconnect(db)



# That's a lot of data! Let's look at just a few rows.
edges %>% head()

# `start_code` - we've got the unique identifier
#               for each bluebikes station 
#               that a person checked a bike out from. 
# - `end_code` shows the station they returned 
#            the bike to afterwards. 
# - `day` shows that date it all occurred during. 
# - `rush indicates whether it happened during rush hour in the ```"am"``` or ```"pm"```. 
# - `count` is the number of rides (roughly = people).

# Great! We've got the edges in this network, weighted by number of rides.


## 1.2 Querying Node Data ###############################

# Next, we're going to gather the nodes in this network.
# These nodes are bluebikes stations, each located in a geoid.

db = dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")

# Check variables available
db %>%
  tbl("stationbg_dataset") %>%
  glimpse()

# Let's get the block group population of African American residents where each station is.
# Compare two versions of our variable - here, we have a lot of missing data,
# so we used spatial interpolation from the 5 neighboring block groups 
# to estimate each block group's percentage. 
# (See! Our spatial smoothing from last week does matter!)
# This is called pop_black_2020_smooth5.

### Build test query #########################################

# Check how many nodes - okay that's a doable size, no sampling needed
db %>% tbl("stationbg_dataset") %>% summarize(count = n())

# Build a test query, where we will create a binary classifier 
# for each station
db %>%
  tbl("stationbg_dataset") %>%
  select(code, geoid, pop_black_2020_smooth5) %>%
  # And let's classify it as 
  # above 50% or below 50%
  mutate(maj_black = if_else(pop_black_2020_smooth5 > 0.5, "yes", "no")) 

### Collect Query ######################

# Complete the query and download the nodes
nodes = db %>%
  tbl("stationbg_dataset") %>%
  select(code, geoid, pop_black_2020_smooth5) %>%
  mutate(maj_black = if_else(pop_black_2020_smooth5 > 0.5, "yes", "no")) %>%
  collect()

# Disconnect
dbDisconnect(db)

# 2. Network Joins ########################################

# Okay, so we've got edge and node data...
# but it only will help us if we can combine it in ways useful to us.

# All edges have a 'from' node, a 'to' node, and a 'weight'.
# In this case, our network is temporal.
# -- our 'from' node is the 'start_code'
# -- our 'to' node is the 'end_code'
# -- our weight is 'count'
# -- each 'day'-'rush' pair forms its own network.

edges %>% head()

## Single Node Join #####################################

# We can join in the traits of our nodes to our edge dataset,
# using the traits of our source/from/start node.
# To do so, we need to specifically state
# the name of the ID variable in edges ("start_code")
# and the name of the ID variable in nodes ("code")
# which we will join by.
# These don't have to be the same; we can link them by saying
# by = c("start_code" = "code")

edges %>%
  left_join(by = c("start_code" = "code"), y = nodes)

# Now we have a LOT of information!

# In fact, it's probably a better idea to do as small a join as possible.

# Join in using the node 'code' JUST the variable `maj_black`
edges %>%
  left_join(
    by = c("start_code" = "code"),
    y = nodes %>% select(code, maj_black))

# It probably will help us to actually rename 
# our node variables before joining them in,
# so that it's clear that this is actually 
# the variable maj_black pertaining to the start_code.

edges %>%
  left_join(
    by = c("start_code" = "code"),
    # Select and rename variables...
    y = nodes %>% select(code, start_black = maj_black))

# What can we do with that information?


# How many rides started from a station
# in a majority Black neighborhood (block group)?
edges %>%
  left_join(by = c("start_code" = "code"), 
            y = nodes %>% select(code, start_black = maj_black)) %>%
  group_by(start_black) %>%
  summarize(count = n())

# I'm seeing 256 rides in this year (2011) started in majority Black neighborhoods.

# Handy side benefit: if start_black or end_black == NA,
# that means the start_code station or end_code station 
# are not in boston proper (eg. maybe in Cambridge or beyond.) 
# I'd like to zoom into Boston, for the time being.







## Double-Node Joins ########################################

# Next, let's try joining in node data BOTH for
# traits of the starting station neighborhood and
# traits of the ending station neighborhood

# It becomes REALLY important to rename variables here;
# we'll call our varibles 
# start_black - % Black in 2020 in start station block group
# end_black - % Black in 2020 in end station block group

# Build a query object
data = edges %>%
  # Join in start node traits
  left_join(
    by = c("start_code" = "code"),
    y = nodes %>% select(code, start_black = maj_black)) %>%
  # Join in end node traits
  left_join(
    by = c("end_code" = "code"),
    y = nodes %>% select(code, end_black = maj_black)) 

# View a few rows
data %>% head()




## Join BEFORE Collecting #################################

# Finally, it might be more efficient for you to actually perform your join
# BEFORE collecting data.
# This is only possible if both tables being joined are from the same database.

# Let's try it.

# Connect
db = dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")

# Build an edges query - but don't collect it!
q_edges = db %>%
  tbl("tally_rush_edges") %>%
  # zoom into just 2021
  filter(str_sub(day, 1,4) == "2021") %>%
  # zoom into just morning
  filter(rush == "am")

# Build a nodes query - but don't collect it!
q_nodes = db %>%
  tbl("stationbg_dataset") %>%
  # Narrow columns
  select(code, geoid, pop_black_2020_smooth5) %>%
  # Classify it as above 50% or below 50%
  mutate(maj_black = if_else(pop_black_2020_smooth5 > 0.5, "yes", "no"))  %>%
  # Select ONLY final variables you need for joining
  select(code, maj_black)


# Build the joining query, using q_edges and q_nodes as if they were real tables
q_data = q_edges %>%
  # Join in start node traits
  left_join(
    by = c("start_code" = "code"),
    y = q_nodes %>% select(code, start_black = maj_black)) %>%
  # Join in end node traits
  left_join(
    by = c("end_code" = "code"),
    y = q_nodes %>% select(code, end_black = maj_black)) %>%
  # Remove rows where start_black == NA or end_black == NA
  filter(start_black != "NA" & end_black != "NA")


# Time Benchmark the Query
system.time({
  q_data %>% collect()
})

# Collect the query!
data = q_data %>% collect()

# View it
data  %>% head()




## Join & Aggregate BEFORE Collecting ###########################

# Finally, what if we want to do some aggregation?
# We could optionally have our database do the aggregation before collecting,
# which would reduce the amount of data that has to get sent.

# For example, suppose we were measuring frequency:
# How often do people bike between 
# predominantly Black neighborhoods?
# Let's find out, using ```group_by()``` and ```summarize()```. 
# Let's add up the total trips (stored in ```count```) 
# for each subgroup.

# Let's use our q_data query we made before;
# we'll made a new q_stat query 
q_stat = q_data %>%
  group_by(start_black, end_black) %>%
  summarize(trips = sum(count, na.rm = TRUE)) 

# We can preview the result 
q_stat

# And we can test how long would it take to process the entire request?
system.time({
  q_stat %>% collect()
})

# And we could collect it like so
stat = q_stat %>% collect()

# View it!
stat

# Suppose we have some extra formatting to do.
# This is a good task to have R do after collect()-ing the data.
stat %>%
  ungroup() %>%
  mutate(total = sum(trips),
         percent = trips / total) %>%
  # Clean up the percentages
  mutate(percent = round(percent*100, digits = 1))


# Disconnect
dbDisconnect(db)

# Clean up
rm(list = ls())


# 3. Learning Checks #####################################


### LC 1 ###########################################

# How many rides during PM rush hour in 2017 started from a station in a majority Hispanic/Latino block group?
# Use your skills developed in Single Node Join to test it out.
# Hint: use pop_hisplat_2020_smooth5.

# See solutions in 21S_databases.R.




### LC 2 ###############################################

# During PM Rush Hour in 2017, 
# how many trips went from a majority Hispanic/Latino neighborhood 
# to a majority Non-Hispanic/Non-Latino neighborhood?


# See solutions in 21S_databases.R.

### LC 3 ####################################################

# Get the total number of trips during PM rush hour in 2017.
# Using system.time({}), find out how long it takes to collect() that query.
# Compare this against getting the total number of trips during PM rush hour between 2017 and 2021.


# See solutions in 21S_databases.R.






#' @name data/bluebikes/examples.R
#' @title Examples for Bluebikes Dataset
#' @author Tim Fraser
#' @description
#' Here's some background information and examples
#' for working with the Bluebikes dataset
#' which will help you get comfortable with this big dataset.



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

# Our data is saved in ```data/bluebikes```. 
# Please use this folder for all your data needs. 


## Unzip ##########################

# First, remember to unzip the bluebikes dataset,
# using the code shared in setup_bluebikes.R, reproduced below.
# unzip("data/bluebikes/bluebikes.zip", junkpaths = TRUE, exdir = "data/bluebikes")


# Here's our data:

## Datasets ############################

# - `data/bluebikes/bluebikes.sqlite`: a HUGE compendium of datasets.
# These files are a little too big to access on their own, 
# so we will access them via the SQLite database. Contains:

  # - `tally_rush_edges` (in `bluebikes.sqlite`): a dataset tallying 
  # number of rides each day during morning and evening rushhour.
  
  # - `tally_rush` (in `bluebikes.sqlite`): a dataset tallying number 
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


# 1. View Data #######################################

## Packages ####################

library(dpylr)
library(readr)
library(ggplot2)
library(RSQLite)
library(stringr)
library(lubridate)

## Loading bluebikes ##############################

# Let's investigate our data.

# Tell R to hire a 'SQL translator' object, which we'll name 'db',
# sourced from our bluebikes data
db <- dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")

## Example Query #####################

db %>%
  # Tell R to look in the tally_rush dataset in our SQLite database
  # It will return the first 1000 rows
  tbl("tally_rush") %>%
  # Grab first 6 rows
  head()

## Collecting Data #####################

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


## Using ```str_sub()``` ####################

# `str_sub()`, also known as string-sub, 
# is an amazing function loaded within the ```stringr``` package. 
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

## Summarizing Data
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

## Visualize Collected data ##################
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
  mutate(day = as.POSIXct(day))


myviz %>% 
  ggplot(mapping = aes(x = day, y = count)) +
  geom_jitter(size = 3, color = "purple", alpha = 0.5) +
  labs(subtitle = "Yay October!")


# 2. Network Data ############################

# The big one we need to worry about file size
# with is ```tally_rush_edges`. 
# It's also the coolest!

myedges <- db %>%
  tbl("tally_rush_edges") %>%
  # zoom into just 2021
  filter(str_sub(day, 1,4) == "2021") %>%
  # zoom into just morning
  filter(rush == "am") %>%
  # It's just 20000 rows, so let's collect it. I'd avoid going above 50K.
collect() %>%
  as_tibble()

# How many rows?
myedges %>%
  summarize(count = n())

# Wow! That's a lot of data! Let's look at just a few rows.
myedges %>% head()

DBI::dbDisconnect(db)

# `start_code` - we've got the unique identifier
#               for each bluebikes station 
#               that a person checked a bike out from. 
# - `end_code` shows the station they returned 
#            the bike to afterwards. 
# - `day` shows that date it all occurred during. 
# - `rush indicates whether it happened during rush hour in the ```"am"``` or ```"pm"```. 
# - `count` is the number of rides (roughly = people).


mystations <- read_rds("data/bluebikes/stationbg_dataset.rds") %>%
  # Let's get the block group population of African Americans where each station is,
# using our spatially smoothed estimate, 'pop_black_2020_smooth5'
select(code, pop_black_2020_smooth5) %>%
  # And let's classify it as 
  # above 50% or below 50%
  mutate(maj_black = if_else(pop_black_2020_smooth5 > 0.5, "yes", "no")) %>%
  # convert to tibble, 
  as_tibble() %>%
  select(code, maj_black)

mystations %>% head()

# 3. Mobility Patterns ###########################

# Finally, let's join in our demographic traits
# to get a glimpse of what this mobility network 
# looks like in terms of which neighborhoods 
# are getting connected 
# - eg. *who* rides? 
# We will classify each station based on 
# its block group's traits!
  
mydemo <- myedges %>%
  # We can join in the source traits,
  # Eg. whether the station is in a majority Black neighborhood
  left_join(by = c("start_code" = "code"), 
            y = mystations %>% select(code, start_black = maj_black)) %>% 
  # We can join in the destination traits,
  # Eg. whether that station is in a majority Black neighborhood
  left_join(by = c("end_code" = "code"), 
            y = mystations %>% select(code, end_black = maj_black)) 

# Handy side benefit: if start_black or end_black == NA, that means the start_code station or end_code station are not in boston proper (eg. maybe in Cambridge or beyond.) 
# I'd like to zoom into Boston, for the time being.
mydemo %>% head()

# You can repeat this process with *any* of the demographic variables 
# in ```stationbg_dataset.rds``` 
# (as long as it makes sense. Rule of thumb: simple analyses are usually better 
# and closer to the truth).

## Analyzing Frequency ###############################

# How often do people bike between 
# predominantly Black neighborhoods?
# Let's find out, using ```group_by()``` and ```summarize()```. 
# Let's add up the total trips (stored in ```count```) 
# for each subgroup.

mydemo %>%
  group_by(start_black, end_black) %>%
  summarize(trips = sum(count, na.rm = TRUE))

# This is very cool, but we should probably
# deal with those NAs. The NAs refer to stations 
# outside of Boston - to keep this manageable,
# we're just looking at movement within Boston. 
# We can ```filter()``` out ```"NA"``` observations like this:

mydemo %>%
  # Remove rows where start_black == NA or end_black == NA
  filter(start_black != "NA" & end_black != "NA") %>%
  group_by(start_black, end_black) %>%
  summarize(trips = sum(count, na.rm = TRUE))


mytab <- mydemo %>%
  # Remove rows where start_black == NA or end_black == NA
  filter(start_black != "NA" & end_black != "NA") %>%
  group_by(start_black, end_black) %>%
  summarize(trips = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(trips),
         percent = trips / total) %>%
  # Clean up the percentages
  mutate(percent = round(percent*100, digits = 1))

# view it!
mytab


# So that's... disquieting. Yikes. 
# I mean, it's an individual choice to ride or not, 
# but when a public program sees low rates of participation 
# from neighborhoods of color as small as that, 
# it's not really an equitable policy outcome.


## Visualize it! ###################################

### Bar Charts! ############################

# It's bar-chart time! So many bar charts!

mytab %>%
  ggplot(mapping = aes(x = start_black, y = trips, fill = end_black)) +
  geom_col(position = "fill") +
  labs(subtitle = "What % of Rides that started out in Black Neighborhoods\nended in Black Neighborhoods?",
       y = "% of Rides",
       x = "Starting Station\nin Majority Black Neighborhood?",
       fill = "Ending Station\nin Majority\nBlack\nNeighborhood?")


### Filtered Bar Charts! #######################

# Sometimes filtered bar charts are more intuitive to read.

mytab %>%
  mutate(type = paste(start_black, "->", end_black)) %>%
  ggplot(mapping = aes(x = type, y = percent)) +
  geom_col() +
  geom_text(mapping = aes(label = percent), vjust = 0, nudge_y = 2) +
  labs(subtitle = "What % of Rides that started out in Black Neighborhoods\nended in Black Neighborhoods?",
       y = "% of Rides",
       x = "Starting Neighborhood >50% Black? -> Ending Station >50% Black?")


### Heat Maps! #############################

# Another great option is ```geom_tile()```, also known as heatmaps.

library(viridis)
mytab %>%
  select(start_black, end_black, percent) %>%
  ggplot(mapping = aes(x = start_black, y = end_black, fill = percent, label = percent)) +
  geom_tile(color = "white") +
  geom_text(size = 8) +
  # Viridis is back!!!
  scale_fill_viridis(option = 'mako', begin = 0.2, end = 0.8) +
  theme_classic(base_size = 14) +
  labs(x = "Starting Station\nin Majority Black Neighborhood?", 
       y = 'Ending Station\nin Majority Black Neighborhood?',
       subtitle = "% of BlueBikes Ridership between Black/White Neighborhoods")



# 4. Networks over Time ##############################

# Oh hang on, didn't we have this data for every place over time?
  
# Load Packages
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(RSQLite)
library(DBI)

# Tell R to hire a 'SQL translator' object, which we'll name 'mydat',
# sourced from our bluebikes data
db <- dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")

mytime <- db %>%
  tbl("tally_rush_edges") %>%
  # zoom into just morning
  filter(rush == "am") %>%
  # drop unnecessary variables, to keep this dataset as small as possible
  select(-rush) %>%
  collect()

dbDisconnect(db); remove(db)

# WOW! that's a huge dataset. We hit 2 million rows!!!
# This is all the station-pairs from 2011 to 2021.
# mytime %>% head()  



# Okay, this is going to be rough, but here we go!
  
# We're going to join in demographics,
# filter to valid cases, and get our percentages for every year.

mytimedemo <- mytime %>%
  left_join(by = c("start_code" = "code"), 
            y = mystations %>% 
              select(code, start_black = maj_black)) %>% 
  # We can join in the destination traits,
  # Eg. whether that station is in a majority Black neighborhood
  left_join(by = c("end_code" = "code"), 
            y = mystations %>% 
              select(code, end_black = maj_black))  %>%
  # We can join in the source traits,
  # Remove rows where start_black == NA or end_black == NA
  filter(start_black != "NA" & end_black != "NA") %>%
  # Now count up by YEAR
  group_by(year = str_sub(day, 1, 4),
           # and by start and end type
           start_black, end_black) %>%
  summarize(trips = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  # Now for each year, calculate percentage
  group_by(year) %>%
  mutate(total = sum(trips),
         percent = trips / total) %>%
  # Clean up the percentages
  mutate(percent = round(percent*100, digits = 1))


# and let's quickly get rid of that 2 million row dataset so our computer can breath easy.
remove(mytime)

# Check it out!!!!
  
  
mytimedemo

# Let's finish up by visualizing some equity questions over time!

mytimedemo %>%
  # Zoom into just majority non-black-non-black pairs
  filter(start_black == "no", end_black == "no") %>%
  ggplot(mapping = aes(x = year, y = percent, group = 1)) +
  geom_point(size = 5) +
  geom_line() +
  theme_classic(base_size = 14) +
  labs(y = "% Trips between Majority Non-Black Neighborhoods,\nout of Total Trips",
       x = "Year",
       subtitle = "Very tiny changes in Demographic Mobility\nin BlueBikes Program over Time")

# Conclusion #######################

# Using these basic tools, we constructed a series of 
# preliminary evaluations to explore the demographic 
# and socioeconomic breakdown of Bluebikes ridership,
# with a focus on equity. 

# These help us answer questions like: 
# Is the program reaching a 
# broad slice of the population? Or not so much? 
# When and where is it succeeding vs. struggling?


# Let's Cleanup 
rm(list = ls()); gc()

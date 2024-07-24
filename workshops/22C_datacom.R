#' @name 22C_datacom.R
#' @author Tim Fraser
#' @title Lesson: Data Communication with Networks
#' @description
#' 
#' The BlueBikes Mobility Network is an enormous network, 
#' comprising >5 million station-pairs and millions of rides over about a decade.
#' How should we communicate patterns about it?
#' 
#' If we look at classic network analysis methods,
#' we see a lot about centrality, betweenness, etc.
#' But these methods are not always ideal for big networks,
#' where we can't analyze the entire network at the same time.
#' 
#' In these cases, I find that big network datasets
#' are best suited to descriptive statistics.
#' Eg. sums, percentages, means, standard deviations, etc.
#' 
#' Using complex methods on big data is... not always a recipe for success.
#' Our analyses might not be controlling for relevant network structural factors, etc.
#' But, really clear, descriptive patterns - they're hard to ignore.
#' 
#' In this lesson, we're going to learn a few ways to summarize 
#' and communicate quantities of interest from networks,
#' in a database format.
#' 
#' This lesson expands on the tools introduced in 21C_databases.R.
#' I strongly recommend completing that lesson first.



# Let's apply the new skills we developed for network joins,
# and generate some visualizations.

# We'll be thinking about the tables 'tally_rush_edges' and 'stationbg_dataset'
# located in our database.

# Let's join in our demographic traits
# to get a glimpse of what this mobility network 
# looks like in terms of which neighborhoods 
# are getting connected 
# - eg. *who* rides? 
# We will classify each station based on 
# its block group's traits!

# Here are a few methods that use this strategy.

# 0. Packages ###############################################

library(dpylr) # data wrangling
library(readr) # reading data
library(ggplot2) # visualizing data
library(viridis) # for color palettes
library(DBI) # for databases
library(RSQLite) # for SQLite
library(stringr) # for string manipulation


# 1. Frequency in a Subset ###################################

# First, let's narrow into a subset of data,
# and then calculate the frequency of key node traits.

## Data Wrangling #######################################

# Connect to database
db = dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")

# Get edges...
# Narrow into a subset - rides in 2021 during AM rush hour
q_edges = db %>%
  tbl("tally_rush_edges") %>%
  filter(rush == "am") %>%
  filter(str_sub(day, 1,4) == "2021")

# Get nodes...
# Classify each station by whether or not the station
# is in a majority Black neighborhood ("yes" vs. "no")
q_nodes = db %>%
  tbl("stationbg_dataset") %>%
  mutate(majority = if_else(pop_black_2020_smooth5 > 0.5,
                            true = "yes", false = "no")) %>%
  select(code, majority)

# Get dataset
q_data = q_edges %>%
  # join in source traits
  left_join(by = c("start_code" = "code"), 
            y = q_nodes %>% select(code, start_black = majority)) %>% 
  # join in destination traits,
  left_join(by = c("end_code" = "code"), 
            y = q_nodes %>% select(code, end_black = majority)) %>%
  # Filter to just edges with valid data
  filter(start_black != "NA" & end_black != "NA")

# Note: if start_black or end_black == NA,
# that means the start_code station or end_code station 
# are not in Boston proper (eg. maybe in Cambridge or beyond.) 
# I'd like to zoom into Boston, for the time being.

q_data

# Aggregate to get the source-by-destination trait tallies
stat = q_data %>%
  group_by(start_black, end_black) %>%
  summarize(trips = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(trips, na.rm = TRUE)) %>%
  collect()

stat

# Now that we've collected, do extra formatting
mytab = stat %>%
  mutate(total = sum(trips, na.rm = TRUE),
         percent = trips / total) %>%
  # Clean up the percentages
  mutate(percent = round(percent*100, digits = 1)) %>%
  # Add a label
  mutate(type = paste(start_black, "->", end_black))
  

mytab

# So that's... disquieting. Yikes. 
# I mean, it's an individual choice to ride or not, 
# but when a public program sees low rates of participation 
# from neighborhoods of color as small as that, 
# it's not really an equitable policy outcome.


# You can repeat this process with *any* of the demographic variables 
# in `stationbg_dataset` 
# (as long as it makes sense. Rule of thumb: simple analyses are usually better 
# and closer to the truth).

dbDisconnect(db)





## Visualize It! #######################################


### Bar Charts! ############################

# It's bar-chart time! So many bar charts!
mytab

ggplot() +
  geom_col(data = mytab, 
           mapping = aes(x = start_black, y = trips, fill = end_black))


ggplot() +
  geom_col(data = mytab, 
           mapping = aes(x = start_black, y = trips, fill = end_black),
           position = "fill")


ggplot() +
  geom_col(data = mytab, 
           mapping = aes(x = start_black, y = trips, fill = end_black),
           position = "fill") +
  labs(subtitle = "What % of Rides that started out in Black Neighborhoods\nended in Black Neighborhoods?",
       y = "% of Rides",
       x = "Starting Station\nin Majority Black Neighborhood?",
       fill = "Ending Station\nin Majority\nBlack\nNeighborhood?")




### Filtered Bar Charts! #######################

# Sometimes filtered bar charts are more intuitive to read.
mytab

ggplot() +
  geom_col(data = mytab, mapping = aes(x = type, y = percent)) +
  geom_text(data = mytab, 
            mapping = aes(x = type, y = percent, label = percent),
            vjust = 0, nudge_y = 2)
  

ggplot() +
  geom_col(data = mytab, mapping = aes(x = type, y = percent)) +
  geom_text(data = mytab, mapping = aes(x = type, y = percent, label = percent),
            vjust = 0, nudge_y = 2) +
  labs(subtitle = "What % of Rides that started out in Black Neighborhoods\nended in Black Neighborhoods?",
       y = "% of Rides",
       x = "Starting Neighborhood >50% Black? -> Ending Station >50% Black?")




### Heat Maps! #############################

# Another great option is ```geom_tile()```, also known as heatmaps.



ggplot() +
  geom_tile(
    data = mytab,        
    mapping = aes(x = start_black, y = end_black, fill = percent),
    color = "white")



ggplot() +
  geom_tile(
    data = mytab,        
    mapping = aes(x = start_black, y = end_black, fill = percent),
    color = "white") +
  geom_text(
    data = mytab,        
    mapping = aes(x = start_black, y = end_black, label = percent),
    color = "white", size = 8)





ggplot() +
  geom_tile(
    data = mytab,        
    mapping = aes(x = start_black, y = end_black, fill = percent),
    color = "white") +
  geom_text(
    data = mytab,        
    mapping = aes(x = start_black, y = end_black, label = percent),
    color = "white",
    size = 8)  +
  # Viridis is back!!!
  scale_fill_viridis(option = 'mako', begin = 0.2, end = 0.8) +
  theme_classic(base_size = 14) +
  labs(x = "Starting Station\nin Majority Black Neighborhood?", 
       y = 'Ending Station\nin Majority Black Neighborhood?',
       subtitle = "% of BlueBikes Ridership between Black/White Neighborhoods")





# 2. Networks over Time ##############################

# Oh hang on, didn't we have this data for every place over time?

### Change over Time by Race ##############################################

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

q_edges <- db %>%
  tbl("tally_rush_edges") %>%
  # zoom into just morning
  filter(rush == "am") %>%
  # drop unnecessary variables, to keep this dataset as small as possible
  select(-rush)

# This is all the station-pairs from 2011 to 2021.
# WOW! that's a huge dataset. >2 million rows!!!

q_edges # preview

# Get nodes...
# Classify each station by whether or not the station
# is in a majority Black neighborhood ("yes" vs. "no")
q_nodes = db %>%
  tbl("stationbg_dataset") %>%
  mutate(maj_black = if_else(pop_black_2020_smooth5 > 0.5, 
                             true = "yes", false = "no")) %>%
  select(code, maj_black)


q_nodes # preview

# Get dataset
# We're going to join in demographics,
# filter to valid cases, and get our percentages for every year.
q_data = q_edges %>%
  # join in source traits
  left_join(by = c("start_code" = "code"), 
            y = q_nodes %>% select(code, start_black = maj_black)) %>% 
  # join in destination traits,
  left_join(by = c("end_code" = "code"), 
            y = q_nodes %>% select(code, end_black = maj_black)) %>%
  # Filter to just edges with valid data
  filter(start_black != "NA" & end_black != "NA")

q_data # preview


q_stat = q_data %>%
  # Get year
  mutate(year = str_sub(day, 1,4)) %>%
  # Now count up by YEAR, start type, and end type
  group_by(year, start_black, end_black) %>%
  summarize(trips = sum(count, na.rm = TRUE)) %>%
  ungroup() 

q_stat # preview

# Collect the stats
stat = q_stat %>% collect()


# Disconnect
dbDisconnect(db)




# Format the stats locally
mytab = stat %>%
  # Now for each year, calculate percentage
  group_by(year) %>%
  mutate(total = sum(trips),
         percent = trips / total) %>%
  # Clean up the percentages
  mutate(percent = round(percent*100, digits = 1)) %>%
  # Make a group id
  mutate(type = paste0(start_black, " -> ", end_black))


mytab




# Let's finish up by visualizing some equity questions over time!

#### Line Plot! ########################################################

# Get just a subset of interest
subset = mytab %>%
  filter(start_black == "no", end_black == "no")


ggplot() +
  geom_point(data = subset, mapping = aes(x = year, y = percent), size = 5) +
  geom_line(data = subset, mapping = aes(x = year, y = percent, group = type))
  


ggplot() +
  geom_point(data = subset, mapping = aes(x = year, y = percent), size = 5) +
  geom_line(data = subset, mapping = aes(x = year, y = percent, group = 1)) +
  theme_classic(base_size = 14) +
  labs(y = "% Trips between Majority Non-Black Neighborhoods,\nout of Total Trips",
       x = "Year",
       subtitle = "Very tiny changes in Demographic Mobility\nin BlueBikes Program over Time")




#### Bar Plot #######################################################

ggplot() +
  geom_col(data = mytab,
           mapping = aes(x = year, y = trips, group = type, fill = type),
           position = "fill")

ggplot() +
  geom_col(data = mytab, 
           mapping = aes(x = year, y = percent, group = type, fill = type), 
           position = "fill")
# Not a very exciting plot, I admit - but functional!






## Aggregate then Investigate #####################################

# Alternatively, we could aggregate the data to the yearly level
# THEN join our node data.

# connect to database
db <- dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")

q_edges <- db %>%
  tbl("tally_rush_edges") %>%
  # zoom into just morning
  filter(rush == "am") %>%
  # drop unnecessary variables, to keep this dataset as small as possible
  select(-rush) %>%
  # Get a year variable
  mutate(year = str_sub(day, 1,4)) %>%
  # Filter...
  filter(start_code != "NA" & end_code != "NA") %>%
  # Aggregate by year 
  group_by(start_code, end_code, year) %>%
  summarize(trips = sum(count, na.rm = TRUE)) %>%
  ungroup()


# Now, we have an annual temporal network, with this many edges
q_edges %>% summarize(count = n())

q_edges %>% head() # view it

# Let's get some income based traits
q_nodes = db %>%
  tbl("stationbg_dataset") %>%
  mutate(income_over_60K = pop_60001_100000_2019_smooth5 + pop_100000_plus_2019_smooth5) %>%
  select(code, income_over_60K) %>%
  # Split up nodes into 4 quartiles,
  # where 1 has less than 25% earning over 60K
  # while 4 has over 75% earning over 60K
  mutate(income = case_when(
    income_over_60K < 0.25 ~ 1,
    income_over_60K < 0.50 & income_over_60K >= 0.25 ~ 2,
    income_over_60K < 0.75 & income_over_60K >= 0.50 ~ 3,
    income_over_60K < 1 & income_over_60K >= 0.75 ~ 4
  ))
  

q_nodes

# Let's do some joining...

# join in source traits
q_data = q_edges %>%
  left_join(by = c("start_code" = "code"), 
          y = q_nodes %>% select(code, start_income = income)) %>% 
  # join in destination traits,
  left_join(by = c("end_code" = "code"), 
            y = q_nodes %>% select(code, end_income = income)) %>%
  # Filter to just edges with valid data
  filter(start_income != "NA" & end_income != "NA") %>%
  # Now aggregate it by year and start type and end type
  group_by(year, start_income, end_income) %>%
  summarize(trips = sum(trips, na.rm = TRUE)) %>%
  ungroup()


# Collect it!
data = q_data %>% collect()

dbDisconnect(db)

#### Tile Plot ##################################


# One year
ggplot() +
  geom_tile(data = data %>% filter(year == 2021),
            mapping = aes(x = end_income, y = start_income, fill = trips)) +
  scale_fill_viridis(option = "plasma", trans = "log",
                     labels = scales::label_number())


# Many years
gg = ggplot() +
  geom_tile(data = data,
            mapping = aes(x = end_income, y = start_income, 
                          fill = trips),
            color = "white") +
  scale_fill_viridis(option = "plasma", trans = "log",
                     labels = scales::label_number()) +
  facet_wrap(~year) +
  labs(x = "Destination Neighborhood Wealth Quartile",
       y = "Source Neighborhood Wealth Quartile") +
  theme_bw()



# Finishing touches
gg +
  # We should probably make those x and y labels clearer, no?
  scale_y_continuous(
    breaks = c(1,2,3,4),
    labels = c("1" = "<25%", "2" = "<50%", "3" = "<75%", "4" = "<100%")
  ) +
  scale_x_continuous(
    breaks = c(1,2,3,4),
    labels = c("1" = "<25%", "2" = "<50%", "3" = "<75%", "4" = "<100%")
  ) +
  # Add a clarification
  labs(caption = "Wealth Quartile shows how much of that block group earned over 60K in 2019.") +
  labs(title = "Mobility between Neighborhoods by Wealth") +
  # Maybe get rid of the background grid
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank()) 


# 3. Map ##########################################

# ONLY if relevant you might consider mapping the network.
# And if so, filter those edges and/or aggregate them.
# Otherwise, it's going to be way too many features to plot.
# ~Try to keep <10,000 features (rows) on a plot.
# It takes a long time to render otherwise.


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

q_edges <- db %>%
  tbl("tally_rush_edges") %>%
  # zoom into just morning
  filter(rush == "am") %>%
  # drop unnecessary variables, to keep this dataset as small as possible
  select(-rush) %>%
  mutate(year = str_sub(day, 1,4)) %>%
  filter(start_code != "NA" & end_code != "NA") %>%
  group_by(year, start_code, end_code) %>%
  summarize(trips = sum(count, na.rm = TRUE)) %>%
  ungroup()

q_edges

# Get nodes...
# Classify each station by whether or not the station
# is in a majority Black neighborhood ("yes" vs. "no")
q_nodes = db %>%
  tbl("stationbg_dataset") %>%
  mutate(maj_black = if_else(pop_black_2020_smooth5 > 0.5, 
                             true = "yes", false = "no")) %>%
  select(code, maj_black, x, y)


q_nodes # preview

# Get dataset
# We're going to join in demographics,
# filter to valid cases, and get our percentages for every year.
q_data = q_edges %>%
  # join in source traits
  left_join(by = c("start_code" = "code"), 
            y = q_nodes %>% 
              select(code, start_black = maj_black, 
                     start_x = x, start_y = y)) %>% 
  # join in destination traits,
  left_join(by = c("end_code" = "code"), 
            y = q_nodes %>% 
              select(code, end_black = maj_black, 
                     end_x = x, end_y = y)) %>%
  # Filter to just edges with valid data
  filter(start_black != "NA" & end_black != "NA")



q_data %>% summarize(count = n())


data = q_data %>%
  filter(year == "2021") %>%
  collect()

data


# Get some block group polygons
bg = read_rds("data/bluebikes/bgdataset.rds") %>% 
  select(geoid, geometry)

# Visualize

ggplot() +
  # Plotted my edges
  geom_segment(
    data = data,
    mapping = aes(x = start_x, y = start_y,
                  xend = end_x, yend = end_y),
    color = "lightgrey", alpha = 0.2
    ) +
  theme_void()



ggplot() +
  geom_sf(data = bg, fill = "black", color = "#373737", 
          linewidth = 0.1) +
  # Plotted my edges
  geom_segment(
    data = data,
    mapping = aes(x = start_x, y = start_y,
                  xend = end_x, yend = end_y),
    color = "lightgrey", alpha = 0.05
  ) +
  theme_bw() +
  coord_sf(xlim = c(-71.2, -71.0),
           ylim = c(42.3, 42.4))



subset = data %>%
  filter(start_code == "A32000")




ggplot() +
  # Plot background
  geom_sf(data = bg, fill = "black", color = "#373737", 
          linewidth = 0.1) +
  # Plotted my edges
  geom_segment(
    data = data,
    mapping = aes(x = start_x, y = start_y,
                  xend = end_x, yend = end_y),
    color = "lightgrey", alpha = 0.05
  ) +
  # Plot subset
  geom_segment(
    data = subset,
    mapping = aes(x = start_x, y = start_y,
                  xend = end_x, yend = end_y),
    color = "dodgerblue"
  ) +
  theme_bw() +
  coord_sf(xlim = c(-71.2, -71.0),
           ylim = c(42.3, 42.4))




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

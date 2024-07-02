#' @name social_infra/example.R
#' @author Tim Fraser
#' @description
#' This data comes from a new study, describing 25 cities' 
#' social infrastructure rates for every city block.
#' 
#' This data is not yet released, so please do not share widely :)

# The social infrastructure database is a massive database 
# recording social infrastructure sites,
# broken into 4 major types, including
# - parks (eg. green space, community gardens, bike paths)
# - community spaces (eg. libraries, community centers)
# - social businesses (eg. coffeeshops)
# - places of worship (eg. mosques, synagogues, and churches)
#
# For details about the general data collection strategy,
# please read: 
# 
# Fraser, T., Cherdchaiyapong, N., Tekle, W., Thomas,
# E., Zayas, J., Page-Tan, C., & Aldrich, D. P. (2022).
# Trust but verify: Validating new measures for 
# mapping social infrastructure in cities. 
# Urban Climate, 46, 101287.
# https://drive.google.com/file/d/1zYtZNVE-J6Eb-Zsqg1icpnce6m65e96x/view?usp=sharing
#
# For appendix, see:
# https://drive.google.com/file/d/10jEc2ZAunfo9C_NSUL6FgcpcsQc13iqu/view?usp=sharing


# To investigate this data, the following packages are recommended:
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(viridis)


# This database includes several data resources.
# You are encouraged to load just the first few rows of each
# to familiarize yourself with the data structure,
# while not overwhelming your computer.
# You are encouraged to clear your local environment and cache frequently.
# For example:
rm(list = ls()); gc()

# 
#' @name bounds.geojson
#' @description
#' Spatial polygons for city boundaries for every city in the study dataset.
#' >25 cities are covered, including the 25 most populous cities in America.
#' @format a spatial dataframe with the following variables
#'  \describe{
#'    \item{name}{name of city, for joining with other databases. }
#'    \item{geometry}{ spatial geometry for city boundaries. }
#'  }
read_sf("data/social_infra/bounds.geojson") %>%
  head()


#' @name bg.geojson
#' @description
#' spatial polygons for every block group in the study region
#' @format a spatial dataframe with the following variables
#'  \describe{
#'    \item{geoid}{unique census geoid code for each census block group in study. }
#'    \item{area_land}{ area of block group in square kilometers (I think) }
#'    \item{name}{name of city, for joining with other databases. }
#'    \item{geometry}{ spatial geometry for city boundaries. }
#'  }
read_sf("data/social_infra/bg.geojson") %>%
  head()


#' @name bg_data.rds
#' @description
#' Census Block Group data over time for block groups in the study region.
#' Many more socio-economic variables are available at the **block group** level than the **block** level.
#' You are recommended to use block group level variables when able.
#' @format a tabular data.frame with the following variables
#'  \describe{
#'    \item{name}{ name of city, for joining with other databases. }
#'    \item{geoid}{unique census geoid code for each census block group in study. }
#'    \item{year}{year of census data. Each year is sourced from the American Community Survey 5-year averages (ACS-5). So, 2015 represents the average of any responses from 2013, 2014, 2015, 2016, and 2017. }
#'    \item{pop}{population in that year, in census block group.}
#'    \item{women}{% of residents who are women, in that census block group.}
#'    \item{over_65}{% of residents who are elders (over age 65), in that census block group.}
#'    \item{hisplat}{% of residents who identify as Hispanic or Latino, in that census block group. Note that the US census treats Hispanic/Latino as an ethnicity, not a race, meaning that it should not be added to other racial demographic categories. They will not sum up to zero.}
#'    \item{white}{% of residents who identify as White, in that census block group.}
#'    \item{black}{% of residents who identify as Black, in that census block group.}
#'    \item{natam}{% of residents who identify as Native American, in that census block group.}
#'    \item{asian}{% of residents who identify as Asian, in that census block group.}
#'    \item{some_college}{% of residents who have some college education, meaning 1 or more years of college education, in that census block group.}
#'    \item{income_0_60K}{% of residents who have a household income of 0~60K USD, in that census block group.}
#'    \item{median_household_income}{Median Household Income in USD, in that census block group.}
#'    \item{unemployment}{unemployment rate, measured as total unemployed persons / total labor force size.}
#'    }
read_rds("data/social_infra/bg_data.rds") %>% 
  glimpse()




#' @name tally1km.geojson
#' @description
#' Spatial grid data for 1 square kilometer grid cells spanning city boundaries for every city under analysis.
#' Includes rates of social infrastructure by 4 types and 2020 decennial census block estimates for select demographics.
#' @format a spatial data.frame with the following variables
#'  \describe{
#'    \item{cell}{ unique grid cell id, covering 1 square kilometer. }
#'    \item{name}{ name of city, for joining with other databases. }
#'    \item{community_space}{rate of community spaces, per 1000 residents per square kilometer.}
#'    \item{place_of_worship}{rate of places of worship, per 1000 residents per square kilometer.}
#'    \item{social_business}{rate of social businesses, per 1000 residents per square kilometer.}
#'    \item{park}{rate of parks, per 1000 residents per square kilometer.}
#'    \item{pop_density}{estimated population density in 2020, in 1000s of persons per square kilometer. Spatially averaged from all blocks overlapping this grid cell. }
#'    \item{white}{estimated % White residents in 2020. Spatially averaged from all blocks overlapping this grid cell. }
#'    \item{black}{estimated % Black residents in 2020. Spatially averaged from all blocks overlapping this grid cell. }
#'    \item{natam}{estimated % Native American residents in 2020. Spatially averaged from all blocks overlapping this grid cell. }
#'    \item{asian}{estimated % Asian residents in 2020. Spatially averaged from all blocks overlapping this grid cell. }
#'    \item{hisplat}{estimated % Hispanic/Latino residents in 2020. Spatially averaged from all blocks overlapping this grid cell. Note that the US census treats Hispanic/Latino as an ethnicity, not a race, meaning that it should not be added to the preceding demographic categories. They will not sum up to zero. }
#'    \item{units_occupied}{ estimated % of housing units occupied in 2020. Spatially averaged from all blocks overlapping this grid cell. }
#'    \item{geometry}{ spatial geometry for this grid cell. } 
#'  }
read_sf("data/social_infra/tally1km.geojson") %>%
  head()




#' @name sites.geojson
#' @description
#' Spatial points data for every social infrastructure site from our queries.
#' For rates of social infrastructure (normalized by population and area), see `tally1km.geojson` instead.
#' @format a spatial dataframe with the following variables
#'  \describe{
#'    \item{place_id}{unique place ID from Google Places API. }
#'    \item{name}{ name of site according to Google Places API }
#'    \item{type}{ type of social infrastructure, grouped into  `"Park"`, `"Community Space"`, `"Social Business"`, or `"Place of Worship"` }
#'    \item{term}{ original search term used by Google Places API to find this site. }
#'    \item{geometry}{ spatial geometry for site point ([x,y] coordinates) }
#'  }
read_sf("data/social_infra/sites.geojson") %>%
  head()


# Other datasets are also available, but not recommended for dashboard use at this time,
# as the data processing behind their estimates requires further description.



# Here are some great ways this data can be used...


# Use Case 0: Describe Cities ############################

library(dplyr)
library(readr)
library(ggplot2)

bg = read_rds("data/social_infra/bg_data.rds") %>%
  # Grab just variables of interest
  select(name, year, geoid, median_household_income) %>%
  # Brief data cleaning....
  mutate(across(.cols = median_household_income, .fns = ~case_when(.x == "-666666666" ~ NA, TRUE ~ .x))) %>%
  # For each city and year...
  group_by(name, year) %>%
  # get average median household income 
  summarize(mean = mean(median_household_income, na.rm = TRUE))

ggplot() +
  geom_line(
    data = bg, 
    mapping = aes(x = factor(year), y = mean, group = name),
    color = "grey") +
  theme_classic() +
  labs(y = "Median Household Income [USD] in an Average Block Group",
       x = "Year")


# Use Case 1: Visualize Social Infrastructure Rates! ############################## 

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(viridis)

# Load in the grid
grid = read_sf("data/social_infra/tally1km.geojson") %>%
  filter(name == "nyc")


# Plot the grid
ggplot() +
  geom_sf(data = grid, mapping = aes(fill = log(park) )) +
  scale_fill_viridis(na.value = "black", option = "plasma") +
  theme_void() 


rm(list = ls())



# Use Case 2: Visualize Social Infrastructure Points! ############################## 

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(viridis)

# Load in the boundaries for LA
bounds = read_sf("data/social_infra/bounds.geojson") %>%
  filter(name == "la")

# Find me all social infrastructure points located within LA
sites = read_sf("data/social_infra/sites.geojson") %>%
  # By joining the points to this boundary file;
  # Rather than a left join, let's make it an inner_join, dropping points outside LA
  st_join(bounds %>% select(), left = FALSE)



# Plot the grid
ggplot() +
  geom_sf(data = bounds, fill = "tan", color = "black") +
  geom_sf(data = sites, mapping = aes(fill = type), 
          shape = 21, color = "white", size = 3, stroke = 0.5) +
  scale_fill_viridis(discrete = TRUE, option = "plasma", begin = 0, end = 0.8) +
  theme_void() +
  theme(legend.position = "bottom")


rm(list = ls())




# Use Case 3: Describe Areas with High Social Infrastructure ###############################

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(viridis)

# Find the cell(s) with the highest rate of social businesses per city.
mycells = read_sf("data/social_infra/tally1km.geojson") %>%
  group_by(name) %>%
  filter(community_space == max(community_space, na.rm = TRUE)) %>% 
  # Let's look at just nyc
  filter(name == "nyc") %>%
  select(cell, name, community_space)

# Get the block groups where those cells live...
bg = read_sf("data/social_infra/bg.geojson") %>%
  st_transform(crs = 4326) %>% 
  select(geoid, geometry)

ids = mycells %>%
  # Join in the geoids of any overlapping block groups
  st_join(bg) %>% 
  # Convert to tibble/dataframe
  as_tibble() %>%
  # Get just distinct geoids
  select(geoid) %>%
  distinct()
  
# Remove bg
remove(bg)

# What were the demographic trends in their block groups over time?
data = read_rds("data/social_infra/bg_data.rds") %>%
  # Brief data cleaning....
  mutate(across(.cols = pop:unemployment, .fns = ~case_when(.x == "-666666666" ~ NA, TRUE ~ .x))) %>%
  # Get just variables you need
  select(name, year, geoid, median_household_income) %>%
  # Filter to just blockgroups containing these 'max' cases for social businesses
  filter(geoid %in% ids$geoid)



# Let's look at just the optimal case in new york city...
# What happened to the median income
# in block groups with the highest rate of community spaces in NYC?
ggplot() +
  geom_line(
    data = data,
    mapping = aes(
      x = factor(year), 
      y = median_household_income, 
      group = geoid),
    color = 'darkgrey'
    ) +
  theme_bw() +
  labs(y = "Median Household Income (USD)",
       x = "Year",
       title = "Change in Income in NYC Block Groups with Highest Rate of Community Spaces")
  

rm(list = ls())


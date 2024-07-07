# 11P_dataviz.R
# Practice: Mapping Practicum!
# Prof: Tim Fraser


# This practice exercise investigates divisions in social infrastructure 
# in neighborhoods in the center of Boston. 
# Social infrastructure refers to the physical spaces 
# in our communities that build social ties between residents. 
# These include community spaces, like libraries or community centers, 
# places of worship, like mosques, synagogues, and churches, social businesses, 
# like cafes, barbershops, and nail salons, and parks,
# like green belts, squares, and fountains. 

# In our training, we investigated variation in total social infrastructure, 
# but what if trends in social infrastructure differ more
# when we break them down by the type of social infrastructure? 
# You will use mapping in the sf package to visualize and tabulate your data, 
# and then unleash your tidyverse and lm toolkit 
# to analyze trends in one type of social infrastructure.


# 0. Import Data ###########################

# First, please import the following data for your analysis.

# - Social Infrastructure Points ("boston_social_infra.geojson")
# - Grid Cell Polygons ("boston_grid.geojson")
# - T Metro Lines ("boston_train_lines.geojson")
# - Census Data for Polygons ("boston_census_data.csv")
# - Block Group Polygons ("boston_block_groups.geojson")


# Import packages
library(dplyr)
library(readr)
library(GGally)
library(viridis)
library(sf)
library(ggspatial)

# Import data
# Get social infrastructure sites (points)
mypoints <- read_sf("workshops/12C_databases_4/boston_social_infra.geojson")
# Get grid cells in Central Boston (polygons)
myshapes <- read_sf("workshops/12C_databases_4/boston_grid.geojson")
# Get T metro lines (lines)
mylines <- read_sf("workshops/12C_databases_4/boston_train_lines.geojson")
# Get average census traits for grid cells (data.frame)
mydata <- read_csv("workshops/12C_databases_4/boston_census_data.csv")
# Get census block groups (polygons)
mybg <- read_sf("workshops/12C_databases_4/boston_block_groups.geojson")



# Task 1: Spatial Data Wrangling ######################


# In this lab, you will analyze one type of social infrastructure
# (specified by the group variable in mypoints). 
# Please choose 1 from the following 
# 4 types of social infrastructure: 
# "Community Spaces", "Places of Worship", "Social Businesses" or "Parks".

# Part A. First, using your data wrangling tools, 
# please count how many of each type of social infrastructure
# are in the study region. Which type is most common? 
# Where does your chosen type of social infrastructure rank?

# Part B. Second, using st_join(), 
# please count the total number of sites of your 
# chosen type of social infrastructure in each of our 73 grid cells in Boston. 
# Save your result as a data.frame called mytally.

# Part C. Then, using left_join(), 
# please merge mytally with our mydata data.frame. 
# Use the population density variable (pop_density) 
# you gained to calculate for each grid cell 
# the rate of your type of social infrastructure, 
# per 1000 residents per square kilometer. 
# filter() to just grid cells 
# with a population density greater than 0. 
# Please name your final dataset of rates 
# and other demographic variables as myrates.


# Task 2: Mapping ##########################################

# Next, combining your new sf toolkit and your ggplot skills, 
# please visualize the rates of your chosen type 
# of social infrastructure in Boston’s 73 grid cells. Be sure to…

# - Color: Make intentional color and design choices.

# - Readability: Crop your map, apply a good theme, and add helpful labels.

# - Storytelling: Give an informative title that 
# summarizes the geographic patterns you find.

# - Context: Integrate census block group polygons (mybg) 
#  or T lines (mylines) to help add geographic context for your reader.



# Task 3: Analysis ##########################################

# Finally, using your data wrangling toolkit,
# please analyze the rates of your chosen type 
# of social infrastructure in your myrates data.frame, 
# drawing on the other demographic variables you joined in.
# Your analysis should include at least 2 parts -
# a descriptive analysis (eg. mean, median, etc.) and a model. 
# Please choose 1 from each of the sections below:
  
# (Note: Remember that you’ll probably need to convert
# it to a tibble before further analysis.)


## Descriptive Analysis Options (Pick 1): #########################

# - Neighborhoods: Calculate the average rate of your type 
# of social infrastructure in each neighborhood in Boston, 
# and visualize it in ggplot(). 
# Which neighborhoods experience the most vs. the lowest 
# of this type of social infrastructure?
  
  
# - Correlations: What variables correlate most strongly
# with your chosen type of social infrastructure? 
# Create an intuitive visual in ggplot() or GGally 
# that describes associations with at least 3 covariates.


# - Inequality: Investigate differences in average rates 
# of your type of social infrastructure for grid cells 
# whose residents are predominantly white vs. grid cells
# with high rates of Black, Latinx, or Asian residents. 
# (You will need to choose a threshold for
# what counts as ‘high’ rates. 
# Is it 25% 50%? Briefly justify your choice.) 
# Then, create an intuitive visual in ggplot()
# that describes this difference.


## Modeling Options (Pick 1): #############################
  
# - Maximize R2: Build a model that explains
# as high a percentage of variation (R2) in 
# social infrastructure rates as you can. 
# Report your results. What trends do you find?

# - Simulation: Simulate the effect of 
# one key demographic variable (eg. median_income) 
# on the rate of your chosen type of social infrastructure 
# in an average city block, 
# controlling for several relevant demographic variables.


# Conclusion ################################

# Congratulations! You made it! You can work with geographic data!




  
  

  

  











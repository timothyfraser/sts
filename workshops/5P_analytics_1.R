#' practice_5_analytics_1.R
#
# Series: Analytics 1
# Topic: Techniques for Temporal Data
# Lagged Variables, Means, and Lines-of-Best-Fit (Regression)
# Prof. Fraser

# Here are a series of practice exercises using nycflights,
# which will show up in Lesson 2 as examples of data communication

# Set working directory to main folder
setwd("/cloud/project")

# Task 0 #############################
# Load the dplyr, readr, and broom packages



# Task 1 #############################
# Load in flights, containing just year, month, day, arr_delay, carrier, and origin,
# and filtering to just JFK and La Guardia airports.





# Task 2 #############################
# Do flights coming from different airports of origin
# tend to have different arrival delays?

# Do flights coming from LGA 
# tend to have greater arrival time delays
# than flights coming from JFK?
# Find the overall bivariate effect of flying from LGA on arrival delays.


# Task 3 ############################
# How does that effect vary over time? 
# Using group_by() from dplyr and tidy() from broom,
# get the effect of LGA for each month



# Task 4 #########################################
# Visualize these effects over time



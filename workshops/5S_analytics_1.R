#' 5S_analytics_1.R
# Solutions to Practice Exercises in 5P_analytics_1.R
#'
# Series: Analytics 1
# Topic: Techniques for Temporal Data
# Lagged Variables, Means, and Lines-of-Best-Fit (Regression)
# Prof. Fraser
#' practice_5_analytics_1.R

# Here are a series of practice exercises using nycflights,
# which will show up in Lesson 2 as examples of data communication

# Set working directory to main folder
setwd("/cloud/project")

# Task 0 #############################
# Load the dplyr, readr, and broom packages
library(dplyr)
library(readr)
library(broom)

# Task 1 #############################
# Load in flights, containing just year, month, day, arr_delay, carrier, and origin,
# and filtering to just JFK and La Guardia airports.
flights = read_csv("data/flights.csv") %>%
  # Filter to just JFK and La Guardia
  filter(origin %in% c("LGA", "JFK")) %>%
  # Just these core variables
  select(year, month, day, arr_delay, carrier, origin)

# What are our unique airports of origin?
# LGA, JFK
flights$origin %>% unique()


# Task 2 #############################
# Do flights coming from different airports of origin
# tend to have different arrival delays?

# Do flights coming from LGA 
# tend to have greater arrival time delays
# than flights coming from JFK?
# Find the overall bivariate effect of flying from LGA on arrival delays.

m1 = flights %>%
  lm(formula = arr_delay ~ origin)

# Looks like 0.232 more minutes of delay from LGA than JFK.
m1

# Task 3 ############################
# How does that effect vary over time? 
# Using group_by() from dplyr and tidy() from broom,
# get the effect of LGA for each month

effects = flights %>%
  group_by(month) %>%
  reframe(  lm(formula = arr_delay ~ origin) %>% tidy()  ) %>%
  filter(term == "originLGA")

# Task 4 #########################################
# Visualize these effects over time
ggplot() +
  geom_col(data = effects, mapping = aes(x = month, y = estimate)) +
  labs(x = "Month", y = "Change in Arrival Time (Minutes)",
       title = "La Guardia Flights vs. JFK Flights (baseline)")







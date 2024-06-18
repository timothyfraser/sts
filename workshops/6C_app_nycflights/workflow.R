#' @name workflow.R
#' @author Tim Fraser
#' @title Workflow for Dashboard App
#' @description
#' 
#' I strongly, strongly, strongly encourage you to write a 
#' `workflow.R` script first when building your app.
#' This script is designed to help you build out your process 
#' in a linear flow,
#' to make sure that your process works when run linearly,
#' and to debug issues.

# This app will help us understand the nycflights database.
# A good dashboard, however, doesn't just help you see ALL the data.
# It guides you towards meaningful measures.
# So, what are our meaningful measures of an airport system?

# Which carriers tend to arrive late/on time most often?
# How late/early are these carriers?


# GET STATIC DATA ################################

# If you're using static data, get that data into your app's folder

# Set folder to overall project
setwd(rstudioapi::getActiveProject())

# Read and write data to your app's folder
read_csv("data/airlines.csv") %>% 
  write_csv("workshops/6C_app_nycflights/airlines.csv")

read_csv("data/flights.csv") %>% 
  # Let's take a random sample
  sample_n(size = 20000) %>%
  # And get the main variables we care about here
  select(month, day, carrier, origin, 
         arr_delay, arr_time, sched_arr_time) %>%
  write_csv("workshops/6C_app_nycflights/flights_sample.csv")

# Use built in data to get month names
tibble(month = 1:12, month_abb = month.abb, month_name = month.name) %>%
  write_csv("workshops/6C_app_nycflights/months.csv")


# APP SETUP ########################

# Set your working directory to your app's folder
setwd(paste0(rstudioapi::getActiveProject(), "/workshops/6C_app_nycflights"))

## Load packages #################
library(dplyr) # data wrangling
library(readr) # reading data
library(ggplot2) # data vizualization

## Read in data ###############
airlines = read_csv("airlines.csv")
flights = read_csv("flights_sample.csv")
months = read_csv("months.csv")

## Inputs #####################
# Make an 'input' list, just like in your app
input = list(month = 12, carrier = "AA", origin = c("JFK", "LGA", "EWR"))

# View it
input

## Stats ######################
# Wherever possible, do just 1 calculation, as few times as you can.
# Let's start overall.
stat = flights %>%
  # Filter to just flights that started at these airports
  filter(origin %in% c("JFK", "LGA", "EWR")) %>%
  # For each carrier...
  group_by(month, carrier) %>%
  # Let's get the mean, stdev, and confidence intervals
  summarize(
    mean = mean(arr_delay, na.rm = TRUE),
    sd = sd(arr_delay, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    lower = mean + se*qnorm(0.025),
    upper = mean + se*qnorm(0.975)
  ) %>%
  # Ungroup
  ungroup() %>%
  # Join in the carrier names
  left_join(by = "carrier", y = airlines) %>%
  # Join in month name data
  left_join(by = "month", y = months)
  # What can we do with this information?

# View it
stat 

# REACTIVE STATS ########################

## stat_one_month #################################

# View the results for all carriers, for just one month.
stat_one_month = stat %>%
  # filter by selected month
  filter(month == input$month)

# View it
stat_one_month

# Compare different carriers in one month
gg_one_month = ggplot() +
  geom_col(
    data = stat_one_month,
    mapping = aes(x = carrier, y = mean)) +
  coord_flip() +
  labs(x = "Airline", y = "Mean Arrival Delay (minutes)")

# View it
gg_one_month


## stat_one_carrier #################################

# Let's view the results for just that one carrier, over time
stat_one_carrier = stat %>%
  # Filter by selected carrier
  filter(carrier == input$carrier)

# View it
stat_one_carrier

# Visualize just one carrier over time.
gg_one_carrier = ggplot() +
  geom_ribbon(
    data = stat_one_carrier,
    # Show month_abb as label, but order by month
    mapping = aes(x = reorder(month_abb, month), ymin = lower, ymax = upper, group = carrier, fill = name),
    alpha = 0.5 # transparency is helpful
  ) +
  geom_line(
    data = stat_one_carrier,
    mapping = aes(x = reorder(month_abb, month), y = mean, group = carrier, color = carrier)
  ) +
  labs(x = "Month", y = "Mean Arrival Delay (minutes)\n[with 95% Confidence Intervals]",
       fill = "Airline", title = "How Late is Your Airline?") +
  # you can ditch the legend for color or fill like this
  guides(color = "none") 

# View it
gg_one_carrier


## stat_highlight #################################

# Let's get some highlight stats for your carrier at one specific time
stat_highlight = stat %>%
  filter(carrier == input$carrier, month == input$month) %>%
  # Format a number for highlighting
  mutate(highlight = scales::number(mean, accuracy = 0.1) ) %>%
  # Summarize a label
  mutate(label = paste0(
    "In ", month_name, ", ",
    name, " flights to NYC had an average arrival delay of ", 
    highlight, " minutes."
  ))

# View it
stat_highlight$label



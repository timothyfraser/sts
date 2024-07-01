# 5E_analytics_1.R
# Class example 
# Temporal Data Analytics

# Load packages
library(dplyr)
library(readr)
library(broom)

# Load data
cities = read_csv("data/jp_matching_experiment.csv") %>%
  # Tell R to treat year and prefecture as ordered categories
  mutate(year = factor(year),
         pref = factor(pref))

# View it
cities



# Basic Regression vs. Multiple Regression

# basic regression 
# with 1 explanatory variable
cities %>% 
  lm(formula = social_capital ~ damage_rate)

# multiple regression
# regression with +2 explanatory variables
cities %>%
  lm(formula = social_capital ~ damage_rate + income_per_capita)



cities %>%
  lm(formula = social_capital ~ damage_rate + year + pop_density + 
       exp_dis_relief_per_capita + 
       income_per_capita + unemployment + pop_women + pop_over_age_65 + 
       pref)

# Rescale measures into z-scores
# How many standard deviations any value is 
# away from the mean of that vector
rescaled = cities %>%
  mutate(social_capital = scale(social_capital),
         damage_rate = scale(damage_rate),
         pop_density = scale(pop_density),
         exp_dis_relief_per_capita = scale(exp_dis_relief_per_capita),
         income_per_capita = scale(income_per_capita),
         unemployment = scale(unemployment),
         pop_women = scale(pop_women),
         pop_over_age_65 = scale(pop_over_age_65) )

rescaled 

# Let's make some final models

# First model estimates damage_rate, controlling just for minimal covariates,
# namely population density and year
m1 = rescaled %>%
  lm(formula = social_capital ~ damage_rate + pop_density + year)

# Second model adds basic spending
m2 = rescaled %>%
  lm(formula = social_capital ~ damage_rate + pop_density + year +
       exp_dis_relief_per_capita )

# Third model add socioeconomic covariates
m3 = rescaled %>%
  lm(formula = social_capital ~ damage_rate + pop_density + year +
       exp_dis_relief_per_capita +
       income_per_capita + unemployment + pop_women + pop_over_age_65)

# Add prefecture for geoegraphy
m4 = rescaled %>%
  lm(formula = social_capital ~ damage_rate + pop_density + year +
       exp_dis_relief_per_capita +
       income_per_capita + unemployment + pop_women + pop_over_age_65 +
       pref)

# m1,m2,m3,m4

# We can use the texreg package to view the results
texreg::screenreg(
  l = list(m1,m2,m3,m4), 
  omit.coef = "year")

# Or get model results from the broom package.
bind_rows(
  m1 %>%
    broom::tidy() %>%
    mutate(model = 1),
  m2 %>%
    broom::tidy() %>%
    mutate(model = 2),
  m3 %>%
    broom::tidy() %>%
    mutate(model = 3),
  m4 %>%
    broom::tidy() %>%
    mutate(model = 4) ) %>%
  select(model, term, estimate)


# Similarly, we can get R2 from the broom::glance() function
bind_rows(
  m1 %>%
    broom::glance() %>%
    mutate(model = 1),
  m2 %>%
    broom::glance() %>%
    mutate(model = 2),
  m3 %>%
    broom::glance() %>%
    mutate(model = 3),
  m4 %>%
    broom::glance() %>%
    mutate(model = 4) ) 


# Clean up!
rm(list = ls())


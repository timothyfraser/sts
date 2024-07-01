# class_5_analytics_1.R
#
# Series: Analytics 1
# Topic: Techniques for Temporal Data
# Lagged Variables, Means, and Lines-of-Best-Fit (Regression)
# Prof. Fraser

# So, we know how to query and visualize tables from our databases.
# But with big data, that's SO MUCH DATA!
# How do we summarize that information into single actionable numbers?


# For a good intro to regression, see this tutorial:
# https://moderndive.com/5-regression.html


# 0. Import Data ########################################

# Let's use some data on Japanese solar power to investigate.

# In this workshop, we’ll use the data.frame jp, 
# which is a matched sample of 43 monthly observations 
# on 147 Japanese municipalities,
# where some experienced disaster deaths or damage
# from the 2011 tsunami and earthquake (disaster == 1), 
# while others did not (disaster == 0). 
# This creates a total dataset of 6,321 city-month observations.

# Load packages
library(dplyr) # for data-wrangling
library(readr) # for reading in data
library(ggplot2) # for visualization
library(tidyr) # for drop-na
library(broom) # for regression model helpers
library(texreg) # for quick model tables

# Save this solar dataset as an object in my environment
jp = read_csv("data/jp_solar.csv") %>%
  select(muni_code, date, year, solar_rate, disaster, pop) %>%
  arrange(muni_code, date)





# In this dataset, each row is a city-month!

# View first 3 rows of dataset
jp %>% head(3)

# In this dataset, our variables mean:
#  
# - solar_rate: rate of new rooftop solar units 
#               adopted in that city that month, 
#               normalized per 1000 residents.
# - disaster: dichotomous variable indicating 
#             whether city experienced disaster deaths or damage 
#             from the 2011 tsunami and earthquake (disaster == 1),
#             or did not (disaster == 0).
# - pop: population from city’s most recent 5-year census tally.


# 1. Rates as Outcome Variables ###################################

# We’re going to investigate the rate of rooftop solar adoption 
# per 1000 residents in each town over time (solar_rate).

jp$solar_rate %>% summary()
jp$solar_rate %>% hist()

# Whoa! That is a LOT of zeros. This should concern you.

# How many are we talking here?
jp %>%
  summarize(zeros = sum(solar_rate == 0),
            percent = zeros / n())

# Going to cause negative predictions
jp %>%
  lm(formula = solar_rate ~ disaster + pop + factor(date)) %>%
  predict()  %>%
  summary()

# Eh. Let's not worry about it this time.
# We'll cover how to handle negative predictions
# in a future lesson next week.

# 2. Outliers over Time ###############################

# Instead, let's look at our solar data over time.
jp

# Let's visualize the time trend for each city...
ggplot() +
  geom_line(data = jp, mapping = aes(x = date, y = solar_rate, group = muni_code),
            color = "grey") 
# WHOA! Something really funky happened in between 2017 and 2018.
# It's probably not a good idea to give this observation equal weight to all the others.
# I suspect some municipalities waited until later
# and reported all their new solar panels all at once that month.
# So let's do some data cleaning.

# We're going to find that observation and clip it.
jp %>%
  filter(solar_rate >= 50)
# Looks like the culprit is 2017-09-25

# Let's filter it out, starting fresh.
jp = jp %>%
  filter(date != "2017-09-25")


# Let's revisualize it....
# Much better!
ggplot() +
  geom_line(data = jp, mapping = aes(x = date, y = solar_rate, group = muni_code),
            color = "grey") 



# 3. Temporal Lags ##############################

# But what if the solar_rate is just due to a kind of temporal effect,
# where because a lot of people installed rooftop solar one month, 
# their neighbors did the next month?

# We can evaluate this using lags!

jp2 = jp %>%
  # Must order by city and time
  arrange(muni_code, date) %>%
  # Must group by municipality - IMPORTANT
  group_by(muni_code) %>%
  # Then lag the solar rate, using n to specify how many steps.
  mutate(
    # First let's lag by 1 row,
    solar_lag1 = lag(solar_rate, n = 1),
    # Then let's lag by 2 rows
    solar_lag2 = lag(solar_rate, n = 2),
    # Then let's lag by 3 rows
    solar_lag3 = lag(solar_rate, n = 3)) %>%
  ungroup()  


# Let's view it
jp2 %>%
  select(muni_code, date, solar_rate, solar_lag1, solar_lag2, solar_lag3)

# Another way to view it
jp2 %>%
  select(muni_code, date, contains("solar")) 


# Another way to view it
jp2 %>%
  select(muni_code, date, contains("solar")) %>%
  View()


# 4. Effect of Past on Future ############################


# We've estimated...
# Effect of the disaster on solar
# Effect of each year on solar, compared to 2014 baseline (intercept)
# Effect of solar from last month
m1 = jp2 %>%
  lm(formula = solar_rate ~ disaster + factor(year) + 
       solar_lag1)

# A vector
m1$coefficients 

# Can't group_by or summarize this.

# broom::tidy --> coefficients
# broom::glance --> R2


# We can view the model coefficients here... (the estimated effects)
m1 %>% broom::tidy()
m1 %>% tidy()

# We can view the model accuracy / model fit statistic R2 here
# R2 = 100% --> GREAT explanatory power
# R2 = 0% --> TERRIBLE explanatory power
# look at the r.squared column
m1 %>% glance()


## 4.1 Controlling for year ###################

# What if we add more covariates???
m0 = jp2 %>%
  lm(formula = solar_rate ~ disaster + factor(year) )

# Accounting for effect of solar adoption one month prior
m1 = jp2 %>%
  lm(formula = solar_rate ~ disaster + factor(year) + 
       solar_lag1)

# Accounting for effect of solar adoption one month prior
# AND two months prior...
m2 = jp2 %>%
  lm(formula = solar_rate ~ disaster + factor(year) + 
       solar_lag1 + solar_lag2)

# Accounting for effect of solar adoption one month prior
# AND two months prior
# AND three months prior...
m3 = jp2 %>%
  lm(formula = solar_rate ~ disaster + factor(year) + 
       solar_lag1 + solar_lag2 + solar_lag3)

# Let's use texreg package's screenreg function to help us
# view all 4 of our models at once
texreg::screenreg(
  l = list(m0, m1,m2,m3)
)



# This table helps us do the following:
# We can evaluate, hey, what happened to the effect of disasters
# as we added each new variable?
# Did it stay positive?
# We're refining the effect of disasters


# But our R2 is so-so. Can we increase it, if we control for other factors?

## 4.1 Controlling for month ###################

# What if we control for each discrete month, rather than each discrete year?

# Let's try this again, but with date.
m4 = jp2 %>%
  lm(formula = solar_rate ~ disaster + factor(date) )

m5 = jp2 %>%
  lm(formula = solar_rate ~ disaster + factor(date) + 
       solar_lag1)

m6 = jp2 %>%
  lm(formula = solar_rate ~ disaster + factor(date) + 
       solar_lag1 + solar_lag2)

m7 = jp2 %>%
  lm(formula = solar_rate ~ disaster + factor(date) + 
       solar_lag1 + solar_lag2 + solar_lag3)


# Let's make the model table
texreg::screenreg(
  l = list(m4, m5, m6, m7)
)

# Lots of dates. We can omit any stat
# whose name includes the string 'date' from the table
# by using omit.coef = 'date'
texreg::screenreg(
  l = list(m4, m5, m6, m7), 
  omit.coef = "date"
)



# 5. Effects Vary over Time ###################

# But what if the effect of disasters itself truly varies over time?
# In that case, maybe it's more interesting to see how that bivarate effect
# changes in each month.


## 5.1 Average Monthly Effect ####################

# We can use group_by() to do this.
# We can pair group_by() and reframe() to estimate a linear model for each group...
# and we can use tidy() from broom package on the model
# to turn the model into a data.frame
# reframe() will be happier if we feed it a data.frame.

effects = jp2 %>%
  group_by(date) %>%
  reframe(  lm(formula = solar_rate ~ disaster ) %>% tidy()  ) 

# View all our effects
effects

# Let's filter to just disaster effects, for comparability
effects2 = effects %>%
  filter(term == "disaster")

# Let's view them!
# Note: in the original recording, I had not yet filtered out the extreme outlier,
# so this plot will look a little different for you.
# Don't worry - your version is right!
ggplot() +
  geom_line(data = effects2, mapping = aes(x = date, y = estimate))


# What's the average monthly effect?
# What's the max monthly effect?
effects2 %>%
  summarize(avg = mean(estimate),
            max = max(estimate))

# Don't need this any more.
# effects2 %>%
#   filter(estimate <= 5)
# 
# 
# effects3 = effects2 %>%
#   filter(estimate <= 5)
# 
# ggplot() +
#   geom_line(data = effects3, mapping = aes(x = date, y = estimate))

# We could get the unique month-year pairs...
mymonths = jp %>%
  select(date, year) %>%
  distinct()

# And we could join them in to get the year too.
effects4 = jp2 %>%
  group_by(date) %>%
  reframe(  lm(formula = solar_rate ~ disaster ) %>% tidy()  ) %>%
  filter(term == "disaster") %>%
  # filter(estimate <= 5) %>% # no longer needed
  left_join(by = "date", y = mymonths)

effects4
# See how we now have the year?

# PS: just realized there's an easier way to do this 
# that doesn't require joining.
# You can group by TWO variables, 
# and those variables stay with you through the reframing process.
# So we can group by date and year.
# Eg. this should work find
effects4 = jp2 %>%
  group_by(year, date) %>%
  reframe(  lm(formula = solar_rate ~ disaster ) %>% tidy()  ) %>%
  filter(term == "disaster") 

# Same
effects4


## 5.2 Average Monthly Effect per Year ####################

# We could then get the AVERAGE monthly effect PER YEAR
stat = effects4 %>%
  group_by(year) %>%
  summarize(avg = mean(estimate))

# View it
stat


# And we could plot the average monthly effect per year over time!
ggplot() +
  geom_col(data = stat, mapping = aes(x = year, y = avg)) +
  geom_line(data = stat, mapping = aes(x = year, y = avg))


# Finally, suppose we want to overlay this with our month-by-month plot.
# A little bit of data wrangling could get us there.

# We just need to turn the stat object's 'year' vector into a 'Date' type,
# rather than the 'dbl' (numeric) type.
# We could do this like so...
stat2 = stat %>%
  # Let's append a month and day, and turn it into a Date
  mutate(year = year %>% paste0("-01-01") %>% as.Date())


# We could combine these plots like so...
ggplot() +
  # We could plot the average effect per month
  geom_line(data = effects4, mapping = aes(x = date, y = estimate), color = "grey", linewidth = 2)  +
  geom_line(data = stat2, mapping = aes(x = year, y = avg), color = "steelblue")

# Or perhaps...
ggplot() +
  geom_col(data = stat2, mapping = aes(x = year, y = avg), fill = "grey") +
  geom_line(data = effects4, mapping = aes(x = date, y = estimate), color = "steelblue", linewidth = 2)


## 5.3 Clean Visual ####################

# Oooh, yeah, that's a nice one.
# Let's repeat all the code in one final blurb together.

# Load packages
library(dplyr) # for data-wrangling
library(readr) # for reading in data
library(ggplot2) # for visualization
library(tidyr) # for drop-na
library(broom) # for regression model helpers
library(texreg) # for quick model tables

# Save this solar dataset as an object in my environment
jp = read_csv("data/jp_solar.csv") %>%
  select(muni_code, date, year, solar_rate, disaster, pop) %>%
  arrange(muni_code, date) %>%
  # Remove outlier
  filter(date != "2017-09-25") %>%
  # Must order by city and time
  arrange(muni_code, date) %>%
  # Must group by municipality - IMPORTANT
  group_by(muni_code) %>%
  # Then lag the solar rate, using n to specify how many steps.
  mutate(
    # First let's lag by 1 row,
    solar_lag1 = lag(solar_rate, n = 1),
    # Then let's lag by 2 rows
    solar_lag2 = lag(solar_rate, n = 2),
    # Then let's lag by 3 rows
    solar_lag3 = lag(solar_rate, n = 3)) %>%
  ungroup()  


# Calculate statistics...
effects = jp %>%
  # For each month (and year)
  group_by(year, date) %>%
  # get the tidy() model table of disaster effects on solar rates
  reframe(  lm(formula = solar_rate ~ disaster ) %>% tidy()  ) %>%
  # Filter to disaster effects
  filter(term == "disaster") %>%
  # Convert year to a date format
  mutate(year = year %>% paste0("-01-01") %>% as.Date())

# Get average monthly effect per year
stat = effects %>%
  group_by(year) %>%
  summarize(avg = mean(estimate))


# Visualize!
ggplot() +
  geom_col(data = stat, mapping = aes(x = year, y = avg), fill = "grey") +
  geom_line(data = effects, mapping = aes(x = date, y = estimate), color = "steelblue", linewidth = 2) +
  theme_bw() +
  labs(title = "Monthly Effects of Disasters on Rooftop Solar Adoption",
       y = "Change in Rooftop Solar Units\nper 1000 residents",
       x = "Months (2014-2019)")


# Cleanup!
rm(list = ls())




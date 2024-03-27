# class_4_databases_3.R
# Series: Databases 3
# Topic: Database Functions: Grouping, Summarizing, and Reframing
# Prof. Fraser

# We're going to learn some 'level-2' data wrangling functions, 
# that allow us to reshape and merge different data sets.
# I STRONGLY encourage you to type the code out yourself - helps with muscle memory.
# It's like tiny puzzles! Yay puzzles!

# Below, we're going to learn to use the dplyr package (pronounced 'dip-ler')
# There are several Tasks and Learning Checks (LC 1, LC2, etc.) in this tutorial, 
# Please run through them all to try it out yourself!

# 1. Load packages and Data (~1 min)

# 2. summarize() (~2 min)

# 3. group_by() (~2 min)

# 4. reframe() (~2 min)

# 5. left_join() (~4 min)

# 6. APPLICATION: Visualizing Distributions

# Please progress through each of these tasks 
# and answer the learning check questions as you go.
# In each task, you will 'discover' the uses of each function, 
# and make inferences together. Support your colleagues!


# 0. Load Packages and Data ############################################

library(dplyr) # for data wrangling
library(ggplot2) # for visualization

# For the first section of this tutorial, 
# we're going to again, make our own small data.frame.

# First, we're going to make a hypothetical dataset of coffee purchases,
# around Boston metro stops
purchases <- data.frame(
  # A unique id number for each customer
  customer_id = c(1, 1, 1, 1, 1, 1, 
                  2, 2, 3, 3, 3, 3, 4, 5, 8),
  # Location of Dunkin Donuts where coffee was purchased
  location = c("Huntington", "Ruggles", "Ruggles",
               "Ruggles", "Hayden", "Hayden",
               "Ruggles", "Huntington", "Mass Ave", "Mass Ave",
               "Hayden", "Hayden", "Mass Ave", "Ruggles", "Ruggles"),
  # Total cost of purchase
  price = c(5.5, 5.5, 2.5, 4.5, 3.0, 4.5, 
            4.5, 3.5, 3.0, 3.5, 4.0, 4.5,
            2.5, 2.5, 2.5)
)

purchases # view it

# Next, we're going to make a hypothetical dataset of customer names
mycustomers <- data.frame(
  # Name of customer
  customer = c("Tim", "Kate","Jon", "Courtney", "David", "Sameera", "Melanie"),
  # Unique ID number of each customer
  customer_id = c(1, 2, 3, 4, 5, 6, 7),
  # rate of reward points they earn for each purchase
  rewards = c(2, 2, 1.5, 1.5, 1, 1, 1)
)

mycustomers # view it


# 1. summarize() ##########################################

# First, we're going to learn the summarize() function from dplyr...
# by putting it into action!

## LC 1  ########################################

# Learning Check:
# What does the summarize function do?

purchases$price %>% mean()

purchases %>% 
  summarize(mean_price = mean(price))

purchases %>% 
  summarize(mean_price = mean(price),
            sd_price = sd(price))


# notice how we use the ` ` marks to add spaces in our labels?
purchases %>% 
  summarize(`mean of price` = mean(price), 
            `sd of price` = sd(price))


# Oh, and what do we learn about mutate() vs. summarize() from comparing these two code chunks?
purchases %>% 
  summarize(mean_price = mean(price),
            sd_price = sd(price),
            label = "summary statistics")

purchases %>% 
  summarize(mean_price = mean(price),
            sd_price = sd(price)) %>%
  mutate(label = "summary statistics")



# 2. group_by() ##########################################

# Next, we're going to learn a helper function. 
# It only works when you pair it with another dplyr function.
# It splits your dataset into smaller datasets, using the values of your grouping variable.

## LC 2 #####################################

# Learning Check:
# So why would we want to use group-by? 
# Compare the following chunks below, and explain in two sentences below.

purchases %>%
  summarize(total_sales = sum(price))

purchases %>%
  filter(location == "Hayden") %>%
  summarize(total_sales = sum(price))

purchases %>%
  group_by(location) %>%
  summarize(total_sales = sum(price))

purchases %>%
  group_by(location) %>%
  summarize(total_sales = sum(price),
            mean_sales = mean(price))


purchases %>%
  group_by(location) %>%
  summarize(total_sales = sum(price),
            mean_sales = mean(price),
            number_of_sales = n()) 
# Whoa - that's a cool function; what is n() counting? 
# (Hint: if you add up the number of sales, what number is it?)


# 3. reframe() ###########################################

# If we want to summarize a table in a way 
# that will create multiple rows per group, 
# we need reframe() instead of summarize().

purchases %>%
  reframe(price_range = seq(
    from = min(price, na.rm = TRUE), 
    to = max(price, na.rm = TRUE), 
    by = 0.5))

# What did we just do??
#   We made a sequence of numbers with seq() 
#   that spans *from* the minimum price *to* the maximum price
#   spaced *by* intervals of 0.5 dollars.
#   But, that vector is has 7 values! 
#   summarize() shrinks data.frames into 1 row, so it won't work.
#   Instead, we need to `reframe()` that data to get multiple rows.

## LC 3 ###################################################

# Learning Check:

# We can use reframe() to create new summary tables of statistics, 
# in shapes different from the original table.
# Run the code chunk below.
# If we wanted to add the max price to 3rd row in this table, how might we adjust this code?
# Original version:
purchases %>%
  reframe(type = c("mean", "sd"),
          value = c(mean(price), sd(price)))

# Your Version with the max()
# ...




## LC 4 ###################################################

# Learning Check:
# We can even use group_by() together with reframe().
# Run the two chunks of code below.
# How did group_by() change our reframing process below?

purchases %>%
  reframe(range = c(min(price), max(price) ))

purchases %>%
  group_by(customer_id) %>%
  reframe(range = c(min(price), max(price) ))


# 4. left_join() ##########################################

# Last, let's learn the superpower of dplyr - joining datasets.
# Whenever you have a unique identifier - like an ID code for a customer reward account,
# you can use that to automatically transfer values from one data.frame to another.

# This is really helpful if you want to calculate how much each person purchased, 
# but their names are in a different dataset.

# We take our purchases dataset,
purchases %>%
  # And every time we have a match in the customer_id variable,
  left_join(by = "customer_id", 
            # we append the values for that matching row from our y data.frame.
            y = mycustomers)

# We can chain on some useful functions.
purchases %>%
  left_join(by = "customer_id",
            y = mycustomers) %>%
  group_by(customer_id, customer) %>%
  summarize(total_sales = sum(price))

# We have an NA, because one customer (customer #8) doesn't have a name.

## LC 5 #####################################

# Learning Check:
# We have many types of join functions, including left_join(), right_join(), and inner_join().
# I prefer left_join(), but that's just me.
# Using the definitions below, explain...
#    Why does each code chunk below have a different number of rows?

# left_join(): join customer data (right dataset) into the purchases data (left dataset)
purchases %>%
  left_join(by = "customer_id", y = mycustomers)

# right_join(): join purchases data (left dataset) into the customer data (right dataset)
purchases %>%
  right_join(by = "customer_id", y = mycustomers)

# inner_join(): join purchases and customer data, and keep just the matches
purchases %>%
  inner_join(by = "customer_id", y = mycustomers)


# I strongly recommend you just forget about inner_join and always use left_join().


## LC 4 #####################################

# Learning Check:
# Using the code below, who has the most Dunkin' Rewards points? 
# Where did that rewards_points field come from?

purchases %>%
  left_join(by = "customer_id", y = mycustomers) %>%
  mutate(rewards_points = price * rewards) %>%
  group_by(customer) %>%
  summarize(total_rewards_points = sum(rewards_points, na.rm = TRUE))



# APPLICATION: Visualizing distributions ###########################

# We used group_by() and summarize() above, 
# but another way to understand your data is to visualize it!

# These all allow us to visualize distributions of our coffee purchases by location.

# A. We've learned histograms...

# Histograms by panel
purchases %>%
  ggplot(mapping = aes(x = price, fill = location)) +
  geom_histogram() +
  facet_wrap(~location) 

# But we can also use their close cousin, density plots!

# density plots (these approximate the shape of distributions)
purchases %>%
  ggplot(mapping = aes(x = price, fill = location)) +
  geom_density() +
  facet_wrap(~location) 




# B. We've learned boxplots....

# A boxplot!
purchases %>%
  ggplot(mapping = aes(x = location, y = price)) +
  geom_boxplot()

# But we can also use their close cousin, violin plots!

# A 'violin' plot!
purchases %>%
  ggplot(mapping = aes(x = location, y = price)) +
  geom_violin()



# C. We can even use error bars to make the interquartile range.
purchases %>%
  # grouping by location
  group_by(location) %>%
  # calculating summary statistics
  summarize(lower = quantile(price, probs = 0.25),
            upper = quantile(price, probs = 0.75)) %>%
  # And adding some new aes() criteria, like 'ymin =' and 'ymax ='
  ggplot(mapping = aes(x = location, ymin = lower, ymax = upper)) +
  # Plus this new function
  geom_errorbar()



# Or we can depict the same range with lines
purchases %>%
  # grouping by location
  group_by(location) %>%
  # calculating summary statistics
  summarize(lower = quantile(price, probs = 0.25),
            median = quantile(price, probs = 0.5),
            upper = quantile(price, probs = 0.75)) %>%
  # And adding some new aes() criteria, like 'ymin =' and 'ymax ='
  ggplot(mapping = aes(x = location, y = median, ymin = lower, ymax = upper)) +
  # Plus this new function!
  geom_linerange() +
  # With points for the median!
  geom_point() 



# Each of these show distributions!

### Learning Check 8

# Visualize the distribution of rewards points earned by location, 
# using one of the new strategies above!
# Hint: you will need to use left_join()! 




# PRACTICE 1: Databases 1
# Topic: Data Wrangling
# Prof. Tim Fraser
#
# This is an older version of the 1C_databases_1.R exercise.
# It uses a different example dataset.
# You are welcome to use this for more practice.
#
# Sometimes we need to edit and arrange our data better.
# We call this 'data wrangling,' and it's a super useful skill.
# Small functions can save you dozens of hours of work,
# and it makes you really employable. 

# Plus, it's like tiny puzzles! Yay puzzles!

# Below, we're going to learn to use the dplyr package (pronounced 'dip-ler')
# There are several Tasks and Learning Checks (LC 1, LC2, etc.) in this tutorial, 
# Please run through them all to try it out yourself!

# 1. Load packages and Data (~1 min)

# 2. Using Pipelines (~1 min)

# 3. select() and rename() (~2 min)

# 4. mutate() (~4 min)

# 5. filter() and arrange() (~3 min)

# Please progress through each of these tasks 
# and answer the learning check questions as you go.
# In each task, you will 'discover' the uses of each function, 
# and make inferences together. Support your colleagues!

# 0. SETUP ###################################

## 0.1 Load Packages and Data #################################

library(dplyr) # for data wrangling


# For the first section of this tutorial, 
# we're going to make our own small data.frame.

# First, we're going to make a hypothetical dataset of consumer coffee purchases,
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


# Let's view each.
purchases


# Next, we're going to make a hypothetical dataset of customer names
mycustomers <- data.frame(
  # Name of customer
  customer = c("Tim", "Kate","Jon", "Courtney", "David", "Sameera", "Melanie"),
  # Unique ID number of each customer
  customer_id = c(1, 2, 3, 4, 5, 6, 7),
  # rate of reward points they earn for each purchase
  rewards = c(2, 2, 1.5, 1.5, 1, 1, 1)
)


mycustomers


## LC 1 #################################################

# Learning Check:
# What does a row refer to in the 'purchases' dataset? 
# What does a row mean in the 'mycustomers' dataset?


## 0.2 Using Pipelines ####################################

# The dyply package contains many useful functions, like glimpse().
# Another key function is the pipeline, represented by this symbol: %>%

# Pipelines connect data (dataframes, vectors, or values) to functions.



## LC 2 ######################################################

# Learning Check:
# Run the following functions. 
# Is the output for the following the same or different? What does the pipeline do?
# Why are pipelines helpful?

mean(purchases$price)
purchases$price %>% mean()

min(purchases$price)
purchases$price %>% min()

unique(purchases$location) # ooh, a new function - what does unique() do?
purchases$location %>% unique()

length(unique(purchases$location)) # ooh, another new function - what does length() do?
purchases$location %>% unique() %>% length()


# From now on, please always use pipelines when you can! 
# Life gets so much easier.



# 1. select() and rename() ###################################

# The dplyr package gives us several helpful functions 
# which we can pair with pipelines to make life easier.

## LC 3 ############################################

# Learning Check:
# Compare the following four chunks of code. 
# What does the select() function do? Why do the contents of the select function matter?

# Chunk 1
purchases

# Chunk 3
purchases %>%
  select(location, price)

# Chunk 3
purchases %>%
  select(-customer_id)

# Chunk 4
purchases %>%
  select(contains("e"))




## LC 4 #############################################

# Learning Check:
# Compare the following chunks of code. 
# What does the rename() function do? How is it different from select?

purchases

purchases %>%
  select(shop = location, price)

purchases %>%
  select(shop = 2, price)

purchases %>%
  rename(shop = location)

purchases %>%
  rename(shop = 2)



# 2. mutate() ###########################################

# Sometimes, we need to change values in a column (vector).
# We can do that by replacing an old vector with a new vector, 
# or just adding a new vector. mutate() helps us do that.

## LC 5 ###################################

# Learning Check:
# Compare the following chunks of code.

# What does the mutate function do? 
# Under what conditions can you use it?
# Why does it help us more than making calculations in the format of 'mydataframe$myvector - 1' ?

purchases

purchases %>%
  mutate(price = price - 1)

purchases$price - 1

purchases %>%
  mutate(price = 1)

purchases %>%
  mutate(discount = 1)

purchases %>%
  mutate(discount = 1,
         price = price - discount)



## LC 6 #########################################

# Learning Check:
# The values you assign a variable in mutate 
# must always be either just 1 value in length, 
# or the length of the data.frame (15 rows in this case).

# Why? Because you can't fit 16 values into 15 rows, for example.
# and because R knows to repeat 1 value 15 times if you give it just 1 value, 
# but R won't know what to do otherwise.

# Check out the error we get below. Why does this happen?

purchases %>%
  mutate(discount = c(1, 2, 3))

# Do we get an error when we do the following two code chunks? Why or why not
purchases %>%
  mutate(discount = 1)

purchases %>%
  mutate(discount = c(1, 1.5, 3, 1, 3, 1, 2, 1, 3, 1, 0 , 0, 1, 1, 2))



# 3. filter() and arrange() #################################

## LC 7 #########################################################

# The filter() function is a real powerhouse for data wrangling.
# Compare the original purchases dataframe to when we apply the filter function. 
# What does it do in each situation? Please describe what it does above each chunk.

purchases

purchases %>%
  filter(price > 4)

purchases %>%
  filter(price < 4)

purchases %>%
  filter(price == 5.5)

purchases %>%
  filter(price != 5.5)

purchases %>%
  filter(price %in% c(3.0, 5.5))

purchases %>%
  filter(!price %in% c(3.0, 5.5))



## LC 8 #############################################

# Learning Check:
# Let's try some more complex ones. 
# What happens when you combine them? What's the difference between a comma, '&', and '|'?

purchases %>%
  filter(price > 4, location == "Hayden")

purchases %>%
  filter(price > 4 & location == "Hayden")

purchases %>%
  filter(price > 4 | location == "Hayden")



## LC 9 ################################################

# Learning Check
# arrange() is a very powerful function. Compare the following code chunks. 
# What happens when you arrange by price? How does it change when we add desc(), or filter()?

purchases %>%
  arrange(price)

purchases %>%
  arrange(desc(price))

purchases %>%
  arrange(location, desc(price))

purchases %>%
  filter(location %in% c("Hayden", "Huntington")) %>%
  arrange(location, desc(price))




# You're done!
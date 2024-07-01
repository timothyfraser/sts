# 4P_databases_2.R
# Practice: Databases 2
# Topic: Airline Economics
# Prof: Tim Fraser

## (Modern Dive Learning Check 3.20)
# https://moderndive.com/3-wrangling.html

#################################### 
# Prompt: Airline Economics
####################################

# Airlines have a strong interest in making sure that 
# every seat on one of their planes is full. 
# That's because, like hotels, airlines buy planes, 
# but those seats are not always occupied.

# An airline industry measure of a passenger airline’s capacity is 
# the available seat miles, which is equal to the 
# total number of seats multiplied by the 
# number of miles or kilometers flown 
# summed over all flights.

# Please calculate the number of available seat miles, 
# drawing on the multiple datasets available in 
# the nycflights13 dataset.

# Using the datasets included in the nycflights13 package, 
# compute the available seat miles for each airline 
# sorted in descending order. 

# After completing all the necessary data wrangling steps, 
# the resulting data frame should have 16 rows (one for each airline)
# and 2 columns (airline name and available seat miles). 

##################################################
# Hint:
##################################################

# Crucial: Unless you are very confident in what you are doing, 
# it is worthwhile not starting to code right away. 
# Rather, first sketch out on paper all the necessary 
# data wrangling steps not using exact code,
# but rather high-level pseudocode that is informal
# yet detailed enough to articulate what you are doing. 
# This way you won’t confuse what you are trying to do (the algorithm) 
# with how you are going to do it (writing dplyr code).




###################################################################
# Task 1: Load dplyr, nycflights13, and ggplot2 packages
###################################################################





###################################################################
# Task 2: Examine datasets
###################################################################

# - Take a close look at all the datasets using the function glimpse(): 
# flights, weather, planes, airports, and airlines
# Identify which variables are necessary to compute available seat miles.



###################################################################
# Task 3: Examine Modern Dive Table 3.2
###################################################################

# - Figure 3.7 showing how the various datasets can be joined will 
# also be useful. Consider the data wrangling verbs in 
# Modern Dive Table 3.2 as your toolbox!
# https://moderndive.com/3-wrangling.html






###################################################################
# Task 4: Build Dataset
###################################################################

# - Gather dataset of available seat miles, using instructions above.
# - At the end, be sure to get the actual airline names, 
# and arrange in descending order by available seat miles





###################################################################
# Task 5: Visualize Total Available Seat Miles
###################################################################

# - 5A: Visualize which airlines have the most total available seat miles.

# - Add a theme, labels, and some color.

# - trick: to order the x-axis by a variable, use:
#   x = reorder(my_xaxis_variable, my_yaxis_variable)
#   (variable names are hypothetical)

# - might be easier to measure total available seat miles in millions.
# - try adding + coord_flip() 


# - 5B: What other ways could you hypothetically visualize this?

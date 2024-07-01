README `/bluebikes`

Welcome! This folder presents the main materials for investigating the Boston Bluebikes Dataset, a compendium of a decade of bikesharing data from the Boston Bluebikes (formerly Hubway) program.

Check out our working paper here for more details.
https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4076776

Remember to cite!
Fraser, Timothy and Van Woert, Katherine and Olivieri, Sophia and Baron, Jonathan and Buckley, Katelyn and Lalli, Pamela, Cycling Cities: Measuring Transportation Equity in Bikeshare Networks. Available at SSRN: https://ssrn.com/abstract=4076776 or http://dx.doi.org/10.2139/ssrn.4076776

Here's some brief introduction to the data presented.


First step: Unzip the `data/bluebikes/bluebikes.zip` file into `data/bluebikes/bluebikes.sqlite`.

```{r}
unzip("data/bluebikes/bluebikes.zip")

```


We're going to use this SQLite database to access a refined version of our data. SQLite is really handy because we can ask it to do lots of number-crunching without loading all the millions of rows into our R environment, which would normally cause it to crash. Instead, we can feed SQLite basic dplyr functions, like ```select()```, ```mutate()```, ```filter()```, ```group_by()```, and ```summarize()```, and then ask the SQLite database to ```collect()``` the resulting data and give it to us in R. This output (should) be much, much, much smaller, at a size R can handle.

Our data is saved in ```our_data```. Please use this folder for all your data needs. I'd recommend not looking elsewhere, as it gets messy real fast :) Here's our data:

## {.tabset .tabset-pills}

### Datasets

- ```data/bluebikes/bluebikes.sqlite```: a HUGE compendium of datasets. These files are a little too big to access on their own, so we will access them via the SQLite database. Contains:

- ```tally_rush_edges``` (in ```bluebikes.sqlite```): a dataset tallying number of rides each day during morning and evening rushhour.

- ```tally_rush``` (in ```bluebikes.sqlite```): a dataset tallying number of rides each day during morning and evening rushhour, for EACH START AND END STATION. That's a LOT!

- ```data/bluebikes/stationbg_dataset.rds```: an ```sf``` dataset of geolocated points for all bluebike stations present in our data. Contains the ```geoid``` of the census block group each station is located in. Also contains all the traits of that block group!


Others You might run into, but don't need to think about as much.

- ```data/bluebikes/dates.rds```: a dataset of dates and days of the week for the past 10 years. Useful for filtering, but not strictly necessary.

- ```data/bluebikes/bgdataset.rds```: an ```sf``` dataset of all census block group polygons in Boston. Contains all the traits of each block group.


### Codebook

Here's a quick summary of all variable names you will run into.

- ```code```: unique ID for each bluebikes station.

- ```geoid```: unique ID for each census block group.

- ```count```: total rides occuring during that time, between those places.

Example Demographics Variable Set:

- ```pop_density_2020```: population density per square kilometer in 2020. Some places are missing data.

- ```pop_density_2020_smooth5```: population density, with missing data filled in by taking the median of their 5 nearest neighboring census block groups.

- ```pop_density_2020_smooth10```: population density, with missing data filled in by taking the median of their 10 nearest neighboring census block groups. Either is fine to use.

I generated many other variables too, saved in ```data/bluebikes/stationbg_dataset.rds.``` Let's look at them real quick.

```{r, message = FALSE, warning = FALSE}
read_rds("data/bluebikes/stationbg_dataset.rds") %>% 
  select(-contains("smooth")) %>%
  names()
```
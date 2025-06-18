# download_data.R
# A script for downloading all necessary data for this database
# Note: for instructor use only.

library(dplyr)
library(readr)
library(downloader)
library(sf)
setwd(rstudioapi::getActiveProject())

# Stop message
stop("Don't run this script.\nThis is just for the instructor to download the data. \nSee data/risk_index/README.md for how to use the data.")


# Risk Index Data #####################################

# Download file file
downloader::download(
  url = "https://hazards.fema.gov/nri/Content/StaticDocuments/DataDownload//NRI_Table_CensusTracts/NRI_Table_CensusTracts.zip",
  destfile = "data/risk_index/file.zip")
# Unzip file
unzip(zipfile = "data/risk_index/file.zip", exdir = "data/risk_index")

# Let's extract a simplified version of this entry...
vars = read_csv("data/risk_index/NRIDataDictionary.csv") %>%
  select(name = `Field Name`)

# Save core variables here...
read_csv("data/risk_index/NRI_Table_CensusTracts.csv",
         n_max = Inf, show_col_types = FALSE) %>%
  select(any_of(vars$name[1:43]))  %>%
  select(-STATE, -COUNTY, -STATEFIPS, -COUNTYTYPE, -COUNTYFIPS, -TRACT, -NRI_ID) %>%
  saveRDS("data/risk_index/risk.rds")

# Save hazard specific variables here...
read_csv("data/risk_index/NRI_Table_CensusTracts.csv",
         n_max = Inf, show_col_types = FALSE) %>%
  select(any_of(vars$name[c(1:13, 44:469) ]))  %>%
  select(-STATE, -COUNTY, -STATEFIPS, -COUNTYTYPE, -COUNTYFIPS, -TRACT) %>%
  select(STATEABBRV, STCOFIPS,TRACTFIPS, contains("_EVNTS"), contains("_RISKV"), contains("_RISKS"), contains("_RISKP")) %>%
  saveRDS("data/risk_index/risk_others.rds")


# Extract county metadata
read_csv("data/risk_index/NRI_Table_CensusTracts.csv",
         n_max = Inf, show_col_types = FALSE) %>%
  select(STATE, STATEABBRV, COUNTY, STCOFIPS) %>%
  distinct() %>%
  saveRDS("data/risk_index/counties.rds")

# Delete original csv, no longer necessary.
unlink("data/risk_index/NRI_Table_CensusTracts.csv")

# Delete original zipfile, no longer necessary.
unlink("data/risk_index/file.zip")


# SPATIAL DATA #####################################
library(sf)
# Download 1 state's risk...
tigris::tracts(state = "NY", cb = TRUE) %>%
  st_as_sf() %>% st_transform(crs = 4326) %>%
  select(state = STUSPS, geoid = GEOID, geometry) %>%
  write_sf("data/risk_index/tracts_ny.geojson", append = TRUE)

# Now download entire country...
# for each state...
for(i in 2:length(state.abb)){
  # download that state...
  tigris::tracts(state = state.abb[i], cb = TRUE) %>%
    st_as_sf() %>% st_transform(crs = 4326) %>%
    select(state = STUSPS, geoid = GEOID, geometry) %>%
    write_sf("data/risk_index/tracts.geojson", append = TRUE)
  # Completion message
  cat("\n", state.abb[i], "\n")
}

# Zip the geojson
zip(zipfile = "data/risk_index/tracts.zip", files = "data/risk_index/tracts.geojson")

# Delete
unlink("data/risk_index/tracts.geojson")

read_rds("data/risk_index/risk_others.rds") %>% head(1) %>% glimpse()


read_csv("data/risk_index/NRI_HazardInfo.csv") %>% 
  select(Hazard, Prefix, Start, End_)

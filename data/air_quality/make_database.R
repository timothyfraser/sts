# make_database.R

# Need to make this into a database?

library(dplyr) # for data wrangling
library(readr) # for reading data
library(DBI) # for database connections
library(RSQlite) # for SQLite
library(dbplyr) # for database SQL queries with dplyr


# Create a database connection object at this path
db = dbConnect(RSQLite::SQLite(), "mydb.sqlite")  

# Get an empty data.frame with the right headers
headers = read_csv("data/air_quality/air_quality.csv", n_max = 0)

# Initialize the table and field/column types
dbWriteTable(conn = db, name = "air_quality", value = headers, row.names = FALSE)

# Load data into memory, in chunks if needed
data = read_csv("data/air_quality/air_quality.csv")

# Append the data into the table, in chunks if needed
dbWriteTable(conn = db, name = "air_quality", value = data, append = TRUE)

# Test query the data
db %>% tbl("air_quality") %>% head() %>% collect()

# Disconnect from the database
dbDisconnect(db)

# Cleanup
rm(list = ls())

# Remove database
unlink("mydb.sqlite")

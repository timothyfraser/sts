# 8P_databases_3.R
# Topic: Make Your First Database!
# Tim Fraser

# This script helps you query from and develop a Supabase database!
# 
# Supabase is a cloud-hosted PostgreSQL database that provides:
# - A free tier perfect for learning
# - Direct PostgreSQL access via DBI/RPostgres
# - Row Level Security (RLS) for controlling who can see what data
#
# In this script, you'll learn:
# 1. How to configure Supabase tables for instructor-only vs student-only access
# 2. How to query Supabase using DBI and RPostgres from R

# 0. SETUP ##################################################

## 0.1 Load Packages #################################

library(DBI)      # for database connections
library(RPostgres) # for PostgreSQL connections (Supabase uses PostgreSQL)
library(dplyr)     # for data manipulation with pipelines
library(dbplyr)    # for extending dplyr to work with databases
library(readr)    # for reading CSV files

## 0.2 Load Environment Variables ####################

# Load environment variables from .env file
# readRenviron() is a built-in R function that reads .env files
# No external package needed!
readRenviron("workshops/8P_database/.env")

# Get Supabase database connection details from environment variables
# You'll need to add these to your .env file:
# SUPABASE_HOST=db.your-project-id.supabase.co
# SUPABASE_PORT=5432
# SUPABASE_DB=postgres
# SUPABASE_USER=postgres
# SUPABASE_PASSWORD=your-database-password-here
# SUPABASE_SERVICE_PASSWORD=your-service-role-password-here (instructor only!)

SUPABASE_HOST = Sys.getenv("SUPABASE_HOST")
SUPABASE_PORT = Sys.getenv("SUPABASE_PORT", "5432")
SUPABASE_DB = Sys.getenv("SUPABASE_DB", "postgres")
SUPABASE_USER = Sys.getenv("SUPABASE_USER", "postgres")
SUPABASE_PASSWORD = Sys.getenv("SUPABASE_PASSWORD")

# Check if credentials are set


# 1. SUPABASE CONFIGURATION GUIDE ##################################################

# Supabase uses Row Level Security (RLS) to control who can see what data.
# Here's how to set up tables that only you (instructor) can see vs tables 
# that only students can see:

## 1.1 Instructor-Only Tables #################################

# OPTION A: Use a Private Schema (Recommended)
# 
# Create tables in a schema other than 'public' (e.g., 'private').
# Tables in non-public schemas are NOT accessible via standard connections.
# Only accessible with service role password or explicit schema grants.
#
# In Supabase SQL Editor, run:
# CREATE SCHEMA private;
# CREATE TABLE private.scores (
#   id SERIAL PRIMARY KEY,
#   name TEXT,
#   score INTEGER
# );
# 
# Then, access these tables using the root password.


# 2. CONNECTING TO SUPABASE FROM R ##################################################

## 2.1 Connect to Supabase Database #################################

# Connect to Supabase using DBI and RPostgres
# Supabase is PostgreSQL, so we use RPostgres driver
connect = function(password = SUPABASE_PASSWORD, user = SUPABASE_USER) {
  db = dbConnect(
    drv = RPostgres::Postgres(),
    host = SUPABASE_HOST,
    port = as.integer(SUPABASE_PORT),
    dbname = SUPABASE_DB,
    user = user,
    password = password
  )
  return(db)
}

db = connect()

# db %>% dbListTables()


# 6. RESOURCES ##################################################

# Supabase Documentation:
# - PostgreSQL Connection: https://supabase.com/docs/guides/database/connecting-to-postgres
# - Row Level Security: https://supabase.com/docs/guides/database/postgres/row-level-security

# R Documentation:
# - DBI: https://dbi.r-dbi.org/
# - RPostgres: https://rpostgres.r-dbi.org/
# - dbplyr: https://dbplyr.tidyverse.org/

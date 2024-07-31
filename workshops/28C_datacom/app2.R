#' @name app2.R
#' @title Test Live App for Streamlining a ShinyApp
#' @description 
#' When working with really big databases,
#' How can we get rid of errors due to over-filtering?
#' 
#' We learned one method - updateSelectInput() - but it only works when updating the input can actually fix the visual.
#' Sometimes, it is not physically possible
#' to generate a valid subset of data from certain filters.
#' So, rather than showing a gross 'Error' - we might want to say, 'That data is not available now.'
#' How would we do that?
#' 
#' When you run this app, to see the error and our solution,
#' try selecting station W32008 and year 2011.
#' You'll see the contrast between the two plots more clearly.
#' 
#'
# To run this app, you're going to need to first run this commented-out code,
# To add a copy of the bluebikes database to our test app folder 28C_datacom
# unzip(zipfile = "data/bluebikes/bluebikes.zip", junkpaths = TRUE, exdir = "workshops/28C_datacom")
# gc()

# To run this app, you can either click Run App or run this code below.
# shiny::runApp("workshops/28C_datacom/app2.R")

library(DBI) # database connections
library(RSQLite) # database connections
library(dplyr) # data wrangling
library(readr) # reading in data
library(ggplot2) # data viz
library(shiny)
library(bslib)
library(stringr)


ui = function(){
  
  # Connect
  db = dbConnect(drv = RSQLite::SQLite(), "bluebikes.sqlite")
  # Get all possible stations
  choices_station = db %>%
    tbl("stationbg_dataset") %>%
    select(code) %>%
    collect()
  # Disconnect
  dbDisconnect(db)
  
  page(
    # INPUTS ###########################
    card(
      ## $year ##########################
      # Select year of data
      selectInput(inputId = "year", label = "CHOOSE YEAR", choices = c(2011:2021), selected = "2020"),
      ## $station #######################
      # Select bluebikes station
      selectInput(inputId = "station", label = "CHOOSE STATION",
                  choices = choices_station$code, selected = "A32000")
    ),
    # OUTPUTS ####################
    card(
      ## $visualbad ######################
      # One plot output
      plotOutput(outputId = "visualbad"),
      ## $visualgood ######################
      # A plot output showing our visual, but no 'Error' warning
      plotOutput(outputId = "visualgood")
    )
  )
  
}


server = function(input, output, session){
  
  
  # OUTPUTS ##########################
  
  ## $visualbad #############################
  # Here's a visual that sometimes will not render properly.
  # In this particular case, it produces a blank plot 
  # if you select station W32008 and year 2011.
  output$visualbad = renderPlot({
    
    # require input$year to be valid before proceeding...
    myyear = input$year
    mystation = input$station
    
    # Connect
    db = dbConnect(drv = RSQLite::SQLite(), "bluebikes.sqlite")
    # Query
    data = db %>%
      tbl("tally_rush_edges") %>%
      filter(rush == "pm") %>%
      mutate(year = stringr::str_sub(day, 1,4)) %>%
      # Filter to one specific year
      filter(year == myyear ) %>%
      # Filter to one specific station
      filter(start_code == mystation ) %>%
      group_by(year, start_code, end_code) %>%
      summarize(trip = n()) %>%
      collect()
    # Disconnect
    dbDisconnect(db)

    # Visualize the resulting data
    ggplot() +
      geom_tile(data = data, mapping = aes(x = start_code, y = end_code, fill = trip))
    
  }) %>% bindEvent({ input$year; input$station })
  
  
  ## $visualgood ############################
  # Here's a version of this visual that is 'error-controlled'
  # We add an if-else statement.
  # If it passes that statement, we render the plot.
  # If it doesn't pass that statement, we render a default not-available plot message.
  output$visualgood = renderPlot({
    
    # require input$year to be valid before proceeding...
    myyear = input$year
    mystation = input$station
    
    # Connect
    db = dbConnect(drv = RSQLite::SQLite(), "bluebikes.sqlite")
    # Query
    data = db %>%
      tbl("tally_rush_edges") %>%
      filter(rush == "pm") %>%
      mutate(year = stringr::str_sub(day, 1,4)) %>%
      # Filter to one specific year
      filter(year == myyear ) %>%
      # Filter to one specific station
      filter(start_code == mystation ) %>%
      group_by(year, start_code, end_code) %>%
      summarize(trip = n()) %>%
      collect()
    # Disconnect
    dbDisconnect(db)
    
    
    # Test Condition
    # Design a condition test here.
    # How many rows of data are available?
    n = length(data$year) > 0
    # If no rows are available, make a default plot.
    if(n == 0){
      # Make a blank plot
      ggplot() +
        # Add some text message
        geom_text(mapping = aes(x = 0, y = 0, label = "Sorry! That query isn't currently supported.")) +
        # Make a nice blank theme
        theme_void()
      
      # But if data IS available, make the real plot.
    }else if(n > 0){
      # Visualize the resulting data
      ggplot() +
        geom_tile(data = data, mapping = aes(x = start_code, y = end_code, fill = trip))
    }
    
  }) %>% bindEvent({ input$year; input$station })
  
  
  
    
}


shinyApp(ui = ui, server = server)

#' @name app.R
#' @title Test Live App for Streamlining a ShinyApp
#' @description 
#' When working with ENORMOUS databases,
#' how can we debug our apps?
#' Here's a process of debugging - what's wrong with my database filter?

# To run this app, you're going to need to first run this commented-out code,
# To add a copy of the bluebikes database to our test app folder 28C_datacom
# unzip(zipfile = "data/bluebikes/bluebikes.zip", junkpaths = TRUE, exdir = "workshops/28C_datacom")
# gc()

# To run this app, you can either click Run App or run this code below.
# shiny::runApp("workshops/28C_datacom/app.R")
# Bluebikes 

library(DBI) # database connections
library(RSQLite) # database connections
library(dplyr) # data wrangling
library(readr) # reading in data
library(ggplot2) # data viz
library(shiny)
library(bslib)
library(stringr)


ui = function(){
  
  page(
    # INPUTS ###########################
    card(
      ## $year ##########################
      # One input
      selectInput(inputId = "year", label = "CHOOSE YEAR", choices = c("2018", "2019", "2020"), selected = "2020")
    ),
    # OUTPUTS ####################
    card(
      ## $visual ######################
      # One plot output
      plotOutput(outputId = "visual")
    ),
    # Debugger
    # Let's make a card full of text outputs for debugging
    card(
      ## $text #############################
      verbatimTextOutput(outputId = "text"),
      ## $text2 #############################
      verbatimTextOutput(outputId = "text2"),
      ## $text3 #############################
      verbatimTextOutput(outputId = "text3"),
      ## $text4 #############################
      verbatimTextOutput(outputId = "text4"),
      ## $text5 #############################
      verbatimTextOutput(outputId = "text5")
    )
    
  )
  
}


server = function(input, output, session){

  
  # REACTIVE ###########################
  
  # network() ###########################
  # Reactive Dataframe
  network = reactive({
    # require input$year to be valid before proceeding...
    myyear = input$year
    # Connect
    db = dbConnect(drv = RSQLite::SQLite(), "bluebikes.sqlite")
    # Query
    data = db %>%
      tbl("tally_rush_edges") %>%
      filter(rush == "pm") %>%
      mutate(year = stringr::str_sub(day, 1,4)) %>%
      filter(year == myyear ) %>%
      group_by(year, start_code, end_code) %>%
      summarize(trip = n()) %>%
      collect()
    # Disconnect
    dbDisconnect(db)
    # Return Result
    data
  }) %>% bindEvent({ input$year })



  
  # OUTPUTS ##########################
  
  # $visual #############################
  output$visual = renderPlot({
    # Okay, here's our visual.
    ggplot() +
      geom_tile(data = network(), mapping = aes(x = start_code, y = end_code, fill = trip))

  }) %>% bindEvent({ network() })


  ## $text #############################
  output$text = renderText({
    # Connect
    db = dbConnect(drv = RSQLite::SQLite(), "./bluebikes.sqlite")
    # Get a vector of tables
    mytabs = db %>% dbListTables()
    # Disconnect
    dbDisconnect(db)
    # Collapse the table names
    mystring = paste0(mytabs, collapse = ", ")
    # Return the string
    mystring
  }) %>% bindEvent({ input$year })
  
  
  ## $text2 #############################
  output$text2 = renderText({
    # Connect
    db = dbConnect(drv = RSQLite::SQLite(), "./bluebikes.sqlite")

    data = db %>%
      tbl("tally_rush_edges") %>%
      filter(rush == "pm") %>%
      mutate(year = stringr::str_sub(day, 1,4)) %>%
      head(1) %>%
      collect()
    
    dbDisconnect(db)
    
    data$year[1]
  })
  
  ## $text3 #############################
  output$text3 = renderText({
    input$year
  })
  
  ## $text4 #############################
  # Let's test - can we query it non-reactively?
  # eg. without using input$year
  output$text4 = renderText({
    
    # Connect
    db = dbConnect(drv = RSQLite::SQLite(), "./bluebikes.sqlite")
    
    data = db %>%
      tbl("tally_rush_edges") %>%
      filter(rush == "pm") %>%
      mutate(year = stringr::str_sub(day, 1,4)) %>%
      filter(year == "2020") %>%
      collect()
    
    # One strategy is to print the data to the console
    print(data)
    
    # success - so looks like the issue must be something about our filter()
    # having to do with input$year specifically
    dbDisconnect(db)
    
    data$year[1]

  }) %>% bindEvent({ input$year })

  ## $text4 #############################
  # Let's test - can we query it non-reactively?
  # eg. without using input$year
  output$text4 = renderText({
    
    # Connect
    db = dbConnect(drv = RSQLite::SQLite(), "./bluebikes.sqlite")
    
    data = db %>%
      tbl("tally_rush_edges") %>%
      filter(rush == "pm") %>%
      mutate(year = stringr::str_sub(day, 1,4)) %>%
      filter(year == "2020") %>%
      collect()
    
    # One strategy is to print the data to the console
    print(data)
    
    # success - so looks like the issue must be something about our filter()
    # having to do with input$year specifically
    dbDisconnect(db)
    
    data$year[1]

  }) %>% bindEvent({ input$year })
  
  
  ## $text5 #############################
  # Let's test - can we query it reactively?
  # eg. using input$year
  output$text5 = renderText({

    # YES! The issue was kind of funky!
    # It's always a good idea to declare your inputs EARLY in a rendering function.
    # This is because it forces R to not evaluate the rest of your rendering function
    # until that input$year is in fact valid.
    # For example, alternatively, we could get stuck **waiting** 
    # for input$year to become a real value at runtime,
    # and unless we have some other process to trigger, 
    # it might only render one time at the beginning, before input$year actually loads.
    # Weird, right?
    myyear = input$year
    
    # Connect
    db = dbConnect(drv = RSQLite::SQLite(), "./bluebikes.sqlite")
    
    data = db %>%
      tbl("tally_rush_edges") %>%
      # Let's filter in a little futher.
      filter(rush == "pm", start_code == "A32000") %>%
      mutate(year = stringr::str_sub(day, 1,4)) %>%
      filter(year == myyear) %>%
      collect()
    
    # One strategy is to print the data to the console
    print(data)
    
    # success - so looks like the issue must be something about our filter()
    # having to do with input$year specifically
    dbDisconnect(db)
    
    data$year[1]
    
  }) %>% bindEvent({ input$year })
  
  
}


shinyApp(ui = ui, server = server)

#' @name app2.R
#' @author Tim Fraser
#' @title nycflights app V2
#' @description 
#' 
#' Next, let's try and bring the user interface up to a bare minimum.
#' We can use `shiny`'s functions to improve our `ui`, like:
#' - `sidebarLayout()`
#' - `sidebarPanel()`
#' - `mainPanel()`
#' - `headerPanel()`
#' - `column()`
#' - `h1()` and `h2()`

global = function(){   
  
  library(dplyr) # data wrangling
  library(readr) # reading data
  library(ggplot2) # data vizualization
  
  library(shiny) # main shiny app package
  
}


ui = function(){   
  
  # data #################################################
  # Get any helper data you need to start
  airlines = read_csv("airlines.csv")
  months = read_csv("months.csv")
  # Make a named vector, so you can select by Names but get back specific ids, eg. "Dec" = 12
  choices_month = setNames(object = months$month, nm = months$month_name) 
  choices_airlines = setNames(object = airlines$carrier, nm = airlines$name)
  
  
  fluidPage(
    title = "NYC Flights",
    # header ###############################################
    # Make a header panel
    headerPanel(title = h1("NYC Flights Dashboard (2013)")),
    # Make a split layout
    sidebarLayout(
      position = "left", fluid = TRUE,
      # sidebar ###################################
      # With this sidebar...
      sidebarPanel = sidebarPanel(
        ## selectors #########################################
        h2("CHOOSE YOUR SCENARIO"),
        # Create menu for changing inputs
        selectInput(inputId = "month", label = "MONTH", choices = choices_month, selected = 1),
        selectInput(inputId = "carrier", label = "AIRLINE", choices = choices_airlines, selected = "AA")      
      ),
      # main #########################################
      # and with this main panel
      mainPanel = mainPanel(
        h2("AIRLINE PERFORMANCE OVER TIME"),
        ## plotters ###########################################
        # Create plots
        column(width = 6, plotOutput(outputId = "plot_one_month") ),
        column(width = 6, plotOutput(outputId = "plot_one_carrier") ),
        # Below that,
        textOutput("text_highlight")
      )
    )
  )
  
  
}



server = function(input, output, session){  
  
  # data ########################################
  # Read in data
  airlines = read_csv("airlines.csv")
  months = read_csv("months.csv")
  flights = read_csv("flights_sample.csv") %>% 
    # Let's take a random sample
    sample_n(size = 20000) %>%
    # And get the main variables we care about here
    select(month, day, carrier, origin, 
           arr_delay, arr_time, sched_arr_time)
  
  # stats #######################################
  # Wherever possible, do just 1 calculation, as few times as you can.
  # Let's start overall.
  stat = flights %>%
    # Filter to just flights that started at these airports
    filter(origin %in% c("JFK", "LGA", "EWR")) %>%
    # For each carrier...
    group_by(month, carrier) %>%
    # Let's get the mean, stdev, and confidence intervals
    summarize(
      mean = mean(arr_delay, na.rm = TRUE),
      sd = sd(arr_delay, na.rm = TRUE),
      n = n(),
      se = sd / sqrt(n),
      lower = mean + se*qnorm(0.025),
      upper = mean + se*qnorm(0.975)
    ) %>%
    # Ungroup
    ungroup() %>%
    # Join in the carrier names
    left_join(by = "carrier", y = airlines) %>%
    # Join in month name data
    left_join(by = "month", y = months)
  # What can we do with this information?
  
  
  
  # plots ##########################################
  
  ## plot_one_month #########################################
  
  # Render a plot to the output 'plot_one_month'
  output$plot_one_month = renderPlot({
    # View the results for all carriers, for just one month.
    stat_one_month = stat %>%
      # filter by selected month
      filter(month == input$month)
    
    # Compare different carriers in one month
    gg_one_month = ggplot() +
      geom_col(
        data = stat_one_month,
        mapping = aes(x = name, y = mean)) +
      coord_flip() +
      labs(x = "Airline", y = "Mean Arrival Delay (minutes)")
    # Return the visualization
    gg_one_month
    
    # Trigger this plot to rerender when input$month changes
  }) %>% bindEvent({ input$month })
  
  
  ## plot_one_carrier #########################################
  
  # Render a plot to the output 'plot_one_carrier'
  output$plot_one_carrier = renderPlot({
    # Let's view the results for just that one carrier, over time
    stat_one_carrier = stat %>%
      # Filter by selected carrier
      filter(carrier == input$carrier)
    
    # Visualize just one carrier over time.
    gg_one_carrier = ggplot() +
      geom_ribbon(
        data = stat_one_carrier,
        mapping = aes(x = reorder(month_name, month), ymin = lower, ymax = upper, group = carrier, fill = name),
        alpha = 0.5 # transparency is helpful
      ) +
      geom_line(
        data = stat_one_carrier,
        mapping = aes(x = reorder(month_name, month), y = mean, group = carrier, color = carrier)
      ) +
      labs(x = "Month", y = "Mean Arrival Delay (minutes)\n[with 95% Confidence Intervals]",
           fill = "Airline", title = "How Late is Your Airline?") +
      # you can ditch the legend for color or fill like this
      guides(color = "none")
    
    # Return the plot
    gg_one_carrier
    # Trigger this plot to re-render when input$carrier changes
  }) %>% bindEvent({ input$carrier  })
  
  
  # text_highlight #############################################
  
  ## Render to text output 'text_highlight'
  output$text_highlight = renderText({
    # Let's get some highlight stats for your carrier at one specific time
    stat_highlight = stat %>%
      filter(carrier == input$carrier, month == input$month) %>%
      # Format a number for highlighting
      mutate(highlight = scales::number(mean, accuracy = 0.1) ) %>%
      # Summarize a label
      mutate(label = paste0(
        "In ", month_name, ", ",
        name, " flights to NYC had an average arrival delay of ", 
        highlight, " minutes."
      ))
    
    # Output a single text blob value. Must have just length 1.
    stat_highlight$label
    # When EITHER carrier or month changes, update this text.
  }) %>% bindEvent({ input$carrier; input$month })
  
  
  
  
}

shinyApp(ui = ui, server = server, onStart = global)




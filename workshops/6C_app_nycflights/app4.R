#' @name app4.R
#' @author Tim Fraser
#' @title nycflights app V4
#' @description 
#' 
#' What if we incorporated more complex reactivity?
#' For example, if we'd like to ever reuse a calculation in a separate rendering,
#' we need to create a 'reactive' object using `shiny::reactive()`.
#' 
#' 
#' Let's make our data `stat` contingent on airport of `origin`, 
#' 
#' - EDIT 1: First, to do this, we'll need to add a `shiny::selectInput()` for `origin`
#' 
#' - EDIT 2: Then, we need to add in `origin` into the grouping and joining for `stat`, 
#'           so we can filter by origin == input$origin later.
#' 
#' - EDIT 3: We'll turn the `stat` data.frame into a `reactive()` object `stat()`, contingent on `input$origin`
#' 
#' - EDIT 4: Everywhere we wrote `stat` before, we'll now write `stat()`.
#' 
#' - EDIT 5: In any function where `stat()` gets called, add a `bindEvent` condition for `stat()`
#' 
#' - EDIT 6: To make our lives easier later, we're also going to turn `stat_highlight` into a reactive `stat_highlight()`.



# Best to run your packages at startup
library(dplyr) # data wrangling
library(readr) # reading data
library(ggplot2) # data vizualization

library(shiny) # main shiny app package
library(bslib) # easier html construction

# We don't have anything in particular to put in our global function at the minute
global = function(){    }



ui = function(){   
  
  # Get any helper data you need to starts
  airlines = read_csv("airlines.csv")
  months = read_csv("months.csv")
  origins = read_csv("origins.csv")
  
  # Make a named vector, so you can select by Names but get back specific ids, eg. "Dec" = 12
  choices_month = setNames(object = months$month, nm = months$month_name) 
  choices_airlines = setNames(object = airlines$carrier, nm = airlines$name)
  choices_origins = setNames(object = origins$origin, nm = origins$origin_abb)  
  
  # TITLE CARD ###################################
  c1 = card(
    # Make a card header whose background is the primary color (class = bg-primary)
    card_header(class = "bg-primary",
                # Add this title                
                card_title("NYC Flights Database (2013)"))
  )
  
  # SELECTOR CARD #################################
  c2 = bslib::card(
    # Make a simple card header and title 
    card_header(     card_title("FILTER FLIGHTS")  ),
    # Make a card body section
    card_body(
      # **EDIT 1** ########################################
      # add selectInput() for "origin" to get a input$origin
      selectInput(inputId = "origin", label = "ORIGIN", choices = choices_origins, selected = "JFK"),
      selectInput(inputId = "month", label = "MONTH", choices = choices_month, selected = 1),
      selectInput(inputId = "carrier", label = "AIRLINE", choices = choices_airlines, selected = "AA")
    )
  )
  
  # PLOT CARD ##########################
  c3 = bslib::layout_column_wrap(
    card(plotOutput(outputId = "plot_one_month")),
    card(plotOutput(outputId = "plot_one_carrier")),
    width = 0.5
  )
  
  # TEXT CARD ##############################
  c4 = bslib::card(
    bslib::card_header("Spotlight", class = "bg-dark"),
    bslib::card_footer(textOutput("text_highlight"))
  )
  
  # Or add a sidebar-main split layout like this...  
  bslib::page(
    title = "NYC Flights", 
    # add a bootstrap theme to the page
    theme = bslib::bs_theme(preset = "cerulean"),
    # Stack cards
    c1, 
    # Put next cards in a sidebar-main panel split layout
    bslib::layout_sidebar(
      # Sidebar...
      sidebar = bslib::sidebar(c2), 
      # main panel
      c3,
      c4)
    
  )
  
  
}


server = function(input, output, session){  
  
  # data ########################################
  # Read in data
  airlines = read_csv("airlines.csv")
  months = read_csv("months.csv")
  origins = read_csv("origins.csv")
  flights = read_csv("flights_sample.csv") %>% 
    # Let's take a random sample
    sample_n(size = 20000) %>%
    # And get the main variables we care about here
    select(month, day, carrier, origin, 
           arr_delay, arr_time, sched_arr_time)
  
  # stat() #######################################
  # Wherever possible, do just 1 calculation, as few times as you can.
  
  ## **EDIT 3** ###################################################
  # Turn stat into reactive object stat()
  # Generate stat() reactively.
  stat = reactive({
    # Let's start overall.
    flights %>%
      ## **EDIT 2** #################################################
      ## add input$origin as a filter and origin as a grouping variable
      # Filter to just flights that started at these airports
      filter(origin %in% input$origin) %>%
      # For each carrier, month, and origin...
      group_by(origin, month, carrier) %>%
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
      left_join(by = "month", y = months) %>%
      # Join in origins name data
      left_join(by = "origin", y = origins)
    
    # Trigger whenever input$origin changes
  }) %>% bindEvent({ input$origin })
  
  
  
  ## plot_one_month #########################################
  
  # Render a plot to the output 'plot_one_month'
  output$plot_one_month = renderPlot({
    ## **EDIT 4a** ###########################
    # rename stat --> stat()
    # View the results for all carriers, for just one month.
    stat_one_month = stat() %>%
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
    
    ## **EDIT 5a** ###########################
    # add stat() to bindEvent(), so it will re-render whenever stat() updates
    # Trigger this plot to rerender when input$month changes
  }) %>% bindEvent({ stat(); input$month })
  
  
  ## plot_one_carrier #########################################
  
  # Render a plot to the output 'plot_one_carrier'
  output$plot_one_carrier = renderPlot({
    
    ## **EDIT 4c** ###########################
    # rename stat --> stat()
    # Let's view the results for just that one carrier, over time
    stat_one_carrier = stat() %>%
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
    
    ## **EDIT 5b** ###########################
    # add stat() to bindEvent(), so it will re-render whenever stat() updates
    # Trigger this plot to re-render when input$carrier changes
  }) %>% bindEvent({ stat(); input$carrier  })
  
  
  # stat_highlight() ##########################################
  
  ## **EDIT 6a** ######################################
  # Create reactive data.frame as 'stat_highlight()'
  stat_highlight = reactive({
    
    ## **EDIT 4c** ###########################
    # rename stat --> stat()
    # Let's get some highlight stats for your carrier at one specific time
    stat() %>%
      filter(carrier == input$carrier, month == input$month) %>%
      # Format a number for highlighting
      mutate(highlight = scales::number(mean, accuracy = 0.1) ) %>%
      # Summarize a label
      mutate(label = paste0(
        "In ", month_name, ", ",
        name, " flights to ", origin_name, " in NYC had an average arrival delay of ", 
        highlight, " minutes."
      ))
    
    ## **EDIT 5c** ###########################
    # add stat() to bindEvent(), so it will re-render whenever stat() updates
    # When EITHER stat() or carrier or month changes, update this text.
  }) %>% bindEvent({ stat(); input$carrier; input$month })
  
  ## text_highlight #############################################
  
  ## Render to text output 'text_highlight'
  output$text_highlight = renderText({
    
    ## **EDIT 6b** ######################################
    # use reactive data.frame stat_highlight() to get label
    
    # Output a single text blob value. Must have just length 1.
    stat_highlight()$label
    
    ## **EDIT 6c** ######################################
    # add stat_highlight() to bindEvent(), so it will re-render whenever stat_highlight() updates
    # Trigger whenever stat_highlight() changes
  }) %>% bindEvent({ stat_highlight() })
  
}


# Run app
shiny::shinyApp(ui = ui, server = server, onStart = global)

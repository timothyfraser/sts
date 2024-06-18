#' @name app5.R
#' @author Tim Fraser
#' @title nycflights app V5
#' @description 
#' 
#' What if we want to add interactivity to a visual itself?
#' The `plotly` package lets you convert directly from ggplot into `plotly` interactive visuals.
#' We'll just need to update a few things:
#' 
#' - add `library(plotly)` to `global()`
#' - Wherever we make a ggplot, make it into plotly with `plotly::ggplotly()`
#' - use `tooltip = c("var1", "var2")` to select which variable names from the visual to show in tooltip
#' - update all `renderPlot({ })` chunks to be `renderPlotly({})`
#' - update all `plotOutput({ })` chunks to be `plotlyOutput({ })`

global = function(){   
  
  library(dplyr) # data wrangling
  library(readr) # reading data
  library(ggplot2) # data vizualization
  
  library(shiny) # main shiny app package
  library(bslib) # easier html construction
  library(plotly) # interactive visuals
}


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
      selectInput(inputId = "origin", label = "ORIGIN", choices = choices_origins, selected = "JFK"),
      selectInput(inputId = "month", label = "MONTH", choices = choices_month, selected = 1),
      selectInput(inputId = "carrier", label = "AIRLINE", choices = choices_airlines, selected = "AA")
    )
  )
  
  # PLOT CARD ##########################
  c3 = bslib::layout_column_wrap(
    card(plotlyOutput(outputId = "plot_one_month")),
    card(plotlyOutput(outputId = "plot_one_carrier")),
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
  
  # stats #######################################
  # Wherever possible, do just 1 calculation, as few times as you can.
  
  # Generate stat() reactively.
  stat = reactive({
    # Let's start overall.
    flights %>%
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
  
  # plots ##########################################
  
  ## plot_one_month #########################################
  
  # Render a plot to the output 'plot_one_month'
  output$plot_one_month = renderPlotly({
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

    # Make it plotly
    pp_one_month = plotly::ggplotly(gg_one_month, tooltip = c("name", "mean"))
    # return the visualization
    pp_one_month
    # Trigger this plot to rerender when input$month changes
  }) %>% bindEvent({ stat(); input$month })
  
  
  ## plot_one_carrier #########################################
  
  # Render a plot to the output 'plot_one_carrier'
  output$plot_one_carrier = renderPlotly({
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
    
    # Make it plotly
    pp_one_carrier = plotly::ggplotly(gg_one_carrier, tooltip = c("mean"))
    # return the visualization
    pp_one_carrier
    

    # Trigger this plot to re-render when input$carrier changes
  }) %>% bindEvent({ stat(); input$carrier  })
  
  
  
  # stat_highlight() ##########################################
  
  # Create reactive data.frame as 'stat_highlight()'
  stat_highlight = reactive({
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
    # When EITHER stat() or carrier or month changes, update this text.
  }) %>% bindEvent({ stat(); input$carrier; input$month })
  
  ## text_highlight #############################################
  
  ## Render to text output 'text_highlight'
  output$text_highlight = renderText({
    # Output a single text blob value. Must have just length 1.
    stat_highlight()$label
    # Trigger whenever stat_highlight() changes
  }) %>% bindEvent({ stat_highlight() })
  
}


# Run app
shiny::shinyApp(ui = ui, server = server, onStart = global)

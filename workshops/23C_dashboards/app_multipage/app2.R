#' @name app2.R
#' 
#' We could add updateSelectInput() in an observe({ }) chunk,
#' using a bindEvent() trigger.
#' 
#' This means that whenever input$continent changes,
#' We add new choices and default selected options
#' to our input$country selector.
#' 
#' See lines 55 onward.

library(dplyr)
library(readr)
library(ggplot2)
library(shiny)
library(bslib)
library(gapminder)


global = function(){  }

ui = function(){
  
  page(
    
    selectInput(inputId = "continent", label = "CHOOSE CONTINENT", 
                choices = c("Asia", "Europe")),
    selectInput(inputId = "country", label = "CHOOSE COUNTRY", 
                choices = c("Afghanistan", "Germany")),
    
    plotOutput(outputId = "chart")
  )
  
}


server = function(input, output, session){
  
  data = gapminder::gapminder  
  
  # A common error we run into is over-filtering....
  # Can't look at countries that are not in the selected continent.
  output$chart = renderPlot({
    
    subset = data %>%
      filter(continent == input$continent) %>%
      filter(country == input$country)
    
    subset$lifeExp %>% hist()
  })  
  
  
  # When input$continent changes, update the choices and selected choice in
  # the selectInput() function for input$country.
  observe({
    # Testing values
    # input = list(country = "Afghanistan", continent = "Asia")
    
    mychoices = tibble(continent = c("Asia", "Europe"),
                       country = c("Afghanistan", "Germany"))
    
    valid_choices = mychoices %>%
      filter(continent %in% input$continent)
    
    updateSelectInput(inputId = "country", 
                      choices = valid_choices$country, 
                      selected = valid_choices$country[1])
    
  }) %>% bindEvent({ input$continent })
  
}

shinyApp(ui, server, onStart = global)
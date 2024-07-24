#' @name app3.R
#' 
#' Let's demo a multipage app,
#' building off of our original app.R
#' 
#' We could make 2 pages as separate objects,
#' then stack them atop each other as elements in a single page.
#' 
#' eg.
#' page( page1, page2 )

library(dplyr)
library(readr)
library(ggplot2)
library(shiny)
library(bslib)
library(gapminder)


global = function(){  }

# See edits to user interface!
ui = function(){
  
  page1 = page(
    card_header("First Page"),
    selectInput(inputId = "continent", label = "CHOOSE CONTINENT", 
                choices = c("Asia", "Europe")),
    selectInput(inputId = "country", label = "CHOOSE COUNTRY", 
                choices = c("Afghanistan", "Germany")),
    
    plotOutput(outputId = "chart")
  )
  
  # Anything can go into this second page.
  # Your inputs and outputs must be uniquely named,
  page2 = page(
    card_header("Second Page"),
    # Make a new selector with a unique ID
    selectInput(inputId = "continent2", label = "CHOOSE CONTINENT", 
                choices = c("Asia", "Europe")),
    plotOutput(outputId = "chart2")
  )
  
  
  # Bundle my two pages.
  
  page(
    page1,
    page2
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
  
  
  
}

shinyApp(ui, server, onStart = global)
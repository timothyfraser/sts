#' @name app5.R
#' 
#' Let's demo a multipage app.
#' 
#' Building off app4.R, we could go on to add additional content.
#' We could put our navset_card_pill() chunk INSIDE another page,
#' allowing us to toggle between page1 and page2,
#' but also have a shared header, footer, or any extra cards we want to add.

library(dplyr)
library(readr)
library(ggplot2)
library(shiny)
library(bslib)
library(gapminder)


global = function(){  }

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
  
  myui = bslib::navset_card_pill(
    selected = "first",
    nav_panel(title = "PAGE 1", value = "first", page1),
    nav_panel(title = "PAGE 2", value = "second", page2)
  )
  
  
  output = page(
    card(card_header(card_title("My Rad App"))),
    # My big multi-page chunk
    myui
  )
  
  return(output)
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
  
  
  output$chart2 = renderPlot({
    
    data %>%
      filter(continent == input$continent2) %>%
      ggplot(mapping = aes(x = lifeExp, y = pop )) +
      geom_point()
  })
  
}

shinyApp(ui, server, onStart = global)
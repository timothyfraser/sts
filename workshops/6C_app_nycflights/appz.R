#' @name appz.R
# This app is a simple example app for getting the hang of using Shiny.
# It matches the same formatting as the Shiny Cheatsheet.
# The key difference is that THIS app uses **plotly to make an interactive visual**

#install.packages("shiny")
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Build a user interface page
ui = fluidPage(
  # a numeric input box, with unique id 'n'
  numericInput(inputId = "n", label = "Sample Size", value = 25),
  # followed by
  # a plot output box, with unique id 'hist'
  plotlyOutput(outputId = "hist")
)

# Build a server function
server = function(input, output, session){
  # render a plot to be the output 'hist'
  output$hist = renderPlotly({
    
    data = tibble(x = rnorm(input$n)) %>%
      mutate(label = paste0("Value: ", round(x, 2)))
    
    gg = ggplot() +
      geom_histogram(data = data, mapping = aes(x = x, text = label))
    
    plotly::ggplotly(gg, tooltip = c("text"))
    
  }) %>% bindEvent({ input$n })
}

# Launch the app
shinyApp(ui = ui, server = server)
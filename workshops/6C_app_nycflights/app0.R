#' @name app0.R
# This app is a simple example app for getting the hang of using Shiny.
# It matches the same formatting as the Shiny Cheatsheet.

library(shiny)
library(dplyr)

ui = fluidPage(
  numericInput(inputId = "n", label = "Sample Size", value = 25),
  plotOutput(outputId = "hist")
)

server = function(input, output, session){
  output$hist = renderPlot({
    hist( rnorm(n = input$n) )
  }) %>% bindEvent({ input$n })
}

shinyApp(ui = ui, server = server)
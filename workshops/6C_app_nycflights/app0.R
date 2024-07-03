#' @name app0.R
# This app is a simple example app for getting the hang of using Shiny.
# It matches the same formatting as the Shiny Cheatsheet.

#install.packages("shiny")
library(shiny)
library(dplyr)

# Build a user interface page
ui = fluidPage(
  # a numeric input box, with unique id 'n'
  numericInput(inputId = "n", label = "Sample Size", value = 25),
  # followed by
  # a plot output box, with unique id 'hist'
  plotOutput(outputId = "hist")
)

# Build a server function
server = function(input, output, session){
  # render a plot to be the output 'hist'
  output$hist = renderPlot({
    # using the input 'n' in a process to make a histogram
    # (rnorm(n) is a random sampler - it samples n observations)
    hist( rnorm(n = input$n) )
    # Bind this render process to the input 'n'
    # so when the event (input 'n' changes) occurs,
    # re-render the plot.
  }) %>% bindEvent({ input$n })
}

# Launch the app
shinyApp(ui = ui, server = server)
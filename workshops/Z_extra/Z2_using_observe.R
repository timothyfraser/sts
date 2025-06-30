# Z2_using_observe.R

# What is observe(), and why is it different from reactive()?
# reactive() and observe() are very similar, and can be used to achieve the same objective, but do so in slightly different ways.  The short answer is:
#  - stat = reactive({ ... }) returns an output value that gets stored in the reactive object stat (for example).
#  - observe({ ... }) has no output. But it can reactively do things to various reactive objects.

# Let's try an example. 

# Suppose we have an app that's trying to show the difference between mean miles per gallon
# for a set of cars depending on how many cylinders that car's engine runs on. 


# We make the user interface...
library(shiny)
library(dplyr)
# Let's get data on a bunch of cars data
data = mtcars

# make user interface
ui = function(){
  fluidPage(
    # input$type: how many cylinders 
    selectInput(inputId = "type", label = "Cylinders", choices = c(4,6), selected = 4),
    # output$stat: a string blurb output
    verbatimTextOutput(outputId = "stat") 
  )    
}

# To try each of the server functions below, just uncomment the desired server function, comment out any other server functions, and then run the app script.
# blank server function
# server = function(input,output,session){}


# OR make server function with reactive()
# server = function(input, output, session){
#   
#   # create a reactive function that outputs a mean
#   mystat = reactive({
#     result = data %>% filter(cyl == input$type) %>% summarize(mean = mean(mpg))
#     # return the result
#     result$mean
#   }) %>% 
#     # updates whenever input$type changes
#     bindEvent({input$type})
#   
#   # dynamically update output$stat with this, WHENEVER mystat() changes
#   output$stat = renderText({ mystat()  }) 
# }


# OR make server function that uses a reactive observer
server = function(input, output, session){
  # Create an empty reactive object to hold a reactively updating value
  val = reactiveValues(mean = NULL)
  
  # create a reactive observer that performs a process when triggered
  observe({
    
    result = data %>% filter(cyl == input$type) %>% summarize(mean = mean(mpg))

    # Assign result$mean to reactive list val$mean
    val$mean = result$mean
    
    # notice how this doesn't end with returning result$mean directly?
    # use observers when you want to update multiple outputs, or run through some process 
    
    # do this process whenever input$type changes
  }) %>% bindEvent({ input$type })
  
  # dynamically update output$stat using the contents of the reactive list item val$mean.
  # Just like input$type, val$mean is reactive by default.
  output$stat = renderText({ val$mean  })
  
}

# Test run the app...
shinyApp(ui, server)

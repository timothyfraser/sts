# Load required libraries
# dplyr  -> data manipulation
# shiny  -> web app framework
# bslib  -> modern Bootstrap-based UI components (e.g., page(), card())
library(dplyr)
library(shiny)
library(bslib)

# ------------------------------------------------------------------
# LOAD STATIC DATA
# ------------------------------------------------------------------
# For this simple example we use the built-in mtcars dataset.
# In a real app, this could be a database query or external file.
data = mtcars

print("STAGE: LOAD data")
print(head(data, 1))   # Quick check that data loaded correctly


# ------------------------------------------------------------------
# DEFINE UI
# ------------------------------------------------------------------
print("# LOAD ui()")

ui = function(){
  
  # page() comes from bslib and provides a clean layout wrapper.
  # We place inputs and outputs inside cards for visual structure.
  page(
    
    # Card 1: User input
    card(
      # Dropdown for selecting cylinder count.
      # input$cyl will be available in server().
      selectInput(
        inputId = "cyl",
        label = "Cylinders",
        choices = c(4,6,8)
      )
    ),
    
    # Card 2: Output display
    card(
      # This will display the computed mean MPG.
      textOutput(outputId = "meanmpg")
    )
  )
}


# ------------------------------------------------------------------
# DEFINE SERVER
# ------------------------------------------------------------------
print("# LOAD server()")

server = function(input, output, session){
  
  # ----------------------------------------------------------------
  # Reactive expression: stat()
  # ----------------------------------------------------------------
  # This re-runs automatically whenever input$cyl changes.
  # It filters the dataset to only rows matching the selected
  # number of cylinders.
  stat = reactive({
    
    # Filter dataset based on selected cylinder value
    df = data %>% 
      filter(cyl == input$cyl)
    
    # Debug print so we can observe reactivity in the console
    print(paste0(
      "--- df: ",
      nrow(df),
      " rows | class: ",
      class(df)
    ))
    
    # Return filtered dataframe
    df
  })

  
  # ----------------------------------------------------------------
  # Output rendering
  # ----------------------------------------------------------------
  # renderText() re-executes whenever stat() changes.
  # Because stat() depends on input$cyl, this output updates
  # automatically when the dropdown changes.
  output$meanmpg = renderText({ 
    
    # Get filtered dataframe from reactive
    df = stat()
    
    # Compute mean MPG
    value = df$mpg %>% mean()
    
    # Debug print
    print(paste0("--- mean mpg: ", value))
    
    # The last expression is returned and displayed
    value
  })
  
}


# ------------------------------------------------------------------
# RUN APPLICATION
# ------------------------------------------------------------------
# Connects UI and server into a working Shiny app.
shinyApp(ui = ui, server = server)

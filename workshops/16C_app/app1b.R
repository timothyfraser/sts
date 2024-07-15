#' @name app1b.R
#' @description
#' app1a.R, but remade using our bslib functions. 
#' I personally recommend bslib - it just seems a lot clearer to me. 
#' 
#' Applies `workflow1.R`
#' 
#' For a `shinyuieditor` equivalent, see `app1a.R`

# Packages ############################
library(shiny)
library(bslib)
library(ggplot2)

# ui ###################################
## page #################################
ui = bslib::page(
  title = "Social Infrastructure in Boston", # what shows up on browser tab
  
  # Simple title
  bslib::card(
    bslib::card_header(
      bslib::card_title("Boston Social Infrastructure by Neighborhood")
    )
  ),
  
  bslib::layout_sidebar(
    # Make a sidebar
    sidebar = bslib::sidebar(
      "Select the neighborhood you want to view social infrastructure sites for.",
      ## input$type ###############################
      shiny::checkboxGroupInput(
        inputId = "type", label = "TYPE OF SITE", 
        choices = c("Community Spaces", "Places of Worship",  "Social Businesses", "Parks"),
        selected = c("Community Spaces")
      ),
      ## input$area #########################
      shiny::checkboxGroupInput(
        inputId = "area", label = "NEIGHBORHOOD",
        choices = c("Roxbury", "Mission Hill", "Dorchester", "Jamaica Plain", "Downtown"),
        selected = c("Downtown")
      )
      
    ),
    # Make the main panel a card
    bslib::card(
      bslib::card_header("Plot"),
      ## output$plot ##########################
      bslib::card_body(
        shiny::plotOutput(outputId = "plot", width = "100%", height = "100%")
      )
    )
  )
)




server <- function(input, output) {
  
  # DATA #######################################
  # Get Boston neighborhoods
  neighborhoods = read_sf("boston_neighborhoods.geojson") %>%
    select(name = blockgr2020_ctr_neighb_name, geometry) %>%
    filter(!name %in% c("West Roxbury", "Roslindale", "Mattapan", "Hyde Park",
                        "Brighton", "Allston", "Charlestown", "East Boston", "Harbor Islands"))
  
  # Get social infrastructure sites
  points = read_sf("boston_social_infra.geojson")
  
  # Coordinates
  xlim = c(-71.14, -71.01)
  ylim = c(42.37, 42.28)
  
  # REACTIVITY #################################
  output$plot = renderPlot({
    # Filter to a certain 1+ neighborhoods
    polygons_poi = neighborhoods %>% filter(name %in% input$area)
    # Filter to just these points    
    points_poi = points %>% filter(group %in% input$type)
    
    # Plot the full map, highlighting the neighborhood
    gg = ggplot() +
      geom_sf(data = neighborhoods) +
      geom_sf(data = points_poi, alpha = 0.5) +
      geom_sf(data = polygons_poi, fill = NA, linewidth = 1.5, color = 'dodgerblue') +
      geom_sf_label(data = neighborhoods, mapping = aes(label = name)) +
      theme_bw(base_size = 14) +
      coord_sf(xlim = xlim, ylim = ylim)
    gg
  })  
  
}

shinyApp(ui, server)

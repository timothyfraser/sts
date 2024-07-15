#' @name app1a.R
#' @description
#' An app whose user interface is adapted from app.R,
#' which was made with `shinyuieditor`
#' 
#' For a `bslib` equivalent, see `app1b.R`

# Packages ############
library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(ggplot2)
library(viridis)

# User Interface ###############
ui <- grid_page(
  layout = c(
    "header  header  ",
    "sidebar plot "
  ),
  row_sizes = c(
    "100px",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "1rem",
  
  grid_card(
    area = "sidebar",
    card_header("Settings"),
    card_body(
      
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
      ),
      em("Select the neighborhood you want to view social infrastructure sites for.")
        
    )
  ),
  
  grid_card_text(
    area = "header",
    content = "Boston Social Infrastructure by Neighborhood",
    alignment = "start",
    is_title = FALSE
  ),
  
  grid_card(
    area = "plot",
    card_header("Plot"),
    card_body(
      plotOutput(
        outputId = "plot",
        width = "100%",
        height = "100%"
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
    # Filter to a certain neighborhood
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



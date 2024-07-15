#' @name app2.R
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
      bslib::card_header("VISUALS"),
      # Split half layout
      bslib::layout_column_wrap(width = 0.5,
                                ## output$map ##########################
                                bslib::card_body(
                                  shiny::plotOutput(outputId = "map", width = "100%", height = "100%")
                                ),
                                ## output$bars ###########################
                                bslib::card_body(
                                  shiny::plotOutput(outputId = "bars", width = "100%", height = "100%")
                                )
                                
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
  
  ### **EDIT 1** ######################################
  # Add new data
  # Let's get a default list of categories
  categories = tibble(
    group = c("Community Spaces", "Parks", "Places of Worship", "Social Business")
  ) # notice we skipped the "Other" category - we don't want to visualize it.
  
  # Streets!
  streets = read_sf("streets.geojson")
  
  
  # REACTIVITY #################################
  
  ### **EDIT 2** #################################
  # Add poi() and tally() as reactive data
  
  ## poi() ##########################
  # Narrow into just the points in those areas
  poi = reactive({
    points %>%
      mutate(group = factor(group)) %>%
      # Just social infrastructure sites in our selected types
      filter(group %in% input$type) %>%
      # Spatially filter by just these neighborhoods
      st_join(
        y = neighborhoods %>% 
          filter(name %in% input$area) %>%
          select(name, geometry), 
        left = FALSE)
  })
  
  ## tally() #############################
  # Get total tally of sites per type
  tally = reactive({ 
    poi() %>% 
      as_tibble() %>% 
      group_by(group) %>%
      summarize(count = n()) %>%
      # But we want to compare against ALL categories, right?
      # So let's join this into the set of all categories
      right_join(by = "group", y = categories) %>%
      # And fill in NAs with 0, since they are not present
      mutate(count = case_when(is.na(count) ~ 0, TRUE ~ count))
  })
  
  
  
  # OUTPUTS #######################################
  
  ### **EDIT 3** ##############################
  # add output$bars and output$map
  
  ## bars ###################################
  output$bars = renderPlot({
    gg_bars = ggplot() +
      geom_col(
        data = tally(), 
        mapping = aes(x = group, y = count, fill = group)) +
      # Add text labels with the 'label' aesthetic
      geom_text(
        data = tally(), 
        mapping = aes(x = group, y = count, label = count),
        # Static traits
        vjust = 0, # Vertically justify to the bottom (0)
        nudge_y = 1 # bump up by 1 point on y axis
      ) +
      theme(legend.position = "none") 
    
    gg_bars # View it
  })
  
  ## map ####################################
  output$map = renderPlot({
    
    
    # Quickly filter the polygons by name
    polygons_poi = neighborhoods %>% 
      filter(name %in% input$area)
    
    # Find the bounding box (bb) of your new polygons
    bb = polygons_poi %>%
      st_bbox() 
    
    # You can query it like... 
    bb$xmin
    
    # Turn it into a literal polygon box as an sf feature
    box = bb %>% 
      st_as_sfc() %>% # turn into a geometry
      tibble(geometry = .) %>% # put in a tibble
      st_as_sf() # turn into a spatial data.frame
    
    # Get just streets in the box
    streets_box = streets %>%
      st_crop(y = box) # Crop these spatial features to just those in the box
    
    # Show specific neighborhoods 
    gg_map = ggplot() +
      geom_sf(data = streets_box, color = "grey") +
      geom_sf(data = polygons_poi, fill = NA, color = "dodgerblue", linewidth = 2) +
      geom_sf(data = poi(), mapping = aes(fill = group),
              shape = 21, color = "white", size = 5) +
      theme_void() + # clean map
      theme(legend.position = "bottom") +
      # Crop to filtered polygons
      # coord_sf(xlim = c(bb$xmin, bb$xmax),
      #          ylim = c(bb$ymin, bb$ymax)) +
      # Add a map scale and arrow
      ggspatial::annotation_scale(location = "bl") + # bottom left (bl)
      ggspatial::annotation_north_arrow(location = "br") # bottom right (br)
    
    # View it!
    gg_map
    
  })  
  
}

shinyApp(ui, server)



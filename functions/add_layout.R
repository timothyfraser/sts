
# A function 
add_layout = function(graph, by, layout = "fr", ...){
  # Generate a layout for the graph object
  layout = ggraph(graph, layout = layout, ...) %>%
    with(data) %>%
    select(any_of(c(by, "x","y")))
  
  # Join it in.
  graph = graph %>%
    activate("nodes") %>%
    # Drop any existing coordinates
    select(-any_of(c("x", "y"))) %>%
    left_join(by = by, y = layout) %>%
    activate("edges") %>%
    mutate(from_x = .N()$x[ .E()$from ],
           from_y = .N()$y[ .E()$from],
           to_x = .N()$x[ .E()$to],
           to_y = .N()$y[ .E()$to] ) %>%
    activate("nodes")
  return(graph)
}

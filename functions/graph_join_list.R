
#' @name graph_join_list
#' @author Tim Fraser
#' @title Join a List of Tidygraph Objects together, using a shared node ID.
#' @description
#' Binds a bunch of tidygraph graphs together, but does not duplicate nodes.
#' To do so, must do repeated `graph_join()` functions using the shared node ID from "by"
#' @importFrom purrr map2
#' @importFrom tidygraph graph_join activate mutate
#' @importFrom dplyr `%>%`
graph_join_list = function(graph_list, by = "name", .id = NULL){
  # Get number of graphs
  n = length(graph_list)
  # If the graph list is empty, return an empty list
  if(n == 0){ return(list()) }
  
  if(!is.null(.id)){
    # Label the subgraphs
    # Get the names of the graph list
    names_list = names(graph_list)
    
    # If there are any names in the list...
    if(!is.null(names_list)){
      # Extract name of variable 
      # vars_list = gsub(x = names_list, pattern = "[:][ ].*", replacement = "")
      # Extract values from names
      values_list = gsub(x = names_list, pattern = ".*[:][ ]", replacement = "")
      # If no id variable name is provided, then just use '.id'
      
      # Label the edges by subgraph 
      graph_list = purrr::map2(
        .x = graph_list, .y = values_list,
        .f = ~.x %>% activate("edges") %>% mutate(!!sym(.id) := .y) )
    }
  }
  # For each item in a list...
  graph = graph_list[[1]] %>%
    activate("nodes") %>%
    select(any_of(by))
  
  if(n > 1){
    for(i in 2:n){
      other_graph = graph_list[[i]] %>%
        activate("nodes") %>%
        select(any_of(by))
      graph = graph_join(x = graph, y = other_graph, by = by)
    }
  }
  return(graph)  
}



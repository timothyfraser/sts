
#' @name bind_graphs_list
#' @author Tim Fraser
#' @title Bind a List of Tidygraph Objects together
#' @description
#' Meant to mirror the logic of `dplyr::bind_rows()`, which can bind a list of data.frames together,
#' this function binds a list of `tidygraph` objects together into one graph,
#' with a `.id` field attached to each node and edge to distinguish which subgraph it came from.
#' @importFrom purrr map2
#' @importFrom tidygraph bind_graphs activate mutate
#' @importFrom dplyr `%>%`
bind_graphs_list = function(graph_list, .id = NULL){
  # Get number of graphs
  n = length(graph_list)
  # If the graph list is empty, return an empty list
  if(n == 0){ return(list()) }
  
  # If an id variable name is provided, then we'll add names
  if(!is.null(.id)){
    # Label the subgraphs
    # Get the names of the graph list
    names_list = names(graph_list)
    
    # If there are any names in the list...
    if(!is.null(names_list)){
      # Extract values from names
      values_list = gsub(x = names_list, pattern = ".*[:][ ]", replacement = "")
      # Label the edges by subgraph 
      graph_list = purrr::map2(
        .x = graph_list, .y = values_list,
        .f = ~.x %>% activate("edges") %>% mutate(!!sym(.id) := .y) )
      # Label the nodes by subgraph
      graph_list = purrr::map2(
        .x = graph_list, .y = values_list,
        .f = ~.x %>% activate("nodes") %>% mutate(!!sym(.id) := .y) )
    }
  }
  
  # For each item in a list...
  graph = graph_list[[1]]
  # Bundle the graphs together iteratively
  if(n > 1){
    for(i in 2:n){
      graph = bind_graphs(graph, graph_list[[i]])
    }
  }
  return(graph)
}


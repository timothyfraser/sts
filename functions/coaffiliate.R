

#' @name coaffiliate
#' @author Tim Fraser
#' @title Coaffiliate a Bipartite Graph into a Unipartite Graph
#' @param graph a tidygraph object
#' @param type (logical) TRUE or FALSE. If TRUE, refers to mode 1.  If FALSE, refers to mode 2. Corresponds to the bipartite `type` variable in your tidygraph's nodes.
#' @param weight (character) name of edge weight variable for use in coaffiliation calculation (matrix multiplication)
#' @param diag (logical) TRUE or FALSE. Do we keep the diagonal, or do we make it all zeros?
#' @importFrom igraph graph_from_adjacency_matrix as_incidence_matrix
#' @importFrom tidygraph as_tbl_graph
#' @importFrom dplyr `%>%`
coaffiliate = function(graph, type = TRUE, names = TRUE, weight = "weight", diag = FALSE){
  graph %>%
    # Make an incidence matrix
    as_incidence_matrix(names = TRUE, sparse = FALSE) %>%
    # Matrix multiplication
    { if(type == TRUE){  . %*% t(.)
    }else if(type == FALSE){ t(.) %*% . }  } %>%
    # Cut the diagonal, if diag = FALSE
    { if(diag == FALSE){ diag(.) <- 0 }; . } %>%
    # Turn into an igraph from adjacency matrix
    igraph::graph_from_adjacency_matrix(mode = "undirected", weighted = weight) %>%
    # Turn into a tbl_graph from igraph
    as_tbl_graph()
  
  
  
}


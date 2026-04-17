#' 24P_analytics/functions.R
#' A set of functions to help you test-plot tidygraphs quickly.
#' 
#' 
#' Visualize a Bipartite Graph
#'
#' @description
#' Plots a bipartite (two-mode) graph using \code{ggplot2}, with nodes colored
#' by their type (e.g., committee vs. member) and edges drawn as line segments.
#' Node positions are computed by \code{ggraph} using the specified layout
#' algorithm. This is useful for exploratory visualization of affiliation
#' networks before conducting deeper network analysis.
#'
#' @param graph A \code{tbl_graph} (from \code{tidygraph}) representing a
#'   bipartite network. The node table must contain a logical \code{type}
#'   column (FALSE = one mode, TRUE = the other), as produced by
#'   \code{igraph::make_bipartite_graph()} or \code{tidygraph::as_tbl_graph()}.
#'   The node table must also contain a \code{name} column.
#' @param layout A character string specifying the \code{ggraph}/\code{igraph}
#'   layout algorithm used to compute node positions. Defaults to \code{"fr"}
#'   (Fruchterman-Reingold). Other common options include \code{"kk"},
#'   \code{"stress"}, \code{"nicely"}, and \code{"bipartite"} (which respects
#'   the two-mode structure by placing each type on its own row).
#'
#' @return A \code{ggplot} object. You can extend it with additional
#'   \code{ggplot2} layers (e.g., \code{+ theme_void()},
#'   \code{+ scale_fill_manual()}, \code{+ geom_text()}) after calling the
#'   function.
#'
#' @details
#' Internally, the function uses \code{ggraph()} to compute a layout data
#' frame, then draws the graph manually with \code{geom_segment()} (edges)
#' and \code{geom_point()} (nodes). The \code{fill} aesthetic on nodes is
#' mapped to the \code{type} column, so you can control the two-mode color
#' scheme with \code{+ scale_fill_manual()} or
#' \code{+ scale_fill_brewer()} downstream.
#'
#' @seealso
#' \code{\link[ggraph]{ggraph}}, \code{\link[tidygraph]{tbl_graph}},
#' \code{\link{illustrate_unipartite}}
#'
#' @examples
#' \dontrun{
#' library(tidygraph)
#' library(ggraph)
#'
#' graph <- read_rds("data/committees/graph_bipartite.rds")
#'
#' # Default Fruchterman-Reingold layout
#' illustrate_bipartite(graph)
#'
#' # Bipartite layout — places each node type on its own horizontal row
#' illustrate_bipartite(graph, layout = "bipartite")
#'
#' # Extend the returned ggplot object with additional layers
#' illustrate_bipartite(graph, layout = "stress") +
#'   theme_void() +
#'   scale_fill_manual(
#'     values = c("FALSE" = "#4e9af1", "TRUE" = "#f97316"),
#'     labels = c("FALSE" = "Member", "TRUE" = "Committee")
#'   ) +
#'   labs(fill = "Node Type", title = "Committee Affiliation Network")
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom ggplot2 ggplot geom_segment geom_point aes
#' @importFrom tidygraph activate as_tibble
#' @importFrom ggraph ggraph
illustrate_bipartite = function(graph, layout = "fr"){
  
  require(dplyr,     quietly = TRUE, warn.conflicts = FALSE)
  require(ggplot2,   quietly = TRUE, warn.conflicts = FALSE)
  require(tidygraph, quietly = TRUE, warn.conflicts = FALSE)
  require(ggraph,    quietly = TRUE, warn.conflicts = FALSE)
  
  # Compute x-y node positions using ggraph's layout engine
  nodes_layout = ggraph(graph = graph, layout = layout) %>%
    with(data) %>%
    mutate(id = 1:n()) %>%
    select(id, x, y, name, type)
  
  # Extract edges and join in x-y coordinates for both endpoints
  edges = graph %>%
    activate("edges") %>%
    as_tibble() %>%
    left_join(
      y  = nodes_layout %>% select(id, from_x = x, from_y = y),
      by = c("from" = "id")
    ) %>%
    left_join(
      y  = nodes_layout %>% select(id, to_x = x, to_y = y),
      by = c("to" = "id")
    )
  
  # Build plot: edges as segments, nodes as filled circles colored by type
  gg = ggplot() +
    geom_segment(
      data    = edges,
      mapping = aes(x = from_x, y = from_y, xend = to_x, yend = to_y)
    ) +
    geom_point(
      data    = nodes_layout,
      mapping = aes(x = x, y = y, fill = type),
      shape = 21, color = "white"
    )
  
  return(gg)
}


#' Visualize a Unipartite (One-Mode) Graph
#'
#' @description
#' Plots a unipartite (one-mode) graph using \code{ggplot2}, drawing edges as
#' line segments and nodes as filled circles. Supports two placement strategies:
#' \code{"coords"} uses geographic latitude/longitude columns already stored in
#' the node table (ideal for spatial networks like road or evacuation graphs),
#' while any other valid \code{ggraph} layout string uses an algorithmic layout
#' computed from graph structure alone.
#'
#' @param graph A \code{tbl_graph} (from \code{tidygraph}) representing a
#'   unipartite network. The node table must contain a \code{name} column. When
#'   \code{layout = "coords"}, the node table must also contain numeric
#'   \code{lat} and \code{lon} columns (decimal degrees recommended).
#' @param layout A character string specifying how node positions are computed.
#'   \describe{
#'     \item{\code{"coords"}}{(default) Maps the node table's \code{lon} column
#'       to the x-axis and \code{lat} to the y-axis. Use this for spatially
#'       embedded networks (e.g., evacuation routes, road networks) where
#'       geographic position is meaningful.}
#'     \item{any \code{ggraph} layout string}{Computes positions algorithmically
#'       from graph structure. Common options: \code{"fr"}
#'       (Fruchterman-Reingold), \code{"kk"} (Kamada-Kawai), \code{"stress"},
#'       \code{"nicely"}.}
#'   }
#'
#' @return A \code{ggplot} object. You can extend it with additional
#'   \code{ggplot2} layers (e.g., \code{+ theme_void()},
#'   \code{+ coord_sf()}, \code{+ geom_label()}) after calling the function.
#'
#' @details
#' When \code{layout = "coords"}, edge endpoint coordinates are pulled directly
#' from the node table using \code{tidygraph}'s \code{.N()} and \code{.E()}
#' accessors inside \code{mutate()}, which allow cross-referencing node
#' attributes while the edge table is active. This avoids a separate join and
#' preserves spatial fidelity. For algorithmic layouts, coordinates are computed
#' via \code{ggraph()} and then joined onto the edge table by node index.
#'
#' @seealso
#' \code{\link[ggraph]{ggraph}}, \code{\link[tidygraph]{tbl_graph}},
#' \code{\link{illustrate_bipartite}}
#'
#' @examples
#' \dontrun{
#' library(tidygraph)
#' library(ggraph)
#' library(dplyr)
#'
#' # --- Spatial network (lat/lon coordinates in node table) ---
#' nodes <- read_rds("data/evacuation/nodes.rds")
#' edges <- read_rds("data/evacuation/edges.rds")
#'
#' graph <- tbl_graph(
#'   nodes    = nodes,
#'   edges    = edges,
#'   directed = TRUE,
#'   node_key = "name"
#' )
#'
#' illustrate_unipartite(graph, layout = "coords") +
#'   theme_void() +
#'   labs(title = "Hurricane Dorian Evacuation Network")
#'
#' # --- Committee co-affiliation network (algorithmic layout) ---
#' nodes <- read_csv("workshops/24P_analytics/nodes.csv")
#' edges <- read_csv("workshops/24P_analytics/edges.csv")
#'
#' graph <- tbl_graph(
#'   nodes    = nodes,
#'   edges    = edges,
#'   directed = TRUE,
#'   node_key = "name"
#' )
#'
#' illustrate_unipartite(graph, layout = "fr") +
#'   theme_void() +
#'   labs(title = "Co-affiliation Network (Fruchterman-Reingold)")
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom ggplot2 ggplot geom_segment geom_point aes
#' @importFrom tidygraph activate as_tibble .N .E
#' @importFrom ggraph ggraph
illustrate_unipartite = function(graph, layout = "coords"){
  
  require(dplyr,     quietly = TRUE, warn.conflicts = FALSE)
  require(ggplot2,   quietly = TRUE, warn.conflicts = FALSE)
  require(tidygraph, quietly = TRUE, warn.conflicts = FALSE)
  require(ggraph,    quietly = TRUE, warn.conflicts = FALSE)
  
  if(layout == "coords"){
    
    # Pull edge endpoint coordinates directly from the node table.
    # .N() accesses the node table while edges are active; indexing by
    # .E()$from / .E()$to maps each edge to its endpoint's lat/lon.
    edges = graph %>%
      activate("edges") %>%
      mutate(
        from_y = .N()$lat[ .E()$from ],
        from_x = .N()$lon[ .E()$from ],
        to_y   = .N()$lat[ .E()$to   ],
        to_x   = .N()$lon[ .E()$to   ]
      ) %>%
      as_tibble()
    
    # Node positions come straight from the geographic coordinates
    nodes_layout = graph %>%
      activate("nodes") %>%
      as_tibble() %>%
      mutate(id = 1:n()) %>%
      select(id, x = lon, y = lat, name)
    
  } else {
    
    # Compute x-y node positions using ggraph's layout engine
    nodes_layout = ggraph(graph = graph, layout = layout) %>%
      with(data) %>%
      mutate(id = 1:n()) %>%
      select(id, x, y, name)
    
    # Extract edges and join in x-y coordinates for both endpoints
    edges = graph %>%
      activate("edges") %>%
      as_tibble() %>%
      left_join(
        y  = nodes_layout %>% select(id, from_x = x, from_y = y),
        by = c("from" = "id")
      ) %>%
      left_join(
        y  = nodes_layout %>% select(id, to_x = x, to_y = y),
        by = c("to" = "id")
      )
  }
  
  # Build plot: edges as segments, nodes as filled circles
  gg = ggplot() +
    geom_segment(
      data    = edges,
      mapping = aes(x = from_x, y = from_y, xend = to_x, yend = to_y)
    ) +
    geom_point(
      data    = nodes_layout,
      mapping = aes(x = x, y = y),
      shape = 21, color = "black", size = 3, fill = "white"
    )
  
  return(gg)
}

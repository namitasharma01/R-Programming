#Advanced Programming in R - Lab 3
#Dijkstra algorithm

name  <- "Namita Sharma"
liuid <- "namsh440"

dijkstra2 <- function(graph, init_node){
  
  if( !is.data.frame(graph) || 
      !(which(graph[["v1"]]==init_node) || which(graph[["v2"]]==init_node)) ){
    stop("Invalid inputs")
  } 

  # Create vertex set for all unique vertices in the graph
  vertex_Q <- unique(graph[["v1"]])
  names(vertex_Q) <- as.character(1:length(vertex_Q))
  
  # Create distance set for all vertices in the graph
  dist <- numeric(length(vertex_Q))
  names(dist) <- names(vertex_Q)
  
  # Create previous node set to store shortest path to all vertices in the graph 
  prev <- numeric(length(vertex_Q))
  names(prev) <- names(vertex_Q)
  
  # Set distance as Infinity for each vertex in graph and
  # Set the previous node in the shortest route as NA for each vertex in the graph  
  for (i in 1:length(vertex_Q)) {
    dist[as.character(i)] <- Inf
    prev[as.character(i)] <- NA
  }
  
  # Set the distance for the initial node as zero 
  dist[ which(vertex_Q==init_node) ] <- 0
  
  i <- 1
  while (length(vertex_Q) != 0){
    # Select a vertex in Q with minimum distance in dist
    u <- vertex_Q[ names(dist[ which(dist == min(dist[ names(vertex_Q) ])) ]) ]
    
    # Remove vertex u from Q
    vertex_Q <- vertex_Q[ -which(vertex_Q==u) ]
    
    for (v in vertex_Q) {
      # Neighbours of vertex u that are still in Q and their distance from u
      neighbour <- graph[ which( graph[["v1"]]==u ), c("v2", "w")]
      
      if( any(neighbour[["v2"]] == v) ){
        # Alternate distance to vertex v
        alt <- dist[names(u)] + neighbour[ which( neighbour[["v2"]]==v ), "w" ]
      
        if( alt < dist[ names(vertex_Q[ which(vertex_Q==v) ]) ]){
          # Replace distance for vertex v in dist if alt is shorter than distance in dist
          dist[ names(vertex_Q[ which(vertex_Q==v) ]) ] <- alt
          prev[ names(vertex_Q[ which(vertex_Q==v) ]) ] <- u
        }
      }
    }
    i <- i+1
  }
  names(dist) <- NULL
  return(dist)
}

is.numeric.scalar <- function(x){
  return( is.numeric(x) && length(x)==1 )
}

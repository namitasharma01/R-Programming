#Advanced Programming in R - Lab 3
#Dijkstra algorithm

name  <- "Namita Sharma"
liuid <- "namsh440"

dijkstra <- function(graph, init_node){
  
  if( !is.data.frame(graph) || 
      !is.numeric.scalar(init_node) || 
      !(which(graph[["v1"]]==init_node) || which(graph[["v2"]]==init_node)) ){
    stop("Invalid inputs")
  } 

  # Create vertex set 
  vertex_Q <- unique(graph[["v1"]])
  
  # Set distance as Infinity and previous node as NA for each vertex in graph
  dist <- numeric(length(vertex_Q))
  prev <- numeric(length(vertex_Q))  
  for (i in vertex_Q) {
    dist[i] <- Inf
    prev[i] <- NA
  }
  dist[init_node] <- 0
  
  i <- 1
  while(length(vertex_Q) != 0){
    # u - vertex in Q with minimum distance dist[u]
    u <- which(dist == min( dist[vertex_Q] ))
    
    # Remove vertex u from Q
    vertex_Q <- vertex_Q[ -which(vertex_Q==u) ]
    
    for (v in vertex_Q) {
      # Neighbours of vertex u that are still in Q and their distance from u
      neighbour <- graph[["v2"]][ which(graph[["v1"]] == u) ]
      neighbour_dist <- graph[["w"]][which(graph[["v1"]] == u) ]
      
      if( any(neighbour == v) ){
        alt <- dist[u] + neighbour_dist[ which(neighbour==v) ]
        
        if(alt < dist[v]){
          dist[v] <- alt
          prev[v] <- u
        }
      }
    }
    i <- i+1
  }
  return(dist)
}

is.numeric.scalar <- function(x){
  return( is.numeric(x) && length(x)==1 )
}

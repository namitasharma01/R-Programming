#' Advanced Programming in R - Lab 6
#' Knapsack Problem - Greedy heuristics approach. This is an approximation
#' approach that trades the accuracy of optimization in return for a significant
#' gain in speed of computation.

#' @name greedy_knapsack
#' @aliases greedy_knapsack
#' @title Greedy Heuristics Algorithm
#' @description This function finds the optimum value for the knapsack problem.
#' @param x Dataframe
#' @param W Numeric scalar
#' @return List with two elements:
#'         1. value - optimum value that can be included in the knapsack
#'         2. elements - the list of elements that can be included in the knapsack
#' @usage greedy_knapsack(x, W)
#' @examples greedy_knapsack(x = knapsack_objects[1:12, ], W = 3500)
#' @source \url{"https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm"}
#' @export

# NOTE
# The time taken by greedy algorithm for n = 1000000 is 201.9903 milliseconds

greedy_knapsack <- function(x, W) {
  if (any(x < 0) ||
      any(names(x) %in% c("w", "v") == FALSE) ||
      W < 0) {
    stop("Invalid inputs!")
  }

  greedy = list(value    = 0,
                elements = integer())

  # Get the value-by-weight ratio for all elements and consider them
  # in decreasing order to put in the knapsack
  density <- x[["v"]] / x[["w"]]
  element <- order(density, decreasing = TRUE)

  i<- 1
  while (W - x[element[i], "w"] >= 0) {
    greedy$value <- greedy$value + x[element[i], "v"]
    W <- W - x[element[i], "w"]
    i <- i + 1
  }
  greedy$elements <- element[1:(i - 1)]

  return(greedy)
}

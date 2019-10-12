#' Advanced Programming in R - Lab 6
#' Knapsack Problem - Dynamic Programming Algorithm. This approach finds the exact
#' optimimum solution by building an optimization matrix and has lower complexity
#' than the brute force algorithm. The speed of this algorithm is further improved
#' by implementing it in Rcpp
#'
#' @name dynamic_knapsack
#' @aliases dynamic_knapsack
#' @title Dynamic Programming Algorithm
#' @description This function finds the optimum value for the knapsack problem.
#'              An Rcpp implementation can be called by setting fast as TRUE.
#' @param x Dataframe
#' @param W Numeric scalar
#' @param fast Logical scalar
#' @return List with two elements:
#'         1. value - optimum value that can be included in the knapsack
#'         2. elements - the list of elements that can be included in the knapsack
#' @usage dynamic_knapsack(x, W, fast = FALSE)
#' @examples dynamic_knapsack(x = knapsack_objects[1:12, ], W = 3500)
#' \dontrun{
#'   dynamic_knapsack(x = knapsack_objects[1:12, ], W = 3500, fast = TRUE)
#' }
#' @source /url{"https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem"}
#' @export

dynamic_knapsack = function(x, W, fast = FALSE) {
  if (any(x < 0) ||
      any(names(x) %in% c("w", "v") == FALSE) ||
      W < 0) {
    stop("Invalid inputs!")
  }

  if (fast == TRUE) {
    # Implement Dynamic programming in C++
    knapsack <- dynamic_knapsack_cpp(x = x, W = W)

  } else {
    # Implement Dynamic programming in R
    knapsack <- dynamic_knapsack_r(x = x, W = W)
  }
  return(knapsack)
}

dynamic_knapsack_r <- function(x, W) {
  # Function output- optimum value and list of elements
  knapsack = list(value    = 0,
                  elements = integer())
  n <- nrow(x)
  m <- matrix(data = -1,
              nrow = n,
              ncol = W)

  # For the first row in matrix m, we can calculate optimum value based
  # only on the first element- by including it for knapsack capacities
  # greater than weight of the first element and excluding it for knapsack
  # capacities less than the weight of the first element
  if (x[1, "w"] > W) {
    m[1, ] <- 0
  } else {
    m[1, x[1, "w"]:W] <- x[1, "w"]
    m[1, which(m[1, ] == -1)] <- 0
  }

  for (i in 2:n) {
    for (j in 1:W) {
      if (x[i, "w"] > j) {
        # If weight of current element i is greater than assumed
        # capacity j of knapsack, then take the best optimum value
        # calculated upto previous element from i - 1 row
        m[i, j] <- m[i - 1, j]

      } else {
        # If weight of current element i is within the assumed capacity
        # j of knapsack, then calculate the new optimum value by comparing
        # both cases - including element i and not including element i
        if (j - x[i, "w"] == 0) {
          # Exception case when the new elements weight is equal to the
          # knapsack capacity j
          m[i, j] <- pmax(m[i - 1, j], x[i, "v"])
        } else {
          m[i, j] <- pmax(m[i - 1, j], m[i - 1, j - x[i, "w"]] + x[i, "v"])
        }
      }
    }
  }

  # The optimum value that can be put in the knapsack is m[n][W]
  knapsack$value <- m[n,W]

  # Determine the elements included for the optimum value
  i <- n
  j <- W
  k <- 1
  while (i > 1 && j > 0) {
    if (m[i, j] != m[i - 1, j]) {
      # Add the value of the element to the knapsack value and decrement its
      # weight from remaining knapsack capacity j
      knapsack$elements[k] <- i
      j <- j - x[i, "w"]
      k <- k + 1
    }
    # Move on to the next element if the current element is not included
    # in the knapsack
    i <- i - 1
  }

  # First element
  if (i == 1 && j - x[1, "w"] >= 0) {
    knapsack$elements[k + 1] <- i
  }
  return(knapsack)
}

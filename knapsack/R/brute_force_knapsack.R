#' Advanced Programming in R - Lab 6
#' Knapsack Problem - Brute Force Algorithm. This approach finds the exact
#' optimimum solution by comparing each of the possible outcomes and has high
#' computational complexity. Parallelization has been implemented for this
#' technique to improve the speed of the algorithm
#'
#' @name brute_force_knapsack
#' @aliases brute_force_knapsack
#' @title Brute Force Algorithm
#' @description This function finds the optimum value for the knapsack problem.
#'              Parallelization can be achieved by setting parallel as TRUE
#' @param x Dataframe
#' @param W Numeric scalar
#' @param parallel Logical scalar
#' @return List with two elements:
#'         1. value - optimum value that can be included in the knapsack
#'         2. elements - the list of elements that can be included in the knapsack
#' @usage brute_force_knapsack(x, W, parallel)
#' @examples brute_force_knapsack(x = knapsack_objects[1:12, ], W = 3500)
#' \dontrun{
#'   brute_force_knapsack(x = knapsack_objects[1:12, ], W = 3500, parallel = TRUE)
#' }
#' @export

# NOTE
# The time taken by brute force algorithm for n = 16 is 2.31187 seconds

brute_force_knapsack <- function(x, W, parallel = FALSE) {
  if (any(x < 0) ||
      any(names(x) %in% c("w", "v") == FALSE) ||
      W < 0) {
    stop("Invalid inputs!")
  }

  n <- nrow(x)
  brute_force = list(value    = 0,
                     elements = integer())

  if (parallel == TRUE) {
    cores    <- parallel::detectCores()
    clusters <- parallel::makeCluster(cores, type = "PSOCK")

    # Consider all possible combinations of n elements that can be
    # put in the knapsack
    combinations <- 1:((2 ^ n) - 1)

    # Parallelize the most time intensive operations:
    # 1. calculating all combination of elements to be included in the knapsack
    # 2. Calculating total weights of all combinations
    # 3. Calculating total values of all combinations
    elements <- parallel::parLapply(cl = clusters, X = combinations, fun = function(y) {
                  binary_i <- intToBits(y)
                  elements <- which(binary_i == 01)
                  return(elements)
                })

    values   <- parallel::parSapply(cl = clusters, X = elements, FUN = function(y) {
                  return(sum(x[y, "v"]))
                })

    weights  <- parallel::parSapply(cl = clusters, X = elements, FUN = function(y) {
                  return(sum(x[y, "w"]))
                })

    parallel::stopCluster(cl = clusters)

    brute_force$value    <- max(values[which(weights <= W)])
    brute_force$elements <- elements[[which(values == brute_force$value)]]

  } else {
    # Consider all possible combinations of n elements that can be
    # put in the knapsack
    for (i in 1:((2 ^ n) - 1)) {
      binary_i <- intToBits(i)
      elements <- which(binary_i == 01)

      v_tot <- sum(x[elements, "v"])
      w_tot <- sum(x[elements, "w"])

      # Store the combination of elements with the highest total value
      # with total weight less than or equal to knapsack capacity
      if (v_tot > brute_force$value && w_tot <= W) {
        brute_force$value    <- v_tot
        brute_force$elements <- elements
      }
    }
  }

  return(brute_force)
}

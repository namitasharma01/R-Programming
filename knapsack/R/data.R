#' Knapsack Objects
#'
#' This dataset contains a list of objects with associated values and weights
#' of which a subset needs to be included in the knapsack such that the total
#' value of the subset is maximized while its total weight does not exceed the
#' weight capacity of the knapsack.
#'
#' @format \code{knapsack_objects} is a data frame with 2000 objects (rows) and 2
#'   attributes (columns) named \code{v}, \code{w}.
#'
#' \describe{
#'   \item{v}{values, value associated with each element}
#'   \item{w}{weights, weight associated with each element}
#' }
#'
"knapsack_objects"

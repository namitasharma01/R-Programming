% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{dynamic_knapsack_cpp}
\alias{dynamic_knapsack_cpp}
\title{Rcpp implementation of dynamic programming for knapsack problem}
\usage{
dynamic_knapsack_cpp(x, W)
}
\arguments{
\item{x}{Dataframe}

\item{W}{Numeric scalar}
}
\value{
List with two elements:
        1. value - optimum value that can be included in the knapsack
        2. elements - the list of elements that can be included in the knapsack
}
\description{
Rcpp implementation of dynamic programming for knapsack problem
}

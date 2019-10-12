#include <Rcpp.h>
using namespace Rcpp;

//' Rcpp implementation of dynamic programming for knapsack problem
//'
//' @param x Dataframe
//' @param W Numeric scalar
//' @return List with two elements:
//'         1. value - optimum value that can be included in the knapsack
//'         2. elements - the list of elements that can be included in the knapsack
//' @export
// [[Rcpp::export]]
List dynamic_knapsack_cpp(DataFrame x, int W) {

  int n = x.nrows();
  NumericVector values  = x["v"];
  IntegerVector weights = x["w"];
  NumericMatrix m(n+1, W+1);

  for(int i = 1; i <= n; i++) {
    for(int j = 0; j <= W; j++) {
      if(weights[i-1] > j) {
        m(i, j) = m(i-1, j);
      } else {
        m(i, j) = std::max(m(i-1, j), m(i-1, j-weights[i-1]) + values[i-1]);
      }
    }
  }
  double value = m(n, W);
  IntegerVector elements(n);

  int i = n;
  int j = W;
  int k = 0;

  while(i > 0) {
    if(m(i, j) != m(i - 1, j)) {
      elements[k++] = i;
      j = j-weights[i-1];
    }
    i--;
  }

  return(List::create(Named("value")    = value,
                      Named("elements") = elements));
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R

*/

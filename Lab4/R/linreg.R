#' Advanced Programming in R - Lab 4
#' Linear Regression using ordinary least squares

linreg <- setRefClass("linreg",
                      fields = list(
                        X            = "matrix",
                        Y            = "numeric",
                        beta_cap     = "matrix",
                        Y_cap        = "matrix",
                        E_cap        = "matrix",
                        sigsq_cap    = "matrix",
                        var_beta_cap = "matrix",
                        formula      = "formula"
                      ),
                      methods = list(
                        print = function(){
                          beta_cap_v <- as.vector(beta_cap)
                          names(beta_cap_v) <- dimnames(beta_cap)[[1]]

                          return(beta_cap_v)
                        },

                        plot = function(){
                          ggplot2::qplot(x = Y_cap,
                                         y = E_cap,
                                         main = "Residuals vs Fitted",
                                         xlab = paste("Fitted values \nlm(",format(formula),")"),
                                         ylab = "Residuals",
                                         )
                          #ggplot2::plot(Y_cap, sqrt(E_cap))
                        },

                        resid = function(){
                          return(as.vector(E_cap))
                        },

                        pred = function(){
                          return(as.vector(Y_cap))
                        },

                        coef = function(){
                          beta_cap_v <- as.vector(beta_cap)
                          names(beta_cap_v) <- dimnames(beta_cap)[[1]]

                          return(beta_cap_v)
                        },

                        summary = function(){

                        }
                        )
                      )


linreg1 <- function(formula, data){

  X        <- model.matrix(formula)
  Y        <- data[,all.vars(formula)[1]]
  beta_cap <- solve(t(X) %*% X) %*% (t(X) %*% Y)
  Y_cap    <- X %*% beta_cap
  E_cap    <- Y - Y_cap

  linreg_obj <- linreg$new(X        = X,
                           Y        = Y,
                           beta_cap = beta_cap,
                           Y_cap    = Y_cap,
                           E_cap    = E_cap,
                           formula  = formula)
  return(linreg_obj)
}

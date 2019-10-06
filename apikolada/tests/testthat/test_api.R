context("apikolada")

test_that("get.muni() method works", {
  obj.api.kolada <- apikolada::api.kolada$new()

  expected.df <- data.frame("id"    = c("1440", "1489", "0764", "0604", "1984", "2506"),
                            "title" = c("Ale", "Alingsås", "Alvesta", "Aneby", "Arboga", "Arjeplog"),
                            "type"  = c("K", "K", "K", "K", "K", "K"),
                            stringsAsFactors = FALSE)

  expect_equal(head(obj.api.kolada$get.muni()), expected.df)
})

test_that("get.ou.muni() method works", {
  obj.api.kolada <- apikolada::api.kolada$new()

  expected.df <- data.frame("id"           = c("V15E018000101", "V15E018000201", "V15E018000301", "V15E018000308", "V15E018000401", "V15E018000801"),
                            "municipality" = c("0180", "0180", "0180", "0180", "0180", "0180"),
                            "title"        = c("Abrahamsbergsskolan", "Aspuddens skola", "Bagarmossens skola", "Brotorpsskolan", "Gullingeskolan", "Bäckahagens skola"),
                            stringsAsFactors = FALSE)

  expect_equal(head(obj.api.kolada$get.ou.muni("Stockholm")), expected.df)
})

test_that("get.ou.muni() method works", {
  obj.api.kolada <- apikolada::api.kolada$new()

  expected.df <- data.frame("id"           = c("V15E018000101", "V15E018000201", "V15E018000301", "V15E018000308", "V15E018000401", "V15E018000801"),
                            "municipality" = c("0180", "0180", "0180", "0180", "0180", "0180"),
                            "title"        = c("Abrahamsbergsskolan", "Aspuddens skola", "Bagarmossens skola", "Brotorpsskolan", "Gullingeskolan", "Bäckahagens skola"),
                            stringsAsFactors = FALSE)

  expect_equal(head(obj.api.kolada$get.ou.muni("Stockholm")), expected.df)
})

#test_that("print() method works", {
#  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#
#  expect_output(linreg_mod$print(),"linreg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris\\)")
#  expect_output(linreg_mod$print(),"( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length")
#})
#
#test_that("pred() method works", {
#  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#
#  expect_equal(round(unname(linreg_mod$pred()[c(1,5,7)]),2), c(1.85, 1.53, 1.09))
#})
#
#test_that("resid() method works", {
#  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#
#  expect_equal(round(unname(linreg_mod$resid()[c(7,13,27)]),2), c(0.31, -0.58, -0.20))
#})
#
#test_that("coef() method works", {
#  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#
#  expect_true(all(round(unname(linreg_mod$coef()),2) %in% c(-2.52, -1.34, 1.78)))
#})
#
#
#test_that("summary() method works", {
#  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#
#  expect_output(linreg_mod$summary(), "\\(Intercept\\)( )*-2.5[0-9]*( )*0.5[0-9]*( )*-4.4[0-9]*( )*.*( )*\\*\\*\\*")
#  expect_output(linreg_mod$summary(), "Sepal.Width( )*-1.3[0-9]*( )*0.1[0-9]*( )*-10.9[0-9]*( )*.*( )*\\*\\*\\*")
#  expect_output(linreg_mod$summary(), "Sepal.Length( )*1.7[0-9]*( )*0.0[0-9]*( )*27.5[0-9]*( )*.*( )*\\*\\*\\*")
#  expect_output(linreg_mod$summary(), "Residual standard error: 0.6[0-9]* on 147 degrees of freedom")
#})
#

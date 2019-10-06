context("apikolada")

test_that("get.muni() method works", {
  expected.df <- data.frame("id"    = c("1440", "1489", "0764", "0604", "1984", "2506"),
                            "title" = c("Ale", "Alingsås", "Alvesta", "Aneby", "Arboga", "Arjeplog"),
                            "type"  = c("K", "K", "K", "K", "K", "K"),
                            stringsAsFactors = FALSE)

  obj.api.kolada <- apikolada::api.kolada$new()
  expect_equal(head(obj.api.kolada$get.muni()), expected.df)
})

test_that("get.ou.muni() method works", {
  expected.df <- data.frame("id"           = c("V15E018000101", "V15E018000201", "V15E018000301", "V15E018000308", "V15E018000401", "V15E018000801"),
                            "municipality" = c("0180", "0180", "0180", "0180", "0180", "0180"),
                            "title"        = c("Abrahamsbergsskolan", "Aspuddens skola", "Bagarmossens skola", "Brotorpsskolan", "Gullingeskolan", "Bäckahagens skola"),
                            stringsAsFactors = FALSE)

  obj.api.kolada <- apikolada::api.kolada$new()
  expect_equal(head(obj.api.kolada$get.ou.muni("Stockholm")), expected.df)
})

test_that("get.kpi.group() method works", {
  obj.api.kolada <- apikolada::api.kolada$new()

  expected.df <- data.frame("id"           = c("V15E018000101", "V15E018000201", "V15E018000301", "V15E018000308", "V15E018000401", "V15E018000801"),
                            "municipality" = c("0180", "0180", "0180", "0180", "0180", "0180"),
                            "title"        = c("Abrahamsbergsskolan", "Aspuddens skola", "Bagarmossens skola", "Brotorpsskolan", "Gullingeskolan", "Bäckahagens skola"),
                            stringsAsFactors = FALSE)

  expect_equal(head(obj.api.kolada$get.ou.muni("Stockholm")), expected.df)
})



context("dynamic_knapsack")

RNGkind(sample.kind = "Rounding")
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(dk <- dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(dk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(dynamic_knapsack("text", 3500))
  expect_error(dynamic_knapsack(x = knapsack_objects[1:8,] * -1, W = 3500))
})

test_that("Function return correct results.", {
  dk <- dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(dk$value), 16770)
  expect_true(all(round(dk$elements) %in% c(5, 8)))

  dk <- dynamic_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(dk$value), 16770)
  expect_true(all(round(dk$elements) %in% c(5, 8)))

  dk <- dynamic_knapsack(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(dk$value), 15428)
  expect_true(all(round(dk$elements) %in% c(3, 8)))

  dk <- dynamic_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(dk$value), 15428)
  expect_true(all(round(dk$elements) %in% c(3, 8)))

  st <- system.time(dk <- greedy_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)

  dk <- dynamic_knapsack(x = knapsack_objects[1:100,], W = 2000)
  expect_equal(round(dk$value), 59284)
})

context("dijkstra")

wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

test_that("outputs are correct in the Dijkstra algorithm.", {
  expect_equal(dijkstra(wiki_graph,1), c(0,7,9,20,20,11))
  expect_equal(dijkstra(wiki_graph,3), c(9,10,0,11,11,2))
})


test_that("Error messages are returned for erronous input in the Dijkstra algorithm.", {
  wiki_wrong_graph <- wiki_graph
  names(wiki_wrong_graph) <- c("v1, v3, w")
  expect_error(dijkstra(wiki_wrong_graph, 3))
  wiki_wrong_graph <- wiki_graph[1:2]
  expect_error(dijkstra(wiki_wrong_graph, 3))
  expect_error(dijkstra(wiki_graph, 7))
  expect_error(dijkstra(as.matrix(wiki_graph), 3))  
})

#----Test Case2------
random_graph <-
  data.frame(v1=c(10,10,10,20,20,20,30,30,30,30,40,40,40,50,50,60,60,60),
             v2=c(20,30,60,10,30,40,10,20,40,60,20,30,50,40,60,10,30,50),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

test_that("outputs are correct in the Dijkstra algorithm.", {
  expect_equal(dijkstra(random_graph,10), c(0,7,9,20,20,11))
  expect_equal(dijkstra(random_graph,30), c(9,10,0,11,11,2))
})


test_that("Error messages are returned for erronous input in the Dijkstra algorithm.", {
  wiki_wrong_graph <- random_graph
  names(wiki_wrong_graph) <- c("v1, v3, w")
  expect_error(dijkstra(wiki_wrong_graph, 30))
  wiki_wrong_graph <- random_graph[1:2]
  expect_error(dijkstra(wiki_wrong_graph, 30))
  expect_error(dijkstra(random_graph, 70))
  expect_error(dijkstra(as.matrix(random_graph), 30))  
})

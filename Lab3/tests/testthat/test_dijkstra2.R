context("dijkstra2")

wiki_graph2 <-
  data.frame(v1=c(10,10,10,20,20,20,30,30,30,30,40,40,40,50,50,60,60,60),
             v2=c(20,30,60,10,30,40,10,20,40,60,20,30,50,40,60,10,30,50),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

test_that("outputs are correct in the Dijkstra algorithm.", {
  expect_equal(dijkstra2(wiki_graph2,10), c(0,7,9,20,20,11))
  expect_equal(dijkstra2(wiki_graph2,30), c(9,10,0,11,11,2))
})


test_that("Error messages are returned for erronous input in the Dijkstra algorithm.", {
  wiki_wrong_graph <- wiki_graph2
  names(wiki_wrong_graph) <- c("v1, v3, w")
  expect_error(dijkstra2(wiki_wrong_graph, 30))
  wiki_wrong_graph <- wiki_graph2[1:2]
  expect_error(dijkstra2(wiki_wrong_graph, 30))
  expect_error(dijkstra2(wiki_graph2, 70))
  expect_error(dijkstra2(as.matrix(wiki_graph2), 30))  
})


wiki_graph3 <-
  data.frame(v1=c("a","a","a","b","b","b","c","c","c","c","d","d","d","e","e","f","f","f"),
             v2=c("b","c","f","a","c","d","a","b","d","f","b","c","e","d","f","a","c","e"),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

test_that("outputs are correct in the Dijkstra algorithm.", {
  expect_equal(dijkstra2(wiki_graph3,"a"), c(0,7,9,20,20,11))
  expect_equal(dijkstra2(wiki_graph3,"c"), c(9,10,0,11,11,2))
})


test_that("Error messages are returned for erronous input in the Dijkstra algorithm.", {
  wiki_wrong_graph <- wiki_graph3
  names(wiki_wrong_graph) <- c("v1, v3, w")
  expect_error(dijkstra2(wiki_wrong_graph, "c"))
  wiki_wrong_graph <- wiki_graph3[1:2]
  expect_error(dijkstra2(wiki_wrong_graph, "c"))
  expect_error(dijkstra2(wiki_graph3, "g"))
  expect_error(dijkstra2(as.matrix(wiki_graph3), "c"))  
})


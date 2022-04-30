test_that("dist", {
  library(partialised)

  dist <- function(x, y) {
    sqrt(x ^ 2 + y ^ 2)
  }

  pdist <- new_partialised(dist,
                           list(x = 3))
  expect_equal(pdist(y = 4), 5)

  arguments(pdist)
  arguments(pdist)$x <- 6

  expect_equal(pdist(y = 8), 10)

  arg(pdist, "y")
  arg(pdist, "y") <- 8

  expect_equal(pdist(), 10)
})

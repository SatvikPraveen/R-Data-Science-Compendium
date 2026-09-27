test_that("with_seed is reproducible and restores the caller's RNG state", {
  set.seed(99)
  before <- .Random.seed
  a <- with_seed(1, runif(3))
  expect_identical(.Random.seed, before)
  b <- with_seed(1, runif(3))
  expect_identical(a, b)
})

test_that("with_seed(NULL) simply evaluates the code", {
  set.seed(5)
  expected <- runif(2)
  set.seed(5)
  expect_identical(with_seed(NULL, runif(2)), expected)
})

test_that("rng_streams returns distinct, reproducible L'Ecuyer streams", {
  kind_before <- RNGkind()
  s1 <- rng_streams(4, seed = 10)
  s2 <- rng_streams(4, seed = 10)
  expect_identical(s1, s2)
  expect_length(unique(s1), 4L)
  expect_identical(RNGkind(), kind_before)
  expect_true(all(vapply(s1, length, integer(1)) == 7L))
})

test_that("rng_streams requires a seed", {
  expect_error(rng_streams(2, seed = NULL), class = "rdsc_error_input")
})

test_that("with_stream restores the RNG kind", {
  kind_before <- RNGkind()
  s <- rng_streams(1, seed = 1)[[1]]
  x <- with_stream(s, function() runif(1))
  y <- with_stream(s, function() runif(1))
  expect_identical(x, y)
  expect_identical(RNGkind(), kind_before)
})

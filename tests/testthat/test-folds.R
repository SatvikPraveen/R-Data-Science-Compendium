test_that("random folds are balanced partitions", {
  f <- make_folds(103, k = 10, seed = 1)
  expect_length(f, 103)
  expect_setequal(unique(f), 1:10)
  expect_lte(diff(range(table(f))), 1)
})

test_that("stratified folds balance a rare class", {
  y <- rep(c(0, 1), c(90, 10))
  f <- make_folds(100, k = 5, strata = y, seed = 2)
  expect_true(all(table(f, y)[, "1"] == 2))
  expect_lte(diff(range(table(f))), 1)
})

test_that("numeric strata are binned", {
  y <- seq(0, 1, length.out = 80)
  f <- make_folds(80, k = 4, strata = y, n_bins = 4, seed = 1)
  bins <- cut(y, quantile(y, 0:4 / 4), include.lowest = TRUE)
  expect_true(all(table(f, bins) == 5))
})

test_that("grouped folds keep groups together", {
  g <- rep(letters[1:12], times = 1:12)
  f <- make_folds(length(g), k = 4, groups = g, seed = 3)
  expect_true(all(tapply(f, g, function(v) length(unique(v))) == 1))
  sizes <- table(f)
  expect_lt(max(sizes) - min(sizes), 12)
})

test_that("make_folds is reproducible", {
  expect_identical(make_folds(50, 5, seed = 9), make_folds(50, 5, seed = 9))
})

test_that("make_folds validates its inputs", {
  expect_error(make_folds(10, k = 1), class = "rdsc_error_input")
  expect_error(make_folds(10, strata = 1:10, groups = 1:10),
               class = "rdsc_error_input")
  expect_error(make_folds(10, groups = 1:3), class = "rdsc_error_input")
  expect_error(make_folds(10, k = 5, groups = rep(1:2, 5)),
               class = "rdsc_error_input")
  expect_error(make_folds(4, strata = c(1, NA, 1, 2)),
               class = "rdsc_error_input")
})

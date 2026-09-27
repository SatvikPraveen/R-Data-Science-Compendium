test_that("exact two-sample p-value is correct for a hand-computed case", {
  res <- perm_test(c(1, 2, 3), c(4, 5, 6))
  expect_true(res$exact)
  expect_equal(unname(res$parameter), choose(6, 3))
  # Only the observed split and its mirror image reach |diff| = 3.
  expect_equal(res$p.value, 2 / 20)
  expect_equal(perm_test(c(1, 2, 3), c(4, 5, 6),
                         alternative = "less")$p.value, 1 / 20)
  expect_equal(perm_test(c(1, 2, 3), c(4, 5, 6),
                         alternative = "greater")$p.value, 1)
  expect_s3_class(res, "htest")
})

test_that("exact paired test counts sign flips correctly", {
  d <- c(1, 2, 3, 4)
  # observed mean 2.5 is the maximum; only all-positive and all-negative
  # sign vectors are as extreme in absolute value.
  res <- perm_test(d)
  expect_equal(res$p.value, 2 / 16)
  paired <- perm_test(d + 10, rep(10, 4), paired = TRUE)
  expect_equal(paired$p.value, res$p.value)
})

test_that("floating-point ties are counted as extreme", {
  x <- c(0.1, 0.2, 0.3)
  y <- c(0.3, 0.2, 0.1)
  res <- perm_test(x, y, statistic = function(a, b) sum(a) - sum(b))
  expect_equal(res$p.value, 1)
})

test_that("Monte Carlo p-values follow Phipson & Smyth and are never zero", {
  set.seed(1)
  x <- rnorm(30, 3)
  y <- rnorm(30)
  res <- perm_test(x, y, R = 999, seed = 1)
  expect_false(res$exact)
  expect_equal(res$p.value, 1 / 1000)
  expect_gt(res$p.value.mcse, 0)
})

test_that("Monte Carlo test is reproducible and restores RNG state", {
  x <- rnorm(25)
  y <- rnorm(25)
  set.seed(4)
  state <- .Random.seed
  a <- perm_test(x, y, R = 199, seed = 3)
  expect_identical(.Random.seed, state)
  b <- perm_test(x, y, R = 199, seed = 3)
  expect_identical(a$null_distribution, b$null_distribution)
})

test_that("Monte Carlo permutation test controls the type I error rate", {
  skip_on_cran()
  p <- vapply(seq_len(300), function(i) {
    xy <- with_seed(i, rnorm(30))
    perm_test(xy[1:15], xy[16:30], R = 199, exact = FALSE, seed = i)$p.value
  }, numeric(1))
  expect_lt(mean(p <= 0.05), 0.05 + 3 * sqrt(0.05 * 0.95 / 300))
})

test_that("exact and Monte Carlo p-values agree approximately", {
  x <- c(19.1, 22.4, 20.8, 24.0, 21.7, 23.3)
  y <- c(18.2, 17.9, 20.1, 19.4, 16.8, 18.8)
  ex <- perm_test(x, y)
  mc <- perm_test(x, y, exact = FALSE, R = 20000, seed = 1)
  expect_lt(abs(mc$p.value - ex$p.value), 4 * mc$p.value.mcse)
})

test_that("custom statistics are supported", {
  res <- perm_test(c(5, 6, 7, 8), c(1, 2, 3, 30),
                   statistic = function(a, b) median(a) - median(b))
  expect_equal(unname(res$statistic), 6.5 - 2.5)
})

test_that("perm_test validates its inputs", {
  expect_error(perm_test("a"), class = "rdsc_error_input")
  expect_error(perm_test(1:3, paired = TRUE), class = "rdsc_error_input")
  expect_error(perm_test(1:3, 4:6, mu = 1), class = "rdsc_error_input")
  expect_error(perm_test(1:3, 1:4, paired = TRUE), class = "rdsc_error_input")
  expect_error(perm_test(1:3, exact = "yes"), class = "rdsc_error_input")
  expect_error(perm_test(rnorm(30), rnorm(30), exact = TRUE),
               class = "rdsc_error_input")
  expect_error(perm_test(1:3, 4:6, statistic = function(a, b) NA),
               class = "rdsc_error_input")
})

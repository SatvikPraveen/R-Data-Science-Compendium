test_that("boot_ci reproduces boot::boot and boot::boot.ci exactly", {
  skip_if_not_installed("boot")
  set.seed(1)
  x <- rexp(40)
  ci <- boot_ci(x, mean, R = 1999, seed = 42)

  b <- with_seed(42, boot::boot(x, function(d, i) mean(d[i]), R = 1999))
  expect_identical(unname(b$t[, 1]), ci$replicates)

  ref <- boot::boot.ci(b, type = c("perc", "basic", "norm", "bca"),
                       L = jackknife_influence(x, mean))
  iv <- ci$intervals
  expect_equal(unname(unlist(iv[iv$type == "percentile", c("lower", "upper")])),
               unname(ref$percent[4:5]), tolerance = 1e-12)
  expect_equal(unname(unlist(iv[iv$type == "basic", c("lower", "upper")])),
               unname(ref$basic[4:5]), tolerance = 1e-12)
  expect_equal(unname(unlist(iv[iv$type == "normal", c("lower", "upper")])),
               unname(ref$normal[2:3]), tolerance = 1e-12)
  expect_equal(unname(unlist(iv[iv$type == "bca", c("lower", "upper")])),
               unname(ref$bca[4:5]), tolerance = 1e-12)
})

test_that("boot_ci resamples rows of data frames", {
  skip_if_not_installed("boot")
  stat <- function(d) cor(d$mpg, d$wt)
  ci <- boot_ci(mtcars, stat, R = 999, type = "percentile", seed = 7)
  b <- with_seed(7, boot::boot(mtcars, function(d, i) stat(d[i, ]), R = 999))
  expect_identical(unname(b$t[, 1]), ci$replicates)
  expect_equal(ci$estimate, stat(mtcars))
})

test_that("boot_ci is reproducible and leaves the RNG untouched", {
  set.seed(3)
  state <- .Random.seed
  a <- boot_ci(1:20, median, R = 199, seed = 11)
  expect_identical(.Random.seed, state)
  b <- boot_ci(1:20, median, R = 199, seed = 11)
  expect_identical(a$intervals, b$intervals)
})

test_that("boot_ci returns the requested types in order with sane limits", {
  ci <- boot_ci(c(2.1, 3.4, 1.9, 5.6, 4.4, 3.3, 2.8, 4.9), mean, R = 999,
                type = c("bca", "normal"), seed = 1)
  expect_identical(ci$intervals$type, c("bca", "normal"))
  expect_true(all(ci$intervals$lower < ci$intervals$upper))
  expect_s3_class(ci, "rdsc_boot_ci")
  expect_output(print(ci), "bootstrap")
  expect_s3_class(as.data.frame(ci), "data.frame")
})

test_that("jackknife influence of the mean is the centred data", {
  x <- c(1, 4, 2, 8, 5)
  expect_equal(jackknife_influence(x, mean), x - mean(x))
})

test_that("BCa acceleration is zero for symmetric data", {
  x <- c(-3, -1, 0, 1, 3)
  expect_equal(jackknife_acceleration(x, mean), 0)
})

test_that("degenerate statistics give NA BCa limits with a warning", {
  expect_warning(
    ci <- boot_ci(rep(1, 10), mean, R = 99, type = "bca", seed = 1),
    "undefined"
  )
  expect_true(all(is.na(ci$intervals[, c("lower", "upper")])))
})

test_that("non-finite replicates are dropped with a warning", {
  stat <- function(x) if (length(unique(x)) < 3) NA_real_ else mean(x)
  expect_warning(boot_ci(c(1, 2, 3, 4), stat, R = 99, type = "percentile",
                         seed = 1), "not finite")
})

test_that("norm_inter matches boot:::norm.inter including interpolation", {
  skip_if_not_installed("boot")
  t <- sort(rnorm(500))
  alpha <- c(0.013, 0.2, 0.5, 0.977)
  expect_equal(norm_inter(t, alpha),
               unname(boot:::norm.inter(t, alpha)[, 2]))
})

test_that("boot_ci validates its inputs", {
  expect_error(boot_ci(1:10, "mean"), class = "rdsc_error_input")
  expect_error(boot_ci(1, mean), class = "rdsc_error_input")
  expect_error(boot_ci(1:10, mean, level = 1.2), class = "rdsc_error_input")
  expect_error(boot_ci(1:10, function(x) c(1, 2)), class = "rdsc_error_input")
  expect_error(boot_ci(1:10, mean, R = 1.5), class = "rdsc_error_input")
})

test_that("percentile interval attains nominal coverage for a normal mean", {
  skip_on_cran()
  covered <- vapply(seq_len(200), function(i) {
    x <- with_seed(i, rnorm(60, mean = 5))
    iv <- boot_ci(x, mean, R = 499, type = "percentile", seed = i)$intervals
    iv$lower <= 5 && 5 <= iv$upper
  }, logical(1))
  # 95% nominal; MCSE ~ 1.5 percentage points
  expect_gt(mean(covered), 0.88)
  expect_lt(mean(covered), 0.99)
})

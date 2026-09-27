x <- sleep$extra[sleep$group == 1]
y <- sleep$extra[sleep$group == 2]

test_that("Cohen's d is consistent with the pooled-variance t statistic", {
  d <- cohens_d(x, y)
  tt <- t.test(x, y, var.equal = TRUE)
  expect_equal(d$t, unname(tt$statistic))
  expect_equal(d$estimate, unname(tt$statistic) * sqrt(1 / 10 + 1 / 10))
  expect_equal(d$df, 18)
})

test_that("paired d_z is consistent with the paired t statistic", {
  d <- cohens_d(x, y, paired = TRUE)
  tt <- t.test(x, y, paired = TRUE)
  expect_equal(d$t, unname(tt$statistic))
  expect_equal(d$estimate, mean(x - y) / sd(x - y))
  expect_identical(d$design, "paired (d_z)")
})

test_that("one-sample d uses mu", {
  d <- cohens_d(x, mu = 1)
  expect_equal(d$estimate, (mean(x) - 1) / sd(x))
})

test_that("noncentral-t limits invert the CDF exactly", {
  d <- cohens_d(x, y, level = 0.9)
  s <- sqrt(1 / 10 + 1 / 10)
  expect_equal(pt(d$t, 18, ncp = d$lower / s), 0.95, tolerance = 1e-8)
  expect_equal(pt(d$t, 18, ncp = d$upper / s), 0.05, tolerance = 1e-8)
})

test_that("the interval for d = 0 is symmetric", {
  d <- cohens_d(c(-1, 0, 1, -2, 2), c(-1, 0, 1, -2, 2) + 0)
  expect_equal(d$estimate, 0)
  expect_equal(d$lower, -d$upper, tolerance = 1e-8)
})

test_that("Hedges' correction matches the known approximation", {
  df <- c(5, 10, 50, 200)
  expect_equal(hedges_correction(df), 1 - 3 / (4 * df - 1), tolerance = 1e-3)
  expect_equal(hedges_correction(18), 0.9576, tolerance = 1e-4)
  g <- hedges_g(x, y)
  d <- cohens_d(x, y)
  expect_equal(g$estimate, d$estimate * hedges_correction(18))
  expect_equal(g$lower, d$lower * hedges_correction(18))
  expect_identical(g$measure, "Hedges' g")
})

test_that("Hedges' g is (nearly) unbiased where Cohen's d is not", {
  skip_on_cran()
  sims <- vapply(seq_len(2000), function(i) {
    z <- with_seed(i, rnorm(10))
    a <- z[1:5] + 1
    b <- z[6:10]
    c(cohens_d(a, b)$estimate, hedges_g(a, b)$estimate)
  }, numeric(2))
  bias <- rowMeans(sims) - 1
  mcse <- apply(sims, 1, sd) / sqrt(2000)
  expect_gt(bias[1], 3 * mcse[1])       # d is biased upwards
  expect_lt(abs(bias[2]), 3 * mcse[2])  # g is not
})

test_that("noncentral-t interval attains nominal coverage", {
  skip_on_cran()
  covered <- vapply(seq_len(500), function(i) {
    z <- with_seed(i, rnorm(30))
    d <- cohens_d(z[1:12] + 0.5, z[13:30])
    d$lower <= 0.5 && 0.5 <= d$upper
  }, logical(1))
  expect_equal(mean(covered), 0.95, tolerance = 3 * sqrt(0.95 * 0.05 / 500))
})

test_that("effect size objects print and coerce", {
  d <- cohens_d(x, y)
  expect_s3_class(d, "rdsc_effect_size")
  expect_output(print(d), "Cohen's d")
  expect_named(as.data.frame(d),
               c("measure", "design", "estimate", "lower", "upper", "level",
                 "se", "df"))
})

test_that("effect sizes validate their inputs", {
  expect_error(cohens_d(1), class = "rdsc_error_input")
  expect_error(cohens_d(x, paired = TRUE), class = "rdsc_error_input")
  expect_error(cohens_d(x, y, mu = 1), class = "rdsc_error_input")
  expect_error(cohens_d(x, y[-1], paired = TRUE), class = "rdsc_error_input")
  expect_error(hedges_correction(1), class = "rdsc_error_input")
  expect_error(cohens_d(c(1, 1, 1)), "undefined")
})

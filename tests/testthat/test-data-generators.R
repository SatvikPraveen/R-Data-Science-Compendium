test_that("simulate_linear recovers its coefficients", {
  d <- simulate_linear(5000, beta = c(1, -0.5, 0), intercept = 2, sigma = 1,
                       rho = 0.5, seed = 1)
  expect_named(d, c("y", "x1", "x2", "x3"))
  fit <- lm(y ~ ., data = d)
  ci <- confint(fit, level = 0.999)
  truth <- c(2, 1, -0.5, 0)
  expect_true(all(ci[, 1] < truth & truth < ci[, 2]))
  expect_equal(attr(d, "truth")$beta, c(1, -0.5, 0))
})

test_that("predictors have AR(1) correlation", {
  d <- simulate_linear(20000, beta = c(0, 0, 0), rho = 0.6, seed = 2)
  r <- cor(d[, -1])
  expect_equal(r, ar1_cor(3, 0.6), tolerance = 0.03, ignore_attr = TRUE)
})

test_that("simulate_logistic produces valid probabilities and outcomes", {
  d <- simulate_logistic(3000, beta = c(1, -1), intercept = -1, seed = 3)
  expect_true(all(d$y %in% 0:1))
  expect_true(all(d$p > 0 & d$p < 1))
  fit <- glm(y ~ x1 + x2, data = d, family = binomial)
  ci <- suppressMessages(confint.default(fit, level = 0.999))
  truth <- c(-1, 1, -1)
  expect_true(all(ci[, 1] < truth & truth < ci[, 2]))
})

test_that("generators are reproducible and validate input", {
  expect_identical(simulate_linear(10, 1, seed = 1),
                   simulate_linear(10, 1, seed = 1))
  expect_error(simulate_linear(10, "a"), class = "rdsc_error_input")
  expect_error(simulate_linear(10, 1, sigma = -1), class = "rdsc_error_input")
  expect_error(ar1_cor(3, 1), class = "rdsc_error_input")
})

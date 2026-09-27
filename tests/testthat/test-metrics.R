test_that("regression metrics are correct", {
  y <- c(1, 2, 3, 4)
  p <- c(1.5, 2, 2, 5)
  expect_equal(rmse(y, p), sqrt(mean(c(0.25, 0, 1, 1))))
  expect_equal(mae(y, p), mean(c(0.5, 0, 1, 1)))
  expect_equal(r_squared(y, y), 1)
  expect_equal(r_squared(y, rep(mean(y), 4)), 0)
  expect_error(rmse(1:3, 1:2), class = "rdsc_error_input")
})

# Independent O(n^2) reference implementation of DeLong's method.
delong_reference <- function(y, s) {
  pos <- s[y == 1]
  neg <- s[y == 0]
  psi <- outer(pos, neg, function(a, b) (a > b) + 0.5 * (a == b))
  v10 <- rowMeans(psi)
  v01 <- colMeans(psi)
  list(auc = mean(psi),
       se = sqrt(var(v10) / length(pos) + var(v01) / length(neg)))
}

test_that("AUC and DeLong SE match a brute-force implementation", {
  set.seed(10)
  y <- rbinom(150, 1, 0.4)
  s <- round(rnorm(150, mean = y), 1) # rounding creates ties
  ref <- delong_reference(y, s)
  ci <- auc_ci(y, s, transform = "none")
  expect_equal(auc(y, s), ref$auc)
  expect_equal(ci$se, ref$se)
  expect_equal(c(ci$lower, ci$upper),
               ref$auc + c(-1, 1) * qnorm(0.975) * ref$se)
})

test_that("AUC equals the normalised Mann-Whitney statistic", {
  set.seed(2)
  y <- rep(0:1, each = 30)
  s <- rnorm(60, y)
  w <- wilcox.test(s[y == 1], s[y == 0])$statistic
  expect_equal(auc(y, s), unname(w) / (30 * 30))
})

test_that("AUC handles factors, logicals and edge cases", {
  y <- factor(c("no", "no", "yes", "yes"), levels = c("no", "yes"))
  expect_equal(auc(y, c(0.1, 0.2, 0.8, 0.9)), 1)
  expect_equal(auc(c(FALSE, FALSE, TRUE, TRUE), c(0.9, 0.8, 0.2, 0.1)), 0)
  expect_equal(auc(c(0, 0, 1, 1), c(1, 1, 1, 1)), 0.5)
  ci <- auc_ci(c(0, 0, 1, 1), c(0.1, 0.2, 0.8, 0.9))
  expect_true(ci$lower >= 0 && ci$upper <= 1)
  expect_error(auc(c(0, 1, 1), c(1, 2, 3)), class = "rdsc_error_input")
  expect_error(auc(c(0, 2, 1, 0), 1:4), class = "rdsc_error_input")
})

test_that("logit-scale AUC interval stays inside [0, 1]", {
  set.seed(4)
  y <- rep(0:1, each = 15)
  s <- rnorm(30, 2 * y)
  expect_gt(auc(y, s), 0.85)
  ci <- auc_ci(y, s)
  expect_true(ci$lower > 0 && ci$upper < 1)
  expect_true(ci$lower < ci$auc && ci$auc < ci$upper)
})

test_that("scoring rules are correct", {
  y <- c(0, 0, 1, 1)
  p <- c(0.1, 0.4, 0.35, 0.8)
  expect_equal(brier_score(y, p), mean((p - y)^2))
  expect_equal(log_loss(y, p),
               -mean(c(log(0.9), log(0.6), log(0.35), log(0.8))))
  expect_true(is.finite(log_loss(c(0, 1), c(1, 0))))
  expect_error(brier_score(y, c(0.1, 0.2, 0.3, 1.2)),
               class = "rdsc_error_input")
})

test_that("calibration recovers intercept 0 and slope 1 for true risks", {
  set.seed(1)
  p <- runif(5000, 0.05, 0.95)
  y <- rbinom(5000, 1, p)
  cal <- calibration(y, p)
  expect_equal(cal$parameter, c("intercept", "slope"))
  expect_true(cal$lower[1] < 0 && 0 < cal$upper[1])
  expect_true(cal$lower[2] < 1 && 1 < cal$upper[2])
})

test_that("calibration slope is below one for overconfident predictions", {
  set.seed(1)
  lp <- rnorm(4000)
  y <- rbinom(4000, 1, plogis(lp))
  cal <- calibration(y, plogis(2 * lp))
  expect_equal(cal$estimate[2], 0.5, tolerance = 0.1)
})

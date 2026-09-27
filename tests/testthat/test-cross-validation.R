fit_lm <- function(d) lm(mpg ~ wt + hp, data = d)

test_that("leave-one-out CV matches the closed-form PRESS statistic", {
  cv <- cross_validate(mtcars, fit_lm, outcome = "mpg", metric = mae,
                       k = nrow(mtcars), seed = 1)
  m <- fit_lm(mtcars)
  loo_resid <- residuals(m) / (1 - hatvalues(m))
  expect_equal(sort(abs(mtcars$mpg - cv$predictions$pred)),
               unname(sort(abs(loo_resid))))
})

test_that("pooled LOOCV RMSE is the square root of PRESS / n", {
  cv <- cross_validate(mtcars, fit_lm, outcome = "mpg", metric = rmse,
                       k = nrow(mtcars), seed = 1)
  m <- fit_lm(mtcars)
  press <- sum((residuals(m) / (1 - hatvalues(m)))^2)
  expect_equal(cv$pooled_estimate, sqrt(press / nrow(mtcars)))
  # Fold-averaged RMSE with single-observation folds is the MAE instead.
  expect_equal(cv$estimate, mean(abs(residuals(m) / (1 - hatvalues(m)))))
  expect_lt(cv$estimate, cv$pooled_estimate)
})

test_that("pooled and fold-averaged estimates agree for MAE with equal folds", {
  cv <- cross_validate(mtcars, fit_lm, "mpg", metric = mae, k = 4, seed = 1)
  expect_equal(cv$estimate, cv$pooled_estimate)
})

test_that("cross_validate returns per-fold results and OOF predictions", {
  cv <- cross_validate(mtcars, fit_lm, "mpg", k = 4, repeats = 3, seed = 2)
  expect_s3_class(cv, "rdsc_cv")
  expect_equal(nrow(cv$folds), 12)
  expect_equal(nrow(cv$predictions), 3 * 32)
  expect_false(anyNA(cv$predictions$pred))
  expect_equal(cv$estimate, mean(cv$folds$metric))
  expect_output(print(cv), "4-fold")
})

test_that("cross_validate is reproducible", {
  a <- cross_validate(mtcars, fit_lm, "mpg", k = 5, seed = 1)
  b <- cross_validate(mtcars, fit_lm, "mpg", k = 5, seed = 1)
  expect_identical(a$folds, b$folds)
})

test_that("glm models are predicted on the response scale by default", {
  fit_glm <- function(d) glm(am ~ wt, data = d, family = binomial)
  cv <- cross_validate(mtcars, fit_glm, "am", metric = brier_score, k = 4,
                       strata = "am", seed = 1)
  expect_true(all(cv$predictions$pred >= 0 & cv$predictions$pred <= 1))
})

test_that("grouped CV accepts a column name", {
  d <- transform(mtcars, g = rep(1:8, each = 4))
  cv <- cross_validate(d, fit_lm, "mpg", k = 4, groups = "g", seed = 1)
  by_group <- tapply(cv$predictions$fold, d$g, function(v) length(unique(v)))
  expect_true(all(by_group == 1))
})

test_that("cross_validate validates its inputs", {
  expect_error(cross_validate(1:3, fit_lm, "mpg"), class = "rdsc_error_input")
  expect_error(cross_validate(mtcars, fit_lm, "nope"),
               class = "rdsc_error_input")
  expect_error(cross_validate(mtcars, fit_lm, "mpg", strata = "nope"),
               class = "rdsc_error_input")
  expect_error(cross_validate(mtcars, fit_lm, "mpg", strata = 1:3),
               class = "rdsc_error_input")
})

test_that("nested CV selects per outer fold and reports naive optimism", {
  cands <- list(
    wt = function(d) lm(mpg ~ wt, data = d),
    wt_hp = function(d) lm(mpg ~ wt + hp, data = d),
    all = function(d) lm(mpg ~ ., data = d)
  )
  ncv <- nested_cv(mtcars, cands, "mpg", outer_k = 4, inner_k = 4, seed = 3)
  expect_s3_class(ncv, "rdsc_nested_cv")
  expect_equal(nrow(ncv$outer), 4)
  expect_equal(nrow(ncv$inner), 12)
  expect_true(all(ncv$outer$selected %in% names(cands)))
  expect_equal(ncv$naive_estimate, min(ncv$candidate_cv))
  expect_false(anyNA(ncv$predictions$pred))
  expect_equal(ncv$pooled_estimate,
               rmse(ncv$predictions$truth, ncv$predictions$pred))
  # With a single candidate, nested and naive CV coincide.
  one <- nested_cv(mtcars, cands["wt"], "mpg", outer_k = 4, inner_k = 4,
                   seed = 3)
  expect_equal(one$estimate, one$naive_estimate)
  expect_equal(one$pooled_estimate, one$naive_pooled_estimate)
  expect_output(print(ncv), "Nested")
})

test_that("nested CV removes the selection bias of naive CV", {
  skip_on_cran()
  # Pure-noise outcome: every candidate has true RMSE = sd(y) ~ 1, but naive
  # CV picks whichever noise predictor happened to look best.
  opt <- vapply(1:15, function(i) {
    d <- as.data.frame(with_seed(i, matrix(rnorm(40 * 21), 40)))
    names(d) <- c("y", paste0("x", 1:20))
    cands <- lapply(paste0("x", 1:20), function(v) {
      f <- stats::as.formula(paste("y ~", v))
      function(tr) lm(f, data = tr)
    })
    names(cands) <- paste0("x", 1:20)
    r <- nested_cv(d, cands, "y", outer_k = 5, inner_k = 5, seed = i)
    r$estimate - r$naive_estimate
  }, numeric(1))
  expect_gt(mean(opt), 0)
})

test_that("nested_cv validates candidates", {
  expect_error(nested_cv(mtcars, list(function(d) lm(mpg ~ wt, d)), "mpg"),
               class = "rdsc_error_input")
  expect_error(nested_cv(mtcars, list(a = 1), "mpg"),
               class = "rdsc_error_input")
})

#' Simulate data with a known ground truth
#'
#' Generators for linear and logistic regression data with correlated
#' Gaussian predictors. The true coefficients are stored in the `"truth"`
#' attribute so that estimators can be evaluated against them.
#'
#' @details
#' Predictors follow a multivariate normal distribution with unit variances
#' and first-order autoregressive correlation
#' \eqn{\mathrm{Cor}(X_j, X_k) = \rho^{|j-k|}}. For `simulate_linear()`,
#' \eqn{Y = \beta_0 + X\beta + \varepsilon} with
#' \eqn{\varepsilon \sim N(0, \sigma^2)}. For `simulate_logistic()`,
#' \eqn{Y \sim \mathrm{Bernoulli}\{\mathrm{expit}(\beta_0 + X\beta)\}}.
#'
#' @param n Number of observations.
#' @param beta Numeric vector of slope coefficients; its length sets the
#'   number of predictors.
#' @param intercept Intercept \eqn{\beta_0}.
#' @param sigma Residual standard deviation (`simulate_linear()` only).
#' @param rho AR(1) correlation between adjacent predictors, in (-1, 1).
#' @param seed Optional seed; the caller's RNG state is restored on exit.
#' @return A data frame with outcome `y` and predictors `x1`, ..., `xp`, with
#'   attribute `truth` (a list of the generating parameters). For
#'   `simulate_logistic()` the true event probabilities are in column `p`.
#' @examples
#' d <- simulate_linear(200, beta = c(1, 0.5, 0), sigma = 2, seed = 1)
#' coef(lm(y ~ ., data = d))
#' attr(d, "truth")$beta
#'
#' d2 <- simulate_logistic(500, beta = c(1, -1), intercept = -1, seed = 1)
#' mean(d2$y)
#' @name simulate_data
NULL

#' @rdname simulate_data
#' @export
simulate_linear <- function(n, beta, intercept = 0, sigma = 1, rho = 0,
                            seed = NULL) {
  n <- check_count(n, "n")
  check_scalar_number(sigma, "sigma", lower = 0)
  x_and_lp <- simulate_design(n, beta, intercept, rho, seed)
  with_seed(if (!is.null(seed)) seed + 1L, {
    y <- x_and_lp$lp + stats::rnorm(n, sd = sigma)
  })
  out <- data.frame(y = y, x_and_lp$x)
  attr(out, "truth") <- list(intercept = intercept, beta = beta,
                             sigma = sigma, rho = rho)
  out
}

#' @rdname simulate_data
#' @export
simulate_logistic <- function(n, beta, intercept = 0, rho = 0, seed = NULL) {
  n <- check_count(n, "n")
  x_and_lp <- simulate_design(n, beta, intercept, rho, seed)
  p <- stats::plogis(x_and_lp$lp)
  with_seed(if (!is.null(seed)) seed + 1L, {
    y <- stats::rbinom(n, 1L, p)
  })
  out <- data.frame(y = y, x_and_lp$x, p = p)
  attr(out, "truth") <- list(intercept = intercept, beta = beta, rho = rho)
  out
}

#' @rdname simulate_data
#' @param p Number of predictors (`ar1_cor()` only).
#' @export
ar1_cor <- function(p, rho) {
  p <- check_count(p, "p")
  check_scalar_number(rho, "rho", lower = -1, upper = 1,
                      lower_open = TRUE, upper_open = TRUE)
  rho^abs(outer(seq_len(p), seq_len(p), "-"))
}

simulate_design <- function(n, beta, intercept, rho, seed) {
  check_numeric(beta, "beta")
  check_scalar_number(intercept, "intercept")
  check_seed(seed)
  p <- length(beta)
  sigma_x <- ar1_cor(p, rho)
  x <- with_seed(seed, {
    z <- matrix(stats::rnorm(n * p), n, p)
    z %*% chol(sigma_x)
  })
  colnames(x) <- paste0("x", seq_len(p))
  list(x = as.data.frame(x), lp = as.vector(intercept + x %*% beta))
}

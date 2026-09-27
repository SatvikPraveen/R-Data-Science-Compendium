#' Regression performance metrics
#'
#' Root mean squared error, mean absolute error and out-of-sample
#' \eqn{R^2 = 1 - \sum (y - \hat y)^2 / \sum (y - \bar y)^2}.
#'
#' @param truth Numeric vector of observed outcomes.
#' @param pred Numeric vector of predictions.
#' @return A single number.
#' @examples
#' rmse(c(1, 2, 3), c(1.1, 1.9, 3.2))
#' mae(c(1, 2, 3), c(1.1, 1.9, 3.2))
#' r_squared(c(1, 2, 3), c(1.1, 1.9, 3.2))
#' @name regression_metrics
NULL

#' @rdname regression_metrics
#' @export
rmse <- function(truth, pred) {
  check_pair(truth, pred)
  sqrt(mean((truth - pred)^2))
}

#' @rdname regression_metrics
#' @export
mae <- function(truth, pred) {
  check_pair(truth, pred)
  mean(abs(truth - pred))
}

#' @rdname regression_metrics
#' @export
r_squared <- function(truth, pred) {
  check_pair(truth, pred)
  1 - sum((truth - pred)^2) / sum((truth - mean(truth))^2)
}

check_pair <- function(truth, pred) {
  check_numeric(truth, "truth")
  check_numeric(pred, "pred")
  check_same_length(truth, pred, "truth", "pred")
}

#' Area under the ROC curve with DeLong confidence interval
#'
#' `auc()` returns the empirical area under the receiver operating
#' characteristic curve, equal to the Mann-Whitney probability
#' \eqn{P(S_1 > S_0) + \frac12 P(S_1 = S_0)} that a random positive case
#' scores higher than a random negative one. `auc_ci()` adds the
#' nonparametric variance estimate of DeLong, DeLong and Clarke-Pearson
#' (1988), computed in \eqn{O(n \log n)} via the midrank algorithm of Sun and
#' Xu (2014).
#'
#' @details
#' By default the confidence interval is formed on the logit scale and
#' back-transformed, which keeps it inside \[0, 1\] and improves coverage
#' when the AUC is near its bounds (Pepe 2003, Section 5.2). Use
#' `transform = "none"` for the untransformed Wald interval.
#'
#' @param truth Binary outcome: 0/1, logical, or a two-level factor (the
#'   second level is the positive class).
#' @param score Numeric risk score; higher values indicate the positive
#'   class.
#' @param level Confidence level.
#' @param transform `"logit"` or `"none"`.
#'
#' @return `auc()`: a single number. `auc_ci()`: a one-row data frame with
#'   `auc`, `se`, `lower`, `upper`, `level`, `n_pos` and `n_neg`.
#'
#' @references
#' DeLong, E. R., DeLong, D. M. and Clarke-Pearson, D. L. (1988). Comparing
#' the areas under two or more correlated receiver operating characteristic
#' curves: a nonparametric approach. *Biometrics*, 44(3), 837--845.
#' \doi{10.2307/2531595}
#'
#' Sun, X. and Xu, W. (2014). Fast implementation of DeLong's algorithm for
#' comparing the areas under correlated receiver operating characteristic
#' curves. *IEEE Signal Processing Letters*, 21(11), 1389--1393.
#' \doi{10.1109/LSP.2014.2337313}
#'
#' Pepe, M. S. (2003). *The Statistical Evaluation of Medical Tests for
#' Classification and Prediction*. Oxford University Press.
#'
#' @examples
#' fit <- glm(am ~ wt, data = mtcars, family = binomial)
#' p <- fitted(fit)
#' auc(mtcars$am, p)
#' auc_ci(mtcars$am, p)
#' @name auc
NULL

#' @rdname auc
#' @export
auc <- function(truth, score) {
  auc_components(truth, score)$auc
}

#' @rdname auc
#' @export
auc_ci <- function(truth, score, level = 0.95,
                   transform = c("logit", "none")) {
  check_level(level)
  transform <- match.arg(transform)
  comp <- auc_components(truth, score)
  a <- comp$auc
  se <- sqrt(stats::var(comp$v10) / comp$n_pos +
               stats::var(comp$v01) / comp$n_neg)
  z <- stats::qnorm((1 + level) / 2)
  if (transform == "logit" && a > 0 && a < 1) {
    se_logit <- se / (a * (1 - a))
    lim <- stats::plogis(stats::qlogis(a) + c(-1, 1) * z * se_logit)
  } else {
    lim <- pmin(pmax(a + c(-1, 1) * z * se, 0), 1)
  }
  data.frame(auc = a, se = se, lower = lim[1L], upper = lim[2L],
             level = level, n_pos = comp$n_pos, n_neg = comp$n_neg)
}

auc_components <- function(truth, score) {
  y <- check_binary(truth, "truth")
  check_numeric(score, "score")
  check_same_length(y, score, "truth", "score")
  pos <- score[y == 1L]
  neg <- score[y == 0L]
  m <- length(pos)
  n <- length(neg)
  if (m < 2L || n < 2L) {
    abort_input("Need at least two positive and two negative cases.")
  }
  r_all <- rank(c(pos, neg))
  r_pos <- rank(pos)
  r_neg <- rank(neg)
  v10 <- (r_all[seq_len(m)] - r_pos) / n
  v01 <- 1 - (r_all[m + seq_len(n)] - r_neg) / m
  list(auc = mean(v10), v10 = v10, v01 = v01, n_pos = m, n_neg = n)
}

#' Probabilistic classification metrics
#'
#' The Brier score (mean squared error of predicted probabilities) and the
#' log loss (mean negative Bernoulli log-likelihood). Both are strictly
#' proper scoring rules, so they reward calibrated as well as discriminating
#' predictions (Gneiting and Raftery 2007).
#'
#' @inheritParams auc
#' @param prob Predicted probabilities of the positive class.
#' @param eps Probabilities are truncated to `[eps, 1 - eps]` before taking
#'   logarithms.
#' @return A single number.
#' @references
#' Gneiting, T. and Raftery, A. E. (2007). Strictly proper scoring rules,
#' prediction, and estimation. *Journal of the American Statistical
#' Association*, 102(477), 359--378. \doi{10.1198/016214506000001437}
#' @examples
#' y <- c(0, 0, 1, 1)
#' p <- c(0.1, 0.4, 0.35, 0.8)
#' brier_score(y, p)
#' log_loss(y, p)
#' @name scoring_rules
NULL

#' @rdname scoring_rules
#' @export
brier_score <- function(truth, prob) {
  y <- check_prob_pair(truth, prob)
  mean((prob - y)^2)
}

#' @rdname scoring_rules
#' @export
log_loss <- function(truth, prob, eps = 1e-15) {
  y <- check_prob_pair(truth, prob)
  p <- pmin(pmax(prob, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

check_prob_pair <- function(truth, prob) {
  y <- check_binary(truth, "truth")
  check_numeric(prob, "prob")
  check_same_length(y, prob, "truth", "prob")
  if (any(prob < 0 | prob > 1)) {
    abort_input("`prob` must lie in [0, 1].")
  }
  y
}

#' Calibration intercept and slope
#'
#' Assesses the calibration of predicted probabilities by logistic
#' recalibration (Cox 1958). The **calibration slope** is the coefficient
#' of \eqn{\mathrm{logit}(\hat p)} in a logistic regression of the outcome;
#' values below 1 indicate predictions that are too extreme (overfitting).
#' **Calibration-in-the-large** is the intercept of a logistic regression
#' with \eqn{\mathrm{logit}(\hat p)} as an offset; positive values indicate
#' systematic under-prediction. Perfect calibration corresponds to an
#' intercept of 0 and a slope of 1 (Van Calster et al. 2019).
#'
#' @inheritParams scoring_rules
#' @param level Confidence level for the Wald intervals.
#' @return A data frame with rows `intercept` and `slope` and columns
#'   `estimate`, `se`, `lower`, `upper`.
#' @references
#' Cox, D. R. (1958). Two further applications of a model for binary
#' regression. *Biometrika*, 45(3/4), 562--565. \doi{10.2307/2333203}
#'
#' Van Calster, B., McLernon, D. J., van Smeden, M., Wynants, L. and
#' Steyerberg, E. W. (2019). Calibration: the Achilles heel of predictive
#' analytics. *BMC Medicine*, 17, 230. \doi{10.1186/s12916-019-1466-7}
#' @examples
#' set.seed(1)
#' p <- runif(500)
#' y <- rbinom(500, 1, p)
#' calibration(y, p) # close to intercept 0, slope 1
#' @export
calibration <- function(truth, prob, level = 0.95, eps = 1e-8) {
  y <- check_prob_pair(truth, prob)
  check_level(level)
  lp <- stats::qlogis(pmin(pmax(prob, eps), 1 - eps))
  z <- stats::qnorm((1 + level) / 2)
  d <- data.frame(y = y, lp = lp)
  fit_int <- stats::glm(y ~ 1, offset = lp, family = stats::binomial(),
                        data = d)
  fit_slope <- stats::glm(y ~ lp, family = stats::binomial(), data = d)
  est <- c(stats::coef(fit_int)[[1L]], stats::coef(fit_slope)[[2L]])
  se <- c(sqrt(diag(stats::vcov(fit_int)))[[1L]],
          sqrt(diag(stats::vcov(fit_slope)))[[2L]])
  data.frame(parameter = c("intercept", "slope"), estimate = est, se = se,
             lower = est - z * se, upper = est + z * se,
             stringsAsFactors = FALSE)
}

#' Nonparametric bootstrap confidence intervals
#'
#' Computes percentile, basic, normal-approximation and bias-corrected and
#' accelerated (BCa) confidence intervals for a scalar statistic using the
#' ordinary nonparametric bootstrap.
#'
#' @details
#' Observations (elements of a vector, or rows of a data frame or matrix) are
#' resampled with replacement `R` times. Interval endpoints for the
#' percentile, basic and BCa methods are obtained from the ordered bootstrap
#' replicates using interpolation on the normal quantile scale (Davison and
#' Hinkley 1997, eq. 5.8). With the same seed, resampling indices, replicates
#' and the percentile, basic and normal intervals are identical to those of
#' [boot::boot()] and [boot::boot.ci()].
#'
#' For the BCa interval the bias-correction is
#' \eqn{\hat z_0 = \Phi^{-1}\{\#(\hat\theta^*_r < \hat\theta)/R\}}
#' and the acceleration is estimated from the jackknife influence values
#' \eqn{L_i = (n-1)(\bar\theta_{(\cdot)} - \hat\theta_{(i)})} as
#' \eqn{\hat a = \sum L_i^3 / \{6 (\sum L_i^2)^{3/2}\}} (Efron 1987).
#'
#' Choose `R` so that \eqn{(R + 1)\alpha/2} is an integer (e.g. 999, 1999,
#' 9999) to avoid interpolation for the percentile and basic intervals.
#'
#' @param data A numeric vector, data frame or matrix. Resampling is over
#'   elements (vectors) or rows (data frames and matrices).
#' @param statistic A function taking resampled `data` and returning a single
#'   number.
#' @param R Number of bootstrap replicates.
#' @param level Confidence level in (0, 1).
#' @param type Character vector of interval types to compute; any of
#'   `"percentile"`, `"basic"`, `"normal"` and `"bca"`.
#' @param seed Optional seed for reproducibility. The caller's RNG state is
#'   restored on exit.
#'
#' @return An object of class `rdsc_boot_ci`: a list with elements
#'   `estimate` (the statistic on the original data), `bias` and `se`
#'   (bootstrap estimates), `intervals` (a data frame with columns `type`,
#'   `level`, `lower`, `upper`), `replicates`, `acceleration`,
#'   `bias_correction`, `R` and `n`.
#'
#' @references
#' Efron, B. (1987). Better bootstrap confidence intervals. *Journal of the
#' American Statistical Association*, 82(397), 171--185.
#' \doi{10.1080/01621459.1987.10478410}
#'
#' Davison, A. C. and Hinkley, D. V. (1997). *Bootstrap Methods and Their
#' Application*. Cambridge University Press. \doi{10.1017/CBO9780511802843}
#'
#' @seealso [perm_test()] for permutation tests.
#' @examples
#' set.seed(1)
#' x <- rexp(40)
#' ci <- boot_ci(x, mean, R = 999, seed = 42)
#' ci
#' as.data.frame(ci)
#'
#' # A statistic of a data frame: the correlation between two columns
#' ci_cor <- boot_ci(mtcars, function(d) cor(d$mpg, d$wt), R = 999, seed = 1)
#' ci_cor$intervals
#' @export
boot_ci <- function(data, statistic, R = 1999L, level = 0.95,
                    type = c("percentile", "basic", "normal", "bca"),
                    seed = NULL) {
  check_function(statistic)
  R <- check_count(R, "R", min = 2L)
  check_level(level)
  type <- unique(match.arg(type, several.ok = TRUE))
  check_seed(seed)
  n <- n_obs(data)
  if (n < 2L) {
    abort_input("`data` must contain at least two observations.")
  }

  t0 <- eval_statistic(statistic, data, "the original data")

  t_star <- with_seed(seed, {
    idx <- sample.int(n, n * R, replace = TRUE)
    dim(idx) <- c(R, n)
    vapply(seq_len(R), function(r) {
      out <- statistic(take_obs(data, idx[r, ]))
      if (length(out) != 1L || !is.numeric(out)) NA_real_ else as.numeric(out)
    }, numeric(1))
  })

  finite <- is.finite(t_star)
  if (!all(finite)) {
    warning(sprintf(
      "%d of %d bootstrap replicates were not finite and were dropped.",
      sum(!finite), R
    ), call. = FALSE)
  }
  t_ok <- t_star[finite]
  if (length(t_ok) < 2L) {
    stop("Fewer than two finite bootstrap replicates; cannot form intervals.",
         call. = FALSE)
  }

  alpha <- (1 + c(-level, level)) / 2
  bias <- mean(t_ok) - t0
  se <- stats::sd(t_ok)

  z0 <- NA_real_
  acc <- NA_real_
  rows <- list()

  if ("percentile" %in% type) {
    rows$percentile <- norm_inter(t_ok, alpha)
  }
  if ("basic" %in% type) {
    rows$basic <- 2 * t0 - rev(norm_inter(t_ok, alpha))
  }
  if ("normal" %in% type) {
    rows$normal <- t0 - bias + stats::qnorm(alpha) * se
  }
  if ("bca" %in% type) {
    z0 <- stats::qnorm(sum(t_ok < t0) / length(t_ok))
    acc <- jackknife_acceleration(data, statistic)
    if (!is.finite(z0) || !is.finite(acc)) {
      warning("BCa interval is undefined (degenerate bootstrap or jackknife ",
              "distribution); returning NA.", call. = FALSE)
      rows$bca <- c(NA_real_, NA_real_)
    } else {
      z_alpha <- stats::qnorm(alpha)
      adj <- stats::pnorm(z0 + (z0 + z_alpha) / (1 - acc * (z0 + z_alpha)))
      rows$bca <- norm_inter(t_ok, adj)
    }
  }

  rows <- rows[type]
  intervals <- data.frame(
    type = names(rows),
    level = level,
    lower = vapply(rows, `[`, numeric(1), 1L),
    upper = vapply(rows, `[`, numeric(1), 2L),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      estimate = t0,
      bias = bias,
      se = se,
      intervals = intervals,
      replicates = t_star,
      acceleration = acc,
      bias_correction = z0,
      R = R,
      n = n,
      call = match.call()
    ),
    class = "rdsc_boot_ci"
  )
}

eval_statistic <- function(statistic, data, what) {
  out <- statistic(data)
  if (!is.numeric(out) || length(out) != 1L || !is.finite(out)) {
    abort_input("`statistic` must return a single finite number on %s.", what)
  }
  as.numeric(out)
}

# Quantiles of bootstrap replicates using interpolation on the normal scale
# (Davison & Hinkley, 1997, eq. 5.8). Matches boot:::norm.inter().
norm_inter <- function(t, alpha) {
  t <- sort(t)
  R <- length(t)
  rk <- (R + 1) * alpha
  k <- trunc(rk)
  out <- numeric(length(alpha))
  for (j in seq_along(alpha)) {
    if (k[j] == rk[j] && k[j] >= 1 && k[j] <= R) {
      out[j] <- t[k[j]]
    } else if (k[j] <= 0) {
      out[j] <- t[1L]
    } else if (k[j] >= R) {
      out[j] <- t[R]
    } else {
      z <- stats::qnorm(alpha[j])
      z_lo <- stats::qnorm(k[j] / (R + 1))
      z_hi <- stats::qnorm((k[j] + 1) / (R + 1))
      out[j] <- t[k[j]] + (z - z_lo) / (z_hi - z_lo) * (t[k[j] + 1L] - t[k[j]])
    }
  }
  if (any(k < 1 | k >= R)) {
    warning("Extreme order statistics used as interval endpoints; ",
            "increase `R`.", call. = FALSE)
  }
  out
}

#' Jackknife influence values
#'
#' Leave-one-out (jackknife) estimates of the empirical influence values of
#' a statistic, \eqn{L_i = (n - 1)(\bar\theta_{(\cdot)} - \hat\theta_{(i)})}.
#'
#' @inheritParams boot_ci
#' @return A numeric vector of length `n`.
#' @examples
#' jackknife_influence(c(1, 4, 2, 8, 5), mean)
#' @export
jackknife_influence <- function(data, statistic) {
  check_function(statistic)
  n <- n_obs(data)
  if (n < 2L) {
    abort_input("`data` must contain at least two observations.")
  }
  theta_i <- vapply(seq_len(n), function(i) {
    as.numeric(statistic(take_obs(data, -i)))
  }, numeric(1))
  (n - 1) * (mean(theta_i) - theta_i)
}

jackknife_acceleration <- function(data, statistic) {
  L <- jackknife_influence(data, statistic)
  denom <- sum(L^2)
  if (!is.finite(denom) || denom == 0) {
    return(NA_real_)
  }
  sum(L^3) / (6 * denom^1.5)
}

#' @export
print.rdsc_boot_ci <- function(x, digits = max(3L, getOption("digits") - 3L),
                               ...) {
  cat("Nonparametric bootstrap confidence intervals\n")
  cat(sprintf("n = %d, R = %d replicates\n\n", x$n, x$R))
  cat(sprintf("Estimate: %s  (bias %s, SE %s)\n\n",
              format(x$estimate, digits = digits),
              format(x$bias, digits = digits),
              format(x$se, digits = digits)))
  iv <- x$intervals
  iv$lower <- format(iv$lower, digits = digits)
  iv$upper <- format(iv$upper, digits = digits)
  iv$level <- sprintf("%g%%", 100 * iv$level)
  print(iv, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.rdsc_boot_ci <- function(x, ...) {
  cbind(estimate = x$estimate, x$intervals)
}

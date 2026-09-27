#' Standardised mean differences with exact confidence intervals
#'
#' Cohen's \eqn{d} and Hedges' bias-corrected \eqn{g} for one-sample, paired
#' and independent two-sample designs, with confidence intervals obtained by
#' inverting the noncentral \eqn{t} distribution.
#'
#' @details
#' **Independent samples.** \eqn{d = (\bar x - \bar y) / s_p}, where
#' \eqn{s_p} is the pooled standard deviation. Under normality with equal
#' variances, \eqn{t = d / \sqrt{1/n_1 + 1/n_2}} follows a noncentral
#' \eqn{t} distribution with \eqn{n_1 + n_2 - 2} degrees of freedom and
#' noncentrality \eqn{\delta / \sqrt{1/n_1 + 1/n_2}}.
#'
#' **One-sample and paired.** \eqn{d = (\bar d - \mu_0) / s_d}, computed on
#' the differences for paired data (often written \eqn{d_z}). Here
#' \eqn{t = d\sqrt{n}} with \eqn{n - 1} degrees of freedom.
#'
#' **Confidence interval.** The limits for the noncentrality parameter
#' \eqn{\lambda} solve \eqn{P(T_{\nu,\lambda} \le t_{obs}) = 1 - \alpha/2}
#' and \eqn{\alpha/2} (Steiger and Fouladi 1997; Cumming and Finch 2001) and
#' are rescaled to the \eqn{d} metric. The interval is exact under the model
#' assumptions, unlike the common large-sample approximation, which is
#' reported as `se` for reference.
#'
#' **Hedges' g** multiplies \eqn{d} (and its limits) by the exact
#' small-sample correction
#' \eqn{J(\nu) = \Gamma(\nu/2) / \{\sqrt{\nu/2}\,\Gamma((\nu-1)/2)\}}
#' (Hedges 1981), which makes \eqn{g} unbiased for \eqn{\delta}.
#'
#' Note that [stats::pt()] is only accurate for noncentrality parameters up
#' to about 37.6 in absolute value; larger observed \eqn{t} statistics yield a
#' warning.
#'
#' @param x Numeric vector.
#' @param y Optional numeric vector: the second group, or the paired
#'   observations if `paired = TRUE`.
#' @param paired Logical; compute the standardised mean of the paired
#'   differences \eqn{d_z}.
#' @param mu Null value for one-sample and paired designs.
#' @param level Confidence level.
#'
#' @return An object of class `rdsc_effect_size`: a list with `estimate`,
#'   `lower`, `upper`, `level`, `se` (large-sample approximation), `df`,
#'   `t` (the corresponding \eqn{t} statistic), `n` and `design`.
#'
#' @references
#' Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect
#' size and related estimators. *Journal of Educational Statistics*, 6(2),
#' 107--128. \doi{10.3102/10769986006002107}
#'
#' Steiger, J. H. and Fouladi, R. T. (1997). Noncentrality interval
#' estimation and the evaluation of statistical models. In L. L. Harlow,
#' S. A. Mulaik and J. H. Steiger (Eds.), *What If There Were No Significance
#' Tests?* (pp. 221--257). Erlbaum.
#'
#' Cumming, G. and Finch, S. (2001). A primer on the understanding, use, and
#' calculation of confidence intervals that are based on central and
#' noncentral distributions. *Educational and Psychological Measurement*,
#' 61(4), 532--574. \doi{10.1177/0013164401614002}
#'
#' @examples
#' x <- sleep$extra[sleep$group == 1]
#' y <- sleep$extra[sleep$group == 2]
#' cohens_d(x, y)
#' hedges_g(x, y)
#' cohens_d(x, y, paired = TRUE)
#' @name effect_sizes
NULL

#' @rdname effect_sizes
#' @export
cohens_d <- function(x, y = NULL, paired = FALSE, mu = 0, level = 0.95) {
  standardised_difference(x, y, paired = paired, mu = mu, level = level,
                          correct = FALSE)
}

#' @rdname effect_sizes
#' @export
hedges_g <- function(x, y = NULL, paired = FALSE, mu = 0, level = 0.95) {
  standardised_difference(x, y, paired = paired, mu = mu, level = level,
                          correct = TRUE)
}

#' @rdname effect_sizes
#' @param df Degrees of freedom (`hedges_correction()` only).
#' @export
hedges_correction <- function(df) {
  check_numeric(df, "df")
  if (any(df <= 1)) {
    abort_input("`df` must be greater than 1.")
  }
  exp(lgamma(df / 2) - log(sqrt(df / 2)) - lgamma((df - 1) / 2))
}

standardised_difference <- function(x, y, paired, mu, level, correct) {
  check_numeric(x, "x", min_length = 2L)
  if (!is.null(y)) check_numeric(y, "y", min_length = 2L)
  check_scalar_number(mu, "mu")
  check_level(level)
  if (paired && is.null(y)) {
    abort_input("`y` must be supplied when `paired = TRUE`.")
  }

  if (!is.null(y) && !paired) {
    if (mu != 0) {
      abort_input("`mu` is only used for one-sample and paired designs.")
    }
    n1 <- length(x)
    n2 <- length(y)
    df <- n1 + n2 - 2
    sp <- sqrt(((n1 - 1) * stats::var(x) + (n2 - 1) * stats::var(y)) / df)
    d <- (mean(x) - mean(y)) / sp
    scale <- sqrt(1 / n1 + 1 / n2)
    se <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
    n <- c(n1, n2)
    design <- "independent samples"
  } else {
    diffs <- if (paired) {
      check_same_length(x, y, "x", "y")
      x - y
    } else {
      x
    }
    n <- length(diffs)
    df <- n - 1
    d <- (mean(diffs) - mu) / stats::sd(diffs)
    scale <- 1 / sqrt(n)
    se <- sqrt(1 / n + d^2 / (2 * n))
    design <- if (paired) "paired (d_z)" else "one sample"
  }

  if (!is.finite(d)) {
    stop("Effect size is undefined (zero standard deviation).", call. = FALSE)
  }

  t_obs <- d / scale
  alpha <- 1 - level
  ncp <- ncp_interval(t_obs, df, alpha)
  lower <- ncp[1L] * scale
  upper <- ncp[2L] * scale

  if (correct) {
    j <- hedges_correction(df)
    d <- d * j
    lower <- lower * j
    upper <- upper * j
    se <- se * j
  }

  structure(
    list(
      estimate = d,
      lower = lower,
      upper = upper,
      level = level,
      se = se,
      df = df,
      t = t_obs,
      n = n,
      design = design,
      measure = if (correct) "Hedges' g" else "Cohen's d"
    ),
    class = "rdsc_effect_size"
  )
}

# Confidence limits for the noncentrality parameter of a t distribution.
ncp_interval <- function(t_obs, df, alpha) {
  if (abs(t_obs) > 37.62) {
    warning("|t| > 37.62: noncentral t probabilities may be inaccurate.",
            call. = FALSE)
  }
  solve_ncp <- function(p) {
    f <- function(ncp) stats::pt(t_obs, df, ncp) - p
    stats::uniroot(f, interval = c(t_obs - 2, t_obs + 2),
                   extendInt = "downX", tol = 1e-10)$root
  }
  c(solve_ncp(1 - alpha / 2), solve_ncp(alpha / 2))
}

#' @export
print.rdsc_effect_size <- function(x,
                                   digits = max(3L, getOption("digits") - 3L),
                                   ...) {
  cat(sprintf("%s (%s)\n", x$measure, x$design))
  cat(sprintf("  estimate = %s, %g%% CI [%s, %s]\n",
              format(x$estimate, digits = digits), 100 * x$level,
              format(x$lower, digits = digits),
              format(x$upper, digits = digits)))
  cat(sprintf("  df = %g, n = %s\n", x$df, paste(x$n, collapse = " + ")))
  invisible(x)
}

#' @export
as.data.frame.rdsc_effect_size <- function(x, ...) {
  data.frame(measure = x$measure, design = x$design, estimate = x$estimate,
             lower = x$lower, upper = x$upper, level = x$level, se = x$se,
             df = x$df, stringsAsFactors = FALSE)
}

#' Permutation and randomisation tests
#'
#' Two-sample permutation tests and one-sample / paired sign-flip
#' (randomisation) tests for an arbitrary test statistic, with exact
#' enumeration when the reference set is small and valid Monte Carlo
#' p-values otherwise.
#'
#' @details
#' **Two-sample test** (`y` supplied, `paired = FALSE`): under the null
#' hypothesis that `x` and `y` come from the same distribution the group
#' labels are exchangeable. The reference distribution is obtained by
#' reallocating the pooled observations to groups of the original sizes.
#'
#' **One-sample / paired test** (`y` missing, or `paired = TRUE`): the test is
#' applied to `d = x - mu` (or `d = x - y - mu`). Under the null hypothesis
#' that `d` is symmetric about zero the signs are exchangeable, and the
#' reference distribution is obtained by sign-flipping.
#'
#' **Exact vs Monte Carlo.** If the number of distinct rearrangements is at
#' most `max_exact` (or `exact = TRUE`), all rearrangements are enumerated and
#' the p-value is exact. Otherwise `R` random rearrangements are drawn and the
#' p-value is computed as \eqn{(b + 1) / (R + 1)}, where \eqn{b} is the number
#' of rearrangements at least as extreme as the observed one. Unlike
#' \eqn{b / R}, this estimator never returns zero and gives a test whose
#' type I error rate does not exceed the nominal level (Phipson and Smyth
#' 2010).
#'
#' **Two-sided alternative.** "At least as extreme" is defined as
#' \eqn{|T^*| \ge |T_{obs}|}, which is appropriate for statistics whose null
#' distribution is centred at zero (such as the default difference in means).
#'
#' Comparisons are made with a small relative tolerance so that
#' rearrangements whose statistic equals the observed one up to
#' floating-point error are counted as ties.
#'
#' @param x Numeric vector of observations (first sample).
#' @param y Optional numeric vector: the second sample, or the paired
#'   observations if `paired = TRUE`.
#' @param statistic Test statistic. For two-sample tests a function
#'   `function(x, y)`; for one-sample and paired tests a function `function(d)`.
#'   Defaults to the difference in means and the mean respectively.
#' @param paired Logical; if `TRUE` perform a paired sign-flip test on
#'   `x - y`.
#' @param mu Null value for the one-sample / paired location.
#' @param alternative One of `"two.sided"`, `"greater"` or `"less"`.
#' @param R Number of Monte Carlo rearrangements.
#' @param exact `NULL` (default) to enumerate exactly when feasible, `TRUE` to
#'   force enumeration or `FALSE` to force Monte Carlo.
#' @param max_exact Largest reference set that is enumerated when
#'   `exact = NULL`.
#' @param seed Optional seed; the caller's RNG state is restored on exit.
#'
#' @return An object of class `htest` with additional elements
#'   `null_distribution` (the rearrangement statistics), `exact` (logical)
#'   and `p.value.mcse` (the Monte Carlo standard error of the p-value, zero
#'   for exact tests).
#'
#' @references
#' Phipson, B. and Smyth, G. K. (2010). Permutation p-values should never be
#' zero: calculating exact p-values when permutations are randomly drawn.
#' *Statistical Applications in Genetics and Molecular Biology*, 9(1),
#' Article 39. \doi{10.2202/1544-6115.1585}
#'
#' Good, P. I. (2005). *Permutation, Parametric, and Bootstrap Tests of
#' Hypotheses* (3rd ed.). Springer. \doi{10.1007/b138696}
#'
#' @examples
#' # Exact two-sample test (choose(12, 6) = 924 rearrangements)
#' x <- c(19.1, 22.4, 20.8, 24.0, 21.7, 23.3)
#' y <- c(18.2, 17.9, 20.1, 19.4, 16.8, 18.8)
#' perm_test(x, y)
#'
#' # Monte Carlo test of a difference in medians
#' set.seed(2)
#' a <- rexp(40)
#' b <- rexp(35, rate = 0.6)
#' perm_test(a, b, statistic = function(x, y) median(x) - median(y),
#'           R = 1999, seed = 10)
#'
#' # Paired sign-flip test
#' perm_test(sleep$extra[1:10], sleep$extra[11:20], paired = TRUE)
#' @export
perm_test <- function(x, y = NULL, statistic = NULL, paired = FALSE, mu = 0,
                      alternative = c("two.sided", "greater", "less"),
                      R = 9999L, exact = NULL, max_exact = 10000L,
                      seed = NULL) {
  alternative <- match.arg(alternative)
  check_numeric(x, "x", min_length = 1L)
  if (!is.null(y)) check_numeric(y, "y", min_length = 1L)
  R <- check_count(R, "R")
  max_exact <- check_count(max_exact, "max_exact")
  check_seed(seed)
  check_scalar_number(mu, "mu")
  if (!is.null(exact) && !(isTRUE(exact) || isFALSE(exact))) {
    abort_input("`exact` must be NULL, TRUE or FALSE.")
  }
  data_name <- paste(deparse1(substitute(x)),
                     if (!is.null(y)) paste("and", deparse1(substitute(y))))

  two_sample <- !is.null(y) && !paired
  if (paired && is.null(y)) {
    abort_input("`y` must be supplied when `paired = TRUE`.")
  }

  if (two_sample) {
    if (mu != 0) {
      abort_input("`mu` is only used for one-sample and paired tests.")
    }
    statistic <- statistic %||% function(x, y) mean(x) - mean(y)
    check_function(statistic)
    pooled <- c(x, y)
    nx <- length(x)
    n <- length(pooled)
    n_ref <- choose(n, nx)
    t_obs <- statistic(x, y)
    method <- "two-sample permutation test"
    compute <- function(in_x) statistic(pooled[in_x], pooled[-in_x])
    enumerate <- function() {
      apply(utils::combn(n, nx), 2L, compute)
    }
    draw <- function() {
      vapply(seq_len(R), function(r) compute(sample.int(n, nx)), numeric(1))
    }
  } else {
    if (paired) {
      check_same_length(x, y, "x", "y")
      d <- x - y - mu
      method <- "paired sign-flip randomisation test"
    } else {
      d <- x - mu
      method <- "one-sample sign-flip randomisation test"
    }
    statistic <- statistic %||% mean
    check_function(statistic)
    n <- length(d)
    n_ref <- 2^n
    t_obs <- statistic(d)
    compute <- function(signs) statistic(signs * d)
    enumerate <- function() {
      codes <- seq.int(0, n_ref - 1)
      vapply(codes, function(code) {
        bits <- as.integer(intToBits(code))[seq_len(n)]
        compute(1 - 2 * bits)
      }, numeric(1))
    }
    draw <- function() {
      vapply(seq_len(R), function(r) {
        compute(sample(c(-1, 1), n, replace = TRUE))
      }, numeric(1))
    }
  }

  if (!is.numeric(t_obs) || length(t_obs) != 1L || !is.finite(t_obs)) {
    abort_input("`statistic` must return a single finite number.")
  }

  use_exact <- if (is.null(exact)) n_ref <= max_exact else exact
  if (use_exact && n_ref > 1e6) {
    abort_input(
      "Exact enumeration of %.0f rearrangements is infeasible; use `exact = FALSE`.",
      n_ref
    )
  }

  t_null <- if (use_exact) enumerate() else with_seed(seed, draw())

  tol <- sqrt(.Machine$double.eps) * max(1, abs(t_obs))
  extreme <- switch(
    alternative,
    two.sided = abs(t_null) >= abs(t_obs) - tol,
    greater = t_null >= t_obs - tol,
    less = t_null <= t_obs + tol
  )
  b <- sum(extreme)
  if (use_exact) {
    p <- b / length(t_null)
    mcse <- 0
  } else {
    p <- (b + 1) / (R + 1)
    mcse <- sqrt(p * (1 - p) / R)
  }

  structure(
    list(
      statistic = c(T = t_obs),
      parameter = c(rearrangements = if (use_exact) n_ref else R),
      p.value = p,
      null.value = stats::setNames(mu, if (two_sample) "location shift" else "location"),
      alternative = alternative,
      method = paste0(if (use_exact) "Exact " else "Monte Carlo ", method),
      data.name = data_name,
      null_distribution = t_null,
      exact = use_exact,
      p.value.mcse = mcse
    ),
    class = "htest"
  )
}

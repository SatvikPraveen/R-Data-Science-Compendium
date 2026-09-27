#' Assign observations to cross-validation folds
#'
#' Creates random, stratified or grouped \eqn{k}-fold partitions.
#'
#' @details
#' * **Random**: observations are randomly permuted and dealt into `k` folds
#'   whose sizes differ by at most one.
#' * **Stratified** (`strata`): the dealing is done within each stratum, so
#'   every fold has approximately the same distribution of `strata`. Numeric
#'   `strata` with more than `n_bins` distinct values are first binned at
#'   their quantiles, which is useful for regression outcomes.
#' * **Grouped** (`groups`): all observations from the same group (e.g.
#'   patient, site or time block) are placed in the same fold, preventing
#'   leakage between correlated observations. Groups are dealt into folds in
#'   decreasing order of size to balance fold sizes.
#'
#' @param n Number of observations.
#' @param k Number of folds.
#' @param strata Optional vector of length `n` to stratify on.
#' @param groups Optional vector of length `n` of group labels.
#' @param n_bins Number of quantile bins for numeric `strata`.
#' @param seed Optional seed; the caller's RNG state is restored on exit.
#' @return An integer vector of length `n` with fold labels in `1:k`.
#' @examples
#' y <- rep(c(0, 1), c(90, 10))
#' f <- make_folds(length(y), k = 5, strata = y, seed = 1)
#' table(f, y) # each fold holds two of the ten positives
#' @export
make_folds <- function(n, k = 10L, strata = NULL, groups = NULL,
                       n_bins = 4L, seed = NULL) {
  n <- check_count(n, "n", min = 2L)
  k <- check_count(k, "k", min = 2L)
  n_bins <- check_count(n_bins, "n_bins", min = 2L)
  check_seed(seed)
  if (!is.null(strata) && !is.null(groups)) {
    abort_input("Supply at most one of `strata` and `groups`.")
  }
  with_seed(seed, {
    if (!is.null(groups)) {
      if (length(groups) != n) abort_input("`groups` must have length `n`.")
      if (anyNA(groups)) abort_input("`groups` must not contain NA.")
      g <- as.character(groups)
      sizes <- table(g)
      if (length(sizes) < k) {
        abort_input("Need at least `k` = %d distinct groups (found %d).",
                    k, length(sizes))
      }
      # Random tie-breaking, then greedy assignment of the largest groups to
      # the currently smallest fold.
      ord <- names(sizes)[order(-as.vector(sizes), stats::runif(length(sizes)))]
      load <- numeric(k)
      assign_to <- stats::setNames(integer(length(ord)), ord)
      for (lab in ord) {
        smallest <- which(load == min(load))
        target <- smallest[sample.int(length(smallest), 1L)]
        assign_to[lab] <- target
        load[target] <- load[target] + sizes[[lab]]
      }
      unname(assign_to[g])
    } else {
      if (is.null(strata)) {
        strata <- rep(1L, n)
      } else if (length(strata) != n) {
        abort_input("`strata` must have length `n`.")
      }
      if (anyNA(strata)) abort_input("`strata` must not contain NA.")
      if (is.numeric(strata) && length(unique(strata)) > n_bins) {
        breaks <- unique(stats::quantile(strata, seq(0, 1, length.out = n_bins + 1L)))
        strata <- cut(strata, breaks, include.lowest = TRUE)
      }
      folds <- integer(n)
      offset <- 0L
      for (s in split(seq_len(n), strata, drop = TRUE)) {
        idx <- s[sample.int(length(s))]
        folds[idx] <- ((seq_along(idx) - 1L + offset) %% k) + 1L
        offset <- (offset + length(idx)) %% k
      }
      folds
    }
  })
}

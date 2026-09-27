#' Repeated k-fold cross-validation
#'
#' Estimates the out-of-sample performance of a modelling procedure by
#' (repeated, optionally stratified or grouped) \eqn{k}-fold
#' cross-validation.
#'
#' @details
#' The entire modelling procedure is passed as `fit`, so any data-dependent
#' preprocessing (scaling, imputation, feature selection, tuning) performed
#' inside `fit` is re-estimated within every training fold. Performing such
#' steps on the full data before cross-validation leaks information and
#' biases the estimate optimistically (Hastie, Tibshirani and Friedman 2009,
#' Section 7.10.2).
#'
#' The reported `se` is the naive standard error
#' \eqn{\mathrm{sd}(\text{fold metrics}) / \sqrt{kR}}, which treats fold
#' estimates as independent. Because training sets overlap it typically
#' **underestimates** the true sampling variability (Bengio and Grandvalet
#' 2004; Bates, Hastie and Tibshirani 2024); interpret it as a lower bound.
#'
#' @param data A data frame.
#' @param fit A function `function(train)` returning a fitted model.
#' @param outcome Name of the outcome column in `data`.
#' @param metric A function `function(truth, pred)` returning a number, such
#'   as [rmse()], [mae()], [auc()], [brier_score()] or [log_loss()].
#' @param predict_fun A function `function(model, newdata)` returning
#'   predictions. The default calls [stats::predict()] with
#'   `type = "response"` when the model inherits from `glm`.
#' @param k Number of folds.
#' @param repeats Number of repetitions with different random partitions.
#' @param strata,groups Optional stratification or grouping: a column name or
#'   a vector of length `nrow(data)`; see [make_folds()].
#' @param seed Optional seed; the caller's RNG state is restored on exit.
#'
#' @return An object of class `rdsc_cv` with elements `estimate`, `se`,
#'   `folds` (per-fold metrics), `predictions` (out-of-fold predictions for
#'   every repeat), `k` and `repeats`.
#'
#' @references
#' Hastie, T., Tibshirani, R. and Friedman, J. (2009). *The Elements of
#' Statistical Learning* (2nd ed.). Springer.
#' \doi{10.1007/978-0-387-84858-7}
#'
#' Bengio, Y. and Grandvalet, Y. (2004). No unbiased estimator of the
#' variance of k-fold cross-validation. *Journal of Machine Learning
#' Research*, 5, 1089--1105.
#'
#' Bates, S., Hastie, T. and Tibshirani, R. (2024). Cross-validation: what
#' does it estimate and how well does it do it? *Journal of the American
#' Statistical Association*, 119(546), 1434--1445.
#' \doi{10.1080/01621459.2023.2197686}
#'
#' @seealso [nested_cv()] for honest evaluation of procedures that involve
#'   model selection.
#' @examples
#' cv <- cross_validate(mtcars, fit = function(d) lm(mpg ~ wt + hp, data = d),
#'                      outcome = "mpg", k = 5, repeats = 3, seed = 1)
#' cv
#' @export
cross_validate <- function(data, fit, outcome, metric = rmse,
                           predict_fun = default_predict, k = 10L,
                           repeats = 1L, strata = NULL, groups = NULL,
                           seed = NULL) {
  check_cv_inputs(data, fit, outcome, metric, predict_fun)
  k <- check_count(k, "k", min = 2L)
  repeats <- check_count(repeats, "repeats")
  check_seed(seed)
  strata <- resolve_column(strata, data, "strata")
  groups <- resolve_column(groups, data, "groups")
  n <- nrow(data)

  with_seed(seed, {
    per_fold <- vector("list", repeats)
    preds <- vector("list", repeats)
    for (r in seq_len(repeats)) {
      folds <- make_folds(n, k, strata = strata, groups = groups)
      oof <- rep(NA_real_, n)
      scores <- numeric(k)
      for (j in seq_len(k)) {
        test <- which(folds == j)
        model <- fit(data[-test, , drop = FALSE])
        p <- as.numeric(predict_fun(model, data[test, , drop = FALSE]))
        oof[test] <- p
        scores[j] <- metric(data[[outcome]][test], p)
      }
      per_fold[[r]] <- data.frame(repeat_id = r, fold = seq_len(k),
                                  n_test = as.vector(table(factor(folds, seq_len(k)))),
                                  metric = scores)
      preds[[r]] <- data.frame(row = seq_len(n), repeat_id = r, fold = folds,
                               truth = data[[outcome]], pred = oof)
    }
    folds_df <- do.call(rbind, per_fold)
    structure(
      list(
        estimate = mean(folds_df$metric),
        se = stats::sd(folds_df$metric) / sqrt(nrow(folds_df)),
        folds = folds_df,
        predictions = do.call(rbind, preds),
        k = k,
        repeats = repeats,
        metric = deparse1(substitute(metric))
      ),
      class = "rdsc_cv"
    )
  })
}

#' Nested cross-validation for honest evaluation of model selection
#'
#' Estimates the generalisation performance of a procedure that *selects*
#' among candidate models (or hyperparameter values) by cross-validation.
#'
#' @details
#' Reporting the best cross-validated score among several candidates is
#' optimistically biased, because the same folds are used both to choose the
#' model and to estimate its performance (Varma and Simon 2006; Cawley and
#' Talbot 2010). Nested cross-validation removes this bias: within each outer
#' training set an inner cross-validation selects the candidate, which is
#' then refitted on the whole outer training set and scored on the untouched
#' outer test fold.
#'
#' For comparison the function also reports the **naive** estimate: the best
#' candidate's score from an ordinary cross-validation on the full data using
#' the outer partition. The difference `naive - nested` estimates the
#' optimism of the naive approach.
#'
#' @inheritParams cross_validate
#' @param candidates A named list of fitting functions `function(train)`.
#' @param outer_k,inner_k Number of outer and inner folds.
#' @param minimize Logical; `TRUE` if smaller values of `metric` are better.
#' @param strata Optional stratification for the outer and inner partitions:
#'   a column name or a vector of length `nrow(data)`; see [make_folds()].
#'
#' @return An object of class `rdsc_nested_cv` with elements `estimate` and
#'   `se` (nested), `naive_estimate`, `naive_choice`, `outer` (per-fold
#'   results including the selected candidate), and `inner` (all inner-loop
#'   scores).
#'
#' @references
#' Varma, S. and Simon, R. (2006). Bias in error estimation when using
#' cross-validation for model selection. *BMC Bioinformatics*, 7, 91.
#' \doi{10.1186/1471-2105-7-91}
#'
#' Cawley, G. C. and Talbot, N. L. C. (2010). On over-fitting in model
#' selection and subsequent selection bias in performance evaluation.
#' *Journal of Machine Learning Research*, 11, 2079--2107.
#'
#' @examples
#' cands <- list(
#'   wt      = function(d) lm(mpg ~ wt, data = d),
#'   wt_hp   = function(d) lm(mpg ~ wt + hp, data = d),
#'   all     = function(d) lm(mpg ~ ., data = d)
#' )
#' ncv <- nested_cv(mtcars, cands, outcome = "mpg", outer_k = 4,
#'                  inner_k = 4, seed = 3)
#' ncv
#' @export
nested_cv <- function(data, candidates, outcome, metric = rmse,
                      predict_fun = default_predict, outer_k = 5L,
                      inner_k = 5L, minimize = TRUE, strata = NULL,
                      seed = NULL) {
  if (!is.list(candidates) || length(candidates) < 1L ||
      is.null(names(candidates)) || !all(nzchar(names(candidates))) ||
      !all(vapply(candidates, is.function, logical(1)))) {
    abort_input("`candidates` must be a named list of functions.")
  }
  check_cv_inputs(data, candidates[[1L]], outcome, metric, predict_fun)
  outer_k <- check_count(outer_k, "outer_k", min = 2L)
  inner_k <- check_count(inner_k, "inner_k", min = 2L)
  check_seed(seed)
  strata <- resolve_column(strata, data, "strata")
  n <- nrow(data)
  best_of <- if (minimize) which.min else which.max

  score_fold <- function(fitter, train, test) {
    model <- fitter(train)
    metric(test[[outcome]], as.numeric(predict_fun(model, test)))
  }

  with_seed(seed, {
    outer <- make_folds(n, outer_k, strata = strata)
    outer_rows <- vector("list", outer_k)
    inner_rows <- vector("list", outer_k)
    naive_scores <- matrix(NA_real_, outer_k, length(candidates),
                           dimnames = list(NULL, names(candidates)))

    for (j in seq_len(outer_k)) {
      test_idx <- which(outer == j)
      train <- data[-test_idx, , drop = FALSE]
      test <- data[test_idx, , drop = FALSE]

      inner <- make_folds(nrow(train), inner_k,
                          strata = if (!is.null(strata)) strata[-test_idx])
      inner_scores <- vapply(candidates, function(fitter) {
        mean(vapply(seq_len(inner_k), function(i) {
          ii <- which(inner == i)
          score_fold(fitter, train[-ii, , drop = FALSE],
                     train[ii, , drop = FALSE])
        }, numeric(1)))
      }, numeric(1))
      chosen <- names(candidates)[best_of(inner_scores)]

      naive_scores[j, ] <- vapply(candidates, score_fold, numeric(1),
                                  train = train, test = test)

      outer_rows[[j]] <- data.frame(
        fold = j, n_test = length(test_idx), selected = chosen,
        inner_score = inner_scores[[chosen]],
        outer_score = naive_scores[j, chosen],
        stringsAsFactors = FALSE
      )
      inner_rows[[j]] <- data.frame(fold = j, candidate = names(candidates),
                                    score = unname(inner_scores),
                                    stringsAsFactors = FALSE)
    }
    outer_df <- do.call(rbind, outer_rows)
    naive_means <- colMeans(naive_scores)
    naive_choice <- names(candidates)[best_of(naive_means)]

    structure(
      list(
        estimate = mean(outer_df$outer_score),
        se = stats::sd(outer_df$outer_score) / sqrt(outer_k),
        naive_estimate = naive_means[[naive_choice]],
        naive_choice = naive_choice,
        candidate_cv = naive_means,
        outer = outer_df,
        inner = do.call(rbind, inner_rows),
        outer_k = outer_k,
        inner_k = inner_k,
        minimize = minimize
      ),
      class = "rdsc_nested_cv"
    )
  })
}

default_predict <- function(model, newdata) {
  if (inherits(model, "glm")) {
    stats::predict(model, newdata = newdata, type = "response")
  } else {
    stats::predict(model, newdata = newdata)
  }
}

check_cv_inputs <- function(data, fit, outcome, metric, predict_fun) {
  if (!is.data.frame(data)) abort_input("`data` must be a data frame.")
  check_function(fit, "fit")
  check_function(metric, "metric")
  check_function(predict_fun, "predict_fun")
  if (!is.character(outcome) || length(outcome) != 1L ||
      !outcome %in% names(data)) {
    abort_input("`outcome` must name a column of `data`.")
  }
  invisible(TRUE)
}

resolve_column <- function(x, data, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.character(x) && length(x) == 1L && nrow(data) != 1L) {
    if (!x %in% names(data)) {
      abort_input("`%s` column '%s' not found in `data`.", arg, x)
    }
    return(data[[x]])
  }
  if (length(x) != nrow(data)) {
    abort_input("`%s` must be a column name or have length nrow(data).", arg)
  }
  x
}

#' @export
print.rdsc_cv <- function(x, digits = 4L, ...) {
  cat(sprintf("%d-fold cross-validation, %d repeat(s)\n", x$k, x$repeats))
  cat(sprintf("  metric: %s\n", x$metric))
  cat(sprintf("  estimate = %s (naive SE %s)\n",
              format(x$estimate, digits = digits),
              format(x$se, digits = digits)))
  invisible(x)
}

#' @export
print.rdsc_nested_cv <- function(x, digits = 4L, ...) {
  cat(sprintf("Nested cross-validation (%d outer x %d inner folds)\n",
              x$outer_k, x$inner_k))
  cat(sprintf("  nested estimate = %s (SE %s)\n",
              format(x$estimate, digits = digits),
              format(x$se, digits = digits)))
  cat(sprintf("  naive estimate  = %s (best candidate: %s)\n",
              format(x$naive_estimate, digits = digits), x$naive_choice))
  sel <- table(x$outer$selected)
  cat("  selected in outer folds:",
      paste(sprintf("%s (%d)", names(sel), as.vector(sel)), collapse = ", "),
      "\n")
  invisible(x)
}

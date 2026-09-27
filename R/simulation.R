#' Run a reproducible Monte Carlo simulation study
#'
#' Executes the data-generating and analysis steps of a simulation study over
#' a grid of scenarios, recording one or more rows of results per replicate.
#' Designed around the ADEMP framework (Aims, Data-generating mechanisms,
#' Estimands, Methods, Performance measures) of Morris, White and Crowther
#' (2019).
#'
#' @details
#' **Reproducibility.** Each (scenario, replicate) pair is assigned its own
#' L'Ecuyer-CMRG random number stream derived from `seed` (see
#' [rng_streams()]). Results are therefore bit-for-bit identical between the
#' sequential and parallel backends, regardless of the number of workers or
#' the order in which tasks are scheduled, and adding scenarios does not
#' change the results of existing ones as long as their position is fixed.
#' The caller's RNG state is restored on exit.
#'
#' **Failures.** Errors raised by `generate` or `analyse` do not abort the
#' study. They are recorded in the `.error` column (and the first warning of
#' each replicate in `.warning`) so that non-convergence can be reported as
#' recommended by Morris et al. (2019).
#'
#' **Parallelism.** With `backend = "future"` replicates are evaluated with
#' [future.apply::future_lapply()] using whatever [future::plan()] the user
#' has set, e.g. `future::plan("multisession", workers = 4)`.
#'
#' @param generate A function returning one simulated data set. It is called
#'   with the columns of the current row of `scenarios` as named arguments.
#' @param analyse A function taking the simulated data set and returning
#'   either a named numeric vector (one result row) or a data frame (one row
#'   per method, for example).
#' @param n_sim Number of replicates per scenario.
#' @param scenarios Optional data frame of data-generating parameters, one row
#'   per scenario. Column names must match arguments of `generate`.
#' @param seed Master seed (required).
#' @param backend `"sequential"` or `"future"`.
#'
#' @return A data frame of class `rdsc_simulation` with the scenario
#'   parameters, `.scenario` and `.rep` indices, the columns returned by
#'   `analyse`, and `.error` / `.warning` diagnostics. Attributes record the
#'   seed, number of replicates, scenario variables, run time and session
#'   details.
#'
#' @references
#' Morris, T. P., White, I. R. and Crowther, M. J. (2019). Using simulation
#' studies to evaluate statistical methods. *Statistics in Medicine*, 38(11),
#' 2074--2102. \doi{10.1002/sim.8086}
#'
#' @seealso [sim_performance()] to summarise the results.
#' @examples
#' # Coverage of the t-interval for the mean of skewed data
#' gen <- function(n) rexp(n)
#' ana <- function(x) {
#'   tt <- t.test(x)
#'   c(estimate = mean(x), se = sd(x) / sqrt(length(x)),
#'     lower = tt$conf.int[1], upper = tt$conf.int[2])
#' }
#' res <- run_simulation(gen, ana, n_sim = 200,
#'                       scenarios = data.frame(n = c(10, 50)), seed = 2024)
#' sim_performance(res, true = 1)
#' @export
run_simulation <- function(generate, analyse, n_sim, scenarios = NULL, seed,
                           backend = c("sequential", "future")) {
  check_function(generate)
  check_function(analyse)
  n_sim <- check_count(n_sim, "n_sim")
  backend <- match.arg(backend)
  if (missing(seed) || is.null(seed)) {
    abort_input("`seed` is required for a reproducible simulation study.")
  }
  check_seed(seed)
  if (is.null(scenarios)) {
    scenarios <- data.frame(row.names = 1L)
  }
  if (!is.data.frame(scenarios) || nrow(scenarios) < 1L) {
    abort_input("`scenarios` must be a data frame with at least one row.")
  }
  scenario_vars <- names(scenarios)
  n_scen <- nrow(scenarios)

  tasks <- expand.grid(.rep = seq_len(n_sim), .scenario = seq_len(n_scen))
  streams <- rng_streams(nrow(tasks), seed = seed)

  run_one <- function(i) {
    s <- tasks$.scenario[i]
    params <- as.list(scenarios[s, , drop = FALSE])
    first_warning <- NA_character_
    out <- withCallingHandlers(
      tryCatch(
        with_stream(streams[[i]], function() {
          dat <- do.call(generate, params)
          analyse(dat)
        }),
        error = function(e) structure(conditionMessage(e), class = "sim_err")
      ),
      warning = function(w) {
        if (is.na(first_warning)) first_warning <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
    rows <- if (inherits(out, "sim_err")) {
      data.frame(.error = as.character(out), stringsAsFactors = FALSE)
    } else {
      res <- as_result_rows(out)
      res$.error <- rep(NA_character_, nrow(res))
      res
    }
    rows$.warning <- first_warning
    cbind(
      scenarios[rep(s, nrow(rows)), , drop = FALSE],
      .scenario = s,
      .rep = tasks$.rep[i],
      rows,
      row.names = NULL
    )
  }

  state <- get_rng_state()
  on.exit(restore_rng_state(state), add = TRUE)
  started <- Sys.time()
  pieces <- if (backend == "future") {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop("Package 'future.apply' is required for backend = \"future\".",
           call. = FALSE)
    }
    future.apply::future_lapply(seq_len(nrow(tasks)), run_one,
                                future.seed = NULL)
  } else {
    lapply(seq_len(nrow(tasks)), run_one)
  }
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  out <- rbind_fill(pieces)
  lead <- c(scenario_vars, ".scenario", ".rep")
  trail <- c(".error", ".warning")
  out <- out[, c(lead, setdiff(names(out), c(lead, trail)), trail),
             drop = FALSE]

  n_err <- sum(!is.na(out$.error))
  if (n_err > 0L) {
    warning(sprintf("%d of %d replicates failed; see the `.error` column.",
                    n_err, nrow(tasks)), call. = FALSE)
  }

  structure(
    out,
    class = c("rdsc_simulation", "data.frame"),
    seed = seed,
    n_sim = n_sim,
    scenario_vars = scenario_vars,
    elapsed = elapsed,
    backend = backend,
    session = list(
      r_version = R.version.string,
      package_version = as.character(utils::packageVersion(
        "RDataScienceCompendium"
      )),
      rng_kind = "L'Ecuyer-CMRG"
    )
  )
}

as_result_rows <- function(out) {
  if (is.data.frame(out)) {
    return(out)
  }
  if (is.list(out)) {
    out <- unlist(out)
  }
  if (is.atomic(out) && !is.null(names(out)) && all(nzchar(names(out)))) {
    return(as.data.frame(as.list(out), stringsAsFactors = FALSE))
  }
  stop("`analyse` must return a named vector or a data frame.", call. = FALSE)
}

# Row-bind data frames with possibly different columns, filling with NA.
rbind_fill <- function(dfs) {
  cols <- unique(unlist(lapply(dfs, names)))
  template <- list()
  for (df in dfs) {
    for (nm in names(df)) {
      if (is.null(template[[nm]])) template[[nm]] <- df[[nm]][0]
    }
  }
  filled <- lapply(dfs, function(df) {
    missing_cols <- setdiff(cols, names(df))
    for (nm in missing_cols) {
      df[[nm]] <- rep(template[[nm]][NA_integer_], nrow(df))
    }
    df[cols]
  })
  do.call(rbind, filled)
}

#' @export
print.rdsc_simulation <- function(x, ...) {
  n_err <- sum(!is.na(x$.error))
  cat(sprintf(
    "<rdsc_simulation> %d result rows from %d scenario(s) x %d replicates\n",
    nrow(x), length(unique(x$.scenario)), attr(x, "n_sim")
  ))
  cat(sprintf("seed = %s, backend = %s, elapsed = %.1f s, failures = %d\n\n",
              format(attr(x, "seed")), attr(x, "backend"),
              attr(x, "elapsed"), n_err))
  print(utils::head(as.data.frame(x), 6L))
  if (nrow(x) > 6L) cat("...\n")
  invisible(x)
}

#' Performance measures for a simulation study, with Monte Carlo SEs
#'
#' Summarises the output of [run_simulation()] (or any data frame of
#' replicate-level results) using the performance measures and Monte Carlo
#' standard error (MCSE) formulas of Morris, White and Crowther (2019,
#' Table 6).
#'
#' @details
#' Let \eqn{\hat\theta_i}, \eqn{i = 1, \dots, n}, be the estimates in a
#' group and \eqn{\theta} the true value. The measures reported (when the
#' required columns are supplied) are:
#'
#' | Measure | Definition | MCSE |
#' |---|---|---|
#' | `bias` | \eqn{\bar{\hat\theta} - \theta} | \eqn{\sqrt{S^2_{\hat\theta}/n}} |
#' | `empirical_se` | \eqn{S_{\hat\theta}} | \eqn{S_{\hat\theta}/\sqrt{2(n-1)}} |
#' | `mse` | \eqn{n^{-1}\sum(\hat\theta_i - \theta)^2} | see reference |
#' | `model_se` | \eqn{\sqrt{n^{-1}\sum \widehat{SE}_i^2}} | \eqn{\sqrt{\mathrm{Var}(\widehat{SE}^2)/(4n\,\mathrm{ModSE}^2)}} |
#' | `rel_error_model_se` | \eqn{100(\mathrm{ModSE}/\mathrm{EmpSE} - 1)} | see reference |
#' | `coverage` | \eqn{n^{-1}\sum 1(L_i \le \theta \le U_i)} | \eqn{\sqrt{C(1-C)/n}} |
#' | `be_coverage` | coverage of \eqn{\bar{\hat\theta}} (bias-eliminated) | \eqn{\sqrt{C(1-C)/n}} |
#' | `mean_width` | \eqn{n^{-1}\sum (U_i - L_i)} | \eqn{S_{U-L}/\sqrt n} |
#' | `rejection` | \eqn{n^{-1}\sum 1(p_i \le \alpha)} | \eqn{\sqrt{P(1-P)/n}} |
#'
#' Replicates with a missing estimate (e.g. failed fits) are excluded and
#' counted in `n_missing`.
#'
#' @param results A data frame of replicate-level results.
#' @param true The true value of the estimand: a single number, or the name
#'   of a column of `results` (for estimands that vary across scenarios).
#' @param estimate,se,lower,upper,p_value Names of the columns holding the
#'   point estimate, its model-based standard error, the confidence limits
#'   and the p-value. Measures whose columns are absent are skipped.
#' @param alpha Nominal significance level for `rejection`.
#' @param by Grouping columns. Defaults to the scenario variables recorded by
#'   [run_simulation()] plus a `method` column, if present.
#'
#' @return A data frame of class `rdsc_performance` with the grouping
#'   columns and `measure`, `estimate`, `mcse`, `n_rep` (replicates used) and
#'   `n_missing`. The count is not called `n` so that it cannot clash with a
#'   scenario variable of that name.
#'
#' @references
#' Morris, T. P., White, I. R. and Crowther, M. J. (2019). Using simulation
#' studies to evaluate statistical methods. *Statistics in Medicine*, 38(11),
#' 2074--2102. \doi{10.1002/sim.8086}
#'
#' @examples
#' set.seed(1)
#' est <- rnorm(1000, mean = 0.1, sd = 1)
#' res <- data.frame(estimate = est, se = 1,
#'                   lower = est - 1.96, upper = est + 1.96)
#' sim_performance(res, true = 0)
#' @export
sim_performance <- function(results, true, estimate = "estimate", se = "se",
                            lower = "lower", upper = "upper",
                            p_value = "p_value", alpha = 0.05, by = NULL) {
  if (!is.data.frame(results)) {
    abort_input("`results` must be a data frame.")
  }
  check_scalar_number(alpha, "alpha", lower = 0, upper = 1,
                      lower_open = TRUE, upper_open = TRUE)
  if (!estimate %in% names(results)) {
    abort_input("Column `%s` not found in `results`.", estimate)
  }
  if (is.character(true)) {
    if (length(true) != 1L || !true %in% names(results)) {
      abort_input("`true` must be a number or the name of a column.")
    }
    true_vals <- results[[true]]
  } else {
    check_scalar_number(true, "true")
    true_vals <- rep(true, nrow(results))
  }
  if (is.null(by)) {
    by <- c(attr(results, "scenario_vars"),
            intersect("method", names(results)))
  }
  missing_by <- setdiff(by, names(results))
  if (length(missing_by)) {
    abort_input("Grouping column(s) not found: %s.",
                paste(missing_by, collapse = ", "))
  }

  has <- function(col) !is.null(col) && col %in% names(results)
  results$.true <- true_vals
  groups <- if (length(by)) {
    split(results, results[by], drop = TRUE, sep = "\r")
  } else {
    list(results)
  }

  out <- lapply(groups, function(g) {
    th_all <- g[[estimate]]
    keep <- !is.na(th_all)
    g <- g[keep, , drop = FALSE]
    n <- nrow(g)
    th <- g[[estimate]]
    tv <- g$.true
    rows <- list()
    add <- function(measure, est, mcse) {
      rows[[length(rows) + 1L]] <<- data.frame(
        measure = measure, estimate = est, mcse = mcse,
        stringsAsFactors = FALSE
      )
    }
    if (n >= 2L) {
      err <- th - tv
      emp_se <- stats::sd(th)
      add("bias", mean(err), stats::sd(th) / sqrt(n))
      add("empirical_se", emp_se, emp_se / sqrt(2 * (n - 1)))
      mse <- mean(err^2)
      add("mse", mse, sqrt(sum((err^2 - mse)^2) / (n * (n - 1))))
      if (has(se)) {
        s2 <- g[[se]]^2
        mod_se <- sqrt(mean(s2))
        v_s2 <- stats::var(s2)
        add("model_se", mod_se, sqrt(v_s2 / (4 * n * mod_se^2)))
        ratio <- mod_se / emp_se
        add("rel_error_model_se", 100 * (ratio - 1),
            100 * ratio * sqrt(v_s2 / (4 * n * mod_se^4) + 1 / (2 * (n - 1))))
      }
      if (has(lower) && has(upper)) {
        lo <- g[[lower]]
        up <- g[[upper]]
        cover <- mean(lo <= tv & tv <= up, na.rm = TRUE)
        add("coverage", cover, sqrt(cover * (1 - cover) / n))
        be <- mean(lo <= mean(th) & mean(th) <= up, na.rm = TRUE)
        add("be_coverage", be, sqrt(be * (1 - be) / n))
        w <- up - lo
        add("mean_width", mean(w, na.rm = TRUE),
            stats::sd(w, na.rm = TRUE) / sqrt(n))
      }
      if (has(p_value)) {
        rej <- mean(g[[p_value]] <= alpha, na.rm = TRUE)
        add("rejection", rej, sqrt(rej * (1 - rej) / n))
      }
    }
    res <- if (length(rows)) {
      do.call(rbind, rows)
    } else {
      data.frame(measure = character(), estimate = numeric(),
                 mcse = numeric())
    }
    res$n_rep <- rep(n, nrow(res))
    res$n_missing <- rep(sum(!keep), nrow(res))
    if (length(by)) {
      key <- g[1L, by, drop = FALSE]
      if (n == 0L) key <- data.frame(lapply(key, function(v) v[NA_integer_]))
      res <- cbind(key[rep(1L, nrow(res)), , drop = FALSE], res,
                   row.names = NULL)
    }
    res
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  structure(out, class = c("rdsc_performance", "data.frame"), by = by)
}

#' @export
print.rdsc_performance <- function(x, digits = 3L, ...) {
  df <- as.data.frame(x)
  df$estimate <- sprintf("%.*f (%.*f)", digits, df$estimate, digits, df$mcse)
  names(df)[names(df) == "estimate"] <- "estimate (MCSE)"
  df$mcse <- NULL
  cat("Simulation performance measures (Morris et al., 2019)\n\n")
  print(df, row.names = FALSE)
  invisible(x)
}

#' Number of replicates needed for a target Monte Carlo standard error
#'
#' Planning formulas from Morris, White and Crowther (2019, Section 5.3).
#' For proportions (coverage, rejection rates),
#' \eqn{n = p(1-p)/\mathrm{MCSE}^2}; for bias,
#' \eqn{n = \sigma^2/\mathrm{MCSE}^2}, where \eqn{\sigma} is the anticipated
#' empirical standard error of the estimator.
#'
#' @param target_mcse Desired Monte Carlo standard error.
#' @param measure `"proportion"` (coverage, power, type I error) or `"bias"`.
#' @param p Anticipated proportion; `0.5` gives the worst case.
#' @param sd Anticipated empirical SE of the estimator (for `"bias"`).
#' @return The required number of replicates (rounded up).
#' @examples
#' # MCSE of 0.5 percentage points for coverage near 95%
#' sim_n_required(0.005, p = 0.95)
#' @export
sim_n_required <- function(target_mcse, measure = c("proportion", "bias"),
                           p = 0.5, sd = NULL) {
  measure <- match.arg(measure)
  check_scalar_number(target_mcse, "target_mcse", lower = 0,
                      lower_open = TRUE)
  n <- switch(
    measure,
    proportion = {
      check_scalar_number(p, "p", lower = 0, upper = 1)
      p * (1 - p) / target_mcse^2
    },
    bias = {
      if (is.null(sd)) abort_input("`sd` is required when measure = \"bias\".")
      check_scalar_number(sd, "sd", lower = 0, lower_open = TRUE)
      sd^2 / target_mcse^2
    }
  )
  # Round first so that e.g. 1900.0000000002 is not bumped to 1901.
  as.integer(ceiling(round(n, 8)))
}

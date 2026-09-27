# Study 2: optimism of naive cross-validation after model selection.
#
# ADEMP (Morris, White & Crowther, 2019)
#   Aims:        quantify the bias of (a) reporting the best candidate's
#                ordinary CV error and (b) nested CV, as estimators of the
#                prediction error of a "select by CV, then refit" procedure.
#   Data:        p iid N(0, 1) candidate predictors; y = beta * x1 + e,
#                e ~ N(0, 1); training size n in {50, 200}; p in {5, 25};
#                beta in {0 (pure noise), 0.5}. An independent test set of
#                10,000 observations gives the true error of each fitted
#                model.
#   Estimand:    RMSE of the final model on new data from the same
#                distribution (conditional prediction error).
#   Methods:     naive = min over candidates of 5-fold CV RMSE on the full
#                data; nested = 5 x 5 nested CV. Each is computed both by
#                averaging per-fold RMSEs ("fold") and from the pooled
#                out-of-fold predictions ("pooled").
#   Performance: bias (estimate - true RMSE), empirical SE, MSE; MCSE.
#   Replicates:  1000 per scenario.
#
# Nested CV estimates the error of the procedure trained on 80% of the data,
# so a small positive bias is expected for it by design. Averaging per-fold
# RMSEs adds a negative (Jensen) bias when test folds are small; the pooled
# variants isolate the two effects.

source(file.path("analysis", "scripts", "00-setup.R"))
backend <- setup_backend()

scenarios <- expand.grid(n = c(50L, 200L), p = c(5L, 25L), beta = c(0, 0.5))
n_sim <- if (quick) 20L else 1000L
n_test <- 10000L

# A closure, so that `n_test` travels with the function to parallel workers.
generate <- local({
  n_test <- n_test
  function(n, p, beta) {
    make <- function(m) {
      x <- matrix(stats::rnorm(m * p), m, p)
      d <- data.frame(y = beta * x[, 1L] + stats::rnorm(m), x)
      names(d) <- c("y", paste0("x", seq_len(p)))
      d
    }
    list(train = make(n), test = make(n_test))
  }
})

analyse <- function(dat) {
  preds <- setdiff(names(dat$train), "y")
  candidates <- lapply(preds, function(v) {
    f <- stats::as.formula(paste("y ~", v))
    function(tr) stats::lm(f, data = tr)
  })
  names(candidates) <- preds
  ncv <- RDataScienceCompendium::nested_cv(dat$train, candidates,
    outcome = "y", outer_k = 5L,
    inner_k = 5L
  )
  final <- candidates[[ncv$naive_choice]](dat$train)
  true_rmse <- RDataScienceCompendium::rmse(
    dat$test$y, stats::predict(final, newdata = dat$test)
  )
  data.frame(
    method = c("naive", "nested", "naive", "nested"),
    aggregation = c("fold", "fold", "pooled", "pooled"),
    estimate = c(ncv$naive_estimate, ncv$estimate,
                 ncv$naive_pooled_estimate, ncv$pooled_estimate),
    true_rmse = true_rmse,
    selected_true_predictor = ncv$naive_choice == "x1"
  )
}

sim <- run_simulation(generate, analyse,
  n_sim = n_sim,
  scenarios = scenarios, seed = 20250102L,
  backend = backend
)

perf <- sim_performance(sim,
  true = "true_rmse",
  by = c("n", "p", "beta", "method", "aggregation")
)

select_rate <- aggregate(
  selected_true_predictor ~ n + p + beta,
  data = sim[sim$method == "naive" & sim$aggregation == "fold", ], FUN = mean
)

write_results(perf, "cv-selection-performance.csv")
write_results(select_rate, "cv-selection-rates.csv")
write_provenance(sim, "cv-selection")
saveRDS(sim, file.path(results_dir, "cv-selection-replicates.rds"),
  compress = "xz"
)

gen <- function(n, mu = 0) rnorm(n, mean = mu)
ana <- function(x) {
  tt <- t.test(x)
  c(estimate = mean(x), se = sd(x) / sqrt(length(x)),
    lower = tt$conf.int[1], upper = tt$conf.int[2], p_value = tt$p.value)
}

test_that("run_simulation returns one row per replicate and scenario", {
  sc <- data.frame(n = c(10, 20), mu = c(0, 1))
  res <- run_simulation(gen, ana, n_sim = 25, scenarios = sc, seed = 1)
  expect_s3_class(res, "rdsc_simulation")
  expect_equal(nrow(res), 50)
  expect_identical(names(res)[1:4], c("n", "mu", ".scenario", ".rep"))
  expect_true(all(c("estimate", "se", ".error", ".warning") %in% names(res)))
  expect_identical(attr(res, "scenario_vars"), c("n", "mu"))
  expect_equal(attr(res, "n_sim"), 25)
  expect_output(print(res), "rdsc_simulation")
})

test_that("run_simulation is reproducible and does not disturb the RNG", {
  set.seed(123)
  state <- .Random.seed
  kind <- RNGkind()
  a <- run_simulation(gen, ana, 10, data.frame(n = 5), seed = 42)
  expect_identical(.Random.seed, state)
  expect_identical(RNGkind(), kind)
  b <- run_simulation(gen, ana, 10, data.frame(n = 5), seed = 42)
  expect_identical(a$estimate, b$estimate)
  c <- run_simulation(gen, ana, 10, data.frame(n = 5), seed = 43)
  expect_false(identical(a$estimate, c$estimate))
})

test_that("adding scenarios does not change earlier scenarios' results", {
  a <- run_simulation(gen, ana, 10, data.frame(n = 5), seed = 42)
  b <- run_simulation(gen, ana, 10, data.frame(n = c(5, 50)), seed = 42)
  expect_identical(a$estimate, b$estimate[b$.scenario == 1])
})

test_that("future backend gives identical results to sequential", {
  skip_if_not_installed("future.apply")
  skip_on_cran()
  seq_res <- run_simulation(gen, ana, 8, data.frame(n = c(5, 10)), seed = 9)
  old <- future::plan("sequential")
  on.exit(future::plan(old), add = TRUE)
  fut_res <- run_simulation(gen, ana, 8, data.frame(n = c(5, 10)), seed = 9,
                            backend = "future")
  expect_identical(seq_res$estimate, fut_res$estimate)
})

test_that("analyse may return several rows (methods) per replicate", {
  ana2 <- function(x) {
    data.frame(method = c("mean", "median"),
               estimate = c(mean(x), median(x)))
  }
  res <- run_simulation(gen, ana2, 5, data.frame(n = 11), seed = 1)
  expect_equal(nrow(res), 10)
  perf <- sim_performance(res, true = 0)
  expect_setequal(unique(perf$method), c("mean", "median"))
})

test_that("errors and warnings are captured, not thrown", {
  flaky <- function(x) {
    if (x[1] > 0) stop("boom")
    warning("careful")
    c(estimate = mean(x))
  }
  expect_warning(
    res <- run_simulation(function() rnorm(3), flaky, 40, seed = 1),
    "replicates failed"
  )
  expect_true(any(res$.error == "boom", na.rm = TRUE))
  expect_true(all(res$.warning[is.na(res$.error)] == "careful"))
  expect_true(all(is.na(res$estimate[!is.na(res$.error)])))
  perf <- sim_performance(res, true = 0)
  expect_equal(unique(perf$n_missing), sum(!is.na(res$.error)))
})

test_that("run_simulation validates its inputs", {
  expect_error(run_simulation(gen, ana, 5), class = "rdsc_error_input")
  expect_error(run_simulation(gen, ana, 0, seed = 1),
               class = "rdsc_error_input")
  expect_error(run_simulation("gen", ana, 5, seed = 1),
               class = "rdsc_error_input")
  expect_error(run_simulation(gen, ana, 5, scenarios = list(n = 1), seed = 1),
               class = "rdsc_error_input")
})

test_that("sim_performance matches hand-computed Morris et al. formulas", {
  est <- c(0.9, 1.3, 1.1, 0.7, 1.4, 1.0)
  se <- c(0.2, 0.3, 0.25, 0.2, 0.35, 0.3)
  res <- data.frame(estimate = est, se = se,
                    lower = est - 1.96 * se, upper = est + 1.96 * se,
                    p_value = c(0.01, 0.2, 0.04, 0.5, 0.03, 0.06))
  perf <- sim_performance(res, true = 1)
  get <- function(m) perf[perf$measure == m, c("estimate", "mcse")]
  n <- length(est)

  expect_equal(unname(unlist(get("bias"))),
               c(mean(est) - 1, sd(est) / sqrt(n)))
  expect_equal(unname(unlist(get("empirical_se"))),
               c(sd(est), sd(est) / sqrt(2 * (n - 1))))
  mse <- mean((est - 1)^2)
  expect_equal(unname(unlist(get("mse"))),
               c(mse, sqrt(sum(((est - 1)^2 - mse)^2) / (n * (n - 1)))))
  mod <- sqrt(mean(se^2))
  expect_equal(unname(unlist(get("model_se"))),
               c(mod, sqrt(var(se^2) / (4 * n * mod^2))))
  cover <- mean(res$lower <= 1 & 1 <= res$upper)
  expect_equal(unname(unlist(get("coverage"))),
               c(cover, sqrt(cover * (1 - cover) / n)))
  expect_equal(get("rejection")$estimate, 3 / 6)
  expect_equal(get("rel_error_model_se")$estimate,
               100 * (mod / sd(est) - 1))
  expect_output(print(perf), "MCSE")
})

test_that("sim_performance accepts a column of true values and groups", {
  res <- run_simulation(gen, ana, 200, data.frame(n = 20, mu = c(0, 2)),
                        seed = 5)
  res$truth <- res$mu
  perf <- sim_performance(res, true = "truth")
  cov <- perf[perf$measure == "coverage", ]
  expect_equal(nrow(cov), 2)
  expect_true(all(abs(cov$estimate - 0.95) < 4 * cov$mcse + 1e-9))
  bias <- perf[perf$measure == "bias", ]
  expect_true(all(abs(bias$estimate) < 4 * bias$mcse))
})

test_that("sim_performance skips measures whose columns are absent", {
  perf <- sim_performance(data.frame(estimate = rnorm(10)), true = 0)
  expect_setequal(perf$measure, c("bias", "empirical_se", "mse"))
})

test_that("sim_performance validates its inputs", {
  df <- data.frame(estimate = 1:3)
  expect_error(sim_performance(1:3, 0), class = "rdsc_error_input")
  expect_error(sim_performance(df, "nope"), class = "rdsc_error_input")
  expect_error(sim_performance(df, 0, estimate = "x"),
               class = "rdsc_error_input")
  expect_error(sim_performance(df, 0, by = "g"), class = "rdsc_error_input")
})

test_that("sim_n_required implements the planning formulas", {
  expect_equal(sim_n_required(0.005, p = 0.95), 1900L)
  expect_equal(sim_n_required(0.01), 2500L)
  expect_equal(sim_n_required(0.01, "bias", sd = 0.5), 2500L)
  expect_error(sim_n_required(0.01, "bias"), class = "rdsc_error_input")
})

test_that("the replicate count does not clash with a scenario variable `n`", {
  res <- run_simulation(gen, ana, 5, data.frame(n = c(5, 8)), seed = 1)
  perf <- sim_performance(res, true = 0)
  expect_false(anyDuplicated(names(perf)) > 0)
  expect_setequal(unique(perf$n), c(5, 8))
  expect_true(all(perf$n_rep == 5))
})

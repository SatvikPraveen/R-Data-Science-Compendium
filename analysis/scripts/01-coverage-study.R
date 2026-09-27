# Study 1: coverage of confidence intervals for the mean of skewed data.
#
# ADEMP (Morris, White & Crowther, 2019)
#   Aims:        compare the coverage and width of five 95% intervals for a
#                population mean under right skew and small samples.
#   Data:        X ~ LogNormal(0, sigma^2); n in {10, 20, 40, 80};
#                sigma in {0.25, 0.5, 1} (skewness 0.78, 1.75, 6.18).
#   Estimand:    theta = E[X] = exp(sigma^2 / 2).
#   Methods:     Student t; bootstrap percentile, basic, normal and BCa
#                (R = 1999 resamples).
#   Performance: coverage, bias-eliminated coverage, mean width; MCSE.
#   Replicates:  5000 per scenario, giving MCSE <= 0.31 percentage points
#                for coverage near 95% (sim_n_required(0.0031, p = 0.95)).

source(file.path("analysis", "scripts", "00-setup.R"))
backend <- setup_backend()

scenarios <- expand.grid(n = c(10L, 20L, 40L, 80L), sigma = c(0.25, 0.5, 1))
n_sim <- if (quick) 100L else 5000L

generate <- function(n, sigma) stats::rlnorm(n, meanlog = 0, sdlog = sigma)

analyse <- function(x) {
  tt <- stats::t.test(x)
  bs <- RDataScienceCompendium::boot_ci(
    x, mean, R = 1999L,
    type = c("percentile", "basic", "normal", "bca")
  )
  data.frame(
    method = c("t", bs$intervals$type),
    estimate = mean(x),
    se = c(stats::sd(x) / sqrt(length(x)), rep(bs$se, 4L)),
    lower = c(tt$conf.int[1L], bs$intervals$lower),
    upper = c(tt$conf.int[2L], bs$intervals$upper)
  )
}

sim <- run_simulation(generate, analyse, n_sim = n_sim,
                      scenarios = scenarios, seed = 20250101L,
                      backend = backend)
sim$theta <- exp(sim$sigma^2 / 2)

perf <- sim_performance(sim, true = "theta",
                        by = c("n", "sigma", "method"))

write_results(perf, "coverage-performance.csv")
write_provenance(sim, "coverage")
saveRDS(sim, file.path(results_dir, "coverage-replicates.rds"),
        compress = "xz")

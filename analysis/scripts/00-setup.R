# Shared configuration for the analysis pipeline.
#
# Every script is run from the repository root, e.g.
#   Rscript analysis/scripts/01-coverage-study.R
# and uses the *installed* version of the package (`make install`), so that
# parallel workers load exactly the same code.

suppressPackageStartupMessages({
  library(RDataScienceCompendium)
})

results_dir <- file.path("analysis", "results")
figures_dir <- file.path("analysis", "figures")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

# Number of parallel workers; override with e.g. RDSC_WORKERS=1 for a
# strictly sequential run. Results do not depend on this setting.
workers <- as.integer(Sys.getenv("RDSC_WORKERS",
                                 max(1L, parallel::detectCores() - 2L)))

# Start parallel workers (if available) and return the backend name to pass
# to run_simulation().
setup_backend <- function() {
  if (workers > 1L && requireNamespace("future.apply", quietly = TRUE)) {
    future::plan(future::multisession, workers = workers)
    "future"
  } else {
    "sequential"
  }
}

# Reduce replicate counts for a quick smoke test: RDSC_QUICK=1.
quick <- identical(Sys.getenv("RDSC_QUICK"), "1")

write_results <- function(df, name) {
  path <- file.path(results_dir, name)
  utils::write.csv(as.data.frame(df), path, row.names = FALSE)
  message("wrote ", path)
  invisible(path)
}

write_provenance <- function(sim, name) {
  info <- c(
    sprintf("study: %s", name),
    sprintf("date: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("seed: %s", attr(sim, "seed")),
    sprintf("n_sim: %d", attr(sim, "n_sim")),
    sprintf("rng: %s", attr(sim, "session")$rng_kind),
    sprintf("backend: %s (%d workers)", attr(sim, "backend"), workers),
    sprintf("elapsed_seconds: %.1f", attr(sim, "elapsed")),
    sprintf("failed_replicates: %d", sum(!is.na(sim$.error))),
    sprintf("package_version: %s", attr(sim, "session")$package_version),
    sprintf("r_version: %s", attr(sim, "session")$r_version),
    sprintf("platform: %s", R.version$platform)
  )
  writeLines(info, file.path(results_dir, paste0(name, "-provenance.txt")))
}

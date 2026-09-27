#!/usr/bin/env Rscript
# Smoke test for the tutorial modules, case studies and Shiny apps.
#
#   Rscript modules/check-modules.R            # everything
#   Rscript modules/check-modules.R scripts    # only the R scripts
#   Rscript modules/check-modules.R apps       # only the Shiny apps
#   Rscript modules/check-modules.R reports    # only the case studies
#
# Each item runs in a fresh R process inside a temporary copy of the
# repository, so nothing is written to the working tree:
#   * scripts: source() the file, then call every run_*_demo() function it
#     defines (non-interactively, with verbose = FALSE where supported);
#   * reports: render the R Markdown case study;
#   * apps:    build the app object and run its server once with
#              shiny::testServer().
# Exits with a non-zero status if anything fails.

args <- commandArgs(trailingOnly = TRUE)
what <- if (length(args)) args else c("scripts", "reports", "apps")

repo <- normalizePath(".")
if (!file.exists(file.path(repo, "modules", "check-modules.R"))) {
  stop("Run this from the repository root.", call. = FALSE)
}

work <- file.path(tempdir(), "rdsc-modules")
unlink(work, recursive = TRUE)
dir.create(work)
invisible(file.copy(file.path(repo, c("modules", "shiny-apps")), work, recursive = TRUE))

run_child <- function(code, wd, timeout = 900) {
  script <- tempfile(fileext = ".R")
  writeLines(code, script)
  out <- tempfile()
  status <- system2(
    file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)),
    stdout = out, stderr = out, timeout = timeout,
    env = paste0("R_LIBS_USER=", Sys.getenv("R_LIBS_USER"))
  )
  log <- readLines(out, warn = FALSE)
  list(ok = identical(as.integer(status), 0L), log = log)
}

in_dir <- function(dir, body) {
  c(sprintf("setwd(%s)", deparse(dir)),
    "grDevices::pdf(NULL)",
    "options(warn = 1, width = 100)",
    body)
}

items <- list()

if ("scripts" %in% what) {
  scripts <- list.files(file.path(work, "modules"), pattern = "\\.R$",
                        recursive = TRUE, full.names = TRUE)
  scripts <- scripts[!basename(scripts) %in% c("check-modules.R", "install-dependencies.R", "test-runner.R")]
  for (f in scripts) {
    items[[sub(paste0(work, "/"), "", f, fixed = TRUE)]] <- in_dir(dirname(f), c(
      sprintf("source(%s)", deparse(basename(f))),
      "demos <- grep('^run_.*_demo$', ls(), value = TRUE)",
      "for (d in demos) {",
      "  fn <- get(d)",
      "  a <- if ('verbose' %in% names(formals(fn))) list(verbose = FALSE) else list()",
      "  cat('>> ', d, '\\n', sep = '')",
      "  invisible(do.call(fn, a))",
      "}",
      "cat('demos run:', length(demos), '\\n')"
    ))
  }
}

if ("reports" %in% what) {
  reports <- list.files(file.path(work, "modules", "case-studies"),
                        pattern = "\\.Rmd$", full.names = TRUE)
  for (f in reports) {
    items[[sub(paste0(work, "/"), "", f, fixed = TRUE)]] <- in_dir(dirname(f), c(
      sprintf("rmarkdown::render(%s, quiet = TRUE, envir = new.env())",
              deparse(basename(f)))
    ))
  }
}

if ("apps" %in% what) {
  apps <- list.dirs(file.path(work, "shiny-apps"), recursive = FALSE)
  for (d in apps) {
    items[[sub(paste0(work, "/"), "", d, fixed = TRUE)]] <- in_dir(d, c(
      "app <- shiny::shinyAppDir('.')",
      "shiny::testServer(app, { session$flushReact() })",
      "cat('app built and server initialised\\n')"
    ))
  }
}

results <- data.frame(item = names(items), ok = NA, seconds = NA_real_)
for (i in seq_along(items)) {
  t0 <- Sys.time()
  r <- run_child(items[[i]], wd = work)
  results$ok[i] <- r$ok
  results$seconds[i] <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat(sprintf("[%s] %-60s %6.1fs\n", if (r$ok) "PASS" else "FAIL",
              results$item[i], results$seconds[i]))
  if (!r$ok) {
    cat(paste0("    ", utils::tail(r$log, 25)), sep = "\n")
  }
}

cat(sprintf("\n%d/%d passed\n", sum(results$ok), nrow(results)))
if (!all(results$ok)) quit(status = 1)

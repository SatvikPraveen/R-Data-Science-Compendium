#!/usr/bin/env Rscript
# Install everything needed to develop the package and reproduce the analysis.
#
#   Rscript setup.R
#
# Package dependencies are read from DESCRIPTION, so this list never drifts
# from what R CMD check uses. For a pinned, fully reproducible environment use
# the Docker image instead (see docker/Dockerfile).

repos <- getOption("repos")
if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) {
  repos <- c(CRAN = "https://cloud.r-project.org")
}

desc <- read.dcf("DESCRIPTION", fields = c("Imports", "Suggests"))
parse_deps <- function(x) {
  if (is.na(x)) return(character())
  deps <- trimws(strsplit(x, ",")[[1]])
  sub("\\s*\\(.*\\)$", "", deps)
}
base_pkgs <- rownames(installed.packages(priority = "base"))
pkg_deps <- setdiff(c(parse_deps(desc[, "Imports"]), parse_deps(desc[, "Suggests"])),
                    base_pkgs)

dev_tools <- c("roxygen2", "lintr", "covr", "pkgdown", "rcmdcheck", "styler")
analysis_deps <- c("rprojroot", "future", "future.apply", "rmarkdown", "knitr")

wanted <- unique(c(pkg_deps, dev_tools, analysis_deps))
missing <- setdiff(wanted, rownames(installed.packages()))

if (length(missing)) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = repos)
} else {
  message("All dependencies are already installed.")
}

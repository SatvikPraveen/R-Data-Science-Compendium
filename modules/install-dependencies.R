#!/usr/bin/env Rscript
# Install the packages used by the tutorial modules, case studies and Shiny
# apps. Run from the repository root:
#
#   Rscript modules/install-dependencies.R          # install what is missing
#   Rscript modules/install-dependencies.R --list   # only print the list
#
# The list is discovered from the code itself (library(), require(),
# requireNamespace() and pkg:: references), so it cannot drift out of date.
# Packages that are no longer on CRAN are reported, not installed.

legacy_dependencies <- function(root = ".") {
  files <- c(
    list.files(file.path(root, "modules"), pattern = "\\.(R|Rmd)$",
               recursive = TRUE, full.names = TRUE),
    list.files(file.path(root, "shiny-apps"), pattern = "\\.R$",
               recursive = TRUE, full.names = TRUE)
  )
  files <- files[!basename(files) %in% c("install-dependencies.R", "check-modules.R")]
  code <- unlist(lapply(files, readLines, warn = FALSE))
  code <- sub("#.*$", "", code) # ignore comments
  patterns <- c(
    "(?:library|require|requireNamespace)\\(\\s*[\"']?([A-Za-z][A-Za-z0-9.]*)",
    "\\b([A-Za-z][A-Za-z0-9.]*):::?"
  )
  found <- unlist(lapply(patterns, function(p) {
    m <- regmatches(code, regexec(p, code, perl = TRUE))
    unlist(lapply(m, `[`, 2L))
  }))
  found <- found[!is.na(found)]
  # Repeated gregexpr for lines with several pkg:: references.
  more <- unlist(regmatches(code, gregexpr("\\b[A-Za-z][A-Za-z0-9.]*(?=::)", code, perl = TRUE)))
  pkgs <- unique(c(found, more))
  base <- rownames(utils::installed.packages(priority = "base"))
  sort(setdiff(pkgs, c(base, "pkg", "rmarkdown", "knitr")), method = "radix")
}

if (sys.nframe() == 0L) {
  pkgs <- c(legacy_dependencies(), "rmarkdown", "knitr")
  if ("--list" %in% commandArgs(TRUE)) {
    writeLines(pkgs)
    quit(save = "no")
  }
  repos <- getOption("repos")
  if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  on_cran <- rownames(utils::available.packages(repos = repos))
  missing <- setdiff(pkgs, rownames(utils::installed.packages()))
  unavailable <- setdiff(missing, on_cran)
  if (length(unavailable)) {
    message("Not on CRAN (optional features will be skipped): ",
            paste(unavailable, collapse = ", "))
  }
  todo <- intersect(missing, on_cran)
  if (length(todo)) {
    message("Installing ", length(todo), " packages: ", paste(todo, collapse = ", "))
    utils::install.packages(todo, repos = repos)
  } else {
    message("All ", length(pkgs), " module dependencies are installed.")
  }
}

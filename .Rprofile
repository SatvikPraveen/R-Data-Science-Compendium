# R Data Science Portfolio - R Session Configuration
# ==================================================
# Global R configuration for consistent development environment

# Initialize renv for dependency management
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# Set global options for better development experience
options(
  # Repositories
  repos = c(
    CRAN = "https://cran.rstudio.com/",
    BioConductor = "https://bioconductor.org/packages/3.18"
  ),
  
  # Display options
  width = 120,
  max.print = 1000,
  scipen = 10,
  digits = 4,
  
  # Development options
  warn = 1,  # Show warnings as they occur
  error = recover,  # Enter browser on error for debugging
  show.error.locations = TRUE,
  
  # Parallel processing
  mc.cores = parallel::detectCores() - 1,
  
  # Package installation
  install.packages.check.source = "no",
  install.packages.compile.from.source = "never",
  
  # HTTP/download options
  timeout = 300,  # 5 minutes
  download.file.method = "auto",
  
  # RStudio specific
  rstudio.help.showDataPreview = TRUE,
  
  # Shiny options
  shiny.port = 3838,
  shiny.host = "127.0.0.1",
  
  # Data table options
  datatable.print.nrows = 20,
  datatable.print.class = TRUE,
  datatable.print.keys = TRUE
)

# Set up environment variables for consistent paths
Sys.setenv(
  R_PROFILE_USER = normalizePath("~/.Rprofile", mustWork = FALSE),
  R_ENVIRON_USER = normalizePath("~/.Renviron", mustWork = FALSE)
)

# Custom startup message
.startup_message <- function() {
  if (interactive()) {
    cat("\n")
    cat("🔬 R Data Science Portfolio Environment\n")
    cat("=======================================\n")
    cat("R version:", R.version.string, "\n")
    cat("Platform:", R.version$platform, "\n")
    
    if (requireNamespace("renv", quietly = TRUE)) {
      cat("Environment: renv", renv::settings$use.cache(), "\n")
    }
    
    cat("Working directory:", getwd(), "\n")
    cat("Available cores:", parallel::detectCores(), "\n")
    
    # Show loaded packages
    loaded_pkgs <- search()
    base_pkgs <- c("package:stats", "package:graphics", "package:grDevices", 
                   "package:utils", "package:datasets", "package:methods", 
                   "package:base")
    additional_pkgs <- setdiff(loaded_pkgs, base_pkgs)
    
    if (length(additional_pkgs) > 0) {
      cat("Loaded packages:", paste(gsub("package:", "", additional_pkgs), collapse = ", "), "\n")
    }
    
    cat("\n📚 Quick Commands:\n")
    cat("  help_ds()     - Show data science functions\n")
    cat("  load_common() - Load common packages\n")
    cat("  setup_proj()  - Setup new analysis project\n")
    cat("  clear_env()   - Clear environment safely\n")
    cat("\n")
  }
}

# Helper functions for data science workflow
help_ds <- function() {
  cat("🔍 R Data Science Portfolio - Quick Reference\n")
  cat("==============================================\n\n")
  
  cat("📊 Data Manipulation:\n")
  cat("  source('R/02-data-manipulation/base-r-operations.R')\n")
  cat("  source('R/02-data-manipulation/data-cleaning.R')\n\n")
  
  cat("📈 Visualization:\n")
  cat("  source('R/03-visualization/base-plots.R')\n")
  cat("  source('R/03-visualization/custom-themes.R')\n")
  cat("  source('R/03-visualization/ggplot2-mastery.R')\n\n")
  
  cat("🤖 Machine Learning:\n")
  cat("  source('R/05-machine-learning/feature-engineering.R')\n")
  cat("  source('R/05-machine-learning/model-evaluation.R')\n\n")
  
  cat("🚀 Shiny Apps:\n")
  cat("  shiny::runApp('shiny-apps/data-explorer')\n")
  cat("  shiny::runApp('shiny-apps/ml-model-comparison')\n")
  cat("  shiny::runApp('shiny-apps/portfolio-dashboard')\n\n")
  
  cat("📝 Analysis Notebooks:\n")
  cat("  rmarkdown::render('analysis/exploratory-data-analysis.Rmd')\n")
  cat("  rmarkdown::render('analysis/statistical-modeling.Rmd')\n\n")
}

load_common <- function() {
  cat("Loading common data science packages...\n")
  
  common_packages <- c(
    "dplyr", "ggplot2", "tidyr", "readr", "stringr",
    "lubridate", "purrr", "tibble", "forcats",
    "data.table", "DT", "plotly"
  )
  
  for (pkg in common_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      library(pkg, character.only = TRUE, quietly = TRUE)
      cat("✓", pkg, "\n")
    } else {
      cat("✗", pkg, "(not installed)\n")
    }
  }
  
  cat("\nCommon packages loaded! 🎉\n")
}

setup_proj <- function(name = NULL) {
  if (is.null(name)) {
    name <- readline("Enter project name: ")
  }
  
  if (name == "" || is.na(name)) {
    cat("❌ Invalid project name\n")
    return(invisible())
  }
  
  project_dir <- file.path("analysis", paste0("project-", gsub("[^A-Za-z0-9-]", "-", name)))
  
  if (dir.exists(project_dir)) {
    cat("❌ Project directory already exists:", project_dir, "\n")
    return(invisible())
  }
  
  cat("📁 Creating project structure...\n")
  
  # Create directories
  dir.create(project_dir, recursive = TRUE)
  dir.create(file.path(project_dir, "data"))
  dir.create(file.path(project_dir, "output"))
  dir.create(file.path(project_dir, "scripts"))
  
  # Create template files
  rmd_template <- sprintf('---
title: "%s Analysis"
author: "R Data Science Portfolio"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

# Load required packages
library(dplyr)
library(ggplot2)
library(readr)

# Source utility functions
source("../../R/02-data-manipulation/data-cleaning.R")
source("../../R/03-visualization/custom-themes.R")
```

## Overview

Brief description of the analysis.

## Data Loading

```{r data-loading}
# Load and explore data

```

## Analysis

```{r analysis}
# Main analysis code

```

## Results

```{r results}
# Present results and visualizations

```

## Conclusions

Summary of findings and next steps.
', name)
  
  writeLines(rmd_template, file.path(project_dir, "analysis.Rmd"))
  
  # Create .gitkeep files
  file.create(file.path(project_dir, "data", ".gitkeep"))
  file.create(file.path(project_dir, "output", ".gitkeep"))
  file.create(file.path(project_dir, "scripts", ".gitkeep"))
  
  cat("✅ Project created:", project_dir, "\n")
  cat("📝 Template file:", file.path(project_dir, "analysis.Rmd"), "\n")
  cat("🚀 Ready to start analysis!\n")
  
  return(invisible(project_dir))
}

clear_env <- function(keep = c("help_ds", "load_common", "setup_proj", "clear_env")) {
  all_objects <- ls(envir = .GlobalEnv)
  to_remove <- setdiff(all_objects, keep)
  
  if (length(to_remove) > 0) {
    rm(list = to_remove, envir = .GlobalEnv)
    cat("🧹 Environment cleared! Kept:", paste(keep, collapse = ", "), "\n")
  } else {
    cat("✨ Environment already clean!\n")
  }
  
  # Run garbage collection
  gc(verbose = FALSE)
}

# Set up custom error handling
.error_handler <- function() {
  if (interactive()) {
    cat("🚨 An error occurred! Use traceback() to see the call stack.\n")
    cat("💡 Tip: Set options(error = browser) for interactive debugging.\n")
  }
}

# Custom package loading with better error messages
load_package <- function(package, quietly = TRUE) {
  if (!requireNamespace(package, quietly = TRUE)) {
    cat("📦 Package '", package, "' not found. Installing...\n", sep = "")
    
    tryCatch({
      install.packages(package, quiet = quietly)
      library(package, character.only = TRUE, quietly = quietly)
      cat("✅ Successfully installed and loaded '", package, "'\n", sep = "")
    }, error = function(e) {
      cat("❌ Failed to install '", package, "': ", e$message, "\n", sep = "")
      return(FALSE)
    })
  } else {
    library(package, character.only = TRUE, quietly = quietly)
    if (!quietly) cat("✅ Loaded '", package, "'\n", sep = "")
  }
  return(TRUE)
}

# Performance profiling helpers
start_profiling <- function(filename = "Rprof.out") {
  if (exists(".profiling_active", envir = .GlobalEnv) && .profiling_active) {
    cat("⚠️ Profiling already active. Stop current session first.\n")
    return(invisible())
  }
  
  Rprof(filename, memory.profiling = TRUE, gc.profiling = TRUE)
  assign(".profiling_active", TRUE, envir = .GlobalEnv)
  assign(".profiling_file", filename, envir = .GlobalEnv)
  cat("🔍 Profiling started. File:", filename, "\n")
}

stop_profiling <- function(show_summary = TRUE) {
  if (!exists(".profiling_active", envir = .GlobalEnv) || !.profiling_active) {
    cat("⚠️ No active profiling session.\n")
    return(invisible())
  }
  
  Rprof(NULL)
  assign(".profiling_active", FALSE, envir = .GlobalEnv)
  
  if (show_summary && file.exists(.profiling_file)) {
    cat("📊 Profiling summary:\n")
    print(summaryRprof(.profiling_file))
  }
  
  cat("✅ Profiling stopped.\n")
}

# Memory management helpers
check_memory <- function() {
  if (interactive()) {
    cat("💾 Memory Usage:\n")
    cat("================\n")
    
    # Object sizes in global environment
    env_objects <- ls(envir = .GlobalEnv)
    if (length(env_objects) > 0) {
      object_sizes <- sapply(env_objects, function(x) {
        object.size(get(x, envir = .GlobalEnv))
      })
      
      # Convert to MB and sort
      object_sizes_mb <- sort(object_sizes / (1024^2), decreasing = TRUE)
      
      cat("Top objects by size:\n")
      for (i in 1:min(5, length(object_sizes_mb))) {
        cat(sprintf("  %s: %.2f MB\n", names(object_sizes_mb)[i], object_sizes_mb[i]))
      }
      
      cat(sprintf("Total objects: %d (%.2f MB)\n", 
                  length(object_sizes), sum(object_sizes_mb)))
    } else {
      cat("No objects in global environment.\n")
    }
    
    # System memory
    gc_info <- gc()
    cat("\nGarbage Collection:\n")
    print(gc_info)
  }
}

# Project structure validation
validate_project <- function() {
  cat("🔍 Validating project structure...\n")
  
  required_dirs <- c(
    "R", "data", "analysis", "shiny-apps", "tests", 
    "R/01-fundamentals", "R/02-data-manipulation", "R/03-visualization",
    "R/04-statistical-analysis", "R/05-machine-learning", "R/06-advanced-topics"
  )
  
  required_files <- c(
    "README.md", "DESCRIPTION", ".gitignore", "renv.lock"
  )
  
  issues <- character(0)
  
  # Check directories
  for (dir in required_dirs) {
    if (!dir.exists(dir)) {
      issues <- c(issues, paste("Missing directory:", dir))
    }
  }
  
  # Check files
  for (file in required_files) {
    if (!file.exists(file)) {
      issues <- c(issues, paste("Missing file:", file))
    }
  }
  
  if (length(issues) == 0) {
    cat("✅ Project structure is valid!\n")
  } else {
    cat("❌ Project structure issues found:\n")
    for (issue in issues) {
      cat("  -", issue, "\n")
    }
  }
  
  return(length(issues) == 0)
}

# Development mode toggle
dev_mode <- function(enable = TRUE) {
  if (enable) {
    options(
      warn = 2,  # Convert warnings to errors
      error = browser,  # Enter browser on error
      keep.source = TRUE,
      deparse.max.lines = 10
    )
    cat("🔧 Development mode ENABLED\n")
    cat("  - Warnings converted to errors\n")
    cat("  - Browser opens on errors\n")
    cat("  - Source code tracking enabled\n")
  } else {
    options(
      warn = 1,  # Show warnings normally
      error = NULL,  # Default error handling
      keep.source = FALSE
    )
    cat("📦 Development mode DISABLED\n")
    cat("  - Normal warning behavior\n")
    cat("  - Default error handling\n")
  }
}

# Quick data exploration
quick_explore <- function(data, n_rows = 6) {
  if (!is.data.frame(data)) {
    cat("❌ Input must be a data frame\n")
    return(invisible())
  }
  
  cat("📋 Quick Data Exploration\n")
  cat("=========================\n")
  cat("Dimensions:", nrow(data), "rows ×", ncol(data), "columns\n\n")
  
  cat("📊 Column Types:\n")
  col_types <- sapply(data, class)
  type_summary <- table(col_types)
  for (type in names(type_summary)) {
    cat("  ", type, ":", type_summary[type], "\n")
  }
  
  cat("\n🔍 First", n_rows, "rows:\n")
  print(head(data, n_rows))
  
  cat("\n📈 Numeric Summary:\n")
  numeric_cols <- sapply(data, is.numeric)
  if (any(numeric_cols)) {
    print(summary(data[, numeric_cols, drop = FALSE]))
  } else {
    cat("  No numeric columns found\n")
  }
  
  cat("\n❓ Missing Values:\n")
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  missing_counts <- missing_counts[missing_counts > 0]
  if (length(missing_counts) > 0) {
    for (col in names(missing_counts)) {
      pct <- round(missing_counts[col] / nrow(data) * 100, 1)
      cat("  ", col, ":", missing_counts[col], "(", pct, "%)\n")
    }
  } else {
    cat("  No missing values found ✅\n")
  }
}

# Package status check
check_packages <- function() {
  cat("📦 Package Status Check\n")
  cat("=======================\n")
  
  required_packages <- c(
    "dplyr", "ggplot2", "tidyr", "readr", "stringr", "lubridate",
    "caret", "randomForest", "xgboost", "glmnet",
    "shiny", "shinydashboard", "DT", "plotly",
    "rmarkdown", "knitr", "devtools", "testthat"
  )
  
  installed_packages <- installed.packages()[, "Package"]
  
  cat("Required packages status:\n")
  for (pkg in required_packages) {
    if (pkg %in% installed_packages) {
      version <- packageVersion(pkg)
      cat("  ✅", pkg, "(", as.character(version), ")\n")
    } else {
      cat("  ❌", pkg, "(not installed)\n")
    }
  }
  
  # Check for updates
  if (interactive()) {
    cat("\n🔄 Checking for updates...\n")
    old_packages <- old.packages()
    if (!is.null(old_packages)) {
      cat("Packages with updates available:\n")
      for (i in 1:nrow(old_packages)) {
        cat("  📅", old_packages[i, "Package"], 
            "(", old_packages[i, "Installed"], "→", old_packages[i, "ReposVer"], ")\n")
      }
    } else {
      cat("All packages are up to date! ✅\n")
    }
  }
}

# Set up completion for custom functions
if (interactive() && requireNamespace("utils", quietly = TRUE)) {
  utils::rc.settings(ipck = TRUE)
}

# Custom history settings
if (interactive()) {
  # Increase history size
  Sys.setenv(R_HISTSIZE = 10000)
  
  # Load history if it exists
  hist_file <- file.path(getwd(), ".Rhistory")
  if (file.exists(hist_file)) {
    try(loadhistory(hist_file), silent = TRUE)
  }
}

# Run startup message
if (interactive()) {
  # Delay startup message slightly to ensure clean output
  addTaskCallback(function(...) {
    .startup_message()
    TRUE  # Remove this callback after running once
  }, name = "startup-message")
}

# Clean up internal functions from global environment
rm(.startup_message, .error_handler)

# Ensure renv is properly configured
if (file.exists("renv.lock") && !exists("renv")) {
  cat("🔄 Initializing renv environment...\n")
  if (requireNamespace("renv", quietly = TRUE)) {
    renv::restore(prompt = FALSE)
  }
}
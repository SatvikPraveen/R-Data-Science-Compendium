#!/usr/bin/env Rscript

# =============================================================================
# R Data Science Portfolio Project Setup
# =============================================================================

cat("🎯 Initializing R Data Science Portfolio Project\n\n")

# Check R version
r_version <- getRversion()
if (r_version < "4.2.0") {
  warning("R version 4.2.0 or higher recommended. Current version: ", r_version)
}

# Essential packages for the project
essential_packages <- c(
  # Core tidyverse
  "dplyr", "ggplot2", "readr", "tidyr", "stringr", "purrr", "tibble", "forcats",
  
  # Data manipulation and import
  "data.table", "readxl", "haven", "jsonlite", "DBI", "RSQLite",
  
  # Visualization
  "plotly", "ggthemes", "RColorBrewer", "viridis", "scales", "patchwork",
  
  # Statistical analysis
  "broom", "modelr", "car", "lme4", "survival", "forecast", "tseries",
  
  # Machine learning
  "caret", "randomForest", "xgboost", "e1071", "cluster", "factoextra",
  
  # Interactive applications
  "shiny", "shinydashboard", "DT", "leaflet", "crosstalk",
  
  # Development tools
  "renv", "devtools", "usethis", "testthat", "roxygen2", "pkgdown",
  "lintr", "styler", "here", "fs", "glue",
  
  # Documentation and reporting
  "rmarkdown", "knitr", "bookdown", "flexdashboard",
  
  # Advanced topics
  "R6", "future", "parallel", "Rcpp", "profvis"
)

# Function to install packages if missing
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("📦 Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, repos = "https://cran.rstudio.com/", dependencies = TRUE)
  } else {
    cat("✅ All essential packages already installed\n")
  }
}

# Install missing packages
install_if_missing(essential_packages)

# Initialize renv for reproducibility
if (!file.exists("renv.lock")) {
  cat("🔧 Initializing renv for dependency management...\n")
  renv::init()
} else {
  cat("✅ renv already initialized\n")
}

cat("✅ Setup completed successfully!\n\n")
cat("🚀 Next steps:\n")
cat("1. Open VS Code: code .\n")
cat("2. Install recommended extensions when prompted\n") 
cat("3. Start exploring R/ modules\n")
cat("4. Run tests: Rscript -e \"testthat::test_dir('tests/')\"\n")
cat("5. Commit your changes: git add . && git commit -m 'Initial setup'\n\n")

cat("📚 Happy coding! Your R portfolio project is ready.\n")

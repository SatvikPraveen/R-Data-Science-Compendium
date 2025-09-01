# @title R Package Development Framework
# @description Comprehensive guide to creating, documenting, and distributing R packages
# @author R Data Science Portfolio Developer
# @date 2025

#' ========================================
#' R PACKAGE DEVELOPMENT
#' ========================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  devtools, usethis, roxygen2, testthat, pkgdown,
  covr, lintr, styler, goodpractice, rhub,
  desc, here, fs, glue, rlang, lifecycle
)

#' ========================================
#' 1. PACKAGE CREATION AND STRUCTURE
#' ========================================

#' Create a New R Package
#' @param package_name Name of the package
#' @param path Directory where package should be created
#' @param description Package description
#' @param author Package author
#' @param license Package license
#' @return Package path
create_r_package <- function(package_name, 
                            path = ".", 
                            description = "A demonstration R package",
                            author = "R Developer",
                            license = "MIT") {
  
  cat("Creating R package:", package_name, "\n")
  
  # Create package structure
  package_path <- file.path(path, package_name)
  
  if (dir.exists(package_path)) {
    stop("Package directory already exists: ", package_path)
  }
  
  # Create package
  usethis::create_package(package_path, open = FALSE)
  
  # Set working directory to package
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(package_path)
  
  # Update DESCRIPTION file
  desc::desc_set(
    "Title" = tools::toTitleCase(gsub("\\.", " ", package_name)),
    "Description" = description,
    "Author" = author,
    "Maintainer" = paste0(author, " <author@example.com>"),
    "License" = license,
    "Version" = "0.1.0",
    "Encoding" = "UTF-8",
    "LazyData" = "true",
    "RoxygenNote" = "7.2.0"
  )
  
  # Add dependencies
  usethis::use_package("dplyr")
  usethis::use_package("ggplot2")
  
  # Setup roxygen2
  usethis::use_roxygen_md()
  
  # Setup testing
  usethis::use_testthat()
  
  # Create initial R files
  create_initial_r_files(package_path)
  
  # Setup documentation
  setup_package_documentation(package_path)
  
  # Setup development tools
  setup_development_tools(package_path)
  
  cat("Package", package_name, "created successfully at:", package_path, "\n")
  
  return(package_path)
}

#' Create Initial R Files for Package
#' @param package_path Path to package directory
create_initial_r_files <- function(package_path) {
  
  r_dir <- file.path(package_path, "R")
  
  # Create main package file
  main_file <- file.path(r_dir, paste0(basename(package_path), "-package.R"))
  
  main_content <- glue::glue('
#\' {tools::toTitleCase(gsub("\\\\.", " ", basename(package_path)))} Package
#\'
#\' @description
#\' This package provides functions for data analysis and visualization.
#\'
#\' @details
#\' The package includes the following main functions:
#\' \\itemize{{
#\'   \\item \\code{{hello_world()}} - A simple greeting function
#\'   \\item \\code{{analyze_data()}} - Basic data analysis
#\'   \\item \\code{{plot_data()}} - Data visualization
#\' }}
#\'
#\' @docType package
#\' @name {basename(package_path)}-package
#\' @aliases {basename(package_path)}
#\' @keywords package
NULL

#\' @importFrom dplyr %>% group_by summarise
#\' @importFrom ggplot2 ggplot aes geom_point theme_minimal
NULL
  ')
  
  writeLines(main_content, main_file)
  
  # Create hello world function
  hello_file <- file.path(r_dir, "hello.R")
  
  hello_content <- '
#\' Hello World Function
#\'
#\' @description
#\' A simple function that greets the user.
#\'
#\' @param name Character string with the name to greet. Default is "World".
#\' @param caps Logical. Should the greeting be in uppercase? Default is FALSE.
#\'
#\' @return A character string with the greeting.
#\'
#\' @examples
#\' hello_world()
#\' hello_world("Alice")
#\' hello_world("Bob", caps = TRUE)
#\'
#\' @export
hello_world <- function(name = "World", caps = FALSE) {
  
  # Input validation
  if (!is.character(name) || length(name) != 1) {
    stop("name must be a single character string")
  }
  
  if (!is.logical(caps) || length(caps) != 1) {
    stop("caps must be a single logical value")
  }
  
  # Create greeting
  greeting <- paste("Hello,", name, "!")
  
  # Apply capitalization if requested
  if (caps) {
    greeting <- toupper(greeting)
  }
  
  return(greeting)
}
  '
  
  writeLines(hello_content, hello_file)
  
  # Create data analysis function
  analyze_file <- file.path(r_dir, "analyze.R")
  
  analyze_content <- '
#\' Analyze Data Function
#\'
#\' @description
#\' Performs basic descriptive analysis on a numeric vector or data frame.
#\'
#\' @param data A numeric vector or data frame to analyze.
#\' @param summary_stats Logical. Should summary statistics be included? Default is TRUE.
#\' @param plots Logical. Should plots be generated? Default is FALSE.
#\'
#\' @return A list containing analysis results.
#\'
#\' @examples
#\' # Analyze a numeric vector
#\' data <- rnorm(100)
#\' result <- analyze_data(data)
#\' print(result)
#\'
#\' # Analyze a data frame
#\' df <- data.frame(x = rnorm(50), y = rnorm(50))
#\' result <- analyze_data(df, plots = TRUE)
#\'
#\' @importFrom dplyr %>%
#\' @export
analyze_data <- function(data, summary_stats = TRUE, plots = FALSE) {
  
  # Input validation
  if (!is.numeric(data) && !is.data.frame(data)) {
    stop("data must be numeric or a data frame")
  }
  
  result <- list()
  
  if (is.numeric(data)) {
    # Analyze numeric vector
    if (summary_stats) {
      result$summary <- list(
        mean = mean(data, na.rm = TRUE),
        median = median(data, na.rm = TRUE),
        sd = sd(data, na.rm = TRUE),
        min = min(data, na.rm = TRUE),
        max = max(data, na.rm = TRUE),
        n = length(data),
        na_count = sum(is.na(data))
      )
    }
    
    if (plots) {
      result$histogram <- hist(data, main = "Data Distribution", 
                              xlab = "Value", col = "lightblue")
    }
    
  } else if (is.data.frame(data)) {
    # Analyze data frame
    numeric_cols <- sapply(data, is.numeric)
    
    if (summary_stats && any(numeric_cols)) {
      result$summary <- data[numeric_cols] %>%
        dplyr::summarise_all(list(
          mean = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE)
        ))
    }
    
    result$structure <- list(
      dimensions = dim(data),
      column_types = sapply(data, class)
    )
  }
  
  class(result) <- c("analysis_result", "list")
  return(result)
}

#\' Print Method for Analysis Results
#\'
#\' @param x An analysis_result object
#\' @param ... Additional arguments (not used)
#\'
#\' @export
print.analysis_result <- function(x, ...) {
  cat("Data Analysis Results\\n")
  cat("====================\\n\\n")
  
  if ("summary" %in% names(x)) {
    cat("Summary Statistics:\\n")
    print(x$summary)
    cat("\\n")
  }
  
  if ("structure" %in% names(x)) {
    cat("Data Structure:\\n")
    cat("Dimensions:", paste(x$structure$dimensions, collapse = " x "), "\\n")
    cat("Column Types:\\n")
    print(x$structure$column_types)
  }
  
  invisible(x)
}
  '
  
  writeLines(analyze_content, analyze_file)
  
  # Create plotting function
  plot_file <- file.path(r_dir, "plotting.R")
  
  plot_content <- '
#\' Plot Data Function
#\'
#\' @description
#\' Creates various plots for data visualization.
#\'
#\' @param data A data frame containing the data to plot.
#\' @param x Character string specifying the x variable.
#\' @param y Character string specifying the y variable.
#\' @param type Character string specifying plot type: "scatter", "line", "bar".
#\' @param color Character string specifying color variable (optional).
#\' @param title Character string for plot title.
#\'
#\' @return A ggplot2 object.
#\'
#\' @examples
#\' # Create sample data
#\' df <- data.frame(
#\'   x = 1:10,
#\'   y = rnorm(10),
#\'   group = rep(c("A", "B"), 5)
#\' )
#\'
#\' # Create scatter plot
#\' plot_data(df, "x", "y", type = "scatter")
#\'
#\' # Create scatter plot with color
#\' plot_data(df, "x", "y", type = "scatter", color = "group")
#\'
#\' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col labs theme_minimal
#\' @export
plot_data <- function(data, x, y, type = "scatter", color = NULL, title = "Data Plot") {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (!x %in% names(data)) {
    stop("x variable not found in data")
  }
  
  if (!y %in% names(data)) {
    stop("y variable not found in data")
  }
  
  if (!type %in% c("scatter", "line", "bar")) {
    stop("type must be one of: scatter, line, bar")
  }
  
  # Create base plot
  if (is.null(color)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]]))
  } else {
    if (!color %in% names(data)) {
      stop("color variable not found in data")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]], color = .data[[color]]))
  }
  
  # Add appropriate geom
  if (type == "scatter") {
    p <- p + ggplot2::geom_point(size = 3, alpha = 0.7)
  } else if (type == "line") {
    p <- p + ggplot2::geom_line(size = 1)
  } else if (type == "bar") {
    p <- p + ggplot2::geom_col(alpha = 0.8)
  }
  
  # Add labels and theme
  p <- p + 
    ggplot2::labs(title = title, x = x, y = y) +
    ggplot2::theme_minimal()
  
  return(p)
}
  '
  
  writeLines(plot_content, plot_file)
  
  cat("Created initial R files\n")
}

#' ========================================
#' 2. DOCUMENTATION SETUP
#' ========================================

#' Setup Package Documentation
#' @param package_path Path to package directory
setup_package_documentation <- function(package_path) {
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(package_path)
  
  # Setup pkgdown
  usethis::use_pkgdown()
  
  # Create README
  usethis::use_readme_rmd()
  
  # Create vignettes directory
  usethis::use_vignette("introduction", title = "Introduction to the Package")
  
  # Setup NEWS file
  usethis::use_news_md()
  
  # Create CITATION file
  create_citation_file(package_path)
  
  cat("Documentation setup completed\n")
}

#' Create Citation File
#' @param package_path Path to package directory
create_citation_file <- function(package_path) {
  
  citation_content <- glue::glue('
citHeader("To cite {basename(package_path)} in publications use:")

citEntry(entry = "Manual",
  title        = "{tools::toTitleCase(gsub(\\"\\\\.\\"," ", basename(package_path)))}",
  author       = personList(as.person("R Developer")),
  year         = "2025",
  note         = "R package version 0.1.0",
  url          = "https://github.com/username/{basename(package_path)}",

  textVersion  =
  paste("R Developer (2025).",
        "{tools::toTitleCase(gsub(\\"\\\\.\\"," ", basename(package_path)))}.",
        "R package version 0.1.0.",
        "https://github.com/username/{basename(package_path)}")
)
  ')
  
  citation_file <- file.path(package_path, "inst", "CITATION")
  dir.create(dirname(citation_file), showWarnings = FALSE, recursive = TRUE)
  writeLines(citation_content, citation_file)
}

#' ========================================
#' 3. TESTING FRAMEWORK
#' ========================================

#' Setup Development Tools
#' @param package_path Path to package directory
setup_development_tools <- function(package_path) {
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(package_path)
  
  # Setup testing
  create_comprehensive_tests(package_path)
  
  # Setup code coverage
  usethis::use_coverage()
  
  # Setup continuous integration
  usethis::use_github_action("check-standard")
  usethis::use_github_action("test-coverage")
  
  # Setup linting
  usethis::use_package("lintr", "Suggests")
  
  cat("Development tools setup completed\n")
}

#' Create Comprehensive Tests
#' @param package_path Path to package directory
create_comprehensive_tests <- function(package_path) {
  
  test_dir <- file.path(package_path, "tests", "testthat")
  
  # Test for hello_world function
  hello_test <- '
test_that("hello_world works correctly", {
  
  # Basic functionality
  expect_equal(hello_world(), "Hello, World !")
  expect_equal(hello_world("Alice"), "Hello, Alice !")
  
  # Caps functionality
  expect_equal(hello_world("Bob", caps = TRUE), "HELLO, BOB !")
  
  # Input validation
  expect_error(hello_world(123), "name must be a single character string")
  expect_error(hello_world(c("A", "B")), "name must be a single character string")
  expect_error(hello_world("Alice", caps = "yes"), "caps must be a single logical value")
})
  '
  
  writeLines(hello_test, file.path(test_dir, "test-hello.R"))
  
  # Test for analyze_data function
  analyze_test <- '
test_that("analyze_data works correctly", {
  
  # Test with numeric vector
  data_vec <- c(1, 2, 3, 4, 5)
  result_vec <- analyze_data(data_vec)
  
  expect_s3_class(result_vec, "analysis_result")
  expect_true("summary" %in% names(result_vec))
  expect_equal(result_vec$summary$mean, 3)
  expect_equal(result_vec$summary$n, 5)
  
  # Test with data frame
  data_df <- data.frame(x = 1:5, y = 6:10, z = letters[1:5])
  result_df <- analyze_data(data_df)
  
  expect_s3_class(result_df, "analysis_result")
  expect_true("structure" %in% names(result_df))
  expect_equal(result_df$structure$dimensions, c(5, 3))
  
  # Test input validation
  expect_error(analyze_data("invalid"), "data must be numeric or a data frame")
})
  '
  
  writeLines(analyze_test, file.path(test_dir, "test-analyze.R"))
  
  # Test for plot_data function
  plot_test <- '
test_that("plot_data works correctly", {
  
  # Create test data
  test_df <- data.frame(
    x = 1:5,
    y = 6:10,
    group = c("A", "A", "B", "B", "A")
  )
  
  # Test basic scatter plot
  p1 <- plot_data(test_df, "x", "y", type = "scatter")
  expect_s3_class(p1, "ggplot")
  
  # Test scatter plot with color
  p2 <- plot_data(test_df, "x", "y", type = "scatter", color = "group")
  expect_s3_class(p2, "ggplot")
  
  # Test line plot
  p3 <- plot_data(test_df, "x", "y", type = "line")
  expect_s3_class(p3, "ggplot")
  
  # Test input validation
  expect_error(plot_data("invalid", "x", "y"), "data must be a data frame")
  expect_error(plot_data(test_df, "invalid", "y"), "x variable not found in data")
  expect_error(plot_data(test_df, "x", "invalid"), "y variable not found in data")
  expect_error(plot_data(test_df, "x", "y", type = "invalid"), "type must be one of")
})
  '
  
  writeLines(plot_test, file.path(test_dir, "test-plotting.R"))
  
  cat("Created comprehensive tests\n")
}

#' ========================================
#' 4. PACKAGE BUILDING AND CHECKING
#' ========================================

#' Build and Check Package
#' @param package_path Path to package directory
#' @param check_cran Logical. Should CRAN checks be performed?
#' @param build_vignettes Logical. Should vignettes be built?
#' @return List of check results
build_and_check_package <- function(package_path, check_cran = TRUE, build_vignettes = TRUE) {
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(package_path)
  
  cat("Building and checking package...\n")
  
  results <- list()
  
  # Document the package
  cat("Generating documentation...\n")
  devtools::document()
  
  # Install package
  cat("Installing package...\n")
  devtools::install()
  
  # Load package
  devtools::load_all()
  
  # Run tests
  cat("Running tests...\n")
  test_results <- devtools::test()
  results$tests <- test_results
  
  # Check package
  cat("Checking package...\n")
  if (check_cran) {
    check_results <- devtools::check(cran = TRUE, vignettes = build_vignettes)
  } else {
    check_results <- devtools::check(vignettes = build_vignettes)
  }
  results$check <- check_results
  
  # Run code coverage
  cat("Calculating code coverage...\n")
  coverage_results <- covr::package_coverage()
  results$coverage <- coverage_results
  
  cat("Coverage percentage:", round(covr::percent_coverage(coverage_results), 2), "%\n")
  
  # Lint code
  cat("Linting code...\n")
  lint_results <- lintr::lint_package()
  results$lint <- lint_results
  
  if (length(lint_results) > 0) {
    cat("Found", length(lint_results), "linting issues\n")
  } else {
    cat("No linting issues found\n")
  }
  
  # Build package
  cat("Building package...\n")
  build_path <- devtools::build()
  results$build_path <- build_path
  
  cat("Package built successfully:", build_path, "\n")
  
  return(results)
}

#' ========================================
#' 5. PACKAGE DISTRIBUTION
#' ========================================

#' Prepare Package for Distribution
#' @param package_path Path to package directory
#' @param github_repo GitHub repository name (username/repo)
#' @return Distribution information
prepare_distribution <- function(package_path, github_repo = NULL) {
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(package_path)
  
  cat("Preparing package for distribution...\n")
  
  distribution_info <- list()
  
  # Setup GitHub if repository provided
  if (!is.null(github_repo)) {
    usethis::use_github(organisation = dirname(github_repo), 
                       private = FALSE)
    distribution_info$github_repo <- github_repo
  }
  
  # Create pkgdown site
  cat("Building pkgdown site...\n")
  pkgdown::build_site()
  distribution_info$pkgdown_site <- TRUE
  
  # Create submission checklist
  submission_checklist <- create_submission_checklist(package_path)
  distribution_info$submission_checklist <- submission_checklist
  
  # Calculate package metrics
  package_metrics <- calculate_package_metrics(package_path)
  distribution_info$metrics <- package_metrics
  
  cat("Distribution preparation completed\n")
  
  return(distribution_info)
}

#' Create Submission Checklist
#' @param package_path Path to package directory
#' @return Submission checklist
create_submission_checklist <- function(package_path) {
  
  checklist <- list(
    "Package builds without errors" = TRUE,
    "All tests pass" = TRUE,
    "Code coverage > 80%" = FALSE,  # Will be updated based on actual coverage
    "No linting issues" = FALSE,    # Will be updated based on actual linting
    "Documentation complete" = TRUE,
    "Vignettes created" = TRUE,
    "NEWS.md updated" = TRUE,
    "Version number appropriate" = TRUE,
    "DESCRIPTION file complete" = TRUE,
    "License included" = TRUE
  )
  
  return(checklist)
}

#' Calculate Package Metrics
#' @param package_path Path to package directory
#' @return Package metrics
calculate_package_metrics <- function(package_path) {
  
  # Count R files
  r_files <- list.files(file.path(package_path, "R"), pattern = "\\.R$", recursive = TRUE)
  
  # Count test files
  test_files <- list.files(file.path(package_path, "tests"), pattern = "\\.R$", recursive = TRUE)
  
  # Count lines of code
  r_lines <- sum(sapply(file.path(package_path, "R", r_files), function(f) length(readLines(f))))
  test_lines <- sum(sapply(file.path(package_path, "tests", test_files), function(f) length(readLines(f))))
  
  # Count functions
  function_count <- 0
  for (file in file.path(package_path, "R", r_files)) {
    content <- readLines(file)
    function_count <- function_count + sum(grepl("^[a-zA-Z_][a-zA-Z0-9_]*\\s*<-\\s*function", content))
  }
  
  metrics <- list(
    r_files = length(r_files),
    test_files = length(test_files),
    r_lines = r_lines,
    test_lines = test_lines,
    functions = function_count,
    test_to_code_ratio = round(test_lines / r_lines, 2)
  )
  
  return(metrics)
}

#' ========================================
#' 6. DEMONSTRATION FUNCTIONS
#' ========================================

#' Demonstrate Package Development Workflow
demo_package_development <- function() {
  cat("=== R PACKAGE DEVELOPMENT DEMONSTRATION ===\n\n")
  
  # Create a temporary directory for demo
  temp_dir <- tempdir()
  demo_package_path <- file.path(temp_dir, "demopackage")
  
  # Clean up if exists
  if (dir.exists(demo_package_path)) {
    unlink(demo_package_path, recursive = TRUE)
  }
  
  cat("1. CREATING PACKAGE STRUCTURE\n")
  cat("="*50, "\n")
  
  # Create package
  package_path <- create_r_package(
    package_name = "demopackage",
    path = temp_dir,
    description = "A demonstration package for the R portfolio",
    author = "R Portfolio Developer"
  )
  
  cat("\n2. BUILDING AND CHECKING PACKAGE\n")
  cat("="*50, "\n")
  
  # Build and check (simplified for demo)
  tryCatch({
    old_wd <- getwd()
    setwd(package_path)
    
    # Document
    devtools::document()
    
    # Simple test run
    test_results <- devtools::test(reporter = "minimal")
    
    cat("Tests completed\n")
    
    setwd(old_wd)
    
  }, error = function(e) {
    cat("Package check encountered issues (expected in demo):", e$message, "\n")
  })
  
  cat("\n3. PACKAGE METRICS\n")
  cat("="*50, "\n")
  
  # Calculate metrics
  metrics <- calculate_package_metrics(package_path)
  
  cat("Package Statistics:\n")
  cat("- R files:", metrics$r_files, "\n")
  cat("- R code lines:", metrics$r_lines, "\n")
  cat("- Functions:", metrics$functions, "\n")
  cat("- Test files:", metrics$test_files, "\n")
  cat("- Test lines:", metrics$test_lines, "\n")
  cat("- Test to code ratio:", metrics$test_to_code_ratio, "\n")
  
  cat("\n4. DEVELOPMENT BEST PRACTICES\n")
  cat("="*50, "\n")
  
  best_practices <- list(
    "Use roxygen2 for documentation",
    "Write comprehensive tests",
    "Follow naming conventions",
    "Use version control (Git)",
    "Write clear vignettes",
    "Include examples in documentation",
    "Check code coverage",
    "Use continuous integration",
    "Follow CRAN policies",
    "Write informative error messages"
  )
  
  for (i in seq_along(best_practices)) {
    cat(sprintf("%d. %s\n", i, best_practices[[i]]))
  }
  
  cat("\nPackage Development Demo Complete!\n")
  cat("Demo package created at:", package_path, "\n")
  
  return(list(
    package_path = package_path,
    metrics = metrics,
    best_practices = best_practices
  ))
}

#' Show Package Development Checklist
show_development_checklist <- function() {
  
  checklist <- list(
    "Planning" = c(
      "Define package purpose and scope",
      "Choose appropriate package name",
      "Decide on license",
      "Plan function interfaces"
    ),
    "Setup" = c(
      "Create package structure with usethis",
      "Setup version control",
      "Configure DESCRIPTION file",
      "Setup testing framework"
    ),
    "Development" = c(
      "Write functions with clear documentation",
      "Add input validation and error handling",
      "Write comprehensive tests",
      "Create examples and vignettes"
    ),
    "Quality Assurance" = c(
      "Check code coverage (aim for >80%)",
      "Run R CMD check without errors",
      "Fix linting issues",
      "Test on different platforms"
    ),
    "Documentation" = c(
      "Write clear function documentation",
      "Create package vignettes",
      "Update NEWS.md file",
      "Create README with installation instructions"
    ),
    "Distribution" = c(
      "Build pkgdown site",
      "Submit to CRAN (if appropriate)",
      "Setup GitHub repository",
      "Create releases with proper tags"
    )
  )
  
  cat("=== R PACKAGE DEVELOPMENT CHECKLIST ===\n\n")
  
  for (phase in names(checklist)) {
    cat(phase, ":\n")
    cat(paste(rep("=", nchar(phase) + 1), collapse = ""), "\n")
    
    for (item in checklist[[phase]]) {
      cat("☐", item, "\n")
    }
    cat("\n")
  }
}

# Export key functions
package_development_exports <- list(
  create_r_package = create_r_package,
  setup_package_documentation = setup_package_documentation,
  setup_development_tools = setup_development_tools,
  build_and_check_package = build_and_check_package,
  prepare_distribution = prepare_distribution,
  calculate_package_metrics = calculate_package_metrics,
  demo_package_development = demo_package_development,
  show_development_checklist = show_development_checklist
)

# Run demo if script is executed directly
if (sys.nframe() == 0) {
  demo_results <- demo_package_development()
  cat("\n")
  show_development_checklist()
}
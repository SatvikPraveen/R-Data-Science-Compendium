# @title Comprehensive Test Runner
# @description Central test runner for R Data Science Portfolio
# @author R Data Science Portfolio
# @date 2025

#' ========================================
#' COMPREHENSIVE TEST RUNNER
#' ========================================

# Test runner for R Data Science Portfolio with detailed reporting

# Required libraries
library(testthat)
library(devtools)

#' Run All Tests with Comprehensive Reporting
#' @param test_dir Directory containing test files
#' @param reporter Test reporter to use
#' @param parallel Whether to run tests in parallel
#' @param coverage Whether to calculate coverage
#' @param output_file File to save detailed results
#' @return Test results summary
run_all_tests <- function(test_dir = "tests/testthat", 
                         reporter = "progress",
                         parallel = FALSE,
                         coverage = TRUE,
                         output_file = NULL) {
  
  cat("🧪 R DATA SCIENCE PORTFOLIO - TEST SUITE\n")
  cat("========================================\n\n")
  
  # Start timing
  start_time <- Sys.time()
  
  # Initialize results tracking
  test_summary <- list(
    start_time = start_time,
    tests_run = 0,
    tests_passed = 0,
    tests_failed = 0,
    tests_skipped = 0,
    warnings = 0,
    errors = character(0),
    coverage_pct = NA
  )
  
  cat("📋 Test Configuration:\n")
  cat("  Test Directory:", test_dir, "\n")
  cat("  Reporter:", reporter, "\n")
  cat("  Parallel:", parallel, "\n")
  cat("  Coverage:", coverage, "\n")
  cat("  Start Time:", format(start_time), "\n\n")
  
  # Check if test directory exists
  if (!dir.exists(test_dir)) {
    cat("❌ Test directory not found:", test_dir, "\n")
    cat("Creating test directory structure...\n")
    dir.create(test_dir, recursive = TRUE)
    return(invisible(test_summary))
  }
  
  # List available test files
  test_files <- list.files(test_dir, pattern = "^test.*\\.(r|R)$", full.names = TRUE)
  
  if (length(test_files) == 0) {
    cat("⚠️  No test files found in", test_dir, "\n")
    return(invisible(test_summary))
  }
  
  cat("📁 Found", length(test_files), "test files:\n")
  for (file in test_files) {
    cat("  -", basename(file), "\n")
  }
  cat("\n")
  
  # Run tests with error handling
  tryCatch({
    
    # Configure parallel testing if requested
    if (parallel && requireNamespace("parallel", quietly = TRUE)) {
      options(testthat.parallel = TRUE)
      cat("🔄 Parallel testing enabled\n")
    }
    
    # Run the test suite
    cat("🚀 Running test suite...\n\n")
    
    # Capture test results
    test_results <- test_dir(
      path = test_dir,
      reporter = reporter,
      env = parent.frame(),
      load_helpers = TRUE,
      stop_on_failure = FALSE
    )
    
    # Extract summary information
    test_summary$tests_run <- length(test_results)
    test_summary$tests_passed <- sum(sapply(test_results, function(x) {
      if (inherits(x, "expectation_success")) 1 else 0
    }))
    test_summary$tests_failed <- sum(sapply(test_results, function(x) {
      if (inherits(x, "expectation_failure")) 1 else 0
    }))
    test_summary$tests_skipped <- sum(sapply(test_results, function(x) {
      if (inherits(x, "expectation_skip")) 1 else 0
    }))
    test_summary$warnings <- sum(sapply(test_results, function(x) {
      if (inherits(x, "expectation_warning")) 1 else 0
    }))
    
    # Collect error messages
    errors <- sapply(test_results, function(x) {
      if (inherits(x, "expectation_failure")) {
        paste(x$test, x$message, sep = ": ")
      }
    })
    test_summary$errors <- errors[!is.na(errors)]
    
  }, error = function(e) {
    cat("❌ Error running tests:", e$message, "\n")
    test_summary$errors <- c(test_summary$errors, e$message)
  })
  
  # Calculate test coverage if requested
  if (coverage && requireNamespace("covr", quietly = TRUE)) {
    cat("\n📊 Calculating test coverage...\n")
    
    tryCatch({
      coverage_result <- covr::package_coverage(
        path = ".",
        type = "tests",
        quiet = TRUE
      )
      
      test_summary$coverage_pct <- covr::percent_coverage(coverage_result)
      cat("Coverage:", round(test_summary$coverage_pct, 2), "%\n")
      
    }, error = function(e) {
      cat("⚠️  Could not calculate coverage:", e$message, "\n")
    })
  }
  
  # End timing
  end_time <- Sys.time()
  test_summary$end_time <- end_time
  test_summary$duration <- end_time - start_time
  
  # Print summary
  print_test_summary(test_summary)
  
  # Save detailed results if requested
  if (!is.null(output_file)) {
    save_test_results(test_summary, output_file)
  }
  
  return(invisible(test_summary))
}

#' Print Test Summary
#' @param test_summary Test results summary
print_test_summary <- function(test_summary) {
  
  cat("\n" , rep("=", 50), "\n")
  cat("📊 TEST RESULTS SUMMARY\n")
  cat(rep("=", 50), "\n\n")
  
  # Overall status
  overall_status <- if (test_summary$tests_failed == 0) "✅ PASSED" else "❌ FAILED"
  cat("Overall Status:", overall_status, "\n")
  cat("Duration:", round(as.numeric(test_summary$duration), 2), "seconds\n\n")
  
  # Test counts
  cat("📈 Test Statistics:\n")
  cat("  Total Tests:", test_summary$tests_run, "\n")
  cat("  Passed:", test_summary$tests_passed, "\n")
  cat("  Failed:", test_summary$tests_failed, "\n")
  cat("  Skipped:", test_summary$tests_skipped, "\n")
  cat("  Warnings:", test_summary$warnings, "\n")
  
  # Success rate
  if (test_summary$tests_run > 0) {
    success_rate <- round(test_summary$tests_passed / test_summary$tests_run * 100, 1)
    cat("  Success Rate:", success_rate, "%\n")
  }
  
  # Coverage
  if (!is.na(test_summary$coverage_pct)) {
    cat("  Coverage:", round(test_summary$coverage_pct, 1), "%\n")
  }
  
  # Error details
  if (length(test_summary$errors) > 0) {
    cat("\n❌ Error Details:\n")
    for (i in seq_along(test_summary$errors)) {
      cat("  ", i, ".", test_summary$errors[i], "\n")
    }
  }
  
  cat("\n" , rep("=", 50), "\n")
}

#' Save Test Results to File
#' @param test_summary Test results summary
#' @param output_file Output file path
save_test_results <- function(test_summary, output_file) {
  
  cat("\n💾 Saving detailed results to:", output_file, "\n")
  
  # Create output directory if needed
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Prepare detailed report
  report_lines <- c(
    "R Data Science Portfolio - Test Results",
    paste("Generated:", Sys.time()),
    "",
    "SUMMARY",
    "=======",
    paste("Start Time:", test_summary$start_time),
    paste("End Time:", test_summary$end_time),
    paste("Duration:", round(as.numeric(test_summary$duration), 2), "seconds"),
    "",
    "STATISTICS",
    "==========",
    paste("Total Tests:", test_summary$tests_run),
    paste("Passed:", test_summary$tests_passed),
    paste("Failed:", test_summary$tests_failed),
    paste("Skipped:", test_summary$tests_skipped),
    paste("Warnings:", test_summary$warnings)
  )
  
  if (!is.na(test_summary$coverage_pct)) {
    report_lines <- c(report_lines, paste("Coverage:", round(test_summary$coverage_pct, 1), "%"))
  }
  
  if (length(test_summary$errors) > 0) {
    report_lines <- c(
      report_lines,
      "",
      "ERRORS",
      "======",
      test_summary$errors
    )
  }
  
  # Write to file
  writeLines(report_lines, output_file)
  cat("✅ Results saved successfully\n")
}

#' Run Specific Test Categories
#' @param category Test category ("fundamentals", "manipulation", "visualization", "ml", "advanced")
#' @param verbose Whether to show detailed output
run_category_tests <- function(category, verbose = FALSE) {
  
  cat("🔍 Running", toupper(category), "tests...\n")
  
  # Map categories to test files
  category_files <- switch(category,
    "fundamentals" = "test-fundamentals.R",
    "manipulation" = "test-data-manipulation.R", 
    "visualization" = "test-visualization.R",
    "statistical" = "test-statistical-analysis.R",
    "ml" = "test-machine-learning.R",
    "advanced" = "test-advanced-topics.R",
    "shiny" = "test-shiny-apps.R",
    stop("Unknown category: ", category)
  )
  
  test_file <- file.path("tests/testthat", category_files)
  
  if (!file.exists(test_file)) {
    cat("⚠️  Test file not found:", test_file, "\n")
    return(invisible(FALSE))
  }
  
  # Run the specific test file
  test_results <- test_file(test_file, reporter = if (verbose) "detailed" else "summary")
  
  cat("✅", toupper(category), "tests completed\n")
  return(invisible(test_results))
}

#' Quick Test Run (essential tests only)
quick_test <- function() {
  
  cat("⚡ QUICK TEST SUITE\n")
  cat("==================\n\n")
  
  essential_tests <- c(
    "test-fundamentals.R",
    "test-data-manipulation.R",
    "test-visualization.R"
  )
  
  for (test_file in essential_tests) {
    test_path <- file.path("tests/testthat", test_file)
    if (file.exists(test_path)) {
      cat("Running", test_file, "...\n")
      test_file(test_path, reporter = "summary")
    }
  }
  
  cat("\n⚡ Quick test suite completed!\n")
}

#' Benchmark Test Performance
#' @param iterations Number of iterations to run
benchmark_tests <- function(iterations = 3) {
  
  cat("⏱️  BENCHMARK TEST PERFORMANCE\n")
  cat("==============================\n\n")
  
  timings <- numeric(iterations)
  
  for (i in 1:iterations) {
    cat("Iteration", i, "of", iterations, "...\n")
    
    start_time <- Sys.time()
    suppressMessages(quick_test())
    end_time <- Sys.time()
    
    timings[i] <- as.numeric(end_time - start_time)
  }
  
  cat("\n📊 Benchmark Results:\n")
  cat("  Mean Time:", round(mean(timings), 2), "seconds\n")
  cat("  Min Time:", round(min(timings), 2), "seconds\n")
  cat("  Max Time:", round(max(timings), 2), "seconds\n")
  cat("  Std Dev:", round(sd(timings), 2), "seconds\n")
  
  return(invisible(timings))
}

#' Run Tests in Development Mode
#' @param watch Whether to watch for file changes
dev_test <- function(watch = FALSE) {
  
  cat("🔧 DEVELOPMENT TEST MODE\n")
  cat("========================\n\n")
  
  if (watch) {
    cat("👀 Watching for file changes...\n")
    cat("Press Ctrl+C to stop\n\n")
    
    # Simple file watching (would need more sophisticated implementation)
    last_modified <- Sys.time()
    
    while (TRUE) {
      Sys.sleep(2)
      
      # Check for R file modifications
      r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
      test_files <- list.files("tests", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
      all_files <- c(r_files, test_files)
      
      if (length(all_files) > 0) {
        latest_mod <- max(file.mtime(all_files), na.rm = TRUE)
        
        if (latest_mod > last_modified) {
          cat("🔄 File changes detected, running tests...\n")
          quick_test()
          last_modified <- latest_mod
          cat("\n👀 Watching for changes...\n")
        }
      }
    }
  } else {
    quick_test()
  }
}

#' Main function for command line usage
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    run_all_tests()
  } else {
    command <- args[1]
    
    switch(command,
      "all" = run_all_tests(),
      "quick" = quick_test(),
      "dev" = dev_test(),
      "benchmark" = benchmark_tests(),
      "fundamentals" = run_category_tests("fundamentals"),
      "manipulation" = run_category_tests("manipulation"),
      "visualization" = run_category_tests("visualization"),
      "ml" = run_category_tests("ml"),
      "advanced" = run_category_tests("advanced"),
      {
        cat("Unknown command:", command, "\n")
        cat("Available commands: all, quick, dev, benchmark, fundamentals, manipulation, visualization, ml, advanced\n")
      }
    )
  }
}

# Run main function if script is executed directly
if (sys.nframe() == 0) {
  main()
}
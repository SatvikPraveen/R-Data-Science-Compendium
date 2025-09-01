# @title Validation Functions and Data Quality Checks
# @description Comprehensive validation utilities for data quality and integrity
# @author R Data Science Portfolio Developer
# @date 2025

#' ========================================
#' VALIDATION FUNCTIONS AND DATA QUALITY
#' ========================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, assertthat, checkmate, validate, VIM, 
  stringr, lubridate, testthat, methods
)

#' ========================================
#' 1. BASIC DATA VALIDATION
#' ========================================

#' Validate Data Frame Structure
#' @param data Data frame to validate
#' @param required_columns Vector of required column names
#' @param expected_types Named vector of expected column types
#' @param min_rows Minimum number of rows required
#' @param max_rows Maximum number of rows allowed
#' @return Validation results list
validate_data_structure <- function(data, required_columns = NULL, 
                                  expected_types = NULL, min_rows = 1, 
                                  max_rows = Inf) {
  
  validation_results <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    info = list()
  )
  
  # Check if input is a data frame
  if (!is.data.frame(data)) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors, 
                                  "Input is not a data frame")
    return(validation_results)
  }
  
  # Store basic info
  validation_results$info$dimensions <- dim(data)
  validation_results$info$column_names <- names(data)
  validation_results$info$column_types <- sapply(data, class)
  
  # Check required columns
  if (!is.null(required_columns)) {
    missing_cols <- setdiff(required_columns, names(data))
    if (length(missing_cols) > 0) {
      validation_results$valid <- FALSE
      validation_results$errors <- c(validation_results$errors, 
                                    paste("Missing required columns:", 
                                          paste(missing_cols, collapse = ", ")))
    }
  }
  
  # Check expected types
  if (!is.null(expected_types)) {
    for (col_name in names(expected_types)) {
      if (col_name %in% names(data)) {
        actual_type <- class(data[[col_name]])[1]
        expected_type <- expected_types[[col_name]]
        
        if (actual_type != expected_type) {
          validation_results$warnings <- c(validation_results$warnings,
                                          paste("Column", col_name, "has type", 
                                               actual_type, "but expected", expected_type))
        }
      }
    }
  }
  
  # Check row count
  if (nrow(data) < min_rows) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors,
                                  paste("Data has", nrow(data), "rows but minimum is", min_rows))
  }
  
  if (nrow(data) > max_rows) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors,
                                  paste("Data has", nrow(data), "rows but maximum is", max_rows))
  }
  
  return(validation_results)
}

#' Validate Numeric Ranges
#' @param data Data frame
#' @param column_ranges Named list of ranges for numeric columns
#' @param strict Should violations be errors (TRUE) or warnings (FALSE)?
#' @return Validation results
validate_numeric_ranges <- function(data, column_ranges, strict = FALSE) {
  
  validation_results <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    violations = list()
  )
  
  for (col_name in names(column_ranges)) {
    if (!col_name %in% names(data)) {
      validation_results$warnings <- c(validation_results$warnings,
                                      paste("Column", col_name, "not found in data"))
      next
    }
    
    col_data <- data[[col_name]]
    
    if (!is.numeric(col_data)) {
      validation_results$warnings <- c(validation_results$warnings,
                                      paste("Column", col_name, "is not numeric"))
      next
    }
    
    range_def <- column_ranges[[col_name]]
    min_val <- range_def[1]
    max_val <- range_def[2]
    
    # Check for values outside range
    outside_range <- which(col_data < min_val | col_data > max_val)
    outside_range <- outside_range[!is.na(col_data[outside_range])]  # Exclude NAs
    
    if (length(outside_range) > 0) {
      violation_msg <- paste("Column", col_name, "has", length(outside_range), 
                           "values outside range [", min_val, ",", max_val, "]")
      
      validation_results$violations[[col_name]] <- list(
        indices = outside_range,
        values = col_data[outside_range],
        expected_range = c(min_val, max_val)
      )
      
      if (strict) {
        validation_results$valid <- FALSE
        validation_results$errors <- c(validation_results$errors, violation_msg)
      } else {
        validation_results$warnings <- c(validation_results$warnings, violation_msg)
      }
    }
  }
  
  return(validation_results)
}

#' Validate Data Completeness
#' @param data Data frame
#' @param required_complete Vector of column names that must be complete
#' @param max_missing_pct Maximum percentage of missing values allowed per column
#' @return Validation results
validate_completeness <- function(data, required_complete = NULL, max_missing_pct = 20) {
  
  validation_results <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    missing_summary = NULL
  )
  
  # Calculate missing data summary
  missing_summary <- data %>%
    summarise_all(~sum(is.na(.))) %>%
    tidyr::gather(column, missing_count) %>%
    mutate(
      total_count = nrow(data),
      missing_pct = round(missing_count / total_count * 100, 2)
    ) %>%
    arrange(desc(missing_pct))
  
  validation_results$missing_summary <- missing_summary
  
  # Check required complete columns
  if (!is.null(required_complete)) {
    for (col in required_complete) {
      if (col %in% names(data)) {
        missing_count <- sum(is.na(data[[col]]))
        if (missing_count > 0) {
          validation_results$valid <- FALSE
          validation_results$errors <- c(validation_results$errors,
                                        paste("Required complete column", col, 
                                             "has", missing_count, "missing values"))
        }
      }
    }
  }
  
  # Check maximum missing percentage
  high_missing <- missing_summary[missing_summary$missing_pct > max_missing_pct, ]
  if (nrow(high_missing) > 0) {
    for (i in 1:nrow(high_missing)) {
      validation_results$warnings <- c(validation_results$warnings,
                                      paste("Column", high_missing$column[i], 
                                           "has", high_missing$missing_pct[i], 
                                           "% missing values"))
    }
  }
  
  return(validation_results)
}

#' ========================================
#' 2. BUSINESS RULES VALIDATION
#' ========================================

#' Validate Business Rules
#' @param data Data frame
#' @param rules Named list of validation rules (expressions)
#' @param rule_descriptions Named list of rule descriptions
#' @return Validation results
validate_business_rules <- function(data, rules, rule_descriptions = NULL) {
  
  validation_results <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    rule_results = list()
  )
  
  for (rule_name in names(rules)) {
    rule_expr <- rules[[rule_name]]
    
    tryCatch({
      # Evaluate rule
      rule_result <- with(data, eval(parse(text = rule_expr)))
      
      # Count violations
      violations <- sum(!rule_result, na.rm = TRUE)
      violation_indices <- which(!rule_result)
      
      rule_info <- list(
        expression = rule_expr,
        description = if (!is.null(rule_descriptions)) rule_descriptions[[rule_name]] else NULL,
        violations = violations,
        violation_indices = violation_indices,
        total_checked = sum(!is.na(rule_result))
      )
      
      validation_results$rule_results[[rule_name]] <- rule_info
      
      if (violations > 0) {
        violation_msg <- paste("Rule", rule_name, "violated by", violations, "records")
        if (!is.null(rule_descriptions) && !is.null(rule_descriptions[[rule_name]])) {
          violation_msg <- paste(violation_msg, "-", rule_descriptions[[rule_name]])
        }
        validation_results$warnings <- c(validation_results$warnings, violation_msg)
      }
      
    }, error = function(e) {
      validation_results$errors <- c(validation_results$errors,
                                    paste("Error evaluating rule", rule_name, ":", e$message))
      validation_results$valid <- FALSE
    })
  }
  
  return(validation_results)
}

#' Create Standard Business Rules
#' @param data Data frame to create rules for
#' @return List of common business rules
create_standard_business_rules <- function(data) {
  
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  date_cols <- names(data)[sapply(data, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
  
  rules <- list()
  descriptions <- list()
  
  # Non-negative rules for numeric columns
  for (col in numeric_cols) {
    rule_name <- paste0(col, "_non_negative")
    rules[[rule_name]] <- paste0(col, " >= 0")
    descriptions[[rule_name]] <- paste("Values in", col, "should be non-negative")
  }
  
  # Date range rules
  for (col in date_cols) {
    rule_name <- paste0(col, "_reasonable_date")
    rules[[rule_name]] <- paste0(col, " >= as.Date('1900-01-01') & ", col, " <= Sys.Date()")
    descriptions[[rule_name]] <- paste("Dates in", col, "should be reasonable (1900 to today)")
  }
  
  # Custom rules based on common patterns
  if ("age" %in% names(data)) {
    rules$age_reasonable <- "age >= 0 & age <= 150"
    descriptions$age_reasonable <- "Age should be between 0 and 150"
  }
  
  if ("email" %in% names(data)) {
    rules$email_format <- "grepl('^[\\\\w\\\\.-]+@[\\\\w\\\\.-]+\\\\.[a-zA-Z]{2,}$', email)"
    descriptions$email_format <- "Email should have valid format"
  }
  
  return(list(rules = rules, descriptions = descriptions))
}

#' ========================================
#' 3. DATA TYPE VALIDATION
#' ========================================

#' Validate and Suggest Data Types
#' @param data Data frame
#' @param sample_size Number of samples to check for type detection
#' @return Data type validation results
validate_data_types <- function(data, sample_size = 1000) {
  
  validation_results <- list(
    current_types = sapply(data, class),
    suggested_types = character(),
    type_issues = character(),
    conversion_suggestions = list()
  )
  
  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    current_type <- class(col_data)[1]
    
    # Sample data for analysis
    if (length(col_data) > sample_size) {
      sample_data <- sample(col_data[!is.na(col_data)], min(sample_size, sum(!is.na(col_data))))
    } else {
      sample_data <- col_data[!is.na(col_data)]
    }
    
    suggested_type <- suggest_data_type(sample_data)
    validation_results$suggested_types[col_name] <- suggested_type
    
    # Check for type issues
    if (current_type != suggested_type) {
      issue_msg <- paste("Column", col_name, "is", current_type, 
                        "but could be", suggested_type)
      validation_results$type_issues <- c(validation_results$type_issues, issue_msg)
      
      # Provide conversion suggestion
      validation_results$conversion_suggestions[[col_name]] <- list(
        from = current_type,
        to = suggested_type,
        function_suggestion = get_conversion_function(current_type, suggested_type)
      )
    }
  }
  
  return(validation_results)
}

#' Suggest Data Type for a Vector
#' @param x Vector to analyze
#' @return Suggested data type
suggest_data_type <- function(x) {
  
  if (length(x) == 0) return("unknown")
  
  # Remove missing values for analysis
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) == 0) return("logical")  # All NA
  
  # Check if all values can be converted to numeric
  numeric_conversion <- suppressWarnings(as.numeric(as.character(x_clean)))
  if (!any(is.na(numeric_conversion))) {
    # Check if all numbers are integers
    if (all(numeric_conversion == round(numeric_conversion))) {
      return("integer")
    } else {
      return("numeric")
    }
  }
  
  # Check if all values can be converted to Date
  date_conversion <- suppressWarnings(as.Date(as.character(x_clean)))
  if (!any(is.na(date_conversion))) {
    return("Date")
  }
  
  # Check if values look like logical
  unique_vals <- unique(tolower(trimws(as.character(x_clean))))
  if (all(unique_vals %in% c("true", "false", "t", "f", "yes", "no", "y", "n", "1", "0"))) {
    return("logical")
  }
  
  # Check if it should be a factor (limited unique values)
  if (length(unique(x_clean)) / length(x_clean) < 0.1 && length(unique(x_clean)) < 50) {
    return("factor")
  }
  
  return("character")
}

#' Get Conversion Function Suggestion
#' @param from_type Current data type
#' @param to_type Suggested data type
#' @return Conversion function suggestion
get_conversion_function <- function(from_type, to_type) {
  
  conversion_map <- list(
    "character_to_numeric" = "as.numeric(x)",
    "character_to_integer" = "as.integer(x)",
    "character_to_Date" = "as.Date(x)",
    "character_to_logical" = "as.logical(x)",
    "character_to_factor" = "as.factor(x)",
    "factor_to_character" = "as.character(x)",
    "factor_to_numeric" = "as.numeric(as.character(x))",
    "numeric_to_integer" = "as.integer(x)",
    "integer_to_numeric" = "as.numeric(x)"
  )
  
  key <- paste(from_type, to_type, sep = "_to_")
  
  if (key %in% names(conversion_map)) {
    return(conversion_map[[key]])
  } else {
    return(paste0("as.", to_type, "(x)"))
  }
}

#' ========================================
#' 4. STATISTICAL VALIDATION
#' ========================================

#' Detect Statistical Outliers
#' @param data Data frame
#' @param columns Columns to check for outliers
#' @param method Outlier detection method ("iqr", "zscore", "modified_zscore")
#' @param threshold Threshold for outlier detection
#' @return Outlier detection results
detect_outliers <- function(data, columns = NULL, method = "iqr", threshold = 1.5) {
  
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  outlier_results <- list(
    method = method,
    threshold = threshold,
    outliers_by_column = list(),
    summary = data.frame()
  )
  
  summary_data <- data.frame(
    column = character(),
    total_values = integer(),
    outlier_count = integer(),
    outlier_percentage = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (col in columns) {
    if (!col %in% names(data) || !is.numeric(data[[col]])) {
      next
    }
    
    col_data <- data[[col]]
    col_data_clean <- col_data[!is.na(col_data)]
    
    if (length(col_data_clean) < 3) {
      next
    }
    
    outlier_indices <- detect_outliers_single_column(col_data, method, threshold)
    
    outlier_results$outliers_by_column[[col]] <- list(
      indices = outlier_indices,
      values = col_data[outlier_indices],
      total_count = length(col_data_clean),
      outlier_count = length(outlier_indices)
    )
    
    # Add to summary
    summary_data <- rbind(summary_data, data.frame(
      column = col,
      total_values = length(col_data_clean),
      outlier_count = length(outlier_indices),
      outlier_percentage = round(length(outlier_indices) / length(col_data_clean) * 100, 2),
      stringsAsFactors = FALSE
    ))
  }
  
  outlier_results$summary <- summary_data
  
  return(outlier_results)
}

#' Detect Outliers in Single Column
#' @param x Numeric vector
#' @param method Detection method
#' @param threshold Threshold value
#' @return Indices of outliers
detect_outliers_single_column <- function(x, method = "iqr", threshold = 1.5) {
  
  x_clean <- x[!is.na(x)]
  
  if (method == "iqr") {
    Q1 <- quantile(x_clean, 0.25)
    Q3 <- quantile(x_clean, 0.75)
    IQR <- Q3 - Q1
    
    lower_bound <- Q1 - threshold * IQR
    upper_bound <- Q3 + threshold * IQR
    
    outlier_indices <- which(x < lower_bound | x > upper_bound)
    
  } else if (method == "zscore") {
    z_scores <- abs((x - mean(x_clean)) / sd(x_clean))
    outlier_indices <- which(z_scores > threshold)
    
  } else if (method == "modified_zscore") {
    median_val <- median(x_clean)
    mad_val <- mad(x_clean)
    modified_z_scores <- 0.6745 * (x - median_val) / mad_val
    outlier_indices <- which(abs(modified_z_scores) > threshold)
  }
  
  return(outlier_indices)
}

#' Validate Distribution Assumptions
#' @param data Data frame
#' @param columns Columns to test
#' @param tests Vector of tests to perform
#' @return Distribution test results
validate_distributions <- function(data, columns = NULL, 
                                 tests = c("normality", "equal_variance")) {
  
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  test_results <- list()
  
  for (col in columns) {
    if (!col %in% names(data) || !is.numeric(data[[col]])) {
      next
    }
    
    col_data <- data[[col]][!is.na(data[[col]])]
    
    if (length(col_data) < 3) {
      next
    }
    
    col_results <- list()
    
    # Normality tests
    if ("normality" %in% tests) {
      if (length(col_data) >= 3 && length(col_data) <= 5000) {
        shapiro_test <- shapiro.test(col_data)
        col_results$shapiro_wilk <- list(
          statistic = shapiro_test$statistic,
          p_value = shapiro_test$p.value,
          is_normal = shapiro_test$p.value > 0.05
        )
      }
      
      if (length(col_data) > 7) {
        ks_test <- ks.test(col_data, "pnorm", mean(col_data), sd(col_data))
        col_results$kolmogorov_smirnov <- list(
          statistic = ks_test$statistic,
          p_value = ks_test$p.value,
          is_normal = ks_test$p.value > 0.05
        )
      }
    }
    
    test_results[[col]] <- col_results
  }
  
  return(test_results)
}

#' ========================================
#' 5. COMPREHENSIVE VALIDATION REPORT
#' ========================================

#' Generate Comprehensive Validation Report
#' @param data Data frame to validate
#' @param config Validation configuration list
#' @return Comprehensive validation report
generate_validation_report <- function(data, config = NULL) {
  
  # Default configuration
  if (is.null(config)) {
    config <- list(
      required_columns = NULL,
      expected_types = NULL,
      column_ranges = NULL,
      business_rules = NULL,
      max_missing_pct = 20,
      outlier_detection = TRUE,
      distribution_tests = TRUE
    )
  }
  
  report <- list(
    timestamp = Sys.time(),
    dataset_info = list(
      dimensions = dim(data),
      memory_size = format(object.size(data), units = "MB")
    ),
    validation_results = list(),
    summary = list(
      total_errors = 0,
      total_warnings = 0,
      overall_status = "UNKNOWN"
    )
  )
  
  cat("Generating comprehensive validation report...\n")
  
  # 1. Structure validation
  cat("1. Validating data structure...\n")
  structure_results <- validate_data_structure(
    data, 
    required_columns = config$required_columns,
    expected_types = config$expected_types
  )
  report$validation_results$structure <- structure_results
  
  # 2. Completeness validation
  cat("2. Validating data completeness...\n")
  completeness_results <- validate_completeness(
    data, 
    max_missing_pct = config$max_missing_pct
  )
  report$validation_results$completeness <- completeness_results
  
  # 3. Numeric ranges validation
  if (!is.null(config$column_ranges)) {
    cat("3. Validating numeric ranges...\n")
    ranges_results <- validate_numeric_ranges(data, config$column_ranges)
    report$validation_results$ranges <- ranges_results
  }
  
  # 4. Business rules validation
  if (!is.null(config$business_rules)) {
    cat("4. Validating business rules...\n")
    rules_results <- validate_business_rules(data, config$business_rules$rules, 
                                           config$business_rules$descriptions)
    report$validation_results$business_rules <- rules_results
  }
  
  # 5. Data type validation
  cat("5. Validating data types...\n")
  types_results <- validate_data_types(data)
  report$validation_results$data_types <- types_results
  
  # 6. Outlier detection
  if (config$outlier_detection) {
    cat("6. Detecting outliers...\n")
    outliers_results <- detect_outliers(data)
    report$validation_results$outliers <- outliers_results
  }
  
  # 7. Distribution validation
  if (config$distribution_tests) {
    cat("7. Validating distributions...\n")
    distribution_results <- validate_distributions(data)
    report$validation_results$distributions <- distribution_results
  }
  
  # Calculate summary
  total_errors <- 0
  total_warnings <- 0
  
  for (result_name in names(report$validation_results)) {
    result <- report$validation_results[[result_name]]
    if ("errors" %in% names(result)) {
      total_errors <- total_errors + length(result$errors)
    }
    if ("warnings" %in% names(result)) {
      total_warnings <- total_warnings + length(result$warnings)
    }
  }
  
  report$summary$total_errors <- total_errors
  report$summary$total_warnings <- total_warnings
  
  if (total_errors > 0) {
    report$summary$overall_status <- "FAILED"
  } else if (total_warnings > 0) {
    report$summary$overall_status <- "WARNINGS"
  } else {
    report$summary$overall_status <- "PASSED"
  }
  
  cat("Validation report complete!\n")
  cat("Status:", report$summary$overall_status, "\n")
  cat("Errors:", total_errors, "| Warnings:", total_warnings, "\n")
  
  return(report)
}

#' Print Validation Report
#' @param report Validation report from generate_validation_report
#' @param verbose Should detailed results be printed?
print_validation_report <- function(report, verbose = FALSE) {
  
  cat("="*60, "\n")
  cat("DATA VALIDATION REPORT\n")
  cat("="*60, "\n")
  cat("Generated:", format(report$timestamp), "\n")
  cat("Dataset size:", paste(report$dataset_info$dimensions, collapse = " x "), "\n")
  cat("Memory usage:", report$dataset_info$memory_size, "\n")
  cat("Overall status:", report$summary$overall_status, "\n")
  cat("Total errors:", report$summary$total_errors, "\n")
  cat("Total warnings:", report$summary$total_warnings, "\n")
  cat("\n")
  
  if (verbose) {
    for (validation_type in names(report$validation_results)) {
      result <- report$validation_results[[validation_type]]
      
      cat(toupper(validation_type), "VALIDATION\n")
      cat(paste(rep("-", nchar(validation_type) + 11), collapse = ""), "\n")
      
      if ("errors" %in% names(result) && length(result$errors) > 0) {
        cat("ERRORS:\n")
        for (error in result$errors) {
          cat("  ✗", error, "\n")
        }
      }
      
      if ("warnings" %in% names(result) && length(result$warnings) > 0) {
        cat("WARNINGS:\n")
        for (warning in result$warnings) {
          cat("  ⚠", warning, "\n")
        }
      }
      
      cat("\n")
    }
  }
}

#' ========================================
#' 6. DEMONSTRATION FUNCTIONS
#' ========================================

#' Generate Demo Data for Validation
generate_validation_demo_data <- function() {
  
  set.seed(42)
  n <- 1000
  
  # Create demo dataset with various issues
  demo_data <- data.frame(
    id = 1:n,
    name = sample(c("Alice", "Bob", "Charlie", NA), n, replace = TRUE),
    age = c(rnorm(n-50, 35, 10), rep(NA, 30), c(-5, 200, 150, 250, -10)),  # Some invalid ages
    email = sample(c("valid@email.com", "invalid-email", "test@test.org", NA), n, replace = TRUE),
    salary = c(rnorm(n-20, 50000, 15000), rep(NA, 20)),
    score = c(rnorm(n-100, 75, 15), rep(999, 50), rep(-10, 50)),  # Some outliers
    category = sample(c("A", "B", "C"), n, replace = TRUE),
    start_date = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Add some impossible dates
  demo_data$start_date[1:5] <- as.Date("1800-01-01")
  
  return(demo_data)
}

#' Demonstrate Validation Functions
demo_validation_functions <- function() {
  cat("=== VALIDATION FUNCTIONS DEMONSTRATION ===\n\n")
  
  # Generate demo data with issues
  demo_data <- generate_validation_demo_data()
  
  cat("Generated demo dataset with intentional data quality issues\n")
  cat("Dataset dimensions:", paste(dim(demo_data), collapse = " x "), "\n\n")
  
  # Configure validation
  validation_config <- list(
    required_columns = c("id", "name", "age"),
    expected_types = c("id" = "integer", "name" = "character", "age" = "numeric"),
    column_ranges = list("age" = c(0, 120), "score" = c(0, 100)),
    max_missing_pct = 15,
    outlier_detection = TRUE,
    distribution_tests = TRUE
  )
  
  # Create business rules
  business_rules <- create_standard_business_rules(demo_data)
  business_rules$rules$valid_email <- "is.na(email) | grepl('^[\\\\w\\\\.-]+@[\\\\w\\\\.-]+\\\\.[a-zA-Z]{2,}$', email)"
  business_rules$descriptions$valid_email <- "Email should be valid format or missing"
  
  validation_config$business_rules <- business_rules
  
  cat("1. STRUCTURE VALIDATION\n")
  cat("="*30, "\n")
  
  structure_results <- validate_data_structure(
    demo_data, 
    required_columns = validation_config$required_columns,
    expected_types = validation_config$expected_types
  )
  
  cat("Structure validation completed\n")
  cat("Valid:", structure_results$valid, "\n")
  if (length(structure_results$errors) > 0) {
    cat("Errors found:", length(structure_results$errors), "\n")
  }
  
  cat("\n2. OUTLIER DETECTION\n")
  cat("="*30, "\n")
  
  outlier_results <- detect_outliers(demo_data, method = "iqr")
  
  cat("Outlier detection completed\n")
  print(outlier_results$summary)
  
  cat("\n3. COMPREHENSIVE VALIDATION REPORT\n")
  cat("="*30, "\n")
  
  # Generate comprehensive report
  full_report <- generate_validation_report(demo_data, validation_config)
  
  # Print summary
  print_validation_report(full_report, verbose = FALSE)
  
  cat("\n4. VALIDATION SUMMARY\n")
  cat("="*30, "\n")
  
  cat("The demo dataset contains intentional data quality issues:\n")
  cat("- Missing values in required columns\n")
  cat("- Invalid age values (negative, >150)\n")
  cat("- Invalid email formats\n")
  cat("- Statistical outliers in score column\n")
  cat("- Impossible historical dates\n")
  
  cat("\nValidation Functions Demo Complete!\n")
  
  return(list(
    demo_data = demo_data,
    validation_config = validation_config,
    validation_report = full_report
  ))
}

# Export key functions
validation_functions_exports <- list(
  validate_data_structure = validate_data_structure,
  validate_numeric_ranges = validate_numeric_ranges,
  validate_completeness = validate_completeness,
  validate_business_rules = validate_business_rules,
  create_standard_business_rules = create_standard_business_rules,
  validate_data_types = validate_data_types,
  suggest_data_type = suggest_data_type,
  detect_outliers = detect_outliers,
  validate_distributions = validate_distributions,
  generate_validation_report = generate_validation_report,
  print_validation_report = print_validation_report,
  generate_validation_demo_data = generate_validation_demo_data,
  demo_validation_functions = demo_validation_functions
)

# Run demo if script is executed directly
if (sys.nframe() == 0) {
  demo_results <- demo_validation_functions()
}
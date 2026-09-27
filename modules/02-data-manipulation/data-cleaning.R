# @title Comprehensive Data Cleaning Framework
# @description Advanced data cleaning, validation, and quality assessment tools
# @author R Data Science Portfolio
# @date 2025

library(dplyr)
library(stringr)
library(lubridate)

#' ========================================
#' COMPREHENSIVE DATA CLEANING FRAMEWORK
#' ========================================

#' ========================================
#' 1. DATA QUALITY ASSESSMENT
#' ========================================

#' Comprehensive Data Quality Assessment
#' @param data Data frame to assess
#' @param id_column Name of ID column (optional)
#' @return List with quality assessment results
assess_data_quality <- function(data, id_column = NULL) {
  
  cat("DATA QUALITY ASSESSMENT\n")
  cat("=======================\n\n")
  
  results <- list()
  
  # Basic structure
  results$structure <- list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    column_names = names(data),
    data_types = sapply(data, class)
  )
  
  cat("Dataset Structure:\n")
  cat("- Rows:", results$structure$n_rows, "\n")
  cat("- Columns:", results$structure$n_cols, "\n")
  
  # Missing data analysis
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  missing_percentages <- round(missing_counts / nrow(data) * 100, 2)
  
  results$missing_data <- data.frame(
    column = names(missing_counts),
    missing_count = missing_counts,
    missing_percentage = missing_percentages,
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(missing_percentage))
  
  cat("\nMissing Data Summary:\n")
  print(results$missing_data[results$missing_data$missing_count > 0, ])
  
  # Duplicate analysis
  if (!is.null(id_column) && id_column %in% names(data)) {
    duplicate_ids <- sum(duplicated(data[[id_column]]))
    results$duplicates$by_id <- duplicate_ids
    cat("\nDuplicate IDs:", duplicate_ids, "\n")
  }
  
  complete_duplicates <- sum(duplicated(data))
  results$duplicates$complete_rows <- complete_duplicates
  cat("Complete duplicate rows:", complete_duplicates, "\n")
  
  # Data type consistency
  results$type_issues <- check_data_types(data)
  
  # Value range analysis for numeric columns
  numeric_cols <- sapply(data, is.numeric)
  if (any(numeric_cols)) {
    results$numeric_summary <- data[, numeric_cols, drop = FALSE] %>%
      summarise_all(list(
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE)
      ))
  }
  
  # Outlier detection
  results$outliers <- detect_outliers(data)
  
  return(results)
}

#' Check Data Type Consistency
check_data_types <- function(data) {
  
  type_issues <- list()
  
  for (col in names(data)) {
    values <- data[[col]]
    
    # Check for mixed types in character columns
    if (is.character(values)) {
      # Check if should be numeric
      numeric_values <- suppressWarnings(as.numeric(values))
      non_na_original <- sum(!is.na(values))
      non_na_numeric <- sum(!is.na(numeric_values))
      
      if (non_na_numeric > 0 && non_na_numeric == non_na_original) {
        type_issues[[col]] <- "Should be numeric"
      }
      
      # Check if should be date
      if (any(grepl("\\d{4}-\\d{2}-\\d{2}", values, na.rm = TRUE))) {
        type_issues[[col]] <- "Possibly date format"
      }
    }
    
    # Check for factors that should be character
    if (is.factor(values) && length(levels(values)) > 50) {
      type_issues[[col]] <- "Factor with many levels - consider character"
    }
  }
  
  return(type_issues)
}

#' ========================================
#' 2. OUTLIER DETECTION
#' ========================================

#' Comprehensive Outlier Detection
#' @param data Data frame
#' @param method Method for outlier detection
#' @return List with outlier information
detect_outliers <- function(data, method = "all") {
  
  outliers <- list()
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  for (col in numeric_cols) {
    values <- data[[col]]
    values_clean <- values[!is.na(values)]
    
    if (length(values_clean) < 5) next
    
    outliers[[col]] <- list()
    
    # IQR method
    if (method %in% c("all", "iqr")) {
      Q1 <- quantile(values_clean, 0.25)
      Q3 <- quantile(values_clean, 0.75)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      outliers[[col]]$iqr <- list(
        indices = which(values < lower_bound | values > upper_bound),
        values = values[values < lower_bound | values > upper_bound],
        bounds = c(lower_bound, upper_bound)
      )
    }
    
    # Z-score method
    if (method %in% c("all", "zscore")) {
      z_scores <- abs(scale(values_clean))
      outlier_threshold <- 3
      
      outliers[[col]]$zscore <- list(
        indices = which(abs(scale(values)) > outlier_threshold),
        values = values[abs(scale(values)) > outlier_threshold],
        threshold = outlier_threshold
      )
    }
    
    # Modified Z-score method (more robust)
    if (method %in% c("all", "modified_zscore")) {
      median_val <- median(values_clean)
      mad_val <- mad(values_clean)
      modified_z_scores <- 0.6745 * (values_clean - median_val) / mad_val
      
      outliers[[col]]$modified_zscore <- list(
        indices = which(abs(modified_z_scores) > 3.5),
        values = values_clean[abs(modified_z_scores) > 3.5],
        threshold = 3.5
      )
    }
  }
  
  return(outliers)
}

#' ========================================
#' 3. MISSING DATA HANDLING
#' ========================================

#' Comprehensive Missing Data Handler
#' @param data Data frame with missing values
#' @param strategy Strategy for handling missing data
#' @param columns Specific columns to handle (default: all)
#' @return Data frame with missing values handled
handle_missing_data <- function(data, strategy = "smart", columns = NULL) {
  
  if (is.null(columns)) {
    columns <- names(data)
  }
  
  result_data <- data
  imputation_log <- list()
  
  cat("MISSING DATA HANDLING\n")
  cat("=====================\n\n")
  
  for (col in columns) {
    if (!col %in% names(data)) next
    
    values <- data[[col]]
    missing_count <- sum(is.na(values))
    
    if (missing_count == 0) next
    
    cat("Processing column:", col, "- Missing:", missing_count, "\n")
    
    if (strategy == "smart") {
      # Smart strategy based on data type and missingness pattern
      if (is.numeric(values)) {
        if (missing_count / length(values) < 0.05) {
          # Low missingness - use mean/median
          impute_value <- ifelse(length(unique(values[!is.na(values)])) < 10,
                                median(values, na.rm = TRUE),
                                mean(values, na.rm = TRUE))
        } else {
          # High missingness - use median
          impute_value <- median(values, na.rm = TRUE)
        }
        result_data[[col]][is.na(values)] <- impute_value
        imputation_log[[col]] <- paste("Numeric imputation:", impute_value)
        
      } else if (is.character(values) || is.factor(values)) {
        # For categorical, use mode
        mode_value <- names(sort(table(values), decreasing = TRUE))[1]
        result_data[[col]][is.na(values)] <- mode_value
        imputation_log[[col]] <- paste("Categorical imputation:", mode_value)
        
      } else if (inherits(values, "Date")) {
        # For dates, use median
        median_date <- median(values, na.rm = TRUE)
        result_data[[col]][is.na(values)] <- median_date
        imputation_log[[col]] <- paste("Date imputation:", median_date)
      }
      
    } else if (strategy == "remove") {
      # Remove rows with missing values in specified columns
      result_data <- result_data[!is.na(result_data[[col]]), ]
      imputation_log[[col]] <- "Rows removed"
      
    } else if (strategy == "forward_fill") {
      # Forward fill (carry last observation forward)
      result_data[[col]] <- na.locf(values, na.rm = FALSE)
      imputation_log[[col]] <- "Forward fill applied"
      
    } else if (strategy == "interpolate") {
      # Linear interpolation for numeric data
      if (is.numeric(values)) {
        result_data[[col]] <- approx(seq_along(values), values, 
                                    xout = seq_along(values))$y
        imputation_log[[col]] <- "Linear interpolation"
      }
    }
  }
  
  cat("\nImputation Summary:\n")
  for (col in names(imputation_log)) {
    cat("-", col, ":", imputation_log[[col]], "\n")
  }
  
  attr(result_data, "imputation_log") <- imputation_log
  return(result_data)
}

# Helper function for forward fill
na.locf <- function(x, na.rm = TRUE) {
  x[cumsum(!is.na(x))] 
}

#' ========================================
#' 4. STRING CLEANING
#' ========================================

#' Comprehensive String Cleaning
#' @param data Data frame
#' @param text_columns Character columns to clean
#' @return Data frame with cleaned text
clean_text_data <- function(data, text_columns = NULL) {
  
  if (is.null(text_columns)) {
    text_columns <- names(data)[sapply(data, is.character)]
  }
  
  result_data <- data
  
  cat("TEXT DATA CLEANING\n")
  cat("==================\n\n")
  
  for (col in text_columns) {
    if (!col %in% names(data)) next
    
    original_values <- data[[col]]
    cleaned_values <- original_values
    
    cat("Cleaning column:", col, "\n")
    
    # Remove leading/trailing whitespace
    cleaned_values <- str_trim(cleaned_values)
    
    # Standardize case (convert to title case for names, lowercase for others)
    if (grepl("name", col, ignore.case = TRUE)) {
      cleaned_values <- str_to_title(cleaned_values)
    } else {
      cleaned_values <- str_to_lower(cleaned_values)
    }
    
    # Remove extra whitespace
    cleaned_values <- str_squish(cleaned_values)
    
    # Fix common encoding issues
    cleaned_values <- str_replace_all(cleaned_values, "â€™", "'")
    cleaned_values <- str_replace_all(cleaned_values, "â€œ|â€\\?", '"')
    
    # Remove special characters (keeping alphanumeric, spaces, common punctuation)
    cleaned_values <- str_replace_all(cleaned_values, "[^a-zA-Z0-9\\s\\.,\\-'\"()]", "")
    
    # Handle empty strings
    cleaned_values[cleaned_values == ""] <- NA
    
    result_data[[col]] <- cleaned_values
    
    changes <- sum(original_values != cleaned_values, na.rm = TRUE)
    cat("- Changes made:", changes, "\n")
  }
  
  return(result_data)
}

#' ========================================
#' 5. DATE CLEANING
#' ========================================

#' Date Cleaning and Standardization
#' @param data Data frame
#' @param date_columns Date columns to clean
#' @param target_format Target date format
#' @return Data frame with cleaned dates
clean_date_data <- function(data, date_columns = NULL, target_format = "%Y-%m-%d") {
  
  if (is.null(date_columns)) {
    # Auto-detect potential date columns
    date_columns <- names(data)[sapply(data, function(x) {
      if (is.character(x)) {
        any(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}|\\d{2}-\\d{2}-\\d{4}", x, na.rm = TRUE))
      } else {
        inherits(x, "Date") || inherits(x, "POSIXt")
      }
    })]
  }
  
  result_data <- data
  
  cat("DATE DATA CLEANING\n")
  cat("==================\n\n")
  
  for (col in date_columns) {
    if (!col %in% names(data)) next
    
    original_values <- data[[col]]
    
    cat("Processing date column:", col, "\n")
    
    # Try multiple date formats
    date_formats <- c(
      "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%m-%d-%Y", "%d-%m-%Y",
      "%Y/%m/%d", "%d.%m.%Y", "%m.%d.%Y", "%Y.%m.%d",
      "%B %d, %Y", "%d %B %Y", "%b %d, %Y", "%d %b %Y"
    )
    
    parsed_dates <- NULL
    
    # If already Date/POSIXt, convert to standard format
    if (inherits(original_values, "Date") || inherits(original_values, "POSIXt")) {
      parsed_dates <- as.Date(original_values)
    } else {
      # Try parsing with different formats
      for (fmt in date_formats) {
        test_dates <- as.Date(original_values, format = fmt)
        if (sum(!is.na(test_dates)) > sum(!is.na(parsed_dates))) {
          parsed_dates <- test_dates
          cat("- Best format found:", fmt, "\n")
        }
      }
      
      # If no format works, try automatic parsing
      if (sum(!is.na(parsed_dates)) == 0) {
        parsed_dates <- lubridate::ymd(original_values)
        if (sum(!is.na(parsed_dates)) == 0) {
          parsed_dates <- lubridate::mdy(original_values)
        }
        if (sum(!is.na(parsed_dates)) == 0) {
          parsed_dates <- lubridate::dmy(original_values)
        }
      }
    }
    
    # Validate date ranges (e.g., birth dates shouldn't be in future)
    if (grepl("birth|dob", col, ignore.case = TRUE)) {
      future_dates <- parsed_dates > Sys.Date()
      if (any(future_dates, na.rm = TRUE)) {
        cat("- Warning: Future birth dates detected and set to NA\n")
        parsed_dates[future_dates] <- NA
      }
    }
    
    # Check for unreasonable dates (before 1900 or too far in future)
    unreasonable_past <- parsed_dates < as.Date("1900-01-01")
    unreasonable_future <- parsed_dates > as.Date("2030-12-31")
    
    if (any(unreasonable_past, na.rm = TRUE) || any(unreasonable_future, na.rm = TRUE)) {
      cat("- Warning: Unreasonable dates detected\n")
      parsed_dates[unreasonable_past | unreasonable_future] <- NA
    }
    
    result_data[[col]] <- parsed_dates
    
    success_rate <- sum(!is.na(parsed_dates)) / length(parsed_dates)
    cat("- Parsing success rate:", round(success_rate * 100, 1), "%\n")
  }
  
  return(result_data)
}

#' ========================================
#' 6. DUPLICATE HANDLING
#' ========================================

#' Handle Duplicate Records
#' @param data Data frame
#' @param id_column ID column for duplicate detection
#' @param strategy Strategy for handling duplicates
#' @return Data frame with duplicates handled
handle_duplicates <- function(data, id_column = NULL, strategy = "remove_complete") {
  
  cat("DUPLICATE HANDLING\n")
  cat("==================\n\n")
  
  original_rows <- nrow(data)
  
  if (strategy == "remove_complete") {
    # Remove complete duplicate rows
    result_data <- data[!duplicated(data), ]
    removed <- original_rows - nrow(result_data)
    cat("Complete duplicate rows removed:", removed, "\n")
    
  } else if (strategy == "remove_by_id" && !is.null(id_column)) {
    # Remove duplicates based on ID column, keeping first occurrence
    result_data <- data[!duplicated(data[[id_column]]), ]
    removed <- original_rows - nrow(result_data)
    cat("Duplicate IDs removed:", removed, "\n")
    
  } else if (strategy == "keep_latest" && !is.null(id_column)) {
    # Keep the latest record for each ID (requires date column)
    date_cols <- names(data)[sapply(data, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
    
    if (length(date_cols) > 0) {
      date_col <- date_cols[1]  # Use first date column found
      result_data <- data %>%
        arrange(desc(.data[[date_col]])) %>%
        distinct(.data[[id_column]], .keep_all = TRUE)
      removed <- original_rows - nrow(result_data)
      cat("Kept latest records, removed:", removed, "duplicates\n")
    } else {
      warning("No date column found for 'keep_latest' strategy")
      result_data <- data
    }
    
  } else if (strategy == "flag_only") {
    # Add a flag column instead of removing
    result_data <- data
    if (!is.null(id_column)) {
      result_data$is_duplicate_id <- duplicated(data[[id_column]])
    }
    result_data$is_duplicate_row <- duplicated(data)
    
    dup_ids <- sum(result_data$is_duplicate_id)
    dup_rows <- sum(result_data$is_duplicate_row)
    cat("Flagged duplicate IDs:", dup_ids, "\n")
    cat("Flagged duplicate rows:", dup_rows, "\n")
  }
  
  return(result_data)
}

#' ========================================
#' 7. DATA VALIDATION
#' ========================================

#' Validate Data Quality Post-Cleaning
#' @param data Cleaned data frame
#' @param rules List of validation rules
#' @return Validation results
validate_cleaned_data <- function(data, rules = NULL) {
  
  cat("DATA VALIDATION\n")
  cat("===============\n\n")
  
  validation_results <- list()
  
  # Default validation rules
  if (is.null(rules)) {
    rules <- list(
      no_missing_ids = function(d) !any(is.na(d[, 1])),  # Assuming first column is ID
      reasonable_dates = function(d) {
        date_cols <- sapply(d, function(x) inherits(x, "Date"))
        if (any(date_cols)) {
          dates <- d[, date_cols, drop = FALSE]
          all(dates >= as.Date("1900-01-01") & dates <= Sys.Date() + 365, na.rm = TRUE)
        } else TRUE
      },
      no_negative_amounts = function(d) {
        amount_cols <- names(d)[grepl("amount|salary|price|cost", names(d), ignore.case = TRUE)]
        if (length(amount_cols) > 0) {
          all(d[, amount_cols] >= 0, na.rm = TRUE)
        } else TRUE
      }
    )
  }
  
  # Run validation rules
  for (rule_name in names(rules)) {
    rule_function <- rules[[rule_name]]
    
    tryCatch({
      result <- rule_function(data)
      validation_results[[rule_name]] <- list(
        passed = result,
        message = if (result) "PASS" else "FAIL"
      )
      cat("Rule '", rule_name, "':", validation_results[[rule_name]]$message, "\n")
    }, error = function(e) {
      validation_results[[rule_name]] <- list(
        passed = FALSE,
        message = paste("ERROR:", e$message)
      )
      cat("Rule '", rule_name, "':", validation_results[[rule_name]]$message, "\n")
    })
  }
  
  # Summary
  passed_rules <- sum(sapply(validation_results, function(x) x$passed))
  total_rules <- length(validation_results)
  
  cat("\nValidation Summary:", passed_rules, "/", total_rules, "rules passed\n")
  
  return(validation_results)
}

#' ========================================
#' 8. COMPREHENSIVE CLEANING PIPELINE
#' ========================================

#' Complete Data Cleaning Pipeline
#' @param data Raw data frame
#' @param id_column ID column name
#' @param config Cleaning configuration
#' @return List with cleaned data and cleaning report
comprehensive_data_cleaning <- function(data, id_column = NULL, config = NULL) {
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("    COMPREHENSIVE DATA CLEANING PIPELINE\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  # Default configuration
  if (is.null(config)) {
    config <- list(
      assess_quality = TRUE,
      handle_duplicates = TRUE,
      clean_text = TRUE,
      clean_dates = TRUE,
      handle_missing = TRUE,
      validate_results = TRUE,
      duplicate_strategy = "remove_complete",
      missing_strategy = "smart"
    )
  }
  
  cleaning_report <- list(
    original_shape = dim(data),
    steps_performed = character(),
    issues_found = list(),
    final_shape = NULL
  )
  
  result_data <- data
  
  # Step 1: Initial quality assessment
  if (config$assess_quality) {
    cat("STEP 1: INITIAL QUALITY ASSESSMENT\n")
    cat("===================================\n")
    
    quality_assessment <- assess_data_quality(result_data, id_column)
    cleaning_report$initial_quality <- quality_assessment
    cleaning_report$steps_performed <- c(cleaning_report$steps_performed, "quality_assessment")
    cat("\n")
  }
  
  # Step 2: Handle duplicates
  if (config$handle_duplicates) {
    cat("STEP 2: DUPLICATE HANDLING\n")
    cat("==========================\n")
    
    result_data <- handle_duplicates(result_data, id_column, config$duplicate_strategy)
    cleaning_report$steps_performed <- c(cleaning_report$steps_performed, "duplicate_handling")
    cat("\n")
  }
  
  # Step 3: Clean text data
  if (config$clean_text) {
    cat("STEP 3: TEXT DATA CLEANING\n")
    cat("==========================\n")
    
    result_data <- clean_text_data(result_data)
    cleaning_report$steps_performed <- c(cleaning_report$steps_performed, "text_cleaning")
    cat("\n")
  }
  
  # Step 4: Clean date data
  if (config$clean_dates) {
    cat("STEP 4: DATE DATA CLEANING\n")
    cat("==========================\n")
    
    result_data <- clean_date_data(result_data)
    cleaning_report$steps_performed <- c(cleaning_report$steps_performed, "date_cleaning")
    cat("\n")
  }
  
  # Step 5: Handle missing data
  if (config$handle_missing) {
    cat("STEP 5: MISSING DATA HANDLING\n")
    cat("=============================\n")
    
    result_data <- handle_missing_data(result_data, config$missing_strategy)
    cleaning_report$steps_performed <- c(cleaning_report$steps_performed, "missing_data_handling")
    cat("\n")
  }
  
  # Step 6: Final validation
  if (config$validate_results) {
    cat("STEP 6: FINAL VALIDATION\n")
    cat("========================\n")
    
    validation_results <- validate_cleaned_data(result_data)
    cleaning_report$validation_results <- validation_results
    cleaning_report$steps_performed <- c(cleaning_report$steps_performed, "final_validation")
    cat("\n")
  }
  
  # Final report
  cleaning_report$final_shape <- dim(result_data)
  
  cat("CLEANING PIPELINE COMPLETE\n")
  cat("==========================\n")
  cat("Original shape:", paste(cleaning_report$original_shape, collapse = " x "), "\n")
  cat("Final shape:   ", paste(cleaning_report$final_shape, collapse = " x "), "\n")
  cat("Steps performed:", paste(cleaning_report$steps_performed, collapse = ", "), "\n")
  
  return(list(
    cleaned_data = result_data,
    cleaning_report = cleaning_report
  ))
}

#' ========================================
#' 9. DEMONSTRATION FUNCTION
#' ========================================

#' Generate Messy Data for Cleaning Demo
generate_messy_data <- function(n = 500) {
  set.seed(123)
  
  # Create intentionally messy data
  data.frame(
    id = c(1:n, sample(1:50, 20)),  # Some duplicate IDs
    name = c(
      paste("  ", sample(c("john doe", "JANE SMITH", "Bob Johnson", "mary WILSON"), n-50, replace = TRUE), "  "),
      rep("", 50)  # Some empty names
    ),
    email = c(
      paste0(sample(letters, n-30, replace = TRUE), "@company.com"),
      rep(NA, 30)  # Some missing emails
    ),
    birth_date = c(
      sample(c("1990-05-15", "05/20/1985", "12-03-1992", "1988/07/22", "invalid_date"), n-100, replace = TRUE),
      rep(NA, 100)  # Some missing dates
    ),
    salary = c(
      round(rnorm(n-80, 50000, 15000)),
      rep(NA, 50),  # Missing salaries
      rep(-1000, 30)  # Invalid negative salaries
    ),
    department = c(
      sample(c("sales", "ENGINEERING", "  marketing  ", "HR", ""), n-20, replace = TRUE),
      rep(NA, 20)
    ),
    notes = paste("Some notes with special chars:", sample(c("â€™", "â€œtest", "normal text"), n+20, replace = TRUE)),
    stringsAsFactors = FALSE
  )
}

#' Run Data Cleaning Demonstration
run_data_cleaning_demo <- function(verbose = TRUE) {
  
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("         DATA CLEANING DEMONSTRATION\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }
  
  # Generate messy data
  messy_data <- generate_messy_data(300)
  
  if (verbose) {
    cat("Generated messy dataset with intentional quality issues:\n")
    cat("- Duplicate IDs\n")
    cat("- Inconsistent text formatting\n")
    cat("- Missing values\n")
    cat("- Invalid dates\n")
    cat("- Negative salaries\n")
    cat("- Special characters\n\n")
  }
  
  # Run comprehensive cleaning
  cleaning_results <- comprehensive_data_cleaning(
    data = messy_data,
    id_column = "id",
    config = list(
      assess_quality = TRUE,
      handle_duplicates = TRUE,
      clean_text = TRUE,
      clean_dates = TRUE,
      handle_missing = TRUE,
      validate_results = TRUE,
      duplicate_strategy = "remove_by_id",
      missing_strategy = "smart"
    )
  )
  
  if (verbose) {
    cat("\nCLEANING DEMONSTRATION COMPLETE\n")
    cat("Data quality significantly improved through systematic cleaning pipeline.\n")
  }
  
  invisible(cleaning_results)
}

# Example usage
if (interactive()) {
  # Run comprehensive demonstration
  cleaning_demo_results <- run_data_cleaning_demo(verbose = TRUE)
  
  # Access cleaned data and report
  # cleaned_data <- cleaning_demo_results$cleaned_data
  # report <- cleaning_demo_results$cleaning_report
}
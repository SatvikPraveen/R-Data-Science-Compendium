#' @title Advanced Function Design and Programming in R
#' @description Comprehensive demonstration of function creation, design patterns, and best practices
#' @author Portfolio Developer
#' @date 2025

# =============================================================================
# BASIC FUNCTION CREATION AND ANATOMY
# =============================================================================

#' Basic Function Creation Patterns
#'
#' @description Demonstrates fundamental function creation and design principles
#' @return List of function examples and patterns
#' @export
demonstrate_basic_functions <- function() {
  
  results <- list()
  
  # Simple function with single purpose
  calculate_bmi <- function(weight_kg, height_m) {
    if (!is.numeric(weight_kg) || !is.numeric(height_m)) {
      stop("Weight and height must be numeric")
    }
    if (any(weight_kg <= 0) || any(height_m <= 0)) {
      stop("Weight and height must be positive")
    }
    
    bmi <- weight_kg / (height_m^2)
    
    # Classify BMI
    classification <- ifelse(bmi < 18.5, "Underweight",
                    ifelse(bmi < 25, "Normal weight",
                    ifelse(bmi < 30, "Overweight", "Obese")))
    
    data.frame(
      weight_kg = weight_kg,
      height_m = height_m,
      bmi = round(bmi, 2),
      classification = classification
    )
  }
  
  # Function with default arguments
  summarize_numeric <- function(x, 
                               na.rm = TRUE, 
                               trim = 0, 
                               include_outliers = TRUE,
                               quartiles = TRUE) {
    
    if (!is.numeric(x)) {
      stop("Input must be numeric")
    }
    
    basic_stats <- list(
      n = length(x),
      mean = mean(x, na.rm = na.rm, trim = trim),
      median = median(x, na.rm = na.rm),
      sd = sd(x, na.rm = na.rm),
      min = min(x, na.rm = na.rm),
      max = max(x, na.rm = na.rm),
      range = max(x, na.rm = na.rm) - min(x, na.rm = na.rm)
    )
    
    if (quartiles) {
      basic_stats$quartiles <- quantile(x, na.rm = na.rm)
    }
    
    if (include_outliers) {
      q1 <- quantile(x, 0.25, na.rm = na.rm)
      q3 <- quantile(x, 0.75, na.rm = na.rm)
      iqr <- q3 - q1
      
      basic_stats$outliers <- list(
        lower_bound = q1 - 1.5 * iqr,
        upper_bound = q3 + 1.5 * iqr,
        outlier_values = x[x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)]
      )
    }
    
    basic_stats$missing_count <- sum(is.na(x))
    
    return(basic_stats)
  }
  
  # Function with variable arguments (...)
  flexible_calculator <- function(operation, ..., digits = 2) {
    
    values <- list(...)
    
    # Convert all arguments to numeric
    numeric_values <- tryCatch({
      sapply(values, as.numeric)
    }, error = function(e) {
      stop("All values must be convertible to numeric")
    })
    
    result <- switch(operation,
      "sum" = sum(numeric_values, na.rm = TRUE),
      "mean" = mean(numeric_values, na.rm = TRUE),
      "median" = median(numeric_values, na.rm = TRUE),
      "product" = prod(numeric_values, na.rm = TRUE),
      "max" = max(numeric_values, na.rm = TRUE),
      "min" = min(numeric_values, na.rm = TRUE),
      "sd" = sd(numeric_values, na.rm = TRUE),
      "var" = var(numeric_values, na.rm = TRUE),
      stop("Unsupported operation: ", operation)
    )
    
    round(result, digits)
  }
  
  # Store function examples
  results$functions <- list(
    calculate_bmi = calculate_bmi,
    summarize_numeric = summarize_numeric,
    flexible_calculator = flexible_calculator
  )
  
  # Test the functions
  results$tests <- list(
    bmi_test = calculate_bmi(c(70, 80, 60), c(1.75, 1.80, 1.65)),
    summary_test = summarize_numeric(rnorm(100, 50, 10)),
    calculator_test = list(
      sum_result = flexible_calculator("sum", 1, 2, 3, 4, 5),
      mean_result = flexible_calculator("mean", 10, 20, 30, digits = 3),
      product_result = flexible_calculator("product", 2, 3, 4)
    )
  )
  
  return(results)
}

# =============================================================================
# ADVANCED FUNCTION FEATURES
# =============================================================================

#' Advanced Function Features and Patterns
#'
#' @description Demonstrates advanced function programming techniques
#' @return List of advanced function examples
#' @export
demonstrate_advanced_functions <- function() {
  
  results <- list()
  
  # Function returning multiple values
  comprehensive_analysis <- function(data, target_col) {
    
    if (!is.data.frame(data)) {
      stop("Data must be a data frame")
    }
    
    if (!target_col %in% names(data)) {
      stop("Target column not found in data")
    }
    
    target_data <- data[[target_col]]
    
    # Multiple return values in a list
    analysis_results <- list(
      # Basic statistics
      basic_stats = list(
        n = length(target_data),
        mean = mean(target_data, na.rm = TRUE),
        median = median(target_data, na.rm = TRUE),
        sd = sd(target_data, na.rm = TRUE)
      ),
      
      # Distribution analysis
      distribution = list(
        skewness = calculate_skewness(target_data),
        kurtosis = calculate_kurtosis(target_data),
        normality_test = shapiro.test(sample(target_data, min(5000, length(target_data))))
      ),
      
      # Data quality
      quality = list(
        missing_count = sum(is.na(target_data)),
        missing_pct = round(sum(is.na(target_data)) / length(target_data) * 100, 2),
        unique_count = length(unique(target_data)),
        duplicate_count = sum(duplicated(target_data))
      ),
      
      # Relationships with other variables
      correlations = if (is.numeric(target_data)) {
        numeric_cols <- sapply(data, is.numeric)
        cor(data[numeric_cols], use = "complete.obs")[target_col, ]
      } else {
        NULL
      }
    )
    
    # Add class for method dispatch
    class(analysis_results) <- c("comprehensive_analysis", "list")
    
    return(analysis_results)
  }
  
  # Helper functions for advanced analysis
  calculate_skewness <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    mean_x <- mean(x)
    sd_x <- sd(x)
    
    skew <- sum((x - mean_x)^3) / ((n - 1) * sd_x^3)
    return(skew)
  }
  
  calculate_kurtosis <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    mean_x <- mean(x)
    sd_x <- sd(x)
    
    kurt <- sum((x - mean_x)^4) / ((n - 1) * sd_x^4) - 3
    return(kurt)
  }
  
  # Function with environment manipulation
  create_counter <- function(start = 0) {
    # Closure pattern - function that returns a function
    current_value <- start
    
    list(
      get = function() current_value,
      increment = function(by = 1) {
        current_value <<- current_value + by
        current_value
      },
      decrement = function(by = 1) {
        current_value <<- current_value - by
        current_value
      },
      reset = function(value = start) {
        current_value <<- value
        current_value
      }
    )
  }
  
  # Function factory pattern
  create_validator <- function(validation_rules) {
    
    function(data) {
      validation_results <- list()
      
      for (rule_name in names(validation_rules)) {
        rule <- validation_rules[[rule_name]]
        
        tryCatch({
          validation_results[[rule_name]] <- rule(data)
        }, error = function(e) {
          validation_results[[rule_name]] <- paste("Validation error:", e$message)
        })
      }
      
      validation_results$overall_valid <- all(sapply(validation_results, function(x) {
        is.logical(x) && x == TRUE
      }))
      
      return(validation_results)
    }
  }
  
  # Memoization pattern for expensive computations
  create_memoized_function <- function(fn) {
    cache <- new.env(hash = TRUE)
    
    function(...) {
      # Create key from arguments
      key <- digest::digest(list(...), algo = "md5")
      
      if (exists(key, envir = cache)) {
        return(get(key, envir = cache))
      } else {
        result <- fn(...)
        assign(key, result, envir = cache)
        return(result)
      }
    }
  }
  
  # Example: Expensive Fibonacci function
  fibonacci_slow <- function(n) {
    if (n <= 1) return(n)
    return(fibonacci_slow(n - 1) + fibonacci_slow(n - 2))
  }
  
  # Memoized version
  fibonacci_fast <- create_memoized_function(fibonacci_slow)
  
  # Store advanced function examples
  results$advanced_functions <- list(
    comprehensive_analysis = comprehensive_analysis,
    create_counter = create_counter,
    create_validator = create_validator,
    fibonacci_fast = fibonacci_fast
  )
  
  # Test advanced functions
  test_data <- data.frame(
    x = rnorm(100, 50, 10),
    y = rnorm(100, 30, 5),
    category = sample(c("A", "B", "C"), 100, replace = TRUE)
  )
  
  results$advanced_tests <- list(
    analysis_test = comprehensive_analysis(test_data, "x"),
    
    counter_test = {
      counter <- create_counter(10)
      list(
        initial = counter$get(),
        after_increment = counter$increment(5),
        after_decrement = counter$decrement(2),
        after_reset = counter$reset()
      )
    },
    
    validator_test = {
      rules <- list(
        has_data = function(data) nrow(data) > 0,
        has_x_column = function(data) "x" %in% names(data),
        x_is_numeric = function(data) is.numeric(data$x),
        no_infinite_values = function(data) !any(is.infinite(data$x))
      )
      
      validator <- create_validator(rules)
      validator(test_data)
    },
    
    fibonacci_performance = {
      # Compare performance
      time_slow <- system.time(result_slow <- fibonacci_slow(25))
      time_fast <- system.time(result_fast <- fibonacci_fast(25))
      
      list(
        result = result_slow,
        slow_time = time_slow["elapsed"],
        fast_time = time_fast["elapsed"],
        speedup = time_slow["elapsed"] / time_fast["elapsed"]
      )
    }
  )
  
  return(results)
}

# =============================================================================
# FUNCTIONAL PROGRAMMING PATTERNS
# =============================================================================

#' Functional Programming Patterns in R
#'
#' @description Demonstrates functional programming techniques and higher-order functions
#' @return List of functional programming examples
#' @export
demonstrate_functional_programming <- function() {
  
  results <- list()
  
  # Higher-order functions
  apply_operation <- function(data, operation, ...) {
    switch(operation,
      "transform" = lapply(data, function(x, ...) x * ..1, ...),
      "filter" = Filter(function(x) x > ..1, data, ...),
      "reduce" = Reduce(function(acc, x) acc + x, data),
      "map_if" = lapply(data, function(x) if (x > ..1) x^2 else x, ...),
      stop("Unknown operation")
    )
  }
  
  # Function composition
  compose <- function(...) {
    functions <- list(...)
    
    function(x) {
      result <- x
      for (i in length(functions):1) {
        result <- functions[[i]](result)
      }
      result
    }
  }
  
  # Partial application
  partial <- function(fn, ...) {
    args <- list(...)
    
    function(...) {
      do.call(fn, c(args, list(...)))
    }
  }
  
  # Currying
  curry <- function(fn, arity = length(formals(fn))) {
    
    function(...) {
      args <- list(...)
      
      if (length(args) >= arity) {
        do.call(fn, args[1:arity])
      } else {
        curry(partial(fn, ...), arity - length(args))
      }
    }
  }
  
  # Example functions for composition
  add_ten <- function(x) x + 10
  multiply_by_two <- function(x) x * 2
  square <- function(x) x^2
  
  # Create composed function
  complex_transform <- compose(square, multiply_by_two, add_ten)
  
  # Example: Curried multiplication
  multiply <- function(a, b, c) a * b * c
  curried_multiply <- curry(multiply, 3)
  
  # Pipeline pattern
  pipeline <- function(data, ...) {
    operations <- list(...)
    
    result <- data
    for (op in operations) {
      result <- op(result)
    }
    result
  }
  
  # Data processing functions for pipeline
  remove_na <- function(data) data[!is.na(data)]
  remove_outliers <- function(data) {
    q1 <- quantile(data, 0.25)
    q3 <- quantile(data, 0.75)
    iqr <- q3 - q1
    data[data >= (q1 - 1.5 * iqr) & data <= (q3 + 1.5 * iqr)]
  }
  standardize <- function(data) (data - mean(data)) / sd(data)
  
  # Store functional programming examples
  results$functional_patterns <- list(
    apply_operation = apply_operation,
    compose = compose,
    partial = partial,
    curry = curry,
    pipeline = pipeline,
    complex_transform = complex_transform,
    curried_multiply = curried_multiply
  )
  
  # Test functional programming patterns
  test_data <- c(1, 2, 3, 4, 5, NA, 100, 6, 7, 8)
  
  results$functional_tests <- list(
    apply_operations = list(
      transform = apply_operation(1:5, "transform", 3),
      filter = apply_operation(1:10, "filter", 5),
      reduce = apply_operation(1:5, "reduce")
    ),
    
    composition_test = {
      original <- 5
      step1 <- add_ten(original)
      step2 <- multiply_by_two(step1)
      step3 <- square(step2)
      
      composed_result <- complex_transform(original)
      
      list(
        original = original,
        manual_steps = step3,
        composed = composed_result,
        equivalent = step3 == composed_result
      )
    },
    
    curry_test = {
      multiply_by_2 <- curried_multiply(2)
      multiply_by_2_and_3 <- multiply_by_2(3)
      final_result <- multiply_by_2_and_3(4)
      
      list(
        partial_functions_created = TRUE,
        final_result = final_result,
        expected = 2 * 3 * 4,
        correct = final_result == 24
      )
    },
    
    pipeline_test = {
      raw_data <- c(1, 2, 3, NA, 50, 4, 5, 6, 7, 8)
      
      processed_data <- pipeline(
        raw_data,
        remove_na,
        remove_outliers,
        standardize
      )
      
      list(
        original_length = length(raw_data),
        processed_length = length(processed_data),
        processed_mean = round(mean(processed_data), 6),
        processed_sd = round(sd(processed_data), 6)
      )
    }
  )
  
  return(results)
}

# =============================================================================
# ERROR HANDLING AND ROBUST FUNCTION DESIGN
# =============================================================================

#' Error Handling and Robust Function Design
#'
#' @description Demonstrates error handling patterns and defensive programming
#' @return List of error handling examples
#' @export
demonstrate_error_handling <- function() {
  
  results <- list()
  
  # Basic error handling with tryCatch
  safe_division <- function(a, b) {
    
    result <- tryCatch({
      # Input validation
      if (!is.numeric(a) || !is.numeric(b)) {
        stop("Both arguments must be numeric")
      }
      
      if (b == 0) {
        stop("Division by zero is not allowed")
      }
      
      # Perform operation
      division_result <- a / b
      
      list(
        success = TRUE,
        result = division_result,
        message = "Division completed successfully"
      )
      
    }, error = function(e) {
      list(
        success = FALSE,
        result = NULL,
        message = paste("Error:", e$message),
        error_type = class(e)[1]
      )
      
    }, warning = function(w) {
      list(
        success = TRUE,
        result = a / b,
        message = paste("Warning:", w$message),
        warning_occurred = TRUE
      )
    })
    
    return(result)
  }
  
  # Advanced error handling with custom conditions
  validate_and_process <- function(data, column_name, min_value = NULL, max_value = NULL) {
    
    # Custom condition classes
    validation_error <- function(message, call = NULL) {
      structure(
        list(message = message, call = call),
        class = c("validation_error", "error", "condition")
      )
    }
    
    data_warning <- function(message, call = NULL) {
      structure(
        list(message = message, call = call),
        class = c("data_warning", "warning", "condition")
      )
    }
    
    tryCatch({
      # Validation checks
      if (!is.data.frame(data)) {
        stop(validation_error("Input must be a data frame"))
      }
      
      if (!column_name %in% names(data)) {
        stop(validation_error(paste("Column", column_name, "not found in data")))
      }
      
      column_data <- data[[column_name]]
      
      if (!is.numeric(column_data)) {
        stop(validation_error(paste("Column", column_name, "must be numeric")))
      }
      
      # Check for missing values
      if (any(is.na(column_data))) {
        warning(data_warning(paste("Column", column_name, "contains", sum(is.na(column_data)), "missing values")))
      }
      
      # Range validation
      if (!is.null(min_value) && any(column_data < min_value, na.rm = TRUE)) {
        warning(data_warning(paste("Some values in", column_name, "are below minimum threshold")))
      }
      
      if (!is.null(max_value) && any(column_data > max_value, na.rm = TRUE)) {
        warning(data_warning(paste("Some values in", column_name, "are above maximum threshold")))
      }
      
      # Process data
      processed_data <- data
      processed_data[[paste0(column_name, "_processed")]] <- scale(column_data)[, 1]
      
      list(
        success = TRUE,
        data = processed_data,
        summary = list(
          original_mean = mean(column_data, na.rm = TRUE),
          original_sd = sd(column_data, na.rm = TRUE),
          processed_mean = mean(processed_data[[paste0(column_name, "_processed")]], na.rm = TRUE),
          processed_sd = sd(processed_data[[paste0(column_name, "_processed")]], na.rm = TRUE)
        )
      )
      
    }, validation_error = function(e) {
      list(
        success = FALSE,
        error_type = "validation",
        message = e$message
      )
      
    }, data_warning = function(w) {
      # Re-raise the warning to continue execution
      warning(w)
      invokeRestart("muffleWarning")
      
    }, error = function(e) {
      list(
        success = FALSE,
        error_type = "general",
        message = e$message
      )
    })
  }
  
  # Function with comprehensive logging
  logged_operation <- function(data, operation = "summary", log_level = "INFO") {
    
    log_messages <- character()
    
    log_message <- function(level, message) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_entry <- paste0("[", timestamp, "] [", level, "] ", message)
      log_messages <<- c(log_messages, log_entry)
      
      if (log_level %in% c("DEBUG", "INFO", "WARN", "ERROR")) {
        cat(log_entry, "\n")
      }
    }
    
    tryCatch({
      log_message("INFO", paste("Starting operation:", operation))
      log_message("DEBUG", paste("Input data dimensions:", paste(dim(data), collapse = "x")))
      
      result <- switch(operation,
        "summary" = {
          log_message("INFO", "Computing summary statistics")
          summary(data)
        },
        "correlation" = {
          log_message("INFO", "Computing correlation matrix")
          numeric_cols <- sapply(data, is.numeric)
          if (sum(numeric_cols) < 2) {
            stop("Need at least 2 numeric columns for correlation")
          }
          cor(data[numeric_cols], use = "complete.obs")
        },
        "clean" = {
          log_message("INFO", "Cleaning data")
          initial_rows <- nrow(data)
          clean_data <- data[complete.cases(data), ]
          removed_rows <- initial_rows - nrow(clean_data)
          log_message("WARN", paste("Removed", removed_rows, "rows with missing values"))
          clean_data
        },
        {
          log_message("ERROR", paste("Unknown operation:", operation))
          stop("Unsupported operation")
        }
      )
      
      log_message("INFO", "Operation completed successfully")
      
      list(
        success = TRUE,
        result = result,
        log = log_messages
      )
      
    }, error = function(e) {
      log_message("ERROR", paste("Operation failed:", e$message))
      
      list(
        success = FALSE,
        error = e$message,
        log = log_messages
      )
    })
  }
  
  # Store error handling examples
  results$error_handling_functions <- list(
    safe_division = safe_division,
    validate_and_process = validate_and_process,
    logged_operation = logged_operation
  )
  
  # Test error handling
  results$error_handling_tests <- list(
    safe_division_tests = list(
      normal = safe_division(10, 2),
      division_by_zero = safe_division(10, 0),
      invalid_input = safe_division("a", 2)
    ),
    
    validation_tests = {
      test_data <- data.frame(
        x = c(1, 2, 3, NA, 5),
        y = c("a", "b", "c", "d", "e")
      )
      
      list(
        valid_column = validate_and_process(test_data, "x"),
        invalid_column = validate_and_process(test_data, "z"),
        non_numeric_column = validate_and_process(test_data, "y")
      )
    },
    
    logging_tests = {
      test_data <- data.frame(
        a = 1:10,
        b = rnorm(10),
        c = letters[1:10]
      )
      
      list(
        summary_operation = logged_operation(test_data, "summary", "INFO"),
        correlation_operation = logged_operation(test_data, "correlation", "DEBUG"),
        invalid_operation = logged_operation(test_data, "invalid", "ERROR")
      )
    }
  )
  
  return(results)
}

# =============================================================================
# PERFORMANCE OPTIMIZATION AND PROFILING
# =============================================================================

#' Function Performance Optimization
#'
#' @description Demonstrates performance optimization techniques for functions
#' @return List of performance optimization examples
#' @export
demonstrate_performance_optimization <- function() {
  
  results <- list()
  
  # Example: Matrix multiplication optimization
  matrix_multiply_comparison <- function(n = 500) {
    
    # Generate test matrices
    A <- matrix(rnorm(n * n), nrow = n)
    B <- matrix(rnorm(n * n), nrow = n)
    
    # Method 1: Nested loops (inefficient)
    slow_multiply <- function(A, B) {
      n <- nrow(A)
      C <- matrix(0, nrow = n, ncol = n)
      
      for (i in 1:n) {
        for (j in 1:n) {
          for (k in 1:n) {
            C[i, j] <- C[i, j] + A[i, k] * B[k, j]
          }
        }
      }
      return(C)
    }
    
    # Method 2: Built-in matrix multiplication (efficient)
    fast_multiply <- function(A, B) {
      A %*% B
    }
    
    # Performance comparison
    time_slow <- system.time(result_slow <- slow_multiply(A[1:50, 1:50], B[1:50, 1:50]))
    time_fast <- system.time(result_fast <- fast_multiply(A, B))
    
    list(
      slow_time = time_slow["elapsed"],
      fast_time = time_fast["elapsed"],
      speedup = time_slow["elapsed"] / time_fast["elapsed"],
      results_equivalent = all.equal(result_slow, as.matrix(result_fast[1:50, 1:50]), tolerance = 1e-10)
    )
  }
  
  # Memory-efficient function design
  memory_efficient_processing <- function(data, chunk_size = 1000) {
    
    total_rows <- nrow(data)
    results <- list()
    
    # Process data in chunks to manage memory
    for (i in seq(1, total_rows, by = chunk_size)) {
      end_idx <- min(i + chunk_size - 1, total_rows)
      chunk <- data[i:end_idx, ]
      
      # Process chunk
      chunk_result <- list(
        mean = mean(chunk[[1]], na.rm = TRUE),
        sd = sd(chunk[[1]], na.rm = TRUE),
        range = range(chunk[[1]], na.rm = TRUE)
      )
      
      results[[paste0("chunk_", ceiling(i / chunk_size))]] <- chunk_result
    }
    
    # Combine results
    list(
      chunk_results = results,
      overall_summary = list(
        total_chunks = length(results),
        avg_mean = mean(sapply(results, function(x) x$mean)),
        avg_sd = mean(sapply(results, function(x) x$sd))
      )
    )
  }
  
  # Vectorization example
  vectorization_comparison <- function(n = 10000) {
    
    x <- rnorm(n)
    y <- rnorm(n)
    
    # Slow: Element-wise loop
    slow_distance <- function(x, y) {
      distances <- numeric(length(x))
      for (i in 1:length(x)) {
        distances[i] <- sqrt(x[i]^2 + y[i]^2)
      }
      distances
    }
    
    # Fast: Vectorized operation
    fast_distance <- function(x, y) {
      sqrt(x^2 + y^2)
    }
    
    # Performance comparison
    time_slow <- system.time(result_slow <- slow_distance(x, y))
    time_fast <- system.time(result_fast <- fast_distance(x, y))
    
    list(
      slow_time = time_slow["elapsed"],
      fast_time = time_fast["elapsed"],
      speedup = time_slow["elapsed"] / time_fast["elapsed"],
      results_identical = identical(result_slow, result_fast)
    )
  }
  
  # Store performance optimization examples
  results$optimization_functions <- list(
    matrix_multiply_comparison = matrix_multiply_comparison,
    memory_efficient_processing = memory_efficient_processing,
    vectorization_comparison = vectorization_comparison
  )
  
  # Test performance optimizations
  results$performance_tests <- list(
    matrix_test = matrix_multiply_comparison(100),
    
    memory_test = {
      large_data <- data.frame(values = rnorm(5000))
      memory_efficient_processing(large_data, chunk_size = 500)
    },
    
    vectorization_test = vectorization_comparison(5000)
  )
  
  return(results)
}

# =============================================================================
# COMPREHENSIVE DEMONSTRATION RUNNER
# =============================================================================

#' Run Complete Functions Demonstration
#'
#' @description Executes all function programming demonstrations
#' @param verbose Logical, whether to show detailed output
#' @return Invisible list of all results
#' @export
run_functions_demo <- function(verbose = TRUE) {
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("      ADVANCED FUNCTION PROGRAMMING PORTFOLIO\n")
    cat("="*60, "\n\n")
  }
  
  results <- list()
  
  # Run all demonstrations
  if (verbose) cat("1. Basic function patterns...\n")
  results$basic <- demonstrate_basic_functions()
  
  if (verbose) cat("2. Advanced function features...\n")
  results$advanced <- demonstrate_advanced_functions()
  
  if (verbose) cat("3. Functional programming patterns...\n")
  results$functional <- demonstrate_functional_programming()
  
  if (verbose) cat("4. Error handling and robustness...\n")
  results$error_handling <- demonstrate_error_handling()
  
  if (verbose) cat("5. Performance optimization...\n")
  results$performance <- demonstrate_performance_optimization()
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("    FUNCTION PROGRAMMING DEMONSTRATION COMPLETE\n")
    cat("="*60, "\n")
    
    # Summary statistics
    total_functions <- length(unlist(lapply(results, function(x) x$functions), recursive = FALSE))
    cat("Functions demonstrated:", total_functions, "\n")
    cat("Programming patterns covered:", length(results), "\n")
    cat("Performance improvements shown: up to", 
        round(max(c(
          results$performance$performance_tests$matrix_test$speedup,
          results$performance$performance_tests$vectorization_test$speedup
        ), na.rm = TRUE), 1), "x faster\n")
  }
  
  invisible(results)
}

# =============================================================================
# EXAMPLE USAGE
# =============================================================================

if (interactive()) {
  # Run the complete demonstration
  demo_results <- run_functions_demo(verbose = TRUE)
  
  # Example: Access specific results
  # demo_results$basic$tests$bmi_test
  # demo_results$advanced$advanced_tests$fibonacci_performance
  # demo_results$functional$functional_tests$composition_test
  # demo_results$error_handling$error_handling_tests$safe_division_tests
  # demo_results$performance$performance_tests$vectorization_test
}
#' @title Control Structures and Flow Control in R
#' @description Comprehensive demonstration of R's control flow mechanisms
#' @author Portfolio Developer
#' @date 2025

# =============================================================================
# CONDITIONAL STATEMENTS: IF/ELSE CONSTRUCTS
# =============================================================================

#' Advanced Conditional Logic Demonstration
#'
#' @description Shows sophisticated if/else patterns and logical operations
#' @param data Input data frame for conditional analysis
#' @param threshold Numeric threshold for classification
#' @return List containing conditional analysis results
#' @export
#' @examples
#' test_data <- data.frame(score = c(85, 92, 76, 88, 94), grade = c("B", "A", "C", "B", "A"))
#' demonstrate_conditionals(test_data, threshold = 80)
demonstrate_conditionals <- function(data, threshold = 80) {
  
  # Input validation with informative errors
  if (!is.data.frame(data)) {
    stop("Input must be a data frame", call. = FALSE)
  }
  
  if (!is.numeric(threshold)) {
    stop("Threshold must be numeric", call. = FALSE)
  }
  
  results <- list()
  
  # Simple if/else
  results$simple_conditional <- if (nrow(data) > 0) {
    "Data contains records"
  } else {
    "Data is empty"
  }
  
  # Nested conditionals with multiple criteria
  results$classification <- sapply(data$score, function(score) {
    if (is.na(score)) {
      "Missing"
    } else if (score >= 95) {
      "Exceptional"
    } else if (score >= 90) {
      "Excellent" 
    } else if (score >= threshold) {
      "Good"
    } else if (score >= 70) {
      "Satisfactory"
    } else {
      "Needs Improvement"
    }
  })
  
  # Complex logical operations
  results$complex_logic <- data.frame(
    original_score = data$score,
    above_threshold = data$score > threshold,
    high_performer = data$score > threshold & data$score < 100,
    exceptional_case = data$score >= 95 | (data$score >= 85 & data$grade == "A"),
    needs_review = data$score < threshold | is.na(data$score)
  )
  
  # Vectorized conditional operations (ifelse)
  results$vectorized_conditions <- data.frame(
    score = data$score,
    status = ifelse(data$score >= threshold, "Pass", "Review"),
    bonus = ifelse(data$score >= 90, data$score * 0.1, 0),
    category = ifelse(data$score >= 90, "High", 
                     ifelse(data$score >= threshold, "Medium", "Low"))
  )
  
  # Case when style conditionals (using dplyr-like logic)
  results$case_when_style <- data.frame(
    score = data$score,
    detailed_grade = case_when(
      data$score >= 97 ~ "A+",
      data$score >= 93 ~ "A",
      data$score >= 90 ~ "A-",
      data$score >= 87 ~ "B+",
      data$score >= 83 ~ "B",
      data$score >= 80 ~ "B-",
      data$score >= 77 ~ "C+",
      data$score >= 73 ~ "C",
      data$score >= 70 ~ "C-",
      TRUE ~ "F"
    )
  )
  
  # Statistical conditional analysis
  results$stats <- list(
    total_records = nrow(data),
    above_threshold = sum(data$score > threshold, na.rm = TRUE),
    below_threshold = sum(data$score <= threshold, na.rm = TRUE),
    missing_values = sum(is.na(data$score)),
    pass_rate = round(mean(data$score > threshold, na.rm = TRUE) * 100, 2)
  )
  
  return(results)
}

# =============================================================================
# LOOPS: FOR, WHILE, AND REPEAT CONSTRUCTS
# =============================================================================

#' Advanced Loop Patterns and Iterations
#'
#' @description Demonstrates various loop constructs and best practices
#' @param n Number of iterations for demonstrations
#' @param data_list List of data for processing
#' @return List containing loop operation results
#' @export
demonstrate_loops <- function(n = 10, data_list = NULL) {
  
  if (is.null(data_list)) {
    data_list <- list(
      numbers = 1:20,
      letters = letters[1:10],
      random_data = rnorm(15)
    )
  }
  
  results <- list()
  
  # =============================================================================
  # FOR LOOPS: Various patterns and applications
  # =============================================================================
  
  # Basic for loop with index
  results$basic_for <- {
    output <- numeric(n)
    for (i in 1:n) {
      output[i] <- i^2
    }
    output
  }
  
  # For loop with sequence
  results$sequence_for <- {
    fibonacci <- numeric(n)
    fibonacci[1] <- 1
    fibonacci[2] <- 1
    
    for (i in 3:n) {
      fibonacci[i] <- fibonacci[i-1] + fibonacci[i-2]
    }
    fibonacci
  }
  
  # For loop over names/elements
  results$named_for <- {
    processed_data <- list()
    
    for (name in names(data_list)) {
      data <- data_list[[name]]
      
      if (is.numeric(data)) {
        processed_data[[name]] <- list(
          mean = mean(data, na.rm = TRUE),
          sd = sd(data, na.rm = TRUE),
          length = length(data),
          range = range(data, na.rm = TRUE)
        )
      } else {
        processed_data[[name]] <- list(
          length = length(data),
          unique_count = length(unique(data)),
          first_few = head(data, 3)
        )
      }
    }
    processed_data
  }
  
  # Nested for loops (matrix operations)
  results$nested_for <- {
    matrix_size <- 5
    result_matrix <- matrix(0, nrow = matrix_size, ncol = matrix_size)
    
    for (i in 1:matrix_size) {
      for (j in 1:matrix_size) {
        result_matrix[i, j] <- i * j
      }
    }
    result_matrix
  }
  
  # For loop with conditional breaks and next
  results$conditional_for <- {
    search_results <- list()
    target_sum <- 50
    current_sum <- 0
    
    for (i in 1:100) {
      # Skip even numbers
      if (i %% 2 == 0) {
        next
      }
      
      current_sum <- current_sum + i
      search_results$numbers_added <- c(search_results$numbers_added, i)
      
      # Break if target reached
      if (current_sum >= target_sum) {
        search_results$final_sum <- current_sum
        search_results$iterations <- i
        break
      }
    }
    search_results
  }
  
  # =============================================================================
  # WHILE LOOPS: Condition-based iteration
  # =============================================================================
  
  # Basic while loop
  results$basic_while <- {
    counter <- 1
    results_vec <- numeric()
    
    while (counter <= n) {
      results_vec <- c(results_vec, counter^3)
      counter <- counter + 1
    }
    results_vec
  }
  
  # While loop with complex condition
  results$complex_while <- {
    x <- 100
    iterations <- 0
    sequence <- numeric()
    
    while (x > 1 && iterations < 50) {
      sequence <- c(sequence, x)
      
      if (x %% 2 == 0) {
        x <- x / 2
      } else {
        x <- 3 * x + 1
      }
      
      iterations <- iterations + 1
    }
    
    list(
      collatz_sequence = sequence,
      final_value = x,
      iterations = iterations
    )
  }
  
  # While loop for convergence
  results$convergence_while <- {
    tolerance <- 1e-6
    max_iterations <- 1000
    
    # Newton's method for square root
    target <- 25
    x <- target / 2  # Initial guess
    iteration <- 0
    history <- numeric()
    
    while (abs(x^2 - target) > tolerance && iteration < max_iterations) {
      x <- 0.5 * (x + target / x)
      history <- c(history, x)
      iteration <- iteration + 1
    }
    
    list(
      square_root_estimate = x,
      actual_sqrt = sqrt(target),
      error = abs(x - sqrt(target)),
      iterations = iteration,
      convergence_history = history
    )
  }
  
  # =============================================================================
  # REPEAT LOOPS: Infinite loops with break conditions
  # =============================================================================
  
  results$repeat_loop <- {
    # Monte Carlo estimation of pi
    inside_circle <- 0
    total_points <- 0
    pi_estimates <- numeric()
    
    repeat {
      # Generate random point
      x <- runif(1, -1, 1)
      y <- runif(1, -1, 1)
      
      # Check if inside unit circle
      if (x^2 + y^2 <= 1) {
        inside_circle <- inside_circle + 1
      }
      
      total_points <- total_points + 1
      
      # Estimate pi every 100 points
      if (total_points %% 100 == 0) {
        pi_estimate <- 4 * inside_circle / total_points
        pi_estimates <- c(pi_estimates, pi_estimate)
        
        # Break when estimate is close enough or max iterations reached
        if (abs(pi_estimate - pi) < 0.01 || total_points >= 10000) {
          break
        }
      }
    }
    
    list(
      final_pi_estimate = 4 * inside_circle / total_points,
      actual_pi = pi,
      total_points = total_points,
      accuracy = abs(4 * inside_circle / total_points - pi),
      estimate_history = pi_estimates
    )
  }
  
  return(results)
}

# =============================================================================
# VECTORIZATION VS LOOPS: Performance Comparison
# =============================================================================

#' Performance Comparison: Loops vs Vectorization
#'
#' @description Compares performance of different approaches to computation
#' @param data_size Size of data for performance testing
#' @return List containing timing results and recommendations
#' @export
compare_performance <- function(data_size = 10000) {
  
  # Generate test data
  test_data <- rnorm(data_size)
  
  results <- list()
  
  # Task 1: Square all elements
  cat("Testing squaring operation on", data_size, "elements...\n")
  
  # Method 1: For loop
  results$square_for_loop <- system.time({
    result_for <- numeric(data_size)
    for (i in 1:data_size) {
      result_for[i] <- test_data[i]^2
    }
  })
  
  # Method 2: Vectorized operation
  results$square_vectorized <- system.time({
    result_vec <- test_data^2
  })
  
  # Method 3: sapply
  results$square_sapply <- system.time({
    result_sapply <- sapply(test_data, function(x) x^2)
  })
  
  # Task 2: Cumulative sum
  cat("Testing cumulative sum operation...\n")
  
  # Method 1: For loop
  results$cumsum_for_loop <- system.time({
    cumsum_for <- numeric(data_size)
    cumsum_for[1] <- test_data[1]
    for (i in 2:data_size) {
      cumsum_for[i] <- cumsum_for[i-1] + test_data[i]
    }
  })
  
  # Method 2: Built-in function
  results$cumsum_builtin <- system.time({
    cumsum_builtin <- cumsum(test_data)
  })
  
  # Task 3: Conditional operations
  cat("Testing conditional operations...\n")
  
  # Method 1: For loop with if
  results$conditional_for_loop <- system.time({
    result_cond_for <- numeric(data_size)
    for (i in 1:data_size) {
      if (test_data[i] > 0) {
        result_cond_for[i] <- test_data[i] * 2
      } else {
        result_cond_for[i] <- test_data[i] / 2
      }
    }
  })
  
  # Method 2: Vectorized ifelse
  results$conditional_vectorized <- system.time({
    result_cond_vec <- ifelse(test_data > 0, test_data * 2, test_data / 2)
  })
  
  # Performance summary
  results$performance_summary <- data.frame(
    Operation = c("Square (For Loop)", "Square (Vectorized)", "Square (sapply)",
                 "Cumsum (For Loop)", "Cumsum (Built-in)",
                 "Conditional (For Loop)", "Conditional (Vectorized)"),
    
    Time_Elapsed = c(
      results$square_for_loop["elapsed"],
      results$square_vectorized["elapsed"], 
      results$square_sapply["elapsed"],
      results$cumsum_for_loop["elapsed"],
      results$cumsum_builtin["elapsed"],
      results$conditional_for_loop["elapsed"],
      results$conditional_vectorized["elapsed"]
    ),
    
    Relative_Speed = c(
      results$square_vectorized["elapsed"] / results$square_for_loop["elapsed"],
      1,
      results$square_vectorized["elapsed"] / results$square_sapply["elapsed"],
      results$cumsum_builtin["elapsed"] / results$cumsum_for_loop["elapsed"],
      1,
      results$conditional_vectorized["elapsed"] / results$conditional_for_loop["elapsed"],
      1
    )
  )
  
  # Recommendations
  results$recommendations <- list(
    "Use vectorized operations whenever possible - they are typically 10-100x faster",
    "Avoid explicit loops for simple mathematical operations",
    "When loops are necessary, pre-allocate result vectors",
    "Consider apply family functions for complex operations",
    "Use built-in functions (cumsum, cummax, etc.) over manual implementations",
    "Profile your code to identify bottlenecks"
  )
  
  return(results)
}

# =============================================================================
# ADVANCED CONTROL FLOW PATTERNS
# =============================================================================

#' Advanced Control Flow Patterns
#'
#' @description Demonstrates sophisticated control flow techniques
#' @param data Input data for processing
#' @return List of advanced control flow examples
#' @export
advanced_control_patterns <- function(data = NULL) {
  
  if (is.null(data)) {
    data <- data.frame(
      id = 1:100,
      category = sample(c("A", "B", "C"), 100, replace = TRUE),
      value = rnorm(100, 50, 15),
      status = sample(c("active", "inactive", "pending"), 100, replace = TRUE),
      priority = sample(1:5, 100, replace = TRUE)
    )
  }
  
  results <- list()
  
  # Pattern 1: Switch statement equivalent
  results$switch_pattern <- {
    processing_results <- sapply(data$category, function(cat) {
      switch(cat,
        "A" = "High Priority Processing",
        "B" = "Standard Processing", 
        "C" = "Low Priority Processing",
        "Unknown Category"  # default case
      )
    })
    
    table(processing_results)
  }
  
  # Pattern 2: Multiple condition checking with short-circuit evaluation
  results$short_circuit_evaluation <- {
    safe_operations <- sapply(1:nrow(data), function(i) {
      # Short-circuit: if first condition fails, others aren't evaluated
      if (!is.na(data$value[i]) && data$value[i] > 0 && data$status[i] == "active") {
        "Process"
      } else {
        "Skip"
      }
    })
    
    table(safe_operations)
  }
  
  # Pattern 3: Error handling with tryCatch in loops
  results$error_handling_loops <- {
    risky_operations <- list()
    error_log <- character()
    
    for (i in 1:20) {
      result <- tryCatch({
        # Potentially risky operation
        if (i %% 7 == 0) {
          stop("Simulated error at iteration ", i)
        }
        
        # Normal operation
        sqrt(i) * log(i + 1)
        
      }, error = function(e) {
        error_log <<- c(error_log, paste("Error at", i, ":", e$message))
        NA  # Return NA for failed operations
      }, warning = function(w) {
        warning("Warning at iteration ", i, ": ", w$message)
        sqrt(i) * log(i + 1)  # Continue with warning
      })
      
      risky_operations[[i]] <- result
    }
    
    list(
      results = unlist(risky_operations),
      errors = error_log,
      success_rate = sum(!is.na(unlist(risky_operations))) / length(risky_operations)
    )
  }
  
  # Pattern 4: Conditional early returns (using functions)
  results$early_return_pattern <- {
    
    process_record <- function(record) {
      # Early return patterns
      if (is.na(record$value)) {
        return(list(status = "skipped", reason = "missing_value"))
      }
      
      if (record$status != "active") {
        return(list(status = "skipped", reason = "inactive"))
      }
      
      if (record$priority < 3) {
        return(list(status = "skipped", reason = "low_priority"))
      }
      
      # Main processing (only reached if all conditions pass)
      processed_value <- record$value * record$priority
      
      return(list(
        status = "processed",
        original_value = record$value,
        processed_value = processed_value,
        category = record$category
      ))
    }
    
    # Apply to subset of data
    sample_indices <- sample(nrow(data), 20)
    processing_results <- lapply(sample_indices, function(i) {
      process_record(data[i, ])
    })
    
    # Summarize results
    status_summary <- table(sapply(processing_results, function(x) x$status))
    
    list(
      detailed_results = processing_results,
      status_summary = status_summary
    )
  }
  
  # Pattern 5: State machine implementation
  results$state_machine <- {
    
    # Simple state machine for order processing
    process_order <- function(initial_state = "received", events = c("validate", "process", "ship")) {
      
      state <- initial_state
      state_history <- state
      
      for (event in events) {
        new_state <- switch(paste(state, event, sep = "_"),
          "received_validate" = "validated",
          "validated_process" = "processing", 
          "processing_ship" = "shipped",
          "shipped_deliver" = "delivered",
          # Error states
          "received_process" = "error",  # Can't process without validation
          "received_ship" = "error",     # Can't ship without processing
          state  # Stay in current state if transition not valid
        )
        
        state <- new_state
        state_history <- c(state_history, state)
      }
      
      list(
        final_state = state,
        state_history = state_history,
        valid_transition = !any(state_history == "error")
      )
    }
    
    # Test different scenarios
    scenarios <- list(
      normal_flow = c("validate", "process", "ship"),
      skip_validation = c("process", "ship"),
      incomplete = c("validate", "process"),
      invalid_sequence = c("ship", "validate")
    )
    
    scenario_results <- lapply(scenarios, function(events) {
      process_order("received", events)
    })
    
    names(scenario_results) <- names(scenarios)
    scenario_results
  }
  
  return(results)
}

# =============================================================================
# FUNCTIONAL CONTROL STRUCTURES
# =============================================================================

#' Functional Approach to Control Structures
#'
#' @description Demonstrates functional programming patterns for control flow
#' @param data Input data for functional operations
#' @return List of functional programming examples
#' @export
functional_control_patterns <- function(data = 1:100) {
  
  results <- list()
  
  # Higher-order functions for control flow
  results$map_operations <- {
    # Map with different transformations
    list(
      squares = Map(function(x) x^2, data),
      conditionals = Map(function(x) if (x %% 2 == 0) x/2 else x*3, data),
      classifications = Map(function(x) {
        if (x <= 25) "Low"
        else if (x <= 75) "Medium" 
        else "High"
      }, data)
    )
  }
  
  # Filter operations
  results$filter_operations <- {
    list(
      evens = Filter(function(x) x %% 2 == 0, data),
      primes = Filter(function(x) {
        if (x < 2) return(FALSE)
        if (x == 2) return(TRUE)
        !any(x %% 2:sqrt(x) == 0)
      }, data[data <= 100]),
      outliers = Filter(function(x) abs(x - mean(data)) > 2 * sd(data), data)
    )
  }
  
  # Reduce operations
  results$reduce_operations <- {
    list(
      sum = Reduce("+", data),
      product = Reduce("*", data[1:10]),  # Subset to avoid overflow
      max = Reduce(max, data),
      cumulative_sum = Reduce(function(acc, x) c(acc, acc[length(acc)] + x), 
                             data[1:10], init = 0)
    )
  }
  
  return(results)
}

# =============================================================================
# DEMONSTRATION RUNNER
# =============================================================================

#' Run Complete Control Structures Demonstration
#'
#' @description Executes all control structure demonstrations
#' @param verbose Logical, whether to show detailed output
#' @return Invisible list of all results
#' @export
run_control_structures_demo <- function(verbose = TRUE) {
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("    CONTROL STRUCTURES PORTFOLIO DEMONSTRATION\n")
    cat("="*60, "\n\n")
  }
  
  results <- list()
  
  # Test data
  test_data <- data.frame(
    score = c(85, 92, 76, 88, 94, 67, 91, 83, 95, 72),
    grade = c("B", "A", "C", "B", "A", "D", "A", "B", "A", "C")
  )
  
  # Run demonstrations
  if (verbose) cat("1. Testing conditional statements...\n")
  results$conditionals <- demonstrate_conditionals(test_data, threshold = 80)
  
  if (verbose) cat("2. Testing loop constructs...\n")
  results$loops <- demonstrate_loops(n = 10)
  
  if (verbose) cat("3. Performance comparison...\n")
  results$performance <- compare_performance(data_size = 5000)
  
  if (verbose) cat("4. Advanced control patterns...\n")
  results$advanced_patterns <- advanced_control_patterns()
  
  if (verbose) cat("5. Functional control patterns...\n")
  results$functional_patterns <- functional_control_patterns(1:50)
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("    CONTROL STRUCTURES DEMONSTRATION COMPLETE\n") 
    cat("="*60, "\n")
    
    # Performance insights
    perf_summary <- results$performance$performance_summary
    cat("Performance Insights:\n")
    cat("- Vectorized operations are", 
        round(max(perf_summary$Relative_Speed, na.rm = TRUE), 1), 
        "x faster on average\n")
    cat("- Completed", nrow(perf_summary), "performance tests\n")
    cat("- Demonstrated", length(results), "major control pattern categories\n")
  }
  
  invisible(results)
}

# =============================================================================
# EXAMPLE USAGE
# =============================================================================

if (interactive()) {
  # Run the complete demonstration
  demo_results <- run_control_structures_demo(verbose = TRUE)
  
  # Example: Access specific results
  # demo_results$conditionals$classification
  # demo_results$loops$fibonacci_sequence  
  # demo_results$performance$performance_summary
}
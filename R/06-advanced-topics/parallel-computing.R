# @title Parallel Computing and High Performance R
# @description Comprehensive parallel processing, performance optimization, and scalability
# @author R Data Science Portfolio Developer
# @date 2025

#' ========================================
#' PARALLEL COMPUTING IN R
#' ========================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  parallel, foreach, doParallel, doFuture, future, 
  future.apply, snow, Rmpi, microbenchmark, profvis,
  data.table, Rcpp, RcppArmadillo, bigmemory, ff
)

# Detect available cores
available_cores <- parallel::detectCores()
cat("Available CPU cores:", available_cores, "\n")

#' ========================================
#' 1. BASIC PARALLEL OPERATIONS
#' ========================================

#' Parallel Apply Functions
#' @param X Vector or list to apply function over
#' @param FUN Function to apply
#' @param cores Number of cores to use
#' @param ... Additional arguments to FUN
#' @return Results of parallel apply
parallel_apply <- function(X, FUN, cores = available_cores - 1, ...) {
  
  # Create cluster
  cl <- makeCluster(cores)
  on.exit(stopCluster(cl))
  
  # Export necessary objects to cluster
  clusterEvalQ(cl, library(base))
  
  # Parallel apply
  results <- parLapply(cl, X, FUN, ...)
  
  return(results)
}

#' Parallel Sapply with Load Balancing
#' @param X Vector to apply function over
#' @param FUN Function to apply
#' @param cores Number of cores
#' @param chunk_size Size of work chunks
#' @return Simplified results
parallel_sapply_balanced <- function(X, FUN, cores = available_cores - 1, chunk_size = NULL) {
  
  if (is.null(chunk_size)) {
    chunk_size <- max(1, length(X) %/% (cores * 4))
  }
  
  cl <- makeCluster(cores)
  on.exit(stopCluster(cl))
  
  # Load balancing
  clusterSetRNGStream(cl, 123)
  results <- parSapplyLB(cl, X, FUN, chunk.size = chunk_size)
  
  return(results)
}

#' Parallel Map with Error Handling
#' @param .x Vector to iterate over
#' @param .f Function to apply
#' @param .cores Number of cores
#' @param .packages Packages to load on workers
#' @return List of results with error handling
safe_parallel_map <- function(.x, .f, .cores = available_cores - 1, .packages = NULL) {
  
  cl <- makeCluster(.cores)
  on.exit(stopCluster(cl))
  
  # Load packages on workers
  if (!is.null(.packages)) {
    clusterEvalQ(cl, {
      for (pkg in .packages) {
        library(pkg, character.only = TRUE)
      }
    })
    clusterExport(cl, ".packages", envir = environment())
  }
  
  # Safe wrapper function
  safe_function <- function(x) {
    tryCatch(.f(x), error = function(e) {
      list(error = TRUE, message = e$message, value = NA)
    })
  }
  
  results <- parLapply(cl, .x, safe_function)
  
  return(results)
}

#' ========================================
#' 2. FOREACH PARALLEL LOOPS
#' ========================================

#' Setup Parallel Backend
#' @param backend Type of backend ("doParallel", "doFuture")
#' @param workers Number of workers
setup_parallel_backend <- function(backend = "doParallel", workers = available_cores - 1) {
  
  if (backend == "doParallel") {
    cl <- makeCluster(workers)
    registerDoParallel(cl)
    
    cat("Registered doParallel backend with", workers, "workers\n")
    
    # Return cleanup function
    return(function() stopCluster(cl))
    
  } else if (backend == "doFuture") {
    plan(multisession, workers = workers)
    registerDoFuture()
    
    cat("Registered doFuture backend with", workers, "workers\n")
    
    # Return cleanup function
    return(function() plan(sequential))
  }
}

#' Parallel For Loop with Progress
#' @param .x Iteration vector
#' @param .f Function to apply
#' @param .combine How to combine results
#' @param .packages Packages needed
#' @param .export Variables to export
#' @return Combined results
parallel_foreach <- function(.x, .f, .combine = c, .packages = NULL, .export = NULL) {
  
  # Setup progress if interactive
  if (interactive()) {
    .options.snow <- list(progress = function(n) cat(sprintf("Progress: %d%%\r", round(100*n/length(.x)))))
  } else {
    .options.snow <- list()
  }
  
  results <- foreach(
    i = .x,
    .combine = .combine,
    .packages = .packages,
    .export = .export,
    .options.snow = .options.snow
  ) %dopar% {
    .f(i)
  }
  
  if (interactive()) cat("\n")
  
  return(results)
}

#' Parallel Cross-Validation
#' @param data Dataset
#' @param formula Model formula
#' @param k Number of folds
#' @param model_func Model function (lm, glm, etc.)
#' @return Cross-validation results
parallel_cross_validation <- function(data, formula, k = 10, model_func = lm) {
  
  # Create folds
  set.seed(42)
  folds <- sample(rep(1:k, length.out = nrow(data)))
  
  # Parallel CV
  cleanup <- setup_parallel_backend("doParallel")
  on.exit(cleanup())
  
  cv_results <- foreach(
    fold = 1:k,
    .combine = rbind,
    .packages = c("stats"),
    .export = c("data", "formula", "model_func", "folds")
  ) %dopar% {
    
    # Split data
    train_data <- data[folds != fold, ]
    test_data <- data[folds == fold, ]
    
    # Fit model
    model <- model_func(formula, data = train_data)
    
    # Predict
    if (inherits(model, "lm")) {
      predictions <- predict(model, test_data)
      actual <- test_data[[all.vars(formula)[1]]]
      
      # Calculate metrics
      rmse <- sqrt(mean((actual - predictions)^2))
      mae <- mean(abs(actual - predictions))
      r2 <- cor(actual, predictions)^2
      
      data.frame(fold = fold, rmse = rmse, mae = mae, r2 = r2)
      
    } else if (inherits(model, "glm")) {
      predictions <- predict(model, test_data, type = "response")
      actual <- test_data[[all.vars(formula)[1]]]
      
      # For binary classification
      pred_class <- ifelse(predictions > 0.5, 1, 0)
      accuracy <- mean(actual == pred_class)
      
      data.frame(fold = fold, accuracy = accuracy)
    }
  }
  
  return(cv_results)
}

#' ========================================
#' 3. FUTURE-BASED PARALLEL COMPUTING
#' ========================================

#' Async Data Processing Pipeline
#' @param data_list List of datasets
#' @param processing_func Function to apply to each dataset
#' @param plan_type Future plan type
#' @return List of processed datasets
async_data_pipeline <- function(data_list, processing_func, plan_type = "multisession") {
  
  # Set future plan
  old_plan <- plan(plan_type)
  on.exit(plan(old_plan))
  
  # Create futures
  futures_list <- lapply(data_list, function(data) {
    future(processing_func(data))
  })
  
  # Collect results
  results <- lapply(futures_list, value)
  
  return(results)
}

#' Parallel Grid Search
#' @param param_grid Data frame of parameter combinations
#' @param train_func Training function
#' @param eval_func Evaluation function
#' @param data Training data
#' @return Best parameters and performance
parallel_grid_search <- function(param_grid, train_func, eval_func, data) {
  
  plan(multisession)
  on.exit(plan(sequential))
  
  # Create futures for each parameter combination
  futures_list <- apply(param_grid, 1, function(params) {
    future({
      # Train model with these parameters
      model <- do.call(train_func, c(list(data = data), as.list(params)))
      
      # Evaluate model
      performance <- eval_func(model, data)
      
      list(params = params, performance = performance, model = model)
    })
  })
  
  # Collect all results
  all_results <- lapply(futures_list, value)
  
  # Find best result
  performances <- sapply(all_results, function(x) x$performance)
  best_idx <- which.max(performances)
  
  return(list(
    best_params = all_results[[best_idx]]$params,
    best_performance = all_results[[best_idx]]$performance,
    best_model = all_results[[best_idx]]$model,
    all_results = all_results
  ))
}

#' ========================================
#' 4. BIG DATA AND MEMORY MANAGEMENT
#' ========================================

#' Process Large Files in Chunks
#' @param filename Path to large file
#' @param chunk_size Number of rows per chunk
#' @param process_func Function to apply to each chunk
#' @param combine_func Function to combine chunk results
#' @return Combined results
process_large_file_parallel <- function(filename, chunk_size = 10000, 
                                       process_func, combine_func = rbind) {
  
  # Read file info
  total_lines <- length(readLines(filename))
  n_chunks <- ceiling(total_lines / chunk_size)
  
  cat("Processing", total_lines, "lines in", n_chunks, "chunks\n")
  
  # Setup parallel processing
  cleanup <- setup_parallel_backend("doParallel")
  on.exit(cleanup())
  
  # Process chunks in parallel
  chunk_results <- foreach(
    chunk_idx = 1:n_chunks,
    .combine = combine_func,
    .packages = c("utils"),
    .export = c("filename", "chunk_size", "process_func")
  ) %dopar% {
    
    # Calculate row range for this chunk
    start_row <- (chunk_idx - 1) * chunk_size + 1
    end_row <- min(chunk_idx * chunk_size, total_lines)
    
    # Read chunk
    if (tools::file_ext(filename) == "csv") {
      chunk_data <- read.csv(filename, skip = start_row - 1, nrows = end_row - start_row + 1)
    } else {
      # Generic read
      chunk_data <- read.table(filename, skip = start_row - 1, nrows = end_row - start_row + 1)
    }
    
    # Process chunk
    process_func(chunk_data)
  }
  
  return(chunk_results)
}

#' Memory-Efficient Data Processing
#' @param data Large dataset
#' @param batch_size Size of processing batches
#' @param process_func Function to apply to each batch
#' @return Processed results
memory_efficient_processing <- function(data, batch_size = 1000, process_func) {
  
  n_rows <- nrow(data)
  n_batches <- ceiling(n_rows / batch_size)
  
  results <- list()
  
  for (batch_idx in 1:n_batches) {
    # Calculate batch indices
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, n_rows)
    
    # Extract batch
    batch_data <- data[start_idx:end_idx, ]
    
    # Process batch
    batch_result <- process_func(batch_data)
    
    # Store result
    results[[batch_idx]] <- batch_result
    
    # Cleanup memory
    rm(batch_data)
    gc()
    
    # Progress update
    if (batch_idx %% 10 == 0) {
      cat("Processed", batch_idx, "of", n_batches, "batches\n")
    }
  }
  
  return(results)
}

#' ========================================
#' 5. PERFORMANCE PROFILING AND OPTIMIZATION
#' ========================================

#' Benchmark Function Performance
#' @param expr_list Named list of expressions to benchmark
#' @param times Number of times to run each expression
#' @param setup Setup expression
#' @return Benchmark results
benchmark_performance <- function(expr_list, times = 10, setup = NULL) {
  
  if (!is.null(setup)) {
    eval(setup)
  }
  
  # Create microbenchmark list
  mb_list <- expr_list
  
  # Run benchmark
  results <- microbenchmark(
    list = mb_list,
    times = times,
    unit = "ms"
  )
  
  # Summarize results
  summary_stats <- summary(results)
  
  cat("Performance Benchmark Results:\n")
  print(summary_stats)
  
  # Create comparison plot
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plot_obj <- ggplot2::autoplot(results)
    print(plot_obj)
  }
  
  return(list(
    results = results,
    summary = summary_stats
  ))
}

#' Profile Function Memory Usage
#' @param expr Expression to profile
#' @param interval Profiling interval in seconds
#' @return Profiling results
profile_memory_usage <- function(expr, interval = 0.01) {
  
  if (!requireNamespace("profvis", quietly = TRUE)) {
    stop("profvis package required for memory profiling")
  }
  
  # Profile the expression
  prof_results <- profvis::profvis(expr, interval = interval)
  
  # Print summary
  cat("Memory profiling completed\n")
  
  return(prof_results)
}

#' Optimize Data.table Operations
#' @param dt Data.table object
#' @param operations List of operations to benchmark
#' @return Optimized operation recommendations
optimize_datatable_ops <- function(dt, operations) {
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package required")
  }
  
  # Ensure it's a data.table
  if (!data.table::is.data.table(dt)) {
    dt <- data.table::as.data.table(dt)
  }
  
  # Benchmark different approaches
  results <- list()
  
  for (op_name in names(operations)) {
    op_expr <- operations[[op_name]]
    
    # Time the operation
    timing <- system.time({
      result <- eval(op_expr, envir = list(dt = dt))
    })
    
    results[[op_name]] <- list(
      elapsed_time = timing["elapsed"],
      result_size = object.size(result),
      operation = op_expr
    )
  }
  
  # Sort by performance
  performance_order <- order(sapply(results, function(x) x$elapsed_time))
  
  cat("Data.table Operation Performance Ranking:\n")
  for (i in performance_order) {
    op_name <- names(results)[i]
    cat(sprintf("%d. %s: %.3f seconds\n", 
                which(performance_order == i), op_name, results[[i]]$elapsed_time))
  }
  
  return(results)
}

#' ========================================
#' 6. DISTRIBUTED COMPUTING SIMULATION
#' ========================================

#' Simulate MapReduce Operation
#' @param data Large dataset
#' @param map_func Mapper function
#' @param reduce_func Reducer function
#' @param n_mappers Number of mapper processes
#' @return Reduced results
simulate_mapreduce <- function(data, map_func, reduce_func, n_mappers = 4) {
  
  # Split data into chunks for mappers
  chunk_size <- ceiling(nrow(data) / n_mappers)
  data_chunks <- split(data, rep(1:n_mappers, each = chunk_size, length.out = nrow(data)))
  
  cat("Starting MapReduce simulation with", n_mappers, "mappers\n")
  
  # Map phase (parallel)
  cleanup <- setup_parallel_backend("doParallel", workers = n_mappers)
  on.exit(cleanup())
  
  map_results <- foreach(
    chunk = data_chunks,
    .combine = c,
    .export = c("map_func")
  ) %dopar% {
    map_func(chunk)
  }
  
  cat("Map phase completed, starting reduce phase\n")
  
  # Reduce phase
  final_result <- reduce_func(map_results)
  
  cat("MapReduce simulation completed\n")
  
  return(final_result)
}

#' Distributed Data Analysis
#' @param data_sources List of data sources (files, connections, etc.)
#' @param analysis_func Analysis function to apply to each source
#' @param aggregate_func Function to aggregate results
#' @return Aggregated analysis results
distributed_analysis <- function(data_sources, analysis_func, aggregate_func) {
  
  cat("Starting distributed analysis on", length(data_sources), "data sources\n")
  
  # Process each data source in parallel
  plan(multisession)
  on.exit(plan(sequential))
  
  # Create futures for each data source
  analysis_futures <- lapply(data_sources, function(source) {
    future({
      # Load/read data from source
      if (is.character(source)) {
        # Assume it's a filename
        data <- read.csv(source)
      } else {
        data <- source
      }
      
      # Perform analysis
      analysis_func(data)
    })
  })
  
  # Collect results
  analysis_results <- lapply(analysis_futures, value)
  
  # Aggregate results
  final_result <- aggregate_func(analysis_results)
  
  cat("Distributed analysis completed\n")
  
  return(final_result)
}

#' ========================================
#' 7. SPECIALIZED PARALLEL ALGORITHMS
#' ========================================

#' Parallel Monte Carlo Simulation
#' @param n_simulations Total number of simulations
#' @param simulation_func Function that runs one simulation
#' @param n_cores Number of cores to use
#' @param seed Random seed for reproducibility
#' @return Simulation results
parallel_monte_carlo <- function(n_simulations, simulation_func, 
                                n_cores = available_cores - 1, seed = 123) {
  
  # Calculate simulations per core
  sims_per_core <- ceiling(n_simulations / n_cores)
  
  # Create cluster with different seeds for each worker
  cl <- makeCluster(n_cores)
  on.exit(stopCluster(cl))
  
  # Set different random seeds for each worker
  clusterSetRNGStream(cl, seed)
  
  # Export simulation function
  clusterExport(cl, c("simulation_func"), envir = environment())
  
  # Run simulations in parallel
  results <- parLapply(cl, 1:n_cores, function(core_id) {
    
    # Run simulations for this core
    core_results <- replicate(sims_per_core, simulation_func(), simplify = FALSE)
    
    return(core_results)
  })
  
  # Flatten results
  all_results <- unlist(results, recursive = FALSE)
  
  # Trim to exact number requested
  all_results <- all_results[1:min(n_simulations, length(all_results))]
  
  cat("Completed", length(all_results), "Monte Carlo simulations\n")
  
  return(all_results)
}

#' Parallel Bootstrap Sampling
#' @param data Original dataset
#' @param statistic_func Function to calculate statistic
#' @param n_bootstrap Number of bootstrap samples
#' @param n_cores Number of cores
#' @return Bootstrap distribution
parallel_bootstrap <- function(data, statistic_func, n_bootstrap = 1000, 
                              n_cores = available_cores - 1) {
  
  n <- nrow(data)
  boots_per_core <- ceiling(n_bootstrap / n_cores)
  
  cl <- makeCluster(n_cores)
  on.exit(stopCluster(cl))
  
  clusterSetRNGStream(cl, 123)
  clusterExport(cl, c("data", "statistic_func", "n", "boots_per_core"), 
                envir = environment())
  
  bootstrap_results <- parLapply(cl, 1:n_cores, function(core_id) {
    
    core_results <- numeric(boots_per_core)
    
    for (i in 1:boots_per_core) {
      # Create bootstrap sample
      boot_indices <- sample(n, n, replace = TRUE)
      boot_sample <- data[boot_indices, ]
      
      # Calculate statistic
      core_results[i] <- statistic_func(boot_sample)
    }
    
    return(core_results)
  })
  
  # Combine results
  all_bootstrap_stats <- unlist(bootstrap_results)
  
  # Trim to exact number requested
  all_bootstrap_stats <- all_bootstrap_stats[1:min(n_bootstrap, length(all_bootstrap_stats))]
  
  return(all_bootstrap_stats)
}

#' Parallel K-means Clustering
#' @param data Dataset for clustering
#' @param k_values Vector of k values to try
#' @param n_cores Number of cores
#' @param n_starts Number of random starts per k
#' @return Best clustering result
parallel_kmeans_selection <- function(data, k_values, n_cores = available_cores - 1, n_starts = 25) {
  
  cl <- makeCluster(n_cores)
  on.exit(stopCluster(cl))
  
  clusterEvalQ(cl, library(stats))
  clusterExport(cl, c("data", "n_starts"), envir = environment())
  
  # Test each k value in parallel
  kmeans_results <- parLapply(cl, k_values, function(k) {
    
    # Run multiple starts for this k
    best_result <- NULL
    best_tot_withinss <- Inf
    
    for (start in 1:n_starts) {
      result <- kmeans(data, centers = k, nstart = 1)
      
      if (result$tot.withinss < best_tot_withinss) {
        best_result <- result
        best_tot_withinss <- result$tot.withinss
      }
    }
    
    return(list(
      k = k,
      result = best_result,
      tot_withinss = best_tot_withinss,
      between_ss = best_result$betweenss,
      total_ss = best_result$totss
    ))
  })
  
  # Calculate metrics for k selection
  k_metrics <- data.frame(
    k = sapply(kmeans_results, function(x) x$k),
    tot_withinss = sapply(kmeans_results, function(x) x$tot_withinss),
    between_ss = sapply(kmeans_results, function(x) x$between_ss),
    total_ss = sapply(kmeans_results, function(x) x$total_ss)
  )
  
  k_metrics$within_ratio <- k_metrics$tot_withinss / k_metrics$total_ss
  
  # Find elbow point (simplified)
  if (length(k_values) > 2) {
    wss_diff <- diff(k_metrics$tot_withinss)
    wss_diff2 <- diff(wss_diff)
    optimal_k_idx <- which.max(wss_diff2) + 1
    optimal_k <- k_values[optimal_k_idx]
  } else {
    optimal_k <- k_values[which.min(k_metrics$tot_withinss)]
  }
  
  cat("Optimal k selected:", optimal_k, "\n")
  
  return(list(
    optimal_k = optimal_k,
    best_result = kmeans_results[[which(k_values == optimal_k)]]$result,
    all_results = kmeans_results,
    metrics = k_metrics
  ))
}

#' ========================================
#' 8. DEMONSTRATION FUNCTIONS
#' ========================================

#' Generate Demo Data for Parallel Computing
generate_parallel_demo_data <- function() {
  
  set.seed(42)
  
  # Large dataset for processing
  large_data <- data.frame(
    id = 1:10000,
    x = rnorm(10000),
    y = rnorm(10000),
    category = sample(letters[1:10], 10000, replace = TRUE),
    value = runif(10000, 0, 100)
  )
  
  # Time series data
  ts_data <- data.frame(
    date = seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"),
    value = cumsum(rnorm(length(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"))))
  )
  
  # Simulation parameters
  simulation_params <- expand.grid(
    param1 = seq(0.1, 1, 0.1),
    param2 = seq(1, 10, 1),
    param3 = c(TRUE, FALSE)
  )
  
  return(list(
    large_data = large_data,
    ts_data = ts_data,
    simulation_params = simulation_params
  ))
}

#' Demonstrate Basic Parallel Operations
demo_basic_parallel <- function() {
  cat("=== BASIC PARALLEL OPERATIONS DEMO ===\n")
  
  # Generate test data
  test_data <- 1:1000
  
  # Define a computationally intensive function
  expensive_function <- function(x) {
    Sys.sleep(0.001)  # Simulate computation
    sum(1:x)
  }
  
  # Compare serial vs parallel
  cat("Comparing serial vs parallel performance...\n")
  
  # Serial execution
  serial_time <- system.time({
    serial_results <- lapply(test_data[1:100], expensive_function)
  })
  
  # Parallel execution
  parallel_time <- system.time({
    parallel_results <- parallel_apply(test_data[1:100], expensive_function, cores = 2)
  })
  
  cat("Serial time:", serial_time["elapsed"], "seconds\n")
  cat("Parallel time:", parallel_time["elapsed"], "seconds\n")
  cat("Speedup:", round(serial_time["elapsed"] / parallel_time["elapsed"], 2), "x\n")
  
  return(list(
    serial_time = serial_time,
    parallel_time = parallel_time,
    speedup = serial_time["elapsed"] / parallel_time["elapsed"]
  ))
}

#' Demonstrate Advanced Parallel Patterns
demo_advanced_parallel <- function() {
  cat("\n=== ADVANCED PARALLEL PATTERNS DEMO ===\n")
  
  # Generate demo data
  demo_data <- generate_parallel_demo_data()
  
  # Monte Carlo simulation example
  cat("Running parallel Monte Carlo simulation...\n")
  
  # Simple simulation: estimate pi
  estimate_pi_simulation <- function() {
    n <- 10000
    x <- runif(n, -1, 1)
    y <- runif(n, -1, 1)
    inside_circle <- sum(x^2 + y^2 <= 1)
    4 * inside_circle / n
  }
  
  pi_estimates <- parallel_monte_carlo(100, estimate_pi_simulation, n_cores = 2)
  mean_pi_estimate <- mean(unlist(pi_estimates))
  
  cat("Pi estimate from", length(pi_estimates), "simulations:", mean_pi_estimate, "\n")
  cat("Error:", abs(pi - mean_pi_estimate), "\n")
  
  # Bootstrap example
  cat("\nRunning parallel bootstrap...\n")
  
  bootstrap_stat <- function(data) {
    mean(data$value)
  }
  
  bootstrap_dist <- parallel_bootstrap(demo_data$large_data, bootstrap_stat, 
                                      n_bootstrap = 1000, n_cores = 2)
  
  cat("Bootstrap mean:", mean(bootstrap_dist), "\n")
  cat("Bootstrap SE:", sd(bootstrap_dist), "\n")
  
  return(list(
    pi_estimates = pi_estimates,
    bootstrap_distribution = bootstrap_dist
  ))
}

#' Demonstrate Performance Optimization
demo_performance_optimization <- function() {
  cat("\n=== PERFORMANCE OPTIMIZATION DEMO ===\n")
  
  # Generate test data
  test_df <- data.frame(
    x = rnorm(100000),
    y = rnorm(100000),
    group = sample(letters[1:10], 100000, replace = TRUE)
  )
  
  # Compare different approaches for data manipulation
  approaches <- list(
    "base_r" = quote({
      result <- aggregate(x ~ group, data = test_df, FUN = mean)
    }),
    
    "dplyr" = quote({
      library(dplyr)
      result <- test_df %>% 
        group_by(group) %>% 
        summarise(mean_x = mean(x), .groups = "drop")
    })
  )
  
  if (requireNamespace("data.table", quietly = TRUE)) {
    approaches$data_table <- quote({
      library(data.table)
      dt <- as.data.table(test_df)
      result <- dt[, .(mean_x = mean(x)), by = group]
    })
  }
  
  # Benchmark approaches
  benchmark_results <- benchmark_performance(approaches, times = 5)
  
  return(benchmark_results)
}

#' Complete Parallel Computing Demo
demo_parallel_computing <- function() {
  cat("=== COMPREHENSIVE PARALLEL COMPUTING DEMONSTRATION ===\n\n")
  
  basic_results <- demo_basic_parallel()
  advanced_results <- demo_advanced_parallel()
  optimization_results <- demo_performance_optimization()
  
  cat("\n=== SYSTEM INFORMATION ===\n")
  cat("Available cores:", available_cores, "\n")
  cat("R version:", R.version.string, "\n")
  cat("Platform:", R.version$platform, "\n")
  
  cat("\n=== PARALLEL COMPUTING BEST PRACTICES ===\n")
  cat("1. Use parallel processing for CPU-intensive tasks\n")
  cat("2. Consider overhead costs for small datasets\n")
  cat("3. Be mindful of memory usage with large datasets\n")
  cat("4. Use appropriate data structures (data.table for large data)\n")
  cat("5. Profile code to identify bottlenecks\n")
  cat("6. Test on different hardware configurations\n")
  
  cat("\nParallel Computing Demo Complete!\n")
  
  return(list(
    basic = basic_results,
    advanced = advanced_results,
    optimization = optimization_results
  ))
}

# Export key functions
parallel_computing_exports <- list(
  parallel_apply = parallel_apply,
  parallel_sapply_balanced = parallel_sapply_balanced,
  safe_parallel_map = safe_parallel_map,
  setup_parallel_backend = setup_parallel_backend,
  parallel_foreach = parallel_foreach,
  parallel_cross_validation = parallel_cross_validation,
  async_data_pipeline = async_data_pipeline,
  parallel_grid_search = parallel_grid_search,
  process_large_file_parallel = process_large_file_parallel,
  memory_efficient_processing = memory_efficient_processing,
  benchmark_performance = benchmark_performance,
  profile_memory_usage = profile_memory_usage,
  simulate_mapreduce = simulate_mapreduce,
  distributed_analysis = distributed_analysis,
  parallel_monte_carlo = parallel_monte_carlo,
  parallel_bootstrap = parallel_bootstrap,
  parallel_kmeans_selection = parallel_kmeans_selection,
  generate_parallel_demo_data = generate_parallel_demo_data,
  demo_parallel_computing = demo_parallel_computing
)

# Run demo if script is executed directly
if (sys.nframe() == 0) {
  demo_results <- demo_parallel_computing()
}
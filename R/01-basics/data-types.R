#' @title Data Types and Structures in R
#' @description Comprehensive demonstration of R's core data types and structures
#' @author Portfolio Developer
#' @date 2025

# =============================================================================
# VECTORS: The Foundation of R Data Structures
# =============================================================================

#' Demonstrate Vector Types and Operations
#'
#' @description Creates and manipulates different vector types showing R's
#' fundamental data structures and vectorized operations
#' @param show_examples Logical, whether to print examples
#' @return List containing examples of each vector type
#' @export
#' @examples
#' vectors <- demonstrate_vectors()
#' str(vectors)
demonstrate_vectors <- function(show_examples = TRUE) {
  
  if (show_examples) {
    cat("=== R VECTOR TYPES DEMONSTRATION ===\n\n")
  }
  
  # Numeric vectors (double precision)
  numeric_vec <- c(1.5, 2.7, 3.14159, 4.0, 5.8)
  
  # Integer vectors (explicitly declared)
  integer_vec <- c(1L, 2L, 3L, 4L, 5L)
  
  # Character vectors
  char_vec <- c("apple", "banana", "cherry", "date", "elderberry")
  
  # Logical vectors
  logical_vec <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  
  # Complex numbers
  complex_vec <- c(1+2i, 3-4i, 5+0i)
  
  # Named vectors (key-value pairs)
  named_vec <- c(
    first = 10,
    second = 20,
    third = 30,
    fourth = 40
  )
  
  # Sequences and patterns
  seq_vec <- seq(from = 0, to = 100, by = 10)
  rep_vec <- rep(c("A", "B", "C"), times = 3)
  
  if (show_examples) {
    cat("Numeric vector:", toString(numeric_vec), "\n")
    cat("Integer vector:", toString(integer_vec), "\n")
    cat("Character vector:", toString(char_vec), "\n")
    cat("Logical vector:", toString(logical_vec), "\n")
    cat("Complex vector:", toString(complex_vec), "\n")
    cat("Named vector:", toString(named_vec), "\n")
    cat("Sequence vector:", toString(seq_vec), "\n")
    cat("Repeated vector:", toString(rep_vec), "\n\n")
  }
  
  # Return structured results
  list(
    numeric = numeric_vec,
    integer = integer_vec,
    character = char_vec,
    logical = logical_vec,
    complex = complex_vec,
    named = named_vec,
    sequence = seq_vec,
    repeated = rep_vec,
    metadata = list(
      types = c("numeric", "integer", "character", "logical", "complex"),
      lengths = c(
        length(numeric_vec), length(integer_vec), length(char_vec),
        length(logical_vec), length(complex_vec)
      )
    )
  )
}

#' Advanced Vector Operations
#'
#' @description Demonstrates vectorized operations, indexing, and manipulation
#' @param x Numeric vector for operations
#' @param y Numeric vector for operations
#' @return List of operation results
#' @export
vector_operations <- function(x = 1:10, y = 11:20) {
  
  # Vectorized arithmetic (element-wise operations)
  addition <- x + y
  multiplication <- x * y
  power <- x^2
  
  # Logical operations
  greater_than_5 <- x > 5
  between_values <- x >= 3 & x <= 7
  
  # Statistical functions
  stats <- list(
    sum = sum(x),
    mean = mean(x),
    median = median(x),
    sd = sd(x),
    var = var(x),
    range = range(x),
    quantiles = quantile(x, probs = c(0.25, 0.5, 0.75))
  )
  
  # Advanced indexing
  indexing_examples <- list(
    positive_indexing = x[c(1, 3, 5)],           # Select specific positions
    negative_indexing = x[-c(1, 2)],             # Exclude positions
    logical_indexing = x[x > mean(x)],           # Conditional selection
    named_indexing = if(!is.null(names(x))) x[names(x) %in% c("first", "third")] else NULL
  )
  
  # Return comprehensive results
  list(
    arithmetic = list(
      addition = addition,
      multiplication = multiplication,
      power = power
    ),
    logical = list(
      greater_than_5 = greater_than_5,
      between_values = between_values
    ),
    statistics = stats,
    indexing = indexing_examples
  )
}

# =============================================================================
# MATRICES: Two-Dimensional Arrays
# =============================================================================

#' Create and Manipulate Matrices
#'
#' @description Comprehensive matrix operations and linear algebra
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param fill_method Method to fill matrix ("sequential", "random", "identity")
#' @return List containing matrix and operations
#' @export
matrix_operations <- function(nrow = 4, ncol = 4, fill_method = "sequential") {
  
  # Create matrix based on method
  mat <- switch(fill_method,
    "sequential" = matrix(1:(nrow * ncol), nrow = nrow, ncol = ncol),
    "random" = matrix(rnorm(nrow * ncol), nrow = nrow, ncol = ncol),
    "identity" = diag(nrow),
    matrix(1:(nrow * ncol), nrow = nrow, ncol = ncol)  # default
  )
  
  # Add row and column names
  rownames(mat) <- paste0("Row_", 1:nrow)
  colnames(mat) <- paste0("Col_", 1:ncol)
  
  # Matrix operations
  operations <- list()
  
  if (nrow == ncol) {  # Square matrix operations
    operations$transpose <- t(mat)
    operations$determinant <- det(mat)
    
    # Safe inverse calculation
    if (abs(det(mat)) > 1e-10) {
      operations$inverse <- solve(mat)
      operations$identity_check <- round(mat %*% operations$inverse, 10)
    }
    
    operations$eigenvalues <- eigen(mat)$values
    operations$trace <- sum(diag(mat))
  } else {
    operations$transpose <- t(mat)
  }
  
  # General matrix statistics
  operations$dimensions <- dim(mat)
  operations$row_sums <- rowSums(mat)
  operations$col_means <- colMeans(mat)
  operations$matrix_sum <- sum(mat)
  operations$matrix_mean <- mean(mat)
  
  # Matrix indexing examples
  operations$indexing <- list(
    single_element = mat[1, 1],
    entire_row = mat[1, ],
    entire_column = mat[, 1],
    submatrix = mat[1:2, 1:2],
    diagonal = diag(mat)
  )
  
  list(
    matrix = mat,
    operations = operations,
    metadata = list(
      type = class(mat),
      dimensions = dim(mat),
      fill_method = fill_method
    )
  )
}

# =============================================================================
# LISTS: Flexible Data Containers
# =============================================================================

#' Advanced List Operations and Structures
#'
#' @description Demonstrates complex list structures and operations
#' @return Complex nested list structure
#' @export
advanced_list_operations <- function() {
  
  # Create a complex nested list structure
  complex_list <- list(
    # Basic data types
    numbers = 1:10,
    text = c("analysis", "visualization", "modeling"),
    flags = c(TRUE, FALSE, TRUE),
    
    # Nested structures
    nested_data = list(
      experiment_1 = list(
        treatment = rnorm(100, mean = 5, sd = 1),
        control = rnorm(100, mean = 4, sd = 1),
        metadata = list(
          date = Sys.Date(),
          researcher = "Data Scientist",
          significant = TRUE
        )
      ),
      experiment_2 = list(
        treatment = rnorm(50, mean = 7, sd = 1.5),
        control = rnorm(50, mean = 6, sd = 1.5),
        metadata = list(
          date = Sys.Date() - 30,
          researcher = "Junior Analyst",
          significant = FALSE
        )
      )
    ),
    
    # Mixed data types
    mixed_analysis = list(
      dataset_name = "Portfolio Analysis",
      sample_size = 1000,
      variables = c("age", "income", "education", "satisfaction"),
      correlation_matrix = cor(matrix(rnorm(1000*4), ncol = 4)),
      analysis_complete = TRUE
    ),
    
    # Functions as list elements
    functions = list(
      summarize = function(x) c(mean = mean(x), sd = sd(x), n = length(x)),
      plot_hist = function(x, title = "Histogram") {
        hist(x, main = title, col = "lightblue", border = "white")
      }
    )
  )
  
  # List manipulation operations
  operations <- list(
    
    # Accessing elements
    access_examples = list(
      by_name = complex_list$numbers,
      by_index = complex_list[[1]],
      nested_access = complex_list$nested_data$experiment_1$treatment[1:5],
      deep_nested = complex_list$nested_data$experiment_1$metadata$significant
    ),
    
    # List properties
    structure_info = list(
      length = length(complex_list),
      names = names(complex_list),
      class = class(complex_list),
      recursive_length = length(unlist(complex_list))
    ),
    
    # Functional operations with lists
    functional_ops = list(
      apply_to_numeric = lapply(complex_list[sapply(complex_list, is.numeric)], summary),
      map_lengths = sapply(complex_list, length),
      conditional_extract = complex_list[sapply(complex_list, function(x) length(x) > 5)]
    )
  )
  
  list(
    data = complex_list,
    operations = operations,
    analysis = list(
      total_elements = length(unlist(complex_list)),
      numeric_elements = sum(sapply(unlist(complex_list), is.numeric)),
      structure_depth = max(rapply(complex_list, function(x) 1, how = "list"))
    )
  )
}

# =============================================================================
# DATA FRAMES: The Workhorse of Data Analysis
# =============================================================================

#' Comprehensive Data Frame Operations
#'
#' @description Advanced data frame creation, manipulation, and analysis
#' @param n_rows Number of rows to generate
#' @param seed Random seed for reproducibility
#' @return List containing data frame and analysis results
#' @export
advanced_dataframe_operations <- function(n_rows = 1000, seed = 42) {
  
  set.seed(seed)
  
  # Create a realistic dataset
  df <- data.frame(
    # Identifiers
    id = 1:n_rows,
    customer_id = sprintf("CUST_%05d", sample(1:500, n_rows, replace = TRUE)),
    
    # Demographics
    age = sample(18:80, n_rows, replace = TRUE),
    gender = sample(c("Male", "Female", "Other"), n_rows, replace = TRUE, prob = c(0.48, 0.48, 0.04)),
    income = round(rnorm(n_rows, mean = 65000, sd = 25000), 0),
    education = sample(c("High School", "Bachelor", "Master", "PhD"), n_rows, replace = TRUE, prob = c(0.3, 0.4, 0.25, 0.05)),
    
    # Behavioral data
    purchase_amount = round(abs(rnorm(n_rows, mean = 150, sd = 75)), 2),
    purchase_frequency = rpois(n_rows, lambda = 3),
    satisfaction_score = round(runif(n_rows, min = 1, max = 10), 1),
    
    # Dates
    registration_date = as.Date("2020-01-01") + sample(0:1460, n_rows, replace = TRUE),
    last_purchase = as.Date("2023-01-01") + sample(0:365, n_rows, replace = TRUE),
    
    # Logical indicators
    is_premium = sample(c(TRUE, FALSE), n_rows, replace = TRUE, prob = c(0.2, 0.8)),
    email_subscriber = sample(c(TRUE, FALSE), n_rows, replace = TRUE, prob = c(0.7, 0.3)),
    
    stringsAsFactors = FALSE
  )
  
  # Advanced data frame operations
  operations <- list(
    
    # Basic properties
    structure = list(
      dimensions = dim(df),
      column_names = names(df),
      column_types = sapply(df, class),
      row_names = head(rownames(df)),
      summary_stats = summary(df)
    ),
    
    # Data quality assessment
    quality_check = list(
      missing_values = sapply(df, function(x) sum(is.na(x))),
      duplicate_rows = sum(duplicated(df)),
      unique_customers = length(unique(df$customer_id)),
      data_completeness = round(mean(complete.cases(df)) * 100, 2)
    ),
    
    # Advanced subsetting and filtering
    subsetting = list(
      high_value_customers = df[df$purchase_amount > quantile(df$purchase_amount, 0.9), ],
      premium_subscribers = df[df$is_premium & df$email_subscriber, ],
      recent_customers = df[df$registration_date > as.Date("2022-01-01"), ],
      conditional_summary = aggregate(purchase_amount ~ education + gender, data = df, FUN = mean)
    ),
    
    # Data transformation
    transformations = list(
      age_groups = cut(df$age, breaks = c(0, 30, 50, 70, Inf), labels = c("18-30", "31-50", "51-70", "70+")),
      income_deciles = cut(df$income, breaks = quantile(df$income, probs = 0:10/10), labels = paste0("D", 1:10)),
      customer_value = with(df, purchase_amount * purchase_frequency),
      tenure_days = as.numeric(Sys.Date() - df$registration_date)
    ),
    
    # Statistical analysis
    correlations = cor(df[sapply(df, is.numeric)], use = "complete.obs"),
    
    # Advanced aggregations
    aggregations = list(
      by_education = aggregate(cbind(income, purchase_amount, satisfaction_score) ~ education, data = df, FUN = mean),
      by_gender_premium = aggregate(purchase_amount ~ gender + is_premium, data = df, FUN = function(x) c(mean = mean(x), median = median(x), n = length(x)))
    )
  )
  
  # Performance metrics
  performance <- list(
    memory_usage = object.size(df),
    processing_time = system.time({
      temp_result <- aggregate(purchase_amount ~ customer_id, data = df, FUN = sum)
    })["elapsed"]
  )
  
  list(
    data = df,
    operations = operations,
    performance = performance,
    recommendations = list(
      memory_efficient = "Consider data.table for larger datasets",
      indexing = "Customer_id could benefit from indexing",
      data_types = "Consider factors for categorical variables with many levels"
    )
  )
}

# =============================================================================
# FACTORS: Categorical Data Handling
# =============================================================================

#' Advanced Factor Operations
#'
#' @description Comprehensive factor manipulation and categorical data analysis
#' @return List demonstrating factor operations
#' @export
factor_operations <- function() {
  
  # Create various types of factors
  
  # Unordered factor (nominal)
  colors <- factor(c("red", "blue", "green", "red", "blue", "yellow", "red"))
  
  # Ordered factor (ordinal) 
  education_levels <- factor(
    c("High School", "Bachelor", "Master", "PhD", "Bachelor", "High School", "Master"),
    levels = c("High School", "Bachelor", "Master", "PhD"),
    ordered = TRUE
  )
  
  # Factor with custom labels
  satisfaction <- factor(
    c(1, 2, 3, 4, 5, 3, 4, 2, 5, 1),
    levels = 1:5,
    labels = c("Very Poor", "Poor", "Average", "Good", "Excellent"),
    ordered = TRUE
  )
  
  # Factor operations and analysis
  operations <- list(
    
    # Basic factor properties
    properties = list(
      color_levels = levels(colors),
      education_levels = levels(education_levels),
      satisfaction_levels = levels(satisfaction),
      color_table = table(colors),
      education_ordered = is.ordered(education_levels),
      satisfaction_ordered = is.ordered(satisfaction)
    ),
    
    # Factor manipulation
    manipulation = list(
      # Adding new levels
      colors_expanded = factor(colors, levels = c(levels(colors), "purple", "orange")),
      
      # Reordering levels
      colors_reordered = factor(colors, levels = c("blue", "red", "green", "yellow")),
      
      # Combining factors
      combined_data = data.frame(
        education = education_levels,
        satisfaction = satisfaction[1:length(education_levels)]
      ),
      
      # Dropping unused levels
      colors_subset = droplevels(colors[colors %in% c("red", "blue")])
    ),
    
    # Statistical analysis with factors
    analysis = list(
      # Cross-tabulation
      crosstab = if(length(education_levels) == length(satisfaction)) {
        table(education_levels, satisfaction[1:length(education_levels)])
      } else NULL,
      
      # Proportions
      color_proportions = prop.table(table(colors)),
      
      # Summary statistics by factor
      factor_summary = list(
        most_common_color = names(sort(table(colors), decreasing = TRUE))[1],
        education_distribution = table(education_levels),
        satisfaction_mean_numeric = mean(as.numeric(satisfaction))
      )
    ),
    
    # Factor-based modeling preparation
    modeling_prep = list(
      # Dummy variables (one-hot encoding)
      color_dummies = model.matrix(~ colors - 1),
      
      # Contrast matrices
      education_contrasts = contrasts(education_levels),
      
      # Reference level setting
      colors_reref = relevel(colors, ref = "blue")
    )
  )
  
  # Advanced factor use cases
  use_cases <- list(
    
    # Survey data analysis
    survey_example = {
      set.seed(123)
      survey_data <- data.frame(
        respondent_id = 1:100,
        age_group = factor(sample(c("18-25", "26-35", "36-45", "46-60", "60+"), 100, replace = TRUE)),
        product_rating = factor(sample(1:5, 100, replace = TRUE), levels = 1:5, labels = c("Poor", "Fair", "Good", "Very Good", "Excellent"), ordered = TRUE),
        preferred_channel = factor(sample(c("Online", "Store", "Phone", "Mobile App"), 100, replace = TRUE))
      )
      
      list(
        data = survey_data,
        age_distribution = table(survey_data$age_group),
        rating_summary = summary(survey_data$product_rating),
        channel_preference = prop.table(table(survey_data$preferred_channel))
      )
    },
    
    # Time series factors (for seasonal analysis)
    temporal_factors = {
      dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
      list(
        months = factor(format(dates, "%B"), levels = month.name),
        quarters = factor(quarters(dates)),
        weekdays = factor(weekdays(dates), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      )
    }
  )
  
  list(
    basic_factors = list(colors = colors, education = education_levels, satisfaction = satisfaction),
    operations = operations,
    use_cases = use_cases,
    best_practices = list(
      "Always specify levels explicitly for ordered factors",
      "Use droplevels() after subsetting to remove unused levels",
      "Consider factor order for meaningful visualizations",
      "Use contrasts() for custom modeling approaches"
    )
  )
}

# =============================================================================
# SPECIALIZED DATA STRUCTURES
# =============================================================================

#' Demonstrate Specialized R Data Structures
#'
#' @description Shows arrays, time series, and other specialized structures
#' @return List of specialized data structures and operations
#' @export
specialized_structures <- function() {
  
  # Multi-dimensional arrays
  array_3d <- array(1:24, dim = c(4, 3, 2), dimnames = list(
    rows = paste0("R", 1:4),
    cols = paste0("C", 1:3),
    layers = paste0("L", 1:2)
  ))
  
  # Time series objects
  ts_data <- ts(rnorm(48), start = c(2020, 1), frequency = 12)
  
  # Table objects
  contingency_table <- table(
    Gender = sample(c("M", "F"), 100, replace = TRUE),
    Education = sample(c("HS", "College", "Graduate"), 100, replace = TRUE)
  )
  
  # Environment objects
  custom_env <- new.env()
  custom_env$data <- mtcars
  custom_env$analysis_date <- Sys.Date()
  custom_env$compute_mean <- function(x) mean(x, na.rm = TRUE)
  
  list(
    arrays = list(
      three_dimensional = array_3d,
      dimensions = dim(array_3d),
      slice_example = array_3d[, , 1],
      apply_example = apply(array_3d, c(1, 2), sum)
    ),
    
    time_series = list(
      data = ts_data,
      properties = list(
        start = start(ts_data),
        end = end(ts_data),
        frequency = frequency(ts_data)
      ),
      operations = list(
        lag1 = lag(ts_data, 1),
        diff1 = diff(ts_data),
        window = window(ts_data, start = c(2021, 1), end = c(2021, 12))
      )
    ),
    
    tables = list(
      contingency = contingency_table,
      proportions = prop.table(contingency_table),
      margins = addmargins(contingency_table),
      chi_square = chisq.test(contingency_table)
    ),
    
    environments = list(
      custom_environment = custom_env,
      objects_in_env = ls(custom_env),
      environment_parent = parent.env(custom_env)
    )
  )
}

# =============================================================================
# INTERACTIVE DEMONSTRATION FUNCTION
# =============================================================================

#' Run Complete Data Types Demonstration
#'
#' @description Executes all data type demonstrations with formatted output
#' @param verbose Logical, whether to show detailed output
#' @return Invisible list of all results
#' @export
run_data_types_demo <- function(verbose = TRUE) {
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("       R DATA TYPES & STRUCTURES PORTFOLIO\n")
    cat("="*60, "\n\n")
  }
  
  results <- list()
  
  # Run all demonstrations
  if (verbose) cat("1. Vector Operations...\n")
  results$vectors <- demonstrate_vectors(show_examples = verbose)
  results$vector_ops <- vector_operations()
  
  if (verbose) cat("2. Matrix Operations...\n")
  results$matrices <- matrix_operations(nrow = 3, ncol = 3, fill_method = "random")
  
  if (verbose) cat("3. List Operations...\n")
  results$lists <- advanced_list_operations()
  
  if (verbose) cat("4. Data Frame Operations...\n")
  results$dataframes <- advanced_dataframe_operations(n_rows = 100)
  
  if (verbose) cat("5. Factor Operations...\n")
  results$factors <- factor_operations()
  
  if (verbose) cat("6. Specialized Structures...\n")
  results$specialized <- specialized_structures()
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("    DEMONSTRATION COMPLETE\n")
    cat("="*60, "\n")
    cat("Total objects created:", length(unlist(results)), "\n")
    cat("Memory usage:", format(object.size(results), units = "Mb"), "\n")
  }
  
  invisible(results)
}

# =============================================================================
# EXAMPLE USAGE (Run only in interactive mode)
# =============================================================================

if (interactive()) {
  # Run the complete demonstration
  demo_results <- run_data_types_demo(verbose = TRUE)
  
  # Access specific results
  # demo_results$vectors$numeric
  # demo_results$matrices$operations$determinant
  # demo_results$dataframes$data
}
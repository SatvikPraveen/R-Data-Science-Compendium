# @title Comprehensive Feature Engineering Framework
# @description Advanced feature creation, selection, and transformation techniques
# @author R Data Science Portfolio Developer
# @date 2025

#' ========================================
#' COMPREHENSIVE FEATURE ENGINEERING
#' ========================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, ggplot2, lubridate, stringr, forcats, tidyr,
  VIM, mice, corrplot, caret, Boruta, FSelector,
  recipes, textfeatures, tm, SnowballC, wordcloud,
  RcppRoll, forecast, zoo, moments, entropy
)

#' ========================================
#' 1. NUMERIC FEATURE ENGINEERING
#' ========================================

#' Create Polynomial Features
#' @param data Dataset
#' @param columns Columns to create polynomial features for
#' @param degree Maximum polynomial degree
#' @return Dataset with polynomial features
create_polynomial_features <- function(data, columns, degree = 2) {
  
  result_data <- data
  
  for (col in columns) {
    if (is.numeric(data[[col]])) {
      for (d in 2:degree) {
        new_col_name <- paste0(col, "_poly_", d)
        result_data[[new_col_name]] <- data[[col]]^d
      }
    }
  }
  
  return(result_data)
}

#' Create Interaction Features
#' @param data Dataset
#' @param columns Columns to create interactions for
#' @param max_interactions Maximum number of feature interactions
#' @return Dataset with interaction features
create_interaction_features <- function(data, columns, max_interactions = 10) {
  
  result_data <- data
  numeric_cols <- columns[sapply(data[columns], is.numeric)]
  
  if (length(numeric_cols) < 2) {
    warning("Need at least 2 numeric columns for interactions")
    return(result_data)
  }
  
  # Generate all pairwise combinations
  combinations <- combn(numeric_cols, 2, simplify = FALSE)
  
  # Limit to max_interactions
  if (length(combinations) > max_interactions) {
    combinations <- combinations[1:max_interactions]
  }
  
  for (combo in combinations) {
    col1 <- combo[1]
    col2 <- combo[2]
    new_col_name <- paste0(col1, "_x_", col2)
    result_data[[new_col_name]] <- data[[col1]] * data[[col2]]
  }
  
  return(result_data)
}

#' Create Binned Features
#' @param data Dataset
#' @param columns Columns to bin
#' @param n_bins Number of bins
#' @param method Binning method ("equal_width", "equal_freq", "quantile")
#' @return Dataset with binned features
create_binned_features <- function(data, columns, n_bins = 5, method = "equal_width") {
  
  result_data <- data
  
  for (col in columns) {
    if (is.numeric(data[[col]])) {
      
      new_col_name <- paste0(col, "_binned")
      
      if (method == "equal_width") {
        result_data[[new_col_name]] <- cut(data[[col]], breaks = n_bins, labels = FALSE)
      } else if (method == "equal_freq") {
        result_data[[new_col_name]] <- as.numeric(cut(data[[col]], 
                                                    breaks = quantile(data[[col]], 
                                                                     probs = seq(0, 1, length.out = n_bins + 1), 
                                                                     na.rm = TRUE),
                                                    include.lowest = TRUE, labels = FALSE))
      } else if (method == "quantile") {
        result_data[[new_col_name]] <- ntile(data[[col]], n_bins)
      }
    }
  }
  
  return(result_data)
}

#' Create Rolling Statistics Features
#' @param data Dataset with time series data
#' @param value_col Column to calculate rolling stats for
#' @param date_col Date column for ordering
#' @param windows Window sizes for rolling calculations
#' @return Dataset with rolling statistics
create_rolling_features <- function(data, value_col, date_col, windows = c(3, 7, 14, 30)) {
  
  # Sort by date
  data <- data[order(data[[date_col]]), ]
  
  result_data <- data
  values <- data[[value_col]]
  
  for (window in windows) {
    if (window <= nrow(data)) {
      # Rolling mean
      result_data[[paste0(value_col, "_rolling_mean_", window)]] <- 
        RcppRoll::roll_mean(values, n = window, fill = NA, align = "right")
      
      # Rolling standard deviation
      result_data[[paste0(value_col, "_rolling_sd_", window)]] <- 
        RcppRoll::roll_sd(values, n = window, fill = NA, align = "right")
      
      # Rolling min/max
      result_data[[paste0(value_col, "_rolling_min_", window)]] <- 
        RcppRoll::roll_min(values, n = window, fill = NA, align = "right")
      
      result_data[[paste0(value_col, "_rolling_max_", window)]] <- 
        RcppRoll::roll_max(values, n = window, fill = NA, align = "right")
    }
  }
  
  return(result_data)
}

#' Create Lag Features
#' @param data Dataset
#' @param value_col Column to create lags for
#' @param date_col Date column for ordering
#' @param lags Vector of lag periods
#' @return Dataset with lag features
create_lag_features <- function(data, value_col, date_col, lags = c(1, 2, 3, 7, 14)) {
  
  # Sort by date
  data <- data[order(data[[date_col]]), ]
  
  result_data <- data
  
  for (lag_period in lags) {
    lag_col_name <- paste0(value_col, "_lag_", lag_period)
    result_data[[lag_col_name]] <- dplyr::lag(data[[value_col]], lag_period)
  }
  
  return(result_data)
}

#' ========================================
#' 2. CATEGORICAL FEATURE ENGINEERING
#' ========================================

#' Create Target Encoding
#' @param data Dataset
#' @param categorical_col Categorical column to encode
#' @param target_col Target variable
#' @param smoothing Smoothing parameter for regularization
#' @return Dataset with target encoded feature
create_target_encoding <- function(data, categorical_col, target_col, smoothing = 1) {
  
  # Calculate global mean
  global_mean <- mean(data[[target_col]], na.rm = TRUE)
  
  # Calculate category means and counts
  category_stats <- data %>%
    group_by(!!sym(categorical_col)) %>%
    summarise(
      category_mean = mean(!!sym(target_col), na.rm = TRUE),
      category_count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      # Apply smoothing
      smoothed_mean = (category_mean * category_count + global_mean * smoothing) / 
                     (category_count + smoothing)
    )
  
  # Join back to original data
  result_data <- data %>%
    left_join(category_stats %>% select(!!sym(categorical_col), smoothed_mean), 
              by = categorical_col)
  
  # Rename the new column
  names(result_data)[names(result_data) == "smoothed_mean"] <- 
    paste0(categorical_col, "_target_encoded")
  
  return(result_data)
}

#' Create Frequency Encoding
#' @param data Dataset
#' @param categorical_cols Categorical columns to encode
#' @return Dataset with frequency encoded features
create_frequency_encoding <- function(data, categorical_cols) {
  
  result_data <- data
  
  for (col in categorical_cols) {
    # Calculate frequencies
    freq_table <- table(data[[col]])
    
    # Create new column with frequencies
    new_col_name <- paste0(col, "_frequency")
    result_data[[new_col_name]] <- freq_table[data[[col]]]
  }
  
  return(result_data)
}

#' Create One-Hot Encoding with Rare Category Handling
#' @param data Dataset
#' @param categorical_cols Categorical columns to encode
#' @param min_frequency Minimum frequency to keep category separate
#' @param max_categories Maximum number of categories to create dummy variables for
#' @return Dataset with one-hot encoded features
create_onehot_encoding <- function(data, categorical_cols, min_frequency = 0.01, max_categories = 10) {
  
  result_data <- data
  
  for (col in categorical_cols) {
    # Calculate category frequencies
    freq_table <- table(data[[col]])
    total_count <- nrow(data)
    
    # Identify rare categories
    rare_categories <- names(freq_table[freq_table < (min_frequency * total_count)])
    
    # Combine rare categories
    processed_col <- as.character(data[[col]])
    processed_col[processed_col %in% rare_categories] <- "rare_category"
    
    # Keep only top categories if too many
    if (length(unique(processed_col)) > max_categories) {
      top_categories <- names(sort(table(processed_col), decreasing = TRUE))[1:(max_categories-1)]
      processed_col[!processed_col %in% top_categories] <- "other_category"
    }
    
    # Create dummy variables
    dummy_vars <- model.matrix(~ . - 1, data = data.frame(temp = as.factor(processed_col)))
    colnames(dummy_vars) <- paste0(col, "_", colnames(dummy_vars))
    
    # Add to result data
    result_data <- cbind(result_data, dummy_vars)
  }
  
  return(result_data)
}

#' ========================================
#' 3. DATE/TIME FEATURE ENGINEERING
#' ========================================

#' Extract Comprehensive Date Features
#' @param data Dataset
#' @param date_cols Date columns to extract features from
#' @return Dataset with extracted date features
extract_date_features <- function(data, date_cols) {
  
  result_data <- data
  
  for (col in date_cols) {
    if (is.character(data[[col]]) || is.factor(data[[col]])) {
      date_col <- as.Date(data[[col]])
    } else {
      date_col <- as.Date(data[[col]])
    }
    
    # Basic date components
    result_data[[paste0(col, "_year")]] <- year(date_col)
    result_data[[paste0(col, "_month")]] <- month(date_col)
    result_data[[paste0(col, "_day")]] <- day(date_col)
    result_data[[paste0(col, "_quarter")]] <- quarter(date_col)
    result_data[[paste0(col, "_week")]] <- week(date_col)
    result_data[[paste0(col, "_weekday")]] <- wday(date_col)
    result_data[[paste0(col, "_day_of_year")]] <- yday(date_col)
    
    # Cyclical encoding for periodic features
    result_data[[paste0(col, "_month_sin")]] <- sin(2 * pi * month(date_col) / 12)
    result_data[[paste0(col, "_month_cos")]] <- cos(2 * pi * month(date_col) / 12)
    result_data[[paste0(col, "_day_sin")]] <- sin(2 * pi * day(date_col) / 31)
    result_data[[paste0(col, "_day_cos")]] <- cos(2 * pi * day(date_col) / 31)
    result_data[[paste0(col, "_weekday_sin")]] <- sin(2 * pi * wday(date_col) / 7)
    result_data[[paste0(col, "_weekday_cos")]] <- cos(2 * pi * wday(date_col) / 7)
    
    # Business vs weekend
    result_data[[paste0(col, "_is_weekend")]] <- as.numeric(wday(date_col) %in% c(1, 7))
    
    # Days since epoch (for trend)
    result_data[[paste0(col, "_days_since_epoch")]] <- as.numeric(date_col - as.Date("1970-01-01"))
  }
  
  return(result_data)
}

#' Create Time-Based Aggregations
#' @param data Dataset
#' @param date_col Date column
#' @param value_col Value column to aggregate
#' @param id_col ID column for grouping
#' @param periods Aggregation periods
#' @return Dataset with time-based aggregations
create_time_aggregations <- function(data, date_col, value_col, id_col, 
                                   periods = c("week", "month", "quarter")) {
  
  result_data <- data
  
  for (period in periods) {
    # Create period grouping
    if (period == "week") {
      data$period_group <- floor_date(data[[date_col]], "week")
    } else if (period == "month") {
      data$period_group <- floor_date(data[[date_col]], "month")
    } else if (period == "quarter") {
      data$period_group <- floor_date(data[[date_col]], "quarter")
    }
    
    # Calculate aggregations
    period_aggs <- data %>%
      group_by(!!sym(id_col), period_group) %>%
      summarise(
        !!paste0(value_col, "_", period, "_mean") := mean(!!sym(value_col), na.rm = TRUE),
        !!paste0(value_col, "_", period, "_sum") := sum(!!sym(value_col), na.rm = TRUE),
        !!paste0(value_col, "_", period, "_count") := n(),
        .groups = "drop"
      ) %>%
      select(-period_group)
    
    # Join back to original data
    result_data <- result_data %>%
      left_join(period_aggs, by = id_col)
  }
  
  return(result_data)
}

#' ========================================
#' 4. TEXT FEATURE ENGINEERING
#' ========================================

#' Extract Basic Text Features
#' @param data Dataset
#' @param text_cols Text columns to extract features from
#' @return Dataset with text features
extract_text_features <- function(data, text_cols) {
  
  result_data <- data
  
  for (col in text_cols) {
    text_data <- as.character(data[[col]])
    text_data[is.na(text_data)] <- ""
    
    # Basic text statistics
    result_data[[paste0(col, "_length")]] <- nchar(text_data)
    result_data[[paste0(col, "_word_count")]] <- str_count(text_data, "\\S+")
    result_data[[paste0(col, "_sentence_count")]] <- str_count(text_data, "[.!?]+")
    result_data[[paste0(col, "_char_count")]] <- nchar(gsub("\\s", "", text_data))
    
    # Punctuation and special characters
    result_data[[paste0(col, "_exclamation_count")]] <- str_count(text_data, "!")
    result_data[[paste0(col, "_question_count")]] <- str_count(text_data, "\\?")
    result_data[[paste0(col, "_uppercase_count")]] <- str_count(text_data, "[A-Z]")
    result_data[[paste0(col, "_digit_count")]] <- str_count(text_data, "[0-9]")
    
    # Average word length
    words <- str_extract_all(text_data, "\\S+")
    avg_word_length <- sapply(words, function(x) {
      if (length(x) == 0) return(0)
      mean(nchar(x))
    })
    result_data[[paste0(col, "_avg_word_length")]] <- avg_word_length
    
    # Sentiment indicators (basic)
    positive_words <- c("good", "great", "excellent", "amazing", "wonderful", "fantastic", "love")
    negative_words <- c("bad", "terrible", "awful", "hate", "horrible", "disgusting", "worst")
    
    result_data[[paste0(col, "_positive_words")]] <- 
      sapply(text_data, function(x) sum(str_count(tolower(x), positive_words)))
    
    result_data[[paste0(col, "_negative_words")]] <- 
      sapply(text_data, function(x) sum(str_count(tolower(x), negative_words)))
  }
  
  return(result_data)
}

#' Create TF-IDF Features
#' @param data Dataset
#' @param text_col Text column
#' @param max_features Maximum number of features to create
#' @param min_df Minimum document frequency
#' @return Dataset with TF-IDF features
create_tfidf_features <- function(data, text_col, max_features = 100, min_df = 2) {
  
  text_data <- as.character(data[[text_col]])
  text_data[is.na(text_data)] <- ""
  
  # Create corpus
  corpus <- Corpus(VectorSource(text_data))
  
  # Preprocessing
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Create TF-IDF matrix
  tfidf_matrix <- DocumentTermMatrix(corpus, 
                                   control = list(weighting = weightTfIdf,
                                                bounds = list(global = c(min_df, Inf))))
  
  # Reduce dimensions if needed
  if (ncol(tfidf_matrix) > max_features) {
    # Keep most frequent terms
    term_freq <- colSums(as.matrix(tfidf_matrix))
    top_terms <- names(sort(term_freq, decreasing = TRUE)[1:max_features])
    tfidf_matrix <- tfidf_matrix[, top_terms]
  }
  
  # Convert to data frame
  tfidf_df <- as.data.frame(as.matrix(tfidf_matrix))
  colnames(tfidf_df) <- paste0(text_col, "_tfidf_", colnames(tfidf_df))
  
  # Combine with original data
  result_data <- cbind(data, tfidf_df)
  
  return(result_data)
}

#' ========================================
#' 5. STATISTICAL FEATURE ENGINEERING
#' ========================================

#' Create Statistical Features for Groups
#' @param data Dataset
#' @param group_col Grouping column
#' @param numeric_cols Numeric columns to calculate statistics for
#' @return Dataset with statistical features
create_statistical_features <- function(data, group_col, numeric_cols) {
  
  result_data <- data
  
  for (col in numeric_cols) {
    if (is.numeric(data[[col]])) {
      
      # Group statistics
      group_stats <- data %>%
        group_by(!!sym(group_col)) %>%
        summarise(
          !!paste0(col, "_group_mean") := mean(!!sym(col), na.rm = TRUE),
          !!paste0(col, "_group_median") := median(!!sym(col), na.rm = TRUE),
          !!paste0(col, "_group_sd") := sd(!!sym(col), na.rm = TRUE),
          !!paste0(col, "_group_min") := min(!!sym(col), na.rm = TRUE),
          !!paste0(col, "_group_max") := max(!!sym(col), na.rm = TRUE),
          !!paste0(col, "_group_q25") := quantile(!!sym(col), 0.25, na.rm = TRUE),
          !!paste0(col, "_group_q75") := quantile(!!sym(col), 0.75, na.rm = TRUE),
          !!paste0(col, "_group_count") := n(),
          .groups = "drop"
        )
      
      # Join back to original data
      result_data <- result_data %>%
        left_join(group_stats, by = group_col)
      
      # Create relative features
      result_data[[paste0(col, "_vs_group_mean")]] <- 
        result_data[[col]] - result_data[[paste0(col, "_group_mean")]]
      
      result_data[[paste0(col, "_vs_group_median")]] <- 
        result_data[[col]] - result_data[[paste0(col, "_group_median")]]
      
      # Z-score within group
      result_data[[paste0(col, "_group_zscore")]] <- 
        (result_data[[col]] - result_data[[paste0(col, "_group_mean")]]) / 
        result_data[[paste0(col, "_group_sd")]]
    }
  }
  
  return(result_data)
}

#' Create Outlier Features
#' @param data Dataset
#' @param numeric_cols Numeric columns to check for outliers
#' @param method Outlier detection method ("iqr", "zscore", "modified_zscore")
#' @param threshold Threshold for outlier detection
#' @return Dataset with outlier indicator features
create_outlier_features <- function(data, numeric_cols, method = "iqr", threshold = 1.5) {
  
  result_data <- data
  
  for (col in numeric_cols) {
    if (is.numeric(data[[col]])) {
      
      if (method == "iqr") {
        Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
        Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        
        lower_bound <- Q1 - threshold * IQR
        upper_bound <- Q3 + threshold * IQR
        
        result_data[[paste0(col, "_is_outlier")]] <- 
          as.numeric(data[[col]] < lower_bound | data[[col]] > upper_bound)
        
      } else if (method == "zscore") {
        z_scores <- abs(scale(data[[col]])[,1])
        result_data[[paste0(col, "_is_outlier")]] <- as.numeric(z_scores > threshold)
        
      } else if (method == "modified_zscore") {
        median_val <- median(data[[col]], na.rm = TRUE)
        mad_val <- mad(data[[col]], na.rm = TRUE)
        modified_z_scores <- 0.6745 * (data[[col]] - median_val) / mad_val
        result_data[[paste0(col, "_is_outlier")]] <- as.numeric(abs(modified_z_scores) > threshold)
      }
    }
  }
  
  return(result_data)
}

#' ========================================
#' 6. FEATURE SELECTION METHODS
#' ========================================

#' Correlation-based Feature Selection
#' @param data Dataset
#' @param target_col Target variable
#' @param threshold Correlation threshold
#' @return Selected feature names
select_features_correlation <- function(data, target_col, threshold = 0.8) {
  
  # Get numeric columns (excluding target)
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  feature_cols <- setdiff(numeric_cols, target_col)
  
  if (length(feature_cols) < 2) {
    return(feature_cols)
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(data[feature_cols], use = "complete.obs")
  
  # Find highly correlated pairs
  high_cor_pairs <- which(abs(cor_matrix) > threshold & cor_matrix != 1, arr.ind = TRUE)
  
  if (nrow(high_cor_pairs) == 0) {
    return(feature_cols)
  }
  
  # Calculate correlation with target
  target_cor <- cor(data[feature_cols], data[[target_col]], use = "complete.obs")
  
  # Remove features with lower target correlation
  features_to_remove <- c()
  
  for (i in 1:nrow(high_cor_pairs)) {
    row_idx <- high_cor_pairs[i, 1]
    col_idx <- high_cor_pairs[i, 2]
    
    if (row_idx < col_idx) {  # Avoid duplicates
      feature1 <- feature_cols[row_idx]
      feature2 <- feature_cols[col_idx]
      
      # Keep feature with higher target correlation
      if (abs(target_cor[feature1]) < abs(target_cor[feature2])) {
        features_to_remove <- c(features_to_remove, feature1)
      } else {
        features_to_remove <- c(features_to_remove, feature2)
      }
    }
  }
  
  selected_features <- setdiff(feature_cols, unique(features_to_remove))
  
  cat("Removed", length(unique(features_to_remove)), "highly correlated features\n")
  cat("Selected", length(selected_features), "features\n")
  
  return(selected_features)
}

#' Boruta Feature Selection
#' @param data Dataset
#' @param target_col Target variable
#' @param max_runs Maximum number of Boruta runs
#' @return Selected feature names
select_features_boruta <- function(data, target_col, max_runs = 100) {
  
  # Prepare data
  feature_data <- data[complete.cases(data), ]
  
  # Run Boruta
  boruta_result <- Boruta(as.formula(paste(target_col, "~ .")), 
                         data = feature_data, 
                         maxRuns = max_runs)
  
  # Get confirmed and tentative features
  confirmed_features <- names(boruta_result$finalDecision[boruta_result$finalDecision == "Confirmed"])
  tentative_features <- names(boruta_result$finalDecision[boruta_result$finalDecision == "Tentative"])
  
  # Decide on tentative features using Z-scores
  if (length(tentative_features) > 0) {
    tentative_decision <- TentativeRoughFix(boruta_result)
    final_features <- names(tentative_decision$finalDecision[tentative_decision$finalDecision == "Confirmed"])
  } else {
    final_features <- confirmed_features
  }
  
  cat("Boruta selected", length(final_features), "features out of", ncol(feature_data) - 1, "\n")
  
  return(final_features)
}

#' ========================================
#' 7. DEMONSTRATION FUNCTIONS
#' ========================================

#' Generate Feature Engineering Demo Data
generate_feature_engineering_demo_data <- function() {
  
  set.seed(42)
  n <- 1000
  
  # Base data
  data <- data.frame(
    id = 1:n,
    numeric_var1 = rnorm(n, 50, 15),
    numeric_var2 = rnorm(n, 100, 25),
    category = sample(c("A", "B", "C", "D"), n, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
    date_created = seq(as.Date("2023-01-01"), length.out = n, by = "day"),
    text_review = sample(c(
      "This product is amazing and wonderful",
      "Great quality, highly recommend",
      "Not bad, could be better",
      "Terrible experience, very disappointed",
      "Excellent service and fast delivery",
      "Average product, nothing special"
    ), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Create target variable with some relationships
  data$target <- 0.5 * data$numeric_var1 + 0.3 * data$numeric_var2 + 
                ifelse(data$category == "A", 10, 0) + 
                ifelse(data$region == "North", 5, 0) + 
                rnorm(n, 0, 10)
  
  return(data)
}

#' Run Feature Engineering Demo
demo_feature_engineering <- function() {
  
  cat("=== COMPREHENSIVE FEATURE ENGINEERING DEMONSTRATION ===\n\n")
  
  # Generate demo data
  demo_data <- generate_feature_engineering_demo_data()
  
  cat("Original data shape:", nrow(demo_data), "x", ncol(demo_data), "\n\n")
  
  cat("1. NUMERIC FEATURE ENGINEERING\n")
  cat("="*50, "\n")
  
  # Polynomial features
  demo_data <- create_polynomial_features(demo_data, c("numeric_var1", "numeric_var2"), degree = 3)
  cat("Added polynomial features (degree 2-3)\n")
  
  # Interaction features
  demo_data <- create_interaction_features(demo_data, c("numeric_var1", "numeric_var2"))
  cat("Added interaction features\n")
  
  # Binned features
  demo_data <- create_binned_features(demo_data, c("numeric_var1", "numeric_var2"), n_bins = 5)
  cat("Added binned features\n")
  
  cat("\n2. CATEGORICAL FEATURE ENGINEERING\n")
  cat("="*50, "\n")
  
  # Target encoding
  demo_data <- create_target_encoding(demo_data, "category", "target")
  cat("Added target encoding for category\n")
  
  # Frequency encoding
  demo_data <- create_frequency_encoding(demo_data, c("category", "region"))
  cat("Added frequency encoding\n")
  
  # One-hot encoding
  demo_data <- create_onehot_encoding(demo_data, c("region"))
  cat("Added one-hot encoding\n")
  
  cat("\n3. DATE/TIME FEATURE ENGINEERING\n")
  cat("="*50, "\n")
  
  # Date features
  demo_data <- extract_date_features(demo_data, "date_created")
  cat("Extracted comprehensive date features\n")
  
  cat("\n4. TEXT FEATURE ENGINEERING\n")
  cat("="*50, "\n")
  
  # Text features
  demo_data <- extract_text_features(demo_data, "text_review")
  cat("Extracted basic text features\n")
  
  cat("\n5. STATISTICAL FEATURE ENGINEERING\n")
  cat("="*50, "\n")
  
  # Statistical features
  demo_data <- create_statistical_features(demo_data, "category", c("numeric_var1", "numeric_var2"))
  cat("Added group statistical features\n")
  
  # Outlier features
  demo_data <- create_outlier_features(demo_data, c("numeric_var1", "numeric_var2"))
  cat("Added outlier indicator features\n")
  
  cat("\n6. FEATURE SELECTION\n")
  cat("="*50, "\n")
  
  # Correlation-based selection
  numeric_features <- names(demo_data)[sapply(demo_data, is.numeric)]
  feature_cols <- setdiff(numeric_features, "target")
  
  selected_features_cor <- select_features_correlation(
    demo_data[c(feature_cols, "target")], 
    "target", 
    threshold = 0.9
  )
  
  cat("Selected features after correlation filtering:", length(selected_features_cor), "\n")
  
  cat("\nFinal data shape:", nrow(demo_data), "x", ncol(demo_data), "\n")
  cat("Feature engineering increased features from 9 to", ncol(demo_data), "\n")
  
  cat("\nFeature Engineering Demo Complete!\n")
  
  return(list(
    original_data = generate_feature_engineering_demo_data(),
    engineered_data = demo_data,
    selected_features = selected_features_cor,
    feature_count_increase = ncol(demo_data) - 9
  ))
}

# Export key functions
feature_engineering_exports <- list(
  create_polynomial_features = create_polynomial_features,
  create_interaction_features = create_interaction_features,
  create_binned_features = create_binned_features,
  create_rolling_features = create_rolling_features,
  create_lag_features = create_lag_features,
  create_target_encoding = create_target_encoding,
  create_frequency_encoding = create_frequency_encoding,
  create_onehot_encoding = create_onehot_encoding,
  extract_date_features = extract_date_features,
  create_time_aggregations = create_time_aggregations,
  extract_text_features = extract_text_features,
  create_tfidf_features = create_tfidf_features,
  create_statistical_features = create_statistical_features,
  create_outlier_features = create_outlier_features,
  select_features_correlation = select_features_correlation,
  select_features_boruta = select_features_boruta,
  generate_feature_engineering_demo_data = generate_feature_engineering_demo_data,
  demo_feature_engineering = demo_feature_engineering
)

# Run demo if script is executed directly
if (sys.nframe() == 0) {
  demo_results <- demo_feature_engineering()
}
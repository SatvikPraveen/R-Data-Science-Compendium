#' @title Comprehensive Descriptive Statistics and Data Exploration
#' @description Advanced descriptive statistical analysis with professional reporting
#' @author Portfolio Developer  
#' @date 2025

library(dplyr)
library(ggplot2)
library(moments)
library(psych)

# =============================================================================
# COMPREHENSIVE DESCRIPTIVE STATISTICS FRAMEWORK
# =============================================================================

#' Advanced Descriptive Statistics Analysis
#'
#' @description Generates comprehensive descriptive statistics for numerical data
#' @param data Data frame containing the variables to analyze
#' @param variables Character vector of variable names to analyze (if NULL, analyzes all numeric)
#' @param group_by Character string of grouping variable name (optional)
#' @param include_distribution Logical, whether to include distribution analysis
#' @param confidence_level Confidence level for confidence intervals
#' @return List containing comprehensive descriptive analysis
#' @export
comprehensive_descriptive_analysis <- function(data, 
                                              variables = NULL, 
                                              group_by = NULL,
                                              include_distribution = TRUE,
                                              confidence_level = 0.95) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (nrow(data) == 0) {
    stop("Data frame cannot be empty")
  }
  
  # Select variables to analyze
  if (is.null(variables)) {
    variables <- names(data)[sapply(data, is.numeric)]
  } else {
    # Validate variable names
    missing_vars <- setdiff(variables, names(data))
    if (length(missing_vars) > 0) {
      stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
    }
    
    # Check if variables are numeric
    non_numeric <- variables[!sapply(data[variables], is.numeric)]
    if (length(non_numeric) > 0) {
      stop("Non-numeric variables detected: ", paste(non_numeric, collapse = ", "))
    }
  }
  
  if (length(variables) == 0) {
    stop("No numeric variables found for analysis")
  }
  
  # Validate grouping variable
  if (!is.null(group_by)) {
    if (!group_by %in% names(data)) {
      stop("Grouping variable '", group_by, "' not found in data")
    }
  }
  
  results <- list()
  results$metadata <- list(
    analysis_date = Sys.time(),
    variables_analyzed = variables,
    group_by = group_by,
    sample_size = nrow(data),
    confidence_level = confidence_level
  )
  
  # =============================================================================
  # BASIC DESCRIPTIVE STATISTICS
  # =============================================================================
  
  calculate_basic_stats <- function(x, var_name, conf_level = 0.95) {
    
    # Remove missing values for calculations
    x_clean <- x[!is.na(x)]
    n <- length(x_clean)
    
    if (n == 0) {
      return(list(
        variable = var_name,
        n = 0,
        missing = length(x) - n,
        error = "All values are missing"
      ))
    }
    
    # Central tendency measures
    mean_val <- mean(x_clean)
    median_val <- median(x_clean)
    mode_val <- calculate_mode(x_clean)
    
    # Dispersion measures
    variance_val <- var(x_clean)
    sd_val <- sd(x_clean)
    mad_val <- mad(x_clean)  # Median Absolute Deviation
    range_val <- max(x_clean) - min(x_clean)
    iqr_val <- IQR(x_clean)
    
    # Confidence interval for mean
    alpha <- 1 - conf_level
    t_critical <- qt(1 - alpha/2, df = n - 1)
    se_mean <- sd_val / sqrt(n)
    ci_lower <- mean_val - t_critical * se_mean
    ci_upper <- mean_val + t_critical * se_mean
    
    # Position measures
    quantiles <- quantile(x_clean, probs = c(0, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 1.00))
    
    # Coefficient of variation
    cv <- ifelse(mean_val != 0, (sd_val / abs(mean_val)) * 100, NA)
    
    list(
      variable = var_name,
      n = n,
      missing = length(x) - n,
      missing_pct = round((length(x) - n) / length(x) * 100, 2),
      
      # Central tendency
      mean = mean_val,
      median = median_val,
      mode = mode_val,
      
      # Dispersion
      variance = variance_val,
      std_dev = sd_val,
      mad = mad_val,
      range = range_val,
      iqr = iqr_val,
      coefficient_of_variation = cv,
      
      # Confidence interval
      ci_mean_lower = ci_lower,
      ci_mean_upper = ci_upper,
      
      # Position measures
      min = min(x_clean),
      max = max(x_clean),
      q05 = quantiles["5%"],
      q10 = quantiles["10%"],
      q25 = quantiles["25%"],
      q50 = quantiles["50%"],
      q75 = quantiles["75%"],
      q90 = quantiles["90%"],
      q95 = quantiles["95%"]
    )
  }
  
  # Helper function to calculate mode
  calculate_mode <- function(x) {
    if (length(x) == 0) return(NA)
    
    # For continuous data, use density estimation
    if (length(unique(x)) == length(x)) {
      # All values unique - use kernel density estimation
      density_est <- density(x, na.rm = TRUE)
      mode_val <- density_est$x[which.max(density_est$y)]
      return(mode_val)
    } else {
      # Discrete or grouped data
      freq_table <- table(x)
      mode_val <- as.numeric(names(freq_table)[which.max(freq_table)])
      return(mode_val)
    }
  }
  
  # =============================================================================
  # DISTRIBUTION ANALYSIS
  # =============================================================================
  
  calculate_distribution_stats <- function(x, var_name) {
    
    x_clean <- x[!is.na(x)]
    
    if (length(x_clean) < 3) {
      return(list(
        variable = var_name,
        error = "Insufficient data for distribution analysis"
      ))
    }
    
    # Shape measures
    skewness_val <- moments::skewness(x_clean)
    kurtosis_val <- moments::kurtosis(x_clean)
    excess_kurtosis <- kurtosis_val - 3
    
    # Normality tests
    normality_tests <- list()
    
    if (length(x_clean) >= 3 && length(x_clean) <= 5000) {
      normality_tests$shapiro_wilk <- tryCatch({
        shapiro.test(x_clean)
      }, error = function(e) list(p.value = NA, statistic = NA))
    }
    
    if (length(x_clean) >= 8) {
      normality_tests$anderson_darling <- tryCatch({
        nortest::ad.test(x_clean)
      }, error = function(e) list(p.value = NA, statistic = NA))
      
      normality_tests$kolmogorov_smirnov <- tryCatch({
        ks.test(x_clean, "pnorm", mean(x_clean), sd(x_clean))
      }, error = function(e) list(p.value = NA, statistic = NA))
    }
    
    # Outlier detection using multiple methods
    outliers <- detect_outliers(x_clean)
    
    # Distribution classification
    distribution_type <- classify_distribution(skewness_val, kurtosis_val)
    
    list(
      variable = var_name,
      
      # Shape measures
      skewness = skewness_val,
      kurtosis = kurtosis_val,
      excess_kurtosis = excess_kurtosis,
      
      # Distribution classification
      distribution_type = distribution_type,
      
      # Normality tests
      normality_tests = normality_tests,
      
      # Outliers
      outliers = outliers
    )
  }
  
  # Outlier detection function
  detect_outliers <- function(x) {
    
    # Method 1: IQR method
    q1 <- quantile(x, 0.25)
    q3 <- quantile(x, 0.75)
    iqr <- q3 - q1
    
    iqr_outliers <- x[x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)]
    
    # Method 2: Z-score method
    z_scores <- abs((x - mean(x)) / sd(x))
    z_outliers <- x[z_scores > 3]
    
    # Method 3: Modified Z-score method (using MAD)
    mad_val <- mad(x)
    modified_z_scores <- 0.6745 * (x - median(x)) / mad_val
    mad_outliers <- x[abs(modified_z_scores) > 3.5]
    
    list(
      iqr_method = list(
        outliers = iqr_outliers,
        count = length(iqr_outliers),
        percentage = round(length(iqr_outliers) / length(x) * 100, 2)
      ),
      z_score_method = list(
        outliers = z_outliers,
        count = length(z_outliers),
        percentage = round(length(z_outliers) / length(x) * 100, 2)
      ),
      modified_z_method = list(
        outliers = mad_outliers,
        count = length(mad_outliers),
        percentage = round(length(mad_outliers) / length(x) * 100, 2)
      )
    )
  }
  
  # Distribution classification function
  classify_distribution <- function(skewness, kurtosis) {
    
    # Skewness interpretation
    skew_type <- if (abs(skewness) < 0.5) {
      "approximately symmetric"
    } else if (skewness > 0.5) {
      "right-skewed (positive skew)"
    } else {
      "left-skewed (negative skew)"
    }
    
    # Kurtosis interpretation (excess kurtosis)
    excess_kurt <- kurtosis - 3
    kurt_type <- if (abs(excess_kurt) < 0.5) {
      "mesokurtic (normal-like tails)"
    } else if (excess_kurt > 0.5) {
      "leptokurtic (heavy tails)"
    } else {
      "platykurtic (light tails)"
    }
    
    # Overall classification
    if (abs(skewness) < 0.5 && abs(excess_kurt) < 0.5) {
      overall <- "approximately normal"
    } else if (abs(skewness) >= 2 || abs(excess_kurt) >= 2) {
      overall <- "highly non-normal"
    } else {
      overall <- "moderately non-normal"
    }
    
    list(
      skewness_type = skew_type,
      kurtosis_type = kurt_type,
      overall = overall
    )
  }
  
  # =============================================================================
  # GENERATE STATISTICS FOR ALL VARIABLES
  # =============================================================================
  
  if (is.null(group_by)) {
    # Analysis without grouping
    results$basic_statistics <- lapply(variables, function(var) {
      calculate_basic_stats(data[[var]], var, confidence_level)
    })
    names(results$basic_statistics) <- variables
    
    if (include_distribution) {
      results$distribution_analysis <- lapply(variables, function(var) {
        calculate_distribution_stats(data[[var]], var)
      })
      names(results$distribution_analysis) <- variables
    }
    
  } else {
    # Analysis with grouping
    groups <- unique(data[[group_by]])
    groups <- groups[!is.na(groups)]
    
    results$basic_statistics <- list()
    if (include_distribution) {
      results$distribution_analysis <- list()
    }
    
    for (group in groups) {
      group_data <- data[data[[group_by]] == group & !is.na(data[[group_by]]), ]
      
      results$basic_statistics[[as.character(group)]] <- lapply(variables, function(var) {
        calculate_basic_stats(group_data[[var]], var, confidence_level)
      })
      names(results$basic_statistics[[as.character(group)]]) <- variables
      
      if (include_distribution) {
        results$distribution_analysis[[as.character(group)]] <- lapply(variables, function(var) {
          calculate_distribution_stats(group_data[[var]], var)
        })
        names(results$distribution_analysis[[as.character(group)]]) <- variables
      }
    }
  }
  
  # =============================================================================
  # COMPARATIVE ANALYSIS (if grouped)
  # =============================================================================
  
  if (!is.null(group_by)) {
    results$comparative_analysis <- perform_comparative_analysis(data, variables, group_by)
  }
  
  # =============================================================================
  # CORRELATION ANALYSIS
  # =============================================================================
  
  if (length(variables) > 1) {
    results$correlation_analysis <- perform_correlation_analysis(data, variables)
  }
  
  # =============================================================================
  # SUMMARY REPORT
  # =============================================================================
  
  results$summary_report <- generate_summary_report(results, variables, group_by)
  
  # Add class for method dispatch
  class(results) <- c("descriptive_analysis", "list")
  
  return(results)
}

# =============================================================================
# COMPARATIVE ANALYSIS FUNCTIONS
# =============================================================================

#' Perform Comparative Analysis Between Groups
#'
#' @description Analyzes differences between groups using various statistical tests
#' @param data Data frame
#' @param variables Character vector of variables to compare
#' @param group_by Grouping variable
#' @return List of comparative analysis results
perform_comparative_analysis <- function(data, variables, group_by) {
  
  results <- list()
  
  for (var in variables) {
    var_data <- data[!is.na(data[[var]]) & !is.na(data[[group_by]]), ]
    
    if (nrow(var_data) == 0) {
      results[[var]] <- list(error = "No complete cases for analysis")
      next
    }
    
    groups <- unique(var_data[[group_by]])
    n_groups <- length(groups)
    
    # Group-wise statistics
    group_stats <- lapply(groups, function(g) {
      group_values <- var_data[var_data[[group_by]] == g, var]
      list(
        group = g,
        n = length(group_values),
        mean = mean(group_values, na.rm = TRUE),
        median = median(group_values, na.rm = TRUE),
        sd = sd(group_values, na.rm = TRUE),
        iqr = IQR(group_values, na.rm = TRUE)
      )
    })
    names(group_stats) <- groups
    
    # Statistical tests
    tests <- list()
    
    if (n_groups == 2) {
      # Two-group comparisons
      group1_data <- var_data[var_data[[group_by]] == groups[1], var]
      group2_data <- var_data[var_data[[group_by]] == groups[2], var]
      
      # t-test (assuming normality)
      tests$t_test <- tryCatch({
        t.test(group1_data, group2_data)
      }, error = function(e) list(p.value = NA, message = e$message))
      
      # Wilcoxon rank-sum test (non-parametric)
      tests$wilcoxon_test <- tryCatch({
        wilcox.test(group1_data, group2_data)
      }, error = function(e) list(p.value = NA, message = e$message))
      
      # Levene's test for equality of variances
      tests$levene_test <- tryCatch({
        car::leveneTest(as.formula(paste(var, "~", group_by)), data = var_data)
      }, error = function(e) list(p.value = NA, message = e$message))
      
    } else if (n_groups > 2) {
      # Multiple group comparisons
      
      # ANOVA
      tests$anova <- tryCatch({
        aov_result <- aov(as.formula(paste(var, "~", group_by)), data = var_data)
        summary(aov_result)
      }, error = function(e) list(p.value = NA, message = e$message))
      
      # Kruskal-Wallis test (non-parametric ANOVA)
      tests$kruskal_wallis <- tryCatch({
        kruskal.test(as.formula(paste(var, "~", group_by)), data = var_data)
      }, error = function(e) list(p.value = NA, message = e$message))
      
      # Post-hoc tests if ANOVA is significant
      if (!is.null(tests$anova) && !is.na(tests$anova[[1]][["Pr(>F)"]][1]) && 
          tests$anova[[1]][["Pr(>F)"]][1] < 0.05) {
        
        tests$tukey_hsd <- tryCatch({
          aov_result <- aov(as.formula(paste(var, "~", group_by)), data = var_data)
          TukeyHSD(aov_result)
        }, error = function(e) list(message = e$message))
      }
    }
    
    # Effect size calculations
    effect_sizes <- calculate_effect_sizes(var_data, var, group_by)
    
    results[[var]] <- list(
      variable = var,
      n_groups = n_groups,
      group_statistics = group_stats,
      statistical_tests = tests,
      effect_sizes = effect_sizes
    )
  }
  
  return(results)
}

# Effect size calculation function
calculate_effect_sizes <- function(data, var, group_by) {
  
  groups <- unique(data[[group_by]])
  n_groups <- length(groups)
  
  if (n_groups == 2) {
    # Cohen's d for two groups
    group1_data <- data[data[[group_by]] == groups[1], var]
    group2_data <- data[data[[group_by]] == groups[2], var]
    
    mean1 <- mean(group1_data, na.rm = TRUE)
    mean2 <- mean(group2_data, na.rm = TRUE)
    sd1 <- sd(group1_data, na.rm = TRUE)
    sd2 <- sd(group2_data, na.rm = TRUE)
    n1 <- length(group1_data)
    n2 <- length(group2_data)
    
    # Pooled standard deviation
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    
    cohens_d <- (mean1 - mean2) / pooled_sd
    
    # Interpret effect size
    d_interpretation <- if (abs(cohens_d) < 0.2) {
      "negligible"
    } else if (abs(cohens_d) < 0.5) {
      "small"
    } else if (abs(cohens_d) < 0.8) {
      "medium"
    } else {
      "large"
    }
    
    list(
      cohens_d = cohens_d,
      interpretation = d_interpretation
    )
    
  } else if (n_groups > 2) {
    # Eta-squared for multiple groups
    aov_result <- aov(as.formula(paste(var, "~", group_by)), data = data)
    ss_between <- sum(aov_result$effects[-1]^2) * aov_result$df.residual
    ss_total <- sum((data[[var]] - mean(data[[var]], na.rm = TRUE))^2, na.rm = TRUE)
    
    eta_squared <- ss_between / ss_total
    
    # Interpret effect size
    eta_interpretation <- if (eta_squared < 0.01) {
      "negligible"
    } else if (eta_squared < 0.06) {
      "small"
    } else if (eta_squared < 0.14) {
      "medium"
    } else {
      "large"
    }
    
    list(
      eta_squared = eta_squared,
      interpretation = eta_interpretation
    )
  }
}

# =============================================================================
# CORRELATION ANALYSIS
# =============================================================================

#' Perform Comprehensive Correlation Analysis
#'
#' @description Analyzes correlations between variables using multiple methods
#' @param data Data frame
#' @param variables Character vector of variables to analyze
#' @return List of correlation analysis results
perform_correlation_analysis <- function(data, variables) {
  
  # Select only the variables of interest
  cor_data <- data[variables]
  
  # Remove rows with any missing values
  cor_data_complete <- cor_data[complete.cases(cor_data), ]
  
  if (nrow(cor_data_complete) < 3) {
    return(list(error = "Insufficient complete cases for correlation analysis"))
  }
  
  results <- list()
  
  # Pearson correlations
  results$pearson <- list(
    correlation_matrix = cor(cor_data_complete, method = "pearson"),
    p_values = cor_p_values(cor_data_complete, method = "pearson")
  )
  
  # Spearman correlations
  results$spearman <- list(
    correlation_matrix = cor(cor_data_complete, method = "spearman"),
    p_values = cor_p_values(cor_data_complete, method = "spearman")
  )
  
  # Kendall correlations
  results$kendall <- list(
    correlation_matrix = cor(cor_data_complete, method = "kendall"),
    p_values = cor_p_values(cor_data_complete, method = "kendall")
  )
  
  # Correlation strength interpretation
  results$interpretation <- interpret_correlations(results$pearson$correlation_matrix)
  
  # Partial correlations (if more than 2 variables)
  if (length(variables) > 2) {
    results$partial_correlations <- tryCatch({
      psych::partial.r(cor_data_complete)
    }, error = function(e) list(message = e$message))
  }
  
  return(results)
}

# Helper function to calculate correlation p-values
cor_p_values <- function(data, method = "pearson") {
  n <- ncol(data)
  p_matrix <- matrix(NA, n, n)
  rownames(p_matrix) <- colnames(p_matrix) <- colnames(data)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        test_result <- cor.test(data[, i], data[, j], method = method)
        p_matrix[i, j] <- test_result$p.value
      } else {
        p_matrix[i, j] <- 0  # Perfect correlation with itself
      }
    }
  }
  
  return(p_matrix)
}

# Interpret correlation strengths
interpret_correlations <- function(cor_matrix) {
  
  # Extract upper triangle (avoid diagonal and duplicates)
  upper_tri <- upper.tri(cor_matrix)
  cor_values <- cor_matrix[upper_tri]
  
  # Classify correlation strengths
  strengths <- sapply(cor_values, function(r) {
    abs_r <- abs(r)
    if (abs_r < 0.1) {
      "negligible"
    } else if (abs_r < 0.3) {
      "weak"
    } else if (abs_r < 0.5) {
      "moderate"
    } else if (abs_r < 0.7) {
      "strong"
    } else {
      "very strong"
    }
  })
  
  # Get variable pairs
  var_names <- rownames(cor_matrix)
  pairs <- which(upper_tri, arr.ind = TRUE)
  
  interpretation_df <- data.frame(
    variable1 = var_names[pairs[, 1]],
    variable2 = var_names[pairs[, 2]],
    correlation = cor_values,
    abs_correlation = abs(cor_values),
    direction = ifelse(cor_values > 0, "positive", "negative"),
    strength = strengths,
    stringsAsFactors = FALSE
  )
  
  # Sort by absolute correlation strength
  interpretation_df <- interpretation_df[order(interpretation_df$abs_correlation, decreasing = TRUE), ]
  
  return(interpretation_df)
}

# =============================================================================
# SUMMARY REPORT GENERATION
# =============================================================================

#' Generate Summary Report
#'
#' @description Creates a comprehensive summary of the descriptive analysis
#' @param results Analysis results object
#' @param variables Variables analyzed
#' @param group_by Grouping variable (if any)
#' @return Character vector with summary report
generate_summary_report <- function(results, variables, group_by) {
  
  report <- character()
  
  # Header
  report <- c(report, "COMPREHENSIVE DESCRIPTIVE STATISTICS REPORT")
  report <- c(report, paste(rep("=", 50), collapse = ""))
  report <- c(report, paste("Analysis Date:", format(results$metadata$analysis_date, "%Y-%m-%d %H:%M:%S")))
  report <- c(report, paste("Sample Size:", results$metadata$sample_size))
  report <- c(report, paste("Variables Analyzed:", length(variables)))
  
  if (!is.null(group_by)) {
    report <- c(report, paste("Grouped By:", group_by))
  }
  
  report <- c(report, "")
  
  # Variable summaries
  report <- c(report, "VARIABLE SUMMARIES:")
  report <- c(report, paste(rep("-", 20), collapse = ""))
  
  for (var in variables) {
    if (is.null(group_by)) {
      stats <- results$basic_statistics[[var]]
    } else {
      # For grouped analysis, summarize across groups
      stats <- results$basic_statistics[[1]][[var]]  # Use first group as template
    }
    
    if (!"error" %in% names(stats)) {
      report <- c(report, paste("Variable:", var))
      report <- c(report, paste("  Sample Size:", stats$n))
      report <- c(report, paste("  Missing Values:", stats$missing, paste0("(", stats$missing_pct, "%)")))
      report <- c(report, paste("  Mean:", round(stats$mean, 3)))
      report <- c(report, paste("  Median:", round(stats$median, 3)))
      report <- c(report, paste("  Std Dev:", round(stats$std_dev, 3)))
      report <- c(report, paste("  Range:", round(stats$min, 3), "to", round(stats$max, 3)))
      
      if ("coefficient_of_variation" %in% names(stats) && !is.na(stats$coefficient_of_variation)) {
        report <- c(report, paste("  Coefficient of Variation:", round(stats$coefficient_of_variation, 2), "%"))
      }
      
      report <- c(report, "")
    }
  }
  
  # Distribution insights
  if ("distribution_analysis" %in% names(results)) {
    report <- c(report, "DISTRIBUTION INSIGHTS:")
    report <- c(report, paste(rep("-", 20), collapse = ""))
    
    for (var in variables) {
      if (is.null(group_by)) {
        dist_stats <- results$distribution_analysis[[var]]
      } else {
        dist_stats <- results$distribution_analysis[[1]][[var]]  # Use first group
      }
      
      if (!"error" %in% names(dist_stats)) {
        report <- c(report, paste("Variable:", var))
        report <- c(report, paste("  Distribution Type:", dist_stats$distribution_type$overall))
        report <- c(report, paste("  Skewness:", round(dist_stats$skewness, 3), "-", dist_stats$distribution_type$skewness_type))
        report <- c(report, paste("  Kurtosis:", round(dist_stats$kurtosis, 3), "-", dist_stats$distribution_type$kurtosis_type))
        
        # Outliers summary
        if ("outliers" %in% names(dist_stats)) {
          iqr_outliers <- dist_stats$outliers$iqr_method$count
          report <- c(report, paste("  Outliers (IQR method):", iqr_outliers, 
                                   paste0("(", dist_stats$outliers$iqr_method$percentage, "%)")))
        }
        
        report <- c(report, "")
      }
    }
  }
  
  # Correlation insights
  if ("correlation_analysis" %in% names(results) && !"error" %in% names(results$correlation_analysis)) {
    report <- c(report, "CORRELATION INSIGHTS:")
    report <- c(report, paste(rep("-", 20), collapse = ""))
    
    top_correlations <- head(results$correlation_analysis$interpretation, 3)
    
    for (i in 1:nrow(top_correlations)) {
      cor_info <- top_correlations[i, ]
      report <- c(report, paste("  ", cor_info$variable1, "vs", cor_info$variable2, ":"))
      report <- c(report, paste("    Correlation:", round(cor_info$correlation, 3)))
      report <- c(report, paste("    Strength:", cor_info$strength, cor_info$direction))
    }
    
    report <- c(report, "")
  }
  
  # Recommendations
  report <- c(report, "RECOMMENDATIONS:")
  report <- c(report, paste(rep("-", 15), collapse = ""))
  
  # Add specific recommendations based on findings
  if ("distribution_analysis" %in% names(results)) {
    non_normal_vars <- character()
    for (var in variables) {
      if (is.null(group_by)) {
        dist_stats <- results$distribution_analysis[[var]]
      } else {
        dist_stats <- results$distribution_analysis[[1]][[var]]
      }
      
      if (!"error" %in% names(dist_stats) && 
          dist_stats$distribution_type$overall != "approximately normal") {
        non_normal_vars <- c(non_normal_vars, var)
      }
    }
    
    if (length(non_normal_vars) > 0) {
      report <- c(report, paste("- Consider non-parametric tests for variables:", 
                               paste(non_normal_vars, collapse = ", ")))
    }
  }
  
  report <- c(report, "- Review outliers and consider their impact on analysis")
  report <- c(report, "- Check for data quality issues in variables with high missing percentages")
  
  if ("correlation_analysis" %in% names(results) && !"error" %in% names(results$correlation_analysis)) {
    strong_cors <- results$correlation_analysis$interpretation[
      results$correlation_analysis$interpretation$strength %in% c("strong", "very strong"), ]
    
    if (nrow(strong_cors) > 0) {
      report <- c(report, "- Consider multicollinearity in modeling with strongly correlated variables")
    }
  }
  
  return(report)
}

# =============================================================================
# PRINT METHOD FOR DESCRIPTIVE ANALYSIS
# =============================================================================

#' Print Method for Descriptive Analysis
#'
#' @param x Descriptive analysis object
#' @param ... Additional arguments
#' @export
print.descriptive_analysis <- function(x, ...) {
  
  cat(paste(x$summary_report, collapse = "\n"))
  cat("\n\n")
  
  # Additional detailed output options
  cat("Available components:\n")
  cat("- $basic_statistics: Comprehensive descriptive statistics\n")
  cat("- $distribution_analysis: Distribution shape and normality tests\n")
  cat("- $correlation_analysis: Correlation matrices and interpretations\n")
  
  if ("comparative_analysis" %in% names(x)) {
    cat("- $comparative_analysis: Group comparison tests and effect sizes\n")
  }
  
  cat("- $summary_report: Text summary of findings\n")
  cat("- $metadata: Analysis metadata and parameters\n")
}

# =============================================================================
# DEMONSTRATION FUNCTION
# =============================================================================

#' Run Comprehensive Descriptive Statistics Demonstration
#'
#' @description Demonstrates the descriptive statistics framework
#' @param verbose Logical, whether to show detailed output
#' @return Invisible list of demonstration results
#' @export
run_descriptive_stats_demo <- function(verbose = TRUE) {
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("    COMPREHENSIVE DESCRIPTIVE STATISTICS PORTFOLIO\n")
    cat("="*60, "\n\n")
  }
  
  # Generate demonstration data
  set.seed(42)
  demo_data <- data.frame(
    participant_id = 1:200,
    age = sample(18:65, 200, replace = TRUE),
    income = exp(rnorm(200, log(50000), 0.5)),  # Log-normal distribution
    satisfaction = pmax(1, pmin(10, rnorm(200, 7, 2))),  # Bounded normal
    group = sample(c("Control", "Treatment A", "Treatment B"), 200, replace = TRUE),
    education_years = sample(12:20, 200, replace = TRUE, prob = c(rep(0.1, 4), rep(0.15, 5))),
    test_score = rbeta(200, 2, 5) * 100,  # Beta distribution scaled to 0-100
    stringsAsFactors = FALSE
  )
  
  # Add some missing values for realism
  demo_data$income[sample(1:200, 10)] <- NA
  demo_data$satisfaction[sample(1:200, 5)] <- NA
  
  results <- list()
  
  # Basic descriptive analysis
  if (verbose) cat("1. Running basic descriptive analysis...\n")
  results$basic_analysis <- comprehensive_descriptive_analysis(
    data = demo_data,
    variables = c("age", "income", "satisfaction", "education_years", "test_score"),
    include_distribution = TRUE
  )
  
  # Grouped analysis
  if (verbose) cat("2. Running grouped analysis...\n")
  results$grouped_analysis <- comprehensive_descriptive_analysis(
    data = demo_data,
    variables = c("income", "satisfaction", "test_score"),
    group_by = "group",
    include_distribution = TRUE
  )
  
  # Print results if verbose
  if (verbose) {
    cat("\n3. Basic Analysis Results:\n")
    cat("="*30, "\n")
    print(results$basic_analysis)
    
    cat("\n4. Grouped Analysis Results:\n")
    cat("="*30, "\n")
    print(results$grouped_analysis)
    
    cat("\n", "="*60, "\n")
    cat("    DESCRIPTIVE STATISTICS DEMONSTRATION COMPLETE\n")
    cat("="*60, "\n")
  }
  
  invisible(results)
}

# =============================================================================
# EXAMPLE USAGE
# =============================================================================

if (interactive()) {
  # Run the comprehensive demonstration
  demo_results <- run_descriptive_stats_demo(verbose = TRUE)
  
  # Access specific results
  # demo_results$basic_analysis$basic_statistics$age
  # demo_results$basic_analysis$correlation_analysis$interpretation
  # demo_results$grouped_analysis$comparative_analysis$income
}
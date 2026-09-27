#' @title Comprehensive Hypothesis Testing Framework
#' @description Advanced statistical hypothesis testing with effect sizes and power analysis
#' @author Portfolio Developer
#' @date 2025

library(dplyr)
library(broom)
library(pwr)
library(car)
library(nortest)

# =============================================================================
# COMPREHENSIVE HYPOTHESIS TESTING FRAMEWORK
# =============================================================================

#' Comprehensive Hypothesis Testing Suite
#'
#' @description Performs appropriate hypothesis tests based on data characteristics
#' @param data Data frame containing the variables
#' @param response_var Name of the response/dependent variable
#' @param predictor_var Name of the predictor/independent variable (optional)
#' @param group_var Name of grouping variable for group comparisons
#' @param test_type Type of test to perform ("auto", "parametric", "nonparametric")
#' @param alpha Significance level (default: 0.05)
#' @param alternative Alternative hypothesis ("two.sided", "less", "greater")
#' @param effect_size_calc Logical, whether to calculate effect sizes
#' @param power_analysis Logical, whether to perform power analysis
#' @return Comprehensive hypothesis testing results
#' @export
comprehensive_hypothesis_test <- function(data,
                                         response_var,
                                         predictor_var = NULL,
                                         group_var = NULL,
                                         test_type = "auto",
                                         alpha = 0.05,
                                         alternative = "two.sided",
                                         effect_size_calc = TRUE,
                                         power_analysis = TRUE) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }
  
  if (!response_var %in% names(data)) {
    stop("Response variable not found in data")
  }
  
  if (!is.null(predictor_var) && !predictor_var %in% names(data)) {
    stop("Predictor variable not found in data")
  }
  
  if (!is.null(group_var) && !group_var %in% names(data)) {
    stop("Group variable not found in data")
  }
  
  # Initialize results structure
  results <- list(
    metadata = list(
      test_date = Sys.time(),
      response_var = response_var,
      predictor_var = predictor_var,
      group_var = group_var,
      alpha = alpha,
      alternative = alternative,
      sample_size = nrow(data)
    ),
    assumptions = list(),
    tests = list(),
    effect_sizes = list(),
    power_analysis = list(),
    recommendations = character()
  )
  
  # Determine test scenario and appropriate tests
  test_scenario <- determine_test_scenario(data, response_var, predictor_var, group_var)
  results$metadata$test_scenario = test_scenario$scenario
  results$metadata$data_types = test_scenario$data_types
  
  # Assumption checking
  if (test_type %in% c("auto", "parametric")) {
    results$assumptions <- check_statistical_assumptions(data, response_var, predictor_var, group_var, test_scenario)
  }
  
  # Perform appropriate tests
  if (test_scenario$scenario == "one_sample") {
    results$tests <- perform_one_sample_tests(data, response_var, test_type, alpha, alternative, results$assumptions)
    
  } else if (test_scenario$scenario == "two_sample") {
    results$tests <- perform_two_sample_tests(data, response_var, group_var, test_type, alpha, alternative, results$assumptions)
    
  } else if (test_scenario$scenario == "multi_sample") {
    results$tests <- perform_multi_sample_tests(data, response_var, group_var, test_type, alpha, alternative, results$assumptions)
    
  } else if (test_scenario$scenario == "correlation") {
    results$tests <- perform_correlation_tests(data, response_var, predictor_var, test_type, alpha, alternative)
    
  } else if (test_scenario$scenario == "independence") {
    results$tests <- perform_independence_tests(data, response_var, predictor_var, alpha)
  }
  
  # Effect size calculations
  if (effect_size_calc) {
    results$effect_sizes <- calculate_effect_sizes(data, response_var, predictor_var, group_var, test_scenario, results$tests)
  }
  
  # Power analysis
  if (power_analysis) {
    results$power_analysis <- perform_power_analysis(data, response_var, predictor_var, group_var, test_scenario, results$effect_sizes, alpha)
  }
  
  # Generate recommendations
  results$recommendations <- generate_test_recommendations(results, test_scenario, test_type)
  
  # Summary interpretation
  results$summary <- generate_test_summary(results, alpha)
  
  class(results) <- c("hypothesis_test_results", "list")
  return(results)
}

# =============================================================================
# TEST SCENARIO DETERMINATION
# =============================================================================

#' Determine Appropriate Test Scenario
determine_test_scenario <- function(data, response_var, predictor_var, group_var) {
  
  response_type <- determine_variable_type(data[[response_var]])
  
  scenario_info <- list(
    data_types = list(response = response_type)
  )
  
  if (!is.null(predictor_var)) {
    predictor_type <- determine_variable_type(data[[predictor_var]])
    scenario_info$data_types$predictor <- predictor_type
  }
  
  if (!is.null(group_var)) {
    group_type <- determine_variable_type(data[[group_var]])
    scenario_info$data_types$group <- group_type
    n_groups <- length(unique(data[[group_var]][!is.na(data[[group_var]])]))
    scenario_info$n_groups <- n_groups
  }
  
  # Determine scenario
  if (!is.null(group_var) && response_type == "continuous") {
    if (scenario_info$n_groups == 2) {
      scenario_info$scenario <- "two_sample"
    } else {
      scenario_info$scenario <- "multi_sample"
    }
  } else if (!is.null(predictor_var) && response_type == "continuous" && scenario_info$data_types$predictor == "continuous") {
    scenario_info$scenario <- "correlation"
  } else if (!is.null(predictor_var) && (response_type == "categorical" || scenario_info$data_types$predictor == "categorical")) {
    scenario_info$scenario <- "independence"
  } else if (response_type == "continuous") {
    scenario_info$scenario <- "one_sample"
  } else {
    scenario_info$scenario <- "goodness_of_fit"
  }
  
  return(scenario_info)
}

# Helper function to determine variable type
determine_variable_type <- function(variable) {
  if (is.numeric(variable)) {
    if (length(unique(variable[!is.na(variable)])) <= 10) {
      return("discrete")
    } else {
      return("continuous")
    }
  } else {
    return("categorical")
  }
}

# =============================================================================
# ASSUMPTION CHECKING
# =============================================================================

#' Check Statistical Assumptions
check_statistical_assumptions <- function(data, response_var, predictor_var, group_var, test_scenario) {
  
  assumptions <- list()
  
  # Clean data for assumption testing
  if (!is.null(group_var)) {
    clean_data <- data[!is.na(data[[response_var]]) & !is.na(data[[group_var]]), ]
  } else {
    clean_data <- data[!is.na(data[[response_var]]), ]
  }
  
  if (nrow(clean_data) < 3) {
    return(list(error = "Insufficient data for assumption testing"))
  }
  
  # Normality tests
  assumptions$normality <- test_normality(clean_data, response_var, group_var)
  
  # Homogeneity of variance (if applicable)
  if (!is.null(group_var) && test_scenario$scenario %in% c("two_sample", "multi_sample")) {
    assumptions$homogeneity <- test_homogeneity_of_variance(clean_data, response_var, group_var)
  }
  
  # Independence (general check)
  assumptions$independence <- test_independence_assumption(clean_data, response_var)
  
  # Linearity (for correlation)
  if (!is.null(predictor_var) && test_scenario$scenario == "correlation") {
    assumptions$linearity <- test_linearity(clean_data, response_var, predictor_var)
  }
  
  return(assumptions)
}

# Normality testing function
test_normality <- function(data, response_var, group_var = NULL) {
  
  normality_results <- list()
  
  if (is.null(group_var)) {
    # Test normality for single group
    x <- data[[response_var]]
    normality_results$overall <- perform_normality_tests(x, "overall")
    
  } else {
    # Test normality for each group
    groups <- unique(data[[group_var]])
    
    for (group in groups) {
      group_data <- data[data[[group_var]] == group, response_var]
      normality_results[[as.character(group)]] <- perform_normality_tests(group_data, as.character(group))
    }
    
    # Overall test using residuals if possible
    if (length(groups) > 1) {
      # Test normality of residuals from group means
      group_means <- tapply(data[[response_var]], data[[group_var]], mean, na.rm = TRUE)
      residuals <- data[[response_var]] - group_means[data[[group_var]]]
      normality_results$residuals <- perform_normality_tests(residuals, "residuals")
    }
  }
  
  return(normality_results)
}

# Perform multiple normality tests
perform_normality_tests <- function(x, group_name) {
  
  x_clean <- x[!is.na(x)]
  n <- length(x_clean)
  
  if (n < 3) {
    return(list(error = "Insufficient data for normality testing"))
  }
  
  tests <- list()
  
  # Shapiro-Wilk test (for n <= 5000)
  if (n >= 3 && n <= 5000) {
    tests$shapiro_wilk <- tryCatch({
      shapiro.test(x_clean)
    }, error = function(e) list(p.value = NA, statistic = NA, method = "Shapiro-Wilk test"))
  }
  
  # Anderson-Darling test
  if (n >= 7) {
    tests$anderson_darling <- tryCatch({
      nortest::ad.test(x_clean)
    }, error = function(e) list(p.value = NA, statistic = NA, method = "Anderson-Darling test"))
  }
  
  # Kolmogorov-Smirnov test
  if (n >= 3) {
    tests$kolmogorov_smirnov <- tryCatch({
      ks.test(x_clean, "pnorm", mean(x_clean), sd(x_clean))
    }, error = function(e) list(p.value = NA, statistic = NA, method = "Kolmogorov-Smirnov test"))
  }
  
  # Jarque-Bera test
  if (n >= 3) {
    tests$jarque_bera <- tryCatch({
      nortest::jb.test(x_clean)
    }, error = function(e) list(p.value = NA, statistic = NA, method = "Jarque-Bera test"))
  }
  
  # Overall assessment
  p_values <- sapply(tests, function(test) test$p.value)
  p_values <- p_values[!is.na(p_values)]
  
  if (length(p_values) > 0) {
    # Use Bonferroni correction for multiple testing
    adjusted_alpha <- 0.05 / length(p_values)
    normality_violated <- any(p_values < adjusted_alpha)
    
    tests$summary <- list(
      group = group_name,
      n_tests = length(p_values),
      min_p_value = min(p_values),
      normality_assumption = ifelse(normality_violated, "Violated", "Satisfied"),
      recommendation = ifelse(normality_violated, "Use non-parametric tests", "Parametric tests appropriate")
    )
  }
  
  return(tests)
}

# Test homogeneity of variance
test_homogeneity_of_variance <- function(data, response_var, group_var) {
  
  formula_str <- paste(response_var, "~", group_var)
  
  tests <- list()
  
  # Levene's test
  tests$levene <- tryCatch({
    car::leveneTest(as.formula(formula_str), data = data)
  }, error = function(e) list(Pr = NA, method = "Levene's test"))
  
  # Bartlett's test (assumes normality)
  tests$bartlett <- tryCatch({
    bartlett.test(as.formula(formula_str), data = data)
  }, error = function(e) list(p.value = NA, method = "Bartlett's test"))
  
  # Fligner-Killeen test (robust)
  tests$fligner <- tryCatch({
    fligner.test(as.formula(formula_str), data = data)
  }, error = function(e) list(p.value = NA, method = "Fligner-Killeen test"))
  
  # Summary assessment
  p_values <- c(
    if("Pr(>F)" %in% names(tests$levene)) tests$levene$`Pr(>F)`[1] else NA,
    tests$bartlett$p.value,
    tests$fligner$p.value
  )
  
  p_values <- p_values[!is.na(p_values)]
  
  if (length(p_values) > 0) {
    homogeneity_violated <- any(p_values < 0.05)
    
    tests$summary <- list(
      min_p_value = min(p_values),
      homogeneity_assumption = ifelse(homogeneity_violated, "Violated", "Satisfied"),
      recommendation = ifelse(homogeneity_violated, 
                              "Use Welch's t-test or non-parametric alternatives", 
                              "Equal variance tests appropriate")
    )
  }
  
  return(tests)
}

# Test independence assumption
test_independence_assumption <- function(data, response_var) {
  
  x <- data[[response_var]]
  n <- length(x)
  
  # Simple runs test for randomness
  independence_test <- list()
  
  if (n >= 10) {
    independence_test$runs_test <- tryCatch({
      # Convert to binary based on median
      median_val <- median(x, na.rm = TRUE)
      binary_seq <- ifelse(x > median_val, 1, 0)
      binary_seq <- binary_seq[!is.na(binary_seq)]
      
      # Count runs
      runs <- 1
      for (i in 2:length(binary_seq)) {
        if (binary_seq[i] != binary_seq[i-1]) {
          runs <- runs + 1
        }
      }
      
      # Expected runs and variance
      n1 <- sum(binary_seq == 1)
      n2 <- sum(binary_seq == 0)
      expected_runs <- (2 * n1 * n2) / (n1 + n2) + 1
      var_runs <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) / ((n1 + n2)^2 * (n1 + n2 - 1))
      
      z_score <- (runs - expected_runs) / sqrt(var_runs)
      p_value <- 2 * (1 - pnorm(abs(z_score)))
      
      list(
        statistic = z_score,
        p.value = p_value,
        method = "Runs test for randomness"
      )
      
    }, error = function(e) list(p.value = NA, method = "Runs test"))
  }
  
  independence_test$assumption <- "Generally assumed for cross-sectional data"
  independence_test$note <- "Independence should be ensured by study design"
  
  return(independence_test)
}

# Test linearity assumption
test_linearity <- function(data, response_var, predictor_var) {
  
  x <- data[[predictor_var]]
  y <- data[[response_var]]
  
  # Remove missing values
  complete_cases <- !is.na(x) & !is.na(y)
  x <- x[complete_cases]
  y <- y[complete_cases]
  
  if (length(x) < 10) {
    return(list(error = "Insufficient data for linearity testing"))
  }
  
  linearity_tests <- list()
  
  # Simple linearity check using correlation
  linearity_tests$correlation <- cor.test(x, y)
  
  # Residuals from linear fit
  lm_fit <- lm(y ~ x)
  residuals <- residuals(lm_fit)
  fitted_values <- fitted(lm_fit)
  
  # Test for pattern in residuals
  linearity_tests$residual_correlation <- cor.test(fitted_values, residuals)
  
  linearity_tests$summary <- list(
    correlation = linearity_tests$correlation$estimate,
    linearity_assumption = ifelse(abs(linearity_tests$residual_correlation$estimate) < 0.1, "Satisfied", "Questionable"),
    recommendation = ifelse(abs(linearity_tests$residual_correlation$estimate) < 0.1, 
                           "Linear relationship appropriate", 
                           "Consider non-linear models")
  )
  
  return(linearity_tests)
}

# =============================================================================
# SPECIFIC TEST IMPLEMENTATIONS
# =============================================================================

#' Perform One-Sample Tests
perform_one_sample_tests <- function(data, response_var, test_type, alpha, alternative, assumptions) {
  
  x <- data[[response_var]]
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) == 0) {
    return(list(error = "No valid data for testing"))
  }
  
  tests <- list()
  
  # Determine if parametric tests are appropriate
  use_parametric <- test_type == "parametric" || 
    (test_type == "auto" && 
     !is.null(assumptions$normality$overall$summary) && 
     assumptions$normality$overall$summary$normality_assumption == "Satisfied")
  
  # Default test value (can be modified as parameter)
  mu <- 0
  
  if (use_parametric) {
    # One-sample t-test
    tests$t_test <- tryCatch({
      t.test(x_clean, mu = mu, alternative = alternative, conf.level = 1 - alpha)
    }, error = function(e) list(p.value = NA, method = "One Sample t-test"))
  }
  
  # Non-parametric alternative: Wilcoxon signed-rank test
  tests$wilcoxon <- tryCatch({
    wilcox.test(x_clean, mu = mu, alternative = alternative, conf.level = 1 - alpha)
  }, error = function(e) list(p.value = NA, method = "Wilcoxon signed rank test"))
  
  # Sign test
  tests$sign_test <- perform_sign_test(x_clean, mu, alternative)
  
  return(tests)
}

#' Perform Two-Sample Tests
perform_two_sample_tests <- function(data, response_var, group_var, test_type, alpha, alternative, assumptions) {
  
  # Clean data
  clean_data <- data[!is.na(data[[response_var]]) & !is.na(data[[group_var]]), ]
  groups <- unique(clean_data[[group_var]])
  
  if (length(groups) != 2) {
    return(list(error = "Exactly two groups required for two-sample tests"))
  }
  
  group1_data <- clean_data[clean_data[[group_var]] == groups[1], response_var]
  group2_data <- clean_data[clean_data[[group_var]] == groups[2], response_var]
  
  if (length(group1_data) == 0 || length(group2_data) == 0) {
    return(list(error = "Both groups must have data"))
  }
  
  tests <- list()
  
  # Determine test appropriateness
  normality_ok <- !is.null(assumptions$normality) && 
    all(sapply(assumptions$normality[groups], function(x) 
      !is.null(x$summary) && x$summary$normality_assumption == "Satisfied"))
  
  homogeneity_ok <- !is.null(assumptions$homogeneity) && 
    !is.null(assumptions$homogeneity$summary) && 
    assumptions$homogeneity$summary$homogeneity_assumption == "Satisfied"
  
  use_parametric <- test_type == "parametric" || 
    (test_type == "auto" && normality_ok)
  
  if (use_parametric) {
    # Student's t-test (equal variances)
    if (homogeneity_ok || test_type == "parametric") {
      tests$student_t <- tryCatch({
        t.test(group1_data, group2_data, var.equal = TRUE, alternative = alternative, conf.level = 1 - alpha)
      }, error = function(e) list(p.value = NA, method = "Two Sample t-test"))
    }
    
    # Welch's t-test (unequal variances)
    tests$welch_t <- tryCatch({
      t.test(group1_data, group2_data, var.equal = FALSE, alternative = alternative, conf.level = 1 - alpha)
    }, error = function(e) list(p.value = NA, method = "Welch Two Sample t-test"))
  }
  
  # Non-parametric tests
  tests$mann_whitney <- tryCatch({
    wilcox.test(group1_data, group2_data, alternative = alternative, conf.level = 1 - alpha)
  }, error = function(e) list(p.value = NA, method = "Wilcoxon rank sum test"))
  
  # Permutation test
  tests$permutation <- perform_permutation_test(group1_data, group2_data, alternative, n_permutations = 1000)
  
  return(tests)
}

#' Perform Multi-Sample Tests
perform_multi_sample_tests <- function(data, response_var, group_var, test_type, alpha, alternative, assumptions) {
  
  # Clean data
  clean_data <- data[!is.na(data[[response_var]]) & !is.na(data[[group_var]]), ]
  groups <- unique(clean_data[[group_var]])
  
  if (length(groups) < 3) {
    return(list(error = "At least three groups required for multi-sample tests"))
  }
  
  tests <- list()
  
  # Check assumptions
  normality_ok <- !is.null(assumptions$normality) && 
    all(sapply(assumptions$normality[as.character(groups)], function(x) 
      !is.null(x$summary) && x$summary$normality_assumption == "Satisfied"))
  
  homogeneity_ok <- !is.null(assumptions$homogeneity) && 
    !is.null(assumptions$homogeneity$summary) && 
    assumptions$homogeneity$summary$homogeneity_assumption == "Satisfied"
  
  use_parametric <- test_type == "parametric" || 
    (test_type == "auto" && normality_ok && homogeneity_ok)
  
  # Formula for tests
  formula_str <- paste(response_var, "~", group_var)
  
  if (use_parametric) {
    # One-way ANOVA
    tests$anova <- tryCatch({
      aov_result <- aov(as.formula(formula_str), data = clean_data)
      summary_result <- summary(aov_result)
      
      list(
        aov_object = aov_result,
        summary = summary_result,
        p.value = summary_result[[1]][["Pr(>F)"]][1],
        statistic = summary_result[[1]][["F value"]][1],
        method = "One-way ANOVA"
      )
    }, error = function(e) list(p.value = NA, method = "One-way ANOVA"))
    
    # Post-hoc tests if ANOVA is significant
    if (!is.null(tests$anova$p.value) && !is.na(tests$anova$p.value) && tests$anova$p.value < alpha) {
      tests$tukey_hsd <- tryCatch({
        TukeyHSD(tests$anova$aov_object)
      }, error = function(e) list(message = "TukeyHSD failed"))
      
      tests$bonferroni <- tryCatch({
        pairwise.t.test(clean_data[[response_var]], clean_data[[group_var]], 
                       p.adjust.method = "bonferroni")
      }, error = function(e) list(message = "Bonferroni correction failed"))
    }
  }
  
  # Non-parametric alternative: Kruskal-Wallis test
  tests$kruskal_wallis <- tryCatch({
    kruskal.test(as.formula(formula_str), data = clean_data)
  }, error = function(e) list(p.value = NA, method = "Kruskal-Wallis rank sum test"))
  
  # Post-hoc for Kruskal-Wallis if significant
  if (!is.null(tests$kruskal_wallis$p.value) && !is.na(tests$kruskal_wallis$p.value) && 
      tests$kruskal_wallis$p.value < alpha) {
    tests$dunn_test <- tryCatch({
      # Pairwise Wilcoxon with Bonferroni correction
      pairwise.wilcox.test(clean_data[[response_var]], clean_data[[group_var]],
                          p.adjust.method = "bonferroni")
    }, error = function(e) list(message = "Dunn test failed"))
  }
  
  return(tests)
}

#' Perform Correlation Tests
perform_correlation_tests <- function(data, response_var, predictor_var, test_type, alpha, alternative) {
  
  x <- data[[predictor_var]]
  y <- data[[response_var]]
  
  # Remove missing values
  complete_cases <- !is.na(x) & !is.na(y)
  x <- x[complete_cases]
  y <- y[complete_cases]
  
  if (length(x) < 3) {
    return(list(error = "Insufficient data for correlation testing"))
  }
  
  tests <- list()
  
  # Pearson correlation (parametric)
  tests$pearson <- tryCatch({
    cor.test(x, y, method = "pearson", alternative = alternative, conf.level = 1 - alpha)
  }, error = function(e) list(p.value = NA, method = "Pearson's product-moment correlation"))
  
  # Spearman correlation (non-parametric)
  tests$spearman <- tryCatch({
    cor.test(x, y, method = "spearman", alternative = alternative, conf.level = 1 - alpha)
  }, error = function(e) list(p.value = NA, method = "Spearman's rank correlation rho"))
  
  # Kendall correlation (non-parametric)
  tests$kendall <- tryCatch({
    cor.test(x, y, method = "kendall", alternative = alternative, conf.level = 1 - alpha)
  }, error = function(e) list(p.value = NA, method = "Kendall's rank correlation tau"))
  
  return(tests)
}

#' Perform Independence Tests
perform_independence_tests <- function(data, response_var, predictor_var, alpha) {
  
  # Create contingency table
  contingency_table <- table(data[[response_var]], data[[predictor_var]])
  
  if (any(dim(contingency_table) == 0)) {
    return(list(error = "Cannot create contingency table - no valid data"))
  }
  
  tests <- list()
  
  # Chi-square test
  tests$chi_square <- tryCatch({
    chisq.test(contingency_table)
  }, error = function(e) list(p.value = NA, method = "Pearson's Chi-squared test"))
  
  # Fisher's exact test (for 2x2 tables or small expected frequencies)
  if (all(dim(contingency_table) == 2) || any(chisq.test(contingency_table)$expected < 5)) {
    tests$fisher_exact <- tryCatch({
      fisher.test(contingency_table)
    }, error = function(e) list(p.value = NA, method = "Fisher's Exact Test"))
  }
  
  # Store contingency table
  tests$contingency_table <- contingency_table
  
  return(tests)
}

# =============================================================================
# HELPER TEST FUNCTIONS
# =============================================================================

#' Perform Sign Test
perform_sign_test <- function(x, mu, alternative) {
  
  differences <- x - mu
  positive <- sum(differences > 0)
  negative <- sum(differences < 0)
  ties <- sum(differences == 0)
  n <- positive + negative  # Exclude ties
  
  if (n == 0) {
    return(list(p.value = NA, method = "Sign test", note = "All values equal to hypothesized value"))
  }
  
  # Test statistic is number of positive differences
  statistic <- positive
  
  # Calculate p-value based on alternative hypothesis
  if (alternative == "two.sided") {
    p_value <- 2 * min(pbinom(statistic, n, 0.5), 1 - pbinom(statistic - 1, n, 0.5))
  } else if (alternative == "greater") {
    p_value <- 1 - pbinom(statistic - 1, n, 0.5)
  } else if (alternative == "less") {
    p_value <- pbinom(statistic, n, 0.5)
  }
  
  list(
    statistic = statistic,
    parameter = n,
    p.value = p_value,
    method = "Sign test",
    positive = positive,
    negative = negative,
    ties = ties
  )
}

#' Perform Permutation Test
perform_permutation_test <- function(group1, group2, alternative, n_permutations = 1000) {
  
  if (length(group1) == 0 || length(group2) == 0) {
    return(list(p.value = NA, method = "Permutation test"))
  }
  
  # Observed test statistic (difference in means)
  observed_diff <- mean(group1) - mean(group2)
  
  # Combined data
  combined_data <- c(group1, group2)
  n1 <- length(group1)
  n_total <- length(combined_data)
  
  # Permutation distribution
  permuted_diffs <- replicate(n_permutations, {
    # Randomly reassign group labels
    permuted_indices <- sample(n_total, n1)
    perm_group1 <- combined_data[permuted_indices]
    perm_group2 <- combined_data[-permuted_indices]
    
    mean(perm_group1) - mean(perm_group2)
  })
  
  # Calculate p-value
  if (alternative == "two.sided") {
    p_value <- mean(abs(permuted_diffs) >= abs(observed_diff))
  } else if (alternative == "greater") {
    p_value <- mean(permuted_diffs >= observed_diff)
  } else if (alternative == "less") {
    p_value <- mean(permuted_diffs <= observed_diff)
  }
  
  list(
    statistic = observed_diff,
    p.value = p_value,
    method = paste("Permutation test (", n_permutations, " permutations)"),
    permutation_distribution = permuted_diffs
  )
}

# =============================================================================
# EFFECT SIZE CALCULATIONS
# =============================================================================

#' Calculate Effect Sizes
calculate_effect_sizes <- function(data, response_var, predictor_var, group_var, test_scenario, test_results) {
  
  effect_sizes <- list()
  
  if (test_scenario$scenario == "two_sample") {
    effect_sizes <- calculate_two_sample_effect_sizes(data, response_var, group_var)
    
  } else if (test_scenario$scenario == "multi_sample") {
    effect_sizes <- calculate_multi_sample_effect_sizes(data, response_var, group_var)
    
  } else if (test_scenario$scenario == "correlation") {
    effect_sizes <- calculate_correlation_effect_sizes(data, response_var, predictor_var)
    
  } else if (test_scenario$scenario == "independence") {
    effect_sizes <- calculate_independence_effect_sizes(data, response_var, predictor_var)
  }
  
  return(effect_sizes)
}

# Two-sample effect sizes
calculate_two_sample_effect_sizes <- function(data, response_var, group_var) {
  
  clean_data <- data[!is.na(data[[response_var]]) & !is.na(data[[group_var]]), ]
  groups <- unique(clean_data[[group_var]])
  
  if (length(groups) != 2) {
    return(list(error = "Two groups required"))
  }
  
  group1_data <- clean_data[clean_data[[group_var]] == groups[1], response_var]
  group2_data <- clean_data[clean_data[[group_var]] == groups[2], response_var]
  
  # Cohen's d
  mean1 <- mean(group1_data, na.rm = TRUE)
  mean2 <- mean(group2_data, na.rm = TRUE)
  sd1 <- sd(group1_data, na.rm = TRUE)
  sd2 <- sd(group2_data, na.rm = TRUE)
  n1 <- length(group1_data)
  n2 <- length(group2_data)
  
  # Pooled standard deviation
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  cohens_d <- (mean1 - mean2) / pooled_sd
  
  # Hedge's g (bias-corrected Cohen's d)
  correction_factor <- 1 - (3 / (4 * (n1 + n2) - 9))
  hedges_g <- cohens_d * correction_factor
  
  # Common Language Effect Size
  cles <- pnorm(cohens_d / sqrt(2))
  
  # Interpret effect sizes
  d_interpretation <- interpret_cohens_d(abs(cohens_d))
  
  list(
    cohens_d = cohens_d,
    hedges_g = hedges_g,
    common_language_effect_size = cles,
    interpretation = d_interpretation,
    group1_stats = list(mean = mean1, sd = sd1, n = n1),
    group2_stats = list(mean = mean2, sd = sd2, n = n2)
  )
}

# Multi-sample effect sizes
calculate_multi_sample_effect_sizes <- function(data, response_var, group_var) {
  
  clean_data <- data[!is.na(data[[response_var]]) & !is.na(data[[group_var]]), ]
  
  # Calculate eta-squared and omega-squared
  aov_result <- aov(as.formula(paste(response_var, "~", group_var)), data = clean_data)
  aov_summary <- summary(aov_result)
  
  ss_between <- aov_summary[[1]][["Sum Sq"]][1]
  ss_within <- aov_summary[[1]][["Sum Sq"]][2]
  ss_total <- ss_between + ss_within
  ms_within <- aov_summary[[1]][["Mean Sq"]][2]
  df_between <- aov_summary[[1]][["Df"]][1]
  
  # Eta-squared (proportion of variance explained)
  eta_squared <- ss_between / ss_total
  
  # Partial eta-squared
  partial_eta_squared <- ss_between / (ss_between + ss_within)
  
  # Omega-squared (less biased estimate)
  omega_squared <- (ss_between - df_between * ms_within) / (ss_total + ms_within)
  
  # Interpret effect size
  eta_interpretation <- interpret_eta_squared(eta_squared)
  
  list(
    eta_squared = eta_squared,
    partial_eta_squared = partial_eta_squared,
    omega_squared = omega_squared,
    interpretation = eta_interpretation,
    f_statistic = aov_summary[[1]][["F value"]][1]
  )
}

# Correlation effect sizes
calculate_correlation_effect_sizes <- function(data, response_var, predictor_var) {
  
  x <- data[[predictor_var]]
  y <- data[[response_var]]
  
  complete_cases <- !is.na(x) & !is.na(y)
  x <- x[complete_cases]
  y <- y[complete_cases]
  
  if (length(x) < 3) {
    return(list(error = "Insufficient data"))
  }
  
  # Pearson correlation
  r <- cor(x, y, method = "pearson")
  
  # R-squared (coefficient of determination)
  r_squared <- r^2
  
  # Cohen's conventions for correlation
  r_interpretation <- interpret_correlation(abs(r))
  
  list(
    pearson_r = r,
    r_squared = r_squared,
    interpretation = r_interpretation
  )
}

# Independence effect sizes
calculate_independence_effect_sizes <- function(data, response_var, predictor_var) {
  
  contingency_table <- table(data[[response_var]], data[[predictor_var]])
  
  if (any(dim(contingency_table) == 0)) {
    return(list(error = "Cannot create contingency table"))
  }
  
  n <- sum(contingency_table)
  chi_square_stat <- chisq.test(contingency_table)$statistic
  
  # Phi coefficient (for 2x2 tables)
  if (all(dim(contingency_table) == 2)) {
    phi <- sqrt(chi_square_stat / n)
  } else {
    phi <- NA
  }
  
  # Cramér's V
  min_dim <- min(dim(contingency_table))
  cramers_v <- sqrt(chi_square_stat / (n * (min_dim - 1)))
  
  # Contingency coefficient
  contingency_coeff <- sqrt(chi_square_stat / (chi_square_stat + n))
  
  # Interpret effect sizes
  cramers_interpretation <- interpret_cramers_v(cramers_v, min_dim)
  
  list(
    phi = phi,
    cramers_v = cramers_v,
    contingency_coefficient = contingency_coeff,
    interpretation = cramers_interpretation
  )
}

# Effect size interpretation functions
interpret_cohens_d <- function(d) {
  if (d < 0.2) "negligible"
  else if (d < 0.5) "small"
  else if (d < 0.8) "medium"
  else "large"
}

interpret_eta_squared <- function(eta) {
  if (eta < 0.01) "negligible"
  else if (eta < 0.06) "small"
  else if (eta < 0.14) "medium"
  else "large"
}

interpret_correlation <- function(r) {
  if (r < 0.1) "negligible"
  else if (r < 0.3) "small"
  else if (r < 0.5) "medium"
  else if (r < 0.7) "large"
  else "very large"
}

interpret_cramers_v <- function(v, df) {
  if (df == 2) {
    if (v < 0.1) "negligible"
    else if (v < 0.3) "small"
    else if (v < 0.5) "medium"
    else "large"
  } else {
    if (v < 0.07) "negligible"
    else if (v < 0.21) "small"
    else if (v < 0.35) "medium"
    else "large"
  }
}

# =============================================================================
# POWER ANALYSIS
# =============================================================================

#' Perform Power Analysis
perform_power_analysis <- function(data, response_var, predictor_var, group_var, test_scenario, effect_sizes, alpha) {
  
  power_results <- list()
  
  tryCatch({
    if (test_scenario$scenario == "two_sample" && !is.null(effect_sizes$cohens_d)) {
      power_results <- perform_two_sample_power_analysis(data, response_var, group_var, effect_sizes, alpha)
      
    } else if (test_scenario$scenario == "multi_sample" && !is.null(effect_sizes$eta_squared)) {
      power_results <- perform_anova_power_analysis(data, response_var, group_var, effect_sizes, alpha)
      
    } else if (test_scenario$scenario == "correlation" && !is.null(effect_sizes$pearson_r)) {
      power_results <- perform_correlation_power_analysis(data, response_var, predictor_var, effect_sizes, alpha)
    }
  }, error = function(e) {
    power_results$error <- paste("Power analysis failed:", e$message)
  })
  
  return(power_results)
}

# Two-sample power analysis
perform_two_sample_power_analysis <- function(data, response_var, group_var, effect_sizes, alpha) {
  
  clean_data <- data[!is.na(data[[response_var]]) & !is.na(data[[group_var]]), ]
  groups <- unique(clean_data[[group_var]])
  
  n1 <- sum(clean_data[[group_var]] == groups[1])
  n2 <- sum(clean_data[[group_var]] == groups[2])
  
  # Current power
  current_power <- pwr::pwr.t2n.test(
    n1 = n1,
    n2 = n2,
    d = abs(effect_sizes$cohens_d),
    sig.level = alpha
  )$power
  
  # Sample size for 80% power
  sample_size_80 <- pwr::pwr.t.test(
    d = abs(effect_sizes$cohens_d),
    sig.level = alpha,
    power = 0.80,
    type = "two.sample"
  )$n
  
  list(
    current_power = current_power,
    sample_size_for_80_power = ceiling(sample_size_80),
    current_sample_sizes = c(n1 = n1, n2 = n2),
    effect_size_used = abs(effect_sizes$cohens_d)
  )
}

# ANOVA power analysis
perform_anova_power_analysis <- function(data, response_var, group_var, effect_sizes, alpha) {
  
  clean_data <- data[!is.na(data[[response_var]]) & !is.na(data[[group_var]]), ]
  k <- length(unique(clean_data[[group_var]]))
  n_total <- nrow(clean_data)
  
  # Convert eta-squared to Cohen's f
  f <- sqrt(effect_sizes$eta_squared / (1 - effect_sizes$eta_squared))
  
  # Current power
  current_power <- pwr::pwr.anova.test(
    k = k,
    n = n_total / k,
    f = f,
    sig.level = alpha
  )$power
  
  # Sample size for 80% power
  sample_size_80 <- pwr::pwr.anova.test(
    k = k,
    f = f,
    sig.level = alpha,
    power = 0.80
  )$n
  
  list(
    current_power = current_power,
    sample_size_per_group_80_power = ceiling(sample_size_80),
    total_sample_size_80_power = ceiling(sample_size_80 * k),
    current_sample_size = n_total,
    groups = k,
    cohens_f = f
  )
}

# Correlation power analysis
perform_correlation_power_analysis <- function(data, response_var, predictor_var, effect_sizes, alpha) {
  
  x <- data[[predictor_var]]
  y <- data[[response_var]]
  
  complete_cases <- !is.na(x) & !is.na(y)
  n <- sum(complete_cases)
  
  # Current power
  current_power <- pwr::pwr.r.test(
    n = n,
    r = abs(effect_sizes$pearson_r),
    sig.level = alpha
  )$power
  
  # Sample size for 80% power
  sample_size_80 <- pwr::pwr.r.test(
    r = abs(effect_sizes$pearson_r),
    sig.level = alpha,
    power = 0.80
  )$n
  
  list(
    current_power = current_power,
    sample_size_for_80_power = ceiling(sample_size_80),
    current_sample_size = n,
    correlation_used = abs(effect_sizes$pearson_r)
  )
}

# =============================================================================
# RECOMMENDATIONS AND SUMMARY
# =============================================================================

#' Generate Test Recommendations
generate_test_recommendations <- function(results, test_scenario, test_type) {
  
  recommendations <- character()
  
  # Assumption-based recommendations
  if (!is.null(results$assumptions$normality)) {
    normality_issues <- any(sapply(results$assumptions$normality, function(x) {
      !is.null(x$summary) && x$summary$normality_assumption == "Violated"
    }))
    
    if (normality_issues) {
      recommendations <- c(recommendations, 
                          "Consider non-parametric alternatives due to normality violations")
    }
  }
  
  if (!is.null(results$assumptions$homogeneity) && 
      !is.null(results$assumptions$homogeneity$summary) &&
      results$assumptions$homogeneity$summary$homogeneity_assumption == "Violated") {
    
    recommendations <- c(recommendations,
                         "Use Welch's t-test or non-parametric tests due to unequal variances")
  }
  
  # Power-based recommendations
  if (!is.null(results$power_analysis$current_power) && 
      results$power_analysis$current_power < 0.8) {
    
    recommendations <- c(recommendations,
                         paste("Current power is low (", 
                              round(results$power_analysis$current_power * 100, 1), 
                              "%). Consider increasing sample size"))
  }
  
  # Effect size recommendations
  if (!is.null(results$effect_sizes)) {
    if (test_scenario$scenario == "two_sample" && !is.null(results$effect_sizes$interpretation)) {
      if (results$effect_sizes$interpretation == "negligible") {
        recommendations <- c(recommendations,
                             "Effect size is negligible - consider practical significance")
      }
    }
  }
  
  # Multiple testing recommendations
  if (test_scenario$scenario == "multi_sample") {
    recommendations <- c(recommendations,
                         "Apply multiple comparison corrections for post-hoc tests")
  }
  
  if (length(recommendations) == 0) {
    recommendations <- "No specific recommendations. Results appear robust."
  }
  
  return(recommendations)
}

#' Generate Test Summary
generate_test_summary <- function(results, alpha) {
  
  summary_text <- character()
  
  # Header
  summary_text <- c(summary_text, "HYPOTHESIS TESTING SUMMARY")
  summary_text <- c(summary_text, paste(rep("=", 30), collapse = ""))
  
  # Test scenario
  summary_text <- c(summary_text, paste("Test Scenario:", results$metadata$test_scenario))
  summary_text <- c(summary_text, paste("Sample Size:", results$metadata$sample_size))
  summary_text <- c(summary_text, paste("Significance Level:", alpha))
  summary_text <- c(summary_text, "")
  
  # Key results
  if (!is.null(results$tests)) {
    summary_text <- c(summary_text, "KEY RESULTS:")
    
    # Extract p-values from different tests
    p_values <- list()
    
    for (test_name in names(results$tests)) {
      test_result <- results$tests[[test_name]]
      if (!is.null(test_result$p.value) && !is.na(test_result$p.value)) {
        p_values[[test_name]] <- test_result$p.value
      }
    }
    
    if (length(p_values) > 0) {
      for (test_name in names(p_values)) {
        p_val <- p_values[[test_name]]
        significance <- ifelse(p_val < alpha, "Significant", "Not Significant")
        summary_text <- c(summary_text, 
                          paste("  ", test_name, ": p =", round(p_val, 4), "(", significance, ")"))
      }
    }
  }
  
  summary_text <- c(summary_text, "")
  
  # Effect sizes
  if (!is.null(results$effect_sizes) && length(results$effect_sizes) > 0) {
    summary_text <- c(summary_text, "EFFECT SIZES:")
    
    if (!is.null(results$effect_sizes$cohens_d)) {
      summary_text <- c(summary_text, 
                        paste("  Cohen's d:", round(results$effect_sizes$cohens_d, 3),
                              "(", results$effect_sizes$interpretation, ")"))
    }
    
    if (!is.null(results$effect_sizes$eta_squared)) {
      summary_text <- c(summary_text,
                        paste("  Eta-squared:", round(results$effect_sizes$eta_squared, 3),
                              "(", results$effect_sizes$interpretation, ")"))
    }
    
    if (!is.null(results$effect_sizes$pearson_r)) {
      summary_text <- c(summary_text,
                        paste("  Pearson r:", round(results$effect_sizes$pearson_r, 3),
                              "(", results$effect_sizes$interpretation, ")"))
    }
    
    summary_text <- c(summary_text, "")
  }
  
  # Recommendations
  if (length(results$recommendations) > 0) {
    summary_text <- c(summary_text, "RECOMMENDATIONS:")
    for (rec in results$recommendations) {
      summary_text <- c(summary_text, paste("  -", rec))
    }
  }
  
  return(summary_text)
}

# =============================================================================
# PRINT METHOD
# =============================================================================

#' Print Method for Hypothesis Test Results
#' @param x Hypothesis test results object
#' @param ... Additional arguments
#' @export
print.hypothesis_test_results <- function(x, ...) {
  
  cat(paste(x$summary, collapse = "\n"))
  cat("\n\n")
  
  cat("Available components:\n")
  cat("- $tests: Statistical test results\n")
  cat("- $assumptions: Assumption checking results\n")
  cat("- $effect_sizes: Effect size calculations\n")
  cat("- $power_analysis: Power analysis results\n")
  cat("- $recommendations: Statistical recommendations\n")
  cat("- $metadata: Analysis metadata\n")
}

# =============================================================================
# DEMONSTRATION FUNCTION
# =============================================================================

#' Run Comprehensive Hypothesis Testing Demonstration
#' @param verbose Logical, whether to show detailed output
#' @return Invisible list of demonstration results
#' @export
run_hypothesis_testing_demo <- function(verbose = TRUE) {
  
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("    COMPREHENSIVE HYPOTHESIS TESTING PORTFOLIO\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }
  
  # Generate demonstration data
  set.seed(123)
  demo_data <- data.frame(
    participant_id = 1:150,
    treatment_group = rep(c("Control", "Treatment_A", "Treatment_B"), each = 50),
    pre_test_score = rnorm(150, 50, 10),
    post_test_score = c(
      rnorm(50, 52, 10),    # Control (small improvement)
      rnorm(50, 58, 10),    # Treatment A (medium improvement)
      rnorm(50, 62, 12)     # Treatment B (large improvement)
    ),
    age = sample(18:65, 150, replace = TRUE),
    gender = sample(c("Male", "Female"), 150, replace = TRUE),
    satisfaction = sample(1:5, 150, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.2, 0.1)),
    stringsAsFactors = FALSE
  )
  
  demo_data$improvement_score <- demo_data$post_test_score - demo_data$pre_test_score
  
  results <- list()
  
  # Two-sample test
  if (verbose) cat("1. Two-sample comparison (Control vs Treatment_A)...\n")
  two_group_data <- demo_data[demo_data$treatment_group %in% c("Control", "Treatment_A"), ]
  results$two_sample <- comprehensive_hypothesis_test(
    data = two_group_data,
    response_var = "improvement_score",
    group_var = "treatment_group",
    test_type = "auto"
  )
  
  # Multi-sample test (ANOVA)
  if (verbose) cat("2. Multi-sample comparison (all treatment groups)...\n")
  results$multi_sample <- comprehensive_hypothesis_test(
    data = demo_data,
    response_var = "improvement_score",
    group_var = "treatment_group",
    test_type = "auto"
  )
  
  # Correlation test
  if (verbose) cat("3. Correlation analysis (age vs improvement)...\n")
  results$correlation <- comprehensive_hypothesis_test(
    data = demo_data,
    response_var = "improvement_score",
    predictor_var = "age",
    test_type = "auto"
  )
  
  # Independence test
  if (verbose) cat("4. Independence test (gender vs satisfaction)...\n")
  results$independence <- comprehensive_hypothesis_test(
    data = demo_data,
    response_var = "gender",
    predictor_var = "satisfaction",
    test_type = "auto"
  )
  
  # Print results if verbose
  if (verbose) {
    cat("\n5. Results Summary:\n")
    cat(paste(rep("=", 30), collapse = ""), "\n")
    
    cat("\nTwo-Sample Test Results:\n")
    print(results$two_sample)
    
    cat("\nMulti-Sample Test Results:\n")
    print(results$multi_sample)
    
    cat("\nCorrelation Test Results:\n")
    print(results$correlation)
    
    cat("\nIndependence Test Results:\n")
    print(results$independence)
    
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("    HYPOTHESIS TESTING DEMONSTRATION COMPLETE\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }
  
  invisible(results)
}

# =============================================================================
# EXAMPLE USAGE
# =============================================================================

if (interactive()) {
  # Run the comprehensive demonstration
  demo_results <- run_hypothesis_testing_demo(verbose = TRUE)
  
  # Access specific results
  # demo_results$two_sample$tests$welch_t
  # demo_results$multi_sample$effect_sizes
  # demo_results$correlation$power_analysis
}
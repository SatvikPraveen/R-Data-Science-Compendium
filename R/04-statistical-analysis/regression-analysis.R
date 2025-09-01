# @title Comprehensive Regression Analysis Framework
# @description Advanced regression modeling, diagnostics, and interpretation
# @author R Data Science Portfolio Developer
# @date 2025

#' ========================================
#' COMPREHENSIVE REGRESSION ANALYSIS
#' ========================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, ggplot2, broom, car, MASS, glmnet, randomForest,
  corrplot, plotly, GGally, leaps, caret, ModelMetrics,
  performance, see, parameters, insight
)

#' ========================================
#' 1. LINEAR REGRESSION ANALYSIS
#' ========================================

#' Comprehensive Linear Regression Function
#' @param data Dataset for analysis
#' @param formula Model formula
#' @param validation_split Proportion for validation (default 0.2)
#' @return List with model results, diagnostics, and predictions
perform_linear_regression <- function(data, formula, validation_split = 0.2) {
  
  # Data splitting
  set.seed(42)
  train_indices <- sample(1:nrow(data), size = floor((1 - validation_split) * nrow(data)))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # Fit the model
  model <- lm(formula, data = train_data)
  
  # Model summary and diagnostics
  model_summary <- summary(model)
  model_metrics <- glance(model)
  coefficients <- tidy(model, conf.int = TRUE)
  
  # Predictions
  train_pred <- predict(model, train_data)
  test_pred <- predict(model, test_data, interval = "prediction")
  
  # Performance metrics
  train_rmse <- sqrt(mean((train_data[[all.vars(formula)[1]]] - train_pred)^2))
  test_rmse <- sqrt(mean((test_data[[all.vars(formula)[1]]] - test_pred[,1])^2))
  
  # Residual analysis
  residuals_df <- data.frame(
    fitted = fitted(model),
    residuals = residuals(model),
    standardized = rstandard(model),
    studentized = rstudent(model)
  )
  
  return(list(
    model = model,
    summary = model_summary,
    metrics = model_metrics,
    coefficients = coefficients,
    train_rmse = train_rmse,
    test_rmse = test_rmse,
    residuals = residuals_df,
    train_data = train_data,
    test_data = test_data,
    predictions = list(train = train_pred, test = test_pred)
  ))
}

#' Advanced Regression Diagnostics
#' @param regression_result Result from perform_linear_regression
#' @return List of diagnostic plots and tests
regression_diagnostics <- function(regression_result) {
  
  model <- regression_result$model
  residuals_df <- regression_result$residuals
  
  # 1. Residuals vs Fitted
  p1 <- ggplot(residuals_df, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Residuals vs Fitted Values",
         x = "Fitted Values", y = "Residuals") +
    theme_minimal()
  
  # 2. Q-Q Plot
  p2 <- ggplot(residuals_df, aes(sample = standardized)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "Normal Q-Q Plot",
         x = "Theoretical Quantiles", y = "Standardized Residuals") +
    theme_minimal()
  
  # 3. Scale-Location Plot
  p3 <- ggplot(residuals_df, aes(x = fitted, y = sqrt(abs(standardized)))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    labs(title = "Scale-Location Plot",
         x = "Fitted Values", y = "√|Standardized Residuals|") +
    theme_minimal()
  
  # 4. Cook's Distance
  cooks_d <- cooks.distance(model)
  p4 <- ggplot(data.frame(obs = 1:length(cooks_d), cooks = cooks_d), 
               aes(x = obs, y = cooks)) +
    geom_col(alpha = 0.7) +
    geom_hline(yintercept = 4/length(cooks_d), color = "red", linetype = "dashed") +
    labs(title = "Cook's Distance", x = "Observation", y = "Cook's Distance") +
    theme_minimal()
  
  # Statistical tests
  shapiro_test <- shapiro.test(residuals(model))
  bp_test <- bptest(model)
  dw_test <- durbinWatsonTest(model)
  
  return(list(
    plots = list(
      residuals_vs_fitted = p1,
      qq_plot = p2,
      scale_location = p3,
      cooks_distance = p4
    ),
    tests = list(
      normality = shapiro_test,
      homoscedasticity = bp_test,
      autocorrelation = dw_test
    )
  ))
}

#' ========================================
#' 2. MULTIPLE REGRESSION WITH VARIABLE SELECTION
#' ========================================

#' Best Subset Selection
#' @param data Dataset
#' @param response Response variable name
#' @param predictors Vector of predictor names
#' @return Best subset selection results
best_subset_selection <- function(data, response, predictors) {
  
  # Prepare formula
  formula_str <- paste(response, "~", paste(predictors, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Best subset selection
  subset_models <- regsubsets(formula_obj, data = data, nvmax = length(predictors))
  subset_summary <- summary(subset_models)
  
  # Extract metrics
  metrics_df <- data.frame(
    n_vars = 1:length(subset_summary$cp),
    cp = subset_summary$cp,
    bic = subset_summary$bic,
    adjr2 = subset_summary$adjr2,
    rss = subset_summary$rss
  )
  
  # Find optimal models
  best_cp <- which.min(subset_summary$cp)
  best_bic <- which.min(subset_summary$bic)
  best_adjr2 <- which.max(subset_summary$adjr2)
  
  # Plot selection criteria
  p1 <- ggplot(metrics_df, aes(x = n_vars, y = cp)) +
    geom_line() + geom_point() +
    geom_vline(xintercept = best_cp, color = "red", linetype = "dashed") +
    labs(title = "Cp Criterion", x = "Number of Variables", y = "Cp") +
    theme_minimal()
  
  p2 <- ggplot(metrics_df, aes(x = n_vars, y = bic)) +
    geom_line() + geom_point() +
    geom_vline(xintercept = best_bic, color = "red", linetype = "dashed") +
    labs(title = "BIC Criterion", x = "Number of Variables", y = "BIC") +
    theme_minimal()
  
  p3 <- ggplot(metrics_df, aes(x = n_vars, y = adjr2)) +
    geom_line() + geom_point() +
    geom_vline(xintercept = best_adjr2, color = "red", linetype = "dashed") +
    labs(title = "Adjusted R²", x = "Number of Variables", y = "Adjusted R²") +
    theme_minimal()
  
  return(list(
    models = subset_models,
    summary = subset_summary,
    metrics = metrics_df,
    best_models = list(cp = best_cp, bic = best_bic, adjr2 = best_adjr2),
    plots = list(cp = p1, bic = p2, adjr2 = p3)
  ))
}

#' ========================================
#' 3. REGULARIZED REGRESSION (RIDGE/LASSO/ELASTIC NET)
#' ========================================

#' Regularized Regression Analysis
#' @param data Dataset
#' @param response Response variable name
#' @param predictors Vector of predictor names
#' @param alpha Alpha parameter (0=Ridge, 1=Lasso, 0.5=Elastic Net)
#' @return Regularized regression results
regularized_regression <- function(data, response, predictors, alpha = 1) {
  
  # Prepare data
  x <- as.matrix(data[, predictors])
  y <- data[[response]]
  
  # Cross-validation for lambda
  cv_model <- cv.glmnet(x, y, alpha = alpha, nfolds = 10)
  
  # Fit models with optimal lambda
  best_lambda <- cv_model$lambda.min
  lambda_1se <- cv_model$lambda.1se
  
  model_min <- glmnet(x, y, alpha = alpha, lambda = best_lambda)
  model_1se <- glmnet(x, y, alpha = alpha, lambda = lambda_1se)
  
  # Extract coefficients
  coef_min <- coef(model_min)[,1]
  coef_1se <- coef(model_1se)[,1]
  
  # Variable importance (non-zero coefficients)
  important_vars_min <- names(coef_min[coef_min != 0])[-1]  # Remove intercept
  important_vars_1se <- names(coef_1se[coef_1se != 0])[-1]
  
  # Create coefficient plot
  coef_df <- data.frame(
    variable = names(coef_min)[-1],
    lambda_min = coef_min[-1],
    lambda_1se = coef_1se[-1]
  ) %>%
    tidyr::pivot_longer(cols = c(lambda_min, lambda_1se), 
                       names_to = "model", values_to = "coefficient") %>%
    filter(coefficient != 0)
  
  coef_plot <- ggplot(coef_df, aes(x = reorder(variable, abs(coefficient)), 
                                   y = coefficient, fill = model)) +
    geom_col(position = "dodge", alpha = 0.8) +
    coord_flip() +
    labs(title = paste("Regularized Regression Coefficients (α =", alpha, ")"),
         x = "Variables", y = "Coefficient Value") +
    theme_minimal()
  
  return(list(
    cv_model = cv_model,
    model_min = model_min,
    model_1se = model_1se,
    best_lambda = best_lambda,
    lambda_1se = lambda_1se,
    coefficients = list(min = coef_min, se = coef_1se),
    important_vars = list(min = important_vars_min, se = important_vars_1se),
    coef_plot = coef_plot
  ))
}

#' ========================================
#' 4. LOGISTIC REGRESSION
#' ========================================

#' Comprehensive Logistic Regression
#' @param data Dataset
#' @param formula Model formula
#' @param validation_split Proportion for validation
#' @return Logistic regression results
perform_logistic_regression <- function(data, formula, validation_split = 0.2) {
  
  # Data splitting
  set.seed(42)
  train_indices <- sample(1:nrow(data), size = floor((1 - validation_split) * nrow(data)))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # Fit the model
  model <- glm(formula, data = train_data, family = binomial)
  
  # Model summary
  model_summary <- summary(model)
  coefficients <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
  
  # Predictions
  train_probs <- predict(model, train_data, type = "response")
  test_probs <- predict(model, test_data, type = "response")
  
  train_pred <- ifelse(train_probs > 0.5, 1, 0)
  test_pred <- ifelse(test_probs > 0.5, 1, 0)
  
  # Response variable
  response_var <- all.vars(formula)[1]
  
  # Performance metrics
  train_accuracy <- mean(train_pred == train_data[[response_var]])
  test_accuracy <- mean(test_pred == test_data[[response_var]])
  
  # Confusion matrices
  train_cm <- table(Predicted = train_pred, Actual = train_data[[response_var]])
  test_cm <- table(Predicted = test_pred, Actual = test_data[[response_var]])
  
  # ROC analysis
  train_auc <- ModelMetrics::auc(train_data[[response_var]], train_probs)
  test_auc <- ModelMetrics::auc(test_data[[response_var]], test_probs)
  
  return(list(
    model = model,
    summary = model_summary,
    coefficients = coefficients,
    performance = list(
      train_accuracy = train_accuracy,
      test_accuracy = test_accuracy,
      train_auc = train_auc,
      test_auc = test_auc
    ),
    confusion_matrices = list(train = train_cm, test = test_cm),
    predictions = list(
      train_probs = train_probs,
      test_probs = test_probs,
      train_pred = train_pred,
      test_pred = test_pred
    )
  ))
}

#' ========================================
#' 5. POLYNOMIAL AND INTERACTION REGRESSION
#' ========================================

#' Polynomial Regression Analysis
#' @param data Dataset
#' @param x_var Predictor variable
#' @param y_var Response variable
#' @param max_degree Maximum polynomial degree
#' @return Polynomial regression results
polynomial_regression <- function(data, x_var, y_var, max_degree = 5) {
  
  results <- list()
  
  for (degree in 1:max_degree) {
    # Create polynomial terms
    poly_formula <- as.formula(paste(y_var, "~ poly(", x_var, ",", degree, ")"))
    
    # Fit model
    model <- lm(poly_formula, data = data)
    
    # Store results
    results[[paste0("degree_", degree)]] <- list(
      model = model,
      aic = AIC(model),
      bic = BIC(model),
      rsquared = summary(model)$r.squared,
      adj_rsquared = summary(model)$adj.r.squared
    )
  }
  
  # Extract model comparison metrics
  comparison_df <- data.frame(
    degree = 1:max_degree,
    aic = sapply(results, function(x) x$aic),
    bic = sapply(results, function(x) x$bic),
    rsquared = sapply(results, function(x) x$rsquared),
    adj_rsquared = sapply(results, function(x) x$adj_rsquared)
  )
  
  # Find optimal degree
  best_aic <- which.min(comparison_df$aic)
  best_bic <- which.min(comparison_df$bic)
  best_adj_r2 <- which.max(comparison_df$adj_rsquared)
  
  # Create comparison plot
  comparison_plot <- comparison_df %>%
    tidyr::pivot_longer(cols = c(aic, bic, adj_rsquared), 
                       names_to = "metric", values_to = "value") %>%
    ggplot(aes(x = degree, y = value, color = metric)) +
    geom_line(size = 1) + geom_point(size = 2) +
    facet_wrap(~metric, scales = "free_y") +
    labs(title = "Polynomial Degree Selection",
         x = "Polynomial Degree", y = "Metric Value") +
    theme_minimal()
  
  return(list(
    models = results,
    comparison = comparison_df,
    best_degrees = list(aic = best_aic, bic = best_bic, adj_r2 = best_adj_r2),
    comparison_plot = comparison_plot
  ))
}

#' ========================================
#' 6. DEMONSTRATION WITH SYNTHETIC DATA
#' ========================================

# Generate comprehensive regression demonstration data
generate_regression_demo_data <- function(n = 1000) {
  set.seed(42)
  
  # Continuous predictors
  x1 <- rnorm(n, 50, 15)  # Age-like variable
  x2 <- rnorm(n, 75, 20)  # Income-like variable (in thousands)
  x3 <- runif(n, 0, 10)   # Experience-like variable
  
  # Categorical predictors
  category <- sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.4, 0.35, 0.25))
  region <- sample(c("North", "South", "East", "West"), n, replace = TRUE)
  
  # Create interactions and non-linear relationships
  # Linear response
  y_linear <- 2 + 0.5*x1 + 0.3*x2 + 1.2*x3 + 
              ifelse(category == "B", 5, ifelse(category == "C", -3, 0)) +
              rnorm(n, 0, 5)
  
  # Non-linear response
  y_nonlinear <- 10 + 0.1*x1^2 + 0.02*x2*x3 + sin(x3) + 
                 ifelse(category == "B", 8, ifelse(category == "C", -5, 0)) +
                 rnorm(n, 0, 8)
  
  # Binary response for logistic regression
  prob_binary <- plogis(-2 + 0.05*x1 + 0.02*x2 - 0.3*x3 + 
                       ifelse(category == "B", 1, ifelse(category == "C", -0.5, 0)))
  y_binary <- rbinom(n, 1, prob_binary)
  
  return(data.frame(
    x1 = x1, x2 = x2, x3 = x3,
    category = category, region = region,
    y_linear = y_linear,
    y_nonlinear = y_nonlinear,
    y_binary = y_binary
  ))
}

#' Run Complete Regression Analysis Demo
demo_regression_analysis <- function() {
  
  cat("=== COMPREHENSIVE REGRESSION ANALYSIS DEMONSTRATION ===\n\n")
  
  # Generate demo data
  demo_data <- generate_regression_demo_data()
  
  cat("1. LINEAR REGRESSION ANALYSIS\n")
  cat("="*50, "\n")
  
  # Linear regression
  linear_results <- perform_linear_regression(
    demo_data, 
    y_linear ~ x1 + x2 + x3 + category + region
  )
  
  cat("Model Summary:\n")
  print(linear_results$summary)
  
  cat("\nModel Performance:\n")
  cat("Training RMSE:", round(linear_results$train_rmse, 3), "\n")
  cat("Testing RMSE:", round(linear_results$test_rmse, 3), "\n")
  
  # Diagnostics
  diagnostics <- regression_diagnostics(linear_results)
  
  cat("\n2. VARIABLE SELECTION\n")
  cat("="*50, "\n")
  
  # Best subset selection
  subset_results <- best_subset_selection(
    demo_data, "y_linear", c("x1", "x2", "x3")
  )
  
  cat("Best model sizes:\n")
  print(subset_results$best_models)
  
  cat("\n3. REGULARIZED REGRESSION\n")
  cat("="*50, "\n")
  
  # Lasso regression
  lasso_results <- regularized_regression(
    demo_data, "y_linear", c("x1", "x2", "x3"), alpha = 1
  )
  
  cat("LASSO Results:\n")
  cat("Best lambda:", lasso_results$best_lambda, "\n")
  cat("Selected variables:", paste(lasso_results$important_vars$min, collapse = ", "), "\n")
  
  cat("\n4. LOGISTIC REGRESSION\n")
  cat("="*50, "\n")
  
  # Logistic regression
  logistic_results <- perform_logistic_regression(
    demo_data,
    y_binary ~ x1 + x2 + x3 + category
  )
  
  cat("Logistic Regression Performance:\n")
  cat("Training Accuracy:", round(logistic_results$performance$train_accuracy, 3), "\n")
  cat("Testing Accuracy:", round(logistic_results$performance$test_accuracy, 3), "\n")
  cat("Training AUC:", round(logistic_results$performance$train_auc, 3), "\n")
  cat("Testing AUC:", round(logistic_results$performance$test_auc, 3), "\n")
  
  cat("\n5. POLYNOMIAL REGRESSION\n")
  cat("="*50, "\n")
  
  # Polynomial regression
  poly_results <- polynomial_regression(demo_data, "x1", "y_nonlinear", max_degree = 5)
  
  cat("Optimal polynomial degrees:\n")
  print(poly_results$best_degrees)
  
  cat("\nRegression Analysis Demo Complete!\n")
  
  return(list(
    linear = linear_results,
    diagnostics = diagnostics,
    subset = subset_results,
    lasso = lasso_results,
    logistic = logistic_results,
    polynomial = poly_results
  ))
}

# Export key functions
regression_analysis_exports <- list(
  perform_linear_regression = perform_linear_regression,
  regression_diagnostics = regression_diagnostics,
  best_subset_selection = best_subset_selection,
  regularized_regression = regularized_regression,
  perform_logistic_regression = perform_logistic_regression,
  polynomial_regression = polynomial_regression,
  generate_regression_demo_data = generate_regression_demo_data,
  demo_regression_analysis = demo_regression_analysis
)

# Run demo if script is executed directly
if (sys.nframe() == 0) {
  demo_results <- demo_regression_analysis()
}
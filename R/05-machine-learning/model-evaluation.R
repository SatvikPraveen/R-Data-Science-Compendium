# @title Comprehensive Model Evaluation Framework
# @description Advanced techniques for evaluating machine learning models
# @author R Data Science Portfolio
# @date 2025

#' ========================================
#' COMPREHENSIVE MODEL EVALUATION FRAMEWORK
#' ========================================

# Complete toolkit for evaluating classification and regression models

# Required libraries
library(caret)
library(pROC)
library(ROCR)
library(MLmetrics)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)
library(randomForest)
library(glmnet)
library(xgboost)

#' ========================================
#' 1. CLASSIFICATION METRICS
#' ========================================

#' Calculate Classification Metrics
#' @param actual Vector of actual class labels
#' @param predicted Vector of predicted class labels
#' @param probabilities Matrix of predicted probabilities (optional)
#' @param positive_class Label of positive class for binary classification
#' @return List of classification metrics
calculate_classification_metrics <- function(actual, predicted, probabilities = NULL, positive_class = NULL) {
  
  cat("CLASSIFICATION METRICS CALCULATION\n")
  cat("==================================\n\n")
  
  # Ensure factors have same levels
  if (is.factor(actual) && is.factor(predicted)) {
    all_levels <- union(levels(actual), levels(predicted))
    actual <- factor(actual, levels = all_levels)
    predicted <- factor(predicted, levels = all_levels)
  }
  
  # Confusion matrix
  cm <- confusionMatrix(predicted, actual, positive = positive_class)
  
  # Basic metrics
  metrics <- list(
    confusion_matrix = cm$table,
    accuracy = cm$overall['Accuracy'],
    kappa = cm$overall['Kappa'],
    sensitivity = cm$byClass['Sensitivity'],
    specificity = cm$byClass['Specificity'],
    precision = cm$byClass['Pos Pred Value'],
    recall = cm$byClass['Sensitivity'],
    f1_score = cm$byClass['F1'],
    balanced_accuracy = cm$byClass['Balanced Accuracy']
  )
  
  # Multi-class specific metrics
  if (length(unique(actual)) > 2) {
    metrics$macro_precision <- mean(cm$byClass[, 'Pos Pred Value'], na.rm = TRUE)
    metrics$macro_recall <- mean(cm$byClass[, 'Sensitivity'], na.rm = TRUE)
    metrics$macro_f1 <- mean(cm$byClass[, 'F1'], na.rm = TRUE)
    
    metrics$weighted_precision <- weighted.mean(cm$byClass[, 'Pos Pred Value'], 
                                               table(actual), na.rm = TRUE)
    metrics$weighted_recall <- weighted.mean(cm$byClass[, 'Sensitivity'], 
                                            table(actual), na.rm = TRUE)
    metrics$weighted_f1 <- weighted.mean(cm$byClass[, 'F1'], 
                                        table(actual), na.rm = TRUE)
  }
  
  # ROC/AUC metrics (if probabilities provided)
  if (!is.null(probabilities)) {
    if (length(unique(actual)) == 2) {
      # Binary classification
      if (is.null(positive_class)) {
        positive_class <- levels(factor(actual))[2]
      }
      
      pos_probs <- probabilities[, positive_class]
      roc_obj <- roc(actual, pos_probs, levels = c(levels(factor(actual))[1], positive_class))
      
      metrics$auc <- auc(roc_obj)
      metrics$roc_curve <- roc_obj
      
      # Optimal threshold
      optimal_threshold <- coords(roc_obj, "best", ret = "threshold")
      metrics$optimal_threshold <- optimal_threshold
      
    } else {
      # Multi-class AUC (one-vs-rest)
      multiclass_auc <- MultiLogLoss(actual, probabilities)
      metrics$multiclass_logloss <- multiclass_auc
      
      # One-vs-rest AUC for each class
      ovr_auc <- sapply(levels(factor(actual)), function(class) {
        binary_actual <- ifelse(actual == class, 1, 0)
        binary_probs <- probabilities[, class]
        roc_obj <- roc(binary_actual, binary_probs)
        auc(roc_obj)
      })
      metrics$ovr_auc <- ovr_auc
      metrics$mean_ovr_auc <- mean(ovr_auc)
    }
  }
  
  cat("Classification metrics calculated successfully!\n")
  return(metrics)
}

#' Plot Confusion Matrix
#' @param confusion_matrix Confusion matrix from confusionMatrix
#' @param title Plot title
#' @return ggplot object
plot_confusion_matrix <- function(confusion_matrix, title = "Confusion Matrix") {
  
  # Convert to data frame
  cm_df <- as.data.frame(as.table(confusion_matrix))
  names(cm_df) <- c("Predicted", "Actual", "Frequency")
  
  # Calculate percentages
  cm_df$Percentage <- ave(cm_df$Frequency, cm_df$Actual, FUN = function(x) x/sum(x) * 100)
  
  ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Frequency)) +
    geom_tile() +
    geom_text(aes(label = paste0(Frequency, "\n(", round(Percentage, 1), "%)")), 
              color = "white", size = 4, fontface = "bold") +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(
      title = title,
      x = "Predicted Class",
      y = "Actual Class",
      fill = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#' Plot ROC Curves
#' @param roc_objects List of ROC objects
#' @param model_names Names of models
#' @return ggplot object
plot_roc_curves <- function(roc_objects, model_names = NULL) {
  
  if (is.null(model_names)) {
    model_names <- paste("Model", 1:length(roc_objects))
  }
  
  # Combine ROC data
  roc_data <- data.frame()
  
  for (i in seq_along(roc_objects)) {
    roc_obj <- roc_objects[[i]]
    temp_data <- data.frame(
      fpr = 1 - roc_obj$specificities,
      tpr = roc_obj$sensitivities,
      model = model_names[i],
      auc = round(auc(roc_obj), 3)
    )
    roc_data <- rbind(roc_data, temp_data)
  }
  
  # Create labels with AUC
  roc_data$model_label <- paste0(roc_data$model, " (AUC = ", roc_data$auc, ")")
  
  ggplot(roc_data, aes(x = fpr, y = tpr, color = model_label)) +
    geom_line(size = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    labs(
      title = "ROC Curves Comparison",
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)",
      color = "Model"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1))
}

#' ========================================
#' 2. REGRESSION METRICS
#' ========================================

#' Calculate Regression Metrics
#' @param actual Vector of actual values
#' @param predicted Vector of predicted values
#' @return List of regression metrics
calculate_regression_metrics <- function(actual, predicted) {
  
  cat("REGRESSION METRICS CALCULATION\n")
  cat("==============================\n\n")
  
  # Remove missing values
  complete_cases <- complete.cases(actual, predicted)
  actual <- actual[complete_cases]
  predicted <- predicted[complete_cases]
  
  n <- length(actual)
  
  # Basic metrics
  mae <- mean(abs(actual - predicted))
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  
  # R-squared
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  # Adjusted R-squared (assuming p=1 for simple case)
  adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - 2))
  
  # Mean Absolute Percentage Error
  mape <- mean(abs((actual - predicted) / actual)) * 100
  
  # Symmetric Mean Absolute Percentage Error
  smape <- mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
  
  # Mean Absolute Scaled Error (assuming naive forecast as benchmark)
  naive_forecast <- c(NA, actual[-length(actual)])
  naive_mae <- mean(abs(actual[-1] - naive_forecast[-1]), na.rm = TRUE)
  mase <- mae / naive_mae
  
  # Median Absolute Error
  medae <- median(abs(actual - predicted))
  
  # Max Error
  max_error <- max(abs(actual - predicted))
  
  # Explained Variance Score
  var_y <- var(actual)
  var_residual <- var(actual - predicted)
  explained_variance <- 1 - (var_residual / var_y)
  
  metrics <- list(
    n_observations = n,
    mae = mae,
    mse = mse,
    rmse = rmse,
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    mape = mape,
    smape = smape,
    mase = mase,
    medae = medae,
    max_error = max_error,
    explained_variance = explained_variance,
    residuals = actual - predicted
  )
  
  cat("Regression metrics calculated successfully!\n")
  return(metrics)
}

#' Plot Regression Diagnostics
#' @param actual Vector of actual values
#' @param predicted Vector of predicted values
#' @param model_name Name of the model
#' @return List of ggplot objects
plot_regression_diagnostics <- function(actual, predicted, model_name = "Model") {
  
  residuals <- actual - predicted
  fitted <- predicted
  
  # 1. Actual vs Predicted
  p1 <- ggplot(data.frame(actual = actual, predicted = predicted), 
               aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(
      title = paste(model_name, "- Actual vs Predicted"),
      x = "Actual Values",
      y = "Predicted Values"
    ) +
    theme_minimal()
  
  # 2. Residuals vs Fitted
  p2 <- ggplot(data.frame(fitted = fitted, residuals = residuals), 
               aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    labs(
      title = paste(model_name, "- Residuals vs Fitted"),
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal()
  
  # 3. Q-Q plot of residuals
  p3 <- ggplot(data.frame(residuals = residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(
      title = paste(model_name, "- Q-Q Plot of Residuals"),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
  
  # 4. Histogram of residuals
  p4 <- ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
    geom_histogram(bins = 30, alpha = 0.7, fill = "skyblue", color = "black") +
    geom_density(aes(y = ..scaled.. * max(..count..)), color = "red", size = 1) +
    labs(
      title = paste(model_name, "- Distribution of Residuals"),
      x = "Residuals",
      y = "Frequency"
    ) +
    theme_minimal()
  
  return(list(
    actual_vs_predicted = p1,
    residuals_vs_fitted = p2,
    qq_plot = p3,
    residuals_histogram = p4
  ))
}

#' ========================================
#' 3. CROSS-VALIDATION
#' ========================================

#' Perform K-Fold Cross-Validation
#' @param data Data frame containing features and target
#' @param target_col Name of target column
#' @param model_func Function that takes train_data and returns fitted model
#' @param predict_func Function that takes model and test_data and returns predictions
#' @param k Number of folds
#' @param metrics_func Function to calculate metrics
#' @param seed Random seed
#' @return List containing CV results
perform_cv <- function(data, target_col, model_func, predict_func, k = 5, 
                      metrics_func = NULL, seed = 42) {
  
  cat("K-FOLD CROSS-VALIDATION\n")
  cat("=======================\n\n")
  
  set.seed(seed)
  
  # Create folds
  n <- nrow(data)
  folds <- createFolds(data[[target_col]], k = k, list = TRUE, returnTrain = FALSE)
  
  cv_results <- list()
  fold_metrics <- list()
  
  for (i in 1:k) {
    cat("Processing fold", i, "of", k, "\n")
    
    # Split data
    test_indices <- folds[[i]]
    train_data <- data[-test_indices, ]
    test_data <- data[test_indices, ]
    
    # Train model
    model <- model_func(train_data)
    
    # Make predictions
    predictions <- predict_func(model, test_data)
    
    # Calculate metrics if function provided
    if (!is.null(metrics_func)) {
      fold_metrics[[i]] <- metrics_func(test_data[[target_col]], predictions)
    }
    
    # Store results
    cv_results[[i]] <- list(
      fold = i,
      train_size = nrow(train_data),
      test_size = nrow(test_data),
      actual = test_data[[target_col]],
      predicted = predictions,
      model = model
    )
  }
  
  # Aggregate metrics
  if (!is.null(metrics_func)) {
    
    # Extract metric names from first fold
    metric_names <- names(fold_metrics[[1]])
    metric_names <- metric_names[!metric_names %in% c("confusion_matrix", "roc_curve", "residuals")]
    
    aggregated_metrics <- list()
    
    for (metric in metric_names) {
      values <- sapply(fold_metrics, function(x) x[[metric]])
      if (is.numeric(values)) {
        aggregated_metrics[[paste0(metric, "_mean")]] <- mean(values, na.rm = TRUE)
        aggregated_metrics[[paste0(metric, "_sd")]] <- sd(values, na.rm = TRUE)
        aggregated_metrics[[paste0(metric, "_cv")]] <- sd(values, na.rm = TRUE) / mean(values, na.rm = TRUE)
      }
    }
    
    cv_results$aggregated_metrics <- aggregated_metrics
    cv_results$fold_metrics <- fold_metrics
  }
  
  cat("Cross-validation completed!\n")
  return(cv_results)
}

#' ========================================
#' 4. MODEL COMPARISON
#' ========================================

#' Compare Multiple Models
#' @param models List of fitted models
#' @param test_data Test data frame
#' @param target_col Name of target column
#' @param model_names Names of models
#' @param task_type "classification" or "regression"
#' @return Comparison results
compare_models <- function(models, test_data, target_col, model_names = NULL, task_type = "classification") {
  
  cat("MODEL COMPARISON\n")
  cat("================\n\n")
  
  if (is.null(model_names)) {
    model_names <- paste("Model", 1:length(models))
  }
  
  actual <- test_data[[target_col]]
  comparison_results <- list()
  
  for (i in seq_along(models)) {
    cat("Evaluating", model_names[i], "\n")
    
    model <- models[[i]]
    
    # Make predictions
    if (task_type == "classification") {
      predictions <- predict(model, test_data)
      probabilities <- NULL
      
      # Try to get probabilities
      tryCatch({
        probabilities <- predict(model, test_data, type = "prob")
      }, error = function(e) {})
      
      metrics <- calculate_classification_metrics(actual, predictions, probabilities)
      
    } else if (task_type == "regression") {
      predictions <- predict(model, test_data)
      metrics <- calculate_regression_metrics(actual, predictions)
    }
    
    comparison_results[[model_names[i]]] <- list(
      model = model,
      predictions = predictions,
      metrics = metrics
    )
  }
  
  # Create comparison table
  if (task_type == "classification") {
    comparison_table <- data.frame(
      Model = model_names,
      Accuracy = sapply(comparison_results, function(x) round(x$metrics$accuracy, 4)),
      Precision = sapply(comparison_results, function(x) round(x$metrics$precision, 4)),
      Recall = sapply(comparison_results, function(x) round(x$metrics$recall, 4)),
      F1_Score = sapply(comparison_results, function(x) round(x$metrics$f1_score, 4)),
      AUC = sapply(comparison_results, function(x) {
        if (!is.null(x$metrics$auc)) round(x$metrics$auc, 4) else NA
      })
    )
  } else {
    comparison_table <- data.frame(
      Model = model_names,
      RMSE = sapply(comparison_results, function(x) round(x$metrics$rmse, 4)),
      MAE = sapply(comparison_results, function(x) round(x$metrics$mae, 4)),
      R_Squared = sapply(comparison_results, function(x) round(x$metrics$r_squared, 4)),
      MAPE = sapply(comparison_results, function(x) round(x$metrics$mape, 4))
    )
  }
  
  comparison_results$comparison_table <- comparison_table
  
  cat("Model comparison completed!\n")
  return(comparison_results)
}

#' ========================================
#' 5. FEATURE IMPORTANCE
#' ========================================

#' Extract Feature Importance
#' @param model Fitted model
#' @param model_type Type of model ("randomForest", "glmnet", "xgboost", etc.)
#' @param feature_names Names of features
#' @return Data frame with feature importance
extract_feature_importance <- function(model, model_type = "auto", feature_names = NULL) {
  
  cat("EXTRACTING FEATURE IMPORTANCE\n")
  cat("=============================\n\n")
  
  # Auto-detect model type if not specified
  if (model_type == "auto") {
    model_class <- class(model)[1]
    model_type <- switch(model_class,
      "randomForest" = "randomForest",
      "cv.glmnet" = "glmnet",
      "glmnet" = "glmnet",
      "xgb.Booster" = "xgboost",
      "lm" = "linear",
      "glm" = "linear",
      "unknown"
    )
  }
  
  importance_df <- switch(model_type,
    "randomForest" = {
      imp <- importance(model)
      if (ncol(imp) == 1) {
        data.frame(
          feature = rownames(imp),
          importance = imp[, 1],
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          feature = rownames(imp),
          importance = imp[, "MeanDecreaseGini"],
          stringsAsFactors = FALSE
        )
      }
    },
    
    "glmnet" = {
      coefs <- coef(model, s = "lambda.min")
      coef_df <- data.frame(
        feature = rownames(coefs),
        importance = abs(as.vector(coefs)),
        stringsAsFactors = FALSE
      )
      coef_df[coef_df$feature != "(Intercept)" & coef_df$importance > 0, ]
    },
    
    "xgboost" = {
      imp <- xgb.importance(model = model)
      data.frame(
        feature = imp$Feature,
        importance = imp$Gain,
        stringsAsFactors = FALSE
      )
    },
    
    "linear" = {
      coefs <- summary(model)$coefficients
      data.frame(
        feature = rownames(coefs)[-1],  # Exclude intercept
        importance = abs(coefs[-1, "t value"]),
        stringsAsFactors = FALSE
      )
    },
    
    {
      warning("Model type not supported for feature importance extraction")
      return(NULL)
    }
  )
  
  if (!is.null(importance_df)) {
    # Sort by importance
    importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), ]
    rownames(importance_df) <- NULL
    
    cat("Feature importance extracted for", nrow(importance_df), "features\n")
  }
  
  return(importance_df)
}

#' Plot Feature Importance
#' @param importance_df Data frame with feature importance
#' @param top_n Number of top features to plot
#' @param title Plot title
#' @return ggplot object
plot_feature_importance <- function(importance_df, top_n = 20, title = "Feature Importance") {
  
  # Select top N features
  plot_data <- head(importance_df, top_n)
  plot_data$feature <- factor(plot_data$feature, levels = rev(plot_data$feature))
  
  ggplot(plot_data, aes(x = feature, y = importance)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = title,
      x = "Features",
      y = "Importance"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.y = element_text(size = 10)
    )
}

#' ========================================
#' 6. LEARNING CURVES
#' ========================================

#' Generate Learning Curves
#' @param data Training data
#' @param target_col Name of target column
#' @param model_func Function that takes data and returns fitted model
#' @param predict_func Function that takes model and data and returns predictions
#' @param metrics_func Function to calculate metrics
#' @param train_sizes Vector of training sizes to evaluate
#' @param cv_folds Number of CV folds for each size
#' @return Learning curve data
generate_learning_curves <- function(data, target_col, model_func, predict_func, 
                                   metrics_func, train_sizes = NULL, cv_folds = 3) {
  
  cat("GENERATING LEARNING CURVES\n")
  cat("==========================\n\n")
  
  n <- nrow(data)
  
  if (is.null(train_sizes)) {
    train_sizes <- unique(round(seq(0.1, 1.0, by = 0.1) * n))
  }
  
  learning_data <- data.frame()
  
  for (size in train_sizes) {
    cat("Training size:", size, "samples\n")
    
    # Perform CV for this training size
    for (cv in 1:cv_folds) {
      # Sample training data
      train_indices <- sample(n, size)
      train_data <- data[train_indices, ]
      
      # Use remaining data for validation
      val_data <- data[-train_indices, ]
      
      if (nrow(val_data) == 0) {
        val_data <- train_data  # Use training data if no validation data
      }
      
      # Train model
      model <- model_func(train_data)
      
      # Evaluate on training data
      train_pred <- predict_func(model, train_data)
      train_metrics <- metrics_func(train_data[[target_col]], train_pred)
      
      # Evaluate on validation data
      val_pred <- predict_func(model, val_data)
      val_metrics <- metrics_func(val_data[[target_col]], val_pred)
      
      # Store results
      learning_data <- rbind(learning_data, data.frame(
        train_size = size,
        cv_fold = cv,
        train_score = train_metrics[[1]],  # Assuming first metric is primary
        val_score = val_metrics[[1]],
        stringsAsFactors = FALSE
      ))
    }
  }
  
  cat("Learning curves generated!\n")
  return(learning_data)
}

#' Plot Learning Curves
#' @param learning_data Data from generate_learning_curves
#' @param metric_name Name of the metric being plotted
#' @return ggplot object
plot_learning_curves <- function(learning_data, metric_name = "Score") {
  
  # Aggregate by training size
  agg_data <- learning_data %>%
    group_by(train_size) %>%
    summarise(
      train_mean = mean(train_score),
      train_sd = sd(train_score),
      val_mean = mean(val_score),
      val_sd = sd(val_score),
      .groups = 'drop'
    )
  
  # Reshape for plotting
  plot_data <- data.frame(
    train_size = rep(agg_data$train_size, 2),
    score = c(agg_data$train_mean, agg_data$val_mean),
    sd = c(agg_data$train_sd, agg_data$val_sd),
    type = rep(c("Training", "Validation"), each = nrow(agg_data))
  )
  
  ggplot(plot_data, aes(x = train_size, y = score, color = type)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = 0.02) +
    labs(
      title = "Learning Curves",
      x = "Training Size",
      y = metric_name,
      color = "Dataset"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    )
}

#' ========================================
#' 7. DEMONSTRATION FUNCTIONS
#' ========================================

#' Run Model Evaluation Demo
run_model_evaluation_demo <- function() {
  
  cat("===========================================\n")
  cat("MODEL EVALUATION FRAMEWORK DEMONSTRATION\n")
  cat("===========================================\n\n")
  
  # Load sample data
  data(iris)
  
  # Prepare classification data
  iris_data <- iris
  iris_data$Species <- factor(iris_data$Species)
  
  # Split data
  set.seed(42)
  train_indices <- sample(nrow(iris_data), 0.7 * nrow(iris_data))
  train_data <- iris_data[train_indices, ]
  test_data <- iris_data[-train_indices, ]
  
  cat("CLASSIFICATION DEMONSTRATION\n")
  cat("============================\n\n")
  
  # Train a simple model
  rf_model <- randomForest(Species ~ ., data = train_data)
  
  # Make predictions
  predictions <- predict(rf_model, test_data)
  probabilities <- predict(rf_model, test_data, type = "prob")
  
  # Calculate metrics
  class_metrics <- calculate_classification_metrics(
    test_data$Species, predictions, probabilities
  )
  
  # Print some metrics
  cat("Accuracy:", round(class_metrics$accuracy, 4), "\n")
  cat("Kappa:", round(class_metrics$kappa, 4), "\n")
  
  # Feature importance
  importance_df <- extract_feature_importance(rf_model, "randomForest")
  print(importance_df)
  
  cat("\n", rep("=", 50), "\n")
  
  # Regression demonstration with mtcars
  cat("REGRESSION DEMONSTRATION\n")
  cat("========================\n\n")
  
  data(mtcars)
  
  # Split data
  train_indices <- sample(nrow(mtcars), 0.7 * nrow(mtcars))
  train_data <- mtcars[train_indices, ]
  test_data <- mtcars[-train_indices, ]
  
  # Train model
  lm_model <- lm(mpg ~ ., data = train_data)
  
  # Make predictions
  predictions <- predict(lm_model, test_data)
  
  # Calculate metrics
  reg_metrics <- calculate_regression_metrics(test_data$mpg, predictions)
  
  # Print some metrics
  cat("RMSE:", round(reg_metrics$rmse, 4), "\n")
  cat("R-squared:", round(reg_metrics$r_squared, 4), "\n")
  cat("MAE:", round(reg_metrics$mae, 4), "\n")
  
  cat("\n===========================================\n")
  cat("MODEL EVALUATION DEMONSTRATION COMPLETE!\n")
  cat("===========================================\n")
}

# Run demonstration if script is executed directly
if (sys.nframe() == 0) {
  run_model_evaluation_demo()
}
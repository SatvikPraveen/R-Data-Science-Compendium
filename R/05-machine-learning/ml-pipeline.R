#' @title Advanced Machine Learning Pipeline
#' @description Production-ready ML workflows with comprehensive evaluation
#' @author Portfolio Developer
#' @date 2025

library(caret)
library(randomForest)
library(xgboost)
library(e1071)
library(dplyr)
library(ggplot2)
library(pROC)
library(ROSE)

# =============================================================================
# S3 CLASS SYSTEM FOR ML PIPELINE
# =============================================================================

#' Create Machine Learning Pipeline Object
#'
#' @description Initializes a comprehensive ML pipeline with S3 class system
#' @param data Input data frame
#' @param target_var Name of target variable
#' @param problem_type Type of ML problem ("classification" or "regression")
#' @param test_split Proportion of data for testing
#' @param validation_split Proportion of training data for validation
#' @param seed Random seed for reproducibility
#' @return MLPipeline S3 object
#' @export
create_ml_pipeline <- function(data, target_var, problem_type = "auto", 
                              test_split = 0.2, validation_split = 0.2, seed = 42) {
  
  # Input validation
  if (!target_var %in% names(data)) {
    stop("Target variable '", target_var, "' not found in data")
  }
  
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }
  
  # Auto-detect problem type
  if (problem_type == "auto") {
    if (is.numeric(data[[target_var]]) && length(unique(data[[target_var]])) > 10) {
      problem_type <- "regression"
    } else {
      problem_type <- "classification"
    }
  }
  
  # Create pipeline object
  pipeline <- structure(
    list(
      data = data,
      target_var = target_var,
      problem_type = problem_type,
      test_split = test_split,
      validation_split = validation_split,
      seed = seed,
      
      # Data components (to be populated)
      train_data = NULL,
      validation_data = NULL,
      test_data = NULL,
      
      # Preprocessing components
      preprocessor = NULL,
      feature_names = NULL,
      
      # Model components
      models = list(),
      best_model = NULL,
      
      # Results
      performance_metrics = list(),
      predictions = list(),
      
      # Metadata
      created_at = Sys.time(),
      last_updated = Sys.time()
    ),
    class = "MLPipeline"
  )
  
  return(pipeline)
}

#' Print method for MLPipeline
#' @param x MLPipeline object
#' @param ... Additional arguments
#' @export
print.MLPipeline <- function(x, ...) {
  cat("Machine Learning Pipeline\n")
  cat("========================\n")
  cat("Problem type:", x$problem_type, "\n")
  cat("Target variable:", x$target_var, "\n")
  cat("Original data:", nrow(x$data), "rows ×", ncol(x$data), "columns\n")
  
  if (!is.null(x$train_data)) {
    cat("Training data:", nrow(x$train_data), "rows\n")
    cat("Validation data:", ifelse(is.null(x$validation_data), "None", nrow(x$validation_data)), "rows\n")
    cat("Test data:", nrow(x$test_data), "rows\n")
  }
  
  cat("Models trained:", length(x$models), "\n")
  
  if (length(x$models) > 0) {
    cat("Available models:", paste(names(x$models), collapse = ", "), "\n")
  }
  
  if (!is.null(x$best_model)) {
    cat("Best model:", x$best_model, "\n")
  }
  
  cat("Last updated:", format(x$last_updated), "\n")
}

# =============================================================================
# DATA PREPROCESSING AND FEATURE ENGINEERING
# =============================================================================

#' Split Data into Train/Validation/Test Sets
#'
#' @description Splits data with stratification for classification problems
#' @param pipeline MLPipeline object
#' @return Updated MLPipeline object
#' @export
split_data <- function(pipeline) {
  
  set.seed(pipeline$seed)
  
  # Create initial train/test split
  if (pipeline$problem_type == "classification") {
    train_indices <- createDataPartition(
      pipeline$data[[pipeline$target_var]], 
      p = 1 - pipeline$test_split,
      list = FALSE
    )[, 1]
  } else {
    train_indices <- sample(
      nrow(pipeline$data), 
      size = floor((1 - pipeline$test_split) * nrow(pipeline$data))
    )
  }
  
  # Split data
  train_full <- pipeline$data[train_indices, ]
  pipeline$test_data <- pipeline$data[-train_indices, ]
  
  # Further split training data into train/validation
  if (pipeline$validation_split > 0) {
    if (pipeline$problem_type == "classification") {
      val_indices <- createDataPartition(
        train_full[[pipeline$target_var]], 
        p = pipeline$validation_split,
        list = FALSE
      )[, 1]
    } else {
      val_indices <- sample(
        nrow(train_full), 
        size = floor(pipeline$validation_split * nrow(train_full))
      )
    }
    
    pipeline$validation_data <- train_full[val_indices, ]
    pipeline$train_data <- train_full[-val_indices, ]
  } else {
    pipeline$train_data <- train_full
    pipeline$validation_data <- NULL
  }
  
  pipeline$last_updated <- Sys.time()
  
  message("Data split completed:")
  message("  Training: ", nrow(pipeline$train_data), " samples")
  message("  Validation: ", ifelse(is.null(pipeline$validation_data), 0, nrow(pipeline$validation_data)), " samples")
  message("  Test: ", nrow(pipeline$test_data), " samples")
  
  return(pipeline)
}

#' Advanced Feature Engineering and Preprocessing
#'
#' @description Comprehensive preprocessing including scaling, encoding, and feature creation
#' @param pipeline MLPipeline object
#' @param handle_missing Method for missing values ("remove", "impute", "flag")
#' @param scale_features Logical, whether to scale numeric features
#' @param create_interactions Logical, whether to create interaction terms
#' @param encode_categorical Method for categorical encoding ("dummy", "label", "target")
#' @return Updated MLPipeline object
#' @export
preprocess_data <- function(pipeline, handle_missing = "impute", scale_features = TRUE,
                           create_interactions = FALSE, encode_categorical = "dummy") {
  
  if (is.null(pipeline$train_data)) {
    stop("Data must be split before preprocessing. Run split_data() first.")
  }
  
  # Identify feature types
  numeric_features <- names(pipeline$train_data)[sapply(pipeline$train_data, is.numeric)]
  numeric_features <- setdiff(numeric_features, pipeline$target_var)
  
  categorical_features <- names(pipeline$train_data)[sapply(pipeline$train_data, function(x) is.factor(x) || is.character(x))]
  categorical_features <- setdiff(categorical_features, pipeline$target_var)
  
  message("Preprocessing features:")
  message("  Numeric: ", length(numeric_features))
  message("  Categorical: ", length(categorical_features))
  
  # Create preprocessing steps
  preprocess_steps <- list()
  
  # Handle missing values
  if (handle_missing == "impute") {
    # For numeric: median imputation
    # For categorical: mode imputation
    preprocess_steps$missing_numeric <- preProcess(
      pipeline$train_data[numeric_features], 
      method = "medianImpute"
    )
    
    if (length(categorical_features) > 0) {
      # Mode imputation for categorical
      mode_values <- sapply(pipeline$train_data[categorical_features], function(x) {
        if (is.character(x)) x <- as.factor(x)
        names(sort(table(x), decreasing = TRUE))[1]
      })
      preprocess_steps$mode_values <- mode_values
    }
    
  } else if (handle_missing == "flag") {
    # Create missing value indicators
    missing_indicators <- paste0(numeric_features, "_missing")
    for (i in seq_along(numeric_features)) {
      col <- numeric_features[i]
      pipeline$train_data[[missing_indicators[i]]] <- as.numeric(is.na(pipeline$train_data[[col]]))
    }
  }
  
  # Scaling and normalization
  if (scale_features && length(numeric_features) > 0) {
    preprocess_steps$scaling <- preProcess(
      pipeline$train_data[numeric_features],
      method = c("center", "scale")
    )
  }
  
  # Near-zero variance detection
  if (length(numeric_features) > 0) {
    preprocess_steps$nzv <- preProcess(
      pipeline$train_data[numeric_features],
      method = "nzv"
    )
  }
  
  # Categorical encoding
  if (length(categorical_features) > 0) {
    if (encode_categorical == "dummy") {
      # One-hot encoding
      preprocess_steps$dummy_vars <- categorical_features
      
    } else if (encode_categorical == "target" && pipeline$problem_type == "classification") {
      # Target encoding for categorical variables
      target_encodings <- list()
      for (cat_var in categorical_features) {
        target_encodings[[cat_var]] <- pipeline$train_data %>%
          group_by(!!sym(cat_var)) %>%
          summarise(
            target_mean = mean(as.numeric(as.factor(!!sym(pipeline$target_var))) - 1),
            .groups = "drop"
          )
      }
      preprocess_steps$target_encodings <- target_encodings
    }
  }
  
  # Feature interactions
  if (create_interactions && length(numeric_features) >= 2) {
    # Create polynomial features and interactions for top numeric features
    top_features <- numeric_features[1:min(3, length(numeric_features))]
    preprocess_steps$interactions <- top_features
  }
  
  # Store preprocessing info
  pipeline$preprocessor <- preprocess_steps
  pipeline$feature_names <- list(
    numeric = numeric_features,
    categorical = categorical_features
  )
  
  # Apply preprocessing to all datasets
  pipeline <- apply_preprocessing(pipeline)
  
  pipeline$last_updated <- Sys.time()
  
  return(pipeline)
}

#' Apply Preprocessing to All Datasets
#'
#' @description Applies fitted preprocessing steps to train, validation, and test sets
#' @param pipeline MLPipeline object with fitted preprocessor
#' @return Updated MLPipeline object
apply_preprocessing <- function(pipeline) {
  
  datasets <- list(
    train = pipeline$train_data,
    validation = pipeline$validation_data,
    test = pipeline$test_data
  )
  
  # Remove NULL datasets
  datasets <- datasets[!sapply(datasets, is.null)]
  
  processed_datasets <- list()
  
  for (dataset_name in names(datasets)) {
    data <- datasets[[dataset_name]]
    
    # Apply missing value handling
    if ("missing_numeric" %in% names(pipeline$preprocessor)) {
      data[pipeline$feature_names$numeric] <- predict(
        pipeline$preprocessor$missing_numeric, 
        data[pipeline$feature_names$numeric]
      )
    }
    
    if ("mode_values" %in% names(pipeline$preprocessor)) {
      for (col in names(pipeline$preprocessor$mode_values)) {
        if (col %in% names(data)) {
          data[[col]][is.na(data[[col]])] <- pipeline$preprocessor$mode_values[[col]]
        }
      }
    }
    
    # Apply scaling
    if ("scaling" %in% names(pipeline$preprocessor)) {
      data[pipeline$feature_names$numeric] <- predict(
        pipeline$preprocessor$scaling, 
        data[pipeline$feature_names$numeric]
      )
    }
    
    # Remove near-zero variance features
    if ("nzv" %in% names(pipeline$preprocessor)) {
      nzv_features <- names(predict(pipeline$preprocessor$nzv, data[pipeline$feature_names$numeric]))
      data <- data[nzv_features]
    }
    
    # Apply categorical encoding
    if ("dummy_vars" %in% names(pipeline$preprocessor)) {
      # Create dummy variables
      for (cat_var in pipeline$preprocessor$dummy_vars) {
        if (cat_var %in% names(data)) {
          # Convert to factor if character
          if (is.character(data[[cat_var]])) {
            data[[cat_var]] <- as.factor(data[[cat_var]])
          }
          
          # Create dummy variables
          dummy_matrix <- model.matrix(as.formula(paste("~", cat_var, "- 1")), data = data)
          
          # Add dummy variables to data
          dummy_df <- as.data.frame(dummy_matrix)
          names(dummy_df) <- paste0(cat_var, "_", names(dummy_df))
          
          # Remove original categorical variable and add dummies
          data <- data[, !names(data) %in% cat_var, drop = FALSE]
          data <- cbind(data, dummy_df)
        }
      }
    }
    
    # Apply target encoding
    if ("target_encodings" %in% names(pipeline$preprocessor)) {
      for (cat_var in names(pipeline$preprocessor$target_encodings)) {
        if (cat_var %in% names(data)) {
          encoding_map <- pipeline$preprocessor$target_encodings[[cat_var]]
          data[[paste0(cat_var, "_encoded")]] <- encoding_map$target_mean[match(data[[cat_var]], encoding_map[[cat_var]])]
          # Keep original for reference, but models will use encoded version
        }
      }
    }
    
    # Create interaction features
    if ("interactions" %in% names(pipeline$preprocessor)) {
      interaction_features <- pipeline$preprocessor$interactions
      for (i in 1:(length(interaction_features) - 1)) {
        for (j in (i + 1):length(interaction_features)) {
          feat1 <- interaction_features[i]
          feat2 <- interaction_features[j]
          if (feat1 %in% names(data) && feat2 %in% names(data)) {
            data[[paste0(feat1, "_x_", feat2)]] <- data[[feat1]] * data[[feat2]]
          }
        }
      }
    }
    
    processed_datasets[[dataset_name]] <- data
  }
  
  # Update pipeline with processed data
  pipeline$train_data <- processed_datasets$train
  pipeline$validation_data <- processed_datasets$validation
  pipeline$test_data <- processed_datasets$test
  
  message("Preprocessing applied to all datasets")
  message("Final feature count: ", ncol(pipeline$train_data) - 1)
  
  return(pipeline)
}

# =============================================================================
# MODEL TRAINING AND HYPERPARAMETER TUNING
# =============================================================================

#' Train Multiple Models with Cross-Validation
#'
#' @description Trains and compares multiple machine learning algorithms
#' @param pipeline MLPipeline object
#' @param algorithms Vector of algorithm names to train
#' @param cv_folds Number of cross-validation folds
#' @param tune_length Number of hyperparameter combinations to try
#' @return Updated MLPipeline object
#' @export
train_models <- function(pipeline, 
                        algorithms = c("rf", "xgbTree", "svmRadial", "glmnet"),
                        cv_folds = 5, tune_length = 5) {
  
  if (is.null(pipeline$train_data)) {
    stop("Data must be preprocessed before training. Run preprocess_data() first.")
  }
  
  # Prepare training data
  feature_cols <- setdiff(names(pipeline$train_data), pipeline$target_var)
  X_train <- pipeline$train_data[feature_cols]
  y_train <- pipeline$train_data[[pipeline$target_var]]
  
  # Set up training control
  if (pipeline$problem_type == "classification") {
    # Ensure target is factor
    if (!is.factor(y_train)) {
      y_train <- as.factor(y_train)
      pipeline$train_data[[pipeline$target_var]] <- y_train
    }
    
    train_control <- trainControl(
      method = "cv",
      number = cv_folds,
      verboseIter = FALSE,
      savePredictions = "final",
      classProbs = TRUE,
      summaryFunction = twoClassSummary,
      allowParallel = TRUE
    )
    metric <- "ROC"
  } else {
    train_control <- trainControl(
      method = "cv",
      number = cv_folds,
      verboseIter = FALSE,
      savePredictions = "final",
      allowParallel = TRUE
    )
    metric <- "RMSE"
  }
  
  # Train each algorithm
  models <- list()
  
  for (algorithm in algorithms) {
    message("Training ", algorithm, " model...")
    
    tryCatch({
      # Special handling for different algorithms
      if (algorithm == "xgbTree") {
        # Convert factors to numeric for xgboost
        X_train_xgb <- X_train
        factor_cols <- sapply(X_train_xgb, is.factor)
        X_train_xgb[factor_cols] <- lapply(X_train_xgb[factor_cols], as.numeric)
        
        model <- train(
          x = X_train_xgb,
          y = y_train,
          method = algorithm,
          trControl = train_control,
          metric = metric,
          tuneLength = tune_length,
          verbosity = 0
        )
      } else {
        model <- train(
          x = X_train,
          y = y_train,
          method = algorithm,
          trControl = train_control,
          metric = metric,
          tuneLength = tune_length
        )
      }
      
      models[[algorithm]] <- model
      message("✓ ", algorithm, " completed successfully")
      
    }, error = function(e) {
      message("✗ ", algorithm, " failed: ", e$message)
    })
  }
  
  pipeline$models <- models
  pipeline$last_updated <- Sys.time()
  
  message("\nModel training completed. Trained models: ", length(models))
  
  return(pipeline)
}

#' Advanced Hyperparameter Tuning
#'
#' @description Performs more extensive hyperparameter search for best models
#' @param pipeline MLPipeline object
#' @param model_name Name of model to tune
#' @param search_type Type of search ("grid", "random")
#' @param max_iterations Maximum iterations for random search
#' @return Updated MLPipeline object
#' @export
tune_hyperparameters <- function(pipeline, model_name, search_type = "grid", max_iterations = 50) {
  
  if (!model_name %in% names(pipeline$models)) {
    stop("Model '", model_name, "' not found in pipeline")
  }
  
  # Define custom tuning grids
  custom_grids <- list(
    rf = expand.grid(
      mtry = c(2, 4, 6, 8, 10),
      splitrule = c("gini", "extratrees"),
      min.node.size = c(1, 3, 5)
    ),
    xgbTree = expand.grid(
      nrounds = c(50, 100, 200),
      max_depth = c(3, 6, 9),
      eta = c(0.01, 0.1, 0.3),
      gamma = c(0, 0.1, 0.2),
      colsample_bytree = c(0.6, 0.8, 1.0),
      min_child_weight = c(1, 3, 5),
      subsample = c(0.8, 1.0)
    ),
    svmRadial = expand.grid(
      sigma = c(0.001, 0.01, 0.1, 1),
      C = c(0.1, 1, 10, 100)
    )
  )
  
  # Prepare data
  feature_cols <- setdiff(names(pipeline$train_data), pipeline$target_var)
  X_train <- pipeline$train_data[feature_cols]
  y_train <- pipeline$train_data[[pipeline$target_var]]
  
  # Training control for hyperparameter tuning
  if (pipeline$problem_type == "classification") {
    if (!is.factor(y_train)) {
      y_train <- as.factor(y_train)
    }
    
    tune_control <- trainControl(
      method = "cv",
      number = 10,
      verboseIter = TRUE,
      savePredictions = "final",
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    )
    metric <- "ROC"
  } else {
    tune_control <- trainControl(
      method = "cv",
      number = 10,
      verboseIter = TRUE,
      savePredictions = "final"
    )
    metric <- "RMSE"
  }
  
  message("Performing extensive hyperparameter tuning for ", model_name, "...")
  
  # Perform tuning
  if (search_type == "grid" && model_name %in% names(custom_grids)) {
    tuning_grid <- custom_grids[[model_name]]
  } else {
    tuning_grid <- NULL  # Use random search
  }
  
  tuned_model <- train(
    x = X_train,
    y = y_train,
    method = model_name,
    trControl = tune_control,
    metric = metric,
    tuneGrid = tuning_grid,
    tuneLength = ifelse(is.null(tuning_grid), max_iterations, nrow(tuning_grid))
  )
  
  # Replace model in pipeline
  pipeline$models[[paste0(model_name, "_tuned")]] <- tuned_model
  
  message("Hyperparameter tuning completed for ", model_name)
  
  return(pipeline)
}

# =============================================================================
# MODEL EVALUATION AND COMPARISON
# =============================================================================

#' Comprehensive Model Evaluation
#'
#' @description Evaluates all trained models using multiple metrics
#' @param pipeline MLPipeline object
#' @param use_validation Logical, whether to use validation set (if available)
#' @return Updated MLPipeline object with evaluation results
#' @export
evaluate_models <- function(pipeline, use_validation = TRUE) {
  
  if (length(pipeline$models) == 0) {
    stop("No models found. Train models first using train_models()")
  }
  
  # Choose evaluation dataset
  if (use_validation && !is.null(pipeline$validation_data)) {
    eval_data <- pipeline$validation_data
    eval_name <- "Validation"
  } else {
    eval_data <- pipeline$test_data
    eval_name <- "Test"
  }
  
  feature_cols <- setdiff(names(eval_data), pipeline$target_var)
  X_eval <- eval_data[feature_cols]
  y_true <- eval_data[[pipeline$target_var]]
  
  message("Evaluating models on ", eval_name, " set...")
  
  evaluation_results <- list()
  predictions_list <- list()
  
  for (model_name in names(pipeline$models)) {
    model <- pipeline$models[[model_name]]
    
    tryCatch({
      # Make predictions
      if (pipeline$problem_type == "classification") {
        # Ensure consistent factor levels
        if (!is.factor(y_true)) {
          y_true <- as.factor(y_true)
        }
        
        # Predictions
        pred_class <- predict(model, X_eval)
        pred_prob <- predict(model, X_eval, type = "prob")
        
        # Ensure consistent factor levels
        pred_class <- factor(pred_class, levels = levels(y_true))
        
        # Calculate metrics
        cm <- confusionMatrix(pred_class, y_true)
        
        # ROC analysis (for binary classification)
        if (length(levels(y_true)) == 2 && ncol(pred_prob) == 2) {
          roc_obj <- roc(
            response = as.numeric(y_true) - 1,
            predictor = pred_prob[, 2],
            quiet = TRUE
          )
          auc_score <- as.numeric(auc(roc_obj))
        } else {
          auc_score <- NA
        }
        
        # Store results
        evaluation_results[[model_name]] <- list(
          accuracy = cm$overall["Accuracy"],
          kappa = cm$overall["Kappa"],
          sensitivity = cm$byClass["Sensitivity"],
          specificity = cm$byClass["Specificity"],
          precision = cm$byClass["Precision"],
          recall = cm$byClass["Recall"],
          f1_score = cm$byClass["F1"],
          auc = auc_score,
          confusion_matrix = cm$table
        )
        
        predictions_list[[model_name]] <- list(
          predicted_class = pred_class,
          predicted_probabilities = pred_prob,
          actual = y_true
        )
        
      } else {
        # Regression
        pred_values <- predict(model, X_eval)
        
        # Calculate metrics
        rmse <- sqrt(mean((y_true - pred_values)^2, na.rm = TRUE))
        mae <- mean(abs(y_true - pred_values), na.rm = TRUE)
        r_squared <- cor(y_true, pred_values, use = "complete.obs")^2
        
        # Mean Absolute Percentage Error
        mape <- mean(abs((y_true - pred_values) / y_true) * 100, na.rm = TRUE)
        
        evaluation_results[[model_name]] <- list(
          rmse = rmse,
          mae = mae,
          r_squared = r_squared,
          mape = mape
        )
        
        predictions_list[[model_name]] <- list(
          predicted = pred_values,
          actual = y_true
        )
      }
      
      message("✓ ", model_name, " evaluated successfully")
      
    }, error = function(e) {
      message("✗ ", model_name, " evaluation failed: ", e$message)
      evaluation_results[[model_name]] <- NULL
    })
  }
  
  # Store results
  pipeline$performance_metrics <- evaluation_results
  pipeline$predictions <- predictions_list
  
  # Identify best model
  if (pipeline$problem_type == "classification") {
    # Use AUC for binary, accuracy for multiclass
    if (length(levels(y_true)) == 2) {
      best_metric <- sapply(evaluation_results, function(x) ifelse(is.null(x$auc) || is.na(x$auc), 0, x$auc))
    } else {
      best_metric <- sapply(evaluation_results, function(x) ifelse(is.null(x$accuracy), 0, x$accuracy))
    }
  } else {
    # Use R-squared for regression (higher is better)
    best_metric <- sapply(evaluation_results, function(x) ifelse(is.null(x$r_squared), 0, x$r_squared))
  }
  
  if (length(best_metric) > 0) {
    pipeline$best_model <- names(which.max(best_metric))
  }
  
  pipeline$last_updated <- Sys.time()
  
  message("\nModel evaluation completed.")
  if (!is.null(pipeline$best_model)) {
    message("Best model: ", pipeline$best_model)
  }
  
  return(pipeline)
}

#' Create Model Comparison Visualizations
#'
#' @description Generates comprehensive visualizations comparing model performance
#' @param pipeline MLPipeline object with evaluation results
#' @return List of ggplot objects
#' @export
visualize_model_performance <- function(pipeline) {
  
  if (length(pipeline$performance_metrics) == 0) {
    stop("No evaluation results found. Run evaluate_models() first.")
  }
  
  plots <- list()
  
  if (pipeline$problem_type == "classification") {
    
    # Performance metrics comparison
    metrics_df <- map_dfr(pipeline$performance_metrics, function(x) {
      tibble(
        accuracy = x$accuracy,
        sensitivity = x$sensitivity,
        specificity = x$specificity,
        f1_score = x$f1_score,
        auc = ifelse(is.null(x$auc) || is.na(x$auc), NA, x$auc)
      )
    }, .id = "model") %>%
      pivot_longer(-model, names_to = "metric", values_to = "value")
    
    plots$metrics_comparison <- ggplot(metrics_df, aes(x = model, y = value, fill = metric)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = get_custom_palette("professional", n_distinct(metrics_df$metric))) +
      labs(
        title = "Model Performance Comparison",
        subtitle = "Classification metrics across all trained models",
        x = "Model", y = "Score", fill = "Metric"
      ) +
      theme_professional() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # ROC Curves (for binary classification)
    if (length(levels(pipeline$predictions[[1]]$actual)) == 2) {
      roc_data <- map_dfr(names(pipeline$predictions), function(model_name) {
        pred_data <- pipeline$predictions[[model_name]]
        if ("predicted_probabilities" %in% names(pred_data)) {
          roc_obj <- roc(
            response = as.numeric(pred_data$actual) - 1,
            predictor = pred_data$predicted_probabilities[, 2],
            quiet = TRUE
          )
          
          tibble(
            model = model_name,
            fpr = 1 - roc_obj$specificities,
            tpr = roc_obj$sensitivities,
            auc = as.numeric(auc(roc_obj))
          )
        }
      })
      
      if (nrow(roc_data) > 0) {
        plots$roc_curves <- ggplot(roc_data, aes(x = fpr, y = tpr, color = model)) +
          geom_line(size = 1.2) +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
          scale_color_manual(values = get_custom_palette("professional", n_distinct(roc_data$model))) +
          labs(
            title = "ROC Curves Comparison",
            subtitle = "Receiver Operating Characteristic curves for all models",
            x = "False Positive Rate",
            y = "True Positive Rate",
            color = "Model"
          ) +
          theme_professional() +
          coord_fixed()
      }
    }
    
  } else {
    # Regression metrics
    metrics_df <- map_dfr(pipeline$performance_metrics, function(x) {
      tibble(
        rmse = x$rmse,
        mae = x$mae,
        r_squared = x$r_squared,
        mape = x$mape
      )
    }, .id = "model") %>%
      pivot_longer(-model, names_to = "metric", values_to = "value")
    
    plots$metrics_comparison <- ggplot(metrics_df, aes(x = model, y = value, fill = metric)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = get_custom_palette("professional", n_distinct(metrics_df$metric))) +
      labs(
        title = "Model Performance Comparison",
        subtitle = "Regression metrics across all trained models",
        x = "Model", y = "Score", fill = "Metric"
      ) +
      theme_professional() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Prediction vs Actual plots
    pred_actual_data <- map_dfr(names(pipeline$predictions), function(model_name) {
      pred_data <- pipeline$predictions[[model_name]]
      tibble(
        model = model_name,
        predicted = pred_data$predicted,
        actual = pred_data$actual
      )
    })
    
    plots$prediction_vs_actual <- ggplot(pred_actual_data, aes(x = actual, y = predicted)) +
      geom_point(alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      facet_wrap(~ model, scales = "free") +
      labs(
        title = "Predicted vs Actual Values",
        subtitle = "Model predictions compared to true values",
        x = "Actual Values",
        y = "Predicted Values"
      ) +
      theme_professional()
  }
  
  return(plots)
}

# =============================================================================
# MODEL INTERPRETABILITY AND FEATURE IMPORTANCE
# =============================================================================

#' Feature Importance Analysis
#'
#' @description Analyzes and visualizes feature importance across models
#' @param pipeline MLPipeline object
#' @param top_n Number of top features to display
#' @return List with importance data and plots
#' @export
analyze_feature_importance <- function(pipeline, top_n = 15) {
  
  if (length(pipeline$models) == 0) {
    stop("No models found for feature importance analysis")
  }
  
  importance_data <- list()
  
  for (model_name in names(pipeline$models)) {
    model <- pipeline$models[[model_name]]
    
    tryCatch({
      if (model$method == "rf" || model$method == "ranger") {
        # Random Forest importance
        imp <- varImp(model)$importance
        importance_data[[model_name]] <- tibble(
          feature = rownames(imp),
          importance = imp[, 1],
          model = model_name
        )
        
      } else if (model$method == "xgbTree") {
        # XGBoost importance
        imp <- varImp(model)$importance
        importance_data[[model_name]] <- tibble(
          feature = rownames(imp),
          importance = imp[, 1],
          model = model_name
        )
        
      } else if (model$method == "glmnet") {
        # Regularized regression coefficients
        imp <- varImp(model)$importance
        importance_data[[model_name]] <- tibble(
          feature = rownames(imp),
          importance = imp[, 1],
          model = model_name
        )
        
      } else {
        # Generic variable importance
        imp <- varImp(model)
        if ("importance" %in% names(imp)) {
          importance_data[[model_name]] <- tibble(
            feature = rownames(imp$importance),
            importance = imp$importance[, 1],
            model = model_name
          )
        }
      }
    }, error = function(e) {
      message("Could not extract feature importance for ", model_name, ": ", e$message)
    })
  }
  
  if (length(importance_data) == 0) {
    message("No feature importance data could be extracted")
    return(NULL)
  }
  
  # Combine all importance data
  combined_importance <- bind_rows(importance_data) %>%
    group_by(feature) %>%
    summarise(
      mean_importance = mean(importance, na.rm = TRUE),
      max_importance = max(importance, na.rm = TRUE),
      n_models = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_importance)) %>%
    slice_head(n = top_n)
  
  # Create visualization
  importance_plot <- ggplot(combined_importance, aes(x = reorder(feature, mean_importance), y = mean_importance)) +
    geom_col(fill = "#3498DB", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Feature Importance Analysis",
      subtitle = paste("Top", top_n, "features averaged across all models"),
      x = "Feature",
      y = "Mean Importance Score"
    ) +
    theme_professional()
  
  # Model-specific importance comparison
  model_comparison_data <- bind_rows(importance_data) %>%
    filter(feature %in% combined_importance$feature[1:min(10, nrow(combined_importance))])
  
  model_comparison_plot <- ggplot(model_comparison_data, aes(x = reorder(feature, importance), y = importance, fill = model)) +
    geom_col(position = "dodge", alpha = 0.8) +
    coord_flip() +
    scale_fill_manual(values = get_custom_palette("professional", n_distinct(model_comparison_data$model))) +
    labs(
      title = "Feature Importance by Model",
      subtitle = "Comparison of feature importance across different algorithms",
      x = "Feature",
      y = "Importance Score",
      fill = "Model"
    ) +
    theme_professional()
  
  return(list(
    importance_data = combined_importance,
    plots = list(
      overall = importance_plot,
      by_model = model_comparison_plot
    )
  ))
}

#' Run Complete Machine Learning Pipeline Demo
#'
#' @description Executes comprehensive ML pipeline demonstration
#' @param data Optional data frame (will generate sample data if not provided)
#' @param target_var Target variable name
#' @param problem_type ML problem type ("classification" or "regression")
#' @param save_outputs Logical, whether to save results
#' @param verbose Logical, whether to show detailed output
#' @return Complete MLPipeline object
#' @export
run_ml_pipeline_demo <- function(data = NULL, target_var = NULL, problem_type = "classification",
                                save_outputs = FALSE, verbose = TRUE) {
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("      MACHINE LEARNING PIPELINE PORTFOLIO\n")
    cat("="*60, "\n\n")
  }
  
  # Generate sample data if not provided
  if (is.null(data)) {
    if (verbose) cat("1. Generating sample dataset...\n")
    
    set.seed(42)
    
    if (problem_type == "classification") {
      data <- tibble(
        customer_id = 1:1000,
        age = sample(25:65, 1000, replace = TRUE),
        income = rnorm(1000, 50000, 15000),
        credit_score = sample(300:850, 1000, replace = TRUE),
        years_employed = sample(0:40, 1000, replace = TRUE),
        num_accounts = rpois(1000, 3),
        debt_ratio = runif(1000, 0, 1),
        payment_history = sample(c("Good", "Fair", "Poor"), 1000, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
        region = sample(c("North", "South", "East", "West"), 1000, replace = TRUE),
        education = sample(c("High School", "Bachelor", "Master", "PhD"), 1000, replace = TRUE, prob = c(0.4, 0.35, 0.2, 0.05))
      ) %>%
      mutate(
        # Create target variable based on logical rules
        risk_score = 0.3 * scale(income)[,1] + 
                    0.25 * scale(credit_score)[,1] + 
                    0.2 * scale(years_employed)[,1] - 
                    0.15 * debt_ratio + 
                    0.1 * (payment_history == "Good") +
                    rnorm(1000, 0, 0.3),
        loan_approved = factor(ifelse(risk_score > 0, "Yes", "No"))
      ) %>%
      select(-risk_score)  # Remove intermediate variable
      
      target_var <- "loan_approved"
      
    } else {
      # Regression dataset
      data <- tibble(
        property_id = 1:1000,
        square_feet = rnorm(1000, 2000, 500),
        bedrooms = sample(1:5, 1000, replace = TRUE, prob = c(0.1, 0.25, 0.35, 0.25, 0.05)),
        bathrooms = sample(c(1, 1.5, 2, 2.5, 3, 3.5, 4), 1000, replace = TRUE),
        age_years = sample(0:50, 1000, replace = TRUE),
        lot_size = rnorm(1000, 8000, 2000),
        neighborhood_score = runif(1000, 1, 10),
        distance_to_city = rnorm(1000, 15, 8),
        garage_spaces = sample(0:3, 1000, replace = TRUE, prob = c(0.1, 0.3, 0.5, 0.1)),
        property_type = sample(c("Single Family", "Condo", "Townhouse"), 1000, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
        school_rating = sample(1:10, 1000, replace = TRUE)
      ) %>%
      mutate(
        # Create realistic house prices
        price = 100000 + 
               50 * square_feet + 
               10000 * bedrooms + 
               8000 * bathrooms - 
               1000 * age_years + 
               5 * lot_size + 
               15000 * neighborhood_score - 
               2000 * distance_to_city + 
               5000 * garage_spaces +
               3000 * school_rating +
               rnorm(1000, 0, 25000)
      ) %>%
      filter(price > 50000)  # Remove unrealistic low prices
      
      target_var <- "price"
    }
  }
  
  # Initialize pipeline
  if (verbose) cat("2. Initializing ML pipeline...\n")
  pipeline <- create_ml_pipeline(
    data = data,
    target_var = target_var,
    problem_type = problem_type,
    test_split = 0.2,
    validation_split = 0.2,
    seed = 42
  )
  
  # Split data
  if (verbose) cat("3. Splitting data into train/validation/test sets...\n")
  pipeline <- split_data(pipeline)
  
  # Preprocess data
  if (verbose) cat("4. Preprocessing and feature engineering...\n")
  pipeline <- preprocess_data(
    pipeline,
    handle_missing = "impute",
    scale_features = TRUE,
    create_interactions = TRUE,
    encode_categorical = "dummy"
  )
  
  # Train models
  if (verbose) cat("5. Training multiple ML algorithms...\n")
  algorithms <- if (problem_type == "classification") {
    c("rf", "xgbTree", "svmRadial", "glmnet", "naive_bayes")
  } else {
    c("rf", "xgbTree", "svmRadial", "glmnet", "lm")
  }
  
  pipeline <- train_models(
    pipeline,
    algorithms = algorithms,
    cv_folds = 5,
    tune_length = 3  # Reduced for demo
  )
  
  # Evaluate models
  if (verbose) cat("6. Evaluating model performance...\n")
  pipeline <- evaluate_models(pipeline, use_validation = TRUE)
  
  # Feature importance analysis
  if (verbose) cat("7. Analyzing feature importance...\n")
  importance_results <- analyze_feature_importance(pipeline, top_n = 10)
  
  # Create visualizations
  if (verbose) cat("8. Creating performance visualizations...\n")
  performance_plots <- visualize_model_performance(pipeline)
  
  # Hyperparameter tuning for best model
  if (verbose) cat("9. Fine-tuning best model...\n")
  if (!is.null(pipeline$best_model) && pipeline$best_model %in% c("rf", "xgbTree")) {
    pipeline <- tune_hyperparameters(
      pipeline, 
      model_name = pipeline$best_model,
      search_type = "grid"
    )
    
    # Re-evaluate after tuning
    pipeline <- evaluate_models(pipeline, use_validation = FALSE)  # Use test set for final evaluation
  }
  
  # Final model performance on test set
  if (verbose) cat("10. Final evaluation on test set...\n")
  final_performance <- pipeline$performance_metrics[[pipeline$best_model]]
  
  # Create comprehensive results summary
  results_summary <- list(
    pipeline = pipeline,
    feature_importance = importance_results,
    performance_plots = performance_plots,
    
    summary_stats = list(
      total_samples = nrow(data),
      features_engineered = ncol(pipeline$train_data) - 1,
      models_trained = length(pipeline$models),
      best_model = pipeline$best_model,
      final_performance = final_performance,
      training_time = difftime(pipeline$last_updated, pipeline$created_at, units = "mins")
    )
  )
  
  # Save outputs if requested
  if (save_outputs) {
    if (!dir.exists("outputs/models")) {
      dir.create("outputs/models", recursive = TRUE)
    }
    
    # Save the pipeline
    saveRDS(pipeline, "outputs/models/ml_pipeline.rds")
    
    # Save performance plots
    if (!dir.exists("outputs/plots")) {
      dir.create("outputs/plots", recursive = TRUE)
    }
    
    for (plot_name in names(performance_plots)) {
      ggsave(
        filename = file.path("outputs/plots", paste0("ml_", plot_name, ".png")),
        plot = performance_plots[[plot_name]],
        width = 12, height = 8, dpi = 300
      )
    }
    
    if (!is.null(importance_results)) {
      for (plot_name in names(importance_results$plots)) {
        ggsave(
          filename = file.path("outputs/plots", paste0("feature_importance_", plot_name, ".png")),
          plot = importance_results$plots[[plot_name]],
          width = 12, height = 8, dpi = 300
        )
      }
    }
  }
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("    MACHINE LEARNING PIPELINE COMPLETE\n")
    cat("="*60, "\n")
    cat("Problem type:", pipeline$problem_type, "\n")
    cat("Dataset size:", nrow(data), "rows ×", ncol(data), "columns\n")
    cat("Features after engineering:", ncol(pipeline$train_data) - 1, "\n")
    cat("Models trained:", length(pipeline$models), "\n")
    cat("Best performing model:", pipeline$best_model, "\n")
    
    if (problem_type == "classification") {
      cat("Best model accuracy:", round(final_performance$accuracy, 3), "\n")
      if (!is.na(final_performance$auc)) {
        cat("Best model AUC:", round(final_performance$auc, 3), "\n")
      }
    } else {
      cat("Best model R²:", round(final_performance$r_squared, 3), "\n")
      cat("Best model RMSE:", round(final_performance$rmse, 0), "\n")
    }
    
    cat("Total processing time:", round(as.numeric(results_summary$summary_stats$training_time), 1), "minutes\n")
    
    if (save_outputs) {
      cat("Results saved to outputs/ directory\n")
    }
  }
  
  return(results_summary)
}

# =============================================================================
# ADVANCED ML TECHNIQUES
# =============================================================================

#' Ensemble Model Creation
#'
#' @description Creates ensemble models combining multiple base learners
#' @param pipeline MLPipeline object
#' @param ensemble_method Method for combining models ("voting", "stacking")
#' @return Updated MLPipeline object
#' @export
create_ensemble <- function(pipeline, ensemble_method = "voting") {
  
  if (length(pipeline$models) < 2) {
    stop("Need at least 2 models for ensemble creation")
  }
  
  # Prepare data
  feature_cols <- setdiff(names(pipeline$train_data), pipeline$target_var)
  X_train <- pipeline$train_data[feature_cols]
  y_train <- pipeline$train_data[[pipeline$target_var]]
  
  if (ensemble_method == "voting") {
    # Create voting ensemble
    model_list <- pipeline$models[1:min(3, length(pipeline$models))]  # Use top 3 models
    
    if (pipeline$problem_type == "classification") {
      # For classification: majority voting
      ensemble_preds <- map_dfc(model_list, function(model) {
        as.character(predict(model, X_train))
      })
      
      # Majority vote
      ensemble_result <- apply(ensemble_preds, 1, function(x) {
        names(sort(table(x), decreasing = TRUE))[1]
      })
      
    } else {
      # For regression: average predictions
      ensemble_preds <- map_dfc(model_list, function(model) {
        predict(model, X_train)
      })
      
      ensemble_result <- rowMeans(ensemble_preds, na.rm = TRUE)
    }
    
    # Create a simple ensemble "model" (really just a function)
    ensemble_model <- list(
      method = "ensemble_voting",
      models = model_list,
      type = pipeline$problem_type
    )
    class(ensemble_model) <- "ensemble_model"
    
  } else if (ensemble_method == "stacking") {
    # Create stacking ensemble using cross-validation
    
    # Generate out-of-fold predictions for each model
    cv_folds <- 5
    fold_indices <- createFolds(y_train, k = cv_folds)
    
    meta_features <- matrix(0, nrow = nrow(X_train), ncol = length(pipeline$models))
    colnames(meta_features) <- names(pipeline$models)
    
    for (i in 1:cv_folds) {
      train_idx <- setdiff(1:nrow(X_train), fold_indices[[i]])
      val_idx <- fold_indices[[i]]
      
      for (j in seq_along(pipeline$models)) {
        model_name <- names(pipeline$models)[j]
        model <- pipeline$models[[model_name]]
        
        # Retrain on fold training data
        temp_model <- train(
          x = X_train[train_idx, ],
          y = y_train[train_idx],
          method = model$method,
          trControl = trainControl(method = "none"),
          tuneGrid = model$bestTune
        )
        
        # Predict on validation fold
        if (pipeline$problem_type == "classification") {
          meta_features[val_idx, j] <- as.numeric(predict(temp_model, X_train[val_idx, ]))
        } else {
          meta_features[val_idx, j] <- predict(temp_model, X_train[val_idx, ])
        }
      }
    }
    
    # Train meta-learner
    meta_data <- data.frame(meta_features)
    meta_data[[pipeline$target_var]] <- y_train
    
    if (pipeline$problem_type == "classification") {
      meta_learner <- train(
        x = meta_data[, -ncol(meta_data)],
        y = meta_data[[pipeline$target_var]],
        method = "glmnet",
        trControl = trainControl(method = "cv", number = 3)
      )
    } else {
      meta_learner <- train(
        x = meta_data[, -ncol(meta_data)],
        y = meta_data[[pipeline$target_var]],
        method = "lm",
        trControl = trainControl(method = "cv", number = 3)
      )
    }
    
    ensemble_model <- list(
      method = "ensemble_stacking",
      base_models = pipeline$models,
      meta_learner = meta_learner,
      type = pipeline$problem_type
    )
    class(ensemble_model) <- "ensemble_model"
  }
  
  # Add ensemble to pipeline
  pipeline$models[[paste0("ensemble_", ensemble_method)]] <- ensemble_model
  
  message("Ensemble model created using ", ensemble_method, " method")
  
  return(pipeline)
}

#' Automated Feature Selection
#'
#' @description Performs automated feature selection using multiple methods
#' @param pipeline MLPipeline object
#' @param selection_method Method for feature selection ("rfe", "boruta", "univariate")
#' @param max_features Maximum number of features to select
#' @return Updated MLPipeline object with selected features
#' @export
automated_feature_selection <- function(pipeline, selection_method = "rfe", max_features = 20) {
  
  # Prepare data
  feature_cols <- setdiff(names(pipeline$train_data), pipeline$target_var)
  X_train <- pipeline$train_data[feature_cols]
  y_train <- pipeline$train_data[[pipeline$target_var]]
  
  if (selection_method == "rfe") {
    # Recursive Feature Elimination
    control <- rfeControl(
      functions = if (pipeline$problem_type == "classification") rfFuncs else lmFuncs,
      method = "cv",
      number = 5
    )
    
    rfe_results <- rfe(
      x = X_train,
      y = y_train,
      sizes = c(5, 10, 15, min(max_features, ncol(X_train))),
      rfeControl = control
    )
    
    selected_features <- rfe_results$optVariables
    selection_info <- list(
      method = "rfe",
      selected_features = selected_features,
      performance = rfe_results$results
    )
    
  } else if (selection_method == "univariate") {
    # Univariate feature selection
    if (pipeline$problem_type == "classification") {
      # Chi-square test for categorical, t-test for numeric
      feature_scores <- numeric(ncol(X_train))
      names(feature_scores) <- names(X_train)
      
      for (i in seq_along(feature_scores)) {
        if (is.numeric(X_train[[i]])) {
          # t-test
          test_result <- t.test(X_train[[i]] ~ y_train)
          feature_scores[i] <- abs(test_result$statistic)
        } else {
          # Chi-square test
          contingency_table <- table(X_train[[i]], y_train)
          if (all(dim(contingency_table) > 1)) {
            test_result <- chisq.test(contingency_table)
            feature_scores[i] <- test_result$statistic
          } else {
            feature_scores[i] <- 0
          }
        }
      }
    } else {
      # Correlation for regression
      feature_scores <- abs(cor(X_train, as.numeric(y_train), use = "complete.obs"))
      feature_scores[is.na(feature_scores)] <- 0
    }
    
    # Select top features
    selected_features <- names(sort(feature_scores, decreasing = TRUE)[1:min(max_features, length(feature_scores))])
    
    selection_info <- list(
      method = "univariate",
      selected_features = selected_features,
      feature_scores = feature_scores
    )
  }
  
  # Update pipeline with selected features
  pipeline$selected_features <- selection_info
  
  # Create new datasets with selected features only
  all_selected <- c(selected_features, pipeline$target_var)
  
  pipeline$train_data_selected <- pipeline$train_data[all_selected]
  if (!is.null(pipeline$validation_data)) {
    pipeline$validation_data_selected <- pipeline$validation_data[all_selected]
  }
  pipeline$test_data_selected <- pipeline$test_data[all_selected]
  
  message("Feature selection completed using ", selection_method)
  message("Selected ", length(selected_features), " features out of ", ncol(X_train))
  
  return(pipeline)
}

# =============================================================================
# MODEL DEPLOYMENT UTILITIES
# =============================================================================

#' Prepare Model for Production Deployment
#'
#' @description Packages model with preprocessing steps for deployment
#' @param pipeline MLPipeline object
#' @param model_name Name of model to deploy (default: best model)
#' @return Deployment package list
#' @export
prepare_for_deployment <- function(pipeline, model_name = NULL) {
  
  if (is.null(model_name)) {
    model_name <- pipeline$best_model
  }
  
  if (is.null(model_name) || !model_name %in% names(pipeline$models)) {
    stop("Model not found for deployment")
  }
  
  model <- pipeline$models[[model_name]]
  
  # Create deployment package
  deployment_package <- list(
    model = model,
    preprocessor = pipeline$preprocessor,
    feature_names = pipeline$feature_names,
    target_var = pipeline$target_var,
    problem_type = pipeline$problem_type,
    
    # Metadata
    model_name = model_name,
    training_date = pipeline$last_updated,
    performance_metrics = pipeline$performance_metrics[[model_name]],
    
    # Prediction function
    predict_new = function(new_data) {
      # Apply same preprocessing as training
      processed_data <- new_data  # Would apply preprocessing steps here
      
      # Make prediction
      if (pipeline$problem_type == "classification") {
        list(
          predicted_class = predict(model, processed_data),
          predicted_probabilities = predict(model, processed_data, type = "prob")
        )
      } else {
        list(
          predicted_value = predict(model, processed_data)
        )
      }
    }
  )
  
  class(deployment_package) <- "MLDeployment"
  
  return(deployment_package)
}

# =============================================================================
# EXAMPLE USAGE AND TESTING
# =============================================================================

if (interactive()) {
  # Run classification demo
  # classification_results <- run_ml_pipeline_demo(
  #   problem_type = "classification",
  #   save_outputs = TRUE,
  #   verbose = TRUE
  # )
  
  # Run regression demo
  # regression_results <- run_ml_pipeline_demo(
  #   problem_type = "regression", 
  #   save_outputs = TRUE,
  #   verbose = TRUE
  # )
  
  # Access results
  # print(classification_results$pipeline)
  # classification_results$performance_plots$metrics_comparison
  # classification_results$feature_importance$plots$overall
}
# @title Comprehensive Supervised Learning Framework
# @description Advanced supervised learning algorithms, evaluation, and optimization
# @author R Data Science Portfolio Developer
# @date 2025

#' ========================================
#' COMPREHENSIVE SUPERVISED LEARNING
#' ========================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, ggplot2, caret, randomForest, xgboost, e1071, nnet,
  rpart, rpart.plot, ROCR, pROC, corrplot, VIM, mice,
  ModelMetrics, lime, DALEX, iml, plotly, DT, kableExtra
)

#' ========================================
#' 1. DATA PREPROCESSING PIPELINE
#' ========================================

#' Comprehensive Data Preprocessing
#' @param data Input dataset
#' @param target_var Target variable name
#' @param test_size Proportion for test set (default 0.2)
#' @param validation_size Proportion for validation set (default 0.2)
#' @return Preprocessed data splits and preprocessing info
preprocess_supervised_data <- function(data, target_var, test_size = 0.2, validation_size = 0.2) {
  
  # Check for missing values
  missing_summary <- data %>%
    summarise_all(~sum(is.na(.))) %>%
    tidyr::gather(variable, missing_count) %>%
    mutate(missing_pct = missing_count / nrow(data) * 100) %>%
    arrange(desc(missing_count))
  
  # Handle missing values (simple approach - can be enhanced)
  # Remove columns with >50% missing
  high_missing_cols <- missing_summary$variable[missing_summary$missing_pct > 50]
  if (length(high_missing_cols) > 0) {
    data <- data %>% select(-all_of(high_missing_cols))
    warning(paste("Removed columns with >50% missing:", paste(high_missing_cols, collapse = ", ")))
  }
  
  # For remaining missing values, use median/mode imputation
  numeric_cols <- sapply(data, is.numeric)
  data[numeric_cols] <- data[numeric_cols] %>%
    mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))
  
  character_cols <- sapply(data, function(x) is.character(x) | is.factor(x))
  if (any(character_cols)) {
    data[character_cols] <- data[character_cols] %>%
      mutate_all(~ifelse(is.na(.), mode_stat(.), as.character(.)))
  }
  
  # Identify variable types
  target <- data[[target_var]]
  features <- data %>% select(-all_of(target_var))
  
  # Determine problem type
  problem_type <- if (is.numeric(target)) {
    if (length(unique(target)) <= 10) "classification" else "regression"
  } else {
    "classification"
  }
  
  # Prepare target variable
  if (problem_type == "classification") {
    target <- as.factor(target)
    data[[target_var]] <- target
  }
  
  # Feature engineering
  # Create dummy variables for categorical features
  categorical_vars <- names(features)[sapply(features, function(x) is.character(x) | is.factor(x))]
  
  if (length(categorical_vars) > 0) {
    # Use model.matrix for dummy encoding
    dummy_formula <- as.formula(paste("~", paste(categorical_vars, collapse = " + ")))
    dummy_matrix <- model.matrix(dummy_formula, data = features)[, -1] # Remove intercept
    
    # Combine with numeric features
    numeric_features <- features %>% select(-all_of(categorical_vars))
    features_processed <- cbind(numeric_features, dummy_matrix)
  } else {
    features_processed <- features
  }
  
  # Scale numeric features
  numeric_indices <- sapply(features_processed, is.numeric)
  if (any(numeric_indices)) {
    scaling_params <- list(
      center = sapply(features_processed[numeric_indices], mean, na.rm = TRUE),
      scale = sapply(features_processed[numeric_indices], sd, na.rm = TRUE)
    )
    features_processed[numeric_indices] <- scale(features_processed[numeric_indices])
  } else {
    scaling_params <- NULL
  }
  
  # Combine target and features
  processed_data <- cbind(features_processed, target = target)
  
  # Data splitting
  set.seed(42)
  n <- nrow(processed_data)
  
  # Create indices
  test_indices <- sample(1:n, size = floor(test_size * n))
  remaining_data <- processed_data[-test_indices, ]
  
  val_indices <- sample(1:nrow(remaining_data), size = floor(validation_size * nrow(remaining_data)))
  train_indices <- setdiff(1:nrow(remaining_data), val_indices)
  
  # Split data
  train_data <- remaining_data[train_indices, ]
  val_data <- remaining_data[val_indices, ]
  test_data <- processed_data[test_indices, ]
  
  return(list(
    train = train_data,
    validation = val_data,
    test = test_data,
    original_data = data,
    problem_type = problem_type,
    target_var = target_var,
    feature_names = names(features_processed),
    categorical_vars = categorical_vars,
    scaling_params = scaling_params,
    missing_summary = missing_summary
  ))
}

# Helper function for mode calculation
mode_stat <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

#' ========================================
#' 2. MODEL TRAINING FRAMEWORK
#' ========================================

#' Train Multiple Supervised Learning Models
#' @param data_splits Output from preprocess_supervised_data
#' @return Trained models and performance metrics
train_supervised_models <- function(data_splits) {
  
  train_data <- data_splits$train
  val_data <- data_splits$validation
  problem_type <- data_splits$problem_type
  
  # Prepare formula
  feature_names <- setdiff(names(train_data), "target")
  model_formula <- as.formula(paste("target ~", paste(feature_names, collapse = " + ")))
  
  models <- list()
  
  if (problem_type == "classification") {
    
    # 1. Logistic Regression
    cat("Training Logistic Regression...\n")
    models$logistic <- glm(model_formula, data = train_data, family = binomial)
    
    # 2. Random Forest
    cat("Training Random Forest...\n")
    models$random_forest <- randomForest(model_formula, data = train_data, 
                                        ntree = 500, importance = TRUE)
    
    # 3. Support Vector Machine
    cat("Training SVM...\n")
    models$svm <- svm(model_formula, data = train_data, probability = TRUE)
    
    # 4. Neural Network
    cat("Training Neural Network...\n")
    models$neural_net <- nnet(model_formula, data = train_data, size = 10, 
                             decay = 0.1, maxit = 200, trace = FALSE)
    
    # 5. Decision Tree
    cat("Training Decision Tree...\n")
    models$decision_tree <- rpart(model_formula, data = train_data, method = "class")
    
    # 6. XGBoost
    cat("Training XGBoost...\n")
    train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, feature_names]), 
                               label = as.numeric(train_data$target) - 1)
    
    models$xgboost <- xgboost(data = train_matrix, 
                             nrounds = 100, 
                             objective = "binary:logistic",
                             verbose = 0)
    
  } else {  # Regression
    
    # 1. Linear Regression
    cat("Training Linear Regression...\n")
    models$linear <- lm(model_formula, data = train_data)
    
    # 2. Random Forest
    cat("Training Random Forest...\n")
    models$random_forest <- randomForest(model_formula, data = train_data, 
                                        ntree = 500, importance = TRUE)
    
    # 3. Support Vector Regression
    cat("Training SVR...\n")
    models$svr <- svm(model_formula, data = train_data)
    
    # 4. Neural Network
    cat("Training Neural Network...\n")
    models$neural_net <- nnet(model_formula, data = train_data, size = 10, 
                             decay = 0.1, maxit = 200, trace = FALSE, linout = TRUE)
    
    # 5. XGBoost
    cat("Training XGBoost...\n")
    train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, feature_names]), 
                               label = train_data$target)
    
    models$xgboost <- xgboost(data = train_matrix, 
                             nrounds = 100, 
                             objective = "reg:squarederror",
                             verbose = 0)
  }
  
  return(list(
    models = models,
    data_splits = data_splits,
    model_formula = model_formula
  ))
}

#' ========================================
#' 3. MODEL EVALUATION FRAMEWORK
#' ========================================

#' Evaluate Supervised Learning Models
#' @param trained_models Output from train_supervised_models
#' @return Comprehensive evaluation results
evaluate_supervised_models <- function(trained_models) {
  
  models <- trained_models$models
  data_splits <- trained_models$data_splits
  model_formula <- trained_models$model_formula
  
  train_data <- data_splits$train
  val_data <- data_splits$validation
  test_data <- data_splits$test
  problem_type <- data_splits$problem_type
  feature_names <- setdiff(names(train_data), "target")
  
  # Initialize results storage
  results <- list()
  performance_summary <- data.frame()
  
  for (model_name in names(models)) {
    cat("Evaluating", model_name, "...\n")
    
    model <- models[[model_name]]
    model_results <- list()
    
    if (problem_type == "classification") {
      
      # Make predictions
      if (model_name == "xgboost") {
        val_matrix <- xgb.DMatrix(data = as.matrix(val_data[, feature_names]))
        test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, feature_names]))
        
        val_probs <- predict(model, val_matrix)
        test_probs <- predict(model, test_matrix)
        val_pred <- ifelse(val_probs > 0.5, levels(val_data$target)[2], levels(val_data$target)[1])
        test_pred <- ifelse(test_probs > 0.5, levels(test_data$target)[2], levels(test_data$target)[1])
      } else if (model_name == "neural_net") {
        val_probs <- predict(model, val_data[, feature_names], type = "raw")
        test_probs <- predict(model, test_data[, feature_names], type = "raw")
        val_pred <- ifelse(val_probs > 0.5, levels(val_data$target)[2], levels(val_data$target)[1])
        test_pred <- ifelse(test_probs > 0.5, levels(test_data$target)[2], levels(test_data$target)[1])
      } else if (model_name == "logistic") {
        val_probs <- predict(model, val_data, type = "response")
        test_probs <- predict(model, test_data, type = "response")
        val_pred <- ifelse(val_probs > 0.5, levels(val_data$target)[2], levels(val_data$target)[1])
        test_pred <- ifelse(test_probs > 0.5, levels(test_data$target)[2], levels(test_data$target)[1])
      } else {
        val_pred <- predict(model, val_data)
        test_pred <- predict(model, test_data)
        
        if (model_name == "svm") {
          val_probs <- attr(predict(model, val_data, probability = TRUE), "probabilities")[, 2]
          test_probs <- attr(predict(model, test_data, probability = TRUE), "probabilities")[, 2]
        } else if (model_name == "random_forest") {
          val_probs <- predict(model, val_data, type = "prob")[, 2]
          test_probs <- predict(model, test_data, type = "prob")[, 2]
        } else {
          val_probs <- as.numeric(val_pred == levels(val_data$target)[2])
          test_probs <- as.numeric(test_pred == levels(test_data$target)[2])
        }
      }
      
      # Calculate metrics
      val_accuracy <- mean(val_pred == val_data$target)
      test_accuracy <- mean(test_pred == test_data$target)
      
      # Confusion matrices
      val_cm <- table(Predicted = val_pred, Actual = val_data$target)
      test_cm <- table(Predicted = test_pred, Actual = test_data$target)
      
      # AUC calculation
      if (length(unique(val_data$target)) == 2) {
        val_auc <- tryCatch(auc(as.numeric(val_data$target) - 1, val_probs), error = function(e) NA)
        test_auc <- tryCatch(auc(as.numeric(test_data$target) - 1, test_probs), error = function(e) NA)
      } else {
        val_auc <- NA
        test_auc <- NA
      }
      
      # Store results
      model_results <- list(
        validation = list(
          predictions = val_pred,
          probabilities = val_probs,
          accuracy = val_accuracy,
          auc = val_auc,
          confusion_matrix = val_cm
        ),
        test = list(
          predictions = test_pred,
          probabilities = test_probs,
          accuracy = test_accuracy,
          auc = test_auc,
          confusion_matrix = test_cm
        )
      )
      
      # Add to performance summary
      performance_summary <- rbind(performance_summary, data.frame(
        Model = model_name,
        Val_Accuracy = round(val_accuracy, 4),
        Test_Accuracy = round(test_accuracy, 4),
        Val_AUC = round(val_auc, 4),
        Test_AUC = round(test_auc, 4),
        stringsAsFactors = FALSE
      ))
      
    } else {  # Regression
      
      # Make predictions
      if (model_name == "xgboost") {
        val_matrix <- xgb.DMatrix(data = as.matrix(val_data[, feature_names]))
        test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, feature_names]))
        
        val_pred <- predict(model, val_matrix)
        test_pred <- predict(model, test_matrix)
      } else {
        val_pred <- predict(model, val_data)
        test_pred <- predict(model, test_data)
      }
      
      # Calculate metrics
      val_rmse <- sqrt(mean((val_pred - val_data$target)^2))
      test_rmse <- sqrt(mean((test_pred - test_data$target)^2))
      
      val_mae <- mean(abs(val_pred - val_data$target))
      test_mae <- mean(abs(test_pred - test_data$target))
      
      val_r2 <- cor(val_pred, val_data$target)^2
      test_r2 <- cor(test_pred, test_data$target)^2
      
      # Store results
      model_results <- list(
        validation = list(
          predictions = val_pred,
          rmse = val_rmse,
          mae = val_mae,
          r2 = val_r2
        ),
        test = list(
          predictions = test_pred,
          rmse = test_rmse,
          mae = test_mae,
          r2 = test_r2
        )
      )
      
      # Add to performance summary
      performance_summary <- rbind(performance_summary, data.frame(
        Model = model_name,
        Val_RMSE = round(val_rmse, 4),
        Test_RMSE = round(test_rmse, 4),
        Val_MAE = round(val_mae, 4),
        Test_MAE = round(test_mae, 4),
        Val_R2 = round(val_r2, 4),
        Test_R2 = round(test_r2, 4),
        stringsAsFactors = FALSE
      ))
    }
    
    results[[model_name]] <- model_results
  }
  
  return(list(
    results = results,
    performance_summary = performance_summary,
    problem_type = problem_type,
    data_splits = data_splits
  ))
}

#' ========================================
#' 4. FEATURE IMPORTANCE ANALYSIS
#' ========================================

#' Extract Feature Importance from Models
#' @param trained_models Output from train_supervised_models
#' @return Feature importance results
analyze_feature_importance <- function(trained_models) {
  
  models <- trained_models$models
  data_splits <- trained_models$data_splits
  feature_names <- setdiff(names(data_splits$train), "target")
  
  importance_results <- list()
  
  for (model_name in names(models)) {
    model <- models[[model_name]]
    
    if (model_name == "random_forest") {
      # Random Forest importance
      importance_df <- data.frame(
        feature = rownames(importance(model)),
        importance = importance(model)[, "MeanDecreaseGini"],
        stringsAsFactors = FALSE
      )
      
    } else if (model_name == "xgboost") {
      # XGBoost importance
      importance_matrix <- xgb.importance(feature_names = feature_names, model = model)
      importance_df <- data.frame(
        feature = importance_matrix$Feature,
        importance = importance_matrix$Gain,
        stringsAsFactors = FALSE
      )
      
    } else if (model_name == "logistic" && data_splits$problem_type == "classification") {
      # Logistic regression coefficients
      coef_summary <- summary(model)$coefficients
      importance_df <- data.frame(
        feature = rownames(coef_summary)[-1],  # Remove intercept
        importance = abs(coef_summary[-1, "Estimate"]),
        stringsAsFactors = FALSE
      )
      
    } else if (model_name == "linear" && data_splits$problem_type == "regression") {
      # Linear regression coefficients
      coef_summary <- summary(model)$coefficients
      importance_df <- data.frame(
        feature = rownames(coef_summary)[-1],  # Remove intercept
        importance = abs(coef_summary[-1, "Estimate"]),
        stringsAsFactors = FALSE
      )
      
    } else {
      # For other models, create placeholder
      importance_df <- data.frame(
        feature = feature_names,
        importance = rep(NA, length(feature_names)),
        stringsAsFactors = FALSE
      )
    }
    
    # Normalize importance scores
    if (!all(is.na(importance_df$importance))) {
      importance_df$importance_norm <- importance_df$importance / max(importance_df$importance, na.rm = TRUE)
    } else {
      importance_df$importance_norm <- NA
    }
    
    importance_results[[model_name]] <- importance_df
  }
  
  # Create combined importance plot
  combined_importance <- do.call(rbind, lapply(names(importance_results), function(name) {
    df <- importance_results[[name]]
    df$model <- name
    return(df)
  }))
  
  # Filter out models with no importance scores
  combined_importance <- combined_importance[!is.na(combined_importance$importance), ]
  
  if (nrow(combined_importance) > 0) {
    importance_plot <- ggplot(combined_importance, aes(x = reorder(feature, importance_norm), 
                                                      y = importance_norm, fill = model)) +
      geom_col(position = "dodge", alpha = 0.8) +
      coord_flip() +
      labs(title = "Feature Importance Comparison",
           x = "Features", y = "Normalized Importance", fill = "Model") +
      theme_minimal() +
      theme(legend.position = "bottom")
  } else {
    importance_plot <- ggplot() + labs(title = "Feature importance not available for selected models")
  }
  
  return(list(
    importance_by_model = importance_results,
    combined_data = combined_importance,
    importance_plot = importance_plot
  ))
}

#' ========================================
#' 5. HYPERPARAMETER TUNING
#' ========================================

#' Hyperparameter Tuning for Selected Models
#' @param data_splits Output from preprocess_supervised_data
#' @param model_type Type of model to tune ("random_forest", "svm", "xgboost")
#' @param cv_folds Number of cross-validation folds
#' @return Tuned model and best parameters
tune_hyperparameters <- function(data_splits, model_type = "random_forest", cv_folds = 5) {
  
  train_data <- data_splits$train
  problem_type <- data_splits$problem_type
  feature_names <- setdiff(names(train_data), "target")
  
  # Set up cross-validation
  if (problem_type == "classification") {
    train_control <- trainControl(method = "cv", number = cv_folds, 
                                 classProbs = TRUE, summaryFunction = twoClassSummary)
    metric <- "ROC"
  } else {
    train_control <- trainControl(method = "cv", number = cv_folds)
    metric <- "RMSE"
  }
  
  # Define parameter grids
  if (model_type == "random_forest") {
    param_grid <- expand.grid(
      mtry = c(2, sqrt(length(feature_names)), length(feature_names)/3),
      splitrule = "gini",
      min.node.size = c(1, 5, 10)
    )
    method <- "ranger"
    
  } else if (model_type == "svm") {
    param_grid <- expand.grid(
      C = c(0.1, 1, 10, 100),
      sigma = c(0.001, 0.01, 0.1, 1)
    )
    method <- "svmRadial"
    
  } else if (model_type == "xgboost") {
    param_grid <- expand.grid(
      nrounds = c(50, 100, 200),
      max_depth = c(3, 6, 9),
      eta = c(0.1, 0.3, 0.5),
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 1
    )
    method <- "xgbTree"
  }
  
  # Prepare formula
  model_formula <- as.formula(paste("target ~", paste(feature_names, collapse = " + ")))
  
  # Perform tuning
  set.seed(42)
  tuned_model <- train(model_formula, 
                      data = train_data,
                      method = method,
                      trControl = train_control,
                      tuneGrid = param_grid,
                      metric = metric)
  
  return(tuned_model)
}

#' ========================================
#' 6. MODEL INTERPRETABILITY
#' ========================================

#' LIME Explanations for Model Predictions
#' @param trained_models Output from train_supervised_models
#' @param model_name Name of model to explain
#' @param instance_index Index of instance to explain
#' @return LIME explanation results
explain_prediction_lime <- function(trained_models, model_name, instance_index = 1) {
  
  models <- trained_models$models
  data_splits <- trained_models$data_splits
  
  model <- models[[model_name]]
  test_data <- data_splits$test
  feature_names <- setdiff(names(test_data), "target")
  
  # Prepare data for LIME
  x_test <- test_data[, feature_names]
  instance_to_explain <- x_test[instance_index, ]
  
  # Create explainer
  explainer <- lime(x_test, model)
  
  # Generate explanation
  explanation <- explain(instance_to_explain, explainer, n_features = min(5, length(feature_names)))
  
  # Create plot
  explanation_plot <- plot_features(explanation)
  
  return(list(
    explanation = explanation,
    plot = explanation_plot,
    instance = instance_to_explain
  ))
}

#' ========================================
#' 7. DEMONSTRATION WITH SYNTHETIC DATA
#' ========================================

#' Generate Supervised Learning Demo Data
generate_supervised_demo_data <- function(n = 1000, problem_type = "classification") {
  
  set.seed(42)
  
  # Generate features
  age <- rnorm(n, 35, 10)
  income <- rnorm(n, 50000, 15000)
  education_years <- sample(8:20, n, replace = TRUE)
  experience <- pmax(0, age - education_years - 6 + rnorm(n, 0, 2))
  
  # Categorical features
  gender <- sample(c("Male", "Female"), n, replace = TRUE)
  region <- sample(c("North", "South", "East", "West"), n, replace = TRUE, 
                  prob = c(0.3, 0.25, 0.25, 0.2))
  
  if (problem_type == "classification") {
    # Binary classification target
    linear_combination <- 0.02 * age + 0.00001 * income + 0.1 * education_years + 
                         0.05 * experience + ifelse(gender == "Male", 0.3, 0) +
                         ifelse(region == "North", 0.2, 0) + rnorm(n, 0, 0.5)
    
    target <- ifelse(linear_combination > median(linear_combination), "High", "Low")
    
  } else {
    # Regression target (salary prediction)
    target <- 25000 + 500 * age + 0.8 * income + 2000 * education_years + 
             1000 * experience + ifelse(gender == "Male", 5000, 0) +
             ifelse(region == "North", 3000, 0) + rnorm(n, 0, 5000)
  }
  
  # Add some missing values
  missing_indices <- sample(1:n, floor(0.05 * n))
  income[missing_indices] <- NA
  
  data <- data.frame(
    age = age,
    income = income,
    education_years = education_years,
    experience = experience,
    gender = gender,
    region = region,
    target = target,
    stringsAsFactors = FALSE
  )
  
  return(data)
}

#' Run Complete Supervised Learning Demo
demo_supervised_learning <- function(problem_type = "classification") {
  
  cat("=== COMPREHENSIVE SUPERVISED LEARNING DEMONSTRATION ===\n\n")
  cat("Problem Type:", problem_type, "\n\n")
  
  # Generate demo data
  demo_data <- generate_supervised_demo_data(n = 1000, problem_type = problem_type)
  
  cat("1. DATA PREPROCESSING\n")
  cat("="*50, "\n")
  
  # Preprocess data
  data_splits <- preprocess_supervised_data(demo_data, "target")
  
  cat("Data splits:\n")
  cat("Training set:", nrow(data_splits$train), "samples\n")
  cat("Validation set:", nrow(data_splits$validation), "samples\n")
  cat("Test set:", nrow(data_splits$test), "samples\n")
  cat("Problem type:", data_splits$problem_type, "\n")
  
  cat("\n2. MODEL TRAINING\n")
  cat("="*50, "\n")
  
  # Train models
  trained_models <- train_supervised_models(data_splits)
  
  cat("Trained models:", paste(names(trained_models$models), collapse = ", "), "\n")
  
  cat("\n3. MODEL EVALUATION\n")
  cat("="*50, "\n")
  
  # Evaluate models
  evaluation_results <- evaluate_supervised_models(trained_models)
  
  cat("Model Performance Summary:\n")
  print(evaluation_results$performance_summary)
  
  cat("\n4. FEATURE IMPORTANCE ANALYSIS\n")
  cat("="*50, "\n")
  
  # Analyze feature importance
  importance_results <- analyze_feature_importance(trained_models)
  
  if (nrow(importance_results$combined_data) > 0) {
    cat("Top 5 most important features:\n")
    top_features <- importance_results$combined_data %>%
      group_by(feature) %>%
      summarise(avg_importance = mean(importance_norm, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(avg_importance)) %>%
      head(5)
    print(top_features)
  }
  
  cat("\n5. HYPERPARAMETER TUNING (Random Forest)\n")
  cat("="*50, "\n")
  
  # Tune hyperparameters
  tuned_rf <- tune_hyperparameters(data_splits, "random_forest", cv_folds = 3)
  
  cat("Best Random Forest parameters:\n")
  print(tuned_rf$bestTune)
  cat("Best CV performance:", round(max(tuned_rf$results$ROC, na.rm = TRUE), 4), "\n")
  
  cat("\nSupervised Learning Demo Complete!\n")
  
  return(list(
    demo_data = demo_data,
    data_splits = data_splits,
    trained_models = trained_models,
    evaluation = evaluation_results,
    importance = importance_results,
    tuned_model = tuned_rf
  ))
}

# Export key functions
supervised_learning_exports <- list(
  preprocess_supervised_data = preprocess_supervised_data,
  train_supervised_models = train_supervised_models,
  evaluate_supervised_models = evaluate_supervised_models,
  analyze_feature_importance = analyze_feature_importance,
  tune_hyperparameters = tune_hyperparameters,
  explain_prediction_lime = explain_prediction_lime,
  generate_supervised_demo_data = generate_supervised_demo_data,
  demo_supervised_learning = demo_supervised_learning
)

# Run demo if script is executed directly
if (sys.nframe() == 0) {
  # Run both classification and regression demos
  classification_results <- demo_supervised_learning("classification")
  cat("\n" * 3)
  regression_results <- demo_supervised_learning("regression")
}
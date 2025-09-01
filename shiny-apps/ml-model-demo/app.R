# ML Model Demo Shiny App
# Interactive Machine Learning Model Training and Prediction Interface

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(e1071)
library(ROCR)
library(pROC)
library(lime)
library(corrplot)
library(viridis)

# Source custom functions if available
if (file.exists("../../R/utils/data-generators.R")) {
  source("../../R/utils/data-generators.R")
}

# Define UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Interactive ML Model Demo"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data & Features", tabName = "data", icon = icon("database")),
      menuItem("Model Training", tabName = "training", icon = icon("cogs")),
      menuItem("Model Evaluation", tabName = "evaluation", icon = icon("chart-line")),
      menuItem("Predictions", tabName = "predictions", icon = icon("magic")),
      menuItem("Model Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Feature Importance", tabName = "importance", icon = icon("list-ol")),
      menuItem("Model Explainability", tabName = "explainability", icon = icon("lightbulb"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          border-radius: 10px;
          border-top: 3px solid #3498db;
        }
        .box-header {
          background: linear-gradient(45deg, #3498db, #2980b9);
          color: white;
          border-radius: 7px 7px 0 0;
        }
        .btn-primary {
          background: linear-gradient(45deg, #3498db, #2980b9);
          border: none;
          border-radius: 25px;
          padding: 8px 20px;
        }
        .btn-success {
          background: linear-gradient(45deg, #27ae60, #229954);
          border: none;
          border-radius: 25px;
          padding: 8px 20px;
        }
        .info-box {
          border-radius: 10px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      # Data & Features Tab
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Dataset Configuration", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("problem_type", "Problem Type:",
                       choices = list(
                         "Binary Classification" = "binary",
                         "Multi-class Classification" = "multiclass",
                         "Regression" = "regression"
                       )),
            
            selectInput("dataset_type", "Dataset:",
                       choices = list(
                         "Customer Churn" = "churn",
                         "Sales Prediction" = "sales",
                         "Iris Classification" = "iris",
                         "Boston Housing" = "boston",
                         "Custom Generated" = "custom"
                       )),
            
            conditionalPanel(
              condition = "input.dataset_type == 'custom'",
              numericInput("n_samples", "Number of Samples:", value = 1000, min = 100, max = 5000),
              numericInput("n_features", "Number of Features:", value = 10, min = 3, max = 20),
              numericInput("noise_level", "Noise Level:", value = 0.1, min = 0, max = 0.5, step = 0.1)
            ),
            
            br(),
            actionButton("load_dataset", "Load Dataset", class = "btn-primary btn-lg"),
            br(), br(),
            
            conditionalPanel(
              condition = "output.data_loaded",
              h4("Data Split Configuration:"),
              sliderInput("train_split", "Training Set %:", 
                         min = 50, max = 90, value = 70, step = 5),
              br(),
              actionButton("split_data", "Split Data", class = "btn-success")
            )
          ),
          
          box(
            title = "Dataset Overview", status = "info", solidHeader = TRUE, width = 8,
            DT::dataTableOutput("dataset_preview"),
            br(),
            verbatimTextOutput("dataset_info")
          )
        ),
        
        fluidRow(
          box(
            title = "Feature Correlation Matrix", status = "warning", solidHeader = TRUE, width = 6,
            plotOutput("correlation_plot", height = "400px")
          ),
          
          box(
            title = "Target Variable Distribution", status = "success", solidHeader = TRUE, width = 6,
            plotlyOutput("target_distribution", height = "400px")
          )
        )
      ),
      
      # Model Training Tab
      tabItem(tabName = "training",
        fluidRow(
          box(
            title = "Algorithm Selection", status = "primary", solidHeader = TRUE, width = 4,
            checkboxGroupInput("algorithms", "Select Algorithms:",
                              choices = list(
                                "Random Forest" = "rf",
                                "Support Vector Machine" = "svm",
                                "Logistic Regression" = "glm",
                                "Naive Bayes" = "nb",
                                "k-Nearest Neighbors" = "knn"
                              ),
                              selected = c("rf", "svm")),
            
            h4("Cross-Validation Settings:"),
            numericInput("cv_folds", "Number of Folds:", value = 5, min = 3, max = 10),
            numericInput("cv_repeats", "Repeats:", value = 1, min = 1, max = 5),
            
            h4("Performance Metric:"),
            conditionalPanel(
              condition = "input.problem_type == 'binary' || input.problem_type == 'multiclass'",
              selectInput("classification_metric", "Metric:",
                         choices = list(
                           "Accuracy" = "Accuracy",
                           "Kappa" = "Kappa",
                           "ROC" = "ROC"
                         ))
            ),
            
            conditionalPanel(
              condition = "input.problem_type == 'regression'",
              selectInput("regression_metric", "Metric:",
                         choices = list(
                           "RMSE" = "RMSE",
                           "MAE" = "MAE",
                           "R-squared" = "Rsquared"
                         ))
            ),
            
            br(),
            actionButton("train_models", "Train Models", class = "btn-success btn-lg"),
            br(), br(),
            
            conditionalPanel(
              condition = "output.models_trained",
              h4("Training Progress:"),
              verbatimTextOutput("training_status")
            )
          ),
          
          box(
            title = "Training Results", status = "success", solidHeader = TRUE, width = 8,
            DT::dataTableOutput("training_results"),
            br(),
            plotlyOutput("training_comparison", height = "400px")
          )
        )
      ),
      
      # Model Evaluation Tab
      tabItem(tabName = "evaluation",
        fluidRow(
          box(
            title = "Model Selection", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("selected_model", "Select Model for Evaluation:", choices = NULL),
            br(),
            
            conditionalPanel(
              condition = "input.problem_type == 'binary' || input.problem_type == 'multiclass'",
              h4("Classification Metrics:"),
              checkboxGroupInput("eval_metrics", "Display Metrics:",
                                choices = list(
                                  "Confusion Matrix" = "confusion",
                                  "ROC Curve" = "roc",
                                  "Precision-Recall" = "pr",
                                  "Class Statistics" = "class_stats"
                                ),
                                selected = c("confusion", "roc"))
            ),
            
            conditionalPanel(
              condition = "input.problem_type == 'regression'",
              h4("Regression Metrics:"),
              checkboxGroupInput("reg_eval_metrics", "Display Metrics:",
                                choices = list(
                                  "Residual Plot" = "residuals",
                                  "Prediction vs Actual" = "pred_actual",
                                  "Error Distribution" = "error_dist"
                                ),
                                selected = c("residuals", "pred_actual"))
            ),
            
            br(),
            actionButton("evaluate_model", "Evaluate Model", class = "btn-primary")
          ),
          
          box(
            title = "Model Performance", status = "success", solidHeader = TRUE, width = 8,
            verbatimTextOutput("model_performance"),
            br(),
            plotlyOutput("evaluation_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Additional Evaluation Plots", status = "info", solidHeader = TRUE, width = 12,
            plotlyOutput("additional_plots", height = "350px")
          )
        )
      ),
      
      # Predictions Tab
      tabItem(tabName = "predictions",
        fluidRow(
          box(
            title = "Make Predictions", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("prediction_model", "Select Model:", choices = NULL),
            
            h4("Input Features:"),
            uiOutput("feature_inputs"),
            
            br(),
            actionButton("make_prediction", "Predict", class = "btn-success btn-lg"),
            br(), br(),
            
            h4("Prediction Result:"),
            verbatimTextOutput("prediction_output"),
            
            conditionalPanel(
              condition = "input.problem_type == 'binary' || input.problem_type == 'multiclass'",
              br(),
              h4("Prediction Probability:"),
              verbatimTextOutput("prediction_probability")
            )
          ),
          
          box(
            title = "Batch Predictions", status = "info", solidHeader = TRUE, width = 8,
            h4("Upload CSV file for batch predictions:"),
            fileInput("batch_file", "Choose CSV File:", accept = ".csv"),
            br(),
            
            conditionalPanel(
              condition = "output.batch_uploaded",
              actionButton("run_batch", "Run Batch Predictions", class = "btn-primary"),
              br(), br(),
              DT::dataTableOutput("batch_results")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Prediction Visualization", status = "warning", solidHeader = TRUE, width = 12,
            plotlyOutput("prediction_viz", height = "400px")
          )
        )
      ),
      
      # Model Comparison Tab
      tabItem(tabName = "comparison",
        fluidRow(
          box(
            title = "Performance Comparison", status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("model_comparison_plot", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Detailed Comparison Table", status = "info", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("comparison_table")
          ),
          
          box(
            title = "Statistical Significance", status = "warning", solidHeader = TRUE, width = 6,
            verbatimTextOutput("statistical_comparison"),
            br(),
            plotOutput("comparison_boxplot", height = "300px")
          )
        )
      ),
      
      # Feature Importance Tab
      tabItem(tabName = "importance",
        fluidRow(
          box(
            title = "Feature Importance Analysis", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("importance_model", "Select Model:", choices = NULL),
            
            radioButtons("importance_type", "Importance Type:",
                        choices = list(
                          "Variable Importance" = "var_imp",
                          "Permutation Importance" = "perm_imp"
                        )),
            
            numericInput("top_features", "Number of Top Features:", 
                        value = 10, min = 5, max = 20),
            
            br(),
            actionButton("calculate_importance", "Calculate Importance", class = "btn-success")
          ),
          
          box(
            title = "Feature Importance Plot", status = "success", solidHeader = TRUE, width = 8,
            plotlyOutput("importance_plot", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Importance Values", status = "info", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("importance_table")
          ),
          
          box(
            title = "Feature Correlation with Target", status = "warning", solidHeader = TRUE, width = 6,
            plotlyOutput("feature_target_correlation", height = "350px")
          )
        )
      ),
      
      # Model Explainability Tab
      tabItem(tabName = "explainability",
        fluidRow(
          box(
            title = "LIME Explanations", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("lime_model", "Select Model:", choices = NULL),
            
            numericInput("instance_id", "Instance ID to Explain:", 
                        value = 1, min = 1),
            
            numericInput("lime_features", "Number of Features:", 
                        value = 5, min = 3, max = 10),
            
            br(),
            actionButton("generate_lime", "Generate Explanation", class = "btn-primary"),
            
            br(), br(),
            h4("Instance Details:"),
            verbatimTextOutput("instance_details")
          ),
          
          box(
            title = "LIME Explanation Plot", status = "success", solidHeader = TRUE, width = 8,
            plotOutput("lime_plot", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Global Model Behavior", status = "info", solidHeader = TRUE, width = 6,
            h4("Partial Dependence Plots:"),
            selectInput("pdp_feature", "Select Feature:", choices = NULL),
            actionButton("generate_pdp", "Generate PDP", class = "btn-info"),
            br(), br(),
            plotlyOutput("pdp_plot", height = "350px")
          ),
          
          box(
            title = "Model Summary", status = "warning", solidHeader = TRUE, width = 6,
            h4("Model Interpretability Summary:"),
            verbatimTextOutput("model_summary"),
            br(),
            h4("Key Insights:"),
            verbatimTextOutput("model_insights")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    dataset = NULL,
    train_data = NULL,
    test_data = NULL,
    models = list(),
    predictions = NULL,
    feature_names = NULL
  )
  
  # Dataset loading
  observeEvent(input$load_dataset, {
    if (input$dataset_type == "churn") {
      # Generate customer churn dataset
      n <- 1000
      values$dataset <- data.frame(
        age = sample(18:70, n, replace = TRUE),
        tenure = sample(1:120, n, replace = TRUE),
        monthly_charges = round(rnorm(n, 65, 20), 2),
        total_charges = round(rnorm(n, 2500, 1000), 2),
        contract = sample(c("Month-to-month", "One year", "Two year"), n, replace = TRUE),
        internet_service = sample(c("DSL", "Fiber optic", "No"), n, replace = TRUE),
        online_security = sample(c("Yes", "No"), n, replace = TRUE),
        tech_support = sample(c("Yes", "No"), n, replace = TRUE),
        churn = sample(c(0, 1), n, replace = TRUE, prob = c(0.73, 0.27))
      )
      
      # Convert categorical variables to numeric
      values$dataset$contract <- as.numeric(as.factor(values$dataset$contract))
      values$dataset$internet_service <- as.numeric(as.factor(values$dataset$internet_service))
      values$dataset$online_security <- as.numeric(as.factor(values$dataset$online_security))
      values$dataset$tech_support <- as.numeric(as.factor(values$dataset$tech_support))
      
    } else if (input$dataset_type == "sales") {
      # Generate sales prediction dataset
      n <- 1000
      values$dataset <- data.frame(
        advertising_spend = round(rnorm(n, 50000, 15000), 2),
        sales_team_size = sample(5:25, n, replace = TRUE),
        market_size = round(rnorm(n, 1000000, 300000), 0),
        competition_index = round(rnorm(n, 50, 15), 1),
        season = sample(1:4, n, replace = TRUE),
        product_price = round(rnorm(n, 100, 25), 2),
        customer_satisfaction = round(rnorm(n, 7.5, 1.5), 1),
        sales = round(rnorm(n, 100000, 30000), 2)
      )
      
    } else if (input$dataset_type == "iris") {
      values$dataset <- iris
      names(values$dataset)[5] <- "target"
      values$dataset$target <- as.numeric(values$dataset$target) - 1
      
    } else if (input$dataset_type == "custom") {
      # Generate custom dataset based on problem type
      n <- input$n_samples
      p <- input$n_features
      
      # Generate features
      X <- matrix(rnorm(n * p), n, p)
      colnames(X) <- paste0("feature_", 1:p)
      
      if (input$problem_type == "regression") {
        # Generate regression target
        beta <- rnorm(p)
        y <- X %*% beta + rnorm(n, 0, input$noise_level)
        target_name <- "target"
      } else {
        # Generate classification target
        beta <- rnorm(p)
        prob <- plogis(X %*% beta)
        if (input$problem_type == "binary") {
          y <- rbinom(n, 1, prob)
        } else {
          y <- sample(0:2, n, replace = TRUE)
        }
        target_name <- "class"
      }
      
      values$dataset <- data.frame(X, target = y)
      names(values$dataset)[ncol(values$dataset)] <- target_name
    }
    
    # Update feature names
    target_col <- if (input$problem_type == "regression") "target" else 
                  if ("churn" %in% names(values$dataset)) "churn" else
                  if ("sales" %in% names(values$dataset)) "sales" else "target"
    
    values$feature_names <- names(values$dataset)[names(values$dataset) != target_col]
    
    output$data_loaded <- reactive({ !is.null(values$dataset) })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  })
  
  # Data preview
  output$dataset_preview <- DT::renderDataTable({
    if (!is.null(values$dataset)) {
      DT::datatable(values$dataset, options = list(scrollX = TRUE, pageLength = 10))
    }
  })
  
  # Dataset info
  output$dataset_info <- renderText({
    if (!is.null(values$dataset)) {
      paste(
        "Dataset Shape:", nrow(values$dataset), "rows ×", ncol(values$dataset), "columns\n",
        "Features:", ncol(values$dataset) - 1, "\n",
        "Target Variable:", names(values$dataset)[ncol(values$dataset)]
      )
    }
  })
  
  # Correlation plot
  output$correlation_plot <- renderPlot({
    if (!is.null(values$dataset)) {
      numeric_data <- values$dataset[sapply(values$dataset, is.numeric)]
      if (ncol(numeric_data) > 1) {
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        corrplot(cor_matrix, method = "color", type = "upper", 
                tl.cex = 0.8, tl.col = "black")
      }
    }
  })
  
  # Target distribution
  output$target_distribution <- renderPlotly({
    if (!is.null(values$dataset)) {
      target_col <- names(values$dataset)[ncol(values$dataset)]
      
      if (input$problem_type == "regression") {
        p <- ggplot(values$dataset, aes_string(x = target_col)) +
          geom_histogram(bins = 30, fill = "#3498db", alpha = 0.7) +
          theme_minimal() +
          labs(title = "Target Variable Distribution")
      } else {
        p <- ggplot(values$dataset, aes_string(x = factor(target_col))) +
          geom_bar(fill = "#3498db", alpha = 0.7) +
          theme_minimal() +
          labs(title = "Target Class Distribution", x = "Class")
      }
      
      ggplotly(p)
    }
  })
  
  # Data splitting
  observeEvent(input$split_data, {
    req(values$dataset)
    
    set.seed(42)
    train_index <- createDataPartition(values$dataset[, ncol(values$dataset)], 
                                      p = input$train_split/100, list = FALSE)
    
    values$train_data <- values$dataset[train_index, ]
    values$test_data <- values$dataset[-train_index, ]
    
    showNotification("Data successfully split!", type = "success")
  })
  
  # Model training
  observeEvent(input$train_models, {
    req(values$train_data, input$algorithms)
    
    target_col <- names(values$train_data)[ncol(values$train_data)]
    
    # Set up training control
    ctrl <- trainControl(
      method = "cv",
      number = input$cv_folds,
      repeats = input$cv_repeats,
      classProbs = input$problem_type != "regression",
      summaryFunction = if (input$problem_type == "regression") defaultSummary else twoClassSummary
    )
    
    # Get metric
    metric <- if (input$problem_type == "regression") {
      input$regression_metric
    } else {
      input$classification_metric
    }
    
    # Prepare target variable for classification
    if (input$problem_type != "regression") {
      values$train_data[, target_col] <- factor(values$train_data[, target_col])
      values$test_data[, target_col] <- factor(values$test_data[, target_col])
      
      if (input$problem_type == "binary") {
        levels(values$train_data[, target_col]) <- c("No", "Yes")
        levels(values$test_data[, target_col]) <- c("No", "Yes")
      }
    }
    
    # Train models
    values$models <- list()
    
    for (algo in input$algorithms) {
      tryCatch({
        if (algo == "rf") {
          values$models[[algo]] <- train(
            as.formula(paste(target_col, "~ .")),
            data = values$train_data,
            method = "rf",
            trControl = ctrl,
            metric = metric,
            ntree = 100
          )
        } else if (algo == "svm") {
          values$models[[algo]] <- train(
            as.formula(paste(target_col, "~ .")),
            data = values$train_data,
            method = "svmRadial",
            trControl = ctrl,
            metric = metric
          )
        } else if (algo == "glm") {
          method_name <- if (input$problem_type == "regression") "lm" else "glm"
          values$models[[algo]] <- train(
            as.formula(paste(target_col, "~ .")),
            data = values$train_data,
            method = method_name,
            trControl = ctrl,
            metric = metric
          )
        } else if (algo == "nb") {
          values$models[[algo]] <- train(
            as.formula(paste(target_col, "~ .")),
            data = values$train_data,
            method = "naive_bayes",
            trControl = ctrl,
            metric = metric
          )
        } else if (algo == "knn") {
          values$models[[algo]] <- train(
            as.formula(paste(target_col, "~ .")),
            data = values$train_data,
            method = "knn",
            trControl = ctrl,
            metric = metric,
            tuneLength = 5
          )
        }
      }, error = function(e) {
        showNotification(paste("Error training", algo, ":", e$message), type = "error")
      })
    }
    
    # Update model choices
    model_names <- names(values$models)
    updateSelectInput(session, "selected_model", choices = model_names)
    updateSelectInput(session, "prediction_model", choices = model_names)
    updateSelectInput(session, "importance_model", choices = model_names)
    updateSelectInput(session, "lime_model", choices = model_names)
    
    output$models_trained <- reactive({ length(values$models) > 0 })
    outputOptions(output, "models_trained", suspendWhenHidden = FALSE)
    
    showNotification("Models trained successfully!", type = "success")
  })
  
  # Training results
  output$training_results <- DT::renderDataTable({
    if (length(values$models) > 0) {
      results <- data.frame(
        Model = names(values$models),
        Best_Metric = sapply(values$models, function(m) round(max(m$results[, 2]), 4)),
        stringsAsFactors = FALSE
      )
      DT::datatable(results, options = list(dom = 't'))
    }
  })
  
  # Feature input UI
  output$feature_inputs <- renderUI({
    if (!is.null(values$feature_names)) {
      input_list <- list()
      for (i in seq_along(values$feature_names)) {
        feature <- values$feature_names[i]
        if (!is.null(values$train_data)) {
          feature_data <- values$train_data[[feature]]
          if (is.numeric(feature_data)) {
            input_list[[i]] <- numericInput(
              paste0("input_", feature),
              label = feature,
              value = round(mean(feature_data, na.rm = TRUE), 2),
              min = min(feature_data, na.rm = TRUE),
              max = max(feature_data, na.rm = TRUE)
            )
          } else {
            input_list[[i]] <- selectInput(
              paste0("input_", feature),
              label = feature,
              choices = unique(feature_data)
            )
          }
        }
      }
      do.call(tagList, input_list)
    }
  })
  
  # Make prediction
  observeEvent(input$make_prediction, {
    req(input$prediction_model, values$models, values$feature_names)
    
    # Collect input values
    input_data <- data.frame(row.names = 1)
    for (feature in values$feature_names) {
      input_data[[feature]] <- input[[paste0("input_", feature)]]
    }
    
    model <- values$models[[input$prediction_model]]
    pred <- predict(model, input_data)
    
    output$prediction_output <- renderText({
      paste("Prediction:", pred)
    })
    
    if (input$problem_type != "regression") {
      pred_prob <- predict(model, input_data, type = "prob")
      output$prediction_probability <- renderText({
        paste("Probabilities:", paste(names(pred_prob), "=", round(pred_prob, 3), collapse = ", "))
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
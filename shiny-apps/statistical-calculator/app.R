# Statistical Calculator Shiny App
# Advanced Statistical Analysis Tool with Interactive Calculations

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(nortest)
library(car)
library(pwr)
library(effectsize)
library(broom)

# Source custom functions if available
if (file.exists("../../R/utils/data-generators.R")) {
  source("../../R/utils/data-generators.R")
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Advanced Statistical Calculator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "data_input", icon = icon("database")),
      menuItem("Descriptive Statistics", tabName = "descriptive", icon = icon("chart-bar")),
      menuItem("Hypothesis Testing", tabName = "hypothesis", icon = icon("calculator")),
      menuItem("Regression Analysis", tabName = "regression", icon = icon("line-chart")),
      menuItem("Power Analysis", tabName = "power", icon = icon("bolt")),
      menuItem("Effect Size", tabName = "effect_size", icon = icon("resize-arrows-alt")),
      menuItem("Distribution Explorer", tabName = "distributions", icon = icon("bell-curve"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          border-radius: 8px;
        }
        .box-header {
          border-bottom: 2px solid #3498db;
        }
        .btn-primary {
          background-color: #3498db;
          border-color: #3498db;
        }
        .btn-success {
          background-color: #27ae60;
          border-color: #27ae60;
        }
      "))
    ),
    
    tabItems(
      # Data Input Tab
      tabItem(tabName = "data_input",
        fluidRow(
          box(
            title = "Data Input Options", status = "primary", solidHeader = TRUE, width = 6,
            radioButtons("data_source", "Choose Data Source:",
                        choices = list(
                          "Upload CSV File" = "upload",
                          "Generate Sample Data" = "generate",
                          "Manual Input" = "manual"
                        ), selected = "generate"),
            
            conditionalPanel(
              condition = "input.data_source == 'upload'",
              fileInput("file", "Choose CSV File:",
                       accept = c(".csv", ".txt"))
            ),
            
            conditionalPanel(
              condition = "input.data_source == 'generate'",
              selectInput("sample_type", "Sample Data Type:",
                         choices = list(
                           "Customer Data" = "customer",
                           "Sales Data" = "sales",
                           "Experimental Data" = "experiment",
                           "Survey Data" = "survey"
                         )),
              numericInput("sample_size", "Sample Size:", value = 500, min = 10, max = 10000)
            ),
            
            conditionalPanel(
              condition = "input.data_source == 'manual'",
              h4("Enter comma-separated values:"),
              textAreaInput("manual_data", "Data Values:", 
                           placeholder = "Example: 12.5, 15.2, 18.7, 22.1, 19.8",
                           height = "100px")
            ),
            
            br(),
            actionButton("load_data", "Load Data", class = "btn-primary btn-lg")
          ),
          
          box(
            title = "Data Preview", status = "info", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("data_preview"),
            br(),
            verbatimTextOutput("data_summary")
          )
        )
      ),
      
      # Descriptive Statistics Tab
      tabItem(tabName = "descriptive",
        fluidRow(
          box(
            title = "Variable Selection", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("desc_variable", "Select Variable:", choices = NULL),
            checkboxInput("show_outliers", "Show Outliers", value = TRUE),
            checkboxInput("show_distribution", "Show Distribution Plot", value = TRUE),
            br(),
            actionButton("calculate_descriptive", "Calculate Statistics", class = "btn-success")
          ),
          
          box(
            title = "Descriptive Statistics", status = "success", solidHeader = TRUE, width = 8,
            DT::dataTableOutput("descriptive_table"),
            br(),
            plotlyOutput("descriptive_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Normality Tests", status = "warning", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("normality_tests")
          ),
          
          box(
            title = "Distribution Comparison", status = "info", solidHeader = TRUE, width = 6,
            plotlyOutput("qq_plot", height = "350px")
          )
        )
      ),
      
      # Hypothesis Testing Tab
      tabItem(tabName = "hypothesis",
        fluidRow(
          box(
            title = "Test Configuration", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("test_type", "Test Type:",
                       choices = list(
                         "One Sample t-test" = "one_sample_t",
                         "Two Sample t-test" = "two_sample_t",
                         "Paired t-test" = "paired_t",
                         "One-way ANOVA" = "anova",
                         "Chi-square test" = "chi_square",
                         "Wilcoxon test" = "wilcoxon"
                       )),
            
            conditionalPanel(
              condition = "input.test_type == 'one_sample_t'",
              selectInput("one_sample_var", "Variable:", choices = NULL),
              numericInput("test_value", "Test Value (μ₀):", value = 0)
            ),
            
            conditionalPanel(
              condition = "input.test_type == 'two_sample_t'",
              selectInput("two_sample_var", "Numeric Variable:", choices = NULL),
              selectInput("group_var", "Group Variable:", choices = NULL)
            ),
            
            conditionalPanel(
              condition = "input.test_type == 'anova'",
              selectInput("anova_dependent", "Dependent Variable:", choices = NULL),
              selectInput("anova_factor", "Factor Variable:", choices = NULL)
            ),
            
            selectInput("alternative", "Alternative Hypothesis:",
                       choices = list(
                         "Two-sided" = "two.sided",
                         "Greater than" = "greater",
                         "Less than" = "less"
                       )),
            
            numericInput("alpha_level", "Significance Level (α):", 
                        value = 0.05, min = 0.001, max = 0.1, step = 0.001),
            
            br(),
            actionButton("run_test", "Run Test", class = "btn-success btn-lg")
          ),
          
          box(
            title = "Test Results", status = "success", solidHeader = TRUE, width = 8,
            verbatimTextOutput("test_results"),
            br(),
            plotlyOutput("test_visualization", height = "400px")
          )
        )
      ),
      
      # Regression Analysis Tab
      tabItem(tabName = "regression",
        fluidRow(
          box(
            title = "Model Configuration", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("dependent_var", "Dependent Variable:", choices = NULL),
            selectizeInput("independent_vars", "Independent Variables:", 
                          choices = NULL, multiple = TRUE),
            
            radioButtons("regression_type", "Regression Type:",
                        choices = list(
                          "Linear Regression" = "linear",
                          "Logistic Regression" = "logistic",
                          "Polynomial Regression" = "polynomial"
                        )),
            
            conditionalPanel(
              condition = "input.regression_type == 'polynomial'",
              numericInput("poly_degree", "Polynomial Degree:", value = 2, min = 2, max = 5)
            ),
            
            br(),
            actionButton("run_regression", "Fit Model", class = "btn-success btn-lg")
          ),
          
          box(
            title = "Model Results", status = "success", solidHeader = TRUE, width = 8,
            verbatimTextOutput("regression_summary"),
            br(),
            plotlyOutput("regression_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Model Diagnostics", status = "warning", solidHeader = TRUE, width = 6,
            plotlyOutput("residual_plots", height = "350px")
          ),
          
          box(
            title = "Prediction", status = "info", solidHeader = TRUE, width = 6,
            h4("Enter values for prediction:"),
            uiOutput("prediction_inputs"),
            br(),
            actionButton("make_prediction", "Predict", class = "btn-primary"),
            br(), br(),
            verbatimTextOutput("prediction_result")
          )
        )
      ),
      
      # Power Analysis Tab
      tabItem(tabName = "power",
        fluidRow(
          box(
            title = "Power Analysis Configuration", status = "primary", solidHeader = TRUE, width = 6,
            selectInput("power_test", "Test Type:",
                       choices = list(
                         "t-test (two sample)" = "t_test",
                         "ANOVA (one way)" = "anova",
                         "Correlation" = "correlation",
                         "Proportion (two sample)" = "proportion"
                       )),
            
            numericInput("effect_size", "Effect Size:", value = 0.5, min = 0.1, max = 2, step = 0.1),
            numericInput("power_level", "Desired Power:", value = 0.8, min = 0.1, max = 0.99, step = 0.01),
            numericInput("sig_level", "Significance Level:", value = 0.05, min = 0.001, max = 0.1, step = 0.001),
            
            radioButtons("calculate_what", "Calculate:",
                        choices = list(
                          "Sample Size" = "n",
                          "Power" = "power",
                          "Effect Size" = "effect"
                        )),
            
            conditionalPanel(
              condition = "input.calculate_what == 'power' || input.calculate_what == 'effect'",
              numericInput("given_n", "Sample Size:", value = 30, min = 5, max = 1000)
            ),
            
            br(),
            actionButton("calculate_power", "Calculate", class = "btn-success btn-lg")
          ),
          
          box(
            title = "Power Analysis Results", status = "success", solidHeader = TRUE, width = 6,
            verbatimTextOutput("power_results"),
            br(),
            plotlyOutput("power_curve", height = "400px")
          )
        )
      ),
      
      # Effect Size Tab
      tabItem(tabName = "effect_size",
        fluidRow(
          box(
            title = "Effect Size Calculator", status = "primary", solidHeader = TRUE, width = 6,
            selectInput("effect_test", "Test Type:",
                       choices = list(
                         "Cohen's d (t-test)" = "cohens_d",
                         "Eta-squared (ANOVA)" = "eta_squared",
                         "Cramér's V (Chi-square)" = "cramers_v",
                         "R-squared (Regression)" = "r_squared"
                       )),
            
            conditionalPanel(
              condition = "input.effect_test == 'cohens_d'",
              selectInput("cohens_d_var", "Variable:", choices = NULL),
              selectInput("cohens_d_group", "Group Variable:", choices = NULL)
            ),
            
            conditionalPanel(
              condition = "input.effect_test == 'eta_squared'",
              selectInput("eta_dependent", "Dependent Variable:", choices = NULL),
              selectInput("eta_factor", "Factor Variable:", choices = NULL)
            ),
            
            br(),
            actionButton("calculate_effect", "Calculate Effect Size", class = "btn-success btn-lg")
          ),
          
          box(
            title = "Effect Size Results", status = "success", solidHeader = TRUE, width = 6,
            verbatimTextOutput("effect_results"),
            br(),
            h4("Effect Size Interpretation:"),
            verbatimTextOutput("effect_interpretation")
          )
        )
      ),
      
      # Distribution Explorer Tab
      tabItem(tabName = "distributions",
        fluidRow(
          box(
            title = "Distribution Parameters", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("distribution", "Distribution:",
                       choices = list(
                         "Normal" = "normal",
                         "t-distribution" = "t",
                         "Chi-square" = "chisq",
                         "F-distribution" = "f",
                         "Binomial" = "binomial",
                         "Poisson" = "poisson"
                       )),
            
            conditionalPanel(
              condition = "input.distribution == 'normal'",
              numericInput("norm_mean", "Mean:", value = 0),
              numericInput("norm_sd", "Standard Deviation:", value = 1, min = 0.1)
            ),
            
            conditionalPanel(
              condition = "input.distribution == 't'",
              numericInput("t_df", "Degrees of Freedom:", value = 10, min = 1)
            ),
            
            conditionalPanel(
              condition = "input.distribution == 'chisq'",
              numericInput("chisq_df", "Degrees of Freedom:", value = 5, min = 1)
            ),
            
            conditionalPanel(
              condition = "input.distribution == 'f'",
              numericInput("f_df1", "Numerator df:", value = 5, min = 1),
              numericInput("f_df2", "Denominator df:", value = 10, min = 1)
            ),
            
            conditionalPanel(
              condition = "input.distribution == 'binomial'",
              numericInput("binom_n", "Number of trials:", value = 20, min = 1),
              numericInput("binom_p", "Probability:", value = 0.5, min = 0, max = 1)
            ),
            
            conditionalPanel(
              condition = "input.distribution == 'poisson'",
              numericInput("poisson_lambda", "Lambda:", value = 3, min = 0.1)
            ),
            
            br(),
            actionButton("plot_distribution", "Plot Distribution", class = "btn-primary btn-lg")
          ),
          
          box(
            title = "Distribution Visualization", status = "success", solidHeader = TRUE, width = 8,
            plotlyOutput("distribution_plot", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Probability Calculator", status = "info", solidHeader = TRUE, width = 12,
            h4("Calculate probabilities for the selected distribution:"),
            fluidRow(
              column(4,
                numericInput("prob_value", "Value:", value = 0)
              ),
              column(4,
                selectInput("prob_type", "Type:",
                           choices = list(
                             "P(X ≤ x)" = "lower",
                             "P(X > x)" = "upper",
                             "P(X = x)" = "equal"
                           ))
              ),
              column(4,
                br(),
                actionButton("calculate_prob", "Calculate", class = "btn-info")
              )
            ),
            verbatimTextOutput("probability_result")
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
    data = NULL,
    model = NULL
  )
  
  # Data loading
  observeEvent(input$load_data, {
    if (input$data_source == "generate") {
      # Generate sample data based on type
      if (input$sample_type == "customer") {
        values$data <- data.frame(
          age = round(rnorm(input$sample_size, 40, 12)),
          income = round(rnorm(input$sample_size, 55000, 15000)),
          satisfaction = round(rnorm(input$sample_size, 7.5, 1.5), 1),
          purchases = rpois(input$sample_size, 5),
          segment = sample(c("Basic", "Premium", "VIP"), input$sample_size, replace = TRUE)
        )
      } else if (input$sample_type == "sales") {
        values$data <- data.frame(
          revenue = round(rnorm(input$sample_size, 10000, 3000), 2),
          units_sold = rpois(input$sample_size, 50),
          marketing_spend = round(rnorm(input$sample_size, 2000, 500), 2),
          region = sample(c("North", "South", "East", "West"), input$sample_size, replace = TRUE),
          quarter = sample(1:4, input$sample_size, replace = TRUE)
        )
      } else {
        # Default experimental data
        values$data <- data.frame(
          group = rep(c("Control", "Treatment"), each = input$sample_size/2),
          score = c(rnorm(input$sample_size/2, 100, 15), rnorm(input$sample_size/2, 105, 15)),
          age = round(rnorm(input$sample_size, 35, 10)),
          gender = sample(c("Male", "Female"), input$sample_size, replace = TRUE)
        )
      }
    } else if (input$data_source == "manual" && input$manual_data != "") {
      # Parse manual input
      manual_values <- as.numeric(unlist(strsplit(input$manual_data, ",")))
      manual_values <- manual_values[!is.na(manual_values)]
      values$data <- data.frame(value = manual_values)
    } else if (input$data_source == "upload" && !is.null(input$file)) {
      # Read uploaded file
      values$data <- read.csv(input$file$datapath)
    }
    
    # Update variable choices
    if (!is.null(values$data)) {
      numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
      factor_vars <- names(values$data)[sapply(values$data, function(x) is.factor(x) || is.character(x))]
      all_vars <- names(values$data)
      
      updateSelectInput(session, "desc_variable", choices = numeric_vars)
      updateSelectInput(session, "one_sample_var", choices = numeric_vars)
      updateSelectInput(session, "two_sample_var", choices = numeric_vars)
      updateSelectInput(session, "group_var", choices = factor_vars)
      updateSelectInput(session, "anova_dependent", choices = numeric_vars)
      updateSelectInput(session, "anova_factor", choices = factor_vars)
      updateSelectInput(session, "dependent_var", choices = numeric_vars)
      updateSelectizeInput(session, "independent_vars", choices = numeric_vars)
      updateSelectInput(session, "cohens_d_var", choices = numeric_vars)
      updateSelectInput(session, "cohens_d_group", choices = factor_vars)
      updateSelectInput(session, "eta_dependent", choices = numeric_vars)
      updateSelectInput(session, "eta_factor", choices = factor_vars)
    }
  })
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    if (!is.null(values$data)) {
      DT::datatable(values$data, options = list(scrollX = TRUE, pageLength = 10))
    }
  })
  
  # Data summary
  output$data_summary <- renderText({
    if (!is.null(values$data)) {
      paste(
        "Dataset Shape:", nrow(values$data), "rows ×", ncol(values$data), "columns\n",
        "Numeric Variables:", sum(sapply(values$data, is.numeric)), "\n",
        "Factor Variables:", sum(sapply(values$data, function(x) is.factor(x) || is.character(x)))
      )
    }
  })
  
  # Descriptive statistics
  observeEvent(input$calculate_descriptive, {
    req(values$data, input$desc_variable)
    
    var_data <- values$data[[input$desc_variable]]
    
    # Calculate descriptive statistics
    desc_stats <- data.frame(
      Statistic = c("Count", "Mean", "Median", "Std Dev", "Variance", "Min", "Max", 
                   "Q1", "Q3", "Skewness", "Kurtosis"),
      Value = c(
        length(var_data),
        round(mean(var_data, na.rm = TRUE), 3),
        round(median(var_data, na.rm = TRUE), 3),
        round(sd(var_data, na.rm = TRUE), 3),
        round(var(var_data, na.rm = TRUE), 3),
        round(min(var_data, na.rm = TRUE), 3),
        round(max(var_data, na.rm = TRUE), 3),
        round(quantile(var_data, 0.25, na.rm = TRUE), 3),
        round(quantile(var_data, 0.75, na.rm = TRUE), 3),
        round(moments::skewness(var_data, na.rm = TRUE), 3),
        round(moments::kurtosis(var_data, na.rm = TRUE), 3)
      )
    )
    
    output$descriptive_table <- DT::renderDataTable({
      DT::datatable(desc_stats, options = list(dom = 't', pageLength = 15))
    })
    
    # Create visualization
    output$descriptive_plot <- renderPlotly({
      p <- ggplot(values$data, aes_string(x = input$desc_variable)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "#3498db", alpha = 0.7) +
        geom_density(color = "#e74c3c", size = 1) +
        theme_minimal() +
        labs(title = paste("Distribution of", input$desc_variable))
      
      ggplotly(p)
    })
    
    # Normality tests
    normality_results <- data.frame(
      Test = c("Shapiro-Wilk", "Anderson-Darling", "Kolmogorov-Smirnov"),
      Statistic = c(
        round(shapiro.test(var_data)$statistic, 4),
        round(nortest::ad.test(var_data)$statistic, 4),
        round(ks.test(var_data, "pnorm", mean(var_data), sd(var_data))$statistic, 4)
      ),
      P_Value = c(
        round(shapiro.test(var_data)$p.value, 4),
        round(nortest::ad.test(var_data)$p.value, 4),
        round(ks.test(var_data, "pnorm", mean(var_data), sd(var_data))$p.value, 4)
      )
    )
    
    output$normality_tests <- DT::renderDataTable({
      DT::datatable(normality_results, options = list(dom = 't'))
    })
    
    # Q-Q plot
    output$qq_plot <- renderPlotly({
      p <- ggplot(values$data, aes_string(sample = input$desc_variable)) +
        stat_qq() +
        stat_qq_line(color = "#e74c3c") +
        theme_minimal() +
        labs(title = "Q-Q Plot for Normality Assessment")
      
      ggplotly(p)
    })
  })
  
  # Hypothesis testing
  observeEvent(input$run_test, {
    req(values$data)
    
    test_result <- NULL
    
    if (input$test_type == "one_sample_t") {
      req(input$one_sample_var, input$test_value)
      test_result <- t.test(values$data[[input$one_sample_var]], 
                           mu = input$test_value, 
                           alternative = input$alternative)
    } else if (input$test_type == "two_sample_t") {
      req(input$two_sample_var, input$group_var)
      formula_str <- paste(input$two_sample_var, "~", input$group_var)
      test_result <- t.test(as.formula(formula_str), 
                           data = values$data, 
                           alternative = input$alternative)
    } else if (input$test_type == "anova") {
      req(input$anova_dependent, input$anova_factor)
      formula_str <- paste(input$anova_dependent, "~", input$anova_factor)
      aov_result <- aov(as.formula(formula_str), data = values$data)
      test_result <- summary(aov_result)
    }
    
    output$test_results <- renderText({
      if (!is.null(test_result)) {
        capture.output(print(test_result))
      }
    })
  })
  
  # Distribution explorer
  observeEvent(input$plot_distribution, {
    output$distribution_plot <- renderPlotly({
      x_vals <- NULL
      y_vals <- NULL
      
      if (input$distribution == "normal") {
        x_vals <- seq(input$norm_mean - 4*input$norm_sd, 
                     input$norm_mean + 4*input$norm_sd, length.out = 1000)
        y_vals <- dnorm(x_vals, input$norm_mean, input$norm_sd)
      } else if (input$distribution == "t") {
        x_vals <- seq(-4, 4, length.out = 1000)
        y_vals <- dt(x_vals, input$t_df)
      } else if (input$distribution == "chisq") {
        x_vals <- seq(0, input$chisq_df + 3*sqrt(2*input$chisq_df), length.out = 1000)
        y_vals <- dchisq(x_vals, input$chisq_df)
      }
      
      if (!is.null(x_vals) && !is.null(y_vals)) {
        p <- ggplot(data.frame(x = x_vals, y = y_vals), aes(x = x, y = y)) +
          geom_line(color = "#3498db", size = 1.2) +
          geom_area(fill = "#3498db", alpha = 0.3) +
          theme_minimal() +
          labs(title = paste(tools::toTitleCase(input$distribution), "Distribution"),
               x = "x", y = "Density")
        
        ggplotly(p)
      }
    })
  })
  
  # Probability calculator
  observeEvent(input$calculate_prob, {
    prob_result <- NULL
    
    if (input$distribution == "normal") {
      if (input$prob_type == "lower") {
        prob_result <- pnorm(input$prob_value, input$norm_mean, input$norm_sd)
      } else if (input$prob_type == "upper") {
        prob_result <- 1 - pnorm(input$prob_value, input$norm_mean, input$norm_sd)
      } else {
        prob_result <- dnorm(input$prob_value, input$norm_mean, input$norm_sd)
      }
    }
    
    output$probability_result <- renderText({
      if (!is.null(prob_result)) {
        paste("Probability:", round(prob_result, 6))
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
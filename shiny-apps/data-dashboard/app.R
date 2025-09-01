#' Interactive Data Science Portfolio Dashboard
#' 
#' A comprehensive Shiny application showcasing advanced R programming,
#' data visualization, and interactive analytics capabilities.

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)
library(lubridate)

# =============================================================================
# DATA GENERATION AND SETUP
# =============================================================================

# Generate comprehensive sample dataset
generate_portfolio_data <- function(n_customers = 1000, n_products = 50, seed = 42) {
  set.seed(seed)
  
  # Customer data
  customers <- tibble(
    customer_id = paste0("CUST_", str_pad(1:n_customers, 4, "left", "0")),
    name = paste(
      sample(c("John", "Jane", "Michael", "Sarah", "David", "Emily", "Robert", "Lisa", "Mark", "Jennifer"), n_customers, replace = TRUE),
      sample(c("Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller", "Davis", "Wilson", "Moore"), n_customers, replace = TRUE)
    ),
    age = sample(18:75, n_customers, replace = TRUE),
    gender = sample(c("Male", "Female", "Other"), n_customers, replace = TRUE, prob = c(0.48, 0.48, 0.04)),
    country = sample(c("USA", "Canada", "UK", "Germany", "France", "Australia", "Japan", "Brazil"), 
                    n_customers, replace = TRUE, prob = c(0.35, 0.15, 0.12, 0.1, 0.08, 0.08, 0.07, 0.05)),
    registration_date = sample(seq(as.Date("2020-01-01"), Sys.Date(), by = "day"), n_customers),
    customer_segment = sample(c("Premium", "Standard", "Basic"), n_customers, replace = TRUE, prob = c(0.15, 0.55, 0.3)),
    annual_income = round(pmax(20000, rnorm(n_customers, 65000, 25000)), 0),
    credit_score = sample(300:850, n_customers, replace = TRUE),
    satisfaction_score = round(runif(n_customers, 1, 10), 1),
    is_active = sample(c(TRUE, FALSE), n_customers, replace = TRUE, prob = c(0.85, 0.15))
  )
  
  # Product data
  categories <- c("Electronics", "Clothing", "Books", "Home & Garden", "Sports", "Beauty", "Automotive")
  products <- tibble(
    product_id = paste0("PROD_", str_pad(1:n_products, 3, "left", "0")),
    product_name = paste(
      sample(c("Premium", "Deluxe", "Standard", "Basic", "Pro", "Ultimate"), n_products, replace = TRUE),
      sample(categories, n_products, replace = TRUE),
      sample(c("Device", "Tool", "Kit", "Set", "System", "Solution"), n_products, replace = TRUE)
    ),
    category = sample(categories, n_products, replace = TRUE),
    price = round(runif(n_products, 10, 1000), 2),
    cost = round(runif(n_products, 5, 500), 2),
    rating = round(runif(n_products, 1, 5), 1),
    reviews_count = sample(0:1000, n_products, replace = TRUE),
    launch_date = sample(seq(as.Date("2020-01-01"), Sys.Date(), by = "day"), n_products),
    in_stock = sample(c(TRUE, FALSE), n_products, replace = TRUE, prob = c(0.9, 0.1))
  )
  
  # Transaction data
  n_transactions <- n_customers * 3
  transactions <- tibble(
    transaction_id = paste0("TXN_", str_pad(1:n_transactions, 6, "left", "0")),
    customer_id = sample(customers$customer_id, n_transactions, replace = TRUE),
    product_id = sample(products$product_id, n_transactions, replace = TRUE),
    transaction_date = sample(seq(as.Date("2023-01-01"), Sys.Date(), by = "day"), n_transactions, replace = TRUE),
    quantity = sample(1:5, n_transactions, replace = TRUE, prob = c(0.5, 0.25, 0.15, 0.07, 0.03)),
    discount_percent = sample(c(0, 5, 10, 15, 20, 25), n_transactions, replace = TRUE, prob = c(0.4, 0.2, 0.2, 0.1, 0.07, 0.03)),
    channel = sample(c("Online", "Mobile", "Store", "Phone"), n_transactions, replace = TRUE, prob = c(0.4, 0.3, 0.25, 0.05)),
    payment_method = sample(c("Credit Card", "Debit Card", "PayPal", "Bank Transfer"), n_transactions, replace = TRUE),
    status = sample(c("Completed", "Pending", "Cancelled", "Refunded"), n_transactions, replace = TRUE, prob = c(0.85, 0.08, 0.05, 0.02))
  ) %>%
  left_join(products %>% select(product_id, price, category), by = "product_id") %>%
  mutate(
    unit_price = price * (1 - discount_percent / 100),
    total_amount = quantity * unit_price,
    profit_margin = (unit_price - products$cost[match(product_id, products$product_id)]) / unit_price
  )
  
  list(
    customers = customers,
    products = products,
    transactions = transactions
  )
}

# Professional color palette
portfolio_colors <- c(
  primary = "#2C3E50",
  secondary = "#3498DB", 
  success = "#2ECC71",
  warning = "#F39C12",
  danger = "#E74C3C",
  info = "#17A2B8",
  light = "#F8F9FA",
  dark = "#343A40"
)

# Custom ggplot theme
theme_portfolio <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", color = portfolio_colors[["primary"]]),
      plot.subtitle = element_text(size = 12, color = portfolio_colors[["dark"]]),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11, face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", size = 0.5)
    )
}

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  
  # Header
  dashboardHeader(
    title = "R Data Science Portfolio",
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://github.com/yourusername/r-data-science-compendium",
        target = "_blank",
        tags$i(class = "fa fa-github"),
        " GitHub",
        style = "color: white; margin-top: 15px; margin-right: 10px;"
      )
    )
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar_menu",
      
      menuItem("📊 Executive Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("👥 Customer Analytics", tabName = "customers", icon = icon("users")),
      menuItem("📦 Product Analysis", tabName = "products", icon = icon("box")),
      menuItem("💰 Sales Performance", tabName = "sales", icon = icon("chart-line")),
      menuItem("🔍 Data Explorer", tabName = "explorer", icon = icon("search")),
      menuItem("🤖 ML Predictions", tabName = "ml", icon = icon("robot")),
      menuItem("📈 Advanced Analytics", tabName = "advanced", icon = icon("chart-area")),
      
      br(),
      
      # Filters
      h4("Global Filters", style = "margin-left: 20px; color: #2C3E50;"),
      
      dateRangeInput(
        "date_range",
        "Date Range:",
        start = Sys.Date() - 365,
        end = Sys.Date(),
        width = "90%",
        style = "margin-left: 20px;"
      ),
      
      selectInput(
        "customer_segment",
        "Customer Segment:",
        choices = c("All", "Premium", "Standard", "Basic"),
        selected = "All",
        width = "90%",
        style = "margin-left: 20px;"
      ),
      
      selectInput(
        "channel",
        "Sales Channel:",
        choices = c("All", "Online", "Mobile", "Store", "Phone"),
        selected = "All",
        width = "90%",
        style = "margin-left: 20px;"
      ),
      
      br(),
      
      # Data refresh
      actionButton(
        "refresh_data",
        "🔄 Refresh Data",
        class = "btn btn-primary",
        style = "margin-left: 20px; margin-bottom: 20px;"
      ),
      
      # Info box
      div(
        style = "margin: 20px; padding: 15px; background: #f8f9fa; border-radius: 5px; border-left: 4px solid #3498db;",
        h5("💡 Portfolio Showcase", style = "margin-top: 0; color: #2c3e50;"),
        p("This dashboard demonstrates advanced R programming, data visualization, and interactive analytics.", 
          style = "font-size: 12px; margin-bottom: 0; color: #6c757d;")
      )
    )
  ),
  
  # Body
  dashboardBody(
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .box-header {
          border-radius: 8px 8px 0 0;
        }
        
        .small-box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3498db;
        }
        
        .dataTables_wrapper {
          font-family: 'Source Sans Pro', sans-serif;
        }
        
        .plotly {
          border-radius: 8px;
        }
      "))
    ),
    
    tabItems(
      
      # Executive Dashboard Tab
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          valueBoxOutput("total_revenue", width = 3),
          valueBoxOutput("total_customers", width = 3),
          valueBoxOutput("avg_order_value", width = 3),
          valueBoxOutput("customer_satisfaction", width = 3)
        ),
        
        fluidRow(
          box(
            title = "📈 Revenue Trend",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("revenue_trend", height = "300px")
          ),
          
          box(
            title = "🎯 Key Metrics",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            tableOutput("key_metrics")
          )
        ),
        
        fluidRow(
          box(
            title = "🌍 Sales by Region",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("sales_by_region", height = "300px")
          ),
          
          box(
            title = "📦 Top Products",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("top_products", height = "300px")
          )
        )
      ),
      
      # Customer Analytics Tab
      tabItem(
        tabName = "customers",
        
        fluidRow(
          box(
            title = "👥 Customer Demographics",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            tabsetPanel(
              tabPanel(
                "Age Distribution",
                plotlyOutput("customer_age_dist", height = "400px")
              ),
              tabPanel(
                "Geographic Analysis",
                plotlyOutput("customer_geography", height = "400px")
              ),
              tabPanel(
                "Segment Analysis",
                plotlyOutput("customer_segments", height = "400px")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "🔍 Customer Search & Details",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 4,
                selectizeInput(
                  "customer_search",
                  "Search Customer:",
                  choices = NULL,
                  options = list(placeholder = "Type customer name or ID...")
                )
              ),
              column(
                width = 8,
                DT::dataTableOutput("customer_details")
              )
            )
          )
        )
      ),
      
      # Product Analysis Tab
      tabItem(
        tabName = "products",
        
        fluidRow(
          box(
            title = "📦 Product Performance Matrix",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("product_performance", height = "400px")
          ),
          
          box(
            title = "⚙️ Analysis Controls",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            
            selectInput(
              "product_metric_x",
              "X-Axis Metric:",
              choices = c("Price" = "price", "Rating" = "rating", "Reviews" = "reviews_count"),
              selected = "price"
            ),
            
            selectInput(
              "product_metric_y", 
              "Y-Axis Metric:",
              choices = c("Revenue" = "revenue", "Units Sold" = "units_sold", "Profit" = "profit"),
              selected = "revenue"
            ),
            
            selectInput(
              "product_category_filter",
              "Category Filter:",
              choices = c("All Categories"),
              selected = "All Categories"
            ),
            
            hr(),
            
            h5("📈 Product Insights"),
            textOutput("product_insights")
          )
        ),
        
        fluidRow(
          box(
            title = "📊 Category Performance",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("category_performance", height = "350px")
          ),
          
          box(
            title = "⭐ Product Ratings Distribution",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("product_ratings", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "🏆 Top & Bottom Performers",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("product_table")
          )
        )
      ),
      
      # Sales Performance Tab
      tabItem(
        tabName = "sales",
        
        fluidRow(
          box(
            title = "💹 Sales Trend Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            tabsetPanel(
              tabPanel(
                "Daily Trends",
                plotlyOutput("daily_sales", height = "400px")
              ),
              tabPanel(
                "Monthly Analysis",
                plotlyOutput("monthly_sales", height = "400px")
              ),
              tabPanel(
                "Channel Comparison",
                plotlyOutput("channel_sales", height = "400px")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "📈 Sales Forecasting",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("sales_forecast", height = "350px")
          ),
          
          box(
            title = "🎯 Performance Metrics",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            
            h5("💰 Revenue Metrics"),
            tableOutput("revenue_metrics"),
            
            hr(),
            
            h5("📊 Growth Rates"),
            tableOutput("growth_metrics")
          )
        )
      ),
      
      # Data Explorer Tab
      tabItem(
        tabName = "explorer",
        
        fluidRow(
          box(
            title = "🔍 Interactive Data Explorer",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            tabsetPanel(
              id = "explorer_tabs",
              
              tabPanel(
                "Raw Data",
                
                fluidRow(
                  column(
                    width = 3,
                    selectInput(
                      "data_table",
                      "Select Dataset:",
                      choices = c("Customers" = "customers", "Products" = "products", "Transactions" = "transactions"),
                      selected = "transactions"
                    )
                  ),
                  column(
                    width = 3,
                    numericInput(
                      "data_rows",
                      "Rows to Display:",
                      value = 100,
                      min = 10,
                      max = 1000,
                      step = 10
                    )
                  ),
                  column(
                    width = 3,
                    downloadButton(
                      "download_data",
                      "📥 Download CSV",
                      class = "btn btn-success"
                    )
                  ),
                  column(
                    width = 3,
                    actionButton(
                      "data_summary",
                      "📊 Show Summary",
                      class = "btn btn-info"
                    )
                  )
                ),
                
                br(),
                
                DT::dataTableOutput("raw_data_table")
              ),
              
              tabPanel(
                "Custom Visualization",
                
                fluidRow(
                  column(
                    width = 4,
                    h4("📊 Chart Builder"),
                    
                    selectInput(
                      "chart_type",
                      "Chart Type:",
                      choices = c("Scatter Plot" = "scatter", "Bar Chart" = "bar", "Histogram" = "histogram", "Box Plot" = "box"),
                      selected = "scatter"
                    ),
                    
                    selectInput(
                      "x_variable",
                      "X Variable:",
                      choices = NULL
                    ),
                    
                    conditionalPanel(
                      condition = "input.chart_type != 'histogram'",
                      selectInput(
                        "y_variable",
                        "Y Variable:",
                        choices = NULL
                      )
                    ),
                    
                    selectInput(
                      "color_variable",
                      "Color By:",
                      choices = c("None" = "none"),
                      selected = "none"
                    ),
                    
                    conditionalPanel(
                      condition = "input.chart_type == 'scatter'",
                      selectInput(
                        "size_variable",
                        "Size By:",
                        choices = c("None" = "none"),
                        selected = "none"
                      )
                    ),
                    
                    actionButton(
                      "generate_chart",
                      "🎨 Generate Chart",
                      class = "btn btn-primary",
                      style = "width: 100%; margin-top: 10px;"
                    )
                  ),
                  
                  column(
                    width = 8,
                    plotlyOutput("custom_chart", height = "500px")
                  )
                )
              ),
              
              tabPanel(
                "Statistical Summary",
                
                fluidRow(
                  column(
                    width = 6,
                    h4("📈 Descriptive Statistics"),
                    DT::dataTableOutput("descriptive_stats")
                  ),
                  column(
                    width = 6,
                    h4("🔗 Correlation Matrix"),
                    plotlyOutput("correlation_matrix", height = "400px")
                  )
                )
              )
            )
          )
        )
      ),
      
      # ML Predictions Tab
      tabItem(
        tabName = "ml",
        
        fluidRow(
          box(
            title = "🤖 Machine Learning Predictions",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            tabsetPanel(
              tabPanel(
                "Customer Segmentation",
                
                fluidRow(
                  column(
                    width = 4,
                    h4("⚙️ Model Configuration"),
                    
                    selectInput(
                      "ml_features",
                      "Select Features:",
                      choices = c("Age" = "age", "Income" = "annual_income", "Satisfaction" = "satisfaction_score"),
                      selected = c("age", "annual_income", "satisfaction_score"),
                      multiple = TRUE
                    ),
                    
                    numericInput(
                      "n_clusters",
                      "Number of Clusters:",
                      value = 3,
                      min = 2,
                      max = 8,
                      step = 1
                    ),
                    
                    actionButton(
                      "run_clustering",
                      "🚀 Run Clustering",
                      class = "btn btn-success"
                    ),
                    
                    br(), br(),
                    
                    conditionalPanel(
                      condition = "output.clustering_complete",
                      h5("📊 Cluster Summary"),
                      tableOutput("cluster_summary")
                    )
                  ),
                  
                  column(
                    width = 8,
                    plotlyOutput("clustering_plot", height = "500px")
                  )
                )
              ),
              
              tabPanel(
                "Sales Prediction",
                
                fluidRow(
                  column(
                    width = 4,
                    h4("🔮 Sales Predictor"),
                    
                    numericInput(
                      "pred_customer_age",
                      "Customer Age:",
                      value = 35,
                      min = 18,
                      max = 80
                    ),
                    
                    selectInput(
                      "pred_customer_segment",
                      "Customer Segment:",
                      choices = c("Premium", "Standard", "Basic"),
                      selected = "Standard"
                    ),
                    
                    selectInput(
                      "pred_product_category",
                      "Product Category:",
                      choices = c("Electronics", "Clothing", "Books"),
                      selected = "Electronics"
                    ),
                    
                    numericInput(
                      "pred_product_price",
                      "Product Price ($):",
                      value = 100,
                      min = 10,
                      max = 1000
                    ),
                    
                    actionButton(
                      "predict_sale",
                      "🎯 Predict Purchase Probability",
                      class = "btn btn-warning"
                    ),
                    
                    br(), br(),
                    
                    conditionalPanel(
                      condition = "output.prediction_complete",
                      div(
                        style = "padding: 15px; background: #f8f9fa; border-radius: 5px;",
                        h5("🎯 Prediction Result"),
                        textOutput("prediction_result")
                      )
                    )
                  ),
                  
                  column(
                    width = 8,
                    h4("📊 Model Performance"),
                    plotlyOutput("ml_performance", height = "400px")
                  )
                )
              )
            )
          )
        )
      ),
      
      # Advanced Analytics Tab
      tabItem(
        tabName = "advanced",
        
        fluidRow(
          box(
            title = "🧪 Advanced Analytics Laboratory",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            tabsetPanel(
              tabPanel(
                "Cohort Analysis",
                
                fluidRow(
                  column(
                    width = 8,
                    plotlyOutput("cohort_heatmap", height = "500px")
                  ),
                  column(
                    width = 4,
                    h4("📊 Cohort Insights"),
                    
                    div(
                      style = "padding: 15px; background: #e8f4fd; border-radius: 5px; margin-bottom: 15px;",
                      h5("📈 Retention Rate", style = "color: #2c3e50; margin-top: 0;"),
                      textOutput("retention_insight")
                    ),
                    
                    div(
                      style = "padding: 15px; background: #e8f5e8; border-radius: 5px; margin-bottom: 15px;",
                      h5("👥 Customer Behavior", style = "color: #2c3e50; margin-top: 0;"),
                      textOutput("behavior_insight")
                    ),
                    
                    div(
                      style = "padding: 15px; background: #fff3cd; border-radius: 5px;",
                      h5("💡 Recommendations", style = "color: #2c3e50; margin-top: 0;"),
                      textOutput("cohort_recommendations")
                    )
                  )
                )
              ),
              
              tabPanel(
                "RFM Analysis",
                
                fluidRow(
                  column(
                    width = 8,
                    plotlyOutput("rfm_analysis", height = "500px")
                  ),
                  column(
                    width = 4,
                    h4("🎯 RFM Segments"),
                    DT::dataTableOutput("rfm_segments")
                  )
                )
              ),
              
              tabPanel(
                "Market Basket Analysis",
                
                fluidRow(
                  column(
                    width = 6,
                    h4("🛒 Product Associations"),
                    plotlyOutput("market_basket", height = "400px")
                  ),
                  column(
                    width = 6,
                    h4("📋 Association Rules"),
                    DT::dataTableOutput("association_rules")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # Reactive data
  portfolio_data <- reactive({
    input$refresh_data  # Trigger on refresh button
    generate_portfolio_data()
  })
  
  # Filtered data based on global filters
  filtered_data <- reactive({
    data <- portfolio_data()
    
    # Filter transactions by date range
    transactions <- data$transactions %>%
      filter(
        transaction_date >= input$date_range[1],
        transaction_date <= input$date_range[2],
        status == "Completed"
      )
    
    # Filter by customer segment
    if (input$customer_segment != "All") {
      customer_ids <- data$customers %>%
        filter(customer_segment == input$customer_segment) %>%
        pull(customer_id)
      
      transactions <- transactions %>%
        filter(customer_id %in% customer_ids)
    }
    
    # Filter by channel
    if (input$channel != "All") {
      transactions <- transactions %>%
        filter(channel == input$channel)
    }
    
    list(
      customers = data$customers,
      products = data$products,
      transactions = transactions
    )
  })
  
  # Update filter choices
  observe({
    data <- portfolio_data()
    
    # Update customer search choices
    customer_choices <- data$customers %>%
      mutate(label = paste0(name, " (", customer_id, ")")) %>%
      select(customer_id, label)
    
    updateSelectizeInput(
      session,
      "customer_search",
      choices = setNames(customer_choices$customer_id, customer_choices$label),
      server = TRUE
    )
    
    # Update product category filter
    categories <- c("All Categories", unique(data$products$category))
    updateSelectInput(session, "product_category_filter", choices = categories)
    
    # Update chart variables
    numeric_vars <- c("age", "annual_income", "satisfaction_score", "credit_score", "total_amount", "quantity", "price", "rating")
    categorical_vars <- c("customer_segment", "gender", "country", "category", "channel", "payment_method")
    
    updateSelectInput(session, "x_variable", choices = c(numeric_vars, categorical_vars))
    updateSelectInput(session, "y_variable", choices = numeric_vars)
    updateSelectInput(session, "color_variable", choices = c("None" = "none", categorical_vars))
    updateSelectInput(session, "size_variable", choices = c("None" = "none", numeric_vars))
  })
  
  # =============================================================================
  # DASHBOARD TAB OUTPUTS
  # =============================================================================
  
  # Value boxes
  output$total_revenue <- renderValueBox({
    revenue <- filtered_data()$transactions %>%
      summarise(total = sum(total_amount, na.rm = TRUE)) %>%
      pull(total)
    
    valueBox(
      value = paste0("$", scales::comma(round(revenue, 0))),
      subtitle = "Total Revenue",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$total_customers <- renderValueBox({
    customers <- filtered_data()$transactions %>%
      summarise(count = n_distinct(customer_id)) %>%
      pull(count)
    
    valueBox(
      value = scales::comma(customers),
      subtitle = "Active Customers",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_order_value <- renderValueBox({
    aov <- filtered_data()$transactions %>%
      summarise(aov = mean(total_amount, na.rm = TRUE)) %>%
      pull(aov)
    
    valueBox(
      value = paste0("$", round(aov, 0)),
      subtitle = "Avg Order Value",
      icon = icon("shopping-cart"),
      color = "yellow"
    )
  })
  
  output$customer_satisfaction <- renderValueBox({
    satisfaction <- filtered_data()$customers %>%
      summarise(avg_sat = mean(satisfaction_score, na.rm = TRUE)) %>%
      pull(avg_sat)
    
    valueBox(
      value = paste0(round(satisfaction, 1), "/10"),
      subtitle = "Customer Satisfaction",
      icon = icon("star"),
      color = "purple"
    )
  })
  
  # Revenue trend chart
  output$revenue_trend <- renderPlotly({
    trend_data <- filtered_data()$transactions %>%
      mutate(month = floor_date(transaction_date, "month")) %>%
      group_by(month) %>%
      summarise(revenue = sum(total_amount, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(trend_data, aes(x = month, y = revenue)) +
      geom_line(color = portfolio_colors[["primary"]], size = 1.2) +
      geom_point(color = portfolio_colors[["secondary"]], size = 2) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_x_date(date_labels = "%b %Y") +
      labs(
        title = "Monthly Revenue Trend",
        x = "Month",
        y = "Revenue"
      ) +
      theme_portfolio()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Key metrics table
  output$key_metrics <- renderTable({
    data <- filtered_data()$transactions
    
    tibble(
      Metric = c("Total Orders", "Avg Items/Order", "Return Rate", "Conversion Rate"),
      Value = c(
        scales::comma(nrow(data)),
        round(mean(data$quantity, na.rm = TRUE), 1),
        "2.3%",  # Placeholder
        "3.7%"   # Placeholder
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # Sales by region
  output$sales_by_region <- renderPlotly({
    region_data <- filtered_data()$transactions %>%
      left_join(filtered_data()$customers %>% select(customer_id, country), by = "customer_id") %>%
      group_by(country) %>%
      summarise(revenue = sum(total_amount, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(revenue)) %>%
      slice_head(n = 6)
    
    p <- ggplot(region_data, aes(x = reorder(country, revenue), y = revenue, fill = country)) +
      geom_col(alpha = 0.8) +
      scale_fill_viridis_d() +
      scale_y_continuous(labels = scales::dollar_format()) +
      coord_flip() +
      labs(
        title = "Revenue by Country",
        x = "Country",
        y = "Revenue"
      ) +
      theme_portfolio() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Top products
  output$top_products <- renderPlotly({
    product_data <- filtered_data()$transactions %>%
      group_by(product_id) %>%
      summarise(
        revenue = sum(total_amount, na.rm = TRUE),
        units = sum(quantity, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(filtered_data()$products %>% select(product_id, product_name, category), by = "product_id") %>%
      arrange(desc(revenue)) %>%
      slice_head(n = 8) %>%
      mutate(product_name = str_trunc(product_name, 20))
    
    p <- ggplot(product_data, aes(x = reorder(product_name, revenue), y = revenue, fill = category)) +
      geom_col(alpha = 0.8) +
      scale_fill_manual(values = rainbow(length(unique(product_data$category)))) +
      scale_y_continuous(labels = scales::dollar_format()) +
      coord_flip() +
      labs(
        title = "Top Products by Revenue",
        x = "Product",
        y = "Revenue"
      ) +
      theme_portfolio()
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Additional outputs would continue here...
  # For brevity, I'll include a few more key outputs
  
  # =============================================================================
  # CUSTOMER ANALYTICS TAB
  # =============================================================================
  
  output$customer_age_dist <- renderPlotly({
    age_data <- filtered_data()$customers
    
    p <- ggplot(age_data, aes(x = age, fill = customer_segment)) +
      geom_histogram(bins = 20, alpha = 0.7, position = "stack") +
      scale_fill_manual(values = c("Premium" = portfolio_colors[["success"]], 
                                  "Standard" = portfolio_colors[["primary"]], 
                                  "Basic" = portfolio_colors[["warning"]])) +
      labs(
        title = "Customer Age Distribution by Segment",
        x = "Age",
        y = "Count",
        fill = "Segment"
      ) +
      theme_portfolio()
    
    ggplotly(p)
  })
  
  # =============================================================================
  # DATA EXPLORER TAB
  # =============================================================================
  
  output$raw_data_table <- DT::renderDataTable({
    data <- filtered_data()
    
    selected_data <- switch(input$data_table,
      "customers" = data$customers,
      "products" = data$products,
      "transactions" = data$transactions
    )
    
    DT::datatable(
      selected_data %>% slice_head(n = input$data_rows),
      options = list(
        scrollX = TRUE,
        pageLength = 25,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display nowrap"
    )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$data_table, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- filtered_data()
      selected_data <- switch(input$data_table,
        "customers" = data$customers,
        "products" = data$products,
        "transactions" = data$transactions
      )
      write.csv(selected_data, file, row.names = FALSE)
    }
  )
  
  # =============================================================================
  # ML PREDICTIONS TAB
  # =============================================================================
  
  # Clustering analysis
  clustering_results <- reactiveVal(NULL)
  
  observeEvent(input$run_clustering, {
    data <- filtered_data()$customers
    
    # Prepare data for clustering
    cluster_data <- data %>%
      select(all_of(input$ml_features)) %>%
      na.omit() %>%
      scale()
    
    # Perform k-means clustering
    set.seed(42)
    kmeans_result <- kmeans(cluster_data, centers = input$n_clusters)
    
    # Store results
    clustering_results(list(
      data = data,
      cluster_data = cluster_data,
      clusters = kmeans_result$cluster,
      centers = kmeans_result$centers
    ))
  })
  
  output$clustering_complete <- reactive({
    !is.null(clustering_results())
  })
  
  outputOptions(output, "clustering_complete", suspendWhenHidden = FALSE)
  
  output$clustering_plot <- renderPlotly({
    req(clustering_results())
    
    results <- clustering_results()
    plot_data <- results$data %>%
      mutate(cluster = as.factor(results$clusters))
    
    # Use first two features for visualization
    x_var <- input$ml_features[1]
    y_var <- input$ml_features[2]
    
    p <- ggplot(plot_data, aes_string(x = x_var, y = y_var, color = "cluster")) +
      geom_point(alpha = 0.7, size = 2) +
      scale_color_viridis_d() +
      labs(
        title = "Customer Segmentation Results",
        x = str_to_title(gsub("_", " ", x_var)),
        y = str_to_title(gsub("_", " ", y_var))
      ) +
      theme_portfolio()
    
    ggplotly(p)
  })
  
  # Prediction functionality
  prediction_results <- reactiveVal(NULL)
  
  observeEvent(input$predict_sale, {
    # Simple prediction logic (in real app, would use trained model)
    prob <- runif(1, 0.1, 0.9)  # Random probability for demo
    prediction_results(prob)
  })
  
  output$prediction_complete <- reactive({
    !is.null(prediction_results())
  })
  
  outputOptions(output, "prediction_complete", suspendWhenHidden = FALSE)
  
  output$prediction_result <- renderText({
    req(prediction_results())
    
    prob <- prediction_results()
    percentage <- round(prob * 100, 1)
    
    paste0("Purchase Probability: ", percentage, "%\n",
           "Confidence: ", ifelse(prob > 0.7, "High", ifelse(prob > 0.4, "Medium", "Low")))
  })
}

# =============================================================================
# RUN APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)
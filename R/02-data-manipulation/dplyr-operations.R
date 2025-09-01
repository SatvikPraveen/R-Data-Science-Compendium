#' @title Advanced Data Manipulation with dplyr
#' @description Professional data wrangling techniques using the tidyverse
#' @author Portfolio Developer
#' @date 2025

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(forcats)

# =============================================================================
# ADVANCED DPLYR OPERATIONS
# =============================================================================

#' Generate Realistic Business Dataset
#'
#' @description Creates a comprehensive business dataset for demonstration
#' @param n_customers Number of customers to generate
#' @param n_products Number of products in catalog
#' @param seed Random seed for reproducibility
#' @return Tibble with realistic business data
#' @export
generate_business_data <- function(n_customers = 1000, n_products = 50, seed = 42) {
  
  set.seed(seed)
  
  # Product catalog
  products <- tibble(
    product_id = paste0("PROD_", str_pad(1:n_products, 3, "left", "0")),
    category = sample(c("Electronics", "Clothing", "Books", "Home", "Sports"), n_products, replace = TRUE),
    subcategory = case_when(
      category == "Electronics" ~ sample(c("Phones", "Laptops", "Accessories"), n_products, replace = TRUE),
      category == "Clothing" ~ sample(c("Shirts", "Pants", "Shoes", "Dresses"), n_products, replace = TRUE),
      category == "Books" ~ sample(c("Fiction", "Non-Fiction", "Technical"), n_products, replace = TRUE),
      category == "Home" ~ sample(c("Kitchen", "Bedroom", "Living Room"), n_products, replace = TRUE),
      category == "Sports" ~ sample(c("Fitness", "Outdoor", "Team Sports"), n_products, replace = TRUE)
    ),
    product_name = paste(category, subcategory, "Item", 1:n_products),
    base_price = round(runif(n_products, 10, 500), 2),
    cost = round(base_price * runif(n_products, 0.4, 0.7), 2),
    weight_kg = round(runif(n_products, 0.1, 5.0), 2),
    launch_date = sample(seq(as.Date("2020-01-01"), as.Date("2024-12-31"), by = "day"), n_products),
    supplier = sample(paste0("Supplier_", LETTERS[1:10]), n_products, replace = TRUE)
  )
  
  # Customer data
  customers <- tibble(
    customer_id = paste0("CUST_", str_pad(1:n_customers, 4, "left", "0")),
    first_name = sample(c("John", "Jane", "Michael", "Sarah", "David", "Emily", "Robert", "Lisa"), n_customers, replace = TRUE),
    last_name = sample(c("Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller", "Davis"), n_customers, replace = TRUE),
    email = paste0(tolower(first_name), ".", tolower(last_name), "@", sample(c("gmail.com", "yahoo.com", "hotmail.com", "company.com"), n_customers, replace = TRUE)),
    age = sample(18:75, n_customers, replace = TRUE),
    gender = sample(c("M", "F", "Other"), n_customers, replace = TRUE, prob = c(0.48, 0.48, 0.04)),
    country = sample(c("USA", "Canada", "UK", "Germany", "France", "Australia"), n_customers, replace = TRUE, prob = c(0.4, 0.15, 0.15, 0.1, 0.1, 0.1)),
    registration_date = sample(seq(as.Date("2020-01-01"), Sys.Date(), by = "day"), n_customers),
    customer_segment = sample(c("Premium", "Standard", "Basic"), n_customers, replace = TRUE, prob = c(0.2, 0.5, 0.3)),
    lifetime_value = round(rnorm(n_customers, 1500, 800), 0),
    is_active = sample(c(TRUE, FALSE), n_customers, replace = TRUE, prob = c(0.8, 0.2))
  )
  
  # Generate transactions (multiple per customer)
  n_transactions <- n_customers * 3  # Average 3 transactions per customer
  
  transactions <- tibble(
    transaction_id = paste0("TXN_", str_pad(1:n_transactions, 6, "left", "0")),
    customer_id = sample(customers$customer_id, n_transactions, replace = TRUE),
    product_id = sample(products$product_id, n_transactions, replace = TRUE),
    transaction_date = sample(seq(as.Date("2023-01-01"), Sys.Date(), by = "day"), n_transactions, replace = TRUE),
    quantity = sample(1:5, n_transactions, replace = TRUE, prob = c(0.5, 0.25, 0.15, 0.07, 0.03)),
    discount_percent = sample(c(0, 5, 10, 15, 20, 25), n_transactions, replace = TRUE, prob = c(0.4, 0.2, 0.2, 0.1, 0.07, 0.03)),
    channel = sample(c("Online", "Mobile", "Store", "Phone"), n_transactions, replace = TRUE, prob = c(0.4, 0.3, 0.25, 0.05)),
    payment_method = sample(c("Credit Card", "Debit Card", "PayPal", "Cash", "Bank Transfer"), n_transactions, replace = TRUE, prob = c(0.4, 0.25, 0.2, 0.1, 0.05)),
    shipping_cost = ifelse(channel %in% c("Online", "Mobile", "Phone"), round(runif(n_transactions, 5, 25), 2), 0)
  )
  
  # Join to add price and calculate totals
  transactions <- transactions %>%
    left_join(products %>% select(product_id, base_price, category), by = "product_id") %>%
    mutate(
      unit_price = base_price * (1 - discount_percent / 100),
      line_total = quantity * unit_price,
      total_amount = line_total + shipping_cost,
      profit_margin = (unit_price - products$cost[match(product_id, products$product_id)]) / unit_price
    )
  
  list(
    customers = customers,
    products = products,
    transactions = transactions
  )
}

#' Advanced Data Filtering and Selection
#'
#' @description Demonstrates complex filtering scenarios
#' @param data Business data list from generate_business_data()
#' @return List of filtered datasets
#' @export
advanced_filtering <- function(data) {
  
  customers <- data$customers
  products <- data$products
  transactions <- data$transactions
  
  # Complex filtering scenarios
  filters <- list(
    
    # High-value customers with multiple conditions
    high_value_customers = customers %>%
      filter(
        lifetime_value > quantile(lifetime_value, 0.8, na.rm = TRUE),
        is_active == TRUE,
        customer_segment == "Premium",
        registration_date < Sys.Date() - months(6)
      ),
    
    # Products needing inventory attention
    inventory_alerts = products %>%
      filter(
        category %in% c("Electronics", "Clothing"),
        base_price > 100,
        launch_date < Sys.Date() - years(1)
      ) %>%
      arrange(desc(base_price)),
    
    # Recent high-value transactions
    recent_big_transactions = transactions %>%
      filter(
        transaction_date >= Sys.Date() - days(30),
        total_amount > quantile(total_amount, 0.9, na.rm = TRUE),
        payment_method %in% c("Credit Card", "PayPal")
      ),
    
    # Seasonal analysis - Q4 performance
    q4_analysis = transactions %>%
      filter(
        quarter(transaction_date) == 4,
        year(transaction_date) == year(Sys.Date())
      ) %>%
      left_join(customers %>% select(customer_id, customer_segment), by = "customer_id"),
    
    # Cross-category purchasers
    cross_category_customers = transactions %>%
      group_by(customer_id) %>%
      summarise(
        categories_purchased = n_distinct(category),
        total_spent = sum(total_amount, na.rm = TRUE),
        avg_order_value = mean(total_amount, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(categories_purchased >= 3) %>%
      arrange(desc(total_spent))
  )
  
  return(filters)
}

#' Advanced Grouping and Summarization
#'
#' @description Complex aggregation patterns and business metrics
#' @param data Business data list
#' @return List of analytical summaries
#' @export
advanced_summarization <- function(data) {
  
  customers <- data$customers
  products <- data$products
  transactions <- data$transactions
  
  summaries <- list(
    
    # Customer lifetime value analysis
    customer_analytics = transactions %>%
      group_by(customer_id) %>%
      summarise(
        first_purchase = min(transaction_date, na.rm = TRUE),
        last_purchase = max(transaction_date, na.rm = TRUE),
        total_transactions = n(),
        total_revenue = sum(total_amount, na.rm = TRUE),
        avg_order_value = mean(total_amount, na.rm = TRUE),
        total_quantity = sum(quantity, na.rm = TRUE),
        unique_products = n_distinct(product_id),
        avg_discount = mean(discount_percent, na.rm = TRUE),
        preferred_channel = names(sort(table(channel), decreasing = TRUE))[1],
        days_between_purchases = as.numeric(last_purchase - first_purchase) / (total_transactions - 1),
        .groups = "drop"
      ) %>%
      left_join(customers %>% select(customer_id, customer_segment, age, country), by = "customer_id") %>%
      mutate(
        customer_tenure_days = as.numeric(Sys.Date() - first_purchase),
        purchase_frequency = total_transactions / (customer_tenure_days / 365.25),
        clv_estimate = total_revenue * purchase_frequency * 2  # Simplified CLV
      ),
    
    # Product performance metrics
    product_analytics = transactions %>%
      group_by(product_id, category) %>%
      summarise(
        total_units_sold = sum(quantity, na.rm = TRUE),
        total_revenue = sum(total_amount, na.rm = TRUE),
        avg_selling_price = mean(unit_price, na.rm = TRUE),
        avg_discount_given = mean(discount_percent, na.rm = TRUE),
        unique_customers = n_distinct(customer_id),
        first_sale = min(transaction_date, na.rm = TRUE),
        last_sale = max(transaction_date, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(products %>% select(product_id, base_price, cost, launch_date), by = "product_id") %>%
      mutate(
        revenue_per_customer = total_revenue / unique_customers,
        price_elasticity = (avg_selling_price - base_price) / base_price,
        profit_per_unit = avg_selling_price - cost,
        total_profit = profit_per_unit * total_units_sold,
        days_since_launch = as.numeric(Sys.Date() - launch_date),
        sales_velocity = total_units_sold / (days_since_launch / 30)  # Units per month
      ),
    
    # Time-based analysis
    temporal_analytics = transactions %>%
      mutate(
        year_month = floor_date(transaction_date, "month"),
        weekday = wday(transaction_date, label = TRUE),
        hour = hour(transaction_date),
        is_weekend = weekday %in% c("Sat", "Sun")
      ) %>%
      group_by(year_month) %>%
      summarise(
        monthly_revenue = sum(total_amount, na.rm = TRUE),
        monthly_transactions = n(),
        unique_customers = n_distinct(customer_id),
        avg_order_value = mean(total_amount, na.rm = TRUE),
        total_units = sum(quantity, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(year_month) %>%
      mutate(
        revenue_growth = (monthly_revenue - lag(monthly_revenue)) / lag(monthly_revenue) * 100,
        transaction_growth = (monthly_transactions - lag(monthly_transactions)) / lag(monthly_transactions) * 100,
        revenue_ma_3 = zoo::rollmean(monthly_revenue, k = 3, fill = NA, align = "right")
      ),
    
    # Channel and payment analysis
    channel_analytics = transactions %>%
      group_by(channel, payment_method) %>%
      summarise(
        transaction_count = n(),
        total_revenue = sum(total_amount, na.rm = TRUE),
        avg_order_value = mean(total_amount, na.rm = TRUE),
        avg_discount = mean(discount_percent, na.rm = TRUE),
        unique_customers = n_distinct(customer_id),
        .groups = "drop"
      ) %>%
      group_by(channel) %>%
      mutate(
        channel_revenue_share = total_revenue / sum(total_revenue) * 100
      ) %>%
      ungroup(),
    
    # Customer segmentation analysis
    segment_analytics = transactions %>%
      left_join(customers %>% select(customer_id, customer_segment, age, country), by = "customer_id") %>%
      group_by(customer_segment, country) %>%
      summarise(
        customer_count = n_distinct(customer_id),
        total_revenue = sum(total_amount, na.rm = TRUE),
        avg_customer_value = total_revenue / customer_count,
        avg_order_value = mean(total_amount, na.rm = TRUE),
        avg_age = mean(age, na.rm = TRUE),
        transaction_frequency = n() / customer_count,
        .groups = "drop"
      ) %>%
      arrange(desc(avg_customer_value))
  )
  
  return(summaries)
}

#' Advanced Data Transformation and Reshaping
#'
#' @description Complex data reshaping operations
#' @param data Business data list
#' @return List of transformed datasets
#' @export
advanced_transformations <- function(data) {
  
  transactions <- data$transactions
  customers <- data$customers
  products <- data$products
  
  transformations <- list(
    
    # Customer-product matrix (wide format)
    customer_product_matrix = transactions %>%
      group_by(customer_id, category) %>%
      summarise(total_spent = sum(total_amount, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from = category,
        values_from = total_spent,
        values_fill = 0,
        names_prefix = "spent_"
      ),
    
    # Time series data preparation
    daily_metrics = transactions %>%
      group_by(transaction_date, channel) %>%
      summarise(
        daily_revenue = sum(total_amount, na.rm = TRUE),
        daily_transactions = n(),
        daily_customers = n_distinct(customer_id),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = channel,
        values_from = c(daily_revenue, daily_transactions, daily_customers),
        values_fill = 0
      ),
    
    # Cohort analysis preparation
    cohort_data = transactions %>%
      left_join(customers %>% select(customer_id, registration_date), by = "customer_id") %>%
      mutate(
        cohort_month = floor_date(registration_date, "month"),
        transaction_month = floor_date(transaction_date, "month"),
        period_number = interval(cohort_month, transaction_month) %/% months(1)
      ) %>%
      group_by(cohort_month, period_number) %>%
      summarise(
        customers = n_distinct(customer_id),
        revenue = sum(total_amount, na.rm = TRUE),
        .groups = "drop"
      ),
    
    # RFM analysis (Recency, Frequency, Monetary)
    rfm_analysis = transactions %>%
      group_by(customer_id) %>%
      summarise(
        last_purchase_date = max(transaction_date, na.rm = TRUE),
        frequency = n(),
        monetary = sum(total_amount, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        recency = as.numeric(Sys.Date() - last_purchase_date),
        recency_score = ntile(desc(recency), 5),
        frequency_score = ntile(frequency, 5),
        monetary_score = ntile(monetary, 5),
        rfm_score = paste0(recency_score, frequency_score, monetary_score),
        customer_value_segment = case_when(
          recency_score >= 4 & frequency_score >= 4 & monetary_score >= 4 ~ "Champions",
          recency_score >= 3 & frequency_score >= 3 & monetary_score >= 3 ~ "Loyal Customers",
          recency_score >= 3 & frequency_score <= 2 & monetary_score >= 3 ~ "Big Spenders",
          recency_score <= 2 & frequency_score >= 3 & monetary_score >= 3 ~ "At Risk",
          recency_score <= 2 & frequency_score <= 2 & monetary_score <= 2 ~ "Lost Customers",
          TRUE ~ "Others"
        )
      ),
    
    # Advanced feature engineering
    customer_features = transactions %>%
      group_by(customer_id) %>%
      arrange(transaction_date) %>%
      summarise(
        # Transaction patterns
        avg_days_between_purchases = mean(diff(as.numeric(transaction_date)), na.rm = TRUE),
        purchase_consistency = sd(diff(as.numeric(transaction_date)), na.rm = TRUE),
        
        # Channel behavior
        primary_channel = names(sort(table(channel), decreasing = TRUE))[1],
        channel_diversity = n_distinct(channel),
        
        # Product preferences
        preferred_category = names(sort(table(category), decreasing = TRUE))[1],
        category_diversity = n_distinct(category),
        
        # Spending patterns
        spending_trend = ifelse(n() >= 3, 
                               cor(1:n(), total_amount, use = "complete.obs"), 
                               NA),
        price_sensitivity = cor(discount_percent, quantity, use = "complete.obs"),
        
        # Timing patterns
        weekend_shopper = mean(wday(transaction_date) %in% c(1, 7), na.rm = TRUE),
        evening_shopper = mean(hour(transaction_date) >= 18, na.rm = TRUE),
        
        .groups = "drop"
      ) %>%
      left_join(customers %>% select(customer_id, age, customer_segment), by = "customer_id")
  )
  
  return(transformations)
}

#' Advanced String and Text Processing
#'
#' @description Complex text manipulation and analysis
#' @param data Business data list
#' @return List of text analysis results
#' @export
advanced_text_processing <- function(data) {
  
  customers <- data$customers
  products <- data$products
  
  text_analysis <- list(
    
    # Email domain analysis
    email_analysis = customers %>%
      mutate(
        email_domain = str_extract(email, "(?<=@)[^.]+"),
        email_provider = case_when(
          str_detect(email, "gmail|yahoo|hotmail|outlook") ~ "Personal",
          str_detect(email, "company|corp|inc|llc") ~ "Corporate",
          TRUE ~ "Other"
        ),
        email_length = str_length(email),
        has_numbers_in_email = str_detect(email, "\\d"),
        email_complexity = str_count(email, "[._-]")
      ) %>%
      group_by(email_provider, country) %>%
      summarise(
        customer_count = n(),
        avg_lifetime_value = mean(lifetime_value, na.rm = TRUE),
        avg_age = mean(age, na.rm = TRUE),
        .groups = "drop"
      ),
    
    # Product name analysis
    product_text_analysis = products %>%
      mutate(
        # Extract key features from product names
        name_length = str_length(product_name),
        word_count = str_count(product_name, "\\S+"),
        has_brand = str_detect(product_name, "^[A-Z][a-z]+"),
        
        # Extract size information
        has_size = str_detect(product_name, "\\b(Small|Medium|Large|XL|XXL|\\d+inch|\\d+cm)\\b"),
        extracted_size = str_extract(product_name, "\\b(Small|Medium|Large|XL|XXL|\\d+inch|\\d+cm)\\b"),
        
        # Extract color information
        has_color = str_detect(product_name, "\\b(Black|White|Red|Blue|Green|Yellow|Brown|Gray|Pink|Purple)\\b"),
        extracted_color = str_extract(product_name, "\\b(Black|White|Red|Blue|Green|Yellow|Brown|Gray|Pink|Purple)\\b"),
        
        # Create standardized product codes
        product_code = paste0(
          str_sub(str_to_upper(category), 1, 3),
          "_",
          str_sub(str_to_upper(subcategory), 1, 3),
          "_",
          str_pad(row_number(), 3, "left", "0")
        )
      ),
    
    # Customer name analysis
    customer_name_analysis = customers %>%
      mutate(
        # Name characteristics
        first_name_length = str_length(first_name),
        last_name_length = str_length(last_name),
        full_name = paste(first_name, last_name),
        initials = paste0(str_sub(first_name, 1, 1), str_sub(last_name, 1, 1)),
        
        # Name patterns
        has_common_first_name = first_name %in% c("John", "Jane", "Michael", "Sarah", "David", "Emily"),
        has_common_last_name = last_name %in% c("Smith", "Johnson", "Williams", "Brown", "Jones"),
        
        # Generate username suggestions
        username_suggestion = str_c(
          str_to_lower(str_sub(first_name, 1, 3)),
          str_to_lower(str_sub(last_name, 1, 3)),
          sample(100:999, n(), replace = TRUE)
        )
      ) %>%
      select(customer_id, first_name, last_name, full_name, initials, username_suggestion, everything())
  )
  
  return(text_analysis)
}

#' Advanced Joining and Relationship Analysis
#'
#' @description Complex join operations and relationship mapping
#' @param data Business data list
#' @return List of joined and analyzed datasets
#' @export
advanced_joins <- function(data) {
  
  customers <- data$customers
  products <- data$products
  transactions <- data$transactions
  
  # Create additional tables for join demonstrations
  
  # Customer preferences (some customers may not have preferences)
  customer_preferences <- tibble(
    customer_id = sample(customers$customer_id, size = nrow(customers) * 0.7),
    preferred_category = sample(c("Electronics", "Clothing", "Books", "Home", "Sports"), 
                               length(customer_id), replace = TRUE),
    communication_preference = sample(c("Email", "SMS", "Phone", "None"), 
                                    length(customer_id), replace = TRUE),
    newsletter_subscribed = sample(c(TRUE, FALSE), length(customer_id), replace = TRUE)
  )
  
  # Product reviews (not all products have reviews)
  product_reviews <- tibble(
    product_id = sample(products$product_id, size = nrow(products) * 0.8),
    avg_rating = round(runif(length(product_id), 1, 5), 1),
    review_count = sample(1:100, length(product_id), replace = TRUE),
    last_review_date = sample(seq(as.Date("2023-01-01"), Sys.Date(), by = "day"), 
                             length(product_id), replace = TRUE)
  )
  
  # Regional data
  regional_data <- tibble(
    country = unique(customers$country),
    region = c("North America", "North America", "Europe", "Europe", "Europe", "Oceania"),
    currency = c("USD", "CAD", "GBP", "EUR", "EUR", "AUD"),
    tax_rate = c(0.08, 0.12, 0.20, 0.19, 0.20, 0.10),
    shipping_days = c(2, 3, 5, 4, 4, 7)
  )
  
  joins <- list(
    
    # Inner join - customers with complete data
    complete_customer_profiles = customers %>%
      inner_join(customer_preferences, by = "customer_id") %>%
      inner_join(regional_data, by = "country") %>%
      mutate(
        estimated_tax = lifetime_value * tax_rate,
        profile_completeness = "Complete"
      ),
    
    # Left join - all customers with optional preferences
    all_customers_with_preferences = customers %>%
      left_join(customer_preferences, by = "customer_id") %>%
      left_join(regional_data, by = "country") %>%
      mutate(
        has_preferences = !is.na(preferred_category),
        profile_completeness = ifelse(has_preferences, "Complete", "Basic")
      ),
    
    # Right join - all preferences (might have invalid customer IDs)
    preference_focused = customer_preferences %>%
      right_join(customers, by = "customer_id"),
    
    # Full join - comprehensive view
    comprehensive_customer_view = customers %>%
      full_join(customer_preferences, by = "customer_id") %>%
      full_join(regional_data, by = "country"),
    
    # Complex multi-table join with aggregation
    customer_transaction_summary = customers %>%
      left_join(
        transactions %>%
          group_by(customer_id) %>%
          summarise(
            total_orders = n(),
            total_spent = sum(total_amount, na.rm = TRUE),
            avg_order_value = mean(total_amount, na.rm = TRUE),
            last_order_date = max(transaction_date, na.rm = TRUE),
            favorite_channel = names(sort(table(channel), decreasing = TRUE))[1],
            .groups = "drop"
          ),
        by = "customer_id"
      ) %>%
      left_join(customer_preferences, by = "customer_id") %>%
      left_join(regional_data, by = "country") %>%
      mutate(
        days_since_last_order = as.numeric(Sys.Date() - last_order_date),
        customer_status = case_when(
          is.na(last_order_date) ~ "Never Purchased",
          days_since_last_order <= 30 ~ "Active",
          days_since_last_order <= 90 ~ "Recent",
          days_since_last_order <= 365 ~ "Inactive",
          TRUE ~ "Lost"
        )
      ),
    
    # Product analysis with reviews and sales
    product_performance = products %>%
      left_join(product_reviews, by = "product_id") %>%
      left_join(
        transactions %>%
          group_by(product_id) %>%
          summarise(
            units_sold = sum(quantity, na.rm = TRUE),
            revenue = sum(total_amount, na.rm = TRUE),
            avg_discount = mean(discount_percent, na.rm = TRUE),
            first_sale = min(transaction_date, na.rm = TRUE),
            last_sale = max(transaction_date, na.rm = TRUE),
            .groups = "drop"
          ),
        by = "product_id"
      ) %>%
      mutate(
        has_reviews = !is.na(avg_rating),
        has_sales = !is.na(units_sold),
        performance_category = case_when(
          has_sales & has_reviews & avg_rating >= 4 & units_sold >= 10 ~ "Star Product",
          has_sales & units_sold >= 10 ~ "Good Seller",
          has_reviews & avg_rating >= 4 ~ "Well Reviewed",
          has_sales ~ "Some Sales",
          TRUE ~ "No Traction"
        ),
        revenue_per_review = ifelse(review_count > 0, revenue / review_count, NA),
        days_since_launch = as.numeric(Sys.Date() - launch_date)
      ),
    
    # Anti-join examples - finding missing relationships
    customers_without_preferences = customers %>%
      anti_join(customer_preferences, by = "customer_id"),
    
    products_without_reviews = products %>%
      anti_join(product_reviews, by = "product_id"),
    
    # Semi-join examples - filtering based on existence
    customers_with_recent_activity = customers %>%
      semi_join(
        transactions %>% filter(transaction_date >= Sys.Date() - days(30)), 
        by = "customer_id"
      ),
    
    products_recently_sold = products %>%
      semi_join(
        transactions %>% filter(transaction_date >= Sys.Date() - days(90)), 
        by = "product_id"
      )
  )
  
  return(joins)
}

#' Comprehensive Data Quality Assessment
#'
#' @description Thorough data quality analysis and cleaning recommendations
#' @param data Business data list
#' @return List of data quality metrics and recommendations
#' @export
data_quality_assessment <- function(data) {
  
  customers <- data$customers
  products <- data$products
  transactions <- data$transactions
  
  quality_report <- list(
    
    # Missing data analysis
    missing_data = list(
      customers = customers %>%
        summarise(across(everything(), ~ sum(is.na(.x)))) %>%
        pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
        mutate(missing_percentage = missing_count / nrow(customers) * 100),
      
      products = products %>%
        summarise(across(everything(), ~ sum(is.na(.x)))) %>%
        pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
        mutate(missing_percentage = missing_count / nrow(products) * 100),
      
      transactions = transactions %>%
        summarise(across(everything(), ~ sum(is.na(.x)))) %>%
        pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
        mutate(missing_percentage = missing_count / nrow(transactions) * 100)
    ),
    
    # Duplicate analysis
    duplicates = list(
      duplicate_customers = customers %>%
        group_by(email) %>%
        filter(n() > 1) %>%
        arrange(email),
      
      duplicate_transactions = transactions %>%
        group_by(customer_id, product_id, transaction_date, total_amount) %>%
        filter(n() > 1) %>%
        arrange(customer_id, transaction_date)
    ),
    
    # Data consistency checks
    consistency_checks = list(
      # Age consistency
      age_issues = customers %>%
        filter(age < 0 | age > 120) %>%
        select(customer_id, age),
      
      # Price consistency
      price_issues = products %>%
        filter(base_price <= cost | base_price <= 0 | cost <= 0) %>%
        select(product_id, base_price, cost),
      
      # Date consistency
      date_issues = transactions %>%
        filter(transaction_date > Sys.Date() | transaction_date < as.Date("2020-01-01")) %>%
        select(transaction_id, transaction_date),
      
      # Quantity consistency
      quantity_issues = transactions %>%
        filter(quantity <= 0 | quantity > 100) %>%
        select(transaction_id, quantity)
    ),
    
    # Outlier detection
    outliers = list(
      # Customer outliers
      customer_outliers = customers %>%
        mutate(
          age_zscore = abs((age - mean(age, na.rm = TRUE)) / sd(age, na.rm = TRUE)),
          ltv_zscore = abs((lifetime_value - mean(lifetime_value, na.rm = TRUE)) / sd(lifetime_value, na.rm = TRUE))
        ) %>%
        filter(age_zscore > 3 | ltv_zscore > 3),
      
      # Transaction outliers
      transaction_outliers = transactions %>%
        mutate(
          amount_zscore = abs((total_amount - mean(total_amount, na.rm = TRUE)) / sd(total_amount, na.rm = TRUE)),
          quantity_zscore = abs((quantity - mean(quantity, na.rm = TRUE)) / sd(quantity, na.rm = TRUE))
        ) %>%
        filter(amount_zscore > 3 | quantity_zscore > 3)
    ),
    
    # Data distribution analysis
    distributions = list(
      customer_age_distribution = customers %>%
        mutate(age_group = cut(age, breaks = c(0, 25, 35, 45, 55, 65, Inf), 
                              labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "65+"))) %>%
        count(age_group),
      
      transaction_amount_distribution = transactions %>%
        mutate(amount_range = cut(total_amount, breaks = c(0, 50, 100, 200, 500, Inf),
                                 labels = c("$0-50", "$51-100", "$101-200", "$201-500", "$500+"))) %>%
        count(amount_range),
      
      channel_distribution = transactions %>%
        count(channel, sort = TRUE)
    ),
    
    # Referential integrity checks
    referential_integrity = list(
      orphaned_transactions = transactions %>%
        anti_join(customers, by = "customer_id") %>%
        nrow(),
      
      invalid_products = transactions %>%
        anti_join(products, by = "product_id") %>%
        nrow(),
      
      customers_without_transactions = customers %>%
        anti_join(transactions, by = "customer_id") %>%
        nrow()
    )
  )
  
  # Generate recommendations
  recommendations <- list(
    "Remove or impute missing values in critical fields",
    "Investigate and resolve duplicate records",
    "Implement data validation rules for age, prices, and dates",
    "Review outliers for data entry errors",
    "Ensure referential integrity between related tables",
    "Consider standardizing categorical variables",
    "Implement regular data quality monitoring"
  )
  
  list(
    quality_report = quality_report,
    recommendations = recommendations,
    summary = list(
      total_records = nrow(customers) + nrow(products) + nrow(transactions),
      data_quality_score = 85,  # Placeholder calculation
      critical_issues = sum(sapply(quality_report$consistency_checks, nrow)),
      last_assessed = Sys.time()
    )
  )
}

#' Run Complete Data Manipulation Demonstration
#'
#' @description Executes all data manipulation techniques
#' @param verbose Logical, whether to show detailed output
#' @return Invisible list of all results
#' @export
run_data_manipulation_demo <- function(verbose = TRUE) {
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("    ADVANCED DATA MANIPULATION PORTFOLIO\n")
    cat("="*60, "\n\n")
  }
  
  # Generate sample data
  if (verbose) cat("1. Generating business dataset...\n")
  business_data <- generate_business_data(n_customers = 500, n_products = 30)
  
  # Run all demonstrations
  if (verbose) cat("2. Advanced filtering operations...\n")
  filters <- advanced_filtering(business_data)
  
  if (verbose) cat("3. Complex summarization and aggregation...\n")
  summaries <- advanced_summarization(business_data)
  
  if (verbose) cat("4. Data transformation and reshaping...\n")
  transformations <- advanced_transformations(business_data)
  
  if (verbose) cat("5. Text processing and analysis...\n")
  text_analysis <- advanced_text_processing(business_data)
  
  if (verbose) cat("6. Advanced joining operations...\n")
  joins <- advanced_joins(business_data)
  
  if (verbose) cat("7. Data quality assessment...\n")
  quality <- data_quality_assessment(business_data)
  
  results <- list(
    original_data = business_data,
    filtered_data = filters,
    summaries = summaries,
    transformations = transformations,
    text_analysis = text_analysis,
    joins = joins,
    quality_assessment = quality
  )
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("    DATA MANIPULATION DEMONSTRATION COMPLETE\n")
    cat("="*60, "\n")
    cat("Customers processed:", nrow(business_data$customers), "\n")
    cat("Products analyzed:", nrow(business_data$products), "\n")
    cat("Transactions processed:", nrow(business_data$transactions), "\n")
    cat("Total analyses performed:", length(unlist(results, recursive = FALSE)), "\n")
  }
  
  invisible(results)
}

# =============================================================================
# EXAMPLE USAGE (Run only in interactive mode)
# =============================================================================

if (interactive()) {
  # Run the complete demonstration
  demo_results <- run_data_manipulation_demo(verbose = TRUE)
  
  # Example: Access specific results
  # View high-value customers
  # View(demo_results$filtered_data$high_value_customers)
  
  # View customer analytics
  # View(demo_results$summaries$customer_analytics)
  
  # View RFM analysis
  # View(demo_results$transformations$rfm_analysis)
}
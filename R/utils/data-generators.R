#' @title Professional Data Generators for Testing and Demonstration
#' @description Comprehensive data generation utilities for portfolio demonstrations
#' @author Portfolio Developer
#' @date 2025

library(dplyr)
library(lubridate)

# =============================================================================
# BUSINESS DATA GENERATORS
# =============================================================================

#' Generate Realistic Customer Data
#'
#' @description Creates synthetic customer data with realistic patterns and relationships
#' @param n_customers Number of customers to generate
#' @param seed Random seed for reproducibility
#' @param start_date Start date for customer registration
#' @param end_date End date for customer registration
#' @return Data frame with customer information
#' @export
generate_customer_data <- function(n_customers = 1000, 
                                 seed = 42, 
                                 start_date = as.Date("2020-01-01"),
                                 end_date = Sys.Date()) {
  
  set.seed(seed)
  
  # Demographics configuration
  first_names <- c("James", "Mary", "John", "Patricia", "Robert", "Jennifer", "Michael", "Linda",
                   "William", "Elizabeth", "David", "Barbara", "Richard", "Susan", "Joseph", "Jessica",
                   "Thomas", "Sarah", "Christopher", "Karen", "Charles", "Nancy", "Daniel", "Lisa",
                   "Matthew", "Betty", "Anthony", "Helen", "Mark", "Sandra", "Donald", "Donna")
  
  last_names <- c("Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller", "Davis",
                  "Rodriguez", "Martinez", "Hernandez", "Lopez", "Gonzalez", "Wilson", "Anderson", "Thomas",
                  "Taylor", "Moore", "Jackson", "Martin", "Lee", "Perez", "Thompson", "White",
                  "Harris", "Sanchez", "Clark", "Ramirez", "Lewis", "Robinson", "Walker", "Young")
  
  # Generate base customer data
  customers <- tibble(
    customer_id = paste0("CUST_", str_pad(1:n_customers, width = 6, side = "left", pad = "0")),
    first_name = sample(first_names, n_customers, replace = TRUE),
    last_name = sample(last_names, n_customers, replace = TRUE),
    
    # Age with realistic distribution
    age = pmax(18, pmin(85, round(rnorm(n_customers, mean = 42, sd = 15)))),
    
    # Gender with slight female bias (realistic)
    gender = sample(c("Male", "Female", "Other"), n_customers, replace = TRUE, 
                   prob = c(0.48, 0.51, 0.01)),
    
    # Geographic distribution
    country = sample(c("USA", "Canada", "UK", "Germany", "France", "Australia", "Netherlands", "Sweden"),
                    n_customers, replace = TRUE, 
                    prob = c(0.45, 0.15, 0.12, 0.08, 0.07, 0.06, 0.04, 0.03)),
    
    # Registration date
    registration_date = sample(seq(start_date, end_date, by = "day"), n_customers, replace = TRUE)
  ) %>%
  
  # Derived fields with realistic relationships
  mutate(
    # Email generation
    email = paste0(
      tolower(str_sub(first_name, 1, 1)),
      tolower(last_name),
      sample(c("", ".", "_"), n_customers, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
      sample(c("", 1:99), n_customers, replace = TRUE, prob = c(0.6, rep(0.4/99, 99))),
      "@",
      case_when(
        country == "USA" ~ sample(c("gmail.com", "yahoo.com", "hotmail.com", "outlook.com"), n_customers, replace = TRUE),
        country == "UK" ~ sample(c("gmail.com", "yahoo.co.uk", "hotmail.co.uk", "btinternet.com"), n_customers, replace = TRUE),
        country == "Germany" ~ sample(c("gmail.com", "gmx.de", "web.de", "t-online.de"), n_customers, replace = TRUE),
        TRUE ~ sample(c("gmail.com", "yahoo.com", "hotmail.com"), n_customers, replace = TRUE)
      )
    ),
    
    # Income with age and country relationships
    base_income = case_when(
      country == "USA" ~ 55000,
      country == "Canada" ~ 50000,
      country == "UK" ~ 45000,
      country == "Germany" ~ 48000,
      country == "France" ~ 42000,
      country == "Australia" ~ 52000,
      TRUE ~ 40000
    ),
    
    # Age-adjusted income with some randomness
    annual_income = pmax(20000, round(
      base_income * 
      (0.7 + 0.6 * pmin(1, (age - 22) / 40)) *  # Career progression
      exp(rnorm(n_customers, 0, 0.3))            # Individual variation
    )),
    
    # Education level (age-related)
    education = case_when(
      age >= 65 ~ sample(c("High School", "Bachelor", "Master", "PhD"), n_customers, replace = TRUE,
                        prob = c(0.5, 0.3, 0.15, 0.05)),
      age >= 45 ~ sample(c("High School", "Bachelor", "Master", "PhD"), n_customers, replace = TRUE,
                        prob = c(0.4, 0.35, 0.2, 0.05)),
      age >= 30 ~ sample(c("High School", "Bachelor", "Master", "PhD"), n_customers, replace = TRUE,
                        prob = c(0.3, 0.4, 0.25, 0.05)),
      TRUE ~ sample(c("High School", "Bachelor", "Master", "PhD"), n_customers, replace = TRUE,
                   prob = c(0.25, 0.45, 0.25, 0.05))
    ),
    
    # Customer segment based on income and behavior
    customer_segment = case_when(
      annual_income >= 80000 ~ sample(c("Premium", "Gold"), n_customers, replace = TRUE, prob = c(0.7, 0.3)),
      annual_income >= 50000 ~ sample(c("Standard", "Gold"), n_customers, replace = TRUE, prob = c(0.8, 0.2)),
      TRUE ~ sample(c("Basic", "Standard"), n_customers, replace = TRUE, prob = c(0.7, 0.3))
    ),
    
    # Customer satisfaction (correlated with segment)
    satisfaction_score = case_when(
      customer_segment == "Premium" ~ pmax(1, pmin(10, rnorm(n_customers, 8.2, 1.2))),
      customer_segment == "Gold" ~ pmax(1, pmin(10, rnorm(n_customers, 7.8, 1.3))),
      customer_segment == "Standard" ~ pmax(1, pmin(10, rnorm(n_customers, 7.0, 1.5))),
      TRUE ~ pmax(1, pmin(10, rnorm(n_customers, 6.2, 1.7)))
    ),
    
    # Engagement metrics
    email_opens_last_month = rpois(n_customers, lambda = ifelse(customer_segment %in% c("Premium", "Gold"), 8, 4)),
    website_visits_last_month = rpois(n_customers, lambda = ifelse(customer_segment %in% c("Premium", "Gold"), 12, 6)),
    
    # Account status
    is_active = case_when(
      customer_segment == "Premium" ~ sample(c(TRUE, FALSE), n_customers, replace = TRUE, prob = c(0.95, 0.05)),
      customer_segment == "Gold" ~ sample(c(TRUE, FALSE), n_customers, replace = TRUE, prob = c(0.92, 0.08)),
      customer_segment == "Standard" ~ sample(c(TRUE, FALSE), n_customers, replace = TRUE, prob = c(0.87, 0.13)),
      TRUE ~ sample(c(TRUE, FALSE), n_customers, replace = TRUE, prob = c(0.78, 0.22))
    ),
    
    # Customer lifetime value (estimated)
    estimated_clv = round(annual_income * 0.1 * 
                         ifelse(customer_segment == "Premium", 1.5,
                                ifelse(customer_segment == "Gold", 1.3, 
                                      ifelse(customer_segment == "Standard", 1.0, 0.7))))
  ) %>%
  
  select(-base_income)  # Remove temporary variable
  
  return(customers)
}

#' Generate Realistic Product Catalog
#'
#' @description Creates synthetic product data with realistic categories and pricing
#' @param n_products Number of products to generate
#' @param seed Random seed for reproducibility
#' @return Data frame with product information
#' @export
generate_product_catalog <- function(n_products = 500, seed = 42) {
  
  set.seed(seed)
  
  # Product categories and their characteristics
  categories <- list(
    "Electronics" = list(
      subcategories = c("Smartphones", "Laptops", "Tablets", "Headphones", "Smart Home", "Gaming"),
      price_range = c(50, 2500),
      cost_margin = c(0.6, 0.8)  # cost as proportion of price
    ),
    "Clothing" = list(
      subcategories = c("Shirts", "Pants", "Dresses", "Shoes", "Accessories", "Outerwear"),
      price_range = c(15, 300),
      cost_margin = c(0.4, 0.6)
    ),
    "Books" = list(
      subcategories = c("Fiction", "Non-Fiction", "Technical", "Educational", "Children", "Self-Help"),
      price_range = c(8, 80),
      cost_margin = c(0.5, 0.7)
    ),
    "Home & Garden" = list(
      subcategories = c("Furniture", "Kitchen", "Bathroom", "Garden", "Decor", "Storage"),
      price_range = c(20, 1200),
      cost_margin = c(0.55, 0.75)
    ),
    "Sports & Fitness" = list(
      subcategories = c("Exercise Equipment", "Outdoor Sports", "Team Sports", "Fitness Apparel", "Supplements"),
      price_range = c(10, 800),
      cost_margin = c(0.5, 0.7)
    ),
    "Beauty & Health" = list(
      subcategories = c("Skincare", "Makeup", "Hair Care", "Vitamins", "Personal Care", "Fragrance"),
      price_range = c(5, 200),
      cost_margin = c(0.3, 0.6)
    )
  )
  
  # Generate products
  products <- tibble(
    product_id = paste0("PROD_", str_pad(1:n_products, width = 6, side = "left", pad = "0"))
  ) %>%
  
  # Assign categories
  mutate(
    category = sample(names(categories), n_products, replace = TRUE,
                     prob = c(0.25, 0.20, 0.15, 0.15, 0.12, 0.13)),
    
    # Assign subcategories based on category
    subcategory = map2_chr(category, 1:n_products, ~{
      sample(categories[[.x]]$subcategories, 1)
    }),
    
    # Generate product names
    product_name = paste(
      sample(c("Premium", "Deluxe", "Standard", "Basic", "Pro", "Elite", "Smart", "Essential"), 
             n_products, replace = TRUE, prob = c(0.15, 0.12, 0.20, 0.15, 0.12, 0.08, 0.10, 0.08)),
      subcategory,
      sample(c("Series", "Collection", "Edition", "Model", "Kit", "Set", "System", ""), 
             n_products, replace = TRUE, prob = c(0.15, 0.12, 0.10, 0.15, 0.12, 0.08, 0.08, 0.20)),
      sample(c("", "V2", "Pro", "Plus", "Max", "Mini", "2024", "X"), 
             n_products, replace = TRUE, prob = c(0.40, 0.08, 0.10, 0.12, 0.08, 0.07, 0.10, 0.05))
    ),
    
    # Generate prices based on category
    base_price = map2_dbl(category, 1:n_products, ~{
      price_range <- categories[[.x]]$price_range
      runif(1, price_range[1], price_range[2])
    }),
    
    # Round prices to realistic values
    price = case_when(
      base_price < 10 ~ round(base_price, 2),
      base_price < 100 ~ round(base_price / 5) * 5,
      base_price < 500 ~ round(base_price / 10) * 10,
      TRUE ~ round(base_price / 50) * 50
    ),
    
    # Calculate cost based on category margins
    cost = map2_dbl(category, price, ~{
      margin_range <- categories[[.x]]$cost_margin
      margin <- runif(1, margin_range[1], margin_range[2])
      round(.y * margin, 2)
    }),
    
    # Product attributes
    brand = sample(c("TechCorp", "StyleBrand", "HomePlus", "ActiveLife", "BeautyCore", "BookWorld", 
                    "GadgetPro", "FashionForward", "LivingSpace", "FitZone", "GlowUp", "ReadMore"),
                  n_products, replace = TRUE),
    
    # Launch date (newer products in tech, older in books)
    launch_date = case_when(
      category == "Electronics" ~ sample(seq(as.Date("2022-01-01"), Sys.Date(), by = "day"), n_products, replace = TRUE),
      category == "Books" ~ sample(seq(as.Date("2018-01-01"), Sys.Date(), by = "day"), n_products, replace = TRUE),
      TRUE ~ sample(seq(as.Date("2020-01-01"), Sys.Date(), by = "day"), n_products, replace = TRUE)
    ),
    
    # Inventory status
    in_stock = sample(c(TRUE, FALSE), n_products, replace = TRUE, prob = c(0.85, 0.15)),
    stock_quantity = ifelse(in_stock, 
                           sample(0:500, n_products, replace = TRUE), 
                           0),
    
    # Product ratings and reviews
    avg_rating = round(pmax(1, pmin(5, rnorm(n_products, 4.1, 0.8))), 1),
    review_count = rpois(n_products, lambda = ifelse(category == "Electronics", 45, 25)),
    
    # Weight for shipping calculations
    weight_kg = case_when(
      category == "Electronics" ~ round(runif(n_products, 0.1, 5.0), 2),
      category == "Clothing" ~ round(runif(n_products, 0.2, 2.0), 2),
      category == "Books" ~ round(runif(n_products, 0.2, 1.5), 2),
      category == "Home & Garden" ~ round(runif(n_products, 0.5, 25.0), 2),
      category == "Sports & Fitness" ~ round(runif(n_products, 0.3, 15.0), 2),
      TRUE ~ round(runif(n_products, 0.1, 2.0), 2)
    )
  ) %>%
  
  # Clean up product names
  mutate(
    product_name = str_trim(str_replace_all(product_name, "\\s+", " ")),
    profit_margin = round((price - cost) / price * 100, 1)
  ) %>%
  
  select(-base_price)  # Remove temporary variable
  
  return(products)
}

#' Generate Realistic Transaction Data
#'
#' @description Creates synthetic transaction data with realistic patterns
#' @param customers Customer data frame
#' @param products Product data frame
#' @param n_transactions Number of transactions to generate
#' @param start_date Start date for transactions
#' @param end_date End date for transactions
#' @param seed Random seed for reproducibility
#' @return Data frame with transaction information
#' @export
generate_transaction_data <- function(customers, products, 
                                    n_transactions = 5000,
                                    start_date = as.Date("2023-01-01"),
                                    end_date = Sys.Date(),
                                    seed = 42) {
  
  set.seed(seed)
  
  # Transaction probability weights based on customer segment
  customer_weights <- case_when(
    customers$customer_segment == "Premium" ~ 3.0,
    customers$customer_segment == "Gold" ~ 2.5,
    customers$customer_segment == "Standard" ~ 1.5,
    TRUE ~ 1.0
  )
  
  # Product popularity weights
  product_weights <- case_when(
    products$avg_rating >= 4.5 ~ 2.0,
    products$avg_rating >= 4.0 ~ 1.5,
    products$avg_rating >= 3.5 ~ 1.2,
    TRUE ~ 1.0
  ) * ifelse(products$in_stock, 1.0, 0.1)  # Heavily weight in-stock items
  
  transactions <- tibble(
    transaction_id = paste0("TXN_", str_pad(1:n_transactions, width = 8, side = "left", pad = "0")),
    
    # Customer selection (weighted by segment)
    customer_id = sample(customers$customer_id, n_transactions, replace = TRUE, prob = customer_weights),
    
    # Product selection (weighted by rating and stock)
    product_id = sample(products$product_id, n_transactions, replace = TRUE, prob = product_weights),
    
    # Transaction date with seasonal patterns
    transaction_date = sample(seq(start_date, end_date, by = "day"), n_transactions, replace = TRUE,
                             prob = generate_seasonal_weights(start_date, end_date))
  ) %>%
  
  # Join customer and product information
  left_join(customers %>% select(customer_id, customer_segment, country, annual_income), by = "customer_id") %>%
  left_join(products %>% select(product_id, category, price, weight_kg), by = "product_id") %>%
  
  # Generate transaction details
  mutate(
    # Quantity (segment-based)
    quantity = case_when(
      customer_segment == "Premium" ~ sample(1:5, n_transactions, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.15, 0.05)),
      customer_segment == "Gold" ~ sample(1:4, n_transactions, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
      customer_segment == "Standard" ~ sample(1:3, n_transactions, replace = TRUE, prob = c(0.5, 0.35, 0.15)),
      TRUE ~ sample(1:2, n_transactions, replace = TRUE, prob = c(0.7, 0.3))
    ),
    
    # Discount (segment and seasonal-based)
    discount_percent = case_when(
      customer_segment == "Premium" ~ sample(c(0, 5, 10, 15), n_transactions, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
      customer_segment == "Gold" ~ sample(c(0, 5, 10), n_transactions, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
      customer_segment == "Standard" ~ sample(c(0, 5), n_transactions, replace = TRUE, prob = c(0.7, 0.3)),
      TRUE ~ sample(c(0, 5), n_transactions, replace = TRUE, prob = c(0.8, 0.2))
    ),
    
    # Sales channel (segment-based preferences)
    channel = case_when(
      customer_segment == "Premium" ~ sample(c("Online", "Mobile", "Store", "Phone"), n_transactions, replace = TRUE, 
                                           prob = c(0.4, 0.35, 0.2, 0.05)),
      customer_segment == "Gold" ~ sample(c("Online", "Mobile", "Store", "Phone"), n_transactions, replace = TRUE,
                                        prob = c(0.45, 0.3, 0.2, 0.05)),
      customer_segment == "Standard" ~ sample(c("Online", "Mobile", "Store", "Phone"), n_transactions, replace = TRUE,
                                            prob = c(0.35, 0.25, 0.35, 0.05)),
      TRUE ~ sample(c("Online", "Mobile", "Store", "Phone"), n_transactions, replace = TRUE,
                   prob = c(0.3, 0.2, 0.45, 0.05))
    ),
    
    # Payment method (country and segment-based)
    payment_method = case_when(
      country == "Germany" ~ sample(c("Credit Card", "Debit Card", "Bank Transfer", "PayPal"), n_transactions, replace = TRUE,
                                   prob = c(0.3, 0.4, 0.2, 0.1)),
      country %in% c("USA", "Canada") ~ sample(c("Credit Card", "Debit Card", "PayPal", "Apple Pay"), n_transactions, replace = TRUE,
                                              prob = c(0.45, 0.25, 0.2, 0.1)),
      TRUE ~ sample(c("Credit Card", "Debit Card", "PayPal", "Bank Transfer"), n_transactions, replace = TRUE,
                   prob = c(0.4, 0.3, 0.2, 0.1))
    ),
    
    # Calculate amounts
    unit_price = price * (1 - discount_percent / 100),
    subtotal = quantity * unit_price,
    
    # Shipping cost (channel and weight-based)
    shipping_cost = case_when(
      channel == "Store" ~ 0,
      weight_kg * quantity <= 2 ~ 5.99,
      weight_kg * quantity <= 10 ~ 9.99,
      TRUE ~ 15.99
    ),
    
    # Tax (country-based)
    tax_rate = case_when(
      country == "USA" ~ 0.08,
      country == "Canada" ~ 0.13,
      country == "UK" ~ 0.20,
      country == "Germany" ~ 0.19,
      country == "France" ~ 0.20,
      TRUE ~ 0.10
    ),
    
    tax_amount = round(subtotal * tax_rate, 2),
    total_amount = round(subtotal + shipping_cost + tax_amount, 2),
    
    # Transaction status (mostly successful)
    status = sample(c("Completed", "Pending", "Cancelled", "Refunded"), n_transactions, replace = TRUE,
                   prob = c(0.87, 0.05, 0.05, 0.03)),
    
    # Processing time (channel-based)
    processing_time_hours = case_when(
      channel == "Online" ~ round(runif(n_transactions, 0.5, 4), 1),
      channel == "Mobile" ~ round(runif(n_transactions, 0.3, 3), 1),
      channel == "Store" ~ round(runif(n_transactions, 0.1, 0.5), 1),
      TRUE ~ round(runif(n_transactions, 2, 24), 1)
    )
  ) %>%
  
  # Remove temporary columns
  select(-customer_segment, -country, -annual_income, -category, -price, -weight_kg)
  
  return(transactions)
}

# Helper function for seasonal transaction patterns
generate_seasonal_weights <- function(start_date, end_date) {
  
  dates <- seq(start_date, end_date, by = "day")
  weights <- numeric(length(dates))
  
  for (i in seq_along(dates)) {
    date <- dates[i]
    month <- month(date)
    day_of_week <- wday(date)
    
    # Base weight
    weight <- 1.0
    
    # Seasonal adjustments
    weight <- weight * case_when(
      month %in% c(11, 12) ~ 1.8,  # Holiday shopping
      month %in% c(6, 7, 8) ~ 1.2,  # Summer
      month %in% c(1, 2) ~ 0.8,     # Post-holiday lull
      TRUE ~ 1.0
    )
    
    # Day of week adjustments
    weight <- weight * case_when(
      day_of_week %in% c(1, 7) ~ 1.3,  # Weekend
      day_of_week == 6 ~ 1.4,          # Friday
      day_of_week == 2 ~ 0.9,          # Monday
      TRUE ~ 1.0
    )
    
    weights[i] <- weight
  }
  
  return(weights / sum(weights))
}

# =============================================================================
# SPECIALIZED DATA GENERATORS
# =============================================================================

#' Generate Time Series Data
#'
#' @description Creates realistic time series data with trends and seasonality
#' @param start_date Start date for time series
#' @param end_date End date for time series
#' @param frequency Data frequency ("daily", "weekly", "monthly")
#' @param trend_type Type of trend ("linear", "exponential", "logarithmic", "none")
#' @param seasonality_strength Strength of seasonal pattern (0-1)
#' @param noise_level Amount of random noise (0-1)
#' @param seed Random seed
#' @return Data frame with time series data
#' @export
generate_timeseries_data <- function(start_date = as.Date("2020-01-01"),
                                    end_date = Sys.Date(),
                                    frequency = "daily",
                                    trend_type = "linear",
                                    seasonality_strength = 0.3,
                                    noise_level = 0.2,
                                    seed = 42) {
  
  set.seed(seed)
  
  # Generate date sequence
  if (frequency == "daily") {
    dates <- seq(start_date, end_date, by = "day")
  } else if (frequency == "weekly") {
    dates <- seq(start_date, end_date, by = "week")
  } else if (frequency == "monthly") {
    dates <- seq(start_date, end_date, by = "month")
  }
  
  n <- length(dates)
  time_index <- 1:n
  
  # Generate trend component
  trend <- switch(trend_type,
    "linear" = time_index * 0.1,
    "exponential" = exp(time_index * 0.001),
    "logarithmic" = log(time_index + 1) * 10,
    "none" = rep(0, n),
    time_index * 0.1  # default to linear
  )
  
  # Generate seasonal component
  if (frequency == "daily") {
    seasonal_period <- 365.25
  } else if (frequency == "weekly") {
    seasonal_period <- 52
  } else {
    seasonal_period <- 12
  }
  
  seasonal <- seasonality_strength * sin(2 * pi * time_index / seasonal_period) +
              seasonality_strength * 0.5 * sin(4 * pi * time_index / seasonal_period)
  
  # Generate noise
  noise <- rnorm(n, 0, noise_level)
  
  # Combine components
  base_value <- 100  # Base level
  value <- base_value + trend + seasonal + noise
  
  # Create additional realistic variables
  data <- tibble(
    date = dates,
    value = round(value, 2),
    trend_component = round(trend, 2),
    seasonal_component = round(seasonal, 2),
    noise_component = round(noise, 2),
    
    # Moving averages
    ma_7 = zoo::rollmean(value, k = min(7, n), fill = NA, align = "right"),
    ma_30 = zoo::rollmean(value, k = min(30, n), fill = NA, align = "right"),
    
    # Percent change
    pct_change = round((value - lag(value)) / lag(value) * 100, 2),
    
    # Cumulative sum
    cumulative_value = cumsum(value - base_value),
    
    # Volatility (rolling standard deviation)
    volatility = zoo::rollapply(value, width = min(30, n), FUN = sd, fill = NA, align = "right")
  )
  
  return(data)
}

#' Generate A/B Test Data
#'
#' @description Creates realistic A/B test experiment data
#' @param n_participants Number of participants
#' @param conversion_rate_control Baseline conversion rate for control group
#' @param lift_treatment Expected lift in treatment group (as decimal)
#' @param test_duration_days Duration of test in days
#' @param seed Random seed
#' @return Data frame with A/B test results
#' @export
generate_ab_test_data <- function(n_participants = 1000,
                                 conversion_rate_control = 0.12,
                                 lift_treatment = 0.15,
                                 test_duration_days = 30,
                                 seed = 42) {
  
  set.seed(seed)
  
  # Calculate treatment conversion rate
  conversion_rate_treatment <- conversion_rate_control * (1 + lift_treatment)
  
  # Generate participant data
  participants <- tibble(
    participant_id = paste0("USER_", str_pad(1:n_participants, width = 6, side = "left", pad = "0")),
    
    # Random assignment to groups (50/50 split)
    group = sample(c("Control", "Treatment"), n_participants, replace = TRUE),
    
    # Entry date (spread over test duration)
    entry_date = sample(seq(as.Date("2024-01-01"), 
                           as.Date("2024-01-01") + test_duration_days - 1, 
                           by = "day"), 
                       n_participants, replace = TRUE),
    
    # User characteristics that might affect conversion
    age = round(rnorm(n_participants, 35, 12)),
    device_type = sample(c("Desktop", "Mobile", "Tablet"), n_participants, replace = TRUE,
                        prob = c(0.45, 0.45, 0.1)),
    traffic_source = sample(c("Organic", "Paid Search", "Social", "Email", "Direct"), 
                           n_participants, replace = TRUE,
                           prob = c(0.3, 0.25, 0.2, 0.15, 0.1)),
    
    # Session metrics
    session_duration_minutes = round(rexp(n_participants, 1/5), 1),  # Exponential distribution
    page_views = rpois(n_participants, lambda = 4) + 1,
    
    # Country (affects conversion)
    country = sample(c("USA", "UK", "Canada", "Germany", "France", "Other"), 
                    n_participants, replace = TRUE,
                    prob = c(0.4, 0.15, 0.1, 0.1, 0.1, 0.15))
  ) %>%
  
  # Generate conversion outcomes based on group and characteristics
  mutate(
    # Base conversion probability adjusted for characteristics
    base_conversion_prob = case_when(
      group == "Control" ~ conversion_rate_control,
      TRUE ~ conversion_rate_treatment
    ),
    
    # Adjust probability based on user characteristics
    adjusted_conversion_prob = base_conversion_prob * 
      case_when(
        device_type == "Desktop" ~ 1.2,
        device_type == "Mobile" ~ 0.9,
        TRUE ~ 1.0
      ) * 
      case_when(
        traffic_source == "Email" ~ 1.3,
        traffic_source == "Organic" ~ 1.1,
        traffic_source == "Paid Search" ~ 1.0,
        traffic_source == "Social" ~ 0.8,
        TRUE ~ 0.9
      ) * 
      case_when(
        country %in% c("USA", "UK", "Canada") ~ 1.1,
        TRUE ~ 0.95
      ),
    
    # Ensure probability stays within bounds
    final_conversion_prob = pmax(0.01, pmin(0.95, adjusted_conversion_prob)),
    
    # Generate actual conversion outcome
    converted = rbinom(n_participants, 1, final_conversion_prob),
    
    # Revenue if converted (for revenue-based analysis)
    revenue = ifelse(converted == 1, 
                    round(rlnorm(n_participants, meanlog = log(50), sdlog = 0.5), 2),
                    0),
    
    # Time to conversion (for those who converted)
    hours_to_conversion = ifelse(converted == 1,
                                round(rexp(n_participants, 1/24), 1),  # Within 24 hours typically
                                NA),
    
    # Additional engagement metrics
    email_signup = rbinom(n_participants, 1, 0.25),
    newsletter_click = rbinom(n_participants, 1, 0.15),
    social_share = rbinom(n_participants, 1, 0.08)
  ) %>%
  
  select(-base_conversion_prob, -adjusted_conversion_prob, -final_conversion_prob)  # Remove temp columns
  
  return(participants)
}

#' Generate Survey Data
#'
#' @description Creates realistic survey response data with response patterns
#' @param n_responses Number of survey responses
#' @param n_questions Number of questions in survey
#' @param response_rate Overall response rate
#' @param seed Random seed
#' @return Data frame with survey responses
#' @export
generate_survey_data <- function(n_responses = 500,
                                n_questions = 20,
                                response_rate = 0.75,
                                seed = 42) {
  
  set.seed(seed)
  
  # Generate respondent characteristics
  respondents <- tibble(
    respondent_id = paste0("RESP_", str_pad(1:n_responses, width = 5, side = "left", pad = "0")),
    
    # Demographics
    age_group = sample(c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"), 
                      n_responses, replace = TRUE,
                      prob = c(0.15, 0.25, 0.22, 0.18, 0.12, 0.08)),
    
    gender = sample(c("Male", "Female", "Other", "Prefer not to say"), 
                   n_responses, replace = TRUE,
                   prob = c(0.48, 0.49, 0.02, 0.01)),
    
    education = sample(c("High School", "Some College", "Bachelor", "Master", "PhD"), 
                      n_responses, replace = TRUE,
                      prob = c(0.25, 0.20, 0.35, 0.15, 0.05)),
    
    income_bracket = sample(c("<$25k", "$25k-$50k", "$50k-$75k", "$75k-$100k", "$100k+"), 
                           n_responses, replace = TRUE,
                           prob = c(0.15, 0.25, 0.25, 0.20, 0.15)),
    
    # Response quality indicators
    completion_time_minutes = round(rexp(n_responses, 1/12) + 3, 1),  # Min 3 minutes
    response_quality = sample(c("High", "Medium", "Low"), n_responses, replace = TRUE,
                             prob = c(0.6, 0.3, 0.1))
  )
  
  # Generate question responses
  for (i in 1:n_questions) {
    
    # Question type varies
    question_type <- sample(c("likert_5", "likert_7", "yes_no", "multiple_choice"), 1,
                           prob = c(0.4, 0.3, 0.15, 0.15))
    
    if (question_type == "likert_5") {
      # 5-point Likert scale (1=Strongly Disagree, 5=Strongly Agree)
      responses <- sample(1:5, n_responses, replace = TRUE,
                         prob = c(0.1, 0.2, 0.3, 0.3, 0.1))  # Normal distribution
      
    } else if (question_type == "likert_7") {
      # 7-point Likert scale
      responses <- sample(1:7, n_responses, replace = TRUE,
                         prob = c(0.05, 0.1, 0.15, 0.4, 0.15, 0.1, 0.05))
      
    } else if (question_type == "yes_no") {
      # Binary response
      responses <- sample(c(1, 0), n_responses, replace = TRUE, prob = c(0.6, 0.4))
      
    } else {
      # Multiple choice (1-4 options)
      responses <- sample(1:4, n_responses, replace = TRUE,
                         prob = c(0.4, 0.3, 0.2, 0.1))
    }
    
    # Apply non-response based on response rate and quality
    non_response_prob <- case_when(
      respondents$response_quality == "Low" ~ 1 - response_rate + 0.2,
      respondents$response_quality == "Medium" ~ 1 - response_rate + 0.1,
      TRUE ~ 1 - response_rate
    )
    
    # Some questions have higher non-response (sensitive topics)
    if (i %in% c(5, 12, 18)) {  # "Sensitive" questions
      non_response_prob <- non_response_prob + 0.1
    }
    
    # Apply non-response
    responses[runif(n_responses) < non_response_prob] <- NA
    
    # Add to respondents data
    respondents[[paste0("Q", str_pad(i, 2, "left", "0"))]] <- responses
  }
  
  # Add response metadata
  respondents <- respondents %>%
    mutate(
      survey_date = sample(seq(as.Date("2024-01-01"), Sys.Date(), by = "day"), 
                          n_responses, replace = TRUE),
      total_questions_answered = rowSums(!is.na(select(., starts_with("Q")))),
      completion_rate = round(total_questions_answered / n_questions * 100, 1),
      
      # Satisfaction-like derived metric
      satisfaction_score = round(rowMeans(select(., Q01:Q05), na.rm = TRUE), 1)
    )
  
  return(respondents)
}

#' Generate Financial Data
#'
#' @description Creates realistic financial time series data
#' @param symbol Stock symbol or identifier
#' @param start_date Start date for financial data
#' @param end_date End date for financial data
#' @param initial_price Starting price
#' @param volatility Daily volatility (standard deviation)
#' @param trend_strength Overall trend strength
#' @param seed Random seed
#' @return Data frame with OHLCV financial data
#' @export
generate_financial_data <- function(symbol = "DEMO",
                                   start_date = as.Date("2020-01-01"),
                                   end_date = Sys.Date(),
                                   initial_price = 100,
                                   volatility = 0.02,
                                   trend_strength = 0.0005,
                                   seed = 42) {
  
  set.seed(seed)
  
  # Generate date sequence (business days only)
  all_dates <- seq(start_date, end_date, by = "day")
  business_dates <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]
  n_days <- length(business_dates)
  
  # Generate log returns with trend
  log_returns <- rnorm(n_days, mean = trend_strength, sd = volatility)
  
  # Add some autocorrelation (momentum)
  for (i in 2:n_days) {
    log_returns[i] <- log_returns[i] + 0.1 * log_returns[i-1]
  }
  
  # Convert to prices
  log_prices <- log(initial_price) + cumsum(log_returns)
  close_prices <- exp(log_prices)
  
  # Generate OHLC data
  financial_data <- tibble(
    date = business_dates,
    symbol = symbol,
    
    # Close prices from our model
    close = round(close_prices, 2),
    
    # Generate Open prices (close to previous close with some gap)
    open = round(c(initial_price, close[-n_days] * exp(rnorm(n_days-1, 0, 0.005))), 2),
    
    # Generate High prices (always >= max(open, close))
    high = round(pmax(open, close) * exp(abs(rnorm(n_days, 0, 0.01))), 2),
    
    # Generate Low prices (always <= min(open, close))
    low = round(pmin(open, close) * exp(-abs(rnorm(n_days, 0, 0.01))), 2),
    
    # Volume (with some correlation to price movements)
    volume = round(rlnorm(n_days, meanlog = log(1000000), sdlog = 0.5) * 
                  (1 + 2 * abs(log_returns))),  # Higher volume on big moves
    
    # Additional technical indicators
    daily_return = round((close - lag(close)) / lag(close) * 100, 3),
    
    # Simple moving averages
    sma_20 = zoo::rollmean(close, k = 20, fill = NA, align = "right"),
    sma_50 = zoo::rollmean(close, k = 50, fill = NA, align = "right"),
    
    # Bollinger Bands
    bb_middle = sma_20,
    bb_std = zoo::rollapply(close, width = 20, FUN = sd, fill = NA, align = "right"),
    bb_upper = bb_middle + 2 * bb_std,
    bb_lower = bb_middle - 2 * bb_std,
    
    # RSI approximation
    gains = pmax(0, daily_return),
    losses = pmax(0, -daily_return),
    avg_gain = zoo::rollmean(gains, k = 14, fill = NA, align = "right"),
    avg_loss = zoo::rollmean(losses, k = 14, fill = NA, align = "right"),
    rsi = round(100 - (100 / (1 + avg_gain / (avg_loss + 0.001))), 1),
    
    # Market cap proxy (if this were a real stock)
    market_cap = round(close * 1000000 + rnorm(n_days, 0, 10000000))
  ) %>%
  
  select(-gains, -losses, -avg_gain, -avg_loss, -bb_std)  # Remove temporary columns
  
  return(financial_data)
}

# =============================================================================
# COMPREHENSIVE DATASET GENERATOR
# =============================================================================

#' Generate Complete Business Intelligence Dataset
#'
#' @description Creates a comprehensive, interconnected business dataset
#' @param n_customers Number of customers
#' @param n_products Number of products  
#' @param n_transactions Number of transactions
#' @param date_range Date range for data generation
#' @param seed Random seed for reproducibility
#' @return List containing all related datasets
#' @export
generate_comprehensive_business_data <- function(n_customers = 1000,
                                                n_products = 200,
                                                n_transactions = 5000,
                                                date_range = c(as.Date("2023-01-01"), Sys.Date()),
                                                seed = 42) {
  
  set.seed(seed)
  
  cat("Generating comprehensive business dataset...\n")
  
  # Generate core data
  cat("- Creating customer base...\n")
  customers <- generate_customer_data(n_customers, seed, date_range[1], date_range[2])
  
  cat("- Building product catalog...\n")
  products <- generate_product_catalog(n_products, seed + 1)
  
  cat("- Generating transactions...\n")
  transactions <- generate_transaction_data(customers, products, n_transactions, 
                                           date_range[1], date_range[2], seed + 2)
  
  # Generate supplementary datasets
  cat("- Creating A/B test data...\n")
  ab_test <- generate_ab_test_data(n_participants = round(n_customers * 0.3), 
                                  seed = seed + 3)
  
  cat("- Generating survey responses...\n")
  survey <- generate_survey_data(n_responses = round(n_customers * 0.4), 
                                seed = seed + 4)
  
  cat("- Creating time series metrics...\n")
  daily_metrics <- generate_timeseries_data(start_date = date_range[1],
                                           end_date = date_range[2],
                                           frequency = "daily",
                                           seed = seed + 5)
  
  # Calculate derived metrics
  cat("- Computing business metrics...\n")
  
  # Customer metrics
  customer_metrics <- transactions %>%
    filter(status == "Completed") %>%
    group_by(customer_id) %>%
    summarise(
      total_orders = n(),
      total_spent = sum(total_amount, na.rm = TRUE),
      avg_order_value = mean(total_amount, na.rm = TRUE),
      first_purchase = min(transaction_date, na.rm = TRUE),
      last_purchase = max(transaction_date, na.rm = TRUE),
      favorite_channel = names(sort(table(channel), decreasing = TRUE))[1],
      .groups = "drop"
    ) %>%
    mutate(
      customer_lifetime_days = as.numeric(last_purchase - first_purchase),
      purchase_frequency = total_orders / pmax(1, customer_lifetime_days / 30),
      estimated_clv = total_spent * (purchase_frequency / 12) * 2  # Simplified CLV
    )
  
  # Product metrics
  product_metrics <- transactions %>%
    filter(status == "Completed") %>%
    group_by(product_id) %>%
    summarise(
      units_sold = sum(quantity, na.rm = TRUE),
      total_revenue = sum(total_amount, na.rm = TRUE),
      unique_customers = n_distinct(customer_id),
      avg_selling_price = mean(unit_price, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Monthly summary
  monthly_summary <- transactions %>%
    filter(status == "Completed") %>%
    mutate(year_month = floor_date(transaction_date, "month")) %>%
    group_by(year_month) %>%
    summarise(
      monthly_revenue = sum(total_amount, na.rm = TRUE),
      monthly_orders = n(),
      unique_customers = n_distinct(customer_id),
      avg_order_value = mean(total_amount, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(year_month) %>%
    mutate(
      revenue_growth_pct = round((monthly_revenue - lag(monthly_revenue)) / lag(monthly_revenue) * 100, 2),
      orders_growth_pct = round((monthly_orders - lag(monthly_orders)) / lag(monthly_orders) * 100, 2)
    )
  
  # Create final dataset collection
  business_data <- list(
    # Core tables
    customers = customers,
    products = products,
    transactions = transactions,
    
    # Supplementary data
    ab_test_results = ab_test,
    survey_responses = survey,
    daily_metrics = daily_metrics,
    
    # Derived analytics
    customer_metrics = customer_metrics,
    product_metrics = product_metrics,
    monthly_summary = monthly_summary,
    
    # Metadata
    generation_info = list(
      generated_at = Sys.time(),
      seed_used = seed,
      n_customers = n_customers,
      n_products = n_products,
      n_transactions = n_transactions,
      date_range = date_range,
      total_revenue = sum(transactions$total_amount[transactions$status == "Completed"], na.rm = TRUE)
    )
  )
  
  cat("✓ Dataset generation complete!\n")
  cat("  - Customers:", nrow(customers), "\n")
  cat("  - Products:", nrow(products), "\n") 
  cat("  - Transactions:", nrow(transactions), "\n")
  cat("  - Total Revenue: $", scales::comma(business_data$generation_info$total_revenue), "\n")
  
  return(business_data)
}

# =============================================================================
# DATA QUALITY AND VALIDATION FUNCTIONS
# =============================================================================

#' Validate Generated Dataset
#'
#' @description Performs quality checks on generated datasets
#' @param data_list List of datasets to validate
#' @return List of validation results
#' @export
validate_generated_data <- function(data_list) {
  
  validation_results <- list()
  
  for (dataset_name in names(data_list)) {
    
    if (!is.data.frame(data_list[[dataset_name]])) {
      next  # Skip non-dataframe objects
    }
    
    dataset <- data_list[[dataset_name]]
    
    validation_results[[dataset_name]] <- list(
      # Basic checks
      row_count = nrow(dataset),
      column_count = ncol(dataset),
      
      # Missing data
      missing_data = sapply(dataset, function(x) sum(is.na(x))),
      missing_percentage = sapply(dataset, function(x) round(sum(is.na(x)) / length(x) * 100, 2)),
      
      # Data types
      column_types = sapply(dataset, class),
      
      # Duplicates
      duplicate_rows = sum(duplicated(dataset)),
      
      # Date ranges (if date columns exist)
      date_ranges = lapply(dataset[sapply(dataset, function(x) inherits(x, "Date"))], function(x) {
        if (length(x[!is.na(x)]) > 0) {
          c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
        } else {
          c(min = NA, max = NA)
        }
      }),
      
      # Numeric ranges
      numeric_ranges = lapply(dataset[sapply(dataset, is.numeric)], function(x) {
        if (length(x[!is.na(x)]) > 0) {
          c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE), mean = mean(x, na.rm = TRUE))
        } else {
          c(min = NA, max = NA, mean = NA)
        }
      })
    )
  }
  
  return(validation_results)
}

#' Print Validation Summary
#'
#' @description Pretty prints validation results
#' @param validation_results Results from validate_generated_data
#' @export
print_validation_summary <- function(validation_results) {
  
  cat("DATA VALIDATION SUMMARY\n")
  cat("="*30, "\n\n")
  
  for (dataset_name in names(validation_results)) {
    
    val <- validation_results[[dataset_name]]
    
    cat("Dataset:", dataset_name, "\n")
    cat("- Dimensions:", val$row_count, "rows ×", val$column_count, "columns\n")
    cat("- Duplicate rows:", val$duplicate_rows, "\n")
    
    # Missing data summary
    missing_cols <- names(val$missing_data)[val$missing_data > 0]
    if (length(missing_cols) > 0) {
      cat("- Missing data in:", length(missing_cols), "columns\n")
      for (col in missing_cols[1:min(3, length(missing_cols))]) {
        cat("  •", col, ":", val$missing_data[[col]], "missing (", val$missing_percentage[[col]], "%)\n")
      }
      if (length(missing_cols) > 3) cat("  ... and", length(missing_cols) - 3, "more columns\n")
    } else {
      cat("- No missing data\n")
    }
    
    cat("\n")
  }
}

# =============================================================================
# EXPORT FUNCTIONS
# =============================================================================

#' Export Generated Data
#'
#' @description Exports generated datasets to various formats
#' @param data_list List of datasets
#' @param output_dir Output directory
#' @param formats Vector of formats ("csv", "xlsx", "rds")
#' @export
export_generated_data <- function(data_list, output_dir = "data/generated/", formats = c("csv", "rds")) {
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Exporting generated data to:", output_dir, "\n")
  
  for (dataset_name in names(data_list)) {
    
    if (!is.data.frame(data_list[[dataset_name]])) {
      next  # Skip non-dataframe objects
    }
    
    dataset <- data_list[[dataset_name]]
    
    if ("csv" %in% formats) {
      csv_file <- file.path(output_dir, paste0(dataset_name, ".csv"))
      write.csv(dataset, csv_file, row.names = FALSE)
      cat("- Exported", dataset_name, "to CSV\n")
    }
    
    if ("rds" %in% formats) {
      rds_file <- file.path(output_dir, paste0(dataset_name, ".rds"))
      saveRDS(dataset, rds_file)
      cat("- Exported", dataset_name, "to RDS\n")
    }
    
    if ("xlsx" %in% formats && require(openxlsx, quietly = TRUE)) {
      xlsx_file <- file.path(output_dir, paste0(dataset_name, ".xlsx"))
      openxlsx::write.xlsx(dataset, xlsx_file)
      cat("- Exported", dataset_name, "to Excel\n")
    }
  }
  
  # Export the complete dataset as RDS
  if ("rds" %in% formats) {
    complete_file <- file.path(output_dir, "complete_business_data.rds")
    saveRDS(data_list, complete_file)
    cat("- Exported complete dataset collection to RDS\n")
  }
  
  cat("✓ Data export complete!\n")
}

# =============================================================================
# EXAMPLE USAGE AND DEMONSTRATION
# =============================================================================

if (interactive()) {
  # Generate comprehensive business dataset
  # demo_data <- generate_comprehensive_business_data(
  #   n_customers = 500,
  #   n_products = 100,
  #   n_transactions = 2000,
  #   seed = 42
  # )
  
  # Validate the data
  # validation <- validate_generated_data(demo_data)
  # print_validation_summary(validation)
  
  # Export the data
  # export_generated_data(demo_data, "data/demo/", c("csv", "rds"))
}
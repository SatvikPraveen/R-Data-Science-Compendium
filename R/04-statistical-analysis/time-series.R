# @title Comprehensive Time Series Analysis Framework
# @description Advanced time series modeling, forecasting, and decomposition
# @author R Data Science Portfolio Developer
# @date 2025

#' ========================================
#' COMPREHENSIVE TIME SERIES ANALYSIS
#' ========================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, ggplot2, lubridate, forecast, tseries, zoo, xts,
  TTR, PerformanceAnalytics, tidyquant, timetk, modeltime,
  prophet, seasonal, bcp, changepoint, plotly, DT
)

#' ========================================
#' 1. TIME SERIES DATA PREPARATION
#' ========================================

#' Time Series Data Preparation and Validation
#' @param data Data frame with time series data
#' @param date_col Name of date column
#' @param value_col Name of value column
#' @param frequency Frequency of time series (e.g., 12 for monthly, 4 for quarterly)
#' @return Prepared time series object and metadata
prepare_time_series <- function(data, date_col, value_col, frequency = 12) {
  
  # Ensure proper date format
  data[[date_col]] <- as.Date(data[[date_col]])
  
  # Sort by date
  data <- data[order(data[[date_col]]), ]
  
  # Check for missing dates
  date_seq <- seq(from = min(data[[date_col]]), 
                  to = max(data[[date_col]]), 
                  by = ifelse(frequency == 12, "month", 
                         ifelse(frequency == 4, "quarter", "day")))
  
  missing_dates <- setdiff(date_seq, data[[date_col]])
  
  # Create time series object
  ts_data <- ts(data[[value_col]], 
                start = c(year(min(data[[date_col]])), 
                         month(min(data[[date_col]]))), 
                frequency = frequency)
  
  # Basic statistics
  ts_stats <- list(
    length = length(ts_data),
    start_date = min(data[[date_col]]),
    end_date = max(data[[date_col]]),
    missing_values = sum(is.na(ts_data)),
    missing_dates = length(missing_dates),
    mean = mean(ts_data, na.rm = TRUE),
    sd = sd(ts_data, na.rm = TRUE),
    min = min(ts_data, na.rm = TRUE),
    max = max(ts_data, na.rm = TRUE)
  )
  
  return(list(
    ts_object = ts_data,
    data_frame = data,
    statistics = ts_stats,
    missing_dates = missing_dates
  ))
}

#' Time Series Visualization
#' @param ts_prep Prepared time series from prepare_time_series
#' @param title Plot title
#' @return List of visualization plots
visualize_time_series <- function(ts_prep, title = "Time Series Analysis") {
  
  ts_data <- ts_prep$ts_object
  df_data <- ts_prep$data_frame
  
  # Convert ts to data frame for ggplot
  ts_df <- data.frame(
    date = as.Date(time(ts_data)),
    value = as.numeric(ts_data)
  )
  
  # 1. Time series plot
  p1 <- ggplot(ts_df, aes(x = date, y = value)) +
    geom_line(color = "steelblue", size = 0.8) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.3, color = "red") +
    labs(title = paste(title, "- Time Series Plot"),
         x = "Date", y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 2. Seasonal plot
  if (frequency(ts_data) > 1) {
    p2 <- ggseasonplot(ts_data, year.labels = TRUE, year.labels.left = TRUE) +
      labs(title = "Seasonal Plot") +
      theme_minimal()
  } else {
    p2 <- ggplot() + labs(title = "Seasonal Plot - Not applicable for non-seasonal data")
  }
  
  # 3. Subseries plot
  if (frequency(ts_data) > 1) {
    p3 <- ggsubseriesplot(ts_data) +
      labs(title = "Seasonal Subseries Plot") +
      theme_minimal()
  } else {
    p3 <- ggplot() + labs(title = "Subseries Plot - Not applicable for non-seasonal data")
  }
  
  # 4. Lag plot
  p4 <- gglagplot(ts_data, lags = 9, set.lags = 1:9) +
    labs(title = "Lag Plots") +
    theme_minimal()
  
  # 5. ACF plot
  acf_data <- acf(ts_data, plot = FALSE, na.action = na.pass)
  acf_df <- data.frame(
    lag = acf_data$lag,
    acf = acf_data$acf
  )
  
  p5 <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(-1.96/sqrt(length(ts_data)), 1.96/sqrt(length(ts_data))), 
               linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = lag, yend = 0)) +
    labs(title = "Autocorrelation Function (ACF)", x = "Lag", y = "ACF") +
    theme_minimal()
  
  # 6. PACF plot
  pacf_data <- pacf(ts_data, plot = FALSE, na.action = na.pass)
  pacf_df <- data.frame(
    lag = pacf_data$lag,
    pacf = pacf_data$acf
  )
  
  p6 <- ggplot(pacf_df, aes(x = lag, y = pacf)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(-1.96/sqrt(length(ts_data)), 1.96/sqrt(length(ts_data))), 
               linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = lag, yend = 0)) +
    labs(title = "Partial Autocorrelation Function (PACF)", x = "Lag", y = "PACF") +
    theme_minimal()
  
  return(list(
    time_plot = p1,
    seasonal_plot = p2,
    subseries_plot = p3,
    lag_plot = p4,
    acf_plot = p5,
    pacf_plot = p6
  ))
}

#' ========================================
#' 2. TIME SERIES DECOMPOSITION
#' ========================================

#' Comprehensive Time Series Decomposition
#' @param ts_data Time series object
#' @param method Decomposition method ("stl", "classical", "x11")
#' @return Decomposition results and plots
decompose_time_series <- function(ts_data, method = "stl") {
  
  if (frequency(ts_data) == 1) {
    return(list(message = "Decomposition not applicable for non-seasonal data"))
  }
  
  # Perform decomposition
  if (method == "stl") {
    decomp <- stl(ts_data, s.window = "periodic")
    decomp_df <- data.frame(
      date = as.Date(time(ts_data)),
      observed = as.numeric(ts_data),
      trend = as.numeric(decomp$time.series[,"trend"]),
      seasonal = as.numeric(decomp$time.series[,"seasonal"]),
      remainder = as.numeric(decomp$time.series[,"remainder"])
    )
  } else if (method == "classical") {
    decomp <- decompose(ts_data)
    decomp_df <- data.frame(
      date = as.Date(time(ts_data)),
      observed = as.numeric(ts_data),
      trend = as.numeric(decomp$trend),
      seasonal = as.numeric(decomp$seasonal),
      remainder = as.numeric(decomp$random)
    )
  }
  
  # Create decomposition plot
  decomp_long <- decomp_df %>%
    tidyr::pivot_longer(cols = c(observed, trend, seasonal, remainder),
                       names_to = "component", values_to = "value") %>%
    mutate(component = factor(component, levels = c("observed", "trend", "seasonal", "remainder")))
  
  decomp_plot <- ggplot(decomp_long, aes(x = date, y = value)) +
    geom_line(color = "steelblue") +
    facet_wrap(~component, scales = "free_y", ncol = 1) +
    labs(title = paste("Time Series Decomposition -", toupper(method)),
         x = "Date", y = "Value") +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold"))
  
  # Seasonal strength calculation
  seasonal_strength <- 1 - var(decomp_df$remainder, na.rm = TRUE) / 
                          var(decomp_df$seasonal + decomp_df$remainder, na.rm = TRUE)
  
  # Trend strength calculation
  trend_strength <- 1 - var(decomp_df$remainder, na.rm = TRUE) / 
                        var(decomp_df$trend + decomp_df$remainder, na.rm = TRUE)
  
  return(list(
    decomposition = decomp,
    decomp_data = decomp_df,
    decomp_plot = decomp_plot,
    seasonal_strength = seasonal_strength,
    trend_strength = trend_strength,
    method = method
  ))
}

#' ========================================
#' 3. STATIONARITY TESTING
#' ========================================

#' Comprehensive Stationarity Tests
#' @param ts_data Time series object
#' @return Stationarity test results
test_stationarity <- function(ts_data) {
  
  # Augmented Dickey-Fuller test
  adf_test <- adf.test(ts_data, alternative = "stationary")
  
  # KPSS test
  kpss_test <- kpss.test(ts_data, null = "Level")
  
  # Phillips-Perron test
  pp_test <- pp.test(ts_data, alternative = "stationary")
  
  # Box-Ljung test for autocorrelation
  ljung_test <- Box.test(ts_data, lag = min(20, length(ts_data)/5), type = "Ljung-Box")
  
  # Summary of results
  results_summary <- data.frame(
    Test = c("ADF Test", "KPSS Test", "Phillips-Perron Test", "Ljung-Box Test"),
    Statistic = c(adf_test$statistic, kpss_test$statistic, 
                  pp_test$statistic, ljung_test$statistic),
    P_Value = c(adf_test$p.value, kpss_test$p.value, 
                pp_test$p.value, ljung_test$p.value),
    Conclusion = c(
      ifelse(adf_test$p.value < 0.05, "Stationary", "Non-stationary"),
      ifelse(kpss_test$p.value > 0.05, "Stationary", "Non-stationary"),
      ifelse(pp_test$p.value < 0.05, "Stationary", "Non-stationary"),
      ifelse(ljung_test$p.value < 0.05, "Autocorrelated", "No autocorrelation")
    )
  )
  
  return(list(
    adf = adf_test,
    kpss = kpss_test,
    pp = pp_test,
    ljung_box = ljung_test,
    summary = results_summary
  ))
}

#' Make Time Series Stationary
#' @param ts_data Time series object
#' @param method Method for making stationary ("diff", "log_diff", "seasonal_diff")
#' @return Transformed stationary time series
make_stationary <- function(ts_data, method = "diff") {
  
  if (method == "diff") {
    stationary_ts <- diff(ts_data)
    transformation <- "First difference"
  } else if (method == "log_diff") {
    stationary_ts <- diff(log(ts_data))
    transformation <- "Log difference"
  } else if (method == "seasonal_diff") {
    stationary_ts <- diff(ts_data, lag = frequency(ts_data))
    transformation <- "Seasonal difference"
  }
  
  return(list(
    stationary_ts = stationary_ts,
    transformation = transformation,
    original = ts_data
  ))
}

#' ========================================
#' 4. ARIMA MODELING
#' ========================================

#' Automatic ARIMA Model Selection
#' @param ts_data Time series object
#' @param seasonal Boolean for seasonal ARIMA
#' @return ARIMA model results
fit_arima_model <- function(ts_data, seasonal = TRUE) {
  
  # Automatic ARIMA model selection
  if (seasonal && frequency(ts_data) > 1) {
    auto_arima <- auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  } else {
    auto_arima <- auto.arima(ts_data, seasonal = FALSE, stepwise = FALSE)
  }
  
  # Model diagnostics
  residuals <- residuals(auto_arima)
  
  # Ljung-Box test on residuals
  ljung_test <- Box.test(residuals, lag = min(20, length(residuals)/5), type = "Ljung-Box")
  
  # Shapiro-Wilk test for normality
  shapiro_test <- shapiro.test(residuals)
  
  # ARCH test for heteroscedasticity
  arch_test <- ArchTest(residuals, lags = 5)
  
  # Model accuracy metrics
  accuracy_metrics <- accuracy(auto_arima)
  
  # Forecast
  forecast_result <- forecast(auto_arima, h = min(24, length(ts_data)/4))
  
  # Create diagnostic plots
  # Residuals plot
  residuals_df <- data.frame(
    date = as.Date(time(ts_data)),
    residuals = as.numeric(residuals)
  )
  
  p1 <- ggplot(residuals_df, aes(x = date, y = residuals)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Model Residuals", x = "Date", y = "Residuals") +
    theme_minimal()
  
  # Q-Q plot of residuals
  p2 <- ggplot(residuals_df, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "Q-Q Plot of Residuals") +
    theme_minimal()
  
  # ACF of residuals
  acf_resid <- acf(residuals, plot = FALSE)
  acf_resid_df <- data.frame(
    lag = acf_resid$lag,
    acf = acf_resid$acf
  )
  
  p3 <- ggplot(acf_resid_df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-1.96/sqrt(length(residuals)), 1.96/sqrt(length(residuals))), 
               linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = lag, yend = 0)) +
    labs(title = "ACF of Residuals", x = "Lag", y = "ACF") +
    theme_minimal()
  
  return(list(
    model = auto_arima,
    forecast = forecast_result,
    diagnostics = list(
      ljung_box = ljung_test,
      shapiro = shapiro_test,
      arch = arch_test
    ),
    accuracy = accuracy_metrics,
    plots = list(
      residuals = p1,
      qq_plot = p2,
      acf_residuals = p3
    )
  ))
}

#' ========================================
#' 5. EXPONENTIAL SMOOTHING
#' ========================================

#' Exponential Smoothing Models
#' @param ts_data Time series object
#' @return Exponential smoothing results
fit_exponential_smoothing <- function(ts_data) {
  
  # Simple Exponential Smoothing
  ses_model <- ses(ts_data, h = min(12, length(ts_data)/4))
  
  # Holt's method (double exponential smoothing)
  holt_model <- holt(ts_data, h = min(12, length(ts_data)/4))
  
  # Holt-Winters method (triple exponential smoothing)
  if (frequency(ts_data) > 1) {
    hw_model <- hw(ts_data, h = min(12, length(ts_data)/4))
  } else {
    hw_model <- NULL
  }
  
  # ETS model (automatic selection)
  ets_model <- ets(ts_data)
  ets_forecast <- forecast(ets_model, h = min(12, length(ts_data)/4))
  
  # Model comparison
  models <- list(
    SES = ses_model,
    Holt = holt_model,
    ETS = ets_forecast
  )
  
  if (!is.null(hw_model)) {
    models$HoltWinters <- hw_model
  }
  
  # Calculate accuracy metrics
  accuracy_comparison <- sapply(models, function(x) accuracy(x)[1:6])
  
  return(list(
    models = models,
    accuracy_comparison = accuracy_comparison,
    best_model = ets_model
  ))
}

#' ========================================
#' 6. PROPHET FORECASTING
#' ========================================

#' Prophet Model for Time Series Forecasting
#' @param ts_prep Prepared time series data
#' @param periods Number of periods to forecast
#' @return Prophet model results
fit_prophet_model <- function(ts_prep, periods = 30) {
  
  # Prepare data for Prophet
  prophet_data <- data.frame(
    ds = ts_prep$data_frame[[1]],  # Date column
    y = ts_prep$data_frame[[2]]    # Value column
  )
  
  # Fit Prophet model
  prophet_model <- prophet(prophet_data)
  
  # Create future dataframe
  future <- make_future_dataframe(prophet_model, periods = periods)
  
  # Generate forecast
  forecast_result <- predict(prophet_model, future)
  
  # Create plots
  forecast_plot <- plot(prophet_model, forecast_result) +
    labs(title = "Prophet Forecast") +
    theme_minimal()
  
  components_plot <- prophet_plot_components(prophet_model, forecast_result)
  
  # Calculate cross-validation metrics if enough data
  if (nrow(prophet_data) > 100) {
    cv_results <- cross_validation(prophet_model, 
                                  initial = paste(floor(nrow(prophet_data) * 0.8), "days"),
                                  period = "30 days", 
                                  horizon = "30 days")
    performance_metrics <- performance_metrics(cv_results)
  } else {
    cv_results <- NULL
    performance_metrics <- NULL
  }
  
  return(list(
    model = prophet_model,
    forecast = forecast_result,
    plots = list(
      forecast = forecast_plot,
      components = components_plot
    ),
    cross_validation = cv_results,
    metrics = performance_metrics
  ))
}

#' ========================================
#' 7. CHANGE POINT DETECTION
#' ========================================

#' Detect Change Points in Time Series
#' @param ts_data Time series object
#' @param method Method for change point detection ("bcp", "cpt")
#' @return Change point detection results
detect_change_points <- function(ts_data, method = "bcp") {
  
  if (method == "bcp") {
    # Bayesian Change Point analysis
    bcp_result <- bcp(as.numeric(ts_data), mcmc = 5000, burnin = 1000)
    
    # Extract probable change points (probability > 0.5)
    change_points <- which(bcp_result$prob.mean > 0.5)
    
    # Create plot
    bcp_df <- data.frame(
      index = 1:length(ts_data),
      value = as.numeric(ts_data),
      probability = bcp_result$prob.mean
    )
    
    p1 <- ggplot(bcp_df, aes(x = index, y = value)) +
      geom_line() +
      geom_vline(xintercept = change_points, color = "red", linetype = "dashed") +
      labs(title = "Bayesian Change Point Detection", x = "Time", y = "Value") +
      theme_minimal()
    
    p2 <- ggplot(bcp_df, aes(x = index, y = probability)) +
      geom_line() +
      geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
      labs(title = "Change Point Probability", x = "Time", y = "Probability") +
      theme_minimal()
    
    return(list(
      method = "BCP",
      change_points = change_points,
      probabilities = bcp_result$prob.mean,
      plots = list(ts_plot = p1, prob_plot = p2)
    ))
    
  } else if (method == "cpt") {
    # Using changepoint package
    cpt_mean <- cpt.mean(ts_data)
    cpt_var <- cpt.var(ts_data)
    
    change_points_mean <- cpts(cpt_mean)
    change_points_var <- cpts(cpt_var)
    
    # Create plot
    ts_df <- data.frame(
      index = 1:length(ts_data),
      value = as.numeric(ts_data)
    )
    
    p1 <- ggplot(ts_df, aes(x = index, y = value)) +
      geom_line() +
      geom_vline(xintercept = change_points_mean, color = "red", linetype = "dashed", alpha = 0.7) +
      geom_vline(xintercept = change_points_var, color = "blue", linetype = "dotted", alpha = 0.7) +
      labs(title = "Change Point Detection", 
           subtitle = "Red: Mean changes, Blue: Variance changes",
           x = "Time", y = "Value") +
      theme_minimal()
    
    return(list(
      method = "CPT",
      change_points_mean = change_points_mean,
      change_points_var = change_points_var,
      plots = list(ts_plot = p1)
    ))
  }
}

#' ========================================
#' 8. DEMONSTRATION WITH SYNTHETIC DATA
#' ========================================

#' Generate Time Series Demo Data
generate_ts_demo_data <- function() {
  
  # Create monthly data for 5 years
  dates <- seq(from = as.Date("2019-01-01"), to = as.Date("2023-12-01"), by = "month")
  n <- length(dates)
  
  # Generate components
  trend <- 1:n * 0.5 + 100
  seasonal <- 10 * sin(2 * pi * (1:n) / 12)
  noise <- rnorm(n, 0, 5)
  
  # Add some change points
  change_point1 <- floor(n/3)
  change_point2 <- floor(2*n/3)
  
  trend[change_point1:change_point2] <- trend[change_point1:change_point2] + 20
  trend[(change_point2+1):n] <- trend[(change_point2+1):n] - 10
  
  # Combine components
  ts_value <- trend + seasonal + noise
  
  # Create data frame
  ts_data <- data.frame(
    date = dates,
    value = ts_value,
    trend_component = trend,
    seasonal_component = seasonal,
    noise_component = noise
  )
  
  return(ts_data)
}

#' Run Complete Time Series Analysis Demo
demo_time_series_analysis <- function() {
  
  cat("=== COMPREHENSIVE TIME SERIES ANALYSIS DEMONSTRATION ===\n\n")
  
  # Generate demo data
  demo_data <- generate_ts_demo_data()
  
  cat("1. DATA PREPARATION\n")
  cat("="*50, "\n")
  
  # Prepare time series
  ts_prep <- prepare_time_series(demo_data, "date", "value", frequency = 12)
  
  cat("Time Series Statistics:\n")
  print(ts_prep$statistics)
  
  cat("\n2. STATIONARITY TESTING\n")
  cat("="*50, "\n")
  
  # Test stationarity
  stationarity_tests <- test_stationarity(ts_prep$ts_object)
  print(stationarity_tests$summary)
  
  cat("\n3. TIME SERIES DECOMPOSITION\n")
  cat("="*50, "\n")
  
  # Decompose time series
  decomp_results <- decompose_time_series(ts_prep$ts_object, method = "stl")
  
  cat("Seasonal Strength:", round(decomp_results$seasonal_strength, 3), "\n")
  cat("Trend Strength:", round(decomp_results$trend_strength, 3), "\n")
  
  cat("\n4. ARIMA MODELING\n")
  cat("="*50, "\n")
  
  # Fit ARIMA model
  arima_results <- fit_arima_model(ts_prep$ts_object, seasonal = TRUE)
  
  cat("Best ARIMA Model:", toString(arima_results$model), "\n")
  cat("Model AIC:", round(AIC(arima_results$model), 2), "\n")
  cat("Ljung-Box p-value:", round(arima_results$diagnostics$ljung_box$p.value, 4), "\n")
  
  cat("\n5. EXPONENTIAL SMOOTHING\n")
  cat("="*50, "\n")
  
  # Fit exponential smoothing models
  es_results <- fit_exponential_smoothing(ts_prep$ts_object)
  
  cat("ETS Model:", toString(es_results$best_model), "\n")
  cat("Model Accuracy (RMSE):", round(es_results$accuracy_comparison["RMSE", "ETS"], 2), "\n")
  
  cat("\n6. CHANGE POINT DETECTION\n")
  cat("="*50, "\n")
  
  # Detect change points
  cp_results <- detect_change_points(ts_prep$ts_object, method = "bcp")
  
  cat("Detected Change Points:", paste(cp_results$change_points, collapse = ", "), "\n")
  
  cat("\nTime Series Analysis Demo Complete!\n")
  
  return(list(
    data = demo_data,
    preparation = ts_prep,
    visualization = visualize_time_series(ts_prep),
    stationarity = stationarity_tests,
    decomposition = decomp_results,
    arima = arima_results,
    exponential_smoothing = es_results,
    change_points = cp_results
  ))
}

# Export key functions
time_series_exports <- list(
  prepare_time_series = prepare_time_series,
  visualize_time_series = visualize_time_series,
  decompose_time_series = decompose_time_series,
  test_stationarity = test_stationarity,
  make_stationary = make_stationary,
  fit_arima_model = fit_arima_model,
  fit_exponential_smoothing = fit_exponential_smoothing,
  fit_prophet_model = fit_prophet_model,
  detect_change_points = detect_change_points,
  generate_ts_demo_data = generate_ts_demo_data,
  demo_time_series_analysis = demo_time_series_analysis
)

# Run demo if script is executed directly
if (sys.nframe() == 0) {
  demo_results <- demo_time_series_analysis()
}
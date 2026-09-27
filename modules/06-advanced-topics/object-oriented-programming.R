# @title Object-Oriented Programming in R
# @description Comprehensive OOP implementation using S3, S4, and R6 systems
# @author R Data Science Portfolio Developer
# @date 2025

#' ========================================
#' OBJECT-ORIENTED PROGRAMMING IN R
#' ========================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(R6, methods, pryr, testthat)

#' ========================================
#' 1. S3 OBJECT SYSTEM
#' ========================================

#' S3 Class: Statistical Model
#' Simple and flexible object system
#' @param formula Model formula
#' @param data Dataset
#' @param method Fitting method
#' @return StatModel object
StatModel <- function(formula, data, method = "lm") {
  
  # Fit the model based on method
  if (method == "lm") {
    model <- lm(formula, data = data)
  } else if (method == "glm") {
    model <- glm(formula, data = data)
  } else {
    stop("Unsupported method")
  }
  
  # Create S3 object
  result <- list(
    formula = formula,
    data = data,
    method = method,
    model = model,
    fitted_values = fitted(model),
    residuals = residuals(model),
    coefficients = coef(model),
    r_squared = if (method == "lm") summary(model)$r.squared else NULL,
    aic = AIC(model),
    created_at = Sys.time()
  )
  
  class(result) <- c("StatModel", "list")
  return(result)
}

#' @export
print.StatModel <- function(x, ...) {
  cat("Statistical Model (", x$method, ")\n", sep = "")
  cat("Formula: ", deparse(x$formula), "\n")
  cat("Observations: ", nrow(x$data), "\n")
  cat("AIC: ", round(x$aic, 2), "\n")
  if (!is.null(x$r_squared)) {
    cat("R-squared: ", round(x$r_squared, 4), "\n")
  }
  cat("Created: ", format(x$created_at), "\n")
}

#' @export
summary.StatModel <- function(object, ...) {
  cat("=== Statistical Model Summary ===\n")
  print(object)
  cat("\nCoefficients:\n")
  print(object$coefficients)
  
  cat("\nModel Summary:\n")
  print(summary(object$model))
  
  invisible(object)
}

#' @export
plot.StatModel <- function(x, ...) {
  par(mfrow = c(2, 2))
  plot(x$model, ...)
  par(mfrow = c(1, 1))
}

#' @export
predict.StatModel <- function(object, newdata = NULL, ...) {
  predict(object$model, newdata = newdata, ...)
}

#' S3 Class: Time Series Analysis
#' @param data Time series data
#' @param frequency Series frequency
#' @return TSAnalysis object
TSAnalysis <- function(data, frequency = 12) {
  
  # Convert to time series
  if (!is.ts(data)) {
    ts_data <- ts(data, frequency = frequency)
  } else {
    ts_data <- data
  }
  
  # Perform basic analysis
  decomp <- if (frequency > 1) decompose(ts_data) else NULL
  
  result <- list(
    original_data = data,
    ts_data = ts_data,
    frequency = frequency,
    start_date = start(ts_data),
    end_date = end(ts_data),
    length = length(ts_data),
    mean = mean(ts_data, na.rm = TRUE),
    sd = sd(ts_data, na.rm = TRUE),
    decomposition = decomp,
    has_trend = !is.null(decomp),
    created_at = Sys.time()
  )
  
  class(result) <- c("TSAnalysis", "list")
  return(result)
}

#' @export
print.TSAnalysis <- function(x, ...) {
  cat("Time Series Analysis\n")
  cat("Period: ", paste(x$start_date, collapse = "-"), " to ", 
      paste(x$end_date, collapse = "-"), "\n")
  cat("Frequency: ", x$frequency, "\n")
  cat("Length: ", x$length, " observations\n")
  cat("Mean: ", round(x$mean, 2), "\n")
  cat("Std Dev: ", round(x$sd, 2), "\n")
  cat("Has Seasonal Decomposition: ", x$has_trend, "\n")
}

#' @export
plot.TSAnalysis <- function(x, ...) {
  if (x$has_trend) {
    plot(x$decomposition, ...)
  } else {
    plot(x$ts_data, main = "Time Series", ...)
  }
}

#' ========================================
#' 2. S4 OBJECT SYSTEM
#' ========================================

#' S4 Class: Portfolio - Formal class definition
#' More rigid and formal than S3
setClass("Portfolio",
  slots = list(
    name = "character",
    holdings = "data.frame",
    cash = "numeric",
    currency = "character",
    created_date = "Date",
    last_updated = "POSIXct"
  ),
  prototype = list(
    name = "Unnamed Portfolio",
    holdings = data.frame(),
    cash = 0,
    currency = "USD",
    created_date = Sys.Date(),
    last_updated = Sys.time()
  )
)

#' Portfolio constructor
#' @param name Portfolio name
#' @param initial_cash Starting cash amount
#' @param currency Portfolio currency
#' @return Portfolio S4 object
createPortfolio <- function(name, initial_cash = 10000, currency = "USD") {
  new("Portfolio",
    name = name,
    cash = initial_cash,
    currency = currency,
    holdings = data.frame(
      symbol = character(0),
      shares = numeric(0),
      price = numeric(0),
      value = numeric(0),
      stringsAsFactors = FALSE
    )
  )
}

#' @export
setMethod("show", "Portfolio", function(object) {
  cat("Portfolio: ", object@name, "\n")
  cat("Cash: ", object@currency, " ", format(object@cash, big.mark = ","), "\n")
  cat("Holdings: ", nrow(object@holdings), " positions\n")
  
  if (nrow(object@holdings) > 0) {
    total_value <- sum(object@holdings$value) + object@cash
    cat("Total Value: ", object@currency, " ", format(total_value, big.mark = ","), "\n")
    cat("\nPositions:\n")
    print(object@holdings)
  }
  
  cat("Last Updated: ", format(object@last_updated), "\n")
})

#' Add position to portfolio
#' @param portfolio Portfolio object
#' @param symbol Stock symbol
#' @param shares Number of shares
#' @param price Price per share
#' @return Updated portfolio
setGeneric("addPosition", function(portfolio, symbol, shares, price) {
  standardGeneric("addPosition")
})

setMethod("addPosition", "Portfolio", function(portfolio, symbol, shares, price) {
  
  total_cost <- shares * price
  
  if (total_cost > portfolio@cash) {
    stop("Insufficient cash for this position")
  }
  
  # Check if position already exists
  existing_row <- which(portfolio@holdings$symbol == symbol)
  
  if (length(existing_row) > 0) {
    # Update existing position
    old_shares <- portfolio@holdings$shares[existing_row]
    old_value <- portfolio@holdings$value[existing_row]
    
    new_shares <- old_shares + shares
    new_avg_price <- (old_value + total_cost) / new_shares
    
    portfolio@holdings$shares[existing_row] <- new_shares
    portfolio@holdings$price[existing_row] <- new_avg_price
    portfolio@holdings$value[existing_row] <- new_shares * new_avg_price
  } else {
    # Add new position
    new_position <- data.frame(
      symbol = symbol,
      shares = shares,
      price = price,
      value = total_cost,
      stringsAsFactors = FALSE
    )
    portfolio@holdings <- rbind(portfolio@holdings, new_position)
  }
  
  # Update cash and timestamp
  portfolio@cash <- portfolio@cash - total_cost
  portfolio@last_updated <- Sys.time()
  
  return(portfolio)
})

#' Calculate portfolio metrics
setGeneric("getMetrics", function(portfolio) {
  standardGeneric("getMetrics")
})

setMethod("getMetrics", "Portfolio", function(portfolio) {
  
  if (nrow(portfolio@holdings) == 0) {
    return(list(
      total_value = portfolio@cash,
      cash_percentage = 100,
      largest_position = NA,
      position_count = 0
    ))
  }
  
  total_holdings_value <- sum(portfolio@holdings$value)
  total_value <- total_holdings_value + portfolio@cash
  cash_percentage <- (portfolio@cash / total_value) * 100
  
  # Find largest position
  largest_idx <- which.max(portfolio@holdings$value)
  largest_position <- list(
    symbol = portfolio@holdings$symbol[largest_idx],
    value = portfolio@holdings$value[largest_idx],
    percentage = (portfolio@holdings$value[largest_idx] / total_value) * 100
  )
  
  return(list(
    total_value = total_value,
    cash_percentage = cash_percentage,
    largest_position = largest_position,
    position_count = nrow(portfolio@holdings)
  ))
})

#' ========================================
#' 3. R6 OBJECT SYSTEM
#' ========================================

#' R6 Class: Machine Learning Model
#' Reference semantics and encapsulation
MLModel <- R6Class("MLModel",
  public = list(
    
    # Public fields
    model_type = NULL,
    algorithm = NULL,
    trained = FALSE,
    performance = NULL,
    
    # Constructor
    initialize = function(model_type = "classification", algorithm = "random_forest") {
      self$model_type <- model_type
      self$algorithm <- algorithm
      self$trained <- FALSE
      private$created_at <- Sys.time()
      private$training_history <- list()
      
      cat("MLModel initialized:", model_type, "using", algorithm, "\n")
    },
    
    # Public methods
    train = function(data, target_col, ...) {
      cat("Training model...\n")
      
      # Store training metadata
      private$training_data_shape <- dim(data)
      private$target_variable <- target_col
      
      # Simple training simulation based on algorithm
      if (self$algorithm == "random_forest") {
        if (!requireNamespace("randomForest", quietly = TRUE)) {
          stop("randomForest package required")
        }
        
        formula <- as.formula(paste(target_col, "~ ."))
        
        if (self$model_type == "classification") {
          data[[target_col]] <- as.factor(data[[target_col]])
          private$model <- randomForest::randomForest(formula, data = data, ...)
        } else {
          private$model <- randomForest::randomForest(formula, data = data, ...)
        }
        
      } else if (self$algorithm == "linear") {
        formula <- as.formula(paste(target_col, "~ ."))
        
        if (self$model_type == "classification") {
          private$model <- glm(formula, data = data, family = binomial, ...)
        } else {
          private$model <- lm(formula, data = data, ...)
        }
      }
      
      self$trained <- TRUE
      private$training_history <- append(private$training_history, list(Sys.time()))
      
      # Calculate basic performance
      predictions <- self$predict(data)
      self$performance <- private$calculate_performance(data[[target_col]], predictions)
      
      cat("Model trained successfully. Performance:", 
          names(self$performance)[1], "=", round(self$performance[[1]], 3), "\n")
      
      invisible(self)
    },
    
    predict = function(newdata) {
      if (!self$trained) {
        stop("Model must be trained before making predictions")
      }
      
      if (self$algorithm == "random_forest") {
        if (self$model_type == "classification") {
          predict(private$model, newdata, type = "class")
        } else {
          predict(private$model, newdata)
        }
      } else if (self$algorithm == "linear") {
        if (self$model_type == "classification") {
          predict(private$model, newdata, type = "response")
        } else {
          predict(private$model, newdata)
        }
      }
    },
    
    get_info = function() {
      list(
        model_type = self$model_type,
        algorithm = self$algorithm,
        trained = self$trained,
        created_at = private$created_at,
        training_data_shape = private$training_data_shape,
        target_variable = private$target_variable,
        training_count = length(private$training_history),
        performance = self$performance
      )
    },
    
    save_model = function(filename) {
      if (!self$trained) {
        stop("Cannot save untrained model")
      }
      
      model_data <- list(
        model = private$model,
        metadata = self$get_info()
      )
      
      saveRDS(model_data, filename)
      cat("Model saved to:", filename, "\n")
    }
  ),
  
  private = list(
    model = NULL,
    created_at = NULL,
    training_data_shape = NULL,
    target_variable = NULL,
    training_history = NULL,
    
    calculate_performance = function(actual, predicted) {
      if (self$model_type == "classification") {
        accuracy <- mean(actual == predicted)
        return(list(accuracy = accuracy))
      } else {
        rmse <- sqrt(mean((actual - predicted)^2))
        r_squared <- cor(actual, predicted)^2
        return(list(rmse = rmse, r_squared = r_squared))
      }
    }
  )
)

#' R6 Class: Data Pipeline
#' Encapsulated data processing pipeline
DataPipeline <- R6Class("DataPipeline",
  public = list(
    
    # Constructor
    initialize = function(name = "DataPipeline") {
      private$name <- name
      private$steps <- list()
      private$history <- list()
      private$current_data <- NULL
      
      cat("DataPipeline '", name, "' initialized\n", sep = "")
    },
    
    # Add processing step
    add_step = function(step_name, step_function, ...) {
      step <- list(
        name = step_name,
        func = step_function,
        args = list(...),
        added_at = Sys.time()
      )
      
      private$steps[[step_name]] <- step
      cat("Added step '", step_name, "' to pipeline\n", sep = "")
      
      invisible(self)
    },
    
    # Execute pipeline
    execute = function(data, verbose = TRUE) {
      if (length(private$steps) == 0) {
        stop("No steps defined in pipeline")
      }
      
      private$current_data <- data
      execution_log <- list()
      
      for (step_name in names(private$steps)) {
        step <- private$steps[[step_name]]
        
        if (verbose) {
          cat("Executing step:", step_name, "\n")
        }
        
        start_time <- Sys.time()
        
        tryCatch({
          # Execute step
          private$current_data <- do.call(step$func, 
                                        c(list(private$current_data), step$args))
          
          end_time <- Sys.time()
          elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
          
          execution_log[[step_name]] <- list(
            success = TRUE,
            elapsed_time = elapsed,
            data_shape = dim(private$current_data)
          )
          
          if (verbose) {
            cat("  Completed in", round(elapsed, 3), "seconds\n")
          }
          
        }, error = function(e) {
          execution_log[[step_name]] <- list(
            success = FALSE,
            error = e$message
          )
          stop("Pipeline failed at step '", step_name, "': ", e$message)
        })
      }
      
      # Store execution history
      private$history[[length(private$history) + 1]] <- list(
        executed_at = Sys.time(),
        input_shape = dim(data),
        output_shape = dim(private$current_data),
        execution_log = execution_log
      )
      
      if (verbose) {
        cat("Pipeline completed successfully\n")
      }
      
      return(private$current_data)
    },
    
    # Get pipeline status
    get_status = function() {
      list(
        name = private$name,
        steps_count = length(private$steps),
        steps = names(private$steps),
        executions_count = length(private$history),
        last_execution = if (length(private$history) > 0) {
          tail(private$history, 1)[[1]]$executed_at
        } else {
          NULL
        }
      )
    },
    
    # Remove step
    remove_step = function(step_name) {
      if (step_name %in% names(private$steps)) {
        private$steps[[step_name]] <- NULL
        cat("Removed step '", step_name, "' from pipeline\n", sep = "")
      } else {
        warning("Step '", step_name, "' not found")
      }
      
      invisible(self)
    },
    
    # Clear all steps
    clear = function() {
      private$steps <- list()
      cat("All pipeline steps cleared\n")
      invisible(self)
    }
  ),
  
  private = list(
    name = NULL,
    steps = NULL,
    history = NULL,
    current_data = NULL
  )
)

#' ========================================
#' 4. INHERITANCE AND POLYMORPHISM
#' ========================================

#' S3 Inheritance Example
create_shape <- function(type) {
  obj <- list(type = type, created_at = Sys.time())
  class(obj) <- c(type, "Shape")
  obj
}

#' @export
area <- function(x) UseMethod("area")

area.Shape <- function(x) {
  stop("Area method not implemented for base Shape class")
}

area.Circle <- function(x) {
  pi * x$radius^2
}

area.Rectangle <- function(x) {
  x$width * x$height
}

area.Triangle <- function(x) {
  0.5 * x$base * x$height
}

#' Create specific shapes
Circle <- function(radius) {
  obj <- create_shape("Circle")
  obj$radius <- radius
  obj
}

Rectangle <- function(width, height) {
  obj <- create_shape("Rectangle")
  obj$width <- width
  obj$height <- height
  obj
}

Triangle <- function(base, height) {
  obj <- create_shape("Triangle")
  obj$base <- base
  obj$height <- height
  obj
}

#' ========================================
#' 5. DEMONSTRATION FUNCTIONS
#' ========================================

#' Demonstrate S3 System
demo_s3_system <- function() {
  cat("=== S3 OBJECT SYSTEM DEMO ===\n")
  
  # Create sample data
  set.seed(42)
  sample_data <- data.frame(
    x = rnorm(100),
    y = 2 * rnorm(100) + rnorm(100, 0, 0.5)
  )
  
  # Create statistical model
  model <- StatModel(y ~ x, sample_data, method = "lm")
  
  cat("Created StatModel:\n")
  print(model)
  
  cat("\nModel summary:\n")
  summary(model)
  
  # Create time series analysis
  ts_data <- cumsum(rnorm(120))
  ts_analysis <- TSAnalysis(ts_data, frequency = 12)
  
  cat("\nTime Series Analysis:\n")
  print(ts_analysis)
  
  return(list(model = model, ts_analysis = ts_analysis))
}

#' Demonstrate S4 System
demo_s4_system <- function() {
  cat("\n=== S4 OBJECT SYSTEM DEMO ===\n")
  
  # Create portfolio
  portfolio <- createPortfolio("Demo Portfolio", initial_cash = 50000)
  
  cat("Initial portfolio:\n")
  show(portfolio)
  
  # Add some positions
  portfolio <- addPosition(portfolio, "AAPL", 100, 150)
  portfolio <- addPosition(portfolio, "GOOGL", 50, 2500)
  portfolio <- addPosition(portfolio, "MSFT", 75, 300)
  
  cat("\nPortfolio after adding positions:\n")
  show(portfolio)
  
  # Get metrics
  metrics <- getMetrics(portfolio)
  
  cat("\nPortfolio metrics:\n")
  print(metrics)
  
  return(portfolio)
}

#' Demonstrate R6 System
demo_r6_system <- function() {
  cat("\n=== R6 OBJECT SYSTEM DEMO ===\n")
  
  # Create sample data for ML model
  set.seed(42)
  ml_data <- data.frame(
    feature1 = rnorm(200),
    feature2 = rnorm(200),
    feature3 = rnorm(200)
  )
  ml_data$target <- with(ml_data, ifelse(feature1 + feature2 + rnorm(200, 0, 0.5) > 0, 1, 0))
  
  # Create and train ML model
  ml_model <- MLModel$new("classification", "linear")
  ml_model$train(ml_data, "target")
  
  cat("\nModel info:\n")
  print(ml_model$get_info())
  
  # Create data pipeline
  pipeline <- DataPipeline$new("Demo Pipeline")
  
  # Add processing steps
  pipeline$add_step("remove_na", function(data) na.omit(data))
  pipeline$add_step("scale_features", function(data) {
    numeric_cols <- sapply(data, is.numeric)
    data[numeric_cols] <- scale(data[numeric_cols])
    data
  })
  pipeline$add_step("add_interaction", function(data) {
    data$interaction <- data$feature1 * data$feature2
    data
  })
  
  # Execute pipeline
  cat("\nExecuting data pipeline:\n")
  processed_data <- pipeline$execute(ml_data)
  
  cat("\nPipeline status:\n")
  print(pipeline$get_status())
  
  return(list(ml_model = ml_model, pipeline = pipeline, processed_data = processed_data))
}

#' Demonstrate Inheritance
demo_inheritance <- function() {
  cat("\n=== INHERITANCE AND POLYMORPHISM DEMO ===\n")
  
  # Create different shapes
  circle <- Circle(radius = 5)
  rectangle <- Rectangle(width = 4, height = 6)
  triangle <- Triangle(base = 8, height = 3)
  
  shapes <- list(circle, rectangle, triangle)
  
  cat("Shape areas:\n")
  for (shape in shapes) {
    cat(shape$type, "area:", area(shape), "\n")
  }
  
  return(shapes)
}

#' Complete OOP Demo
demo_object_oriented_programming <- function() {
  cat("=== COMPREHENSIVE OBJECT-ORIENTED PROGRAMMING DEMONSTRATION ===\n\n")
  
  s3_results <- demo_s3_system()
  s4_results <- demo_s4_system()
  r6_results <- demo_r6_system()
  inheritance_results <- demo_inheritance()
  
  cat("\n=== OOP SYSTEMS COMPARISON ===\n")
  cat("S3: Simple, flexible, functional OOP\n")
  cat("S4: Formal, rigid, with validation\n")
  cat("R6: Reference semantics, encapsulation\n")
  
  cat("\nObject-Oriented Programming Demo Complete!\n")
  
  return(list(
    s3 = s3_results,
    s4 = s4_results,
    r6 = r6_results,
    inheritance = inheritance_results
  ))
}

# Export key functions and classes
oop_exports <- list(
  # S3 Classes
  StatModel = StatModel,
  TSAnalysis = TSAnalysis,
  
  # S4 Classes
  createPortfolio = createPortfolio,
  
  # R6 Classes
  MLModel = MLModel,
  DataPipeline = DataPipeline,
  
  # Inheritance examples
  Circle = Circle,
  Rectangle = Rectangle,
  Triangle = Triangle,
  area = area,
  
  # Demo functions
  demo_s3_system = demo_s3_system,
  demo_s4_system = demo_s4_system,
  demo_r6_system = demo_r6_system,
  demo_inheritance = demo_inheritance,
  demo_object_oriented_programming = demo_object_oriented_programming
)

# Run demo if script is executed directly
if (sys.nframe() == 0) {
  demo_results <- demo_object_oriented_programming()
}
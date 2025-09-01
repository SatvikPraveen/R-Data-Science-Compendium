# @title Advanced Functional Programming in R
# @description Comprehensive functional programming concepts, patterns, and techniques
# @author R Data Science Portfolio Developer
# @date 2025

#' ========================================
#' FUNCTIONAL PROGRAMMING IN R
#' ========================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, purrr, magrittr, functional, memoise, 
  pryr, microbenchmark, foreach, parallel
)

#' ========================================
#' 1. HIGHER-ORDER FUNCTIONS
#' ========================================

#' Function Factory - Creates functions that create functions
#' @param operation Mathematical operation as string
#' @return Function factory
create_math_function <- function(operation) {
  switch(operation,
    "power" = function(exponent) {
      function(x) x^exponent
    },
    "multiply" = function(multiplier) {
      function(x) x * multiplier
    },
    "add" = function(addend) {
      function(x) x + addend
    },
    stop("Unknown operation")
  )
}

#' Closure Example - Function with persistent state
#' @param initial_value Starting value for counter
#' @return Counter function with persistent state
create_counter <- function(initial_value = 0) {
  count <- initial_value
  
  function(increment = 1) {
    count <<- count + increment
    count
  }
}

#' Compose Functions - Function composition utility
#' @param ... Functions to compose (right to left)
#' @return Composed function
compose <- function(...) {
  funs <- list(...)
  function(x) {
    Reduce(function(acc, f) f(acc), rev(funs), init = x)
  }
}

#' Pipe-friendly function composition
#' @param f First function
#' @param g Second function
#' @return Composed function f(g(x))
`%then%` <- function(f, g) {
  function(x) g(f(x))
}

#' ========================================
#' 2. ADVANCED MAP/APPLY OPERATIONS
#' ========================================

#' Parallel Map with Error Handling
#' @param .x Vector to iterate over
#' @param .f Function to apply
#' @param .cores Number of cores to use
#' @param .error_handler Error handling function
#' @return List of results or errors
safe_parallel_map <- function(.x, .f, .cores = 2, .error_handler = function(e) NA) {
  
  # Create safe version of function
  safe_f <- function(x) {
    tryCatch(.f(x), error = .error_handler)
  }
  
  # Setup parallel cluster
  cl <- parallel::makeCluster(.cores)
  on.exit(parallel::stopCluster(cl))
  
  # Export function to cluster
  parallel::clusterExport(cl, c("safe_f", ".f", ".error_handler"), envir = environment())
  
  # Parallel apply
  results <- parallel::parLapply(cl, .x, safe_f)
  
  return(results)
}

#' Multi-type Map - Apply different functions based on type
#' @param .x List of mixed types
#' @param .dispatch Named list of functions for each type
#' @return List of transformed values
map_by_type <- function(.x, .dispatch) {
  
  map(.x, function(item) {
    item_type <- class(item)[1]
    
    if (item_type %in% names(.dispatch)) {
      .dispatch[[item_type]](item)
    } else if ("default" %in% names(.dispatch)) {
      .dispatch[["default"]](item)
    } else {
      item  # Return unchanged if no handler
    }
  })
}

#' Nested Map - Handle deeply nested structures
#' @param .x Nested list structure
#' @param .f Function to apply at leaves
#' @param .depth Maximum depth to traverse
#' @return Transformed nested structure
map_nested <- function(.x, .f, .depth = Inf) {
  
  if (.depth <= 0) return(.x)
  
  if (is.list(.x)) {
    map(.x, ~ map_nested(.x = .x, .f = .f, .depth = .depth - 1))
  } else {
    .f(.x)
  }
}

#' ========================================
#' 3. FUNCTIONAL PROGRAMMING PATTERNS
#' ========================================

#' Currying - Transform multi-argument function into chain of single-argument functions
#' @param .f Function to curry
#' @param ... Partial arguments
#' @return Curried function
curry <- function(.f, ...) {
  args <- list(...)
  
  function(...) {
    all_args <- c(args, list(...))
    do.call(.f, all_args)
  }
}

#' Partial Application - Pre-fill some arguments
#' @param .f Function to partially apply
#' @param ... Arguments to pre-fill
#' @return Partially applied function
partial <- function(.f, ...) {
  args <- list(...)
  
  function(...) {
    all_args <- c(args, list(...))
    do.call(.f, all_args)
  }
}

#' Memoization - Cache function results
#' @param .f Function to memoize
#' @param .cache_size Maximum cache size
#' @return Memoized function
create_memoized <- function(.f, .cache_size = 100) {
  cache <- list()
  
  function(...) {
    key <- digest::digest(list(...))
    
    if (key %in% names(cache)) {
      return(cache[[key]])
    }
    
    result <- .f(...)
    
    # Manage cache size
    if (length(cache) >= .cache_size) {
      cache[[names(cache)[1]]] <- NULL  # Remove oldest
    }
    
    cache[[key]] <- result
    result
  }
}

#' Lazy Evaluation - Create lazy sequences
#' @param .f Generator function
#' @param .initial Initial value
#' @return Lazy sequence generator
create_lazy_sequence <- function(.f, .initial) {
  current <- .initial
  
  function() {
    result <- current
    current <<- .f(current)
    result
  }
}

#' ========================================
#' 4. MONADS AND FUNCTORS
#' ========================================

#' Maybe Monad - Handle NULL values functionally
#' @param value Initial value (can be NULL)
#' @return Maybe object
Maybe <- function(value = NULL) {
  structure(list(value = value), class = "Maybe")
}

#' @export
is_nothing <- function(x) UseMethod("is_nothing")
is_nothing.Maybe <- function(x) is.null(x$value)
is_nothing.default <- function(x) is.null(x)

#' @export
fmap <- function(f, x) UseMethod("fmap")
fmap.Maybe <- function(f, x) {
  if (is_nothing(x)) {
    Maybe(NULL)
  } else {
    Maybe(f(x$value))
  }
}

#' @export
bind <- function(x, f) UseMethod("bind")
bind.Maybe <- function(x, f) {
  if (is_nothing(x)) {
    Maybe(NULL)
  } else {
    f(x$value)
  }
}

#' Either Monad - Handle errors functionally
#' @param value Value for Right, or error for Left
#' @param is_left Whether this is an error (Left)
#' @return Either object
Either <- function(value, is_left = FALSE) {
  structure(list(value = value, is_left = is_left), class = "Either")
}

Left <- function(error) Either(error, is_left = TRUE)
Right <- function(value) Either(value, is_left = FALSE)

#' @export
is_left <- function(x) UseMethod("is_left")
is_left.Either <- function(x) x$is_left

#' @export
fmap.Either <- function(f, x) {
  if (is_left(x)) {
    x
  } else {
    Right(f(x$value))
  }
}

#' @export
bind.Either <- function(x, f) {
  if (is_left(x)) {
    x
  } else {
    f(x$value)
  }
}

#' ========================================
#' 5. ADVANCED FUNCTION UTILITIES
#' ========================================

#' Function Timing Decorator
#' @param .f Function to time
#' @param .units Time units for output
#' @return Function that prints execution time
with_timing <- function(.f, .units = "seconds") {
  function(...) {
    start_time <- Sys.time()
    result <- .f(...)
    end_time <- Sys.time()
    
    elapsed <- as.numeric(difftime(end_time, start_time, units = .units))
    cat("Execution time:", elapsed, .units, "\n")
    
    result
  }
}

#' Retry Decorator - Retry function on failure
#' @param .f Function to retry
#' @param .max_attempts Maximum retry attempts
#' @param .delay Delay between attempts (seconds)
#' @return Function with retry logic
with_retry <- function(.f, .max_attempts = 3, .delay = 1) {
  function(...) {
    attempt <- 1
    
    repeat {
      result <- tryCatch(.f(...), error = function(e) e)
      
      if (!inherits(result, "error") || attempt >= .max_attempts) {
        break
      }
      
      cat("Attempt", attempt, "failed, retrying in", .delay, "seconds...\n")
      Sys.sleep(.delay)
      attempt <- attempt + 1
    }
    
    if (inherits(result, "error")) {
      stop("Function failed after ", .max_attempts, " attempts: ", result$message)
    }
    
    result
  }
}

#' Rate Limiting Decorator
#' @param .f Function to rate limit
#' @param .calls_per_second Maximum calls per second
#' @return Rate-limited function
with_rate_limit <- function(.f, .calls_per_second = 1) {
  last_call <- 0
  min_interval <- 1 / .calls_per_second
  
  function(...) {
    current_time <- as.numeric(Sys.time())
    time_since_last <- current_time - last_call
    
    if (time_since_last < min_interval) {
      Sys.sleep(min_interval - time_since_last)
    }
    
    last_call <<- as.numeric(Sys.time())
    .f(...)
  }
}

#' ========================================
#' 6. FUNCTIONAL DATA TRANSFORMATION
#' ========================================

#' Functional Pipeline Builder
#' @param data Initial data
#' @return Pipeline object
create_pipeline <- function(data) {
  structure(list(data = data, steps = list()), class = "Pipeline")
}

#' @export
add_step <- function(pipeline, .f, ...) UseMethod("add_step")
add_step.Pipeline <- function(pipeline, .f, ...) {
  step <- list(func = .f, args = list(...))
  pipeline$steps <- append(pipeline$steps, list(step))
  pipeline
}

#' @export
execute <- function(pipeline) UseMethod("execute")
execute.Pipeline <- function(pipeline) {
  result <- pipeline$data
  
  for (step in pipeline$steps) {
    result <- do.call(step$func, c(list(result), step$args))
  }
  
  result
}

#' Transducers - Composable data transformations
#' @param .f Transformation function
#' @return Transducer function
map_transducer <- function(.f) {
  function(rf) {
    function(acc, input) {
      rf(acc, .f(input))
    }
  }
}

filter_transducer <- function(.p) {
  function(rf) {
    function(acc, input) {
      if (.p(input)) {
        rf(acc, input)
      } else {
        acc
      }
    }
  }
}

#' Transduce - Apply transducers to data
#' @param xform Composed transducers
#' @param rf Reducing function
#' @param init Initial accumulator
#' @param coll Collection to process
#' @return Final accumulator
transduce <- function(xform, rf, init, coll) {
  transformed_rf <- xform(rf)
  Reduce(transformed_rf, coll, init = init)
}

#' ========================================
#' 7. LAZY EVALUATION AND STREAMS
#' ========================================

#' Stream Constructor
#' @param head First element
#' @param tail_thunk Function that generates rest of stream
#' @return Stream object
Stream <- function(head, tail_thunk = function() NULL) {
  structure(list(head = head, tail_thunk = tail_thunk), class = "Stream")
}

#' @export
stream_head <- function(stream) stream$head

#' @export
stream_tail <- function(stream) {
  if (is.function(stream$tail_thunk)) {
    stream$tail_thunk()
  } else {
    stream$tail_thunk
  }
}

#' @export
stream_take <- function(stream, n) {
  if (n <= 0 || is.null(stream)) return(list())
  
  c(stream_head(stream), stream_take(stream_tail(stream), n - 1))
}

#' Infinite number stream
#' @param start Starting number
#' @param step Step size
#' @return Infinite stream of numbers
numbers_from <- function(start = 1, step = 1) {
  Stream(start, function() numbers_from(start + step, step))
}

#' Fibonacci stream
fibonacci_stream <- function(a = 0, b = 1) {
  Stream(a, function() fibonacci_stream(b, a + b))
}

#' ========================================
#' 8. PERFORMANCE OPTIMIZATION
#' ========================================

#' Tail Call Optimization Simulation
#' @param .f Function to optimize
#' @return Optimized function using trampolines
optimize_tail_calls <- function(.f) {
  function(...) {
    result <- .f(...)
    
    while (is.function(result)) {
      result <- result()
    }
    
    result
  }
}

#' Memoized Recursive Function Builder
#' @param .f Recursive function
#' @return Memoized version
memoize_recursive <- function(.f) {
  cache <- new.env(parent = emptyenv())
  
  function(...) {
    key <- paste(list(...), collapse = ",")
    
    if (exists(key, envir = cache)) {
      get(key, envir = cache)
    } else {
      result <- .f(...)
      assign(key, result, envir = cache)
      result
    }
  }
}

#' ========================================
#' 9. DEMONSTRATION FUNCTIONS
#' ========================================

#' Demonstrate Function Factories
demo_function_factories <- function() {
  cat("=== FUNCTION FACTORIES DEMO ===\n")
  
  # Create power functions
  square <- create_math_function("power")(2)
  cube <- create_math_function("power")(3)
  
  cat("square(5) =", square(5), "\n")
  cat("cube(3) =", cube(3), "\n")
  
  # Create counter
  counter <- create_counter(10)
  cat("Counter: ", counter(), counter(5), counter(-2), "\n")
  
  # Function composition
  add_one <- function(x) x + 1
  multiply_by_two <- function(x) x * 2
  
  composed <- compose(add_one, multiply_by_two)
  cat("Composed function (add 1, then multiply by 2) on 5:", composed(5), "\n")
}

#' Demonstrate Advanced Mapping
demo_advanced_mapping <- function() {
  cat("\n=== ADVANCED MAPPING DEMO ===\n")
  
  # Multi-type mapping
  mixed_data <- list(
    numbers = 1:3,
    character = "hello",
    logical = TRUE,
    data.frame = data.frame(x = 1:2, y = 3:4)
  )
  
  type_dispatch <- list(
    "integer" = function(x) x^2,
    "character" = function(x) toupper(x),
    "logical" = function(x) !x,
    "data.frame" = function(x) nrow(x),
    "default" = function(x) "unknown"
  )
  
  result <- map_by_type(mixed_data, type_dispatch)
  cat("Multi-type mapping results:\n")
  print(result)
  
  # Parallel mapping with error handling
  test_data <- list(1, 2, "invalid", 4, NULL)
  safe_sqrt <- function(x) if (is.numeric(x)) sqrt(x) else stop("Not numeric")
  
  results <- safe_parallel_map(test_data, safe_sqrt, .cores = 2)
  cat("Parallel mapping with errors:\n")
  print(results)
}

#' Demonstrate Monads
demo_monads <- function() {
  cat("\n=== MONADS DEMO ===\n")
  
  # Maybe monad chain
  safe_divide <- function(x, y) {
    if (y == 0) Maybe(NULL) else Maybe(x / y)
  }
  
  safe_sqrt <- function(x) {
    if (x < 0) Maybe(NULL) else Maybe(sqrt(x))
  }
  
  result1 <- Maybe(16) %>%
    bind(function(x) safe_divide(x, 2)) %>%
    bind(function(x) safe_sqrt(x))
  
  result2 <- Maybe(16) %>%
    bind(function(x) safe_divide(x, 0)) %>%  # This will fail
    bind(function(x) safe_sqrt(x))
  
  cat("Maybe monad - successful chain:", result1$value, "\n")
  cat("Maybe monad - failed chain:", is_nothing(result2), "\n")
  
  # Either monad
  safe_parse_number <- function(s) {
    tryCatch(Right(as.numeric(s)), error = function(e) Left(e$message))
  }
  
  either_result1 <- safe_parse_number("42")
  either_result2 <- safe_parse_number("not_a_number")
  
  cat("Either monad - success:", either_result1$value, "\n")
  cat("Either monad - error:", either_result2$value, "\n")
}

#' Demonstrate Streams
demo_streams <- function() {
  cat("\n=== STREAMS DEMO ===\n")
  
  # Take first 10 numbers
  numbers <- numbers_from(1)
  first_ten <- stream_take(numbers, 10)
  cat("First 10 numbers:", paste(first_ten, collapse = ", "), "\n")
  
  # Take first 15 Fibonacci numbers
  fib <- fibonacci_stream()
  first_fifteen_fib <- stream_take(fib, 15)
  cat("First 15 Fibonacci:", paste(first_fifteen_fib, collapse = ", "), "\n")
}

#' Complete Functional Programming Demo
demo_functional_programming <- function() {
  cat("=== COMPREHENSIVE FUNCTIONAL PROGRAMMING DEMONSTRATION ===\n\n")
  
  demo_function_factories()
  demo_advanced_mapping()
  demo_monads()
  demo_streams()
  
  cat("\n=== PERFORMANCE COMPARISON ===\n")
  
  # Compare memoized vs non-memoized fibonacci
  fib_normal <- function(n) {
    if (n <= 1) n else fib_normal(n - 1) + fib_normal(n - 2)
  }
  
  fib_memo <- memoize_recursive(fib_normal)
  
  # Time both versions
  cat("Computing fibonacci(30)...\n")
  
  time_normal <- system.time(result_normal <- fib_normal(30))
  time_memo <- system.time(result_memo <- fib_memo(30))
  
  cat("Normal fibonacci time:", time_normal["elapsed"], "seconds\n")
  cat("Memoized fibonacci time:", time_memo["elapsed"], "seconds\n")
  cat("Speedup:", round(time_normal["elapsed"] / time_memo["elapsed"], 2), "x\n")
  
  cat("\nFunctional Programming Demo Complete!\n")
  
  return(list(
    function_factories = list(square = square, cube = cube),
    mapping_results = result,
    monads = list(maybe = result1, either = either_result1),
    streams = list(numbers = first_ten, fibonacci = first_fifteen_fib),
    performance = list(normal_time = time_normal, memo_time = time_memo)
  ))
}

# Export key functions
functional_programming_exports <- list(
  create_math_function = create_math_function,
  create_counter = create_counter,
  compose = compose,
  curry = curry,
  partial = partial,
  create_memoized = create_memoized,
  safe_parallel_map = safe_parallel_map,
  map_by_type = map_by_type,
  Maybe = Maybe,
  Either = Either,
  Left = Left,
  Right = Right,
  Stream = Stream,
  numbers_from = numbers_from,
  fibonacci_stream = fibonacci_stream,
  with_timing = with_timing,
  with_retry = with_retry,
  with_rate_limit = with_rate_limit,
  create_pipeline = create_pipeline,
  memoize_recursive = memoize_recursive,
  demo_functional_programming = demo_functional_programming
)

# Run demo if script is executed directly
if (sys.nframe() == 0) {
  demo_results <- demo_functional_programming()
}
# Internal argument checks.
#
# All user-facing functions validate their inputs eagerly and fail with a
# classed condition (`rdsc_error_input`) so that callers and tests can
# distinguish invalid input from numerical failures.

abort_input <- function(message, ...) {
  msg <- sprintf(message, ...)
  cond <- structure(
    class = c("rdsc_error_input", "error", "condition"),
    list(message = msg, call = sys.call(-1))
  )
  stop(cond)
}

check_numeric <- function(x, arg = deparse(substitute(x)), min_length = 1L,
                          allow_na = FALSE, finite = TRUE) {
  if (!is.numeric(x)) {
    abort_input("`%s` must be numeric, not %s.", arg, class(x)[1L])
  }
  if (length(x) < min_length) {
    abort_input("`%s` must have length >= %d (has %d).",
                arg, as.integer(min_length), length(x))
  }
  if (!allow_na && anyNA(x)) {
    abort_input("`%s` must not contain missing values.", arg)
  }
  if (finite && any(is.infinite(x))) {
    abort_input("`%s` must not contain infinite values.", arg)
  }
  invisible(x)
}

check_scalar_number <- function(x, arg = deparse(substitute(x)),
                                lower = -Inf, upper = Inf,
                                lower_open = FALSE, upper_open = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    abort_input("`%s` must be a single non-missing number.", arg)
  }
  too_low <- if (lower_open) x <= lower else x < lower
  too_high <- if (upper_open) x >= upper else x > upper
  if (too_low || too_high) {
    abort_input("`%s` must lie in %s%s, %s%s (got %s).", arg,
                if (lower_open) "(" else "[", format(lower),
                format(upper), if (upper_open) ")" else "]", format(x))
  }
  invisible(x)
}

check_count <- function(x, arg = deparse(substitute(x)), min = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      x != round(x) || x < min) {
    abort_input("`%s` must be a single whole number >= %d.", arg,
                as.integer(min))
  }
  invisible(as.integer(x))
}

check_level <- function(level, arg = "level") {
  check_scalar_number(level, arg, lower = 0, upper = 1,
                      lower_open = TRUE, upper_open = TRUE)
}

check_function <- function(f, arg = deparse(substitute(f))) {
  if (!is.function(f)) {
    abort_input("`%s` must be a function.", arg)
  }
  invisible(f)
}

check_binary <- function(y, arg = deparse(substitute(y))) {
  if (is.logical(y)) {
    y <- as.integer(y)
  } else if (is.factor(y)) {
    if (nlevels(y) != 2L) {
      abort_input("`%s` must have exactly two levels.", arg)
    }
    y <- as.integer(y) - 1L
  }
  if (!is.numeric(y) || anyNA(y) || !all(y %in% c(0, 1))) {
    abort_input("`%s` must be a binary 0/1, logical or two-level factor.", arg)
  }
  as.integer(y)
}

check_same_length <- function(x, y, arg_x, arg_y) {
  if (length(x) != length(y)) {
    abort_input("`%s` and `%s` must have the same length (%d vs %d).",
                arg_x, arg_y, length(x), length(y))
  }
  invisible(TRUE)
}

check_seed <- function(seed) {
  if (!is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L || is.na(seed))) {
    abort_input("`seed` must be NULL or a single number.")
  }
  invisible(seed)
}

# Number of observations in a vector or data frame.
n_obs <- function(data) {
  if (is.data.frame(data) || is.matrix(data)) nrow(data) else length(data)
}

# Subset observations of a vector or data frame by row index.
take_obs <- function(data, i) {
  if (is.data.frame(data) || is.matrix(data)) {
    data[i, , drop = FALSE]
  } else {
    data[i]
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x

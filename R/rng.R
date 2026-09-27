# Reproducible random number generation.
#
# Every stochastic function in the package takes a `seed` argument. When a
# seed is supplied the computation is run under that seed and the caller's
# RNG state (both `.Random.seed` and `RNGkind()`) is restored afterwards, so
# calling a package function never perturbs the random stream of the
# surrounding analysis.

get_rng_state <- function() {
  list(
    seed = if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      get(".Random.seed", envir = globalenv(), inherits = FALSE)
    },
    kind = RNGkind()
  )
}

restore_rng_state <- function(state) {
  do.call(RNGkind, as.list(state$kind))
  if (is.null(state$seed)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  } else {
    assign(".Random.seed", state$seed, envir = globalenv())
  }
}

# Evaluate `code` with a temporary seed; no-op wrapper when `seed` is NULL.
with_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(code)
  }
  state <- get_rng_state()
  on.exit(restore_rng_state(state), add = TRUE)
  set.seed(seed)
  code
}

#' Independent random number streams for parallel replication
#'
#' Generates `n` statistically independent L'Ecuyer-CMRG random number
#' streams (L'Ecuyer et al. 2002) from a single master seed. Assigning stream
#' `i` to replicate `i` makes the results of a simulation identical whether it
#' is run sequentially or in parallel, and independent of the number of
#' workers.
#'
#' @param n Number of streams.
#' @param seed Master seed (a single integer).
#' @return A list of `n` integer vectors, each a valid `.Random.seed` for the
#'   `"L'Ecuyer-CMRG"` generator.
#' @references
#' L'Ecuyer, P., Simard, R., Chen, E. J. and Kelton, W. D. (2002). An
#' object-oriented random-number package with many long streams and
#' substreams. *Operations Research*, 50(6), 1073--1075.
#' \doi{10.1287/opre.50.6.1073.358}
#' @examples
#' s <- rng_streams(3, seed = 1)
#' length(s)
#' @export
rng_streams <- function(n, seed) {
  n <- check_count(n, "n")
  check_seed(seed)
  if (is.null(seed)) {
    abort_input("`seed` must be supplied to create reproducible streams.")
  }
  state <- get_rng_state()
  on.exit(restore_rng_state(state), add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(seed)
  streams <- vector("list", n)
  current <- get(".Random.seed", envir = globalenv())
  for (i in seq_len(n)) {
    streams[[i]] <- current
    current <- parallel::nextRNGStream(current)
  }
  streams
}

# Run `f()` using a given L'Ecuyer-CMRG stream, restoring the RNG afterwards.
with_stream <- function(stream, f) {
  state <- get_rng_state()
  on.exit(restore_rng_state(state), add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  assign(".Random.seed", stream, envir = globalenv())
  f()
}

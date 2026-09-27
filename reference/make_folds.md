# Assign observations to cross-validation folds

Creates random, stratified or grouped \\k\\-fold partitions.

## Usage

``` r
make_folds(n, k = 10L, strata = NULL, groups = NULL, n_bins = 4L, seed = NULL)
```

## Arguments

- n:

  Number of observations.

- k:

  Number of folds.

- strata:

  Optional vector of length `n` to stratify on.

- groups:

  Optional vector of length `n` of group labels.

- n_bins:

  Number of quantile bins for numeric `strata`.

- seed:

  Optional seed; the caller's RNG state is restored on exit.

## Value

An integer vector of length `n` with fold labels in `1:k`.

## Details

- **Random**: observations are randomly permuted and dealt into `k`
  folds whose sizes differ by at most one.

- **Stratified** (`strata`): the dealing is done within each stratum, so
  every fold has approximately the same distribution of `strata`.
  Numeric `strata` with more than `n_bins` distinct values are first
  binned at their quantiles, which is useful for regression outcomes.

- **Grouped** (`groups`): all observations from the same group (e.g.
  patient, site or time block) are placed in the same fold, preventing
  leakage between correlated observations. Groups are dealt into folds
  in decreasing order of size to balance fold sizes.

## Examples

``` r
y <- rep(c(0, 1), c(90, 10))
f <- make_folds(length(y), k = 5, strata = y, seed = 1)
table(f, y) # each fold holds two of the ten positives
#>    y
#> f    0  1
#>   1 18  2
#>   2 18  2
#>   3 18  2
#>   4 18  2
#>   5 18  2
```

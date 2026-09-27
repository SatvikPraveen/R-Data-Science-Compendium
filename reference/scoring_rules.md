# Probabilistic classification metrics

The Brier score (mean squared error of predicted probabilities) and the
log loss (mean negative Bernoulli log-likelihood). Both are strictly
proper scoring rules, so they reward calibrated as well as
discriminating predictions (Gneiting and Raftery 2007).

## Usage

``` r
brier_score(truth, prob)

log_loss(truth, prob, eps = 1e-15)
```

## Arguments

- truth:

  Binary outcome: 0/1, logical, or a two-level factor (the second level
  is the positive class).

- prob:

  Predicted probabilities of the positive class.

- eps:

  Probabilities are truncated to `[eps, 1 - eps]` before taking
  logarithms.

## Value

A single number.

## References

Gneiting, T. and Raftery, A. E. (2007). Strictly proper scoring rules,
prediction, and estimation. *Journal of the American Statistical
Association*, 102(477), 359–378.
[doi:10.1198/016214506000001437](https://doi.org/10.1198/016214506000001437)

## Examples

``` r
y <- c(0, 0, 1, 1)
p <- c(0.1, 0.4, 0.35, 0.8)
brier_score(y, p)
#> [1] 0.158125
log_loss(y, p)
#> [1] 0.472288
```

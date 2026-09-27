# Regression performance metrics

Root mean squared error, mean absolute error and out-of-sample \\R^2 =
1 - \sum (y - \hat y)^2 / \sum (y - \bar y)^2\\.

## Usage

``` r
rmse(truth, pred)

mae(truth, pred)

r_squared(truth, pred)
```

## Arguments

- truth:

  Numeric vector of observed outcomes.

- pred:

  Numeric vector of predictions.

## Value

A single number.

## Examples

``` r
rmse(c(1, 2, 3), c(1.1, 1.9, 3.2))
#> [1] 0.1414214
mae(c(1, 2, 3), c(1.1, 1.9, 3.2))
#> [1] 0.1333333
r_squared(c(1, 2, 3), c(1.1, 1.9, 3.2))
#> [1] 0.97
```

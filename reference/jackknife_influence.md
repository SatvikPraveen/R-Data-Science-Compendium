# Jackknife influence values

Leave-one-out (jackknife) estimates of the empirical influence values of
a statistic, \\L_i = (n - 1)(\bar\theta\_{(\cdot)} -
\hat\theta\_{(i)})\\.

## Usage

``` r
jackknife_influence(data, statistic)
```

## Arguments

- data:

  A numeric vector, data frame or matrix. Resampling is over elements
  (vectors) or rows (data frames and matrices).

- statistic:

  A function taking resampled `data` and returning a single number.

## Value

A numeric vector of length `n`.

## Examples

``` r
jackknife_influence(c(1, 4, 2, 8, 5), mean)
#> [1] -3  0 -2  4  1
```

# Simulate data with a known ground truth

Generators for linear and logistic regression data with correlated
Gaussian predictors. The true coefficients are stored in the `"truth"`
attribute so that estimators can be evaluated against them.

## Usage

``` r
simulate_linear(n, beta, intercept = 0, sigma = 1, rho = 0, seed = NULL)

simulate_logistic(n, beta, intercept = 0, rho = 0, seed = NULL)

ar1_cor(p, rho)
```

## Arguments

- n:

  Number of observations.

- beta:

  Numeric vector of slope coefficients; its length sets the number of
  predictors.

- intercept:

  Intercept \\\beta_0\\.

- sigma:

  Residual standard deviation (`simulate_linear()` only).

- rho:

  AR(1) correlation between adjacent predictors, in (-1, 1).

- seed:

  Optional seed; the caller's RNG state is restored on exit.

- p:

  Number of predictors (`ar1_cor()` only).

## Value

A data frame with outcome `y` and predictors `x1`, ..., `xp`, with
attribute `truth` (a list of the generating parameters). For
`simulate_logistic()` the true event probabilities are in column `p`.

## Details

Predictors follow a multivariate normal distribution with unit variances
and first-order autoregressive correlation \\\mathrm{Cor}(X_j, X_k) =
\rho^{\|j-k\|}\\. For `simulate_linear()`, \\Y = \beta_0 + X\beta +
\varepsilon\\ with \\\varepsilon \sim N(0, \sigma^2)\\. For
`simulate_logistic()`, \\Y \sim
\mathrm{Bernoulli}\\\mathrm{expit}(\beta_0 + X\beta)\\\\.

## Examples

``` r
d <- simulate_linear(200, beta = c(1, 0.5, 0), sigma = 2, seed = 1)
coef(lm(y ~ ., data = d))
#>  (Intercept)           x1           x2           x3 
#>  0.007797159  1.009097406  0.202646350 -0.059286978 
attr(d, "truth")$beta
#> [1] 1.0 0.5 0.0

d2 <- simulate_logistic(500, beta = c(1, -1), intercept = -1, seed = 1)
mean(d2$y)
#> [1] 0.366
```

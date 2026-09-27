# Calibration intercept and slope

Assesses the calibration of predicted probabilities by logistic
recalibration (Cox 1958). The **calibration slope** is the coefficient
of \\\mathrm{logit}(\hat p)\\ in a logistic regression of the outcome;
values below 1 indicate predictions that are too extreme (overfitting).
**Calibration-in-the-large** is the intercept of a logistic regression
with \\\mathrm{logit}(\hat p)\\ as an offset; positive values indicate
systematic under-prediction. Perfect calibration corresponds to an
intercept of 0 and a slope of 1 (Van Calster et al. 2019).

## Usage

``` r
calibration(truth, prob, level = 0.95, eps = 1e-08)
```

## Arguments

- truth:

  Binary outcome: 0/1, logical, or a two-level factor (the second level
  is the positive class).

- prob:

  Predicted probabilities of the positive class.

- level:

  Confidence level for the Wald intervals.

- eps:

  Probabilities are truncated to `[eps, 1 - eps]` before taking
  logarithms.

## Value

A data frame with rows `intercept` and `slope` and columns `estimate`,
`se`, `lower`, `upper`.

## References

Cox, D. R. (1958). Two further applications of a model for binary
regression. *Biometrika*, 45(3/4), 562–565.
[doi:10.2307/2333203](https://doi.org/10.2307/2333203)

Van Calster, B., McLernon, D. J., van Smeden, M., Wynants, L. and
Steyerberg, E. W. (2019). Calibration: the Achilles heel of predictive
analytics. *BMC Medicine*, 17, 230.
[doi:10.1186/s12916-019-1466-7](https://doi.org/10.1186/s12916-019-1466-7)

## Examples

``` r
set.seed(1)
p <- runif(500)
y <- rbinom(500, 1, p)
calibration(y, p) # close to intercept 0, slope 1
#>   parameter    estimate         se      lower     upper
#> 1 intercept -0.08050068 0.10867602 -0.2935018 0.1325004
#> 2     slope  0.98576536 0.09605529  0.7975004 1.1740303
```

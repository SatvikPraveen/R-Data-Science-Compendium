# Repeated k-fold cross-validation

Estimates the out-of-sample performance of a modelling procedure by
(repeated, optionally stratified or grouped) \\k\\-fold
cross-validation.

## Usage

``` r
cross_validate(
  data,
  fit,
  outcome,
  metric = rmse,
  predict_fun = default_predict,
  k = 10L,
  repeats = 1L,
  strata = NULL,
  groups = NULL,
  seed = NULL
)
```

## Arguments

- data:

  A data frame.

- fit:

  A function `function(train)` returning a fitted model.

- outcome:

  Name of the outcome column in `data`.

- metric:

  A function `function(truth, pred)` returning a number, such as
  [`rmse()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/regression_metrics.md),
  [`mae()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/regression_metrics.md),
  [`auc()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/auc.md),
  [`brier_score()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/scoring_rules.md)
  or
  [`log_loss()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/scoring_rules.md).

- predict_fun:

  A function `function(model, newdata)` returning predictions. The
  default calls
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html) with
  `type = "response"` when the model inherits from `glm`.

- k:

  Number of folds.

- repeats:

  Number of repetitions with different random partitions.

- strata, groups:

  Optional stratification or grouping: a column name or a vector of
  length `nrow(data)`; see
  [`make_folds()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/make_folds.md).

- seed:

  Optional seed; the caller's RNG state is restored on exit.

## Value

An object of class `rdsc_cv` with elements `estimate` (fold-averaged),
`pooled_estimate`, `se`, `folds` (per-fold metrics), `predictions`
(out-of-fold predictions for every repeat), `k` and `repeats`.

## Details

The entire modelling procedure is passed as `fit`, so any data-dependent
preprocessing (scaling, imputation, feature selection, tuning) performed
inside `fit` is re-estimated within every training fold. Performing such
steps on the full data before cross-validation leaks information and
biases the estimate optimistically (Hastie, Tibshirani and Friedman
2009, Section 7.10.2).

**Fold-averaged versus pooled estimates.** `estimate` is the mean of the
per-fold metrics. `pooled_estimate` evaluates the metric once on all
out-of-fold predictions (per repeat, then averaged over repeats). The
two coincide for metrics that are means of per-observation losses (MAE,
MSE, Brier score, log loss) when folds are of equal size, but not for
nonlinear summaries. Averaging fold-level RMSE, for example, is biased
downwards by Jensen's inequality when test folds are small; the pooled
RMSE is the square root of the cross-validated MSE and does not have
this bias (Forman and Scholz 2010). For AUC, pooling can instead be
distorted by differences in calibration between fold models, so report
both.

The reported `se` is the naive standard error \\\mathrm{sd}(\text{fold
metrics}) / \sqrt{kR}\\, which treats fold estimates as independent.
Because training sets overlap it typically **underestimates** the true
sampling variability (Bengio and Grandvalet 2004; Bates, Hastie and
Tibshirani 2024); interpret it as a lower bound.

## References

Hastie, T., Tibshirani, R. and Friedman, J. (2009). *The Elements of
Statistical Learning* (2nd ed.). Springer.
[doi:10.1007/978-0-387-84858-7](https://doi.org/10.1007/978-0-387-84858-7)

Forman, G. and Scholz, M. (2010). Apples-to-apples in cross-validation
studies: pitfalls in classifier performance measurement. *ACM SIGKDD
Explorations Newsletter*, 12(1), 49–57.
[doi:10.1145/1882471.1882479](https://doi.org/10.1145/1882471.1882479)

Bengio, Y. and Grandvalet, Y. (2004). No unbiased estimator of the
variance of k-fold cross-validation. *Journal of Machine Learning
Research*, 5, 1089–1105.

Bates, S., Hastie, T. and Tibshirani, R. (2024). Cross-validation: what
does it estimate and how well does it do it? *Journal of the American
Statistical Association*, 119(546), 1434–1445.
[doi:10.1080/01621459.2023.2197686](https://doi.org/10.1080/01621459.2023.2197686)

## See also

[`nested_cv()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/nested_cv.md)
for honest evaluation of procedures that involve model selection.

## Examples

``` r
cv <- cross_validate(mtcars, fit = function(d) lm(mpg ~ wt + hp, data = d),
                     outcome = "mpg", k = 5, repeats = 3, seed = 1)
cv
#> 5-fold cross-validation, 3 repeat(s)
#>   metric: rmse
#>   estimate = 2.692 (naive SE 0.2211); pooled = 2.816
```

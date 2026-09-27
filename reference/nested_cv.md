# Nested cross-validation for honest evaluation of model selection

Estimates the generalisation performance of a procedure that *selects*
among candidate models (or hyperparameter values) by cross-validation.

## Usage

``` r
nested_cv(
  data,
  candidates,
  outcome,
  metric = rmse,
  predict_fun = default_predict,
  outer_k = 5L,
  inner_k = 5L,
  minimize = TRUE,
  strata = NULL,
  seed = NULL
)
```

## Arguments

- data:

  A data frame.

- candidates:

  A named list of fitting functions `function(train)`.

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

- outer_k, inner_k:

  Number of outer and inner folds.

- minimize:

  Logical; `TRUE` if smaller values of `metric` are better.

- strata:

  Optional stratification for the outer and inner partitions: a column
  name or a vector of length `nrow(data)`; see
  [`make_folds()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/make_folds.md).

- seed:

  Optional seed; the caller's RNG state is restored on exit.

## Value

An object of class `rdsc_nested_cv` with elements `estimate`,
`pooled_estimate` and `se` (nested), `naive_estimate`,
`naive_pooled_estimate`, `naive_choice`, `candidate_cv` (fold-averaged
score of every candidate), `outer` (per-fold results including the
selected candidate), `inner` (all inner-loop scores) and `predictions`
(outer out-of-fold predictions of the selected models).

## Details

Reporting the best cross-validated score among several candidates is
optimistically biased, because the same folds are used both to choose
the model and to estimate its performance (Varma and Simon 2006; Cawley
and Talbot 2010). Nested cross-validation removes this bias: within each
outer training set an inner cross-validation selects the candidate,
which is then refitted on the whole outer training set and scored on the
untouched outer test fold.

For comparison the function also reports the **naive** estimate: the
best candidate's score from an ordinary cross-validation on the full
data using the outer partition. The difference `naive - nested`
estimates the optimism of the naive approach.

Both are reported fold-averaged (`estimate`, `naive_estimate`) and
pooled over all outer out-of-fold predictions (`pooled_estimate`,
`naive_pooled_estimate`); see
[`cross_validate()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/cross_validate.md)
for why the two differ for metrics such as RMSE.

## References

Varma, S. and Simon, R. (2006). Bias in error estimation when using
cross-validation for model selection. *BMC Bioinformatics*, 7, 91.
[doi:10.1186/1471-2105-7-91](https://doi.org/10.1186/1471-2105-7-91)

Cawley, G. C. and Talbot, N. L. C. (2010). On over-fitting in model
selection and subsequent selection bias in performance evaluation.
*Journal of Machine Learning Research*, 11, 2079–2107.

## Examples

``` r
cands <- list(
  wt      = function(d) lm(mpg ~ wt, data = d),
  wt_hp   = function(d) lm(mpg ~ wt + hp, data = d),
  all     = function(d) lm(mpg ~ ., data = d)
)
ncv <- nested_cv(mtcars, cands, outcome = "mpg", outer_k = 4,
                 inner_k = 4, seed = 3)
ncv
#> Nested cross-validation (4 outer x 4 inner folds)
#>   nested estimate = 2.749 (SE 0.4197)
#>   naive estimate  = 2.749 (best candidate: wt_hp)
#>   pooled: nested = 2.844, naive = 2.844
#>   selected in outer folds: wt_hp (4) 
```

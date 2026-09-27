# Area under the ROC curve with DeLong confidence interval

`auc()` returns the empirical area under the receiver operating
characteristic curve, equal to the Mann-Whitney probability \\P(S_1 \>
S_0) + \frac12 P(S_1 = S_0)\\ that a random positive case scores higher
than a random negative one. `auc_ci()` adds the nonparametric variance
estimate of DeLong, DeLong and Clarke-Pearson (1988), computed in \\O(n
\log n)\\ via the midrank algorithm of Sun and Xu (2014).

## Usage

``` r
auc(truth, score)

auc_ci(truth, score, level = 0.95, transform = c("logit", "none"))
```

## Arguments

- truth:

  Binary outcome: 0/1, logical, or a two-level factor (the second level
  is the positive class).

- score:

  Numeric risk score; higher values indicate the positive class.

- level:

  Confidence level.

- transform:

  `"logit"` or `"none"`.

## Value

`auc()`: a single number. `auc_ci()`: a one-row data frame with `auc`,
`se`, `lower`, `upper`, `level`, `n_pos` and `n_neg`.

## Details

By default the confidence interval is formed on the logit scale and
back-transformed, which keeps it inside \[0, 1\] and improves coverage
when the AUC is near its bounds (Pepe 2003, Section 5.2). Use
`transform = "none"` for the untransformed Wald interval.

## References

DeLong, E. R., DeLong, D. M. and Clarke-Pearson, D. L. (1988). Comparing
the areas under two or more correlated receiver operating characteristic
curves: a nonparametric approach. *Biometrics*, 44(3), 837–845.
[doi:10.2307/2531595](https://doi.org/10.2307/2531595)

Sun, X. and Xu, W. (2014). Fast implementation of DeLong's algorithm for
comparing the areas under correlated receiver operating characteristic
curves. *IEEE Signal Processing Letters*, 21(11), 1389–1393.
[doi:10.1109/LSP.2014.2337313](https://doi.org/10.1109/LSP.2014.2337313)

Pepe, M. S. (2003). *The Statistical Evaluation of Medical Tests for
Classification and Prediction*. Oxford University Press.

## Examples

``` r
fit <- glm(am ~ wt, data = mtcars, family = binomial)
p <- fitted(fit)
auc(mtcars$am, p)
#> [1] 0.9331984
auc_ci(mtcars$am, p)
#>         auc         se     lower     upper level n_pos n_neg
#> 1 0.9331984 0.04811213 0.7547723 0.9844734  0.95    13    19
```

# Performance measures for a simulation study, with Monte Carlo SEs

Summarises the output of
[`run_simulation()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/run_simulation.md)
(or any data frame of replicate-level results) using the performance
measures and Monte Carlo standard error (MCSE) formulas of Morris, White
and Crowther (2019, Table 6).

## Usage

``` r
sim_performance(
  results,
  true,
  estimate = "estimate",
  se = "se",
  lower = "lower",
  upper = "upper",
  p_value = "p_value",
  alpha = 0.05,
  by = NULL
)
```

## Arguments

- results:

  A data frame of replicate-level results.

- true:

  The true value of the estimand: a single number, or the name of a
  column of `results` (for estimands that vary across scenarios).

- estimate, se, lower, upper, p_value:

  Names of the columns holding the point estimate, its model-based
  standard error, the confidence limits and the p-value. Measures whose
  columns are absent are skipped.

- alpha:

  Nominal significance level for `rejection`.

- by:

  Grouping columns. Defaults to the scenario variables recorded by
  [`run_simulation()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/run_simulation.md)
  plus a `method` column, if present.

## Value

A data frame of class `rdsc_performance` with the grouping columns and
`measure`, `estimate`, `mcse`, `n_rep` (replicates used) and
`n_missing`. The count is not called `n` so that it cannot clash with a
scenario variable of that name.

## Details

Let \\\hat\theta_i\\, \\i = 1, \dots, n\\, be the estimates in a group
and \\\theta\\ the true value. The measures reported (when the required
columns are supplied) are:

|  |  |  |
|----|----|----|
| Measure | Definition | MCSE |
| `bias` | \\\bar{\hat\theta} - \theta\\ | \\\sqrt{S^2\_{\hat\theta}/n}\\ |
| `empirical_se` | \\S\_{\hat\theta}\\ | \\S\_{\hat\theta}/\sqrt{2(n-1)}\\ |
| `mse` | \\n^{-1}\sum(\hat\theta_i - \theta)^2\\ | see reference |
| `model_se` | \\\sqrt{n^{-1}\sum \widehat{SE}\_i^2}\\ | see reference |
| `rel_error_model_se` | \\100(\mathrm{ModSE}/\mathrm{EmpSE} - 1)\\ | see reference |
| `coverage` | \\n^{-1}\sum 1(L_i \le \theta \le U_i)\\ | \\\sqrt{C(1-C)/n}\\ |
| `be_coverage` | coverage of \\\bar{\hat\theta}\\ (bias-eliminated) | \\\sqrt{C(1-C)/n}\\ |
| `mean_width` | \\n^{-1}\sum (U_i - L_i)\\ | \\S\_{U-L}/\sqrt n\\ |
| `rejection` | \\n^{-1}\sum 1(p_i \le \alpha)\\ | \\\sqrt{P(1-P)/n}\\ |

Replicates with a missing estimate (e.g. failed fits) are excluded and
counted in `n_missing`.

## References

Morris, T. P., White, I. R. and Crowther, M. J. (2019). Using simulation
studies to evaluate statistical methods. *Statistics in Medicine*,
38(11), 2074–2102.
[doi:10.1002/sim.8086](https://doi.org/10.1002/sim.8086)

## Examples

``` r
set.seed(1)
est <- rnorm(1000, mean = 0.1, sd = 1)
res <- data.frame(estimate = est, se = 1,
                  lower = est - 1.96, upper = est + 1.96)
sim_performance(res, true = 0)
#> Simulation performance measures (Morris et al., 2019)
#> 
#>             measure estimate (MCSE) n_rep n_missing
#>                bias   0.088 (0.033)  1000         0
#>        empirical_se   1.035 (0.023)  1000         0
#>                 mse   1.078 (0.048)  1000         0
#>            model_se   1.000 (0.000)  1000         0
#>  rel_error_model_se  -3.374 (2.162)  1000         0
#>            coverage   0.935 (0.008)  1000         0
#>         be_coverage   0.934 (0.008)  1000         0
#>          mean_width   3.920 (0.000)  1000         0
```

# Standardised mean differences with exact confidence intervals

Cohen's \\d\\ and Hedges' bias-corrected \\g\\ for one-sample, paired
and independent two-sample designs, with confidence intervals obtained
by inverting the noncentral \\t\\ distribution.

## Usage

``` r
cohens_d(x, y = NULL, paired = FALSE, mu = 0, level = 0.95)

hedges_g(x, y = NULL, paired = FALSE, mu = 0, level = 0.95)

hedges_correction(df)
```

## Arguments

- x:

  Numeric vector.

- y:

  Optional numeric vector: the second group, or the paired observations
  if `paired = TRUE`.

- paired:

  Logical; compute the standardised mean of the paired differences
  \\d_z\\.

- mu:

  Null value for one-sample and paired designs.

- level:

  Confidence level.

- df:

  Degrees of freedom (`hedges_correction()` only).

## Value

An object of class `rdsc_effect_size`: a list with `estimate`, `lower`,
`upper`, `level`, `se` (large-sample approximation), `df`, `t` (the
corresponding \\t\\ statistic), `n` and `design`.

## Details

**Independent samples.** \\d = (\bar x - \bar y) / s_p\\, where \\s_p\\
is the pooled standard deviation. Under normality with equal variances,
\\t = d / \sqrt{1/n_1 + 1/n_2}\\ follows a noncentral \\t\\ distribution
with \\n_1 + n_2 - 2\\ degrees of freedom and noncentrality \\\delta /
\sqrt{1/n_1 + 1/n_2}\\.

**One-sample and paired.** \\d = (\bar d - \mu_0) / s_d\\, computed on
the differences for paired data (often written \\d_z\\). Here \\t =
d\sqrt{n}\\ with \\n - 1\\ degrees of freedom.

**Confidence interval.** The limits for the noncentrality parameter
\\\lambda\\ solve \\P(T\_{\nu,\lambda} \le t\_{obs}) = 1 - \alpha/2\\
and \\\alpha/2\\ (Steiger and Fouladi 1997; Cumming and Finch 2001) and
are rescaled to the \\d\\ metric. The interval is exact under the model
assumptions, unlike the common large-sample approximation, which is
reported as `se` for reference.

**Hedges' g** multiplies \\d\\ (and its limits) by the exact
small-sample correction \\J(\nu) = \Gamma(\nu/2) /
\\\sqrt{\nu/2}\\\Gamma((\nu-1)/2)\\\\ (Hedges 1981), which makes \\g\\
unbiased for \\\delta\\.

Note that [`stats::pt()`](https://rdrr.io/r/stats/TDist.html) is only
accurate for noncentrality parameters up to about 37.6 in absolute
value; larger observed \\t\\ statistics yield a warning.

## References

Hedges, L. V. (1981). Distribution theory for Glass's estimator of
effect size and related estimators. *Journal of Educational Statistics*,
6(2), 107–128.
[doi:10.3102/10769986006002107](https://doi.org/10.3102/10769986006002107)

Steiger, J. H. and Fouladi, R. T. (1997). Noncentrality interval
estimation and the evaluation of statistical models. In L. L. Harlow, S.
A. Mulaik and J. H. Steiger (Eds.), *What If There Were No Significance
Tests?* (pp. 221–257). Erlbaum.

Cumming, G. and Finch, S. (2001). A primer on the understanding, use,
and calculation of confidence intervals that are based on central and
noncentral distributions. *Educational and Psychological Measurement*,
61(4), 532–574.
[doi:10.1177/0013164401614002](https://doi.org/10.1177/0013164401614002)

## Examples

``` r
x <- sleep$extra[sleep$group == 1]
y <- sleep$extra[sleep$group == 2]
cohens_d(x, y)
#> Cohen's d (independent samples)
#>   estimate = -0.8322, 95% CI [-1.739, 0.09545]
#>   df = 18, n = 10 + 10
hedges_g(x, y)
#> Hedges' g (independent samples)
#>   estimate = -0.7969, 95% CI [-1.665, 0.09141]
#>   df = 18, n = 10 + 10
cohens_d(x, y, paired = TRUE)
#> Cohen's d (paired (d_z))
#>   estimate = -1.285, 95% CI [-2.118, -0.4146]
#>   df = 9, n = 10
```

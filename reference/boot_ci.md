# Nonparametric bootstrap confidence intervals

Computes percentile, basic, normal-approximation and bias-corrected and
accelerated (BCa) confidence intervals for a scalar statistic using the
ordinary nonparametric bootstrap.

## Usage

``` r
boot_ci(
  data,
  statistic,
  R = 1999L,
  level = 0.95,
  type = c("percentile", "basic", "normal", "bca"),
  seed = NULL
)
```

## Arguments

- data:

  A numeric vector, data frame or matrix. Resampling is over elements
  (vectors) or rows (data frames and matrices).

- statistic:

  A function taking resampled `data` and returning a single number.

- R:

  Number of bootstrap replicates.

- level:

  Confidence level in (0, 1).

- type:

  Character vector of interval types to compute; any of `"percentile"`,
  `"basic"`, `"normal"` and `"bca"`.

- seed:

  Optional seed for reproducibility. The caller's RNG state is restored
  on exit.

## Value

An object of class `rdsc_boot_ci`: a list with elements `estimate` (the
statistic on the original data), `bias` and `se` (bootstrap estimates),
`intervals` (a data frame with columns `type`, `level`, `lower`,
`upper`), `replicates`, `acceleration`, `bias_correction`, `R` and `n`.

## Details

Observations (elements of a vector, or rows of a data frame or matrix)
are resampled with replacement `R` times. Interval endpoints for the
percentile, basic and BCa methods are obtained from the ordered
bootstrap replicates using interpolation on the normal quantile scale
(Davison and Hinkley 1997, eq. 5.8). With the same seed, resampling
indices, replicates and the percentile, basic and normal intervals are
identical to those of
[`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) and
[`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html).

For the BCa interval the bias-correction is \\\hat z_0 =
\Phi^{-1}\\\\(\hat\theta^\*\_r \< \hat\theta)/R\\\\ and the acceleration
is estimated from the jackknife influence values \\L_i =
(n-1)(\bar\theta\_{(\cdot)} - \hat\theta\_{(i)})\\ as \\\hat a = \sum
L_i^3 / \\6 (\sum L_i^2)^{3/2}\\\\ (Efron 1987).

Choose `R` so that \\(R + 1)\alpha/2\\ is an integer (e.g. 999, 1999,
9999) to avoid interpolation for the percentile and basic intervals.

## References

Efron, B. (1987). Better bootstrap confidence intervals. *Journal of the
American Statistical Association*, 82(397), 171–185.
[doi:10.1080/01621459.1987.10478410](https://doi.org/10.1080/01621459.1987.10478410)

Davison, A. C. and Hinkley, D. V. (1997). *Bootstrap Methods and Their
Application*. Cambridge University Press.
[doi:10.1017/CBO9780511802843](https://doi.org/10.1017/CBO9780511802843)

## See also

[`perm_test()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/perm_test.md)
for permutation tests.

## Examples

``` r
set.seed(1)
x <- rexp(40)
ci <- boot_ci(x, mean, R = 999, seed = 42)
ci
#> Nonparametric bootstrap confidence intervals
#> n = 40, R = 999 replicates
#> 
#> Estimate: 0.9721  (bias -0.0049, SE 0.1518)
#> 
#>        type level  lower upper
#>  percentile   95% 0.7009 1.279
#>       basic   95% 0.6649 1.243
#>      normal   95% 0.6795 1.274
#>         bca   95% 0.7259 1.343
as.data.frame(ci)
#>    estimate       type level     lower    upper
#> 1 0.9720744 percentile  0.95 0.7008681 1.279264
#> 2 0.9720744      basic  0.95 0.6648849 1.243281
#> 3 0.9720744     normal  0.95 0.6795268 1.274422
#> 4 0.9720744        bca  0.95 0.7258765 1.343383

# A statistic of a data frame: the correlation between two columns
ci_cor <- boot_ci(mtcars, function(d) cor(d$mpg, d$wt), R = 999, seed = 1)
ci_cor$intervals
#>         type level      lower      upper
#> 1 percentile  0.95 -0.9270929 -0.7949737
#> 2      basic  0.95 -0.9403451 -0.8082258
#> 3     normal  0.95 -0.9309166 -0.7967152
#> 4        bca  0.95 -0.9143585 -0.7571050
```

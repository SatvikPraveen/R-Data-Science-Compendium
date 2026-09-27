# Permutation and randomisation tests

Two-sample permutation tests and one-sample / paired sign-flip
(randomisation) tests for an arbitrary test statistic, with exact
enumeration when the reference set is small and valid Monte Carlo
p-values otherwise.

## Usage

``` r
perm_test(
  x,
  y = NULL,
  statistic = NULL,
  paired = FALSE,
  mu = 0,
  alternative = c("two.sided", "greater", "less"),
  R = 9999L,
  exact = NULL,
  max_exact = 10000L,
  seed = NULL
)
```

## Arguments

- x:

  Numeric vector of observations (first sample).

- y:

  Optional numeric vector: the second sample, or the paired observations
  if `paired = TRUE`.

- statistic:

  Test statistic. For two-sample tests a function `function(x, y)`; for
  one-sample and paired tests a function `function(d)`. Defaults to the
  difference in means and the mean respectively.

- paired:

  Logical; if `TRUE` perform a paired sign-flip test on `x - y`.

- mu:

  Null value for the one-sample / paired location.

- alternative:

  One of `"two.sided"`, `"greater"` or `"less"`.

- R:

  Number of Monte Carlo rearrangements.

- exact:

  `NULL` (default) to enumerate exactly when feasible, `TRUE` to force
  enumeration or `FALSE` to force Monte Carlo.

- max_exact:

  Largest reference set that is enumerated when `exact = NULL`.

- seed:

  Optional seed; the caller's RNG state is restored on exit.

## Value

An object of class `htest` with additional elements `null_distribution`
(the rearrangement statistics), `exact` (logical) and `p.value.mcse`
(the Monte Carlo standard error of the p-value, zero for exact tests).

## Details

**Two-sample test** (`y` supplied, `paired = FALSE`): under the null
hypothesis that `x` and `y` come from the same distribution the group
labels are exchangeable. The reference distribution is obtained by
reallocating the pooled observations to groups of the original sizes.

**One-sample / paired test** (`y` missing, or `paired = TRUE`): the test
is applied to `d = x - mu` (or `d = x - y - mu`). Under the null
hypothesis that `d` is symmetric about zero the signs are exchangeable,
and the reference distribution is obtained by sign-flipping.

**Exact vs Monte Carlo.** If the number of distinct rearrangements is at
most `max_exact` (or `exact = TRUE`), all rearrangements are enumerated
and the p-value is exact. Otherwise `R` random rearrangements are drawn
and the p-value is computed as \\(b + 1) / (R + 1)\\, where \\b\\ is the
number of rearrangements at least as extreme as the observed one. Unlike
\\b / R\\, this estimator never returns zero and gives a test whose type
I error rate does not exceed the nominal level (Phipson and Smyth 2010).

**Two-sided alternative.** "At least as extreme" is defined as
\\\|T^\*\| \ge \|T\_{obs}\|\\, which is appropriate for statistics whose
null distribution is centred at zero (such as the default difference in
means).

Comparisons are made with a small relative tolerance so that
rearrangements whose statistic equals the observed one up to
floating-point error are counted as ties.

## References

Phipson, B. and Smyth, G. K. (2010). Permutation p-values should never
be zero: calculating exact p-values when permutations are randomly
drawn. *Statistical Applications in Genetics and Molecular Biology*,
9(1), Article 39.
[doi:10.2202/1544-6115.1585](https://doi.org/10.2202/1544-6115.1585)

Good, P. I. (2005). *Permutation, Parametric, and Bootstrap Tests of
Hypotheses* (3rd ed.). Springer.
[doi:10.1007/b138696](https://doi.org/10.1007/b138696)

## Examples

``` r
# Exact two-sample test (choose(12, 6) = 924 rearrangements)
x <- c(19.1, 22.4, 20.8, 24.0, 21.7, 23.3)
y <- c(18.2, 17.9, 20.1, 19.4, 16.8, 18.8)
perm_test(x, y)
#> 
#>  Exact two-sample permutation test
#> 
#> data:  x and y
#> T = 3.35, rearrangements = 924, p-value = 0.006494
#> alternative hypothesis: true location shift is not equal to 0
#> 

# Monte Carlo test of a difference in medians
set.seed(2)
a <- rexp(40)
b <- rexp(35, rate = 0.6)
perm_test(a, b, statistic = function(x, y) median(x) - median(y),
          R = 1999, seed = 10)
#> 
#>  Monte Carlo two-sample permutation test
#> 
#> data:  a and b
#> T = -0.65975, rearrangements = 1999, p-value = 0.017
#> alternative hypothesis: true location shift is not equal to 0
#> 

# Paired sign-flip test
perm_test(sleep$extra[1:10], sleep$extra[11:20], paired = TRUE)
#> 
#>  Exact paired sign-flip randomisation test
#> 
#> data:  sleep$extra[1:10] and sleep$extra[11:20]
#> T = -1.58, rearrangements = 1024, p-value = 0.003906
#> alternative hypothesis: true location is not equal to 0
#> 
```

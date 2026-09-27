# Number of replicates needed for a target Monte Carlo standard error

Planning formulas from Morris, White and Crowther (2019, Section 5.3).
For proportions (coverage, rejection rates), \\n =
p(1-p)/\mathrm{MCSE}^2\\; for bias, \\n = \sigma^2/\mathrm{MCSE}^2\\,
where \\\sigma\\ is the anticipated empirical standard error of the
estimator.

## Usage

``` r
sim_n_required(
  target_mcse,
  measure = c("proportion", "bias"),
  p = 0.5,
  sd = NULL
)
```

## Arguments

- target_mcse:

  Desired Monte Carlo standard error.

- measure:

  `"proportion"` (coverage, power, type I error) or `"bias"`.

- p:

  Anticipated proportion; `0.5` gives the worst case.

- sd:

  Anticipated empirical SE of the estimator (for `"bias"`).

## Value

The required number of replicates (rounded up).

## Examples

``` r
# MCSE of 0.5 percentage points for coverage near 95%
sim_n_required(0.005, p = 0.95)
#> [1] 1900
```

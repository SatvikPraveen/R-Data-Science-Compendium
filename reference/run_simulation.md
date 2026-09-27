# Run a reproducible Monte Carlo simulation study

Executes the data-generating and analysis steps of a simulation study
over a grid of scenarios, recording one or more rows of results per
replicate. Designed around the ADEMP framework (Aims, Data-generating
mechanisms, Estimands, Methods, Performance measures) of Morris, White
and Crowther (2019).

## Usage

``` r
run_simulation(
  generate,
  analyse,
  n_sim,
  scenarios = NULL,
  seed,
  backend = c("sequential", "future")
)
```

## Arguments

- generate:

  A function returning one simulated data set. It is called with the
  columns of the current row of `scenarios` as named arguments.

- analyse:

  A function taking the simulated data set and returning either a named
  numeric vector (one result row) or a data frame (one row per method,
  for example).

- n_sim:

  Number of replicates per scenario.

- scenarios:

  Optional data frame of data-generating parameters, one row per
  scenario. Column names must match arguments of `generate`.

- seed:

  Master seed (required).

- backend:

  `"sequential"` or `"future"`.

## Value

A data frame of class `rdsc_simulation` with the scenario parameters,
`.scenario` and `.rep` indices, the columns returned by `analyse`, and
`.error` / `.warning` diagnostics. Attributes record the seed, number of
replicates, scenario variables, run time and session details.

## Details

**Reproducibility.** Each (scenario, replicate) pair is assigned its own
L'Ecuyer-CMRG random number stream derived from `seed` (see
[`rng_streams()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/rng_streams.md)).
Results are therefore bit-for-bit identical between the sequential and
parallel backends, regardless of the number of workers or the order in
which tasks are scheduled, and adding scenarios does not change the
results of existing ones as long as their position is fixed. The
caller's RNG state is restored on exit.

**Failures.** Errors raised by `generate` or `analyse` do not abort the
study. They are recorded in the `.error` column (and the first warning
of each replicate in `.warning`) so that non-convergence can be reported
as recommended by Morris et al. (2019).

**Parallelism.** With `backend = "future"` replicates are evaluated with
[`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)
using whatever
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
the user has set, e.g. `future::plan("multisession", workers = 4)`.

## References

Morris, T. P., White, I. R. and Crowther, M. J. (2019). Using simulation
studies to evaluate statistical methods. *Statistics in Medicine*,
38(11), 2074–2102.
[doi:10.1002/sim.8086](https://doi.org/10.1002/sim.8086)

## See also

[`sim_performance()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/sim_performance.md)
to summarise the results.

## Examples

``` r
# Coverage of the t-interval for the mean of skewed data
gen <- function(n) rexp(n)
ana <- function(x) {
  tt <- t.test(x)
  c(estimate = mean(x), se = sd(x) / sqrt(length(x)),
    lower = tt$conf.int[1], upper = tt$conf.int[2])
}
res <- run_simulation(gen, ana, n_sim = 200,
                      scenarios = data.frame(n = c(10, 50)), seed = 2024)
sim_performance(res, true = 1)
#> Simulation performance measures (Morris et al., 2019)
#> 
#>   n            measure estimate (MCSE) n_rep n_missing
#>  10               bias  -0.016 (0.022)   200         0
#>  10       empirical_se   0.314 (0.016)   200         0
#>  10                mse   0.098 (0.011)   200         0
#>  10           model_se   0.308 (0.009)   200         0
#>  10 rel_error_model_se  -1.821 (5.767)   200         0
#>  10           coverage   0.895 (0.022)   200         0
#>  10        be_coverage   0.905 (0.021)   200         0
#>  10         mean_width   1.294 (0.037)   200         0
#>  50               bias   0.006 (0.010)   200         0
#>  50       empirical_se   0.148 (0.007)   200         0
#>  50                mse   0.022 (0.002)   200         0
#>  50           model_se   0.143 (0.002)   200         0
#>  50 rel_error_model_se  -3.502 (5.065)   200         0
#>  50           coverage   0.940 (0.017)   200         0
#>  50        be_coverage   0.940 (0.017)   200         0
#>  50         mean_width   0.563 (0.008)   200         0
```

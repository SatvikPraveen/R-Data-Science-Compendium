# RDataScienceCompendium: reproducible resampling, simulation and model-evaluation methods

The package provides tested, dependency-light implementations of methods
that underpin credible applied statistics:

## Details

- **Resampling inference**:
  [`boot_ci()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/boot_ci.md),
  [`jackknife_influence()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/jackknife_influence.md),
  [`perm_test()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/perm_test.md).

- **Effect sizes**:
  [`cohens_d()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/effect_sizes.md),
  [`hedges_g()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/effect_sizes.md)
  with exact noncentral-t intervals.

- **Simulation studies**:
  [`run_simulation()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/run_simulation.md),
  [`sim_performance()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/sim_performance.md),
  [`sim_n_required()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/sim_n_required.md),
  [`rng_streams()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/rng_streams.md).

- **Model evaluation**:
  [`make_folds()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/make_folds.md),
  [`cross_validate()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/cross_validate.md),
  [`nested_cv()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/nested_cv.md),
  [`auc_ci()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/auc.md),
  [`brier_score()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/scoring_rules.md),
  [`log_loss()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/scoring_rules.md),
  [`calibration()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/calibration.md),
  [`rmse()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/regression_metrics.md),
  [`mae()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/regression_metrics.md),
  [`r_squared()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/regression_metrics.md).

- **Data generation with known truth**:
  [`simulate_linear()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/simulate_data.md),
  [`simulate_logistic()`](https://satvikpraveen.github.io/R-Data-Science-Compendium/reference/simulate_data.md).

Every stochastic function accepts a `seed` and leaves the caller's
random number stream untouched.

## See also

Useful links:

- <https://github.com/SatvikPraveen/R-Data-Science-Compendium>

- <https://satvikpraveen.github.io/R-Data-Science-Compendium/>

- Report bugs at
  <https://github.com/SatvikPraveen/R-Data-Science-Compendium/issues>

## Author

**Maintainer**: Satvik Praveen <satvikpraveen707@gmail.com> \[copyright
holder\]

Authors:

- Satvik Praveen <satvikpraveen707@gmail.com> \[copyright holder\]

#' RDataScienceCompendium: reproducible resampling, simulation and
#' model-evaluation methods
#'
#' The package provides tested, dependency-light implementations of methods
#' that underpin credible applied statistics:
#'
#' * **Resampling inference**: [boot_ci()], [jackknife_influence()],
#'   [perm_test()].
#' * **Effect sizes**: [cohens_d()], [hedges_g()] with exact noncentral-t
#'   intervals.
#' * **Simulation studies**: [run_simulation()], [sim_performance()],
#'   [sim_n_required()], [rng_streams()].
#' * **Model evaluation**: [make_folds()], [cross_validate()], [nested_cv()],
#'   [auc_ci()], [brier_score()], [log_loss()], [calibration()],
#'   [rmse()], [mae()], [r_squared()].
#' * **Data generation with known truth**: [simulate_linear()],
#'   [simulate_logistic()].
#'
#' Every stochastic function accepts a `seed` and leaves the caller's random
#' number stream untouched.
#'
#' @keywords internal
"_PACKAGE"

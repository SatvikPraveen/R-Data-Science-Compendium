# Contributing

Thank you for considering a contribution. Bug reports, reproducible examples
and pull requests are all welcome.

## Reporting a bug

Please open an issue that includes a **minimal reproducible example** (ideally
made with [reprex](https://reprex.tidyverse.org/)) and the output of
`sessionInfo()`.

## Development workflow

```sh
Rscript setup.R   # install dependencies
make document     # regenerate NAMESPACE and man/ after editing roxygen comments
make test         # run the test suite
make lint         # static analysis; must report no lints
make check        # R CMD check --as-cran; must report 0 errors and 0 warnings
```

Standards for pull requests:

* Every exported function has roxygen documentation with runnable examples
  and, where applicable, references to the method's primary literature.
* New behaviour is covered by tests. Statistical methods should be tested
  against an **independent** reference: a published value, another
  implementation, a closed-form identity or a simulation-based property such
  as coverage or unbiasedness.
* Stochastic functions take a `seed` argument and leave the caller's RNG
  state unchanged (use the internal `with_seed()`).
* Invalid input raises an error of class `rdsc_error_input`.
* Add a bullet to `NEWS.md`.

## Changing the analysis

Results in `analysis/results/` must be regenerated with `make analysis`,
never edited by hand. Commit the updated results, figures and provenance
files together with the code change that produced them.

## Code of conduct

This project follows the [Contributor Covenant](CODE_OF_CONDUCT.md). By
participating you agree to abide by its terms.

# Independent random number streams for parallel replication

Generates `n` statistically independent L'Ecuyer-CMRG random number
streams (L'Ecuyer et al. 2002) from a single master seed. Assigning
stream `i` to replicate `i` makes the results of a simulation identical
whether it is run sequentially or in parallel, and independent of the
number of workers.

## Usage

``` r
rng_streams(n, seed)
```

## Arguments

- n:

  Number of streams.

- seed:

  Master seed (a single integer).

## Value

A list of `n` integer vectors, each a valid `.Random.seed` for the
`"L'Ecuyer-CMRG"` generator.

## References

L'Ecuyer, P., Simard, R., Chen, E. J. and Kelton, W. D. (2002). An
object-oriented random-number package with many long streams and
substreams. *Operations Research*, 50(6), 1073–1075.
[doi:10.1287/opre.50.6.1073.358](https://doi.org/10.1287/opre.50.6.1073.358)

## Examples

``` r
s <- rng_streams(3, seed = 1)
length(s)
#> [1] 3
```

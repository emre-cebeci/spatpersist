# spatpersist scalability benchmark

`benchmark-grid.R` measures the complete ID-creation pipeline on four-period
regular grids. Each period shifts polygons by 0.05 coordinate units, producing
a sparse and predictable overlap graph. Every unit should retain one identity,
and every result is checked with `validate_ids()`.

After installing the current source package, run:

```sh
R CMD INSTALL .
Rscript inst/benchmarks/benchmark-grid.R
```

Optional arguments set comma-separated grid sides, number of periods, and
repetitions:

```sh
Rscript inst/benchmarks/benchmark-grid.R 20,40,80 5 5
```

The default sides of 10, 20, 40, and 80 represent 100, 400, 1,600, and 6,400
units per period. Report the R, `sf`, GEOS, operating-system, and hardware
versions alongside benchmark results when comparing machines or releases.

## Development baseline

On the initial development machine, a single 6,400-row run containing 1,600
units in each of four periods fell from 2.89 seconds to 0.65 seconds after
removing unconditional WKT serialization and full-table candidate rescans.
This is an approximately 78 percent reduction. The 25,600-row case completed
in 3.22 seconds and produced 38,160 transition edges.

These measurements are diagnostic baselines, not performance guarantees.
Irregular, highly overlapping, or unusually complex polygons can produce many
more candidate intersections than this sparse grid.

# spatpersist 0.1.0

## Initial release

- Introduce the package as `spatpersist` with the focused public API
  `persist_ids()`, `id_transitions()`, `id_lineages()`, `validate_ids()`,
  `plot_lineage()`, and `example_units()`.
- Create persistent spatial identity, boundary-version, and lineage identifiers
  for longitudinal polygon data.
- Configure continuity metrics, thresholds, one-to-one match rules, ambiguity
  handling, and maximum time gaps.
- Preserve identifiers across reruns through registry reconciliation.
- Reject exact same-period coextensive geometries that cannot be distinguished
  reproducibly by spatial evidence alone.
- Preserve distinct fractional-second `POSIXt` periods when checking for
  same-period coextensive geometries.
- Define identifiers as dataset-local and document deterministic consolidation
  when new evidence connects previously separate registry lineages.
- Inspect transition evidence and lineage relationships with exported audit
  tables, validation checks, and lineage plots.
- Exercise the package with synthetic examples, automated tests, and a
  reproducible scalability benchmark.
- Document calibration and production workflows in the package vignette.
- Lock the intentional public API with a regression test and add contributor
  guidance that requires synthetic or public reproducible examples.
- Allow manual CI runs, cancel superseded runs, and bound check duration.

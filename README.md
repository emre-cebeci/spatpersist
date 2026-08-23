# spatialid

`spatialid` creates persistent identifiers for polygon units observed over
time. It is designed for datasets in which names, labels, and boundaries may
change and reliable longitudinal IDs do not already exist.

Identity is determined from configurable spatial continuity rules. Names and
other descriptive attributes are preserved in the result but are not used for
matching.

> `spatialid` is under active development. Review transition diagnostics and
> validation results before using generated IDs in analysis.

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("emre-cebeci/spatialid")
```

For a private repository, GitHub authentication must be configured first.

## Quick start

```r
library(spatialid)

polygons <- spatialid_example()

result <- create_spatial_ids(
  polygons,
  time = "year",
  threshold = 0.75,
  metric = "share_old"
)

sf::st_drop_geometry(result)[
  , c(
    "year",
    "name",
    "spatial_id",
    "spatial_version_id",
    "lineage_id",
    "parent_id",
    "transition_type"
  )
]
```

The result remains an `sf` object in its original row order.

## Output identifiers

| Column | Meaning |
|---|---|
| `spatial_id` | One continuing spatial identity. It occurs at most once per time period. |
| `spatial_version_id` | A consecutive boundary spell within a `spatial_id`. |
| `lineage_id` | A broader family connecting related identities through splits, mergers, and replacements. |
| `parent_id` | The strongest predecessor when a related observation receives a new identity. |
| `transition_type` | `initial`, `continuation`, `new`, `split`, `merger`, `replacement`, or `complex`. |

Selected links also receive `match_score`, `match_confidence`, and
`match_ambiguous`. Registry recovery is recorded in `registry_matched`.

## Matching controls

The core identity rule is controlled by:

- `metric`: `"share_old"`, `"share_new"`, or `"iou"`;
- `threshold`: minimum accepted value of that metric;
- `match_rule`: greedy one-to-one matching or stricter mutual-best matching;
- `ambiguity_tolerance`: the score difference treated as a near tie;
- `ambiguity_action`: flag, reject, or stop on ambiguous candidates.

For example, this rejects near ties instead of selecting one deterministically:

```r
conservative <- create_spatial_ids(
  polygons,
  time = "year",
  metric = "iou",
  threshold = 0.75,
  match_rule = "mutual_best",
  ambiguity_action = "new"
)
```

`event_threshold`, `lineage_threshold`, and `version_threshold` separately
control event detection, lineage links, and geometry-version changes.

## Audit and validate

Every positive-area overlap considered by the matching engine is available as
a regular data frame:

```r
transitions <- spatialid_transitions(result)
lineages <- spatialid_lineages(result, time = "year")
issues <- validate_spatial_ids(result, time = "year")

nrow(issues) # zero means all implemented checks passed
```

Diagnostics distinguish spatial eligibility, rule eligibility, ambiguity,
mutual-best status, selection, confidence, and lineage membership.

## Plot a lineage

```r
plot_spatial_lineage(
  result,
  time = "year",
  lineage_id = "LID000004"
)
```

Solid edges are selected identity continuations. Dashed edges are additional
lineage relationships, such as branches created by a split or merger.

## Preserve IDs across reruns

A previous result can be supplied as an optional registry. Matching same-time
geometries retain their issued IDs, while new identities are allocated above
the registry's existing range.

```r
updated_result <- create_spatial_ids(
  updated_polygons,
  time = "year",
  registry = result
)
```

Conflicting registry evidence stops reconciliation rather than silently
renumbering identities.

## Input requirements

- An `sf` object containing only `POLYGON` or `MULTIPOLYGON` geometries.
- One observation per spatial unit and time period.
- A non-missing, sortable time column.
- Valid, non-empty geometries with positive area.
- A consistent CRS across current data and any registry.

Invalid polygons can be repaired explicitly with
`geometry_action = "repair"`. Optional geometry precision and maximum time-gap
controls are also available.

## Current scope

- Matching is spatial and one-to-one; names do not determine identity.
- Consecutive *observed* periods are compared unless `max_time_gap` prevents a
  link.
- A single `parent_id` cannot encode every predecessor in a merger; use
  `spatialid_transitions()` for complete many-to-many evidence.
- A reproducible scalability benchmark is included under
  `inst/benchmarks`; performance still depends strongly on polygon complexity
  and the number of candidate intersections.
- The package contains no bundled administrative datasets or domain-specific administrative
  assumptions.

See `vignette("spatialid-workflow")` for a complete walkthrough.

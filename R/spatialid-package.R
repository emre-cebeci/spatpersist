#' Persistent identifiers for longitudinal polygon data
#'
#' `spatialid` links polygon features observed at different times while keeping
#' four related concepts separate: the persistent identity of a unit, versions
#' of its geometry, its broader lineage, and its immediate parent following a
#' structural transition. Matching is based on polygon overlap and is designed
#' to remain auditable through explicit scores, confidence flags, transition
#' tables, and validation checks.
#'
#' @section Main workflow:
#' Use [create_spatial_ids()] to assign identifiers to a time-indexed `sf`
#' object. Inspect the result with [spatialid_transitions()] and
#' [spatialid_lineages()], and run [validate_spatial_ids()] before relying on
#' the identifiers downstream. [plot_spatial_lineage()] provides a compact
#' visual check of one lineage through time.
#'
#' @section Reproducible reruns:
#' A registry written by [create_spatial_ids()] can be supplied to a later run
#' so that already-observed units retain their identifiers. Registry matches
#' are reported in the `registry_matched` output column. If new spatial evidence
#' connects multiple prior lineages, they consolidate under the smallest
#' existing lineage identifier.
#'
#' @section Interpretation:
#' A persistent ID represents continuity under the selected overlap rule; it is
#' not a claim that the polygon is geometrically unchanged. Geometry changes
#' receive new version IDs. Splits, merges, births, deaths, and ambiguous
#' candidates remain visible in the audit outputs rather than being silently
#' collapsed. All identifiers are dataset-local, not globally unique: their
#' persistence is scoped to one dataset and its registry chain.
#'
#' @seealso `vignette("spatialid-workflow", package = "spatialid")`
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

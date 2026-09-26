#' Persistent identifiers for longitudinal polygon data
#'
#' `spatpersist` links polygon features observed at different times while keeping
#' four related concepts separate: the persistent identity of a unit, versions
#' of its geometry, its broader lineage, and its immediate parent following a
#' structural transition. Matching is based on polygon overlap and is designed
#' to remain auditable through explicit scores, confidence flags, transition
#' tables, and validation checks.
#'
#' @section Start here:
#' Pass an `sf` object with `POLYGON` or `MULTIPOLYGON` geometries and a
#' non-missing, sortable time column to [persist_ids()]. Each row represents
#' one polygon in one observed period. For a runnable synthetic example, see
#' **Examples** below; no external spatial data are needed.
#'
#' @section Choose a continuity rule:
#' `threshold` is the minimum overlap score for continuing an identity;
#' its default is 0.75. The default `metric = "share_old"` divides intersection
#' area by the earlier polygon's area. `"share_new"` uses the later polygon's
#' area, while `"iou"` divides by the union area. These choices can change
#' which boundaries count as continuations. [persist_ids()] also offers
#' `match_rule`, ambiguity controls, and separate thresholds for events,
#' lineages, and geometry versions. Review the resulting links before using
#' IDs in analysis.
#'
#' @section Read the result:
#' [persist_ids()] returns the input `sf` rows in their original order with
#' new columns. `spatial_id` denotes one continuing identity;
#' `spatial_version_id` distinguishes boundary spells within it;
#' `lineage_id` groups related identities across structural changes;
#' `parent_id` records one strongest predecessor for a new identity; and
#' `transition_type` labels the observed transition. A changed boundary can
#' keep its `spatial_id` while receiving a new `spatial_version_id`.
#' `match_score`, `match_confidence`, and `match_ambiguous` help flag links
#' for review. A single `parent_id` cannot describe every predecessor in a
#' merger.
#'
#' @section Audit the assignment:
#' [id_transitions()] exposes the considered positive-area links and their
#' overlap scores, eligibility, selection, and ambiguity flags.
#' [id_lineages()] summarizes identities within lineages.
#' [validate_ids()] checks structural consistency; a zero-row issue table
#' means the implemented checks passed, not that every assignment is
#' substantively correct. [plot_lineage()] draws one lineage for visual
#' inspection. See the workflow vignette for calibration guidance.
#'
#' @section Reproducible reruns:
#' A prior result from [persist_ids()] can be supplied as `registry` to a later run
#' so that already-observed units retain their identifiers. Registry matches
#' are reported in the `registry_matched` output column. If new spatial evidence
#' connects multiple prior lineages, they consolidate under the smallest
#' existing lineage identifier.
#'
#' @section Interpretation:
#' A persistent ID represents continuity under the selected overlap rule; it is
#' not a claim that the polygon is geometrically unchanged. Geometry changes
#' may receive new version IDs. Names and other descriptive attributes do not
#' determine matches. Exact coextensive polygons within the same period are
#' rejected because geometry alone cannot distinguish them reproducibly.
#' All identifiers are dataset-local, not globally unique: their persistence
#' is scoped to one dataset and its registry chain.
#'
#' @seealso [persist_ids()], [id_transitions()], [id_lineages()],
#'   [validate_ids()], [plot_lineage()], [example_units()],
#'   `vignette("spatpersist-workflow", package = "spatpersist")`
#'
#' @examples
#' units <- example_units()
#' ids <- persist_ids(units, time = "year", threshold = 0.75)
#'
#' # The input rows and geometry remain available alongside the new columns.
#' sf::st_drop_geometry(ids)[
#'   , c("year", "name", "spatial_id", "spatial_version_id",
#'       "lineage_id", "transition_type")
#' ]
#'
#' # Check the links and structural consistency of the IDs.
#' links <- id_transitions(ids)
#' head(links[, c("old_spatial_id", "new_spatial_id", "match_score",
#'                "selected")])
#' head(id_lineages(ids, time = "year"))
#' validate_ids(ids, time = "year")
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

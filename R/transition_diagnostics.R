# Build the transition diagnostics table
#
# Internal helper used by create_spatial_ids().
#
build_transition_diagnostics <- function(
    transition_edges,
    time_values,
    spatial_ids,
    spatial_version_ids,
    lineage_ids,
    parent_ids,
    transition_types,
    registry_matched,
    metric,
    match_rule) {

  if (nrow(transition_edges) == 0L) {
    return(
      data.frame(
        previous_time = time_values[integer()],
        current_time = time_values[integer()],
        old_row = integer(),
        new_row = integer(),
        old_spatial_id = character(),
        new_spatial_id = character(),
        old_spatial_version_id = character(),
        new_spatial_version_id = character(),
        old_lineage_id = character(),
        new_lineage_id = character(),
        intersection_area = numeric(),
        share_old = numeric(),
        share_new = numeric(),
        iou = numeric(),
        match_metric = character(),
        match_rule = character(),
        match_score = numeric(),
        eligible = logical(),
        rule_candidate = logical(),
        candidate = logical(),
        selected = logical(),
        mutual_best = logical(),
        ambiguous = logical(),
        match_confidence = numeric(),
        lineage_link = logical(),
        same_lineage = logical(),
        transition_type = character(),
        parent_id = character(),
        registry_matched = logical()
      )
    )
  }

  old_rows <- transition_edges$old_row
  new_rows <- transition_edges$new_row
  old_lineages <- lineage_ids[old_rows]
  new_lineages <- lineage_ids[new_rows]
  scores <- transition_edges[[metric]]

  result <- data.frame(
    previous_time = time_values[old_rows],
    current_time = time_values[new_rows],
    old_row = old_rows,
    new_row = new_rows,
    old_spatial_id = spatial_ids[old_rows],
    new_spatial_id = spatial_ids[new_rows],
    old_spatial_version_id = spatial_version_ids[old_rows],
    new_spatial_version_id = spatial_version_ids[new_rows],
    old_lineage_id = old_lineages,
    new_lineage_id = new_lineages,
    intersection_area = transition_edges$intersection_area,
    share_old = transition_edges$share_old,
    share_new = transition_edges$share_new,
    iou = transition_edges$iou,
    match_metric = rep(metric, nrow(transition_edges)),
    match_rule = rep(match_rule, nrow(transition_edges)),
    match_score = scores,
    eligible = transition_edges$eligible,
    rule_candidate = transition_edges$rule_candidate,
    candidate = transition_edges$candidate,
    selected = transition_edges$selected,
    mutual_best = transition_edges$mutual_best,
    ambiguous = transition_edges$ambiguous,
    match_confidence = transition_edges$match_confidence,
    lineage_link = transition_edges$lineage_link,
    same_lineage = old_lineages == new_lineages,
    transition_type = transition_types[new_rows],
    parent_id = parent_ids[new_rows],
    registry_matched = registry_matched[new_rows]
  )

  result[
    order(
      result$current_time,
      result$new_spatial_id,
      -result$match_score,
      result$old_spatial_id
    ),
  ]
}


#' Inspect spatial transition diagnostics
#'
#' Retrieves the candidate-link diagnostics created by
#' [create_spatial_ids()]. The table contains every positive-area overlap
#' considered across successive observed periods, including its overlap
#' statistics, mutual-best and ambiguity flags, candidate status, selection,
#' and confidence.
#'
#' @param x An `sf` result returned by [create_spatial_ids()].
#'
#' @return A data frame of transition diagnostics.
#'
#' @export
#'
#' @examples
#' result <- create_spatial_ids(spatialid_example(), time = "year")
#' spatialid_transitions(result)
#'
spatialid_transitions <- function(x) {

  diagnostics <- attr(x, "spatialid_transitions", exact = TRUE)

  if (is.null(diagnostics)) {
    stop("`x` does not contain spatialid transition diagnostics.")
  }

  diagnostics
}

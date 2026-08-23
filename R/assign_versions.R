# Assign geometry versions to accepted continuations
#
# Internal helper used by persist_ids().
#
assign_continuing_versions <- function(
    overlap,
    matches,
    old_version_numbers,
    n_new,
    version_threshold) {

  new_version_numbers <- rep(NA_integer_, n_new)

  if (nrow(matches) == 0L) {
    return(new_version_numbers)
  }

  edge_base <- max(c(overlap$new_id, matches$new_id))
  overlap_keys <- (overlap$old_id - 1L) * edge_base + overlap$new_id
  match_keys <- (matches$old_id - 1L) * edge_base + matches$new_id
  edge_rows <- match(match_keys, overlap_keys)
  duplicated_overlap_keys <- unique(
    overlap_keys[duplicated(overlap_keys)]
  )

  if (anyNA(edge_rows) || any(match_keys %in% duplicated_overlap_keys)) {
    stop("Internal error: a selected match must have exactly one overlap edge.")
  }

  geometry_changed <- overlap$iou[edge_rows] < version_threshold
  new_version_numbers[matches$new_id] <-
    old_version_numbers[matches$old_id] + as.integer(geometry_changed)

  new_version_numbers
}

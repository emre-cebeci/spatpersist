# Assign geometry versions to accepted continuations
#
# Internal helper used by create_spatial_ids().
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

  for (i in seq_len(nrow(matches))) {
    old_id <- matches$old_id[[i]]
    new_id <- matches$new_id[[i]]
    edge <- overlap[
      overlap$old_id == old_id & overlap$new_id == new_id,
    ]

    if (nrow(edge) != 1L) {
      stop("Internal error: a selected match must have exactly one overlap edge.")
    }

    geometry_changed <- edge$iou[[1L]] < version_threshold
    new_version_numbers[[new_id]] <-
      old_version_numbers[[old_id]] + as.integer(geometry_changed)
  }

  new_version_numbers
}

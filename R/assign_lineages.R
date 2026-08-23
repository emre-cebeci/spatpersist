# Describe how units enter an observed time period
#
# Internal helper used by persist_ids().
#
describe_transitions <- function(
    overlap,
    matches,
    old_spatial_ids,
    n_new,
    event_threshold,
    lineage_threshold) {

  transition_type <- rep("new", n_new)
  parent_id <- rep(NA_character_, n_new)

  if (nrow(overlap) == 0L) {
    return(
      list(
        transition_type = transition_type,
        parent_id = parent_id
      )
    )
  }

  candidate_strength <- pmax(overlap$share_old, overlap$share_new)
  strongest_order <- order(
    overlap$new_id,
    -candidate_strength,
    -overlap$iou,
    -overlap$intersection_area,
    overlap$old_id
  )
  strongest_rows <- strongest_order[
    !duplicated(overlap$new_id[strongest_order])
  ]

  reference_old <- rep(NA_integer_, n_new)
  reference_old[overlap$new_id[strongest_rows]] <-
    overlap$old_id[strongest_rows]

  selected_old <- rep(NA_integer_, n_new)
  selected_old[matches$new_id] <- matches$old_id
  is_continuation <- !is.na(selected_old)
  reference_old[is_continuation] <- selected_old[is_continuation]

  predecessor_count <- tabulate(
    overlap$new_id[overlap$share_new >= event_threshold],
    nbins = n_new
  )
  successor_count_by_old <- tabulate(
    overlap$old_id[overlap$share_old >= event_threshold],
    nbins = max(overlap$old_id)
  )
  successor_count <- rep(0L, n_new)
  has_reference <- !is.na(reference_old)
  successor_count[has_reference] <-
    successor_count_by_old[reference_old[has_reference]]
  has_lineage_link <- tabulate(
    overlap$new_id[candidate_strength >= lineage_threshold],
    nbins = n_new
  ) > 0L

  transition_type[
    predecessor_count > 1L & successor_count > 1L
  ] <- "complex"
  transition_type[
    predecessor_count > 1L & successor_count <= 1L
  ] <- "merger"
  transition_type[
    predecessor_count <= 1L & successor_count > 1L
  ] <- "split"
  transition_type[
    predecessor_count <= 1L & successor_count <= 1L & is_continuation
  ] <- "continuation"
  transition_type[
    predecessor_count <= 1L & successor_count <= 1L &
      !is_continuation & has_lineage_link
  ] <- "replacement"

  has_parent <- !is_continuation & has_lineage_link & has_reference
  parent_id[has_parent] <- old_spatial_ids[reference_old[has_parent]]

  list(
    transition_type = transition_type,
    parent_id = parent_id
  )
}


# Assign lineage identifiers across all observations
#
# Internal helper used by persist_ids(). Uses a disjoint-set structure
# to identify connected components of accepted continuity and strong spatial
# lineage links.
#
assign_lineage_ids <- function(
    n_rows,
    spatial_ids,
    transition_edges,
    lineage_threshold) {

  parent <- seq_len(n_rows)
  rank <- integer(n_rows)

  find_root <- function(value) {
    root <- value
    while (parent[[root]] != root) {
      root <- parent[[root]]
    }

    while (parent[[value]] != value) {
      next_value <- parent[[value]]
      parent[[value]] <<- root
      value <- next_value
    }

    root
  }

  union_rows <- function(left, right) {
    left_root <- find_root(left)
    right_root <- find_root(right)

    if (left_root == right_root) {
      return(invisible(NULL))
    }

    if (rank[[left_root]] < rank[[right_root]]) {
      temporary <- left_root
      left_root <- right_root
      right_root <- temporary
    }

    parent[[right_root]] <<- left_root
    if (rank[[left_root]] == rank[[right_root]]) {
      rank[[left_root]] <<- rank[[left_root]] + 1L
    }

    invisible(NULL)
  }

  if (nrow(transition_edges) > 0L) {
    lineage_links <- transition_edges$selected |
      pmax(
        transition_edges$share_old,
        transition_edges$share_new
      ) >= lineage_threshold

    linked_edges <- transition_edges[lineage_links, ]

    if (nrow(linked_edges) > 0L) {
      for (i in seq_len(nrow(linked_edges))) {
        union_rows(linked_edges$old_row[[i]], linked_edges$new_row[[i]])
      }
    }
  }

  roots <- vapply(seq_len(n_rows), find_root, integer(1))
  members_by_root <- split(spatial_ids, roots)
  origin_by_root <- vapply(members_by_root, min, character(1))
  lineage_origins <- unname(origin_by_root[as.character(roots)])

  sub("^SID", "LID", lineage_origins)
}

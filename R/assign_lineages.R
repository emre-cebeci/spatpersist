# Describe how units enter an observed time period
#
# Internal helper used by create_spatial_ids().
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

  for (new_id in seq_len(n_new)) {
    candidates <- overlap[overlap$new_id == new_id, ]

    if (nrow(candidates) == 0L) {
      next
    }

    selected_old <- matches$old_id[matches$new_id == new_id]
    is_continuation <- length(selected_old) == 1L

    if (is_continuation) {
      reference_old <- selected_old[[1L]]
    } else {
      candidate_strength <- pmax(candidates$share_old, candidates$share_new)
      candidate_order <- order(
        -candidate_strength,
        -candidates$iou,
        -candidates$intersection_area,
        candidates$old_id
      )
      reference_old <- candidates$old_id[candidate_order[[1L]]]
    }

    predecessor_count <- sum(candidates$share_new >= event_threshold)
    successor_count <- sum(
      overlap$old_id == reference_old &
        overlap$share_old >= event_threshold
    )
    has_lineage_link <- any(
      pmax(candidates$share_old, candidates$share_new) >= lineage_threshold
    )

    if (predecessor_count > 1L && successor_count > 1L) {
      transition_type[[new_id]] <- "complex"
    } else if (predecessor_count > 1L) {
      transition_type[[new_id]] <- "merger"
    } else if (successor_count > 1L) {
      transition_type[[new_id]] <- "split"
    } else if (is_continuation) {
      transition_type[[new_id]] <- "continuation"
    } else if (has_lineage_link) {
      transition_type[[new_id]] <- "replacement"
    }

    if (!is_continuation && has_lineage_link) {
      parent_id[[new_id]] <- old_spatial_ids[[reference_old]]
    }
  }

  list(
    transition_type = transition_type,
    parent_id = parent_id
  )
}


# Assign lineage identifiers across all observations
#
# Internal helper used by create_spatial_ids(). Uses a disjoint-set structure
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

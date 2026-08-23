# Match units across two observed time periods
#
# Internal helper used by create_spatial_ids(). Candidate spatial links are
# ranked deterministically and selected one-to-one.
#
match_units <- function(
    overlap,
    threshold,
    metric,
    match_rule,
    ambiguity_tolerance,
    ambiguity_action) {

  empty_selected <- function() {
    data.frame(
      old_id = integer(),
      new_id = integer(),
      score = numeric(),
      confidence = numeric(),
      ambiguous = logical(),
      mutual_best = logical()
    )
  }

  if (nrow(overlap) == 0L) {
    return(
      list(
        selected = empty_selected(),
        candidates = data.frame(
          old_id = integer(),
          new_id = integer(),
          eligible = logical(),
          rule_candidate = logical(),
          candidate = logical(),
          mutual_best = logical(),
          ambiguous = logical()
        )
      )
    )
  }

  scores <- overlap[[metric]]
  eligible <- is.finite(scores) & scores >= threshold

  best_in_group <- function(group_values) {
    result <- logical(nrow(overlap))
    eligible_rows <- which(eligible)
    groups <- split(eligible_rows, group_values[eligible_rows])

    for (rows in groups) {
      result[rows] <- scores[rows] == max(scores[rows])
    }

    result
  }

  ambiguous_in_group <- function(group_values) {
    result <- logical(nrow(overlap))
    eligible_rows <- which(eligible)
    groups <- split(eligible_rows, group_values[eligible_rows])

    for (rows in groups) {
      best_score <- max(scores[rows])
      near_best <- rows[best_score - scores[rows] <= ambiguity_tolerance]
      if (length(near_best) > 1L) {
        result[near_best] <- TRUE
      }
    }

    result
  }

  best_for_old <- best_in_group(overlap$old_id)
  best_for_new <- best_in_group(overlap$new_id)
  mutual_best <- best_for_old & best_for_new
  ambiguous <- ambiguous_in_group(overlap$old_id) |
    ambiguous_in_group(overlap$new_id)

  rule_candidate <- eligible
  if (match_rule == "mutual_best") {
    rule_candidate <- rule_candidate & mutual_best
  }

  if (ambiguity_action == "error" && any(rule_candidate & ambiguous)) {
    ambiguous_rows <- which(rule_candidate & ambiguous)
    first <- ambiguous_rows[[1L]]
    stop(
      "Ambiguous spatial matches detected (",
      length(ambiguous_rows),
      " candidate links); first ambiguous link is old unit ",
      overlap$old_id[[first]],
      " to new unit ",
      overlap$new_id[[first]],
      "."
    )
  }

  candidate <- rule_candidate
  if (ambiguity_action == "new") {
    candidate <- candidate & !ambiguous
  }

  candidate_details <- data.frame(
    old_id = overlap$old_id,
    new_id = overlap$new_id,
    eligible = eligible,
    rule_candidate = rule_candidate,
    candidate = candidate,
    mutual_best = mutual_best,
    ambiguous = ambiguous
  )

  candidate_rows <- which(candidate)
  if (length(candidate_rows) == 0L) {
    return(
      list(
        selected = empty_selected(),
        candidates = candidate_details
      )
    )
  }

  candidate_rows <- candidate_rows[
    order(
      -scores[candidate_rows],
      -overlap$iou[candidate_rows],
      -overlap$intersection_area[candidate_rows],
      overlap$old_id[candidate_rows],
      overlap$new_id[candidate_rows]
    )
  ]

  selected_rows <- integer()
  used_old <- integer()
  used_new <- integer()

  for (row in candidate_rows) {
    old_id <- overlap$old_id[[row]]
    new_id <- overlap$new_id[[row]]

    if (!(old_id %in% used_old) && !(new_id %in% used_new)) {
      selected_rows <- c(selected_rows, row)
      used_old <- c(used_old, old_id)
      used_new <- c(used_new, new_id)
    }
  }

  competition_margin <- function(row, group_values) {
    competitors <- which(
      eligible &
        group_values == group_values[[row]] &
        seq_len(nrow(overlap)) != row
    )

    if (length(competitors) == 0L) {
      return(1)
    }

    max(0, scores[[row]] - max(scores[competitors]))
  }

  confidence <- vapply(
    selected_rows,
    function(row) {
      min(
        1,
        scores[[row]],
        competition_margin(row, overlap$old_id),
        competition_margin(row, overlap$new_id)
      )
    },
    numeric(1)
  )

  selected <- data.frame(
    old_id = overlap$old_id[selected_rows],
    new_id = overlap$new_id[selected_rows],
    score = scores[selected_rows],
    confidence = confidence,
    ambiguous = ambiguous[selected_rows],
    mutual_best = mutual_best[selected_rows]
  )

  list(
    selected = selected,
    candidates = candidate_details
  )
}

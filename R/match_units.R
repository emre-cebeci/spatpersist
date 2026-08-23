# Match units across two observed time periods
#
# Internal helper used by persist_ids(). Candidate spatial links are
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

  group_diagnostics <- function(group_values) {
    best <- logical(nrow(overlap))
    ambiguous <- logical(nrow(overlap))
    competition_margin <- rep(1, nrow(overlap))
    eligible_rows <- which(eligible)
    groups <- split(eligible_rows, group_values[eligible_rows])

    for (rows in groups) {
      group_scores <- scores[rows]
      best_score <- max(group_scores)
      best_rows <- rows[group_scores == best_score]
      best[best_rows] <- TRUE

      near_best <- rows[best_score - group_scores <= ambiguity_tolerance]
      if (length(near_best) > 1L) {
        ambiguous[near_best] <- TRUE
      }

      if (length(rows) > 1L) {
        competition_margin[rows] <- 0
        if (length(best_rows) == 1L) {
          second_best <- max(group_scores[group_scores < best_score])
          competition_margin[best_rows] <- best_score - second_best
        }
      }
    }

    list(
      best = best,
      ambiguous = ambiguous,
      competition_margin = competition_margin
    )
  }

  old_diagnostics <- group_diagnostics(overlap$old_id)
  new_diagnostics <- group_diagnostics(overlap$new_id)
  mutual_best <- old_diagnostics$best & new_diagnostics$best
  ambiguous <- old_diagnostics$ambiguous |
    new_diagnostics$ambiguous

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

  selected_rows <- integer(length(candidate_rows))
  selected_count <- 0L
  used_old <- logical(max(overlap$old_id))
  used_new <- logical(max(overlap$new_id))

  for (row in candidate_rows) {
    old_id <- overlap$old_id[[row]]
    new_id <- overlap$new_id[[row]]

    if (!used_old[[old_id]] && !used_new[[new_id]]) {
      selected_count <- selected_count + 1L
      selected_rows[[selected_count]] <- row
      used_old[[old_id]] <- TRUE
      used_new[[new_id]] <- TRUE
    }
  }
  selected_rows <- selected_rows[seq_len(selected_count)]

  confidence <- pmin(
    1,
    scores[selected_rows],
    old_diagnostics$competition_margin[selected_rows],
    new_diagnostics$competition_margin[selected_rows]
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

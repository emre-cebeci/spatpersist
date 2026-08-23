#' Validate persistent spatial identifiers
#'
#' Checks a result for structural inconsistencies, including missing output
#' values, duplicate identities within a time period, identities assigned to
#' multiple lineages, invalid geometry-version sequences, invalid transition
#' labels, invalid match scores, and broken parent or continuation links.
#'
#' @param data A data frame or `sf` object containing spatial ID output.
#' @param time A single character string naming the time column in `data`.
#'
#' @return A data frame describing validation issues. A result with zero rows
#'   passed all implemented checks.
#'
#' @export
#'
#' @examples
#' result <- create_spatial_ids(spatialid_example(), time = "year")
#' validate_spatial_ids(result, time = "year")
#'
validate_spatial_ids <- function(data, time) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or sf object.")
  }

  if (!is.character(time) || length(time) != 1L || is.na(time) ||
      !nzchar(time) || !(time %in% names(data))) {
    stop("`time` must name one column in `data`.")
  }

  issues <- list()
  add_issue <- function(check, row, message, severity = "error") {
    issues[[length(issues) + 1L]] <<- data.frame(
      severity = severity,
      check = check,
      row = as.integer(row),
      message = message
    )
  }

  issue_table <- function() {
    if (length(issues) == 0L) {
      return(
        data.frame(
          severity = character(),
          check = character(),
          row = integer(),
          message = character()
        )
      )
    }

    result <- do.call(rbind, issues)
    rownames(result) <- NULL
    result
  }

  required_columns <- c(
    "spatial_id",
    "spatial_version_id",
    "lineage_id",
    "parent_id",
    "transition_type",
    "match_score",
    "match_confidence",
    "match_ambiguous",
    "registry_matched"
  )
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0L) {
    for (column in missing_columns) {
      add_issue(
        check = "missing_column",
        row = NA_integer_,
        message = paste0("Required column `", column, "` is missing.")
      )
    }
    return(issue_table())
  }

  required_values <- c(
    "spatial_id",
    "spatial_version_id",
    "lineage_id",
    "transition_type"
  )
  for (column in required_values) {
    values <- as.character(data[[column]])
    missing_rows <- which(is.na(values) | !nzchar(values))
    for (row in missing_rows) {
      add_issue(
        check = "missing_value",
        row = row,
        message = paste0("`", column, "` is missing.")
      )
    }
  }

  missing_time_rows <- which(is.na(data[[time]]))
  for (row in missing_time_rows) {
    add_issue(
      check = "missing_time",
      row = row,
      message = "The time value is missing."
    )
  }

  valid_identity_rows <- !is.na(data[[time]]) & !is.na(data$spatial_id)
  identity_pairs <- data.frame(
    time = data[[time]][valid_identity_rows],
    spatial_id = data$spatial_id[valid_identity_rows]
  )
  duplicate_pairs <- duplicated(identity_pairs) |
    duplicated(identity_pairs, fromLast = TRUE)
  duplicate_rows <- which(valid_identity_rows)[duplicate_pairs]

  for (row in duplicate_rows) {
    add_issue(
      check = "duplicate_identity_within_time",
      row = row,
      message = paste0(
        "Spatial ID `",
        data$spatial_id[[row]],
        "` occurs more than once at this time."
      )
    )
  }

  spatial_ids <- unique(data$spatial_id[!is.na(data$spatial_id)])
  for (spatial_id in spatial_ids) {
    rows <- which(data$spatial_id == spatial_id)
    lineages <- unique(data$lineage_id[rows])
    lineages <- lineages[!is.na(lineages)]

    if (length(lineages) > 1L) {
      add_issue(
        check = "identity_multiple_lineages",
        row = rows[[1L]],
        message = paste0(
          "Spatial ID `",
          spatial_id,
          "` belongs to multiple lineages."
        )
      )
    }
  }

  valid_version_rows <- rep(TRUE, nrow(data))
  for (row in seq_len(nrow(data))) {
    spatial_id <- data$spatial_id[[row]]
    version_id <- data$spatial_version_id[[row]]

    if (is.na(spatial_id) || is.na(version_id)) {
      valid_version_rows[[row]] <- FALSE
      next
    }

    prefix <- paste0(spatial_id, "-V")
    suffix <- substring(version_id, nchar(prefix) + 1L)
    version_is_valid <- startsWith(version_id, prefix) &&
      grepl("^[0-9]{3,}$", suffix)
    valid_version_rows[[row]] <- version_is_valid

    if (!version_is_valid) {
      add_issue(
        check = "invalid_version_id",
        row = row,
        message = paste0(
          "Version ID `",
          version_id,
          "` does not belong to spatial ID `",
          spatial_id,
          "`."
        )
      )
    }
  }

  numeric_match_columns <- c("match_score", "match_confidence")
  for (column in numeric_match_columns) {
    if (!is.numeric(data[[column]])) {
      add_issue(
        check = "invalid_match_column",
        row = NA_integer_,
        message = paste0("`", column, "` must be numeric.")
      )
      next
    }

    invalid_rows <- which(
      !is.na(data[[column]]) &
        (!is.finite(data[[column]]) |
          data[[column]] < 0 | data[[column]] > 1)
    )
    for (row in invalid_rows) {
      add_issue(
        check = "invalid_match_value",
        row = row,
        message = paste0("`", column, "` must be between 0 and 1.")
      )
    }
  }

  if (!is.logical(data$match_ambiguous)) {
    add_issue(
      check = "invalid_match_column",
      row = NA_integer_,
      message = "`match_ambiguous` must be logical."
    )
  } else {
    missing_ambiguity_rows <- which(is.na(data$match_ambiguous))
    for (row in missing_ambiguity_rows) {
      add_issue(
        check = "invalid_match_value",
        row = row,
        message = "`match_ambiguous` cannot be missing."
      )
    }
  }

  if (!is.logical(data$registry_matched)) {
    add_issue(
      check = "invalid_registry_column",
      row = NA_integer_,
      message = "`registry_matched` must be logical."
    )
  } else {
    missing_registry_rows <- which(is.na(data$registry_matched))
    for (row in missing_registry_rows) {
      add_issue(
        check = "invalid_registry_value",
        row = row,
        message = "`registry_matched` cannot be missing."
      )
    }
  }

  allowed_transitions <- c(
    "initial",
    "continuation",
    "new",
    "split",
    "merger",
    "replacement",
    "complex"
  )
  invalid_transition_rows <- which(
    !is.na(data$transition_type) &
      !(data$transition_type %in% allowed_transitions)
  )
  for (row in invalid_transition_rows) {
    add_issue(
      check = "invalid_transition_type",
      row = row,
      message = paste0(
        "Unknown transition type `",
        data$transition_type[[row]],
        "`."
      )
    )
  }

  observed_times <- sort(unique(data[[time]][!is.na(data[[time]])]))
  time_rank <- match(data[[time]], observed_times)

  for (spatial_id in spatial_ids) {
    rows <- which(data$spatial_id == spatial_id & valid_version_rows)
    if (length(rows) == 0L) {
      next
    }

    rows <- rows[order(time_rank[rows])]
    prefix <- paste0(spatial_id, "-V")
    version_numbers <- as.integer(
      substring(data$spatial_version_id[rows], nchar(prefix) + 1L)
    )

    if (version_numbers[[1L]] != 1L) {
      add_issue(
        check = "invalid_version_sequence",
        row = rows[[1L]],
        message = paste0("Spatial ID `", spatial_id, "` does not start at V001.")
      )
    }

    if (length(rows) > 1L) {
      changes <- diff(version_numbers)
      invalid_changes <- which(!(changes %in% c(0L, 1L))) + 1L

      for (position in invalid_changes) {
        add_issue(
          check = "invalid_version_sequence",
          row = rows[[position]],
          message = paste0(
            "Spatial ID `",
            spatial_id,
            "` has a non-sequential geometry version."
          )
        )
      }
    }
  }

  if (length(observed_times) > 0L) {
    invalid_initial_rows <- which(
      !is.na(time_rank) & time_rank > 1L & data$transition_type == "initial"
    )
    for (row in invalid_initial_rows) {
      add_issue(
        check = "invalid_initial_transition",
        row = row,
        message = "An `initial` observation occurs after the first time period."
      )
    }

    missing_initial_rows <- which(
      time_rank == 1L & data$transition_type != "initial"
    )
    for (row in missing_initial_rows) {
      add_issue(
        check = "invalid_initial_transition",
        row = row,
        message = "A first-period observation is not marked `initial`."
      )
    }
  }

  continuation_rows <- which(data$transition_type == "continuation")
  for (row in continuation_rows) {
    previous_rank <- time_rank[[row]] - 1L
    has_predecessor <- !is.na(previous_rank) && previous_rank >= 1L && any(
      data$spatial_id == data$spatial_id[[row]] &
        time_rank == previous_rank,
      na.rm = TRUE
    )

    if (!has_predecessor) {
      add_issue(
        check = "broken_continuation",
        row = row,
        message = "A continuation has no matching identity in the previous period."
      )
    }
  }

  if (is.numeric(data$match_score) && is.numeric(data$match_confidence)) {
    for (row in seq_len(nrow(data))) {
      if (is.na(time_rank[[row]]) || time_rank[[row]] <= 1L ||
          is.na(data$spatial_id[[row]])) {
        next
      }

      previous_identity_exists <- any(
        data$spatial_id == data$spatial_id[[row]] &
          time_rank == time_rank[[row]] - 1L,
        na.rm = TRUE
      )

      if (previous_identity_exists &&
          (is.na(data$match_score[[row]]) ||
            is.na(data$match_confidence[[row]]))) {
        add_issue(
          check = "missing_match_diagnostics",
          row = row,
          message = "A continuing identity is missing its match score or confidence."
        )
      }
    }
  }

  parent_rows <- which(!is.na(data$parent_id) & nzchar(data$parent_id))
  for (row in parent_rows) {
    parent_id <- data$parent_id[[row]]
    previous_rank <- time_rank[[row]] - 1L
    possible_parents <- which(
      data$spatial_id == parent_id & time_rank == previous_rank
    )

    if (length(possible_parents) == 0L) {
      add_issue(
        check = "missing_parent",
        row = row,
        message = paste0(
          "Parent ID `",
          parent_id,
          "` is not present in the previous period."
        )
      )
      next
    }

    parent_lineages <- data$lineage_id[possible_parents]
    if (!(data$lineage_id[[row]] %in% parent_lineages)) {
      add_issue(
        check = "parent_lineage_mismatch",
        row = row,
        message = "The observation and its parent belong to different lineages."
      )
    }
  }

  issue_table()
}

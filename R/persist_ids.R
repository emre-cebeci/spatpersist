#' Create persistent identifiers for longitudinal polygons
#'
#' Assigns persistent identifiers by comparing polygons in each pair of
#' successive observed time periods. A polygon continues an earlier identity
#' when its selected overlap statistic meets `threshold`. Candidate links are
#' selected one-to-one, so a persistent identifier cannot occur more than once
#' in the same time period.
#'
#' Names and other non-spatial attributes are not used for matching. Unmatched
#' polygons receive new identifiers. Strong spatial links connect related
#' identities in a shared lineage and are used to describe splits, mergers,
#' replacements, and complex reorganizations.
#'
#' Candidate-link diagnostics are stored with the result and can be retrieved
#' with [id_transitions()].
#'
#' Identifiers are dataset-local. Values such as `SID000001` and `LID000001`
#' are persistent only within one dataset and its registry chain; they are not
#' globally unique across independent projects. Exact same-period coextensive
#' geometries are rejected because geometry-only matching cannot distinguish
#' their identities reproducibly.
#'
#' @param data An `sf` object containing polygon geometries observed over time.
#' @param time A single character string naming the time column in `data`.
#' @param threshold A number from 0 to 1 giving the minimum overlap required
#'   for continuity.
#' @param metric The overlap statistic used for matching: `"share_old"`,
#'   `"share_new"`, or `"iou"`.
#' @param match_rule Candidate selection rule: `"greedy"` considers every
#'   eligible link, while `"mutual_best"` considers only links that are best
#'   for both their predecessor and successor.
#' @param ambiguity_tolerance Maximum difference between eligible top scores
#'   that is treated as a near tie.
#' @param ambiguity_action How near ties should be handled: `"flag"` selects
#'   deterministically and marks the result, `"new"` rejects ambiguous links,
#'   and `"error"` stops before assigning IDs.
#' @param event_threshold A number from 0 to 1 giving the minimum share of a
#'   predecessor or successor used when detecting splits and mergers.
#' @param lineage_threshold A number from 0 to 1 giving the minimum share of
#'   the smaller polygon that must overlap to connect two observations in the
#'   same lineage. Accepted identity matches are always connected.
#' @param version_threshold A number from 0 to 1 giving the minimum IoU needed
#'   for a continuing identity to retain its current geometry version. The
#'   default of 1 creates a new version for any boundary change.
#' @param geometry_action How invalid polygon geometries should be handled:
#'   `"error"` or `"repair"`. Repair uses [sf::st_make_valid()] and returns the
#'   repaired geometries.
#' @param geometry_precision Optional positive precision scale passed to
#'   [sf::st_set_precision()] before spatial operations. Coordinates are
#'   rounded as `round(x * geometry_precision) / geometry_precision`; for
#'   example, use 100 for two decimal places. `NULL` preserves input precision.
#' @param max_time_gap Maximum gap across which identities may continue.
#'   Defaults to `Inf`. Finite values require a numeric, `Date`, or `POSIXt`
#'   time column; `Date` gaps use days and `POSIXt` gaps use seconds.
#' @param registry Optional prior `sf` result from [persist_ids()].
#'   Same-time geometries are used to preserve previously issued spatial and
#'   geometry-version identifiers across reruns. Existing lineage identifiers
#'   are also retained unless new spatial evidence connects multiple registry
#'   lineages; the connected component then uses the lexicographically smallest
#'   existing lineage identifier.
#' @param registry_threshold Minimum same-time IoU required to recover an
#'   observation from `registry`.
#'
#' @return The input `sf` object with identifier and transition columns,
#'   including `spatial_id`, `spatial_version_id`, `lineage_id`, `parent_id`,
#'   `transition_type`, `match_score`, `match_confidence`, and
#'   `match_ambiguous`. `registry_matched` indicates recovered observations.
#'
#' @export
#'
#' @examples
#' data <- example_units()
#' result <- persist_ids(data, time = "year", threshold = 0.75)
#' result[, c("year", "name", "spatial_id")]
#'
persist_ids <- function(
    data,
    time,
    threshold = 0.75,
    metric = c("share_old", "share_new", "iou"),
    match_rule = c("greedy", "mutual_best"),
    ambiguity_tolerance = 0.05,
    ambiguity_action = c("flag", "new", "error"),
    event_threshold = 0.1,
    lineage_threshold = 0.6,
    version_threshold = 1,
    geometry_action = c("error", "repair"),
    geometry_precision = NULL,
    max_time_gap = Inf,
    registry = NULL,
    registry_threshold = 0.999999) {

  if (!inherits(data, "sf")) {
    stop("`data` must be an sf object.")
  }

  if (!is.character(time) || length(time) != 1L || is.na(time) ||
      !nzchar(time)) {
    stop("`time` must be a single non-empty column name.")
  }

  if (!(time %in% names(data))) {
    stop("`time` must name a column in `data`.")
  }

  output_columns <- c(
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
  existing_output_columns <- intersect(output_columns, names(data))
  if (length(existing_output_columns) > 0L) {
    stop(
      "`data` already contains output columns: ",
      paste(existing_output_columns, collapse = ", "),
      "."
    )
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one polygon.")
  }

  if (anyNA(data[[time]])) {
    stop("The time column cannot contain missing values.")
  }

  if (!is.numeric(threshold) || length(threshold) != 1L ||
      is.na(threshold) || !is.finite(threshold) ||
      threshold < 0 || threshold > 1) {
    stop("`threshold` must be one finite number from 0 to 1.")
  }

  if (!is.numeric(event_threshold) || length(event_threshold) != 1L ||
      is.na(event_threshold) || !is.finite(event_threshold) ||
      event_threshold < 0 || event_threshold > 1) {
    stop("`event_threshold` must be one finite number from 0 to 1.")
  }

  if (!is.numeric(ambiguity_tolerance) || length(ambiguity_tolerance) != 1L ||
      is.na(ambiguity_tolerance) || !is.finite(ambiguity_tolerance) ||
      ambiguity_tolerance < 0 || ambiguity_tolerance > 1) {
    stop("`ambiguity_tolerance` must be one finite number from 0 to 1.")
  }

  if (!is.numeric(lineage_threshold) || length(lineage_threshold) != 1L ||
      is.na(lineage_threshold) || !is.finite(lineage_threshold) ||
      lineage_threshold < 0 || lineage_threshold > 1) {
    stop("`lineage_threshold` must be one finite number from 0 to 1.")
  }

  if (!is.numeric(version_threshold) || length(version_threshold) != 1L ||
      is.na(version_threshold) || !is.finite(version_threshold) ||
      version_threshold < 0 || version_threshold > 1) {
    stop("`version_threshold` must be one finite number from 0 to 1.")
  }

  if (!is.null(geometry_precision) &&
      (!is.numeric(geometry_precision) || length(geometry_precision) != 1L ||
        is.na(geometry_precision) || !is.finite(geometry_precision) ||
        geometry_precision <= 0)) {
    stop("`geometry_precision` must be NULL or one positive finite number.")
  }

  if (!is.numeric(max_time_gap) || length(max_time_gap) != 1L ||
      is.na(max_time_gap) || max_time_gap <= 0) {
    stop("`max_time_gap` must be one positive number or Inf.")
  }

  if (!is.numeric(registry_threshold) || length(registry_threshold) != 1L ||
      is.na(registry_threshold) || !is.finite(registry_threshold) ||
      registry_threshold < 0 || registry_threshold > 1) {
    stop("`registry_threshold` must be one finite number from 0 to 1.")
  }

  metric <- match.arg(metric)
  match_rule <- match.arg(match_rule)
  ambiguity_action <- match.arg(ambiguity_action)
  geometry_action <- match.arg(geometry_action)

  if (is.finite(max_time_gap) &&
      !(is.numeric(data[[time]]) || inherits(data[[time]], "Date") ||
        inherits(data[[time]], "POSIXt"))) {
    stop(
      "A finite `max_time_gap` requires a numeric, Date, or POSIXt time column."
    )
  }

  data <- prepare_spatial_input(
    data = data,
    geometry_action = geometry_action,
    geometry_precision = geometry_precision
  )
  check_coextensive_rows(data, time = time)

  observed_times <- sort(unique(data[[time]]))
  rows_by_time <- lapply(
    observed_times,
    function(observed_time) {
      order_spatial_rows(data, which(data[[time]] == observed_time))
    }
  )
  spatial_ids <- rep(NA_character_, nrow(data))
  version_numbers <- rep(NA_integer_, nrow(data))
  parent_ids <- rep(NA_character_, nrow(data))
  transition_types <- rep(NA_character_, nrow(data))
  match_scores <- rep(NA_real_, nrow(data))
  match_confidences <- rep(NA_real_, nrow(data))
  match_ambiguous <- rep(FALSE, nrow(data))
  transition_edges <- list()
  next_id <- 1L

  first_rows <- rows_by_time[[1L]]
  first_numbers <- seq.int(next_id, length.out = length(first_rows))
  spatial_ids[first_rows] <- sprintf("SID%06d", first_numbers)
  version_numbers[first_rows] <- 1L
  transition_types[first_rows] <- "initial"
  next_id <- next_id + length(first_rows)

  if (length(observed_times) > 1L) {
    for (period in seq.int(2L, length(observed_times))) {
      old_rows <- rows_by_time[[period - 1L]]
      new_rows <- rows_by_time[[period]]

      if (is.finite(max_time_gap)) {
        gap <- spatial_time_gap(
          observed_times[[period - 1L]],
          observed_times[[period]]
        )
        if (gap > max_time_gap) {
          new_numbers <- seq.int(next_id, length.out = length(new_rows))
          spatial_ids[new_rows] <- sprintf("SID%06d", new_numbers)
          version_numbers[new_rows] <- 1L
          transition_types[new_rows] <- "new"
          next_id <- next_id + length(new_rows)
          next
        }
      }

      overlap <- calculate_overlap(data[old_rows, ], data[new_rows, ])
      match_result <- match_units(
        overlap = overlap,
        threshold = threshold,
        metric = metric,
        match_rule = match_rule,
        ambiguity_tolerance = ambiguity_tolerance,
        ambiguity_action = ambiguity_action
      )
      matches <- match_result$selected

      if (nrow(matches) > 0L) {
        matched_rows <- new_rows[matches$new_id]
        spatial_ids[matched_rows] <-
          spatial_ids[old_rows[matches$old_id]]
        match_scores[matched_rows] <- matches$score
        match_confidences[matched_rows] <- matches$confidence
        match_ambiguous[matched_rows] <- matches$ambiguous
      }

      version_numbers[new_rows] <- assign_continuing_versions(
        overlap = overlap,
        matches = matches,
        old_version_numbers = version_numbers[old_rows],
        n_new = length(new_rows),
        version_threshold = version_threshold
      )

      transition_description <- describe_transitions(
        overlap = overlap,
        matches = matches,
        old_spatial_ids = spatial_ids[old_rows],
        n_new = length(new_rows),
        event_threshold = event_threshold,
        lineage_threshold = lineage_threshold
      )
      transition_types[new_rows] <- transition_description$transition_type
      parent_ids[new_rows] <- transition_description$parent_id

      if (nrow(overlap) > 0L) {
        overlap$old_row <- old_rows[overlap$old_id]
        overlap$new_row <- new_rows[overlap$new_id]
        overlap$eligible <- match_result$candidates$eligible
        overlap$rule_candidate <- match_result$candidates$rule_candidate
        overlap$candidate <- match_result$candidates$candidate
        overlap$mutual_best <- match_result$candidates$mutual_best
        overlap$ambiguous <- match_result$candidates$ambiguous
        overlap$selected <- FALSE
        overlap$match_confidence <- NA_real_

        if (nrow(matches) > 0L) {
          overlap_keys <- paste(overlap$old_id, overlap$new_id, sep = ":")
          match_keys <- paste(matches$old_id, matches$new_id, sep = ":")
          selected_positions <- match(overlap_keys, match_keys)
          overlap$selected <- !is.na(selected_positions)
          overlap$match_confidence <- matches$confidence[selected_positions]
        }

        overlap$lineage_link <- overlap$selected |
          pmax(overlap$share_old, overlap$share_new) >= lineage_threshold

        transition_edges[[length(transition_edges) + 1L]] <- overlap
      }

      unmatched_rows <- new_rows[is.na(spatial_ids[new_rows])]
      if (length(unmatched_rows) > 0L) {
        new_numbers <- seq.int(next_id, length.out = length(unmatched_rows))
        spatial_ids[unmatched_rows] <- sprintf("SID%06d", new_numbers)
        version_numbers[unmatched_rows] <- 1L
        next_id <- next_id + length(unmatched_rows)
      }
    }
  }

  if (length(transition_edges) == 0L) {
    all_transition_edges <- data.frame(
      old_row = integer(),
      new_row = integer(),
      share_old = numeric(),
      share_new = numeric(),
      selected = logical()
    )
  } else {
    all_transition_edges <- do.call(rbind, transition_edges)
  }

  registry_matched <- rep(FALSE, nrow(data))
  registry_lineage <- rep(NA_character_, nrow(data))

  if (!is.null(registry)) {
    registry_result <- match_registry_rows(
      data = data,
      time = time,
      registry = registry,
      registry_threshold = registry_threshold,
      geometry_precision = geometry_precision
    )
    reconciled <- reconcile_registry_ids(
      spatial_ids = spatial_ids,
      version_numbers = version_numbers,
      parent_ids = parent_ids,
      registry = registry_result$registry,
      registry_match = registry_result$registry_match
    )
    spatial_ids <- reconciled$spatial_ids
    version_numbers <- reconciled$version_numbers
    parent_ids <- reconciled$parent_ids
    registry_matched <- reconciled$registry_matched
    registry_lineage <- reconciled$registry_lineage
  }

  lineage_ids <- assign_lineage_ids(
    n_rows = nrow(data),
    spatial_ids = spatial_ids,
    transition_edges = all_transition_edges,
    lineage_threshold = lineage_threshold
  )

  if (!is.null(registry)) {
    lineage_ids <- reconcile_registry_lineages(
      lineage_ids = lineage_ids,
      registry_lineage = registry_lineage
    )
  }

  spatial_version_ids <- paste0(
    spatial_ids,
    "-V",
    sprintf("%03d", version_numbers)
  )

  data$spatial_id <- spatial_ids
  data$spatial_version_id <- spatial_version_ids
  data$lineage_id <- lineage_ids
  data$parent_id <- parent_ids
  data$transition_type <- transition_types
  data$match_score <- match_scores
  data$match_confidence <- match_confidences
  data$match_ambiguous <- match_ambiguous
  data$registry_matched <- registry_matched
  attr(data, "id_transitions") <- build_transition_diagnostics(
    transition_edges = all_transition_edges,
    time_values = data[[time]],
    spatial_ids = spatial_ids,
    spatial_version_ids = spatial_version_ids,
    lineage_ids = lineage_ids,
    parent_ids = parent_ids,
    transition_types = transition_types,
    registry_matched = registry_matched,
    metric = metric,
    match_rule = match_rule
  )
  data
}

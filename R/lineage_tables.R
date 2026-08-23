#' Summarize persistent spatial identities and lineages
#'
#' Creates one row per `spatial_id` with its lineage, parent, observed time
#' span, number of observations and geometry versions, registry recovery count,
#' ambiguity count, and minimum non-missing match confidence.
#'
#' @param data A result returned by [create_spatial_ids()].
#' @param time A single character string naming the time column in `data`.
#'
#' @return A data frame with one row per persistent spatial identity.
#'
#' @export
#'
#' @examples
#' result <- create_spatial_ids(spatialid_example(), time = "year")
#' spatialid_lineages(result, time = "year")
#'
spatialid_lineages <- function(data, time) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or sf object.")
  }

  if (!is.character(time) || length(time) != 1L || is.na(time) ||
      !nzchar(time) || !(time %in% names(data))) {
    stop("`time` must name one column in `data`.")
  }

  required_columns <- c(
    "spatial_id",
    "spatial_version_id",
    "lineage_id",
    "parent_id",
    "transition_type",
    "match_confidence",
    "match_ambiguous",
    "registry_matched"
  )
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "`data` is missing spatialid columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }

  if (nrow(data) == 0L) {
    return(
      data.frame(
        spatial_id = character(),
        lineage_id = character(),
        parent_id = character(),
        first_time = data[[time]][integer()],
        last_time = data[[time]][integer()],
        first_transition = character(),
        n_observations = integer(),
        n_versions = integer(),
        registry_observations = integer(),
        ambiguous_matches = integer(),
        min_match_confidence = numeric()
      )
    )
  }

  observed_times <- sort(unique(data[[time]]))
  time_rank <- match(data[[time]], observed_times)
  spatial_ids <- sort(unique(data$spatial_id))
  summaries <- vector("list", length(spatial_ids))

  for (i in seq_along(spatial_ids)) {
    spatial_id <- spatial_ids[[i]]
    rows <- which(data$spatial_id == spatial_id)
    rows <- rows[order(time_rank[rows])]
    lineages <- unique(data$lineage_id[rows])

    if (length(lineages) != 1L) {
      stop(
        "Spatial ID `",
        spatial_id,
        "` belongs to multiple lineages; run `validate_spatial_ids()`."
      )
    }

    parents <- unique(data$parent_id[rows])
    parents <- parents[!is.na(parents) & nzchar(parents)]
    confidence <- data$match_confidence[rows]
    confidence <- confidence[!is.na(confidence)]

    summaries[[i]] <- data.frame(
      spatial_id = spatial_id,
      lineage_id = lineages[[1L]],
      parent_id = if (length(parents) == 0L) {
        NA_character_
      } else {
        paste(sort(parents), collapse = ";")
      },
      first_time = data[[time]][rows[[1L]]],
      last_time = data[[time]][rows[[length(rows)]]],
      first_transition = data$transition_type[rows[[1L]]],
      n_observations = length(rows),
      n_versions = length(unique(data$spatial_version_id[rows])),
      registry_observations = sum(data$registry_matched[rows]),
      ambiguous_matches = sum(data$match_ambiguous[rows]),
      min_match_confidence = if (length(confidence) == 0L) {
        NA_real_
      } else {
        min(confidence)
      }
    )
  }

  result <- do.call(rbind, summaries)
  result <- result[order(result$lineage_id, result$spatial_id), ]
  rownames(result) <- NULL
  result
}

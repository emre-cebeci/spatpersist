# Match current observations to a prior spatialid result
#
# Internal helper used by create_spatial_ids().
#
match_registry_rows <- function(
    data,
    time,
    registry,
    registry_threshold,
    geometry_precision) {

  if (!inherits(registry, "sf")) {
    stop("`registry` must be an sf result returned by `create_spatial_ids()`.")
  }

  if (!(time %in% names(registry))) {
    stop("The registry does not contain the requested time column.")
  }

  if (nrow(registry) == 0L) {
    stop("`registry` must contain at least one prior observation.")
  }

  if (anyNA(registry[[time]])) {
    stop("The registry time column cannot contain missing values.")
  }

  required_columns <- c("spatial_id", "spatial_version_id", "lineage_id")
  missing_columns <- setdiff(required_columns, names(registry))
  if (length(missing_columns) > 0L) {
    stop(
      "The registry is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }

  if (!isTRUE(sf::st_crs(data) == sf::st_crs(registry))) {
    stop("`data` and `registry` must use the same coordinate reference system.")
  }

  registry <- prepare_spatial_input(
    data = registry,
    geometry_action = "error",
    geometry_precision = geometry_precision
  )

  valid_spatial_ids <- grepl("^SID[0-9]+$", registry$spatial_id)
  if (any(is.na(valid_spatial_ids) | !valid_spatial_ids)) {
    stop("Registry spatial IDs must use the package format `SID` plus digits.")
  }

  version_prefixes <- paste0(registry$spatial_id, "-V")
  version_suffixes <- substring(
    registry$spatial_version_id,
    nchar(version_prefixes) + 1L
  )
  valid_version_ids <- startsWith(
    registry$spatial_version_id,
    version_prefixes
  ) & grepl("^[0-9]{3,}$", version_suffixes)
  if (any(is.na(valid_version_ids) | !valid_version_ids)) {
    stop("Registry geometry-version IDs are malformed or inconsistent.")
  }

  registry_pairs <- data.frame(
    time = as.character(registry[[time]]),
    spatial_id = registry$spatial_id
  )
  if (anyDuplicated(registry_pairs)) {
    stop("The registry contains duplicate spatial IDs within a time period.")
  }

  registry_match <- rep(NA_integer_, nrow(data))
  data_time_keys <- as.character(data[[time]])
  registry_time_keys <- as.character(registry[[time]])

  for (time_key in unique(data_time_keys)) {
    data_rows <- which(data_time_keys == time_key)
    registry_rows <- which(registry_time_keys == time_key)

    if (length(registry_rows) == 0L) {
      next
    }

    data_rows <- order_spatial_rows(data, data_rows)
    registry_rows <- order_spatial_rows(registry, registry_rows)
    overlap <- calculate_overlap(
      registry[registry_rows, ],
      data[data_rows, ]
    )
    registry_result <- match_units(
      overlap = overlap,
      threshold = registry_threshold,
      metric = "iou",
      match_rule = "mutual_best",
      ambiguity_tolerance = 1e-12,
      ambiguity_action = "error"
    )
    matches <- registry_result$selected

    if (nrow(matches) > 0L) {
      registry_match[data_rows[matches$new_id]] <-
        registry_rows[matches$old_id]
    }
  }

  list(
    registry = registry,
    registry_match = registry_match
  )
}


# Reconcile newly calculated IDs with registry IDs
#
# Internal helper used by create_spatial_ids().
#
reconcile_registry_ids <- function(
    spatial_ids,
    version_numbers,
    parent_ids,
    registry,
    registry_match) {

  original_spatial_ids <- spatial_ids
  fresh_ids <- sort(unique(original_spatial_ids))
  id_map <- stats::setNames(rep(NA_character_, length(fresh_ids)), fresh_ids)

  for (fresh_id in fresh_ids) {
    rows <- which(original_spatial_ids == fresh_id & !is.na(registry_match))
    registry_ids <- unique(registry$spatial_id[registry_match[rows]])

    if (length(registry_ids) > 1L) {
      stop(
        "Registry conflict: calculated identity `",
        fresh_id,
        "` maps to multiple existing spatial IDs."
      )
    }

    if (length(registry_ids) == 1L) {
      id_map[[fresh_id]] <- registry_ids[[1L]]
    }
  }

  claimed_ids <- id_map[!is.na(id_map)]
  duplicated_claims <- unique(claimed_ids[duplicated(claimed_ids)])
  if (length(duplicated_claims) > 0L) {
    stop(
      "Registry conflict: multiple calculated identities claim existing ID `",
      duplicated_claims[[1L]],
      "`."
    )
  }

  registry_numbers <- as.integer(sub("^SID", "", registry$spatial_id))
  next_number <- max(registry_numbers) + 1L
  unmapped_ids <- names(id_map)[is.na(id_map)]

  for (fresh_id in unmapped_ids) {
    id_map[[fresh_id]] <- sprintf("SID%06d", next_number)
    next_number <- next_number + 1L
  }

  for (fresh_id in fresh_ids) {
    rows <- which(original_spatial_ids == fresh_id)
    anchored_rows <- rows[!is.na(registry_match[rows])]

    if (length(anchored_rows) == 0L) {
      next
    }

    registry_rows <- registry_match[anchored_rows]
    prefixes <- paste0(registry$spatial_id[registry_rows], "-V")
    registry_versions <- as.integer(
      substring(
        registry$spatial_version_id[registry_rows],
        nchar(prefixes) + 1L
      )
    )
    offsets <- unique(
      registry_versions - version_numbers[anchored_rows]
    )

    if (length(offsets) > 1L) {
      stop(
        "Registry conflict: geometry versions for calculated identity `",
        fresh_id,
        "` cannot be reconciled consistently."
      )
    }

    version_numbers[rows] <- version_numbers[rows] + offsets[[1L]]
    if (any(version_numbers[rows] < 1L)) {
      stop(
        "Registry conflict: preserving existing versions would require a ",
        "geometry version before V001 for identity `",
        fresh_id,
        "`."
      )
    }
  }

  spatial_ids <- unname(id_map[original_spatial_ids])
  parent_rows <- which(!is.na(parent_ids) & nzchar(parent_ids))
  if (length(parent_rows) > 0L) {
    parent_ids[parent_rows] <- unname(id_map[parent_ids[parent_rows]])
  }

  list(
    spatial_ids = spatial_ids,
    version_numbers = version_numbers,
    parent_ids = parent_ids,
    registry_matched = !is.na(registry_match),
    registry_lineage = ifelse(
      is.na(registry_match),
      NA_character_,
      registry$lineage_id[registry_match]
    )
  )
}


# Preserve registry lineage labels where possible
#
# Internal helper used by create_spatial_ids(). Newly connected registry
# lineages consolidate deterministically to their smallest existing label.
#
reconcile_registry_lineages <- function(lineage_ids, registry_lineage) {

  for (lineage_id in unique(lineage_ids)) {
    rows <- which(lineage_ids == lineage_id)
    anchors <- unique(registry_lineage[rows])
    anchors <- anchors[!is.na(anchors)]

    if (length(anchors) > 0L) {
      lineage_ids[rows] <- min(anchors)
    }
  }

  lineage_ids
}

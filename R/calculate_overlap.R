# Calculate spatial overlap between two polygon datasets
#
# Internal helper function used by persist_ids().
#
# Calculates pairwise polygon intersections and returns overlap
# statistics between two spatial datasets.
#
# @param old An sf polygon object representing the earlier period.
# @param new An sf polygon object representing the later period.
#
# @return A data frame containing pairwise overlap statistics.
#
calculate_overlap <- function(old, new) {

  empty_result <- function() {
    data.frame(
      old_id = integer(),
      new_id = integer(),
      intersection_area = numeric(),
      share_old = numeric(),
      share_new = numeric(),
      iou = numeric()
    )
  }
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }
  
  if (!inherits(old, "sf") || !inherits(new, "sf")) {
    stop("Both inputs must be sf objects.")
  }

  if (!isTRUE(sf::st_crs(old) == sf::st_crs(new))) {
    stop("Both inputs must use the same coordinate reference system.")
  }

  if (nrow(old) == 0L || nrow(new) == 0L) {
    return(empty_result())
  }

  if (any(sf::st_is_empty(old)) || any(sf::st_is_empty(new))) {
    stop("Inputs cannot contain empty geometries.")
  }

  old_valid <- sf::st_is_valid(old)
  new_valid <- sf::st_is_valid(new)

  if (any(is.na(old_valid) | !old_valid) ||
      any(is.na(new_valid) | !new_valid)) {
    stop("Inputs must contain valid geometries.")
  }

  old_area_values <- as.numeric(sf::st_area(old))
  new_area_values <- as.numeric(sf::st_area(new))

  if (any(!is.finite(old_area_values) | old_area_values <= 0) ||
      any(!is.finite(new_area_values) | new_area_values <= 0)) {
    stop("Input geometries must have positive, finite areas.")
  }
  
  # Create internal temporary IDs
  old$.spatpersist_old_id <- seq_len(nrow(old))
  new$.spatpersist_new_id <- seq_len(nrow(new))
  
  # Calculate intersections
  intersections <- suppressWarnings(
    sf::st_intersection(
      old[, ".spatpersist_old_id"],
      new[, ".spatpersist_new_id"]
    )
  )
  
  # No overlap case
  if (nrow(intersections) == 0) {
    return(empty_result())
  }
  
  # Calculate intersection area
  intersections$intersection_area <-
    as.numeric(sf::st_area(intersections))

  intersections <- intersections[
    is.finite(intersections$intersection_area) &
      intersections$intersection_area > 0,
  ]

  if (nrow(intersections) == 0L) {
    return(empty_result())
  }
  
  
  # Calculate original polygon areas
  
  old_area <- data.frame(
    old_id = seq_len(nrow(old)),
    old_area = old_area_values
  )
  
  new_area <- data.frame(
    new_id = seq_len(nrow(new)),
    new_area = new_area_values
  )
  
  
  # Convert output into regular data frame
  
  result <- sf::st_drop_geometry(intersections)
  
  names(result) <- c(
    "old_id",
    "new_id",
    "intersection_area"
  )
  
  
  # Add area shares
  
  result <- merge(
    result,
    old_area,
    by = "old_id",
    all.x = TRUE
  )
  
  result <- merge(
    result,
    new_area,
    by = "new_id",
    all.x = TRUE
  )
  
  
  result$share_old <-
    result$intersection_area / result$old_area
  
  result$share_new <-
    result$intersection_area / result$new_area

  # Intersection over Union (Jaccard similarity)

  result$iou <-
    result$intersection_area /
    (result$old_area + result$new_area - result$intersection_area)
  
  
  # Return only useful columns
  
  result[
    ,
    c(
      "old_id",
      "new_id",
      "intersection_area",
      "share_old",
      "share_new",
      "iou"
    )
  ]
}

# Calculate spatial overlap between two polygon datasets
#
# Internal helper function used by create_spatial_ids().
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
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }
  
  if (!inherits(old, "sf") || !inherits(new, "sf")) {
    stop("Both inputs must be sf objects.")
  }
  
  # Create internal temporary IDs
  old$.spatialid_old_id <- seq_len(nrow(old))
  new$.spatialid_new_id <- seq_len(nrow(new))
  
  # Calculate intersections
  intersections <- suppressWarnings(
    sf::st_intersection(
      old[, ".spatialid_old_id"],
      new[, ".spatialid_new_id"]
    )
  )
  
  # No overlap case
  if (nrow(intersections) == 0) {
    
    return(
      data.frame(
        old_id = integer(),
        new_id = integer(),
        intersection_area = numeric(),
        share_old = numeric(),
        share_new = numeric()
      )
    )
  }
  
  # Calculate intersection area
  intersections$intersection_area <-
    as.numeric(sf::st_area(intersections))
  
  
  # Calculate original polygon areas
  
  old_area <- data.frame(
    old_id = seq_len(nrow(old)),
    old_area = as.numeric(sf::st_area(old))
  )
  
  new_area <- data.frame(
    new_id = seq_len(nrow(new)),
    new_area = as.numeric(sf::st_area(new))
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
  
  
  # Return only useful columns
  
  result[
    ,
    c(
      "old_id",
      "new_id",
      "intersection_area",
      "share_old",
      "share_new"
    )
  ]
}
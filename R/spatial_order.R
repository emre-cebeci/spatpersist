# Order rows using geometry only
#
# Internal helper used to make ID allocation and spatial tie-breaking
# reproducible when input rows are reordered.
#
order_spatial_rows <- function(data, rows) {

  geometry <- sf::st_geometry(data[rows, ])
  bounding_boxes <- t(
    vapply(
      geometry,
      function(feature) as.numeric(sf::st_bbox(feature)),
      numeric(4)
    )
  )
  areas <- as.numeric(sf::st_area(geometry))

  # Bounding boxes and areas distinguish nearly all ordinary polygon rows.
  # Serializing every geometry to WKT is substantially more expensive, so use
  # it only for the uncommon rows whose cheaper ordering keys are identical.
  ordering_keys <- data.frame(
    xmin = bounding_boxes[, 1L],
    ymin = bounding_boxes[, 2L],
    xmax = bounding_boxes[, 3L],
    ymax = bounding_boxes[, 4L],
    area = areas
  )
  tied <- duplicated(ordering_keys) |
    duplicated(ordering_keys, fromLast = TRUE)
  geometry_text <- rep("", length(geometry))
  if (any(tied)) {
    geometry_text[tied] <- sf::st_as_text(geometry[tied])
  }

  rows[
    order(
      bounding_boxes[, 1L],
      bounding_boxes[, 2L],
      bounding_boxes[, 3L],
      bounding_boxes[, 4L],
      areas,
      geometry_text
    )
  ]
}

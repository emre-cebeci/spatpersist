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
  geometry_text <- sf::st_as_text(geometry)

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

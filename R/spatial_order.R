# Order rows using geometry only
#
# Internal helper used to make ID allocation and spatial tie-breaking
# reproducible when input rows are reordered.
#
check_coextensive_rows <- function(data, time, argument = "`data`") {

  time_values <- data[[time]]
  first_in_group <- !duplicated(time_values)

  for (first_row_in_group in which(first_in_group)) {
    time_value <- time_values[first_row_in_group]
    rows <- which(time_values == time_value)
    if (length(rows) < 2L) {
      next
    }

    geometry <- sf::st_geometry(data[rows, ])
    bounding_boxes <- t(
      vapply(
        geometry,
        function(feature) as.numeric(sf::st_bbox(feature)),
        numeric(4)
      )
    )
    precision <- sf::st_precision(geometry)
    if (is.finite(precision) && precision > 0) {
      bounding_boxes <- round(bounding_boxes * precision) / precision
    }
    candidate_keys <- data.frame(
      xmin = bounding_boxes[, 1L],
      ymin = bounding_boxes[, 2L],
      xmax = bounding_boxes[, 3L],
      ymax = bounding_boxes[, 4L]
    )
    tied <- duplicated(candidate_keys) |
      duplicated(candidate_keys, fromLast = TRUE)
    if (!any(tied)) {
      next
    }

    candidate_rows <- rows[tied]
    equal_geometries <- sf::st_equals(geometry[tied], sparse = TRUE)

    for (position in seq_along(equal_geometries)) {
      later_matches <- equal_geometries[[position]][
        equal_geometries[[position]] > position
      ]
      if (length(later_matches) == 0L) {
        next
      }

      first_row <- candidate_rows[[position]]
      second_row <- candidate_rows[[later_matches[[1L]]]]
      stop(
        "Exact same-period coextensive geometries are unsupported: ",
        argument,
        " rows ",
        first_row,
        " and ",
        second_row,
        " are spatially identical at time `",
        format(time_value, digits = 17L),
        "`. Geometry-only matching cannot distinguish their identities ",
        "reproducibly.",
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}


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

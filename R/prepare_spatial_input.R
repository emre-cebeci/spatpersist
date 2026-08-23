# Validate and prepare polygon input
#
# Internal helper used by persist_ids().
#
prepare_spatial_input <- function(data, geometry_action, geometry_precision) {

  format_rows <- function(rows) {
    displayed <- paste(utils::head(rows, 10L), collapse = ", ")
    if (length(rows) > 10L) {
      paste0(displayed, ", ...")
    } else {
      displayed
    }
  }

  if (!is.null(geometry_precision)) {
    data <- sf::st_set_precision(data, geometry_precision)
  }

  geometry_types <- as.character(sf::st_geometry_type(data))
  invalid_type_rows <- which(
    !(geometry_types %in% c("POLYGON", "MULTIPOLYGON"))
  )
  if (length(invalid_type_rows) > 0L) {
    stop(
      "`data` must contain only POLYGON or MULTIPOLYGON geometries; ",
      "other geometry types occur in rows: ",
      format_rows(invalid_type_rows),
      "."
    )
  }

  empty_rows <- which(sf::st_is_empty(data))
  if (length(empty_rows) > 0L) {
    stop(
      "Empty geometries occur in rows: ",
      format_rows(empty_rows),
      "."
    )
  }

  validity <- sf::st_is_valid(data)
  invalid_rows <- which(is.na(validity) | !validity)

  if (length(invalid_rows) > 0L && geometry_action == "error") {
    stop(
      "Invalid geometries occur in rows: ",
      format_rows(invalid_rows),
      ". Use `geometry_action = \"repair\"` to attempt repair."
    )
  }

  if (length(invalid_rows) > 0L) {
    data <- sf::st_make_valid(data)

    repaired_types <- as.character(sf::st_geometry_type(data))
    non_polygon_rows <- which(
      !(repaired_types %in% c("POLYGON", "MULTIPOLYGON"))
    )
    if (length(non_polygon_rows) > 0L) {
      stop(
        "Geometry repair did not produce polygon geometries in rows: ",
        format_rows(non_polygon_rows),
        "."
      )
    }

    repaired_validity <- sf::st_is_valid(data)
    unrepaired_rows <- which(is.na(repaired_validity) | !repaired_validity)
    if (length(unrepaired_rows) > 0L) {
      stop(
        "Geometry repair failed in rows: ",
        format_rows(unrepaired_rows),
        "."
      )
    }
  }

  areas <- as.numeric(sf::st_area(data))
  invalid_area_rows <- which(!is.finite(areas) | areas <= 0)
  if (length(invalid_area_rows) > 0L) {
    stop(
      "Geometries must have positive, finite areas; invalid areas occur in rows: ",
      format_rows(invalid_area_rows),
      "."
    )
  }

  data
}


# Measure the gap between two ordered time values
#
# Internal helper used by persist_ids().
#
spatial_time_gap <- function(previous_time, current_time) {

  if (inherits(previous_time, "POSIXt")) {
    return(
      as.numeric(
        difftime(current_time, previous_time, units = "secs")
      )
    )
  }

  as.numeric(current_time - previous_time)
}

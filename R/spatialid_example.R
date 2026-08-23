#' Create an example longitudinal spatial dataset
#'
#' Generates a small synthetic spatial dataset for testing
#' spatialid functions. The example contains spatial units
#' observed across multiple years.
#'
#' @return An sf object containing synthetic polygons.
#' @export
#'
#' @examples
#' example_data <- spatialid_example()
#' example_data

spatialid_example <- function() {
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Please install it first.")
  }
  
  polygons <- sf::st_sfc(
    sf::st_polygon(list(
      matrix(
        c(
          0,0,
          1,0,
          1,1,
          0,1,
          0,0
        ),
        ncol = 2,
        byrow = TRUE
      )
    )),
    
    sf::st_polygon(list(
      matrix(
        c(
          1,0,
          2,0,
          2,1,
          1,1,
          1,0
        ),
        ncol = 2,
        byrow = TRUE
      )
    )),
    
    sf::st_polygon(list(
      matrix(
        c(
          0,0,
          1.2,0,
          1.2,1,
          0,1,
          0,0
        ),
        ncol = 2,
        byrow = TRUE
      )
    ))
  )
  
  sf::st_sf(
    year = c(2000, 2000, 2001),
    name = c("Alpha", "Beta", "Alpha"),
    geometry = polygons
  )
}
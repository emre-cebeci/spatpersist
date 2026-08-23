#' Create an example longitudinal spatial dataset
#'
#' Generates synthetic polygon data showing common spatial
#' continuity problems: stable units, boundary changes,
#' renames, and splits.
#'
#' @return An sf object containing synthetic polygons.
#'
#' @export
#'
#' @examples
#' data <- spatialid_example()
#' plot(data["name"])

spatialid_example <- function() {
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }
  
  make_poly <- function(coords) {
    sf::st_polygon(
      list(
        matrix(
          coords,
          ncol = 2,
          byrow = TRUE
        )
      )
    )
  }
  
  
  polygons <- sf::st_sfc(
    
    # -----------------------------
    # 2000 stable unit: Alpha
    # -----------------------------
    make_poly(c(
      0,0,
      1,0,
      1,1,
      0,1,
      0,0
    )),
    
    # -----------------------------
    # 2001 stable unit: Alpha
    # same geometry
    # -----------------------------
    make_poly(c(
      0,0,
      1,0,
      1,1,
      0,1,
      0,0
    )),
    
    # -----------------------------
    # 2000 boundary-change unit: Beta
    # -----------------------------
    make_poly(c(
      2,0,
      3,0,
      3,1,
      2,1,
      2,0
    )),
    
    # -----------------------------
    # 2001 boundary-change unit: Beta
    # slightly expanded
    # -----------------------------
    make_poly(c(
      2,0,
      3.2,0,
      3.2,1,
      2,1,
      2,0
    )),
    
    # -----------------------------
    # Rename example:
    # 2000 Gamma
    # -----------------------------
    make_poly(c(
      0,2,
      1,2,
      1,3,
      0,3,
      0,2
    )),
    
    # -----------------------------
    # 2001 Delta
    # same polygon, renamed
    # -----------------------------
    make_poly(c(
      0,2,
      1,2,
      1,3,
      0,3,
      0,2
    )),
    
    # -----------------------------
    # 2000 split ancestor: Omega
    # -----------------------------
    make_poly(c(
      2,2,
      4,2,
      4,3,
      2,3,
      2,2
    )),
    
    # -----------------------------
    # 2001 split child: Omega East
    # -----------------------------
    make_poly(c(
      3,2,
      4,2,
      4,3,
      3,3,
      3,2
    )),
    
    # -----------------------------
    # 2001 split child: Omega West
    # -----------------------------
    make_poly(c(
      2,2,
      3,2,
      3,3,
      2,3,
      2,2
    ))
  )
  
  
  sf::st_sf(
    
    year = c(
      2000,
      2001,
      
      2000,
      2001,
      
      2000,
      2001,
      
      2000,
      2001,
      2001
    ),
    
    name = c(
      "Alpha",
      "Alpha",
      
      "Beta",
      "Beta",
      
      "Gamma",
      "Delta",
      
      "Omega",
      "Omega East",
      "Omega West"
    ),
    
    geometry = polygons
  )
}
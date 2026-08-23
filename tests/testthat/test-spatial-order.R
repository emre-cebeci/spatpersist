test_that("geometry text breaks otherwise identical spatial ordering keys", {
  lower_triangle <- sf::st_polygon(
    list(rbind(c(0, 0), c(2, 0), c(0, 2), c(0, 0)))
  )
  upper_triangle <- sf::st_polygon(
    list(rbind(c(0, 0), c(2, 2), c(0, 2), c(0, 0)))
  )
  data <- sf::st_sf(
    label = c("lower", "upper"),
    geometry = sf::st_sfc(lower_triangle, upper_triangle)
  )
  reversed <- data[2:1, ]

  original_order <- spatialid:::order_spatial_rows(data, 1:2)
  reversed_order <- spatialid:::order_spatial_rows(reversed, 1:2)

  expect_equal(
    data$label[original_order],
    reversed$label[reversed_order]
  )
})

test_that("same-period coextensive geometries are rejected", {
  polygon <- sf::st_polygon(
    list(rbind(
      c(0, 0),
      c(1, 0),
      c(1, 1),
      c(0, 1),
      c(0, 0)
    ))
  )
  differently_encoded_polygon <- sf::st_polygon(
    list(rbind(
      c(1, 1),
      c(1, 0),
      c(0, 0),
      c(0, 1),
      c(1, 1)
    ))
  )
  data <- sf::st_sf(
    year = c(2000, 2000),
    label = c("first", "second"),
    geometry = sf::st_sfc(polygon, differently_encoded_polygon)
  )

  expect_error(
    create_spatial_ids(data, time = "year"),
    paste0(
      "Exact same-period coextensive geometries are unsupported: ",
      "`data` rows 1 and 2.*time `2000`"
    )
  )
})

test_that("coextensive geometries in different periods may continue", {
  polygon <- sf::st_polygon(
    list(rbind(
      c(0, 0),
      c(1, 0),
      c(1, 1),
      c(0, 1),
      c(0, 0)
    ))
  )
  data <- sf::st_sf(
    year = c(2000, 2001),
    geometry = sf::st_sfc(polygon, polygon)
  )

  result <- create_spatial_ids(data, time = "year")

  expect_equal(length(unique(result$spatial_id)), 1L)
  expect_equal(result$spatial_version_id, rep("SID000001-V001", 2L))
})

test_that("geometry precision cannot create same-period duplicates", {
  make_polygon <- function(offset) {
    sf::st_polygon(
      list(rbind(
        c(0 + offset, 0),
        c(1 + offset, 0),
        c(1 + offset, 1),
        c(0 + offset, 1),
        c(0 + offset, 0)
      ))
    )
  }
  data <- sf::st_sf(
    year = c(2000, 2000),
    geometry = sf::st_sfc(make_polygon(0), make_polygon(0.01))
  )

  expect_error(
    create_spatial_ids(data, time = "year", geometry_precision = 1),
    "Exact same-period coextensive geometries are unsupported"
  )
})

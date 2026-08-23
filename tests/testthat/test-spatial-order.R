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

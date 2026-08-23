test_that("calculate_overlap returns expected statistics for example data", {
  data <- spatialid_example()
  old <- data[data$year == 2000, ]
  new <- data[data$year == 2001, ]

  result <- spatialid:::calculate_overlap(old, new)

  expect_named(
    result,
    c(
      "old_id",
      "new_id",
      "intersection_area",
      "share_old",
      "share_new",
      "iou"
    )
  )
  expect_equal(nrow(result), 5L)

  alpha <- result[result$old_id == 1L & result$new_id == 1L, ]
  expect_equal(alpha$share_old, 1)
  expect_equal(alpha$share_new, 1)
  expect_equal(alpha$iou, 1)

  beta <- result[result$old_id == 2L & result$new_id == 2L, ]
  expect_equal(beta$share_old, 1)
  expect_equal(beta$share_new, 1 / 1.2)
  expect_equal(beta$iou, 1 / 1.2)

  renamed <- result[result$old_id == 3L & result$new_id == 3L, ]
  expect_equal(renamed$iou, 1)
})

test_that("calculate_overlap describes both branches of a split", {
  data <- spatialid_example()
  old <- data[data$year == 2000, ]
  new <- data[data$year == 2001, ]

  result <- spatialid:::calculate_overlap(old, new)
  split <- result[result$old_id == 4L, ]

  expect_equal(nrow(split), 2L)
  expect_equal(sort(split$new_id), c(4L, 5L))
  expect_equal(split$share_old, c(0.5, 0.5))
  expect_equal(split$share_new, c(1, 1))
  expect_equal(split$iou, c(0.5, 0.5))
})

test_that("calculate_overlap returns a typed empty result for disjoint data", {
  data <- spatialid_example()
  old <- data[data$year == 2000, ]
  new <- data[data$year == 2001, ]
  sf::st_geometry(new) <- sf::st_geometry(new) + c(100, 100)

  result <- spatialid:::calculate_overlap(old, new)

  expect_equal(nrow(result), 0L)
  expect_named(
    result,
    c(
      "old_id",
      "new_id",
      "intersection_area",
      "share_old",
      "share_new",
      "iou"
    )
  )
  expect_type(result$iou, "double")
})

test_that("calculate_overlap rejects non-sf inputs", {
  data <- spatialid_example()

  expect_error(
    spatialid:::calculate_overlap(data.frame(), data),
    "Both inputs must be sf objects",
    fixed = TRUE
  )
})

test_that("calculate_overlap rejects mismatched coordinate systems", {
  data <- spatialid_example()
  old <- sf::st_set_crs(data[data$year == 2000, ], 4326)
  new <- sf::st_set_crs(data[data$year == 2001, ], 3857)

  expect_error(
    spatialid:::calculate_overlap(old, new),
    "same coordinate reference system",
    fixed = TRUE
  )
})

test_that("calculate_overlap omits boundary-only intersections", {
  old <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
    )
  )
  new <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
    )
  )

  result <- spatialid:::calculate_overlap(old, new)

  expect_equal(nrow(result), 0L)
})

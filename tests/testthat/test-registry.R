test_that("a registry preserves issued IDs when earlier-sorting units are added", {
  data <- example_units()
  registry <- persist_ids(data, time = "year")

  make_polygon <- function(xmin, xmax) {
    sf::st_polygon(
      list(rbind(
        c(xmin, 0),
        c(xmax, 0),
        c(xmax, 1),
        c(xmin, 1),
        c(xmin, 0)
      ))
    )
  }

  added <- sf::st_sf(
    year = c(2000, 2001),
    name = c("New unit", "New unit"),
    geometry = sf::st_sfc(
      make_polygon(-2, -1),
      make_polygon(-2, -1)
    )
  )
  expanded <- rbind(data, added)

  without_registry <- persist_ids(expanded, time = "year")
  with_registry <- persist_ids(
    expanded,
    time = "year",
    registry = registry
  )

  expect_false(
    identical(
      without_registry$spatial_id[seq_len(nrow(data))],
      registry$spatial_id
    )
  )
  expect_equal(
    with_registry$spatial_id[seq_len(nrow(data))],
    registry$spatial_id
  )
  expect_equal(
    with_registry$spatial_version_id[seq_len(nrow(data))],
    registry$spatial_version_id
  )
  expect_equal(
    with_registry$lineage_id[seq_len(nrow(data))],
    registry$lineage_id
  )
  expect_true(all(with_registry$registry_matched[seq_len(nrow(data))]))
  expect_false(any(with_registry$registry_matched[(nrow(data) + 1L):nrow(expanded)]))
  expect_equal(
    with_registry$spatial_id[(nrow(data) + 1L):nrow(expanded)],
    rep("SID000007", 2)
  )
  expect_equal(nrow(validate_ids(with_registry, time = "year")), 0L)
})

test_that("a registry anchors versions while later observations are appended", {
  data <- example_units()
  registry <- persist_ids(data, time = "year")
  later_beta <- data[4L, ]
  later_beta$year <- 2002
  later_beta$name <- "Beta later"
  extended <- rbind(data, later_beta)

  result <- persist_ids(
    extended,
    time = "year",
    registry = registry
  )

  expect_equal(result$spatial_id[[10L]], registry$spatial_id[[4L]])
  expect_equal(
    result$spatial_version_id[[10L]],
    registry$spatial_version_id[[4L]]
  )
  expect_false(result$registry_matched[[10L]])
  expect_equal(nrow(validate_ids(result, time = "year")), 0L)
})

test_that("new evidence consolidates registry lineages deterministically", {
  make_polygon <- function(xmin, xmax) {
    sf::st_polygon(
      list(rbind(
        c(xmin, 0),
        c(xmax, 0),
        c(xmax, 1),
        c(xmin, 1),
        c(xmin, 0)
      ))
    )
  }
  initial <- sf::st_sf(
    year = c(2000, 2000),
    geometry = sf::st_sfc(
      make_polygon(0, 1),
      make_polygon(1, 2)
    )
  )
  registry <- persist_ids(initial, time = "year")
  bridge <- sf::st_sf(
    year = 2001,
    geometry = sf::st_sfc(make_polygon(0, 2))
  )
  expanded <- rbind(initial, bridge)

  result <- persist_ids(
    expanded,
    time = "year",
    registry = registry
  )
  expected_lineage <- min(registry$lineage_id)

  expect_equal(result$spatial_id[1:2], registry$spatial_id)
  expect_equal(result$spatial_version_id[1:2], registry$spatial_version_id)
  expect_equal(result$lineage_id, rep(expected_lineage, 3L))
  expect_true(all(result$registry_matched[1:2]))
  expect_false(result$registry_matched[[3L]])
  expect_equal(nrow(validate_ids(result, time = "year")), 0L)
})

test_that("conflicting registry identities stop reconciliation", {
  data <- example_units()
  registry <- persist_ids(data, time = "year")
  registry$spatial_id[[2L]] <- "SID999999"
  registry$spatial_version_id[[2L]] <- "SID999999-V001"

  expect_error(
    persist_ids(data, time = "year", registry = registry),
    "maps to multiple existing spatial IDs"
  )
})

test_that("registry CRS and structure are checked", {
  data <- example_units()
  registry <- persist_ids(data, time = "year")
  mismatched_crs <- sf::st_set_crs(registry, 4326)
  incomplete <- registry
  incomplete$spatial_version_id <- NULL

  expect_error(
    persist_ids(data, time = "year", registry = mismatched_crs),
    "same coordinate reference system"
  )
  expect_error(
    persist_ids(data, time = "year", registry = incomplete),
    "missing required columns"
  )

  duplicate_registry <- registry
  sf::st_geometry(duplicate_registry)[[3L]] <-
    sf::st_geometry(duplicate_registry)[[1L]]
  expect_error(
    persist_ids(data, time = "year", registry = duplicate_registry),
    paste0(
      "Exact same-period coextensive geometries are unsupported: ",
      "`registry`"
    )
  )
})

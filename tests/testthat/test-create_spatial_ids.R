test_that("create_spatial_ids preserves stable, changed, and renamed units", {
  data <- spatialid_example()

  result <- create_spatial_ids(data, time = "year", threshold = 0.75)

  expect_s3_class(result, "sf")
  expect_equal(result$spatial_id[c(1, 2)], rep("SID000001", 2))
  expect_equal(result$spatial_id[c(3, 4)], rep("SID000003", 2))
  expect_equal(result$spatial_id[c(5, 6)], rep("SID000002", 2))
  expect_equal(
    result$spatial_version_id[c(1, 2)],
    rep("SID000001-V001", 2)
  )
  expect_equal(
    result$spatial_version_id[c(3, 4)],
    c("SID000003-V001", "SID000003-V002")
  )
  expect_equal(
    result$spatial_version_id[c(5, 6)],
    rep("SID000002-V001", 2)
  )
})

test_that("create_spatial_ids assigns new IDs to unmatched split children", {
  data <- spatialid_example()

  result <- create_spatial_ids(data, time = "year", threshold = 0.75)

  expect_equal(result$spatial_id[[7]], "SID000004")
  expect_equal(result$spatial_id[c(8, 9)], c("SID000006", "SID000005"))
  expect_equal(
    result$spatial_version_id[c(8, 9)],
    c("SID000006-V001", "SID000005-V001")
  )
  expect_equal(result$transition_type[c(8, 9)], c("split", "split"))
  expect_equal(result$parent_id[c(8, 9)], rep("SID000004", 2))
  expect_equal(result$lineage_id[c(7, 8, 9)], rep("LID000004", 3))
  expect_equal(anyDuplicated(result$spatial_id[result$year == 2001]), 0L)
})

test_that("create_spatial_ids supports alternative overlap metrics", {
  data <- spatialid_example()

  strict_iou <- create_spatial_ids(
    data,
    time = "year",
    threshold = 0.9,
    metric = "iou"
  )
  split_by_new_share <- create_spatial_ids(
    data,
    time = "year",
    threshold = 0.75,
    metric = "share_new"
  )

  expect_false(strict_iou$spatial_id[[3]] == strict_iou$spatial_id[[4]])
  child_ids <- split_by_new_share$spatial_id[c(8, 9)]
  expect_equal(sum(child_ids == split_by_new_share$spatial_id[[7]]), 1L)
  expect_equal(anyDuplicated(child_ids), 0L)
})

test_that("create_spatial_ids validates its public arguments", {
  data <- spatialid_example()

  expect_error(create_spatial_ids(data.frame(), "year"), "must be an sf object")
  expect_error(create_spatial_ids(data, "missing"), "must name a column")
  expect_error(create_spatial_ids(data, "year", -0.1), "from 0 to 1")
  expect_error(create_spatial_ids(data, "year", metric = "dice"), "arg")
  expect_error(
    create_spatial_ids(data, "year", event_threshold = 2),
    "`event_threshold`"
  )
  expect_error(
    create_spatial_ids(data, "year", lineage_threshold = NA_real_),
    "`lineage_threshold`"
  )
  expect_error(
    create_spatial_ids(data, "year", version_threshold = 1.1),
    "`version_threshold`"
  )
  expect_error(
    create_spatial_ids(data, "year", geometry_precision = 0),
    "`geometry_precision`"
  )
  expect_error(
    create_spatial_ids(data, "year", max_time_gap = 0),
    "`max_time_gap`"
  )
  expect_error(
    create_spatial_ids(data, "year", ambiguity_tolerance = -0.1),
    "`ambiguity_tolerance`"
  )
  expect_error(
    create_spatial_ids(data, "year", match_rule = "optimal"),
    "arg"
  )
  expect_error(
    create_spatial_ids(data, "year", registry_threshold = 2),
    "`registry_threshold`"
  )
})

test_that("create_spatial_ids detects mergers and preserves their lineage", {
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

  data <- sf::st_sf(
    year = c(2000, 2000, 2001),
    name = c("West", "East", "Combined"),
    geometry = sf::st_sfc(
      make_polygon(0, 1),
      make_polygon(1, 2),
      make_polygon(0, 2)
    )
  )

  result <- create_spatial_ids(data, time = "year")

  expect_equal(result$transition_type[[3]], "merger")
  expect_equal(result$spatial_id[[3]], "SID000001")
  expect_equal(result$spatial_version_id[[3]], "SID000001-V002")
  expect_true(is.na(result$parent_id[[3]]))
  expect_equal(result$lineage_id, rep("LID000001", 3))
})

test_that("lineages persist across a split and later continuations", {
  example <- spatialid_example()
  split_children <- example[example$year == 2001 & grepl("Omega", example$name), ]
  later_children <- split_children
  later_children$year <- 2002
  later_children$name <- c("Renamed East", "Renamed West")
  data <- rbind(example, later_children)

  result <- create_spatial_ids(data, time = "year")
  omega <- grepl("Omega|Renamed", result$name)

  expect_equal(length(unique(result$lineage_id[omega])), 1L)
  expect_equal(result$transition_type[result$year == 2002], rep("continuation", 2))
  expect_equal(
    result$spatial_id[result$year == 2001 & grepl("Omega", result$name)],
    result$spatial_id[result$year == 2002]
  )
})

test_that("related replacements receive a new identity in the same lineage", {
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

  data <- sf::st_sf(
    year = c(2000, 2001),
    geometry = sf::st_sfc(
      make_polygon(0, 1),
      make_polygon(0.4, 1.4)
    )
  )

  result <- create_spatial_ids(data, time = "year")

  expect_equal(result$spatial_id, c("SID000001", "SID000002"))
  expect_equal(result$lineage_id, rep("LID000001", 2))
  expect_equal(result$parent_id, c(NA_character_, "SID000001"))
  expect_equal(result$transition_type, c("initial", "replacement"))
})

test_that("geometry-based IDs are invariant to input row order", {
  data <- spatialid_example()
  data$source_row <- seq_len(nrow(data))
  shuffled <- data[c(9, 2, 7, 4, 1, 8, 3, 6, 5), ]

  original_result <- create_spatial_ids(data, time = "year")
  shuffled_result <- create_spatial_ids(shuffled, time = "year")
  shuffled_result <- shuffled_result[order(shuffled_result$source_row), ]

  output_columns <- c(
    "spatial_id",
    "spatial_version_id",
    "lineage_id",
    "parent_id",
    "transition_type",
    "match_score",
    "match_confidence",
    "match_ambiguous",
    "registry_matched"
  )
  expect_equal(
    sf::st_drop_geometry(original_result)[, output_columns],
    sf::st_drop_geometry(shuffled_result)[, output_columns],
    ignore_attr = TRUE
  )
})

test_that("transition diagnostics expose candidates and selected links", {
  result <- create_spatial_ids(spatialid_example(), time = "year")

  diagnostics <- spatialid_transitions(result)
  split_diagnostics <- diagnostics[diagnostics$transition_type == "split", ]

  expect_equal(nrow(diagnostics), 5L)
  expect_equal(sum(diagnostics$selected), 3L)
  expect_equal(unique(diagnostics$match_metric), "share_old")
  expect_true(
    all(c("old_spatial_version_id", "new_spatial_version_id") %in%
      names(diagnostics))
  )
  expect_true(
    all(c("eligible", "mutual_best", "ambiguous", "match_confidence") %in%
      names(diagnostics))
  )
  expect_true("registry_matched" %in% names(diagnostics))
  expect_equal(nrow(split_diagnostics), 2L)
  expect_false(any(split_diagnostics$candidate))
  expect_false(any(split_diagnostics$selected))
  expect_true(all(split_diagnostics$same_lineage))
  expect_equal(unique(split_diagnostics$parent_id), "SID000004")
  expect_error(
    spatialid_transitions(spatialid_example()),
    "does not contain"
  )
})

test_that("validate_spatial_ids reports structural problems", {
  result <- create_spatial_ids(spatialid_example(), time = "year")
  expect_equal(nrow(validate_spatial_ids(result, time = "year")), 0L)

  corrupted <- result
  corrupted$spatial_id[[4]] <- corrupted$spatial_id[[2]]
  corrupted$lineage_id[[2]] <- "LID999999"
  corrupted$parent_id[[8]] <- "SID999999"
  corrupted$spatial_version_id[[6]] <- "SID000002-V009"
  corrupted$match_score[[2]] <- 1.5
  corrupted$match_confidence[[6]] <- NA_real_

  issues <- validate_spatial_ids(corrupted, time = "year")

  expect_true("duplicate_identity_within_time" %in% issues$check)
  expect_true("identity_multiple_lineages" %in% issues$check)
  expect_true("missing_parent" %in% issues$check)
  expect_true("invalid_version_sequence" %in% issues$check)
  expect_true("invalid_match_value" %in% issues$check)
  expect_true("missing_match_diagnostics" %in% issues$check)
})

test_that("version thresholds can ignore small accepted boundary changes", {
  data <- spatialid_example()

  exact_versions <- create_spatial_ids(data, time = "year")
  tolerant_versions <- create_spatial_ids(
    data,
    time = "year",
    version_threshold = 0.8
  )

  expect_false(
    exact_versions$spatial_version_id[[3]] ==
      exact_versions$spatial_version_id[[4]]
  )
  expect_equal(
    tolerant_versions$spatial_version_id[c(3, 4)],
    rep("SID000003-V001", 2)
  )
})

test_that("geometry versions represent consecutive boundary spells", {
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

  data <- sf::st_sf(
    year = 2000:2003,
    geometry = sf::st_sfc(
      make_polygon(0, 1),
      make_polygon(0, 1.2),
      make_polygon(0, 1.2),
      make_polygon(0, 1)
    )
  )

  result <- create_spatial_ids(data, time = "year")

  expect_equal(result$spatial_id, rep("SID000001", 4))
  expect_equal(
    result$spatial_version_id,
    c(
      "SID000001-V001",
      "SID000001-V002",
      "SID000001-V002",
      "SID000001-V003"
    )
  )
  expect_equal(nrow(validate_spatial_ids(result, time = "year")), 0L)
})

test_that("invalid polygons can be rejected or explicitly repaired", {
  invalid_polygon <- sf::st_polygon(
    list(rbind(
      c(0, 0),
      c(1, 1),
      c(0, 1),
      c(1, 0),
      c(0, 0)
    ))
  )
  data <- sf::st_sf(
    year = 2000,
    geometry = sf::st_sfc(invalid_polygon)
  )

  expect_error(
    create_spatial_ids(data, time = "year"),
    "Invalid geometries occur"
  )

  repaired <- create_spatial_ids(
    data,
    time = "year",
    geometry_action = "repair"
  )

  expect_true(all(sf::st_is_valid(repaired)))
  expect_true(
    all(as.character(sf::st_geometry_type(repaired)) %in%
      c("POLYGON", "MULTIPOLYGON"))
  )
})

test_that("empty polygon geometries are rejected before matching", {
  data <- sf::st_sf(
    year = 2000,
    geometry = sf::st_sfc(sf::st_polygon())
  )

  expect_error(
    create_spatial_ids(data, time = "year"),
    "Empty geometries occur"
  )
})

test_that("geometry precision is retained in the result", {
  result <- create_spatial_ids(
    spatialid_example(),
    time = "year",
    geometry_precision = 100
  )

  expect_equal(sf::st_precision(result), 100)
})

test_that("maximum time gaps can break otherwise valid continuations", {
  data <- spatialid_example()[c(1, 2), ]
  data$year[[2]] <- 2002

  connected <- create_spatial_ids(data, time = "year")
  separated <- create_spatial_ids(data, time = "year", max_time_gap = 1)

  expect_equal(length(unique(connected$spatial_id)), 1L)
  expect_equal(length(unique(separated$spatial_id)), 2L)
  expect_equal(separated$transition_type, c("initial", "new"))
  expect_equal(nrow(spatialid_transitions(separated)), 0L)
})

test_that("finite maximum gaps require a measurable time column", {
  data <- spatialid_example()[c(1, 2), ]
  data$period <- c("early", "late")

  expect_error(
    create_spatial_ids(data, time = "period", max_time_gap = 1),
    "requires a numeric, Date, or POSIXt"
  )
})

test_that("ambiguous split continuations follow the requested policy", {
  data <- spatialid_example()

  flagged <- create_spatial_ids(
    data,
    time = "year",
    metric = "share_new",
    ambiguity_action = "flag"
  )
  rejected <- create_spatial_ids(
    data,
    time = "year",
    metric = "share_new",
    ambiguity_action = "new"
  )

  flagged_children <- c(8, 9)
  selected_child <- flagged_children[
    flagged$spatial_id[flagged_children] == flagged$spatial_id[[7]]
  ]

  expect_equal(length(selected_child), 1L)
  expect_true(flagged$match_ambiguous[selected_child])
  expect_equal(flagged$match_confidence[selected_child], 0)
  expect_false(any(rejected$spatial_id[flagged_children] == rejected$spatial_id[[7]]))
  expect_error(
    create_spatial_ids(
      data,
      time = "year",
      metric = "share_new",
      ambiguity_action = "error"
    ),
    "Ambiguous spatial matches detected"
  )
})

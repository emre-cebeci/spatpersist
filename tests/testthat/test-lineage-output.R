test_that("spatialid_lineages returns one auditable row per identity", {
  result <- create_spatial_ids(spatialid_example(), time = "year")

  lineages <- spatialid_lineages(result, time = "year")
  beta <- lineages[lineages$spatial_id == "SID000003", ]
  omega_family <- lineages[lineages$lineage_id == "LID000004", ]

  expect_equal(nrow(lineages), 6L)
  expect_equal(beta$n_observations, 2L)
  expect_equal(beta$n_versions, 2L)
  expect_equal(beta$first_time, 2000)
  expect_equal(beta$last_time, 2001)
  expect_equal(beta$min_match_confidence, 1)
  expect_equal(nrow(omega_family), 3L)
  expect_equal(
    sort(stats::na.omit(omega_family$parent_id)),
    rep("SID000004", 2)
  )
})

test_that("lineage diagnostics identify continuation and branch edges", {
  result <- create_spatial_ids(spatialid_example(), time = "year")
  diagnostics <- spatialid_transitions(result)

  expect_true(all(diagnostics$lineage_link))
  expect_equal(sum(diagnostics$lineage_link & diagnostics$selected), 3L)
  expect_equal(sum(diagnostics$lineage_link & !diagnostics$selected), 2L)
})

test_that("plot_spatial_lineage draws and returns filtered plot data", {
  result <- create_spatial_ids(spatialid_example(), time = "year")
  target <- tempfile(fileext = ".pdf")

  grDevices::pdf(target)
  plotted <- plot_spatial_lineage(
    result,
    time = "year",
    lineage_id = "LID000004",
    show_legend = FALSE,
    main = "Split lineage"
  )
  grDevices::dev.off()

  expect_true(file.exists(target))
  expect_gt(file.info(target)$size, 0)
  expect_equal(nrow(plotted$nodes), 3L)
  expect_equal(nrow(plotted$edges), 2L)
  expect_false(any(plotted$edges$selected))
  expect_equal(unique(plotted$nodes$lineage_id), "LID000004")
})

test_that("plot_spatial_lineage rejects unknown or stale lineage data", {
  result <- create_spatial_ids(spatialid_example(), time = "year")

  expect_error(
    plot_spatial_lineage(result, time = "year", lineage_id = "LID999999"),
    "Unknown lineage ID"
  )

  stale <- result
  stale$spatial_id[[1L]] <- "SID999999"
  expect_error(
    plot_spatial_lineage(stale, time = "year"),
    "no longer align"
  )
})

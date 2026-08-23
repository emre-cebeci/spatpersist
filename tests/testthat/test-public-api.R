test_that("the intentional public API remains stable", {
  expected_exports <- c(
    "create_spatial_ids",
    "plot_spatial_lineage",
    "spatialid_example",
    "spatialid_lineages",
    "spatialid_transitions",
    "validate_spatial_ids"
  )

  expect_setequal(getNamespaceExports("spatialid"), expected_exports)
})

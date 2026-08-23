test_that("the intentional public API remains stable", {
  expected_exports <- c(
    "persist_ids",
    "plot_lineage",
    "example_units",
    "id_lineages",
    "id_transitions",
    "validate_ids"
  )

  expect_setequal(getNamespaceExports("spatpersist"), expected_exports)
})

test_that("mutual-best matching is stricter than greedy matching", {
  overlap <- data.frame(
    old_id = c(1L, 1L, 2L, 2L),
    new_id = c(1L, 2L, 1L, 2L),
    intersection_area = c(90, 80, 85, 10),
    share_old = c(0.90, 0.80, 0.85, 0.10),
    share_new = c(0.90, 0.80, 0.85, 0.10),
    iou = c(0.90, 0.80, 0.85, 0.10)
  )

  greedy <- spatialid:::match_units(
    overlap,
    threshold = 0.05,
    metric = "iou",
    match_rule = "greedy",
    ambiguity_tolerance = 0.001,
    ambiguity_action = "flag"
  )
  mutual <- spatialid:::match_units(
    overlap,
    threshold = 0.05,
    metric = "iou",
    match_rule = "mutual_best",
    ambiguity_tolerance = 0.001,
    ambiguity_action = "flag"
  )

  expect_equal(nrow(greedy$selected), 2L)
  expect_equal(nrow(mutual$selected), 1L)
  expect_equal(mutual$selected$old_id, 1L)
  expect_equal(mutual$selected$new_id, 1L)
  expect_true(mutual$selected$mutual_best)
})

test_that("ambiguity policies flag, reject, or stop on near ties", {
  overlap <- data.frame(
    old_id = c(1L, 1L),
    new_id = c(1L, 2L),
    intersection_area = c(90, 88),
    share_old = c(0.90, 0.88),
    share_new = c(0.90, 0.88),
    iou = c(0.90, 0.88)
  )

  flagged <- spatialid:::match_units(
    overlap,
    threshold = 0.75,
    metric = "iou",
    match_rule = "greedy",
    ambiguity_tolerance = 0.05,
    ambiguity_action = "flag"
  )
  rejected <- spatialid:::match_units(
    overlap,
    threshold = 0.75,
    metric = "iou",
    match_rule = "greedy",
    ambiguity_tolerance = 0.05,
    ambiguity_action = "new"
  )

  expect_equal(nrow(flagged$selected), 1L)
  expect_true(flagged$selected$ambiguous)
  expect_equal(flagged$selected$confidence, 0.02, tolerance = 1e-10)
  expect_equal(nrow(rejected$selected), 0L)
  expect_false(any(rejected$candidates$candidate))
  expect_error(
    spatialid:::match_units(
      overlap,
      threshold = 0.75,
      metric = "iou",
      match_rule = "greedy",
      ambiguity_tolerance = 0.05,
      ambiguity_action = "error"
    ),
    "Ambiguous spatial matches detected"
  )
})

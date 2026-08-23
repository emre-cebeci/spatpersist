#!/usr/bin/env Rscript

# Reproducible scalability benchmark for spatpersist. Run from the package source
# directory after installing the current package with `R CMD INSTALL .`.

arguments <- commandArgs(trailingOnly = TRUE)

parse_positive_integer <- function(value, name) {
  parsed <- suppressWarnings(as.integer(value))
  if (length(parsed) != 1L || is.na(parsed) || parsed < 1L) {
    stop("`", name, "` must be a positive integer.")
  }
  parsed
}

if (!requireNamespace("spatpersist", quietly = TRUE)) {
  stop("Install the current spatpersist package before running this benchmark.")
}
if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Package 'sf' is required.")
}

sides <- if (length(arguments) >= 1L) {
  suppressWarnings(as.integer(strsplit(arguments[[1L]], ",", fixed = TRUE)[[1L]]))
} else {
  c(10L, 20L, 40L, 80L)
}
if (length(sides) == 0L || anyNA(sides) || any(sides < 1L)) {
  stop("The first argument must be comma-separated positive grid sizes.")
}

periods <- if (length(arguments) >= 2L) {
  parse_positive_integer(arguments[[2L]], "periods")
} else {
  4L
}
repetitions <- if (length(arguments) >= 3L) {
  parse_positive_integer(arguments[[3L]], "repetitions")
} else {
  3L
}

make_panel <- function(side, periods, shift = 0.05) {
  boundary <- sf::st_as_sfc(
    sf::st_bbox(
      c(xmin = 0, ymin = 0, xmax = side, ymax = side),
      crs = sf::st_crs(3857)
    )
  )
  base <- sf::st_make_grid(
    boundary,
    n = c(side, side),
    what = "polygons"
  )
  panels <- lapply(seq_len(periods), function(period) {
    geometry <- base + c((period - 1L) * shift, 0)
    sf::st_sf(
      year = period,
      source_id = seq_along(geometry),
      geometry = geometry
    )
  })
  do.call(rbind, panels)
}

benchmark_case <- function(side) {
  input <- make_panel(side, periods)
  elapsed <- numeric(repetitions)
  result <- NULL

  for (repetition in seq_len(repetitions)) {
    gc()
    elapsed[[repetition]] <- system.time({
      result <- spatpersist::persist_ids(
        input,
        time = "year",
        ambiguity_tolerance = 0
      )
    })[["elapsed"]]
  }

  issues <- spatpersist::validate_ids(result, time = "year")
  data.frame(
    side = side,
    units_per_period = side^2,
    periods = periods,
    rows = nrow(input),
    transition_edges = nrow(spatpersist::id_transitions(result)),
    median_seconds = stats::median(elapsed),
    minimum_seconds = min(elapsed),
    result_mb = as.numeric(object.size(result)) / 1024^2,
    identities = length(unique(result$spatial_id)),
    valid = nrow(issues) == 0L
  )
}

cat("R", as.character(getRversion()), "\n")
cat("spatpersist", as.character(utils::packageVersion("spatpersist")), "\n")
cat("sf", as.character(utils::packageVersion("sf")), "\n")
cat("GEOS", unname(sf::sf_extSoftVersion()[["GEOS"]]), "\n")
cat("platform", R.version$platform, "\n")
cat("system", Sys.info()[["sysname"]], Sys.info()[["release"]], "\n")
cat("machine", Sys.info()[["machine"]], "\n")
cat("repetitions", repetitions, "\n\n")

results <- do.call(rbind, lapply(sides, benchmark_case))
print(results, row.names = FALSE, digits = 4)

if (!all(results$valid)) {
  stop("At least one benchmark result failed package validation.")
}

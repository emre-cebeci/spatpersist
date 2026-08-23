#' Plot spatial identity lineages over time
#'
#' Draws observations on a time-by-identity layout. Solid links are selected
#' identity continuations; dashed links are additional spatial lineage
#' relationships such as split and merger branches.
#'
#' @param data A result returned by [persist_ids()].
#' @param time A single character string naming the time column in `data`.
#' @param lineage_id Optional character vector of lineage IDs to display.
#'   `NULL` displays every lineage.
#' @param show_legend Logical; display a transition-type legend.
#' @param node_cex Numeric node size.
#' @param ... Additional arguments passed to [graphics::plot.default()].
#'
#' @return Invisibly, a list containing the plotted `nodes` and `edges` data.
#'
#' @export
#'
#' @examples
#' result <- persist_ids(example_units(), time = "year")
#' plot_lineage(result, time = "year", lineage_id = "LID000004")
#'
plot_lineage <- function(
    data,
    time,
    lineage_id = NULL,
    show_legend = TRUE,
    node_cex = 1.2,
    ...) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or sf object.")
  }

  if (!is.character(time) || length(time) != 1L || is.na(time) ||
      !nzchar(time) || !(time %in% names(data))) {
    stop("`time` must name one column in `data`.")
  }

  required_columns <- c("spatial_id", "lineage_id", "transition_type")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop("`data` is not a complete spatpersist result.")
  }

  diagnostics <- id_transitions(data)
  available_lineages <- sort(unique(data$lineage_id))

  if (is.null(lineage_id)) {
    lineage_id <- available_lineages
  } else {
    unknown_lineages <- setdiff(lineage_id, available_lineages)
    if (length(unknown_lineages) > 0L) {
      stop(
        "Unknown lineage ID: ",
        paste(unknown_lineages, collapse = ", "),
        "."
      )
    }
  }

  selected_rows <- which(data$lineage_id %in% lineage_id)
  if (length(selected_rows) == 0L) {
    stop("No observations were selected for plotting.")
  }

  if (nrow(diagnostics) > 0L) {
    invalid_references <- diagnostics$old_row < 1L |
      diagnostics$new_row < 1L |
      diagnostics$old_row > nrow(data) |
      diagnostics$new_row > nrow(data)
    aligned_ids <- !invalid_references &
      data$spatial_id[diagnostics$old_row] == diagnostics$old_spatial_id &
      data$spatial_id[diagnostics$new_row] == diagnostics$new_spatial_id

    if (any(invalid_references) || !all(aligned_ids)) {
      stop(
        "Transition diagnostics no longer align with `data`; recreate the ",
        "spatial IDs before plotting."
      )
    }
  }

  observed_times <- sort(unique(data[[time]][selected_rows]))
  time_keys <- as.character(observed_times)
  identity_lanes <- sort(unique(data$spatial_id[selected_rows]))
  x_position <- match(as.character(data[[time]][selected_rows]), time_keys)
  y_position <- match(data$spatial_id[selected_rows], identity_lanes)

  nodes <- data.frame(
    row = selected_rows,
    time = data[[time]][selected_rows],
    spatial_id = data$spatial_id[selected_rows],
    lineage_id = data$lineage_id[selected_rows],
    transition_type = data$transition_type[selected_rows],
    x = x_position,
    y = y_position
  )

  edge_rows <- diagnostics$lineage_link &
    diagnostics$old_row %in% selected_rows &
    diagnostics$new_row %in% selected_rows
  edges <- diagnostics[edge_rows, ]

  if (nrow(edges) > 0L) {
    edges$x0 <- match(as.character(edges$previous_time), time_keys)
    edges$x1 <- match(as.character(edges$current_time), time_keys)
    edges$y0 <- match(edges$old_spatial_id, identity_lanes)
    edges$y1 <- match(edges$new_spatial_id, identity_lanes)
  } else {
    edges$x0 <- numeric()
    edges$x1 <- numeric()
    edges$y0 <- numeric()
    edges$y1 <- numeric()
  }

  transition_colors <- c(
    initial = "#4E79A7",
    continuation = "#59A14F",
    new = "#9C755F",
    split = "#F28E2B",
    merger = "#E15759",
    replacement = "#B07AA1",
    complex = "#FF9DA7"
  )

  original_margins <- graphics::par("mar")
  adjusted_margins <- original_margins
  adjusted_margins[[2L]] <- max(
    original_margins[[2L]],
    4.5 + 0.45 * max(nchar(identity_lanes))
  )
  graphics::par(mar = adjusted_margins)
  on.exit(graphics::par(mar = original_margins), add = TRUE)

  graphics::plot.default(
    NA,
    xlim = c(0.75, length(time_keys) + 0.25),
    ylim = c(0.5, length(identity_lanes) + 0.5),
    axes = FALSE,
    xlab = time,
    ylab = "",
    ...
  )
  graphics::axis(1, at = seq_along(time_keys), labels = time_keys)
  graphics::axis(2, at = seq_along(identity_lanes), labels = identity_lanes, las = 1)
  graphics::mtext(
    "spatial_id",
    side = 2,
    line = adjusted_margins[[2L]] - 1.5
  )
  graphics::box()

  if (nrow(edges) > 0L) {
    graphics::segments(
      edges$x0,
      edges$y0,
      edges$x1,
      edges$y1,
      lty = ifelse(edges$selected, 1, 2),
      lwd = ifelse(edges$selected, 2, 1.5),
      col = ifelse(edges$selected, "#333333", "#8C8C8C")
    )
  }

  node_colors <- unname(transition_colors[nodes$transition_type])
  node_colors[is.na(node_colors)] <- "#BAB0AC"
  graphics::points(
    nodes$x,
    nodes$y,
    pch = 21,
    bg = node_colors,
    col = "#333333",
    cex = node_cex
  )

  if (isTRUE(show_legend)) {
    displayed_types <- intersect(names(transition_colors), unique(nodes$transition_type))
    graphics::legend(
      "topright",
      legend = displayed_types,
      pt.bg = unname(transition_colors[displayed_types]),
      pch = 21,
      bty = "n",
      cex = 0.8
    )
  }

  invisible(list(nodes = nodes, edges = edges))
}

#' Save a ggplot
#'
#' This is a wrapper for [ggsave()][ggplot2::ggsave] that attempts to make a
#' reasonable guess at the plot size, particularly if they have been set in
#' the `theme(panel.widths, panel.heights)` settings or when the
#' `force_panelsizes()` function has been used.
#'
#' @inheritParams ggplot2::ggsave
#' @inheritDotParams ggplot2::ggsave
#' @param width,height Plot size in units expressed by the `units` argument.
#'   If `NULL` (default), the plot size will be measured. When the plot
#'   does not have a fixed size, these become `NA`, meaning that the size of
#'   the current graphics device is used.
#' @returns The file name with `width` and `height` attributes in inches,
#'   invisibly.
#' @export
#'
#' @examples
#' # A plot with fixed dimensions
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme(
#'     panel.widths = unit(10, "cm"),
#'     panel.heights = unit(2, "cm")
#'   )
#'
#' # Save plot to a temporary file
#' tmp <- tempfile(fileext = ".png")
#' save_plot(tmp, plot = p)
#'
#' # Clean up temporary file
#' unlink(tmp)
save_plot <- function(
  ...,
  plot = get_last_plot(),
  width = NULL,
  height = NULL,
  units = c("in", "cm", "mm", "px"),
  dpi = 300
) {
  gt <- as.gtable(plot)
  units <- arg_match0(units, c("in", "cm", "mm", "px"))

  if (is.null(width)) {
    width <- gt$widths
    if (has_null_unit(width)) {
      width <- NA_real_
    } else {
      width <- sum(width_cm(width)) / 2.54
    }
  } else {
    width <- width / switch(units, `in` = 1, cm = 2.54, mm = 25.4, px = dpi)
  }

  if (is.null(height)) {
    height <- gt$heights
    if (has_null_unit(height)) {
      height <- NA_real_
    } else {
      height <- sum(height_cm(height)) / 2.54
    }
  } else {
    height <- height / switch(units, `in` = 1, cm = 2.54, mm = 25.4, px = dpi)
  }

  out_file <- ggsave(
    ...,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi
  )
  attr(out_file, "width") <- width
  attr(out_file, "height") <- height
  out_file
}

has_null_unit <- function(x) {
  any(unitType(x) == "null")
}

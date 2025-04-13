#' Deprecated functions
#'
#' The functions listed here are deprecated and no longer work.
#'
#' @param ... Not used.
#'
#' @returns None, raises deprecation signal
#' @name deprecated
#'
#' @examples
#' # None
NULL

#' @rawNamespace if (packageVersion("ggplot2") <= "3.5.0") export(guide_axis_logticks)
#' @rdname deprecated
guide_axis_logticks <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "ggh4x::guide_axis_logticks()",
    "ggplot2::guide_axis_logticks()"
  )
}

#' @export
#' @rdname deprecated
guide_axis_manual <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "guide_axis_manual()",
    "legendry::guide_axis_base()"
  )
}

#' @export
#' @rdname deprecated
guide_axis_minor <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "guide_axis_minor()",
    I("`ggplot2::guide_axis(minor.ticks = TRUE)`")
  )
}

#' @export
#' @rdname deprecated
guide_axis_nested <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "guide_axis_nested()",
    "legendry::guide_axis_nested()"
  )
}

#' @export
#' @rdname deprecated
guide_axis_scalebar <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "guide_axis_scalebar()",
    "legendry::primitive_bracket()"
  )
}

#' @export
#' @rdname deprecated
guide_axis_truncated <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "guide_axis_truncated()",
    I("`ggplot2::guide_axis(cap = TRUE)`")
  )
}

#' @export
#' @rdname deprecated
guide_axis_colour <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "guide_axis_truncated()",
    I("`ggplot2::guide_axis(theme)`")
  )
}

#' @export
#' @rdname deprecated
guide_axis_color <- guide_axis_colour

#' @export
#' @rdname deprecated
guide_dendro <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "ggh4x::guide_dendro()",
    "legendry::guide_axis_dendro()"
  )
}

#' @export
#' @rdname deprecated
ggsubset <- function(...) {
  lifecycle::deprecate_stop(
    "0.2.0",
    "ggsubset()",
    details = paste0("This is best replaced by using ",
                     "`data = ~ subset(.x, ...)` instead.")
  )
}

#' @export
#' @rdname deprecated
scale_x_dendrogram <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "ggh4x::scale_x_dendrogram()",
    "legendry::scale_x_dendro()"
  )
}

#' @export
#' @rdname deprecated
scale_y_dendrogram <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "ggh4x::scale_y_dendrogram()",
    "legendry::scale_y_dendro()"
  )
}

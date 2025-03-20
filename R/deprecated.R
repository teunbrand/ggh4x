guide_axis_logticks <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "ggh4x::guide_axis_logticks()",
    "ggplot2::guide_axis_logticks()"
  )
}

guide_axis_manual <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "guide_axis_manual()",
    "legendry::guide_axis_base()"
  )
}

guide_axis_minor <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "guide_axis_minor()",
    I("`ggplot2::guide_axis(minor.ticks = TRUE)`")
  )
}

guide_axis_nested <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "guide_axis_nested()",
    "legendry::guide_axis_nested()"
  )
}

guide_axis_scalebar <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "guide_axis_scalebar()",
    "legendry::primitive_bracket()"
  )
}

guide_axis_truncated <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "guide_axis_truncated()",
    I("`ggplot2::guide_axis(cap = TRUE)`")
  )
}

guide_axis_colour <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "guide_axis_truncated()",
    I("`ggplot2::guide_axis(theme)`")
  )
}

guide_axis_color <- guide_axis_colour

guide_dendro <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "ggh4x::guide_dendro()",
    "legendry::guide_axis_dendro()"
  )
}

ggsubset <- function(...) {
  lifecycle::deprecate_stop(
    "0.2.0",
    "ggsubset()",
    details = paste0("This is best replaced by using ",
                     "`data = ~ subset(.x, ...)` instead.")
  )
}

scale_x_dendrogram <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "ggh4x::scale_x_dendrogram()",
    "legendry::scale_x_dendro()"
  )
}

scale_y_dendrogram <- function(...) {
  lifecycle::deprecate_stop(
    "0.3.0",
    "ggh4x::scale_y_dendrogram()",
    "legendry::scale_y_dendro()"
  )
}

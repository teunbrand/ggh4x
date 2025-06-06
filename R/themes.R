#' @title Theme extensions
#' @name theme_extensions
#'
#' @description Some functions in \pkg{ggh4x} are using extensions to the theme
#' system. These extended theme argument are listed below, along with what
#' elements they are expected to be, and in what function(s) they are used.
#'
#' @usage NULL
#'
#' @param ggh4x.facet.nestline An [`element_line()`][ggplot2::element_line]
#'   used as the parent for the `nest_line` argument in [`facet_nested()`] and
#'   [`facet_nested_wrap()`]. Inherits directly from the '`line`' theme element.
#' @param ggh4x.axis.nestline,ggh4x.axis.nestline.x,ggh4x.axis.nestline.y An
#'   [`element_line()`][ggplot2::element_line] used as the line to separate
#'   different layers of labels in [`guide_axis_nested()`]. Inherits from the
#'   '`axis.ticks`' theme element.
#' @param ggh4x.axis.nesttext.x,ggh4x.axis.nesttext.y An
#'   [`element_text()`][ggplot2::element_text] used to differentiate text higher
#'   in the hierarchy from the axis labels directly next to the axis line in
#'   [`guide_axis_nested()`]. Inherits from the '`axis.text.x`' and
#'   '`axis.text.y`' theme elements respectively.
#' @param ggh4x.axis.ticks.length.minor A [`rel()`][ggplot2::rel] object used to
#'   set the size of minor tick marks relative to the regular tick marks. This
#'   is used in the [`guide_axis_minor()`] and [`guide_axis_logticks()`]
#'   functions. Defaults to `rel(2/3)`.
#' @param ggh4x.axis.ticks.length.mini A [`rel()`][ggplot2::rel] object used to
#'   set the size of the smallest tick marks relative to regular tick marks.
#'   This is only used in the [`guide_axis_logticks()`] function.
#'   Defaults to `rel(1/3)`.
ggh4x_theme_elements <- function() {
  register_theme_elements(
    ggh4x.facet.nestline = element_blank(),
    element_tree = list(
      ggh4x.facet.nestline  = el_def("element_line", "line")
    )
  )
}

# Main function -----------------------------------------------------------

#' Ribbon of panels with nested strips.
#'
#' `facet_nested_wrap()` wraps a sequence of panels onto a two-dimensional
#' layout, and nests grouped facets where possible.
#'
#' @inheritParams facet_wrap2
#' @inheritParams facet_nested
#' @param strip An object created by a call to a strip function, such as
#'   [ggh4x::strip_nested()].
#'
#' @details This function inherits the capabilities of
#'   [ggh4x::facet_wrap2()].
#'
#'   This function only merges strips in the same row or column as they appear
#'   through regular `facet_wrap()` layout behaviour.
#'
#'   Hierarchies are inferred from the order of variables supplied to
#'   `facets`. The first variable is interpreted to be the outermost
#'   variable, while the last variable is interpreted to be the innermost
#'   variable. They display order is always such that the outermost
#'   variable is placed the furthest away from the panels. For more information
#'   about the nesting of strips, please visit the documentation of
#'   [ggh4x::strip_nested()].
#'
#' @return A `FacetNestedWrap` ggproto object that can be added to a plot.
#' @export
#' @family facetting functions
#' @include facet_wrap2.R
#'
#' @seealso See [ggh4x::strip_nested()] for nested strips. See
#'   [ggplot2::facet_wrap()] for descriptions of the original
#'   arguments. See [grid::unit()] for the construction of a
#'   `unit` vector.
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Similar to `facet_wrap2(..., strip = strip_nested())`.
#' p + facet_nested_wrap(vars(cyl, drv))
#'
#' # A nest line inherits from the global theme
#' p + facet_nested_wrap(vars(cyl, drv),
#'                       nest_line = element_line(colour = "red")) +
#'   theme(ggh4x.facet.nestline = element_line(linetype = 3))
facet_nested_wrap <- function(
  facets, nrow = NULL, ncol = NULL,
  scales = "fixed", axes = "margins",
  remove_labels = "none",
  shrink = TRUE, labeller = "label_value",
  as.table = TRUE, drop = TRUE,
  dir = "h", strip.position = "top",
  nest_line = element_line(inherit.blank = TRUE),
  solo_line = FALSE,
  resect = unit(0, "mm"),
  trim_blank = TRUE,
  strip = strip_nested(),
  bleed = NULL
) {
  strip <- assert_strip(strip)
  if (!is.null(bleed)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "facet_nested_wrap(bleed)",
      details = paste0("The `bleed` argument should be set in the ",
                       "`strip_nested()` function instead.")
    )
    strip$params$bleed <- isTRUE(bleed)
  }
  # Convert logical to elements for backward compatibility
  if (isTRUE(nest_line)) {
    nest_line <- element_line()
  }
  if (isFALSE(nest_line)) {
    nest_line <- element_blank()
  }
  if (!inherits(nest_line, c("element_line", "element_blank"))) {
    cli::cli_abort(paste0(
      "The {.arg nest_line} argument must be {.cls element_blank} or inherit ",
      "from {.cls element_line}."
    ))
  }
  params <- list(
    nest_line = nest_line,
    solo_line = isTRUE(solo_line),
    resect = resect
  )
  new_wrap_facets(
    facets, nrow, ncol,
    scales, axes, remove_labels,
    shrink, labeller,
    as.table, drop, dir,
    strip.position, strip,
    trim_blank, params,
    super = FacetNestedWrap
  )
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
FacetNestedWrap <- ggproto(
  "FacetNestedWrap", FacetWrap2,
  finish_panels = function(self, panels, layout, params, theme) {
    add_nest_indicator(panels, params, theme)
  }
)

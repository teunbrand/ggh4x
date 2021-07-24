# Main function -----------------------------------------------------------

#' Ribbon of panels with nested strips.
#'
#' \code{facet_nested_wrap()} wraps a sequence of panels onto a two-dimensional
#' layout, and nests grouped facets where possible.
#'
#' @inheritParams facet_wrap2
#' @inheritParams facet_nested
#' @param strip An object created by a call to a strip function, such as
#'   \code{\link[ggh4x]{strip_nested}()}.
#'
#' @details This function inherits the capabilities of
#'   \code{\link[ggh4x]{facet_wrap2}()}.
#'
#'   This function only merges strips in the same row or column as they appear
#'   through regular \code{facet_wrap()} layout behaviour.
#'
#'   Hierarchies are inferred from the order of variables supplied to
#'   \code{facets}. The first variable is interpreted to be the outermost
#'   variable, while the last variable is interpreted to be the innermost
#'   variable. They display order is always such that the outermost
#'   variable is placed the furthest away from the panels. For more information
#'   about the nesting of strips, please visit the documentation of
#'   \code{\link[ggh4x]{strip_nested}()}.
#'
#' @return A \code{FacetNestedWrap} ggproto object that can be added to a plot.
#' @export
#' @family facetting functions
#' @include facet_wrap2.R
#'
#' @seealso See \code{\link[ggh4x]{strip_nested}} for nested strips. See
#'   \code{\link[ggplot2]{facet_wrap}} for descriptions of the original
#'   arguments. See \code{\link[grid]{unit}} for the construction of a
#'   \code{unit} vector.
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
  nest_line = element_blank(),
  resect = unit(0, "mm"),
  trim_blank = TRUE,
  strip = strip_nested(),
  bleed = NULL
) {
  strip <- assert_strip(strip)
  if (!is.null(bleed)) {
    message(paste0("The `bleed` argument should be set in the ",
                   " `strip_nested()` function."))
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
    abort(paste0("The `nest_line` argument must be an 'element_blank' or ",
                 "inherit from an 'element_line'."))
  }
  params <- list(
    nest_line = nest_line,
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

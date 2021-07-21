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
#'   \code{rows} or \code{cols}. The first variable is interpreted to be the
#'   outermost variable, while the last variable is interpreted to be the
#'   innermost variable. They display order is always such that the outermost
#'   variable is placed the furthest away from the panels. Strips are
#'   automatically grouped when they span a nested variable.
#'
#'   The \code{bleed} argument controls whether lower-level strips are allowed
#'   to be merged when higher-level strips are different, i.e. they can bleed
#'   over hierarchies. Suppose the \code{facet_wrap()} behaviour would be the
#'   following for strips:
#'
#'   \code{[_1_][_2_][_2_]} \cr \code{[_3_][_3_][_4_]}
#'
#'   In such case, the default \code{bleed = FALSE} argument would result in the
#'   following:
#'
#'   \code{[_1_][___2____]} \cr \code{[_3_][_3_][_4_]}
#'
#'   Whereas \code{bleed = TRUE} would allow the following:
#'
#'   \code{[_1_][___2____]} \cr \code{[___3____][_4_]}
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
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#' p + facet_nested_wrap(vars(cyl, drv))
#'
#' # Controlling the nest line
#' p + facet_nested_wrap(vars(cyl, drv), nest_line = TRUE) +
#'   theme(ggh4x.facet.nestline = element_line(linetype = 3))
#'
#' # Ignore nested hierarchies with the 'bleed' argument
#'  p + facet_nested_wrap(vars(drv, cyl), bleed = TRUE)
facet_nested_wrap <- function(
  facets, nrow = NULL, ncol = NULL,
  scales = "fixed", axes = "margins",
  remove_labels = "none",
  shrink = TRUE, labeller = "label_value",
  as.table = TRUE, drop = TRUE,
  dir = "h", strip.position = "top",
  nest_line = FALSE,
  resect = unit(0, "mm"),
  trim_blank = TRUE,
  strip = strip_nested(),
  bleed = NULL
) {

  strip.position <- match.arg(strip.position,
                              c("top", "bottom", "left",  "right"))
  labeller <- .int$check_labeller(labeller)
  facets <- .int$wrap_as_facets_list(facets)

  # Take care of direction
  dir <- match.arg(dir, c("h", "v"))
  if (identical(dir, "v")) {
    nrow_swap <- ncol
    ncol_swap <- nrow
    nrow <- .int$sanitise_dim(nrow_swap)
    ncol <- .int$sanitise_dim(ncol_swap)
  } else {
    nrow <- .int$sanitise_dim(nrow)
    ncol <- .int$sanitise_dim(ncol)
  }

  # Setup free scales
  free  <- .match_facet_arg(scales, c("fixed", "free_x", "free_y", "free"))
  axes  <- .match_facet_arg(axes, c("margins", "x", "y", "all"))
  rmlab <- .match_facet_arg(remove_labels, c("none", "x", "y", "all"))

  if (!is.null(bleed)) {
    message(paste0("The `bleed` argument should be set in the ",
                   " `strip_nested()` function."))
    strip$params$bleed <- isTRUE(bleed)
  }
  strip <- assert_strip(strip)

  if (trim_blank) {
    dim <- NULL
  } else {
    dim <- c(nrow %||% NA_integer_, ncol %||% NA_integer_)
  }

  ggproto(
    NULL, FacetNestedWrap,
    shrink = shrink,
    strip = strip,
    params = list(
      facets = facets,
      free = free,
      as.table = as.table,
      strip.position = strip.position,
      drop = drop,
      ncol = ncol,
      nrow = nrow,
      labeller = labeller,
      dir = dir,
      axes = axes,
      rmlab = rmlab,
      nest_line = nest_line,
      resect = resect,
      independent = list(x = FALSE, y = FALSE),
      dim = dim
    )
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

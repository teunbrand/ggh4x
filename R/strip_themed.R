#' @include strip_vanilla.R
NULL
# constructor -------------------------------------------------------------

#' Strip with themed boxes and texts
#'
#' A style of strips with individually themed strips.
#'
#' @inheritParams strip_vanilla
#' @param text_x,text_y A `list()` with
#'   [`element_text()`][ggplot2::element_text()] elements. See details.
#' @param background_x,background_y A `list()` with
#'   [`element_rect()`][ggplot2::element_rect()] elements. See details.
#' @param by_layer_x,by_layer_y A `logical(1)` that when `TRUE`, maps the
#'   different elements to different layers of the strip. When `FALSE`, maps the
#'   different elements to individual strips, possibly repeating the elements
#'   to match the number of strips through `rep_len()`.
#'
#' @details With respect to the `text_*` and `background_*` arguments, they can
#'   be a list with (a mix of) the following objects:
#'
#'   * `NULL` indicates that the global plot theme applies.
#'   * `element_blank()` omits drawing the background or text.
#'   * An `element` class object inheriting from the `element_text` or
#'     `element_rect` classes.
#'
#'   For constructing homogeneous lists of elements, the
#'   [`elem_list_text()`][elem_list_text()] and
#'   [`elem_list_rect()`][elem_list_rect] are provided for convenience.
#'
#' @return A `StripThemed` ggproto object that can be given as an argument to
#'   facets in ggh4x.
#' @export
#' @md
#' @family strips
#'
#' @examples
#' # Some simple plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Set some theming options, we can use `element_blank()`
#' backgrounds <- list(element_blank(), element_rect(fill = "dodgerblue"))
#' # Or we could use `NULL` to use the global theme
#' texts <- list(element_text(colour = "red"), NULL, element_text(face = "bold"))
#'
#' # Elements are repeated until the fit the number of facets
#' p + facet_wrap2(
#'   vars(drv, year),
#'   strip = strip_themed(
#'     background_x = backgrounds,
#'     text_x = texts
#'   )
#' )
#'
#' # Except when applied to each layer instead of every strip
#' p + facet_wrap2(
#'   vars(drv, year),
#'   strip = strip_themed(
#'     background_x = backgrounds,
#'     text_x = texts,
#'     by_layer_x = TRUE
#'   )
#' )
#'
#' # To conveniently distribute arguments over a list of the same elements,
#' # you can use the following wrappers:
#' p + facet_wrap2(
#'   vars(drv, year),
#'   strip = strip_themed(
#'     text_x = elem_list_text(colour = c("blue", "red")),
#'     background_x = elem_list_rect(fill = c("white", "grey80")),
#'     by_layer_x = TRUE
#'   )
#' )
strip_themed <- function(
  clip = "inherit",
  size = "constant",
  text_x = NULL,
  text_y = NULL,
  background_x = NULL,
  background_y = NULL,
  by_layer_x = FALSE,
  by_layer_y = FALSE
) {

  params <- list(
    clip = arg_match0(clip, c("on", "off", "inherit")),
    size = arg_match0(size, c("constant", "variable"))
  )

  given_elements = list(
    text_x = validate_element_list(text_x, "element_text"),
    text_y = validate_element_list(text_y, "element_text"),
    background_x = validate_element_list(background_x, "element_rect"),
    background_y = validate_element_list(background_y, "element_rect"),
    by_layer_x = isTRUE(by_layer_x),
    by_layer_y = isTRUE(by_layer_y)
  )

  ggproto(
    NULL, StripThemed,
    params = params,
    given_elements = given_elements
  )
}

# ggproto class -----------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
StripThemed <- ggproto(
  "StripElemental", Strip,

  given_elements = list(),

  setup_elements = function(self, theme, type) {
    given <- self$given_elements
    # Actual strip theme elements
    background <- list(
      "x" = calc_element("strip.background.x", theme),
      "y" = calc_element("strip.background.y", theme)
    )
    if (!is.null(given$background_x)) {
      background$x <- lapply(given$background_x, inherit_element,
                             background$x)
      background$x <- lapply(background$x, element_grob)
    } else {
      background$x <- element_grob(background$x)
    }
    if (!is.null(given$background_y)) {
      background$y <- lapply(given$background_y, inherit_element,
                             background$y)
      background$y <- lapply(background$y, element_grob)
    } else {
      background$y <- element_grob(background$y)
    }
    text <- list(
      "x" = list(
        "top"    = calc_element("strip.text.x.top",    theme),
        "bottom" = calc_element("strip.text.x.bottom", theme)
      ),
      "y" = list(
        "left"  = calc_element("strip.text.y.left",  theme),
        "right" = calc_element("strip.text.y.right", theme)
      )
    )
    if (!is.null(given$text_x)) {
      text$x$top    <- lapply(given$text_x, inherit_element, text$x$top)
      text$x$bottom <- lapply(given$text_x, inherit_element, text$x$bottom)
    }
    if (!is.null(given$text_y)) {
      text$y$left   <- lapply(given$text_y, inherit_element, text$y$left)
      text$y$right  <- lapply(given$text_y, inherit_element, text$y$right)
    }
    # Strip placement theme elements
    inside <- list(
      "x" = calc_element("strip.placement.x", theme) %||% "inside" == "inside",
      "y" = calc_element("strip.placement.y", theme) %||% "inside" == "inside"
    )
    if (type == "wrap") {
      padding <- calc_element("strip.switch.pad.wrap", theme)
    } else {
      padding <- calc_element("strip.switch.pad.grid", theme)
    }
    padding <- convertUnit(padding, "cm")

    by_layer <- list(
      "x" = given$by_layer_x,
      "y" = given$by_layer_y
    )

    list(
      padding = padding,
      background = background,
      text = text,
      inside = inside,
      by_layer = by_layer
    )
  }
)


# Helpers -----------------------------------------------------------------

validate_element_list <- function(elem, prototype = "element_text") {
  argname <- deparse(substitute(elem))
  if (is.null(elem)) {
    return(elem)
  }
  if (!inherits(elem, "list")) {
    elem <- list(elem)
  }
  is_proto <- vapply(elem, inherits, logical(1), c(prototype, "element_blank"))
  is_null  <- vapply(elem, is.null,  logical(1))
  if (any(!(is_proto | is_null))) {
    rlang::abort(
      paste0("The `", argname, "` argument should be a list of `", prototype,
             "` objects.")
    )
  }
  return(elem)
}


# Based on ggplot2:::combine_elements
inherit_element <- function(child, parent) {

  if (is.null(parent) || inherits(child, "element_blank")) {
    return(child)
  }

  if (is.null(child)) {
    return(parent)
  }

  if (!inherits(child, "element") && !inherits(parent, "element")) {
    return(child)
  }

  if (inherits(parent, "element_blank")) {
    if (child$inherit.blank) {
      return(parent)
    } else {
      return(child)
    }
  }

  n <- names(child)[vapply(child, is.null, logical(1))]
  child[n] <- parent[n]

  if (inherits(child$size, "rel")) {
    child$size <- parent$size * unclass(child$size)
  }
  return(child)
}




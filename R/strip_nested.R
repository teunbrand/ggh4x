#' @include strip_themed.R
NULL
# Constructor -------------------------------------------------------------

#' Nested strips
#'
#' This strip style groups strips on the same layer that share a label. It is
#' the default strip for [`facet_nested()`][facet_nested()] and
#' [`facet_nested_wrap()`][facet_nested_wrap()].
#'
#' @inheritParams strip_vanilla
#' @param bleed A `logical(1)` indicating whether mergin of lower-layer
#'   variables is allowed when the higher-layer variables are separate. See
#'   details.
#' @param text_x,text_y A `list()` with
#'   [`element_text()`][ggplot2::element_text()] elements. See the details
#'   section in [`strip_themed()`][strip_themed()].
#' @param background_x,background_y A `list()` with
#'   [`element_rect()`][ggplot2::element_rect()] elements. See the details
#'   section in [`strip_themed()`][strip_themed()].
#' @param by_layer_x,by_layer_y A `logical(1)` that when `TRUE`, maps the
#'   different elements to different layers of the strip. When `FALSE`, maps the
#'   different elements to individual strips, possibly repeating the elements to
#'   match the number of strips through `rep_len()`.
#'
#' @details The display order is always such that the outermost
#'   variable is placed the furthest away from the panels. Strips are
#'   automatically grouped when they span a nested variable.
#'
#'   The \code{bleed} argument controls whether lower-layer strips are allowed
#'   to be merged when higher-layer strips are different, i.e. they can bleed
#'   over hierarchies. Suppose the \code{strip_vanilla()} behaviour would be the
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
#' @return A `StripNested` ggproto object that can ge given as an argument to
#'   facets in ggh4x.
#' @export
#' @md
#' @family strips
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Combine the strips
#' p + facet_wrap2(vars(cyl, drv), strip = strip_nested())
#'
#' # The facet_nested and facet_nested_wrap functions have nested strips
#' # automatically
#' p + facet_nested_wrap(vars(cyl, drv))
#'
#' # Changing the bleed argument merges the "f" labels in the top-right
#' p + facet_wrap2(vars(cyl, drv), strip = strip_nested(bleed = TRUE))
strip_nested <- function(
  clip = "inherit",
  size = "constant",
  bleed = FALSE,
  text_x = NULL,
  text_y = NULL,
  background_x = NULL,
  background_y = NULL,
  by_layer_x = FALSE,
  by_layer_y = FALSE
) {
  params <- list(
    clip = arg_match0(clip, c("on", "off", "inherit")),
    size = arg_match0(size, c("constant", "variable")),
    bleed = isTRUE(bleed)
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
    NULL, StripNested,
    params = params,
    given_elements = given_elements
  )
}

# ggproto class -----------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
StripNested <- ggproto(
  "StripNested", StripThemed,

  params = list(bleed = FALSE),

  assemble_strip = function(self, labels, position, elements,
                            params, layout) {
    # Use regular strip assembly if strip is monolayered
    if({nlayers <- ncol(labels)} == 1) {
      out <- ggproto_parent(Strip, self)$assemble_strip(
        labels, position, elements, params, layout
      )
      return(out)
    }
    aes <- if (position %in% c("top", "bottom")) "x" else "y"
    # Do merging of adjacent labels
    bleed <- self$params$bleed
    if (position %in% c("right")) {
      labels <- labels[, rev(seq_ncol(labels)), drop = FALSE]
    }
    if (aes == "x") {
      o <- order(layout$ROW, layout$COL)
      sepvar <- "ROW"
    } else {
      o <- order(layout$COL, layout$ROW)
      sepvar <- "COL"
    }
    labels <- labels[o, ]
    layout <- layout[o, ]

    # Dummy label dataframe
    tmp <- as.data.frame(labels)
    tmp[] <- lapply(tmp, paste0, layout[[sepvar]])
    if (!bleed) {
      # Paste labels with previous layers to prevent bleeding
      tmp[-1] <- lapply(seq_ncol(labels)[-1], function(i) {
        do.call(paste0, tmp[, seq(i), drop = FALSE])
      })
    }
    # Determine merges with run length encoding
    lens <- lapply(tmp, function(x) {rle(x)$lengths})
    ends <- unlist(lapply(lens, cumsum), use.names = FALSE)
    starts <- ends - unlist(lens, use.names = FALSE) + 1

    # Redefine layout
    panel <- as.integer(layout$PANEL)
    layout <- .int$new_data_frame(list(
      t = panel[starts], b = panel[ends],
      l = panel[starts], r = panel[ends],
      layer = rep(seq_along(lens), lengths(lens))
    ))
    index  <- layout$layer
    labels <- labels[cbind(starts, index)]

    # Business as usual
    elems  <- self$init_strip(elements, position, index)
    strips <- self$draw_labels(labels, elems, position, layer_id = index,
                               size = params$size)
    width  <- rep_len(attr(strips, "width"),  nlayers)
    height <- rep_len(attr(strips, "height"), nlayers)

    self$finish_strip(unname(strips), width, height, position, layout,
                      dim = c(nrow(layout), nlayers), params$clip)
  },

  finish_strip = function(strip, width, height, position, layout, dim,
                          clip = "inherit") {
    if (!("layer" %in% names(layout))) {
      out <- Strip$finish_strip(strip, width, height, position,
                                layout, dim, clip)
      return(out)
    }
    empty_strips <- length(strip) == 0 ||
      all(vapply(strip, .int$is.zero, logical(1)))
    if (!empty_strips) {
      index <- layout$layer
      if (position %in% c("bottom", "right")) {
        index  <- dim[2] - index + 1
        width  <- rev(width)
        height <- rev(height)
      }
      if (position %in% c("top", "bottom")) {
        gt <- gtable(widths = width[1], height = height)
        strip <- mapply(function(x, i) {
          gtable_add_grob(gt, x, t = i, l = 1, clip = clip)
        }, x = strip, i = index, SIMPLIFY = FALSE)
      } else {
        gt <- gtable(widths = width, height = height[1])
        strip <- mapply(function(x, i) {
          gtable_add_grob(gt, x, t = 1, l = i, clip = clip)
        }, x = strip, i = index, SIMPLIFY = FALSE)
      }
    }
    layout$grobs <- strip
    layout
  }
)

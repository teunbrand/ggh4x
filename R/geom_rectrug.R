# Main functions ----------------------------------------------------------

#' Rectangular rugs in the margins
#'
#' @description Like rug plots display data points of a 2D plot as lines in the
#'   margins, this function plots rectangles in the margins. Rectangular rugs
#'   are convenient for displaying one-dimensional, ranged annotations for
#'   two-dimensional plots.
#'
#' @inheritParams ggplot2::geom_rect
#' @param outside `logical` of length 1 that controls whether to move the
#'   rectangles outside of the plot area. For the best results, it is probably
#'   best to set `coord_cartesian(clip = "off")` and avoid overlap with the
#'   default axes by changing the sides argument to `"tr"`.
#' @param sides A `string` of length 1 that controls which sides of the
#'   plot the rug-rectangles appear on. A string containing any letters in
#'   `"trbl"` will set it to top, right, bottom and left respectively.
#' @param length A [grid::unit()] object that sets the width and
#'   height of the rectangles in the x- and y-directions respectively. Note that
#'   scale expansion can affect the look of this.
#'
#' @details By default, scales are expanded 5\% on either side of the plot,
#'   whereas the rug rectangles will occupy 3\% of the total plot size by
#'   default. The `geom_rectmargin()` and `geom_tilemargin()` versions do the
#'   same thing, but are parametrised differently; see
#'   [`geom_rect()`][ggplot2::geom_tile].
#'
#'   These functions do not have hard-coded required aesthetics, since the x and
#'   y directions can be omitted by not choosing a side in the corresponding
#'   direction, i.e. y-direction variables are omitted when plotting the rug only
#'   on the top and/or bottom. This can result in errors when the aesthetics are
#'   not specified appropriately, so some caution is advised.
#'
#' @export
#'
#' @return A *Layer* ggproto object.
#'
#' @importFrom ggplot2 layer
#'
#' @section Aesthetics:
#' `geom_rectmargin()` requires either one of the following
#' sets of aesthetics, but also can use both:
#'
#' \itemize{
#'  \item **xmin**
#'  \item **xmax**
#' }
#'
#' and/or:
#'
#' \itemize{
#'  \item **ymin**
#'  \item **ymax**
#' }
#'
#' `geom_tilemargin()` requires either one of the following
#' sets of aesthetics, but can also use both:
#'
#' \itemize{
#'  \item **x**
#'  \item **width**
#' }
#'
#' and/or:
#'
#' \itemize{
#'  \item **y**
#'  \item **height**
#' }
#'
#' Furthermore, `geom_rectmargin()` and `geom_tilemargin()` also
#' understand these shared aesthetics:
#'
#' \itemize{
#'  \item alpha
#'  \item colour
#'  \item fill
#'  \item group
#'  \item linetype
#'  \item size
#' }
#'
#' @seealso [ggplot2::geom_rug()], [`geom_rect()`][ggplot2::geom_tile],
#'   [ggplot2::geom_tile()]
#'
#' @examples
#' # geom_rectmargin() is parameterised by the four corners
#' df <- data.frame(
#'   xmin = c(1, 5),
#'   xmax = c(2, 7),
#'   ymin = c(1, 2),
#'   ymax = c(2, 4),
#'   fill = c("A", "B")
#' )
#'
#'
#' ggplot(df, aes(xmin = xmin, xmax = xmax,
#'                ymin = ymin, ymax = ymax,
#'                fill = fill)) +
#'   geom_rect() +
#'   geom_rectmargin()
#'
#' # geom_tilemargin() is parameterised by center and size
#' df <- data.frame(
#'   x = c(1, 4),
#'   y = c(1, 2),
#'   width = c(2, 1),
#'   height = c(1, 2),
#'   fill = c("A", "B")
#' )
#'
#' ggplot(df, aes(x, y,
#'                width = width, height = height,
#'                fill = fill)) +
#'   geom_tile() +
#'   geom_tilemargin()
geom_rectmargin <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  outside = FALSE,
  sides = "bl",
  length = unit(0.03, "npc"),
  linejoin = "mitre",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomRectMargin,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list2(
      outside = outside,
      sides   = sides,
      length  = length,
      na.rm   = na.rm,
      ...
    )
  )
}

#' @rdname geom_rectmargin
#' @export
geom_tilemargin <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  outside = FALSE,
  sides = "bl",
  length = unit(0.03, "npc"),
  linejoin = "mitre",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomTileMargin,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list2(
      outside = outside,
      sides   = sides,
      length  = length,
      na.rm   = na.rm,
      ...
    )
  )
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
GeomRectMargin <- ggplot2::ggproto(
  "GeomRectMargin", ggplot2::GeomRug,
  draw_panel = function(
    self, data, panel_params, coord, sides = "bl", outside = FALSE,
    length = unit(0.03, "npc"), linejoin = "mitre"
  ) {

    if (!inherits(length, "unit")) {
      cli::cli_abort(paste0(
        "The {.arg length} argument must be a {.cls unit} object."
      ))
    }
    rugs <- list()
    coords <- coord$transform(data, panel_params)
    if (inherits(coord, "CoordFlip")) {
      sides <- chartr("tblr", "rlbt", sides)
    }

    rug_length <- if (!outside) {
      list(min = length, max = unit(1, "npc") - length)
    } else {
      list(min = -1 * length, max = unit(1, "npc") + length)
    }

    gp <- grid::gpar(
      col = alpha(coords$colour, coords$alpha),
      fill = alpha(coords$fill, coords$alpha),
      linejoin = linejoin,
      lty = coords$linetype, lwd = coords$linewidth * .pt,
      lineend = if (identical(linejoin, "round")) "round" else "square"
    )

    if (!is.null(coords$xmin)) {
      if (grepl("b", sides)) {
        rugs$x_b <- grid::rectGrob(
          x = unit(coords$xmin, "native"),
          y = unit(0, "npc"),
          width = unit(coords$xmax - coords$xmin, "native"),
          height = rug_length$min,
          just = c("left", "bottom"),
          gp = gp
        )
      }

      if (grepl("t", sides)) {
        rugs$x_t <- grid::rectGrob(
          x = unit(coords$xmin, "native"),
          y = unit(1, "npc"),
          width = unit(coords$xmax - coords$xmin, "native"),
          height = rug_length$min,
          just = c("left", "top"),
          gp = gp
        )
      }
    }

    if (!is.null(coords$ymin)) {
      if (grepl("l", sides)) {
        rugs$y_l <- grid::rectGrob(
          x = unit(0, "npc"),
          y = unit(coords$ymax, "native"),
          width = rug_length$min,
          height = unit(coords$ymax - coords$ymin, "native"),
          just = c("left", "top"),
          gp = gp
        )
      }

      if (grepl("r", sides)) {
        rugs$y_r <- grid::rectGrob(
          x = unit(1, "npc"),
          y = unit(coords$ymax, "native"),
          width = rug_length$min,
          height = unit(coords$ymax - coords$ymin, "native"),
          just = c("right", "top"),
          gp = gp
        )
      }
    }

    grid::gTree(children = do.call(grid::gList, rugs))
  },
  optional_aes = c("x", "y", "xmin", "xmax", "ymin", "ymax"),
  default_aes = ggplot2::aes(colour = NA, fill = "grey35",
                             linewidth = 0.5, linetype = 1, alpha = NA),
  draw_key = ggplot2::draw_key_polygon
)

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
GeomTileMargin <- ggplot2::ggproto(
  "GeomTileMargin",
  GeomRectMargin,
  extra_params = c("na.rm"),
  setup_data = function(data, params) {
    data$width  <- data$width %||% params$width %||% resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

    transform(data,
              xmin = x - width / 2, xmax = x + width / 2, width = NULL,
              ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },
  default_aes = ggplot2::aes(fill = "grey20", colour = NA,
                             linewidth = 0.1, linetype = 1,
                             alpha = NA, width = NA, height = NA),
  draw_key = ggplot2::draw_key_polygon
)

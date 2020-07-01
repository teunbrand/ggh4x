# Main function ----------------------------------------------------------------

#' Polygon parameterisation for rasters
#'
#' \code{geom_polygonraster} takes data that describes a raster with pixels of
#' the same size and reparametrises the data as a polygon. This allows for more
#' flexible transformations of the data, but comes at an efficiency cost.
#'
#' @inheritParams ggplot2::geom_raster
#'
#' @details For each pixel in a raster, makes a vertex for each of the four
#'   corner points. These coordinates can then by transformed by
#'   \code{coord}-functions such as \code{\link[ggplot2]{coord_polar}} or
#'   \code{position}-functions such as
#'   \code{\link[ggh4x]{position_lineartrans}}. Currently substitutes group
#'   aesthetics right before drawing in favour of pixel identifiers.
#'
#' @section Aesthetics:
#'
#'   \code{geom_raster()} understands the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   \itemize{
#'     \item \strong{x}
#'     \item \strong{y}
#'     \item fill
#'     \item alpha
#'     \item group
#'   }
#'
#' @seealso \code{\link[ggplot2:geom_tile]{geom_raster}}
#'
#' @export
#'
#' @return A \emph{Layer} ggproto object.
#'
#' @examples
#' # Combining with coord_polar()
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   geom_polygonraster(aes(fill = density)) +
#'   coord_polar()
#'
#' # Combining with linear transformations
#' df <- data.frame(x = row(volcano)[TRUE],
#'                  y = col(volcano)[TRUE],
#'                  z = volcano[TRUE])
#'
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_polygonraster(position = position_lineartrans(angle = 30,
#'                                                      shear = c(1, 0)))
geom_polygonraster <- function(
  mapping = NULL, data = NULL, stat = "identity",
  position = position_lineartrans(),
  ..., hjust = 0.5, vjust = 0.5, na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE
) {
  stopifnot(is.numeric(hjust), length(hjust) == 1)
  stopifnot(is.numeric(vjust), length(vjust) == 1)
  layer(data = data, mapping = mapping, stat = stat, geom = GeomPolygonRaster,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(hjust = hjust, vjust = vjust,
                      na.rm = na.rm, ...))
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
GeomPolygonRaster <- ggproto(
  "GeomPolygonRaster", GeomRaster,
  setup_data = function(data, params) {
    w <- resolution(data$x)
    h <- resolution(data$y)
    hjust <- params$hjust %||% 0.5
    vjust <- params$vjust %||% 0.5

    data$id <- seq_len(NROW(data))

    coords <- matrix(
      c(rep(data$x - w * (1 - hjust), 2),
        rep(data$x + w * hjust, 2),
        data$y - h * (1 - vjust),
        rep(data$y + h * vjust, 2),
        data$y - h * (1 - vjust)),
      ncol = 2
    )

    data <- rbind(data, data, data, data)
    data$x <- coords[, 1]
    data$y <- coords[, 2]
    data <- data[order(data$id), ]
    rownames(data) <- NULL
    data
  },
  draw_panel = function(data, panel_params, coord, hjust = 0.5, vjust = 0.5) {
    n <- nrow(data)
    if (n == 1)
      return(zeroGrob())

    data$group <- data$id
    coords <- coord_munch(coord, data, panel_params)

    first <- coords[!duplicated(data$id), ]

    ggplot2:::ggname(
      "geom_polygon",
      polygonGrob(coords$x, coords$y, default.units = "native", id = coords$id,
                  gp = gpar(col = 0,
                            fill = alpha(first$fill, first$alpha),
                            lwd = 0,
                            lty = 0))
    )
  }
)

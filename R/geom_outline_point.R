# Main function -----------------------------------------------------------

#' Points with outline
#'
#' This is a variant of the point geom, wherein overlapping points are given
#' a shared outline. It works by drawing an additional layer of points below a
#' regular layer of points with a thicker stroke.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @return A ggplot `Layer`
#' @export
#' @eval fixup_docs(ggplot2:::rd_aesthetics("geom", "outline_point"))
#'
#' @details Due to the way this geom is implemented, it handles the `alpha`
#'   aesthetic pretty ungracefully.
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, cty, colour = factor(cyl))) +
#'   geom_outline_point(size = 10, stroke = 3)
#' p
#'
#' # The colour of the stroke can be mapped to a scale by setting the
#' # aesthetics to `"stroke_colour"`.
#' p +
#'   aes(stroke_colour = factor(cyl)) +
#'   scale_colour_hue(
#'     aesthetics = "stroke_colour",
#'     l = 50
#'   )
geom_outline_point <- function(
  mapping  = NULL,
  data     = NULL,
  stat     = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data     = data,
    mapping  = mapping,
    stat     = stat,
    geom     = GeomOutlinePoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )
}

# Key drawing -------------------------------------------------------------

draw_key_outline_point <- function(data, params, size) {

  is_solid <- data$shape > 14
  has_fill <- data$shape > 20

  stroke_size <- data$stroke
  stroke_size[is.na(stroke_size)] <- 0
  lwd <- ifelse(is_solid & !has_fill, 0, stroke_size * .stroke / 2)

  foreground <- pointsGrob(
    x   = 0.5, y = 0.5,
    pch = data$shape,
    gp  = gpar(
      col  = alpha(data$colour, data$alpha),
      fill = alpha(data$fill,   data$alpha),
      fontsize = data$size * .pt,
      lwd  = lwd
    )
  )

  size <- data$size * .pt + ifelse(is_solid, stroke_size * .stroke, 0)
  lwd  <- lwd + ifelse(is_solid, 0, stroke_size * .stroke)

  background <- pointsGrob(
    x = 0.5, y = 0.5,
    pch = data$shape,
    gp  = gpar(
      col  = alpha(data$stroke_colour, data$alpha),
      fill = alpha(data$stroke_colour, data$alpha),
      lwd  = lwd,
      fontsize = size
    )
  )

  grobTree(background, foreground)
}

# ggproto class -----------------------------------------------------------

GeomOutlinePoint <- ggproto(
  "GeomOutlinePoint", GeomPoint,

  default_aes = aes(
    shape = 16, colour = "grey50", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5, stroke_colour = "black"
  ),

  draw_key = draw_key_outline_point,

  draw_panel = function(data, panel_params, coord, na.rm = TRUE) {

    coords <- coord$transform(data, panel_params)

    is_solid <- coords$shape > 14
    has_fill <- coords$shape > 20

    stroke_size <- coords$stroke
    stroke_size[is.na(stroke_size)] <- 0

    lwd <- ifelse(is_solid & !has_fill, 0, stroke_size * .stroke / 2)

    foreground <- pointsGrob(
      x   = coords$x, y = coords$y,
      pch = coords$shape,
      gp  = gpar(
        col  = alpha(coords$colour, coords$alpha),
        fill = alpha(coords$fill,   coords$alpha),
        fontsize = coords$size * .pt,
        lwd  = lwd
      )
    )

    size <- coords$size * .pt + ifelse(is_solid, stroke_size * .stroke, 0)
    lwd  <- lwd + ifelse(is_solid, 0, stroke_size * .stroke)

    background <- pointsGrob(
      x = coords$x, y = coords$y,
      pch = coords$shape,
      gp  = gpar(
        col  = alpha(coords$stroke_colour, coords$alpha),
        fill = alpha(coords$stroke_colour, coords$alpha),
        lwd  = lwd,
        fontsize = size
      )
    )

    grob <- grobTree(
      background, foreground
    )
    grob$name <- grobName(grob, "outline_points")
    grob
  }
)




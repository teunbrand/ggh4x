# User function -----------------------------------------------------------

#' Aimed text
#'
#' Similar to `geom_text()`, this geom also generates text but places the
#' text at an angle so that the text seems aimed towards a point defined by
#' `[xend, yend]`.
#'
#' @inheritParams ggplot2::geom_text
#' @param flip_upsidedown A `logical(1)`. If `TRUE` (default), the
#'   angle of text placed at angles between 90 and 270 degrees is flipped so
#'   that it is more comfortable to read. If `FALSE`, will take calculated
#'   angles literally.
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#'   Cannot be jointly specified with `position`.
#'
#' @details The calculated angle is such that the text will be parallel to a
#'   line passing through the coordinates `[x, y]` and `[xend, yend]`.
#'   The calculated angle is added to the `angle` angle aesthetic, so that
#'   you can set text perpendicular to that line by setting `angle = 90`.
#'   These angles are calculated in absolute coordinates, meaning that resizing
#'   the plot will retain the same appearance.
#'
#' @return A ggplot2 `Layer` that can be added to a plot.
#' @export
#'
#' @note When using this geom to aim text at the centre of a polar plot, make
#'   sure the radius range does not have close to zero width.
#'
#' @eval fixup_docs(ggplot2:::rd_aesthetics("geom", "text_aimed"))
#' @examples
#' # Point all labels to upper right corner
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_text_aimed(aes(label = rownames(mtcars)),
#'                   xend = Inf, yend = Inf)
#'
#' # Point all labels to center of polar plot
#' ggplot(mpg, aes(manufacturer)) +
#'   geom_bar(width = 1, aes(fill = manufacturer), show.legend = FALSE) +
#'   geom_text_aimed(aes(label = manufacturer), hjust = 0,
#'                   stat = "count", nudge_y = 2) +
#'   scale_x_discrete(labels = NULL) +
#'   coord_polar()
geom_text_aimed <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0,
  nudge_y = 0,
  flip_upsidedown = TRUE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(
        "Specify either {.arg position} or {.arg nudge_x}/{.arg nudge_y}, \\
        not both."
      )
    }
    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomTextAimed,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list2(
      parse           = parse,
      check_overlap   = check_overlap,
      na.rm           = na.rm,
      flip_upsidedown = flip_upsidedown,
      ...
    )
  )
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
GeomTextAimed <- ggproto(
  "GeomTextAimed", GeomText,
  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, xend = -Inf, yend = -Inf,
    hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
    lineheight = 1.2
  ),
  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE,
                        flip_upsidedown = TRUE) {
    lab <- data$label
    if (parse) {
      if (!is.character(lab)) {
        cli::cli_abort("{.field label} must be a {.cls character} vector.")
      }
      out <- vector("expression", length(lab))
      for (i in seq_along(lab)) {
        expr <- parse(text = lab[[i]])
        out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
      }
      lab <- out
    }

    aim <- data[, c("xend", "yend")]
    colnames(aim) <- c("x", "y")
    data <- coord$transform(data, panel_params)
    aim  <- coord$transform(aim, panel_params)

    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    aimed_textGrob(
      lab,
      x = data$x, y = data$y,
      x0 = aim$x, y0 = aim$y,
      default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      flip_upsidedown = flip_upsidedown,
      check.overlap = check_overlap
    )

  },
  extra_params = c("na.rm", "flip_upsidedown")
)

# Grid functions ----------------------------------------------------------

#' @keywords internal
aimed_textGrob <- function(
  label,
  x = unit(0.5, "npc"), y = unit(0.5, 'npc'),
  x0 = unit(0, "npc"), y0 = unit(0, 'npc'),
  just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE,
  default.units = "npc", name = NULL, gp = gpar(), vp = NULL,
  flip_upsidedown = TRUE
) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(x0))
    x0 <- unit(x0, default.units)
  if (!is.unit(y0))
    y0 <- unit(y0, default.units)

  grob(label = label, x = x, y = y, x0 = x0, y0 = y0,
       just = just, hjust = hjust, vjust = vjust, rot = rot,
       check.overlap = check.overlap, name = name, gp = gp, vp = vp,
       flip_upsidedown = flip_upsidedown,
       cl = "aimed_text")
}

#' @keywords internal
#' @export
#' @method makeContent aimed_text
makeContent.aimed_text <- function(x) {
  # Convert from "native" units to absolute units
  x1 <- convertX(x$x,  "mm", valueOnly = TRUE)
  y1 <- convertY(x$y,  "mm", valueOnly = TRUE)
  x0 <- convertX(x$x0, "mm", valueOnly = TRUE)
  y0 <- convertY(x$y0, "mm", valueOnly = TRUE)

  # Calculate angle to point
  ang <- atan2(y1 - y0, x1 - x0)

  # Apply rotation
  x$rot <- (x$rot + ang * (180  / pi)) %% 360
  if (isTRUE(x$flip_upsidedown)) {
    upsidedown <- x$rot > 90 & x$rot < 270
    x$rot <- ifelse(upsidedown, x$rot + 180, x$rot) %% 360
    x$hjust <- ifelse(upsidedown, 1 - x$hjust, x$hjust)
  }

  # Remove superfluous grob elements
  x$flip_upsidedown <- NULL
  x$x0 <- NULL
  x$y0 <- NULL

  # Re-class as regular text grob
  class(x) <- c("text", class(x)[-1])
  return(x)
}

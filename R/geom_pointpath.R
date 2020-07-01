# Main function -----------------------------------------------------------

#' Point Paths
#'
#' The point path geom is used to make a scatterplot wherein the points are
#' connected with lines in some order. This geom intends to mimick the
#' \code{type = 'b'} style of base R line plots.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @export
#'
#' @section Aesthetics: \code{geom_pointpath()} understands the following
#'   aesthetics (required aesthetics are in bold):
#'   \itemize{\item{\strong{\code{x}}} \item{\strong{\code{y}}}
#'   \item{\code{alpha}} \item{\code{colour}} \item{\code{group}}
#'   \item{\code{shape}} \item{\code{size}} \item{\code{stroke}}
#'   \item{\code{linesize}} \item{\code{linetype}} \item{\code{mult}}}
#'
#' @details The \code{linesize} aesthetic can be interpreted as the \code{size}
#'   aesthetic for \code{geom_line()}. The \code{mult} is a numeric value to
#'   scale the proportion of gaps in the line around points.
#'
#'   While the need for this geom is not very aparrent, since it can be
#'   approximated in a variety of ways, the trick up its sleeve is that it
#'   dynamically adapts the interpoint segments so these  don't deform under
#'   different aspect ratios or device sizes.
#'
#' @return A \emph{Layer} ggproto object.
#'
#' @examples
#' ggplot(pressure, aes(temperature, pressure)) +
#'   geom_pointpath()
geom_pointpath <- function(
  mapping = NULL, data = NULL, stat = "identity",
  position = "identity", ..., na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPointPath,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
GeomPointPath <- ggplot2::ggproto(
  "GeomPointPath", ggplot2::GeomPoint,
  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
    # Default geom point behaviour
    pointgrob <- ggproto_parent(GeomPoint, self)$draw_panel(
      data, panel_params, coord, na.rm = na.rm
    )

    data$id <- seq_len(NROW(data))
    data <- data[order(data$group), , drop = FALSE]
    data <- coord_munch(coord, data, panel_params)

    data <- transform(data,
                      xend = c(tail(x, -1), NA),
                      yend = c(tail(y, -1), NA),
                      keep = c(group[-1] == head(group, -1), FALSE))
    data <- data[data$keep, ]

    ## Make custom grob class
    my_path <- grob(
      x0 = unit(data$x, "npc"), x1 = unit(data$xend, "npc"),
      y0 = unit(data$y, "npc"), y1 = unit(data$yend, "npc"),
      mult = (data$size * .pt + data$stroke * .stroke / 2) * data$mult,
      id = data$id,
      name = "pointpath",
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        lwd = (data$linesize * .pt),
        lty = data$linetype,
        lineend = "butt",
        linejoin = "round", linemitre = 10
      ),
      vp = NULL,
      ### Now this is the important bit:
      cl = if (coord$is_linear()) "gapsegments" else "gapsegmentschain"
    )

    ## Combine grobs
    .int$ggname(
      "geom_pointpath",
      grobTree(my_path, pointgrob)
    )
  },
  # Adding some defaults for lines and mult
  default_aes = ggplot2::aes(
    shape = 19, colour = "black", size = 1.5, fill = NA, alpha = NA,
    stroke = 0.5, linesize = 0.5, linetype = 1, mult = 0.5
  ),
  non_missing_aes = c("size", "colour")
)

# Draw methods ----------------------------------------

#' @title Calculate broken segments for a point-interrupted path
#' @export
#' @usage NULL
#' @format NULL
#' @noRd
#' @keywords internal
makeContext.gapsegmentschain <- function(x) {
  # A much more involved version of the pointpath drawing. It first deletes
  # segments where both the start- and endpoints are within distance of the
  # point. Then it seeks the cases where only the start or endpoint is within
  # distance and does a circle-line intersection to get the proper points.

  # Convert npcs to absolute units
  x0 <- convertX(x$x0, "mm", TRUE)
  y0 <- convertY(x$y0, "mm", TRUE)
  x1 <- convertX(x$x1, "mm", TRUE)
  y1 <- convertY(x$y1, "mm", TRUE)

  # Determine what connection the segments are forming
  id <- rle(x$id)
  start <- {end <- cumsum(id$lengths)} - id$lengths + 1
  start <- rep.int(start, id$lengths)
  end   <- rep.int(end, id$lengths)

  # What datapoints to keep and which to discard
  keep <- rep(TRUE, length(x0))

  # Calculate distances to start
  dist0_start <- sqrt((x0 - x0[start])^2 + (y0 - y0[start])^2)
  dist1_start <- sqrt((x1 - x0[start])^2 + (y1 - y0[start])^2)
  # Update keep
  keep <- keep & (dist0_start > x$mult | dist1_start > x$mult)
  # Keep track of edge cases
  left <- which(dist1_start > x$mult & !(dist0_start > x$mult))

  # Calculate distances to end
  dist0 <- sqrt((x0 - x1[end])^2 + (y0 - y1[end])^2)
  dist1 <- sqrt((x1 - x1[end])^2 + (y1 - y1[end])^2)
  # Update keep
  keep <- keep & (dist0 > x$mult | dist1 > x$mult)
  # Keep track of edge cases
  right <- which((dist0 > x$mult) != (dist1 > x$mult))

  # Edge cases that are both left and right need special care
  # Most likely unluckily munched pieces
  isect <- intersect(left, right)
  if (length(isect) > 0) {
    cut <- crop_segment_ends(x0[isect], x1[isect], y0[isect], y1[isect],
                             x$mult[isect])
    x0[isect] <- cut$x0
    x1[isect] <- cut$x1
    y0[isect] <- cut$y0
    y1[isect] <- cut$y1
    keep[isect] <- cut$keep
    left <- setdiff(left, isect)
    right <- setdiff(right, isect)
  }

  if (sum(keep) == 0) {
    return(zeroGrob())
  }

  # Handle left edgecases
  xy <- intersect_line_circle(
    x1 = x0[left], y1 = y0[left],
    x2 = x1[left], y2 = y1[left],
    cx = x0[start[left]], cy = y0[start[left]],
    r = x$mult[left], prio = 2L
  )
  x0[left] <- xy$x
  y0[left] <- xy$y

  # Handle right edgecases
  xy <- intersect_line_circle(
    x1 = x1[right], y1 = y1[right],
    x2 = x0[right], y2 = y0[right],
    cx = x1[end[right]], cy = y1[end[right]],
    r = x$mult[right], prio = 2L
  )
  x1[right] <- xy$x
  y1[right] <- xy$y

  # Apply keep to graphical parameters.
  x$gp[] <- lapply(x$gp, function(x) {
    if (length(x) == 1L) return(x) else x[keep]
  })

  # Supply new xy coordinates
  x$x0 <- unit(x0[keep], "mm")
  x$x1 <- unit(x1[keep], "mm")
  x$y0 <- unit(y0[keep], "mm")
  x$y1 <- unit(y1[keep], "mm")

  x$mult <- NULL
  x$id <- NULL
  class(x)[1] <- "segments"
  x
}

#' @title Calculate broken segments for a point-interrupted path
#' @export
#' @usage NULL
#' @format NULL
#' @noRd
#' @keywords internal
makeContext.gapsegments <- function(x) {
  # Convert npcs to absolute units
  x0 <- convertX(x$x0, "mm", TRUE)
  y0 <- convertY(x$y0, "mm", TRUE)
  x1 <- convertX(x$x1, "mm", TRUE)
  y1 <- convertY(x$y1, "mm", TRUE)

  cut <- crop_segment_ends(x0, x1, y0, y1, x$mult)

  # # Do trigonometry stuff
  # dx <- x1 - x0
  # dy <- y1 - y0
  # hyp <- sqrt(dx ^ 2 + dy ^ 2)
  # nudge_y <- (dy / hyp) * x$mult
  # nudge_x <- (dx / hyp) * x$mult
  #
  # # Calculate new positions
  # x0 <- x0 + nudge_x
  # x1 <- x1 - nudge_x
  # y0 <- y0 + nudge_y
  # y1 <- y1 - nudge_y

  # Filter overshoot
  # keep <- (sign(dx) == sign(x1 - x0)) & (sign(dy) == sign(y1 - y0))
  x$gp[] <- lapply(x$gp, function(x) {
    if (length(x) == 1L) return(x) else x[cut$keep]
  })

  # Supply new xy coordinates
  x$x0 <- unit(cut$x0[cut$keep], "mm")
  x$x1 <- unit(cut$x1[cut$keep], "mm")
  x$y0 <- unit(cut$y0[cut$keep], "mm")
  x$y1 <- unit(cut$y1[cut$keep], "mm")

  # Set to segments class
  x$mult <- NULL
  x$id <- NULL
  class(x)[1] <- "segments"
  x
}

# Helpers -----------------------------------------------------------------

#' Intersect a circle with a line
#'
#' Circle parameterised as (cx, cy) center point and r radius.
#' Line determined as (x1,y1) and (x2,y2).
#'
#' @param x1 The x-coordinate of the first point
#' @param y1 The y-coordinate of the first point
#' @param x2 The x-coordinate of the second point
#' @param y2 The y-coordinate of the second point
#' @param cx The x-coordinate of the circle centre
#' @param cy The y-coordinate of the circle centre
#' @param r  The radius of the circle
#' @param prio Either 1 or 2: return the intersection that is closer to the
#' first point (1) or closer to the second point (2).
#'
#' @details If no intersection is found, it should return NA.
#'
#' @return A list with elements x and y that are closest to the prio^th point.
#' @keywords internal
#' @noRd
intersect_line_circle <- function(x1, y1, x2, y2, cx, cy, r, prio = 1L) {
  # Center circle at 0,0
  x1 <- x1 - cx
  x2 <- x2 - cx
  y1 <- y1 - cy
  y2 <- y2 - cy

  # Based on https://mathworld.wolfram.com/Circle-LineIntersection.html
  # Calculate distances
  dx <- x2 - x1
  dy <- y2 - y1
  dr2 <- dx ^ 2 + dy ^ 2 # dr = sqrt(dx ^ 2 + dy ^ 2)
  # Calculate determinant
  det <- x1 * y2 - x2 * y1
  # Calculate discriminant
  dis <- r^2 * dr2 - det^2
  dis[dis < 0] <- NA # Below 0: No intersection, 0: tangent, >0: intersection
  # Square root for convenience
  dis <- sqrt(dis)

  # Determine intersection points
  x_1 <- (det * dy + sign(dy) * dx * dis) / dr2
  x_2 <- (det * dy - sign(dy) * dx * dis) / dr2
  y_1 <- (-det * dx + abs(dy) * dis) / dr2
  y_2 <- (-det * dx - abs(dy) * dis) / dr2

  # Calculate distances
  if (prio == 1L) {
    # x1y1 is prioritised
    dist1 <- sqrt((x1 - x_1)^2 + (y1 - y_1)^2)
    dist2 <- sqrt((x1 - x_2)^2 + (y1 - y_2)^2)
  } else {
    # x2y2 is prioritised
    dist1 <- sqrt((x2 - x_1)^2 + (y2 - y_1)^2)
    dist2 <- sqrt((x2 - x_2)^2 + (y2 - y_2)^2)
  }
  # Choose closest point
  test <- dist2 < dist1
  new_x <- ifelse(test, x_2, x_1) + cx
  new_y <- ifelse(test, y_2, y_1) + cy
  list(x = new_x, y = new_y)
}

crop_segment_ends <- function(x0, x1, y0, y1, r) {
  # Do trigonometry stuff
  dx <- x1 - x0
  dy <- y1 - y0
  hyp <- sqrt(dx ^ 2 + dy ^ 2)
  nudge_y <- (dy / hyp) * r
  nudge_x <- (dx / hyp) * r

  # Calculate new positions
  x0 <- x0 + nudge_x
  x1 <- x1 - nudge_x
  y0 <- y0 + nudge_y
  y1 <- y1 - nudge_y

  # Decide to keep
  keep <- (sign(dx) == sign(x1 - x0)) & (sign(dy) == sign(y1 - y0))
  list(x0 = x0, x1 = x1, y0 = y0, y1 = y1, keep = keep)
}

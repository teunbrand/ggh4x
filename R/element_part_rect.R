#' Partial rectangle theme element
#'
#' The \code{element_part_rect()} function draws sides of a rectangle as theme
#' elements. It can substitute \code{element_rect()} theme elements.
#'
#' @param side A \code{character} of length one containing any of \code{"t"},
#'   \code{"l"}, \code{"b"}, \code{"r"}. If these letters are present it will
#'   draw an edge at the top (t), left (l), bottom (b) or right (r)
#'   respectively. Including all or none of these letters will default to normal
#'   \code{element_rect()}.
#' @inheritParams ggplot2::element_rect
#'
#' @return An S3 object of class \code{element_part_rect}.
#' @export
#'
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point() +
#'   facet_grid(Species ~.) +
#'   theme(
#'     strip.background = element_part_rect(side = "tb", colour = "black"),
#'     panel.background = element_part_rect(side = "l", colour = "black")
#'   )
element_part_rect <- function(
  side = "tlbr",
  fill = NULL,
  colour = NULL,
  size = NULL,
  linetype = NULL,
  color = NULL,
  inherit.blank = FALSE
) {
  if (!is.null(color)) {
    colour <- color
  }
  all_sides <- grepl("(?=.*t)(?=.*l)(?=.*r)(?=.*b)", side, perl = TRUE)
  if (all_sides) {
    # Simplifies to regular rectangle
    return(
      element_rect(fill = fill, colour = colour, size = size,
                   linetype = linetype,
                   inherit.blank = inherit.blank)
    )
  }
  no_sides <- !grepl("t|l|r|b", side, perl = TRUE)
  if (no_sides) {
    # Also simplifies to regular rectangle, but with no colour
    return(
      element_rect(fill = fill, colour = NA, size = size, linetype = linetype,
                   inherit.blank = inherit.blank)
    )
  }

  structure(
    list(
      fill = fill,
      colour = colour,
      size = size,
      linetype = linetype,
      inherit.blank = inherit.blank,
      side = side
    ),
    class = c("element_part_rect", "element_rect", "element")
  )
}

#' @export
#' @noRd
element_grob.element_part_rect <- function(
  element,
  x = 0.5,
  y = 0.5,
  width = 1,
  height = 1,
  fill = NULL,
  colour = NULL,
  size = NULL,
  linetype = NULL,
  ...
) {
  gp <- gpar(
    lwd = if (length(size) == 0) NULL else size * .pt,
    col = colour, fill = fill, lty = linetype
  )
  element_gp <- element_gp <- gpar(
    lwd = if (length(element$size) == 0) NULL else element$size * .pt,
    col = element$colour, fill = element$fill, lty = element$linetype
  )

  for (i in names(gp)) {
    element_gp[[i]] <- gp[[i]]
  }

  partrectGrob(x, y, width, height, gp = element_gp,
               sides = element$side, ...)
}

partrectGrob <- function(
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  width = unit(1, "npc"),
  height = unit(1, "npc"),
  default.units = "npc",
  name = NULL,
  gp = gpar(),
  vp = NULL,
  sides = "tlbr"
) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(width))
    width <- unit(width, default.units)
  if (!is.unit(height))
    height <- unit(height, default.units)


  gp <- unclass(gp)

  rectfill <- rectGrob(
    x = x, y = y, width = width, height = height,
    default.units = default.units, name = "fillgrob", vp = vp,
    gp = do.call(gpar, within(gp, col <- NA))
  )

  # initialise
  n <- max(c(length(x), length(width), length(y), length(height)))
  x0 <- x1 <- y0 <- y1 <- NULL

  if (grepl("(?=.*t)", sides, perl = TRUE)) {
    tmp <- list(
      x0 = x - width * 0.5,
      x1 = x + width * 0.5,
      y0 = y + height * 0.5,
      y1 = y + height * 0.5
    )
    tmp <- lapply(tmp, rep, length.out =  n)
    exists <- !is.null(x0)
    if (exists) {
      x0 <- unit.c(x0, tmp$x0)
      x1 <- unit.c(x1, tmp$x1)
      y0 <- unit.c(y0, tmp$y0)
      y1 <- unit.c(y1, tmp$y1)
    } else {
      x0 <- tmp$x0
      x1 <- tmp$x1
      y0 <- tmp$y0
      y1 <- tmp$y1
    }
  }

  if (grepl("(?=.*b)", sides, perl = TRUE)) {
    tmp <- list(
      x0 = x - width * 0.5,
      x1 = x + width * 0.5,
      y0 = y - height * 0.5,
      y1 = y - height * 0.5
    )
    tmp <- lapply(tmp, rep, length.out =  n)
    exists <- !is.null(x0)
    if (exists) {
      x0 <- unit.c(x0, tmp$x0)
      x1 <- unit.c(x1, tmp$x1)
      y0 <- unit.c(y0, tmp$y0)
      y1 <- unit.c(y1, tmp$y1)
    } else {
      x0 <- tmp$x0
      x1 <- tmp$x1
      y0 <- tmp$y0
      y1 <- tmp$y1
    }
  }

  if (grepl("(?=.*l)", sides, perl = TRUE)) {
    tmp <- list(
      x0 = x - width * 0.5,
      x1 = x - width * 0.5,
      y0 = y - height * 0.5,
      y1 = y + height * 0.5
    )
    tmp <- lapply(tmp, rep, length.out =  n)
    exists <- !is.null(x0)
    if (exists) {
      x0 <- unit.c(x0, tmp$x0)
      x1 <- unit.c(x1, tmp$x1)
      y0 <- unit.c(y0, tmp$y0)
      y1 <- unit.c(y1, tmp$y1)
    } else {
      x0 <- tmp$x0
      x1 <- tmp$x1
      y0 <- tmp$y0
      y1 <- tmp$y1
    }
  }

  if (grepl("(?=.*r)", sides, perl = TRUE)) {
    tmp <- list(
      x0 = x + width * 0.5,
      x1 = x + width * 0.5,
      y0 = y - height * 0.5,
      y1 = y + height * 0.5
    )
    tmp <- lapply(tmp, rep, length.out =  n)
    exists <- !is.null(x0)
    if (exists) {
      x0 <- unit.c(x0, tmp$x0)
      x1 <- unit.c(x1, tmp$x1)
      y0 <- unit.c(y0, tmp$y0)
      y1 <- unit.c(y1, tmp$y1)
    } else {
      x0 <- tmp$x0
      x1 <- tmp$x1
      y0 <- tmp$y0
      y1 <- tmp$y1
    }
  }

  sidegrob <- segmentsGrob(
    x0 = x0,
    x1 = x1,
    y0 = y0,
    y1 = y1,
    name = "sidegrob",
    gp = do.call(gpar, within(gp, fill <- NA)),
    vp = vp
  )

  gt <- grobTree(rectfill, sidegrob, name = name, vp = vp)
}

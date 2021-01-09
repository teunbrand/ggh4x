#' String legend
#'
#' This type of legend shows colour and fill mappings as coloured text. It does
#' not draw keys as \code{guide_legend()} does.
#'
#' @inheritParams ggplot2::guide_legend
#' @param family A \code{character(1)} setting a font family for labels.
#' @param face A \code{character(1)} setting a font face for labels. One of the
#'   following: \code{"plain"}, \code{"italic"} or \code{"bold"},
#'   \code{"bold.italic"}.
#' @param size A \code{numeric(1)} setting the label text size in pts.
#' @param spacing.x,spacing.y,spacing A \code{numeric(1)} or \code{unit} for the
#'   spacing between label rows and columns. Internally defaults to half the
#'   size of the title.
#' @param default.units A \code{character(1)} indicating the default units to
#'   use if the \code{spacing.*} arguments are only given as numeric vectors.
#'
#' @return A \code{guide}, \code{stringlegend} S3 object.
#' @export
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = manufacturer))
#'
#' # String legend can be set in the `guides()` function
#' p + guides(colour = guide_stringlegend(ncol = 2))
#'
#' # The string legend can also be set as argument to the scale
#' p + scale_colour_viridis_d(guide = "stringlegend")
guide_stringlegend <- function(
  # Title
  title = waiver(),
  title.position = NULL,
  title.theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,

  # Label
  label.theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,
  family = NULL,
  face = NULL,
  size = NULL,
  spacing.x = NULL,
  spacing.y = NULL,
  spacing = NULL,
  default.units = "pt",

  # General
  direction = NULL,
  nrow = NULL,
  ncol = NULL,
  byrow = FALSE,
  reverse = FALSE,
  order = 0,
  ...
) {
  if (!is.null(spacing.x) & !is.unit(spacing.x)) {
    spacing.x <- unit(spacing.x, default.units)
  }
  if (!is.null(spacing.y) & !is.unit(spacing.y)) {
    spacing.y <- unit(spacing.y, default.units)
  }
  if (!is.null(spacing) & !is.unit(spacing)) {
    spacing <- unit(spacing, default.units)
  }
  spacing.x <- spacing.x %||% spacing
  spacing.y <- spacing.y %||% spacing

  structure(
    list(
      # Title
      title = title,
      title.position = title.position,
      title.theme = title.theme,
      title.hjust = title.hjust,
      title.vjust = title.vjust,

      # Label
      label.theme = label.theme,
      label.hjust = label.hjust,
      label.vjust = label.vjust,
      label.family = family,
      label.face = face,
      label.size = size,
      label.spacing.x = spacing.x,
      label.spacing.y = spacing.y,

      # General
      direction = direction,
      nrow = nrow,
      ncol = ncol,
      byrow = byrow,
      reverse = reverse,
      order = order,

      # Parameters
      available_aes = c("colour", "fill"),
      ...,
      name = "stringlegend"
    ),
    class = c("guide", "stringlegend", "legend")
  )
}

#' @method guide_gengrob stringlegend
#' @export
#' @noRd
guide_gengrob.stringlegend <- function(guide, theme) {

  # Layout
  nbreak <- nrow(guide$key)
  if (!is.null(guide$nrow) && !is.null(guide$ncol) &&
      guide$nrow * guide$ncol < nbreak) {
    stop("`nrow` * `ncol` need to be larger than the number of breaks.")
  }

  if (is.null(guide$nrow) && is.null(guide$ncol)) {
    if (guide$direction == "horizontal") {
      guide$nrow <- ceiling(nbreak / 5)
    } else {
      guide$ncol <- ceiling(nbreak / 20)
    }
  }

  legend.nrow <- guide$nrow %||% ceiling(nbreak / guide$ncol)
  legend.ncol <- guide$ncol %||% ceiling(nbreak / guide$nrow)
  legend.dim  <- c(legend.nrow, legend.ncol)

  # Render title
  title <- render_legend_title(guide, theme)

  default_gap <- 0.5 * unit(title$fontsize, "pt")
  # Gap between title and labels
  hgap <- width_cm(theme$legend.spacing.x  %||% default_gap)
  vgap <- height_cm(theme$legend.spacing.y %||% default_gap)
  # Gap between label rows and columns
  xgap <- width_cm(guide$label.spacing.x %||% default_gap)
  ygap <- height_cm(guide$label.spacing.y %||% default_gap)

  # Render labels
  labels <- render_stringlegend_labels(guide, theme,
                                       legend.dim, nbreak)
  # Setup layout
  if (guide$byrow) {
    vps <- .int$new_data_frame(list(
      R = ceiling(seq(nbreak) / legend.ncol),
      C = (seq(nbreak) - 1) %% legend.ncol + 1
    ))
  } else {
    vps <- arrayInd(seq(nbreak), legend.dim)
    vps <- .int$new_data_frame(list(R = vps[, 1], C = vps[, 2]))
  }
  vps <- transform(vps, label.row = R * 2 - 1, label.col = C * 2 - 1)

  widths  <- head(.int$interleave(labels$width,  xgap), -1)
  heights <- head(.int$interleave(labels$height, ygap), -1)

  # Place title in layout
  switch(
    guide$title.position,
    "top" = {
      widths  <- c(widths, max(0, title$width - sum(widths)))
      heights <- c(title$height, vgap, heights)
      vps <- transform(vps, label.row = label.row + 2)
      vps.title.row <- 1; vps.title.col <- 1:length(widths)
    },
    "bottom" = {
      widths  <- c(widths, max(0, title$width - sum(widths)))
      heights <- c(heights, vgap, title$height)
      vps.title.row <- length(heights); vps.title.col <- 1:length(widths)
    },
    "left" = {
      widths  <- c(title$width, hgap, widths)
      heights <- c(heights, max(0, title$height - sum(heights)))
      vps <- transform(vps, label.col = label.col + 2)
      vps.title.row <- 1:length(heights); vps.title.col <- 1
    },
    "right" = {
      widths  <- c(widths, hgap, title$width)
      heights <- c(heights, max(0, title$height - sum(heights)))
      vps.title.row <- 1:length(heights); vps.title.col <- length(widths)
    }
  )

  background <- element_render(theme, "legend.background")
  padding <- convertUnit(theme$legend.margin %||% margin(),
                         "cm", valueOnly = TRUE)
  widths  <- c(padding[4], widths,  padding[2])
  heights <- c(padding[1], heights, padding[3])

  # Create gtable
  gt <- gtable(widths  = unit(widths, "cm"),
               heights = unit(heights, "cm"))
  gt <- gtable_add_grob(gt, background, name = "background", clip = "off",
                        t = 1, r = -1, b = -1, l = 1)
  gt <- gtable_add_grob(
    gt,
    .int$justify_grobs(
      title$grob, hjust = title$hjust, vjust = title$vjust,
      int_angle = title$theme$angle, debug = title$theme$debug
    ),
    name = "title", clip = "off",
    t = 1 + min(vps.title.row), r = 1 + max(vps.title.col),
    b = 1 + max(vps.title.row), l = 1 + min(vps.title.col)
  )
  gt <- gtable_add_grob(
    gt,
    .int$justify_grobs(
      labels$grob, hjust = labels$hjust, vjust = labels$vjust,
      int_angle = labels$theme$angle, debug = labels$theme$debug
    ),
    name = paste("label", vps$label.row, vps$label.col, sep = "-"),
    clip = "off",
    t = 1 + vps$label.row, r = 1 + vps$label.col,
    b = 1 + vps$label.row, l = 1 + vps$label.col
  )
  gt
}

#' @keywords internal
render_stringlegend_labels <- function(guide, theme, dim, n) {

  label.theme <- guide$label.theme %||% calc_element("legend.text", theme)
  key_nrow <- nrow(guide$key)

  if (is.null(guide$key$.label)) {
    grob <- rep(list(zeroGrob()), key_nrow)
    hjust <- vjust <- NULL
  } else {
    just_defaults <- list(hjust = 0, vjust = 0.5)
    # Exception for expression
    if (just_defaults$hjust == 0 && any(is.expression(guide$key$.label))) {
      just_defaults$hjust <- 1
    }
    if (is.null(guide$label.theme$hjust) && is.null(theme$legend.text$hjust)) {
      label.theme$hjust <- NULL
    }
    if (is.null(guide$label.theme$vjust) && is.null(theme$legend.text$vjust)) {
      label.theme$vjust <- NULL
    }

    hjust <- guide$label.hjust %||% theme$legend.text.align %||%
      label.theme$hjust %||% just_defaults$hjust
    vjust <- guide$label.vjust %||% label.theme$vjust %||% just_defaults$vjust

    colour <- guide$key$fill %||% guide$key$colour %||% "black"
    face   <- guide$label.face %||% label.theme$face
    family <- guide$label.family %||% label.theme$family
    size   <- guide$label.size %||% label.theme$size

    grob <- lapply(seq_len(key_nrow), function(i, ...) {
      g <- element_grob(
        element = label.theme,
        label = guide$key$.label[[i]],
        colour = colour[[i]],
        face = face,
        family = family,
        size = size,
        hjust = hjust,
        vjust = vjust,
        margin_x = TRUE,
        margin_y = TRUE
      )
      g$name <- grobName(g, "guide.label")
      g
    })
  }

  widths  <- width_cm(grob)
  heights <- height_cm(grob)
  blanks  <- rep(0, prod(dim) - n)

  widths <- apply(
    matrix(c(widths, blanks), dim[1], dim[2], byrow = guide$byrow), 2, max
  )

  heights <- apply(
    matrix(c(heights, blanks), dim[1], dim[2], byrow = guide$byrow), 1, max
  )

  list(grob = grob,
       width = widths,
       height = heights,
       hjust = hjust,
       vjust = vjust,
       theme = label.theme)
}

#' @keywords internal
render_legend_title <- function(guide, theme) {
  title.theme <- guide$title.theme %||% calc_element("legend.title", theme)
  hjust <- guide$title.hjust %||% theme$legend.title.align %||%
    title.theme$hjust %||% 0
  vjust <- guide$title.vjust %||% title.theme$vjust %||% 0.5

  grob <- element_grob(
    title.theme,
    label = guide$title,
    hjust = hjust,
    vjust = vjust,
    margin_x = TRUE,
    margin_y = TRUE
  )
  grob$name <- grobName(grob, "guide.title")

  width  <- width_cm(grob)
  height <- height_cm(grob)
  fontsize <- title.theme$size %||%
    calc_element("legend.title", theme)$xize %||%
    calc_element("text", theme)$size %||% 11

  list(grob = grob,
       width = width,
       height = height,
       fontsize = fontsize,
       hjust = hjust,
       vjust = vjust,
       theme = title.theme)
}


# Helpers -----------------------------------------------------------------

# Guard against R CMD check compaints
utils::globalVariables(c("C", "R", "label.row", "label.col"))

# `height_cm()` and `width_cm()` are copies of `ggplot2:::height_cm()`
# and `ggplot2:::width_cm()`.

#' @keywords internal
height_cm <- function(x) {
  if (is.grob(x)) {
    convertHeight(grobHeight(x), "cm", TRUE)
  } else if (is.unit(x)) {
    convertHeight(x, "cm", TRUE)
  } else if (is.list(x)) {
    vapply(x, height_cm, numeric(1))
  } else {
    rlang::abort("Unknown input")
  }
}

#' @keywords internal
width_cm <- function(x) {
  if (is.grob(x)) {
    convertWidth(grobWidth(x), "cm", TRUE)
  } else if (is.unit(x)) {
    convertWidth(x, "cm", TRUE)
  } else if (is.list(x)) {
    vapply(x, width_cm, numeric(1))
  } else {
    rlang::abort("Unknown input")
  }
}

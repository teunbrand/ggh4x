# User function -----------------------------------------------------------

#' Scale bar axis guide
#'
#' `r lifecycle::badge("questioning")`
#' This axis guides draws a scale bar to indicate a distance rather than
#' mark absolute values.
#' The function is questioned due to
#' a possible migration of guide functions after ggplot2 releases a new guide
#' system.
#'
#' @inheritParams guide_axis_truncated
#' @param size A `numeric(1)` for a distance to indicate, in data units. If
#'   `NULL` (default), the median distance between breaks is taken.
#' @param label A `character(1)` to use as scale bar label. If `NULL` (default),
#'   the `size` distance is fed to the scale's labeller.
#' @param just A `numeric(1)` between 0 and 1 to indicate where the scalebar
#'   should be drawn relative to the plot panel. The default (1), places is
#'   at the right or at the top.
#'
#' @details It is discouraged to use this guide in combination with a scale
#'   transformation.
#'
#' @section Theme elements:
#'   This axis guide has an alternative understanding of the following theme
#'   elements:
#'
#'   \describe{
#'     \item{`axis.ticks.*`}{
#'       An `element_line()` to draw the scale bar itself.
#'     }
#'     \item{`axis.ticks.length.*`}{
#'       A `unit()` indicating how far the scale bar should be placed from the
#'       plot panel. Can be a negative unit to place the scale bar inside the
#'       plot panel.
#'     }
#'     \item{`axis.text.*`}{
#'       The `hjust` and `vjust` parameters are used to justify the text along
#'       the scale bar, instead of along itself, in the `x` and `y` directions
#'       respectively.
#'     }
#'   }
#'
#' @return A `axis_scalebar` guide class object.
#' @family axis-guides
#' @export
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Guide as secondary axis
#' p + guides(x.sec = "axis_scalebar")
#'
#' # Customising size and label
#' p + guides(x.sec = guide_axis_scalebar(size = 0.5, label = "0.5 litre"))
#'
#' # Placing the scale bar on top of the plotting panel
#' p + guides(x.sec = guide_axis_scalebar(just = 0.95)) +
#'   theme(axis.ticks.length.x.top = unit(-2, "lines"))
#'
#' # Adding arrows through the axis.ticks theme element
#' p + guides(y.sec = guide_axis_scalebar(size = 10, label = "10\nmpg")) +
#'   theme(axis.ticks.y.right = element_line(arrow = arrow(ends = "both")))
guide_axis_scalebar <- function(
  title    = waiver(),
  size     = NULL,
  label    = NULL,
  colour   = NULL,
  color    = NULL,
  just     = 1,
  position = waiver()
) {
  colour <- color %||% colour
  structure(
    list(
      title    = title,
      size     = size,
      label    = label,
      colour   = colour,
      position = position,
      just     = just,
      available_aes = c("x", "y"),
      name     = "axis"
    ),
    class = c("guide", "axis_scalebar", "axis_ggh4x", "axis")
  )
}

# Internals ---------------------------------------------------------------

#' @noRd
#' @export
guide_train.axis_scalebar <- function(guide, scale, aesthetic = NULL) {

  if (!is.null(scale$scale$trans) && scale$scale$trans$name != "identity") {
    warn("Cannot draw appropriate scale bar for non-linear transformations.")
  }

  aesthetic <- aesthetic %||% scale$aesthetics[1]
  breaks    <- scale$get_breaks()

  empty_ticks <- data_frame0(
    aesthetic = numeric(0),
    .value    = numeric(0),
    .label    = character(0)
  )
  names(empty_ticks) <- c(aesthetic, ".value", ".label")

  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    cli::cli_warn(c(
      "{.fn guide_axis_scalebar} needs appropriate scales:",
      i = "Use one of {.or {.field {guide$available_aes}}}."
    ))
    guide$key <- empty_ticks
  } else if (length(breaks) == 0) {
    guide$key <- empty_ticks
  } else {
    breaks <- if (scale$is_discrete()) scale$map(breaks) else breaks
    breaks <- guide$size %||% median(diff(breaks), na.rm = TRUE)
    limits <- scale$dimension()
    mapped_breaks <- breaks[1] + limits[1]


    ticks <- new_data_frame(setNames(list(mapped_breaks), aesthetic))
    ticks$.value <- breaks[1]
    ticks$.label <- guide$label[1] %||% scale$get_labels(breaks)
    ticks <- ticks[is.finite(ticks[[aesthetic]]), ]

    guide$key <- ticks
  }

  guide$name <- paste0(guide$name, "_", aesthetic)
  guide$hash <- with(guide, hash(list(title, key$.value, key$.label, name)))
  guide
}

#' @noRd
#' @export
guide_gengrob.axis_scalebar <- function(guide, theme) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]
  draw_axis_scalebar(
    key           = guide$key,
    axis_position = guide$position,
    theme         = theme,
    colour        = guide$colour,
    offset        = guide$offset,
    just          = guide$just
  )
}

draw_axis_scalebar <- function(
  key,
  axis_position,
  theme,
  colour,
  offset,
  just = 1
) {
  axis_position <- match.arg(substr(axis_position, 1, 1), c("t", "b", "r", "l"))
  elements <- build_axis_elements(axis_position, theme = theme, colour = colour)
  params <- setup_axis_params(axis_position)
  params$just <- just
  line_grob <- build_axis_line(elements$line, params)

  if (nrow(key) == 0) {
    out <- gTree(
      children = gList(line_grob),
      width    = grobWidth(line_grob),
      height   = grobHeight(line_grob),
      cl = "absoluteGrob"
    )
    return(out)
  }

  label_grobs <- build_scalebar_labels(
    elements, key = key, params = params
  )

  sizes <- unit.c(elements$tick_length)
  tick_grob <- build_scalebar_bar(elements$ticks, sizes,
                                  key[[params$aes]], params)
  elements$tick_length <- max(sizes)
  assemble_axis_grobs(
    ticks = tick_grob, labels = label_grobs,
    lines = line_grob, elements = elements,
    params = params
  )

}

# Helpers -----------------------------------------------------------------

build_scalebar_labels <- function(
  elements, key, dodge = 1, check.overlap = FALSE, params
) {
  aes <- params$aes
  pos <- key[[aes]]
  text_just <- elements$label[c("hjust", "vjust")]
  text_just <- if (aes == "x") text_just$hjust[1] else text_just$vjust[1]
  text_just <- text_just %||% 0.5
  key[[params$aes]] <- (1 - pos) * params$just + text_just * pos

  build_axis_labels(elements, key, dodge, check.overlap, params)
}

build_scalebar_bar <- function(
  element, length, position, params
) {
  pos <- unit(rep(params$pos + (params$tick_dir * 1), 2), "npc")

  position <- (1 - position) * params$just + c(0, 1) * position

  if (!is.unit(position)) {
    position <- unit(position, "native")
  }

  args <- setNames(
    list(element, position, pos, 2),
    c("element", params$aes, params$non_aes, "id.lengths")
  )

  do.call(element_grob, args)
}



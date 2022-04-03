#' @include ggh4x_extensions.R
NULL

# Constructor -------------------------------------------------------------

#' Cartesian coordinates with interior axes
#'
#' This coordinate system places the plot axes at interior positions. Other
#' than this, it behaves like [`coord_cartesian()`][ggplot2::coord_cartesian] or
#' [`coord_fixed()`][ggplot2::coord_fixed] (the latter if the `ratio` argument
#' is set).
#'
#' @inheritParams ggplot2::coord_cartesian
#' @inheritParams ggplot2::coord_fixed
#' @param xintercept,yintercept A `numeric(1)` for the positions where the
#'   orthogonal axes should be placed. If these are outside the bounds of the
#'   limits, the axes are placed to the nearest extreme.
#' @param labels_inside A `logical(1)` when labels should be placed inside the
#'   panel along the axes (`TRUE`) or placed outside the panel
#'   (`FALSE`, default).
#' @param ratio Either `NULL`, or a `numeric(1)` for a fixed aspect ratio,
#'   expressed as `y / x`.
#'
#' @return A `CoordAxesInside` object, which can be added to a plot.
#' @export
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(scale(displ), scale(hwy))) +
#'   geom_point() +
#'   theme(axis.line = element_line())
#'
#' # By default, axis text is still placed outside the panel
#' p + coord_axes_inside()
#'
#' # However, this can simply be changed
#' p + coord_axes_inside(labels_inside = TRUE)
#'
#' # The place where the axes meet can be changed
#' p + coord_axes_inside(xintercept = 1, yintercept = -1)
#'
#' # Axes snap to the nearest limit when out-of-bounds
#' p + coord_axes_inside(xintercept = -5, yintercept = Inf, clip = "off")
#'
#' # Can be combined with other non-default axes
#' p + guides(x = "axis_minor") +
#'   coord_axes_inside()
coord_axes_inside <- function(
  xlim = NULL, ylim = NULL,
  xintercept = 0, yintercept = 0,
  labels_inside = FALSE,
  ratio  = NULL,
  expand = TRUE, default = FALSE,
  clip = "on"
) {

  outer_axes <- theme(
    axis.line.x.bottom  = element_blank(),
    axis.line.x.top     = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.ticks.x.top    = element_blank(),
    axis.line.y.left    = element_blank(),
    axis.line.y.right   = element_blank(),
    axis.ticks.y.left   = element_blank(),
    axis.ticks.y.right  = element_blank()
  )
  if (isTRUE(labels_inside)) {
    outer_axes <- outer_axes + theme(
      axis.text.x.bottom = element_blank(),
      axis.text.x.top    = element_blank(),
      axis.text.y.left   = element_blank(),
      axis.text.y.right  = element_blank(),
      axis.ticks.length.x.bottom = unit(0, "pt"),
      axis.ticks.length.x.top    = unit(0, "pt"),
      axis.ticks.length.y.left   = unit(0, "pt"),
      axis.ticks.length.y.right  = unit(0, "pt")
    )
    inner_axes <- theme()
  } else {
    inner_axes <- theme(
      axis.text.x.bottom = element_blank(),
      axis.text.x.top    = element_blank(),
      axis.text.y.left   = element_blank(),
      axis.text.y.right  = element_blank()
    )
  }

  ggproto(
    NULL, CoordAxesInside,
    limits  = list(x = xlim, y = ylim),
    expand  = expand,
    default = default,
    clip    = clip,
    ratio   = ratio,
    origin  = .int$new_data_frame(
      list(x = xintercept[1], y = yintercept[1])
    ),
    outer_axes = outer_axes,
    inner_axes = inner_axes
  )
}

# ggproto class -----------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
CoordAxesInside <- ggproto(
  "CoordAxesInside", CoordCartesian,
  render_axis_h = function(self, panel_params, theme) {
    CoordCartesian$render_axis_h(panel_params, theme + self$outer_axes)
  },
  render_axis_v = function(self, panel_params, theme) {
    CoordCartesian$render_axis_v(panel_params, theme + self$outer_axes)
  },
  render_bg = function(self, panel_params, theme) {
    theme     <- theme + self$inner_axes
    grid_grob <- CoordCartesian$render_bg(panel_params, theme)
    xaxes     <- CoordCartesian$render_axis_h(panel_params, theme)
    yaxes     <- CoordCartesian$render_axis_v(panel_params, theme)
    origin    <- self$transform(self$origin, panel_params)

    x <- unit(oob_squish(origin$x[1]), "npc")
    y <- unit(oob_squish(origin$y[1]), "npc")

    xaxes$bottom <- replace_vp_coord(xaxes$bottom, "y", y)
    xaxes$top    <- replace_vp_coord(xaxes$top,    "y", y)
    yaxes$left   <- replace_vp_coord(yaxes$left,   "x", x)
    yaxes$right  <- replace_vp_coord(yaxes$right,  "x", x)

    grobTree(
      "grid"   = grid_grob,
      "axis-b" = xaxes$bottom,
      "axis-t" = xaxes$top,
      "axis-l" = yaxes$left,
      "axis-r" = yaxes$right
    )
  },

  is_free = function(self) is.null(self$ratio),

  aspect = function(self, ranges) {
    if (is.null(self$ratio)) return(NULL)
    diff(ranges$y.range) / diff(ranges$x.range) * self$ratio
  }
)

# Helpers -----------------------------------------------------------------

replace_vp_coord <- function(grob, param = "x", value) {
  if (is.null(grob$vp)) {
    return(grob)
  }
  grob$vp[[param]] <- value
  grob
}

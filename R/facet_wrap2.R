# Constructor -------------------------------------------------------------

## External ---------------------------------------------------------------

#' Extended wrapped facets
#'
#' This function behaves like [ggplot2::facet_wrap()], but has a few
#' extra options on axis drawing when scales are fixed.
#'
#' @inheritParams ggplot2::facet_wrap
#' @param scales A `character(1)` or `logical(1)` whether scales are shared
#'   across facets or allowed to vary. One of the following:
#'   \describe{
#'     \item{`"fixed"` or `FALSE`}{Scales are shared across all facets
#'     (default).}
#'     \item{`"free_x"`}{x-scales are allowed to vary.}
#'     \item{`"free_y"`}{y-scales are allowed to vary.}
#'     \item{`"free"` or `TRUE`}{Both scales can vary}
#'   }
#' @param axes A `character(1)` or `logical(1)` where axes should be drawn. One
#'   of the following:
#'   \describe{
#'     \item{`"margins"` or `FALSE`}{Only draw axes at the outer margins
#'       (default).}
#'     \item{`"x"`}{Draw axes at the outer margins and all inner x-axes too.}
#'     \item{`"y"`}{Draw axes at the outer margins and all inner y-axes too.}
#'     \item{`"all"` or `TRUE`}{Draw the axes for every panel.}
#'   }
#' @param remove_labels A `character(1)` or `logical(1)` determining whether
#'   axis text is displayed at inner panels. One of the following:
#'   \describe{
#'     \item{`"none"` or `FALSE`}{Display axis text at all axes (default).}
#'     \item{`"x"`}{Display axis text at outer margins and all inner y-axes.}
#'     \item{`"y"`}{Display axis text at outer margins and all inner x-axes.}
#'     \item{`"all"` or `TRUE`}{Only display axis text at the outer margins.}
#'   }
#' @param trim_blank A `logical(1)`. When `TRUE` (default), does not draw rows
#'   and columns containing no panels. When `FALSE`, the `nrow` and `ncol`
#'   arguments are taken literally, even when there are more than needed to
#'   fit all panels.
#' @param strip An object created by a call to a strip function, such as
#'   [`strip_vanilla`][strip_vanilla()].
#'
#' @return A `Facet` ggproto object that can be added to a plot.
#' @export
#' @family facetting functions
#' @md
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
#'
#' # Repeat all axes for every facet
#' p + facet_wrap2(vars(class), axes = "all")
#'
#' # Repeat only y-axes
#' p + facet_wrap2(vars(class), axes = "y")
#'
#' # Repeat axes without labels
#' p + facet_wrap2(vars(class), axes = "all", remove_labels = "all")
#'
#' # Repeat axes without x-axis labels
#' p + facet_wrap2(vars(class), axes = "all", remove_labels = "x")
facet_wrap2 <- function(
  facets, nrow = NULL, ncol = NULL,
  scales = "fixed", axes = "margins",
  remove_labels = "none",
  shrink = TRUE, labeller = "label_value",
  as.table = TRUE, drop = TRUE,
  dir = "h", strip.position = "top",
  trim_blank = TRUE,
  strip = strip_vanilla()
) {
  new_wrap_facets(
    facets, nrow, ncol,
    scales, axes, remove_labels,
    shrink, labeller,
    as.table, drop, dir,
    strip.position, strip,
    trim_blank,
    super = FacetWrap2
  )
}

## Internal ---------------------------------------------------------------

new_wrap_facets <- function(
  facets, nrow, ncol,
  scales, axes, rmlab,
  shrink, labeller,
  as.table, drop, dir,
  strip.position, strip,
  trim_blank,
  params = list(), super = FacetWrap2
) {
  prototype <- facet_wrap(
    facets = facets, nrow = nrow, ncol = ncol,
    scales = scales, shrink = shrink, labeller = labeller,
    as.table = as.table, drop = drop, dir = dir,
    strip.position = strip.position
  )$params
  axes  <- .match_facet_arg(axes,   c("margins", "x", "y", "all"))
  rmlab <- .match_facet_arg(rmlab,  c("none", "x", "y", "all"))
  strip <- assert_strip(strip)

  dim <- if (trim_blank) NULL else c(prototype$nrow %||% NA, prototype$ncol %||% NA)

  # Make list of parameters
  params <- c(prototype, params, list(dim = dim, axes = axes, rmlab = rmlab))

  ggproto(
    NULL, super,
    shrink = shrink,
    strip = strip,
    params = params
  )
}

# ggproto -----------------------------------------------------------------

# This is just like FacetWrap, just the 'draw_panels()' method has been
# refactored for my own understanding. The draw_panels() method was just a beast
# of a function to deal with, so I chunked it up into smaller portions with
# dedicated tasks.
#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
FacetWrap2 <- ggproto(
  "FacetWrap2", FacetWrap,
  setup_aspect_ratio = function(coord, free, theme, ranges) {
    # Rules for aspect ratio
    aspect_ratio <- theme$aspect.ratio
    if (is.null(aspect_ratio) && !free$x && !free$y) {
      aspect_ratio <- coord$aspect(ranges[[1]])
    }
    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      attr(aspect_ratio, "respect") <- FALSE
    } else {
      attr(aspect_ratio, "respect") <- TRUE
    }

    aspect_ratio
  },
  setup_layout = function(layout, coord, params) {
    # Essentially just switch x/y when coord is flipped.
    # Shouldn't this be the task of the coord (ideally)?
    if (inherits(coord, "CoordFlip")) {
      if (params$free$x) {
        layout$SCALE_X <- seq_len(nrow(layout))
      } else {
        layout$SCALE_X <- 1L
      }
      if (params$free$y) {
        layout$SCALE_Y <- seq_len(nrow(layout))
      } else {
        layout$SCALE_Y <- 1L
      }
    }

    layout
  },
  setup_panel_table = function(self, panels, layout, #empty, position,
                               theme, coord, ranges, params) {
    ncol <- params$dim[2] %||% max(c(layout$.LEFT, layout$.RIGHT))
    nrow <- params$dim[1] %||% max(c(layout$.TOP,  layout$.BOTTOM))

    aspect <- self$setup_aspect_ratio(coord, params$free, theme, ranges)

    respect <- params$respect %||% attr(aspect, "respect") %||% FALSE
    widths  <- params$widths  %||% unit(1, "null")
    heights <- params$heights %||% unit(abs(aspect %||% 1), "null")

    panel_table <- gtable(
      widths  = rep(widths,  length.out = ncol),
      heights = rep(heights, length.out = nrow),
      respect = respect
    )
    panel_table <- gtable_add_grob(
      panel_table, panels,
      t = layout$.TOP,  b = layout$.BOTTOM,
      l = layout$.LEFT, r = layout$.RIGHT,
      z = 1, clip = coord$clip, name = paste0("panel-", seq_along(panels))
    )
    panel_table <- gtable_add_col_space(
      panel_table, calc_element("panel.spacing.x", theme)
    )
    panel_table <- gtable_add_row_space(
      panel_table, calc_element("panel.spacing.y", theme)
    )
    panel_table
  },
  setup_axes = function(axes, layout, params, theme) {

    # Takes rendered axes and picks the right ones for the facets
    nrow  <- max(layout$ROW)
    ncol  <- max(layout$COL)
    index <- cbind(layout$ROW, layout$COL)
    empties <- matrix(TRUE, nrow, ncol)
    empties[index] <- FALSE

    # Initialise empty axes
    top <- bottom <- left <- right <- matrix(list(zeroGrob()), nrow, ncol)
    # Fill axes by scale ID
    top[index]    <- axes$x$top[layout$SCALE_X]
    bottom[index] <- axes$x$bottom[layout$SCALE_X]
    left[index]   <- axes$y$left[layout$SCALE_Y]
    right[index]  <- axes$y$right[layout$SCALE_Y]

    repeat_x <- params$free$x | params$axes$x
    repeat_y <- params$free$y | params$axes$y

    # Remove redundant axes if they don't need to be repeated
    if (!repeat_x) {
      top[-1, ] <- list(zeroGrob())
      bottom[-nrow, ] <- list(zeroGrob())
    }
    if (!repeat_y) {
      left[, -1] <- list(zeroGrob())
      right[, -ncol] <- list(zeroGrob())
    }

    # Purge labels from redundant axes
    if (params$axes$x && params$rmlab$x && !params$free$x) {
      top[-1, ] <- lapply(top[-1, ], purge_guide_labels)
      bottom[-nrow, ] <- lapply(bottom[-nrow, ], purge_guide_labels)
    }
    if (params$axes$y && params$rmlab$y && !params$free$y) {
      left[, -1] <- lapply(left[, -1], purge_guide_labels)
      right[, -ncol] <- lapply(right[, -ncol], purge_guide_labels)
    }

    # Measure after deletion of axes, but before placing them back.
    # This assures that the gap between dangling panels isn't too big.
    measurements <- .measure_axes(list(top = top, bottom = bottom,
                                       left = left, right = right))

    if (!any(empties)) {
      out <- list(
        top = top,
        bottom = bottom,
        left = left,
        right = right,
        measurements = measurements
      )
      return(out)
    }


    # Place back necessary axes that were removed prior, e.g. when there are
    # missing panels so the marginal axes should not be the most extreme one.
    # browser()

    row_ind <- row(empties)
    col_ind <- col(empties)
    inside <- (theme$strip.placement %||% "inside") == "inside"

    bottom_empty <- apply(empties, 2, function(x) c(diff(x) == 1, FALSE))
    if (any(bottom_empty)) {
      pos <- which(bottom_empty, arr.ind = TRUE)
      loc <- data_frame0(ROW = pos[, 1], COL = pos[, 2])
      panels <- vec_match(loc, layout[, c("ROW", "COL")])
      replace <- axes$x$bottom[panels]
      if (params$strip.position == "bottom" && !inside &&
          any(!vapply(replace, is.zero, logical(1))) && !params$free$x) {
        cli::cli_warn(
          "Suppressing axis rendering when {.code strip.position = \"bottom\"} \\
          and {.code strip.placement == \"outside\"}"
        )
      } else {
        bottom[pos] <- replace
      }
    }

    top_empty <- apply(empties, 2, function(x) c(FALSE, diff(x) == -1))
    if (any(top_empty)) {
      pos <- which(top_empty, arr.ind = TRUE)
      loc <- data_frame0(ROW = pos[, 1], COL = pos[, 2])
      panels <- vec_match(loc, layout[, c("ROW", "COL")])
      replace <- axes$x$top[panels]
      if (params$strip.position == "top" && !inside &&
          any(!vapply(replace, is.zero, logical(1))) && !params$free$x) {
        cli::cli_warn(
          "Suppressing axis rendering when {.code strip.position = \"top\"} \\
          and {.code strip.placement == \"outside\"}"
        )
      } else {
        top[pos] <- replace
      }
    }

    right_empty <- t(apply(empties, 1, function(x) c(diff(x) == 1, FALSE)))
    if (any(right_empty)) {
      pos <- which(right_empty, arr.ind = TRUE)
      loc <- data_frame0(ROW = pos[, 1], COL = pos[, 2])
      panels <- vec_match(loc, layout[, c("ROW", "COL")])
      replace <- axes$y$right[panels]
      if (params$strip.position == "right" && !inside &&
          any(!vapply(replace, is.zero, logical(1))) && !params$free$y) {
        cli::cli_warn(
          "Suppressing axis rendering when {.code strip.position = \"right\"} \\
          and {.code strip.placement == \"outside\"}"
        )
      }
      right[pos] <- replace
    }

    left_empty <- t(apply(empties, 1, function(x) c(FALSE, diff(x) == -1)))
    if (any(left_empty)) {
      pos <- which(left_empty, arr.ind = TRUE)
      loc <- data_frame0(ROW = pos[, 1], COL = pos[, 2])
      panels <- vec_match(loc, layout[, c("ROW", "COL")])
      replace <- axes$y$left[panels]
      if (params$strip.position == "left" && !inside &&
          any(!vapply(replace, is.zero, logical(1))) && !params$free$y) {
        cli::cli_warn(
          "Suppressing axis rendering when {.code strip.position = \"left\"} \\
          and {.code strip.placement == \"outside\"}"
        )
      }
      left[pos] <- replace
    }

    list(
      top = top,
      bottom = bottom,
      left = left,
      right = right,
      measurements = measurements
    )
  },
  attach_axes = function(panel_table, axes, sizes) {
    panel_table <-
      weave_tables_row(panel_table, axes$top,   -1, sizes$top,    "axis-t", 3)
    panel_table <-
      weave_tables_row(panel_table, axes$bottom, 0, sizes$bottom, "axis-b", 3)
    panel_table <-
      weave_tables_col(panel_table, axes$left,  -1, sizes$left,   "axis-l", 3)
    panel_table <-
      weave_tables_col(panel_table, axes$right,  0, sizes$right,  "axis-r", 3)
    panel_table
  },
  finish_panels = function(self, panels, layout, params, theme) {
    panels
  },
  draw_panels = function(self, panels, layout,
                         x_scales, y_scales,
                         ranges, coord, data, theme, params) {
    if ((params$free$x || params$free$y) && !coord$is_free()) {
      cli::cli_abort("{.fn {snake_class(coord)}} doesn't support free scales.")
    }
    strip  <- self$strip
    layout <- self$setup_layout(layout, coord, params)

    # Setup parameters
    ncol <- max(layout$COL)
    nrow <- max(layout$ROW)
    layout$.TOP  <- layout$.BOTTOM <- layout$ROW
    layout$.LEFT <- layout$.RIGHT  <- layout$COL
    if (!is.null(params$dim)) {
      if (is.na(params$dim[1])) {
        params$dim[1] <- nrow
      }
      if (is.na(params$dim[2])) {
        params$dim[2] <- ncol
      }
    }

    # Setup panels
    panel_table <- self$setup_panel_table(
      panels, layout, theme, coord, ranges, params
    )

    # Deal with axes
    axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)
    axes <- self$setup_axes(axes, layout, params,theme)
    panel_table <- self$attach_axes(panel_table, axes, axes$measurements)

    # Deal with strips
    strip$setup(layout, params, theme, type = "wrap")
    panel_table <- strip$incorporate_wrap(
      panel_table, params$strip.position, clip = coord$clip, axes$measurements
    )

    self$finish_panels(panels = panel_table, layout = layout,
                       params = params, theme = theme)
  }
)

# Helpers -----------------------------------------------------------------

purge_guide_labels <- function(guide) {
  if (is.zero(guide)) {
    return(guide)
  }

  is_axis <- which(names(guide$children) == "axis")
  axis <- guide$children[[is_axis]]

  dim <- dim(axis)
  is_label <- vapply(axis$grobs, inherits, logical(1),
                     what = c("titleGrob", "richtext_grob", "zeroGrob"))

  axis$layout <- axis$layout[!is_label, , drop = FALSE]
  axis$grobs <- axis$grobs[!is_label]
  axis <- gtable_trim(axis)

  guide$children[[is_axis]] <- axis
  guide$width  <- guide$vp$width  <- sum(axis$widths)
  guide$height <- guide$vp$height <- sum(axis$heights)
  guide
}

.match_facet_arg <- function(value, options, x = 2, y = 3, both = 4, neither = 1,
                             nm = deparse(substitute(value))) {
  if (is.logical(value) && length(value) == 1 && !is.na(value)) {
    if (value) {
      value <- options[both]
    } else {
      value <- options[neither]
    }
  } else {
    value <- rlang::arg_match0(value, options, arg_nm = nm)
  }
  list(
    x = any(value %in% options[c(x, both)]),
    y = any(value %in% options[c(y, both)])
  )
}

.measure_axes <- function(axes) {
  top    <- unit(apply(axes$top,    1, max_height, value_only = TRUE), "cm")
  bottom <- unit(apply(axes$bottom, 1, max_height, value_only = TRUE), "cm")
  left   <- unit(apply(axes$left,   2, max_width,  value_only = TRUE), "cm")
  right  <- unit(apply(axes$right,  2, max_width,  value_only = TRUE), "cm")
  list(top = top, bottom = bottom, left = left, right = right)
}

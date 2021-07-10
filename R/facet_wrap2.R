# User function -----------------------------------------------------------

#' Extended wrapped facets
#'
#' This function behaves like \code{\link[ggplot2]{facet_wrap}()}, but has a few
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
#' @param strip An object created by a call to a strip function, such as
#'   [`strip_vanilla`][strip_vanilla()].
#'
#' @return A \code{Facet} ggproto object that can be added to a plot.
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
  strip = strip_vanilla()
) {

  strip.position <- match.arg(strip.position,
                              c("top", "bottom", "left",  "right"))
  labeller <- .int$check_labeller(labeller)
  facets <- .int$wrap_as_facets_list(facets)

  # Take care of direction
  dir <- match.arg(dir, c("h", "v"))
  if (identical(dir, "v")) {
    nrow_swap <- ncol
    ncol_swap <- nrow
    nrow <- .int$sanitise_dim(nrow_swap)
    ncol <- .int$sanitise_dim(ncol_swap)
  } else {
    nrow <- .int$sanitise_dim(nrow)
    ncol <- .int$sanitise_dim(ncol)
  }

  # Setup facet params
  free  <- .match_facet_arg(scales, c("fixed", "free_x", "free_y", "free"))
  axes  <- .match_facet_arg(axes, c("margins", "x", "y", "all"))
  rmlab <- .match_facet_arg(remove_labels, c("none", "x", "y", "all"))

  strip <- assert_strip(strip)

  ggproto(
    NULL, FacetWrap2,
    shrink = shrink,
    strip  = strip,
    params = list(
      facets = facets,
      free = free,
      as.table = as.table,
      strip.position = strip.position,
      drop = drop,
      ncol = ncol,
      nrow = nrow,
      labeller = labeller,
      dir = dir,
      axes = axes,
      rmlab = rmlab
    )
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
  setup_panel_table = function(self, panels, empty, position,
                               theme, coord, ranges, free) {
    # Puts panels into a gtable with names and spacing
    dim <- dim(empty)
    panel_table <- empty
    panel_table[position] <- panels

    aspect_ratio <- self$setup_aspect_ratio(coord, free, theme, ranges)

    empties <- apply(panel_table, c(1, 2), function(x){.int$is.zero(x[[1]])})
    panel_table <- gtable_matrix(
      "layout", panel_table,
      widths  = unit(rep(1, dim[2]), "null"),
      heights = unit(rep(abs(aspect_ratio), dim[1]), "null"),
      respect = attr(aspect_ratio, "respect"), clip = coord$clip,
      z = matrix(1, ncol = dim[2], nrow = dim[1])
    )
    panel_table$layout$name <- paste0("panel-", rep(seq_len(dim[2]), dim[1]),
                                      "-", rep(seq_len(dim[1]), each = dim[2]))

    panel_table <- gtable_add_col_space(
      panel_table, calc_element("panel.spacing.x", theme)
    )
    panel_table <- gtable_add_row_space(
      panel_table, calc_element("panel.spacing.y", theme)
    )
    list(panels = panel_table, empty = empties)
  },
  setup_axes = function(axes, empty, position, layout,
                        params, empties, theme) {
    # Takes rendered axes and picks the right ones for the facets
    dim <- dim(empty)
    nrow <- dim[1]
    ncol <- dim[2]

    # Initialise empty axes
    top <- bottom <- left <- right <- empty
    # Fill axes by scale ID
    top[position]    <- axes$x$top[layout$SCALE_X]
    bottom[position] <- axes$x$bottom[layout$SCALE_X]
    left[position]   <- axes$y$left[layout$SCALE_Y]
    right[position]  <- axes$y$right[layout$SCALE_Y]

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

    # Place back necessary axes that were removed prior, e.g. when there are
    # missing panels so the marginal axes should not be the most extreme one.
    if (any(empties)) {
      # Find first non-empty panel
      first_row <- which(apply(empties, 1, any))[1] - 1
      first_col <- which(apply(empties, 2, any))[1] - 1

      row_panels <- which(layout$ROW == first_row & layout$COL > first_col)
      col_panels <- which(layout$COL == first_col & layout$ROW > first_row)

      row_pos <- .int$convertInd(layout$ROW[row_panels], layout$COL[row_panels],
                                 nrow)
      col_pos <- .int$convertInd(layout$ROW[col_panels], layout$COL[col_panels],
                                 nrow)

      # Find corresponding axes
      row_axes <- axes$x$bottom[layout$SCALE_X[row_panels]]
      col_axes <- axes$y$right[layout$SCALE_Y[col_panels]]

      inside <- (theme$strip.placement %||% "inside") == "inside"

      # Throw warnings when misalignment is bound to happen
      if (params$strip.position == "bottom" && !inside && !params$free$x &&
          any(!vapply(row_axes, .int$is.zero, logical(1))) && !repeat_x) {
        warning("Supressing axis rendering when strip.position = 'bottom' and ",
                "strip.placement == 'outside'", call. = FALSE)
      } else {
        # Replace empty/purged axes by full axes
        bottom[row_pos] <- row_axes
      }

      # Throw warnings when misalignment is bound to happen
      if (params$strip.position == "right" && !inside && !params$free$y &&
          any(!vapply(col_axes, .int$is.zero, logical(1))) && !repeat_y) {
        warning("Supressing axis rendering when strip.position = 'right' and ",
                "strip.placement == 'outside'", call. = FALSE)
      } else {
        # Replace empty/purged axes by full axes
        right[col_pos] <- col_axes
      }
    }

    list(
      top = top,
      bottom = bottom,
      left = left,
      right = right
    )
  },
  attach_axes = function(panel_table, axes, sizes) {
    panel_table <- .int$weave_tables_row(
      panel_table, axes$top,   -1, sizes$top,    "axis-t", 3
    )
    panel_table <- .int$weave_tables_row(
      panel_table, axes$bottom, 0, sizes$bottom, "axis-b", 3
    )
    panel_table <- .int$weave_tables_col(
      panel_table, axes$left,  -1, sizes$left,   "axis-l", 3
    )
    panel_table <- .int$weave_tables_col(
      panel_table, axes$right,  0, sizes$right,  "axis-r", 3
    )
    panel_table
  },
  finish_panels = function(self, panels, layout, params, theme) {
    panels
  },
  draw_panels = function(self, panels, layout,
                         x_scales, y_scales,
                         ranges, coord, data, theme, params) {
    if ((params$free$x || params$free$y) && !coord$is_free()) {
      stop(.int$snake_class(coord), " doesn't support free scales",
           call. = FALSE)
    }
    strip  <- self$strip
    layout <- self$setup_layout(layout, coord, params)

    # Setup parameters
    ncol <- max(layout$COL)
    nrow <- max(layout$ROW)
    n <- nrow(layout)
    panel_order <- order(layout$ROW, layout$COL)
    layout <- layout[panel_order, ]
    panels <- panels[panel_order]
    panel_pos <- .int$convertInd(layout$ROW, layout$COL, nrow)

    # Setup panels
    empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
    panel_table <- self$setup_panel_table(panels, empty_table, panel_pos,
                                          theme, coord, ranges, params$free)
    empties <- panel_table$empty
    panel_table <- panel_table$panels

    # Deal with axes
    axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)
    axes <- self$setup_axes(axes, empty_table, panel_pos, layout,
                            params, empties, theme)
    sizes <- .measure_axes(axes)
    panel_table <- self$attach_axes(panel_table, axes, sizes)

    # Deal with strips
    strip$setup(layout, params, theme, type = "wrap")
    panel_table <- strip$incorporate_wrap(
      panel_table, params$strip.position, clip = coord$clip, sizes
    )

    self$finish_panels(panels = panel_table, layout = layout,
                       params = params, theme = theme)
  }
)

# Helpers -----------------------------------------------------------------

purge_guide_labels <- function(guide) {
  if (.int$is.zero(guide)) {
    return(guide)
  }

  is_axis <- which(names(guide$children) == "axis")
  axis <- guide$children[[is_axis]]

  dim <- dim(axis)
  is_label <- vapply(axis$grobs, inherits, logical(1), "titleGrob")

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

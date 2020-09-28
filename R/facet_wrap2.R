# User function -----------------------------------------------------------

#' Extended wrapped facets
#'
#' This function behaves like \code{\link[ggplot2]{facet_wrap}()}, but has a few
#' extra options on axis drawing when scales are fixed.
#'
#' @inheritParams ggplot2::facet_wrap
#' @param axes A \code{character} where axes should be drawn. Either
#'   \code{"margins"} (default), \code{"rows"}, \code{"cols"} or \code{"full"}.
#'   Only applies when the scale is free through the \code{scales} argument.
#' @param remove_labels A \code{character} denoting what labels should be
#'   removed when axes are repeated and redundant. Either \code{"none"}
#'   (default), \code{"rows"}, \code{"cols"} or \code{"all"}. Only applies to
#'   relevant position guides included with the \code{axes} argument when scales
#'   are fixed.
#'
#' @return A \code{Facet} ggproto object that can be added to a plot.
#' @export
#' @family facetting functions
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
#'
#' # Repeat all axes for every facet
#' p + facet_wrap2(vars(class), axes = "full")
#'
#' # Repeat only y-axes
#' p + facet_wrap2(vars(class), axes = "cols")
#'
#' # Repeat axes without labels
#' p + facet_wrap2(vars(class), axes = "full", remove_labels = "all")
#'
#' # Repeat axes without x-axis labels
#' p + facet_wrap2(vars(class), axes = "full", remove_labels = "rows")
facet_wrap2 <- function(
  facets, nrow = NULL, ncol = NULL,
  scales = "fixed", axes = "margins",
  remove_labels = "none",
  shrink = TRUE, labeller = "label_value",
  as.table = TRUE, drop = TRUE,
  dir = "h", strip.position = "top"
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

  # Setup free scales
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  # Setup axis positions
  axes <- match.arg(axes, c("margins", "rows", "cols", "full"))
  axes <- list(
    x = any(axes %in% c("rows", "full")),
    y = any(axes %in% c("cols", "full"))
  )

  rmlab <- match.arg(remove_labels, c("none", "rows", "cols", "all"))
  rmlab <- list(
    x = any(rmlab %in% c("rows", "all")),
    y = any(rmlab %in% c("cols", "all"))
  )

  ggproto(
    NULL, FacetWrap2,
    shrink = shrink,
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
  setup_strips = function(layout, params, theme) {
    # Format labels and render strips
    if (length(params$facets) == 0) {
      labels_df <- .int$new_data_frame(list("(all)" = "(all)"), n = 1)
    } else {
      labels_df <- layout[names(params$facets)]
    }
    attr(labels_df, "facet") <- "wrap"
    render_strips(
      structure(labels_df, type = "rows"),
      structure(labels_df, type = "cols"),
      params$labeller, theme
    )
  },
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
  measure_axes = function(axes) {
    top <- unit(
      apply(axes$top,    1, max_height, value_only = TRUE), "cm"
    )
    bottom <- unit(
      apply(axes$bottom, 1, max_height, value_only = TRUE), "cm"
    )
    left <- unit(
      apply(axes$left,   2, max_width,  value_only = TRUE), "cm"
    )
    right <- unit(
      apply(axes$right,  2, max_width,  value_only = TRUE), "cm"
    )
    list(top = top, bottom = bottom, left = left, right = right)
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
  attach_strips = function(panel_table, strips, position, empty, params, theme,
                           clip = "off", sizes) {
    # Setup parameters
    strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm")
    strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
    strip_mat <- empty
    strip_pos <- params$strip.position
    strip_mat[position] <- unlist(unname(strips),
                                  recursive = FALSE)[[strip_pos]]

    if (strip_pos %in% c("top", "bottom")) {
      inside_x <- (calc_element("strip.placement.x", theme) %||% "inside")
      inside_x <- inside_x  == "inside"
      if (strip_pos == "top") {
        offset  <- if (inside_x) -1 else -2
        padding <- sizes$top
      } else {
        offset <- if (inside_x) 0 else 1
        padding <- sizes$bottom
      }
      strip_height <- unit(
        apply(strip_mat, 1, max_height, value_only = TRUE), "cm"
      )

      # Add top/bottom strips
      panel_table <- .int$weave_tables_row(
        panel_table, strip_mat, offset, strip_height, strip_name, 2, clip
      )

      if (!inside_x) {
        # Add padding
        padding[as.numeric(padding) != 0] <- strip_padding
        panel_table <- .int$weave_tables_row(
          panel_table, row_shift = offset, row_height = padding
        )
      }

    } else {
      inside_y <- (calc_element("strip.placement.y", theme) %||% "inside")
      inside_y <- inside_y == "inside"
      if (strip_pos == "left") {
        offset <- if (inside_y) -1 else -2
        padding <- sizes$left
      } else {
        offset <- if (inside_y) 0 else 1
        padding <- sizes$right
      }
      padding[as.numeric(padding) != 0] <- strip_padding
      strip_width <- unit(
        apply(strip_mat, 2, max_width, value_only = TRUE), "cm"
      )

      # Add left/right strips
      panel_table <- .int$weave_tables_col(
        panel_table, strip_mat, offset, strip_width, strip_name, 2, clip
      )

      if (!inside_y) {
        padding[as.numeric(padding) != 0] <- strip_padding
        panel_table <- .int$weave_tables_col(
          panel_table, col_shift = offset, col_width = padding
        )
      }
    }

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
    # browser()

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
    sizes <- self$measure_axes(axes)
    panel_table <- self$attach_axes(panel_table, axes, sizes)

    # Deal with strips
    strips <- self$setup_strips(layout, params, theme)
    panel_table <- self$attach_strips(
      panel_table, strips, panel_pos, empty_table, params, theme,
      clip = coord$clip, sizes
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

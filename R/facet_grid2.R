# User function -----------------------------------------------------------

#' Extended grid facets
#'
#' This function behaves like [ggplot2::facet_grid] with default arguments, but
#' has a few extra options. It can draw partial or full axis guides at inner
#' panels, and position scales can be independent.
#'
#' @inheritParams ggplot2::facet_grid
#' @param scales A `character(1)` or `logical(1)` whether scales are shared
#'   across facets or allowed to vary. Interacts with the `independent`
#'   argument. One of the following:
#'   \describe{
#'     \item{`"fixed"` or `FALSE`}{Scales are shared across all facets
#'     (default).}
#'     \item{`"free_x"`}{x-scales are allowed to vary across rows.}
#'     \item{`"free_y"`}{y-scales are allowed to vary across columns.}
#'     \item{`"free"` or `TRUE`}{Scales can vary across rows and columns.}
#'   }
#' @param space A `character(1)` or `logical(1)` determining whether the size of
#'   panels are proportional to the length of the scales. When the `independent`
#'   argument allows for free scales in a dimension, the panel sizes cannot be
#'   proportional. Note that the `scales` argument must be free in the same
#'   dimension as the `space` argument to have an effect.One of the following:
#'   \describe{
#'     \item{`"fixed"` or `FALSE`}{All panels have the same size (default).}
#'     \item{`"free_x"`}{Panel widths are proportional to the x-scales.}
#'     \item{`"free_y"`}{Panel heights are proportional to the y-scales.}
#'     \item{`"free"` or `TRUE`}{Both the widths and heights vary according to
#'     scales.}
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
#' @param independent A `character(1)` or `logical(1)` determining whether
#'   scales can vary within a row or column of panels, like they can be in
#'   [ggplot2::facet_wrap]. The `scales` argument must be free for the same
#'   dimension before they can be set to independent. One of the following:
#'   \describe{
#'     \item{`"none"` or `FALSE`}{All y-scales should be fixed in a row and all
#'     x-scales are fixed in a column (default).}
#'     \item{`"x"`}{x-scales are allowed to vary within a column.}
#'     \item{`"y"`}{y-scales are allowed to vary within a row.}
#'     \item{`"all"` or `TRUE`}{Both x- and y-scales are allowed to vary within
#'     a column or row respectively.}
#'   }
#' @param strip An object created by a call to a strip function, such as
#'   [`strip_vanilla`][strip_vanilla()].
#'
#' @details Both the `independent` and `space` arguments only have an effect
#'   when the `scales` argument in a dimension is free. However, the
#'   `independent` and `space` arguments can *not* be used to simultaneously set
#'   an independent scale and have the panel size be proportional to that scale.
#'
#' @family facetting functions
#' @return A `Facet` ggproto object that can be added to a plot.
#' @export
#' @md
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
#'
#' # Repeat all axes for every facet
#' p + facet_grid2(cyl ~ drv, axes = "all")
#'
#' # Repeat only y-axes
#' p + facet_grid2(cyl ~  drv, axes = "y")
#'
#' # Repeat axes without x-labels
#' p + facet_grid2(cyl ~ drv, axes = "all", remove_labels = "x")
#'
#' # Grid facets with independent axes for every panel
#' p + facet_grid2(cyl ~ drv, scales = "free", independent = "all")
facet_grid2 <- function(
  rows = NULL,
  cols = NULL,
  scales = "fixed",
  space  = "fixed",
  axes   = "margins",
  remove_labels = "none",
  independent = "none",
  shrink   = TRUE,
  labeller = "label_value",
  as.table = TRUE,
  switch  = NULL,
  drop    = TRUE,
  margins = FALSE,
  strip = strip_vanilla()
) {
  if (is.logical(cols)) {
    abort(paste0("The `col` argument should not be logical.",
                 " Did you intend `margin` instead?"))
  }
  # Match arguments
  free <- .match_facet_arg(scales, c("fixed", "free_x", "free_y", "free"))
  space_free <- .match_facet_arg(space, c("fixed", "free_x", "free_y", "free"))
  axes <- .match_facet_arg(axes, c("margins", "x", "y", "all"))
  rmlab <- .match_facet_arg(remove_labels, c("none", "x", "y", "all"))
  independent <- .match_facet_arg(independent, c("none", "x", "y", "all"))

  # Check incompatible arguments
  params <- .validate_independent(independent, free, space_free, rmlab)

  if (!is.null(switch) && !switch %in% c("both", "x", "y")) {
    abort("Switch must be either 'both', 'x' or 'y'.")
  }

  facets_list <- .int$grid_as_facets_list(rows, cols)
  labeller <- .int$check_labeller(labeller)
  strip <- assert_strip(strip)

  ggproto(
    NULL,
    FacetGrid2,
    shrink = shrink,
    strip = force(strip),
    params = c(list(
      rows = facets_list$rows,
      cols = facets_list$cols,
      margins = margins,
      labeller = labeller,
      as.table = as.table,
      switch = switch,
      drop = drop,
      axes = axes
    ), params)
  )
}

# ggproto -----------------------------------------------------------------

# Important differences with FacetGrid:
# 1) `.$compute_layout()` uses the `self$vars_combine()` instead of the default
# `combine_vars()`. This makes it easier to substitute this function in
# `facet_nested()` that inherits from this.
# 2) `.$compute_layout()` supports the `independent` argument by setting
# 3) The `.$draw_panels()` method has been refactored for my understanding.
# 4) The drawing of axes is now more like FacetWrap2 instead of FacetGrid, to
# support the drawing of inner axes.

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
FacetGrid2 <- ggproto(
  "FacetGrid2", FacetGrid,
  vars_combine = combine_vars,
  compute_layout = function(data, params, self) {
    rows <- params$rows
    cols <- params$cols
    dups <- intersect(names(rows), names(cols))
    if (length(dups) > 0) {
      abort("Facetting variables can only appear in row or cols, not both.\n",
            "Problems: ", paste0(dups, collapse = "'"), call. = FALSE)
    }

    # Use `self$vars_combine` instead of `combine_vars`
    base_rows <- self$vars_combine(data, params$plot_env, rows,
                                   drop = params$drop)
    if (!params$as.table) {
      rev_order <- function(x) {factor(x, levels = rev(.int$ulevels(x)))}
      base_rows[] <- lapply(base_rows, rev_order)
    }
    # Use `self$vars_combine` instead of `combine_vars`
    base_cols <- self$vars_combine(data, params$plot_env, cols,
                                   drop = params$drop)
    base <- .int$df.grid(base_rows, base_cols)

    if (nrow(base) == 0) {
      out <- .int$new_data_frame(
        list(PANEL = factor(1L), ROW = 1L, COL = 1L, SCALE_X = 1L, SCALE_Y = 1L)
      )
      return(out)
    }

    # Adding margins
    base <- .int$reshape_add_margins(base, list(names(rows), names(cols)),
                                     params$margins)
    base <- unique(base)

    # Create panel info
    panel <- .int$id(base, drop = TRUE)
    panel <- factor(panel, levels = seq_len(attr(panel, "n")))

    rows <- if (!length(names(rows))) {
      rep(1L, length(panel))
    } else {
      .int$id(base[names(rows)], drop = TRUE)
    }
    cols <- if (!length(names(cols))) {
      rep(1L, length(panel))
    } else {
      .int$id(base[names(cols)], drop = TRUE)
    }

    panels <- .int$new_data_frame(c(list(PANEL = panel, ROW = rows, COL = cols),
                                  base))
    panels <- panels[order(panels$PANEL), , drop = FALSE]
    rownames(panels) <- NULL

    # Support for independent argument
    if (params$free$x) {
      if (params$independent$x) {
        panels$SCALE_X <- seq_nrow(panels)
      } else {
        panels$SCALE_X <- panels$COL
      }
    } else {
      panels$SCALE_X <- 1L
    }

    if (params$free$y) {
      if (params$independent$y) {
        panels$SCALE_Y <- seq_nrow(panels)
      } else {
        panels$SCALE_Y <- panels$ROW
      }
    } else {
      panels$SCALE_Y <- 1L
    }
    panels
  },
  setup_aspect_ratio = function(coord, free, theme, ranges) {
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
  setup_panel_table = function(panels, layout, space, ranges,
                               aspect, clip, theme) {
    ncol <- max(layout$COL)
    nrow <- max(layout$ROW)
    panel_table <- matrix(panels, nrow = nrow, ncol = ncol, byrow = TRUE)

    if (space$x) {
      ps <- layout$PANEL[layout$ROW == 1]
      widths <- vapply(ps, function(i) diff(ranges[[i]]$x.range), numeric(1))
      widths <- unit(widths, "null")
    } else {
      widths <- rep(unit(1, "null"), ncol)
    }

    if (space$y) {
      ps <- layout$PANEL[layout$COL == 1]
      heights <- vapply(ps, function(i) diff(ranges[[i]]$y.range), numeric(1))
      heights <- unit(heights, "null")
    } else {
      heights <- rep(unit(1, "null"), nrow)
    }

    panel_table <- gtable(
      widths = widths,
      heights = heights,
      respect = attr(aspect, "respect")
    )
    panel_table <- gtable_add_grob(
      panel_table, panels,
      t = layout$ROW, l = layout$COL,
      z = 1, clip = clip,
      name = paste0(
        "panel-", rep(seq_len(nrow), ncol), "-", rep(seq_len(ncol), each = nrow)
      )
    )
    panel_table <- gtable_add_col_space(
      panel_table, calc_element("panel.spacing.x", theme)
    )
    panel_table <- gtable_add_row_space(
      panel_table, calc_element("panel.spacing.y", theme)
    )
    panel_table
  },
  attach_axes = function(panel_table, axes) {
    sizes <- .measure_axes(axes)
    panel_table <- .int$weave_tables_row(
      panel_table, axes$top, -1, sizes$top, "axis-t", 3
    )
    panel_table <- .int$weave_tables_row(
      panel_table, axes$bottom, 0, sizes$bottom, "axis-b", 3
    )
    panel_table <- .int$weave_tables_col(
      panel_table, axes$left, -1, sizes$left, "axis-l", 3
    )
    panel_table <- .int$weave_tables_col(
      panel_table, axes$right, 0, sizes$right, "axis-r", 3
    )
    panel_table
  },

  setup_axes =  function(axes, empty, position, layout, params) {
    dim <- dim(empty)
    nrow <- dim[1]
    ncol <- dim[2]

    # Initalise empty axes
    top <- bottom <- left <- right <- empty
    # Fill axes by scale ID
    top[position]    <- axes$x$top[position]
    bottom[position] <- axes$x$bottom[position]
    left[position]   <- axes$y$left[position]
    right[position]  <- axes$y$right[position]

    repeat_x <- params$independent$x | params$axes$x
    repeat_y <- params$independent$y | params$axes$y

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
    if (params$axes$x && params$rmlab$x && !params$independent$x) {
      top[-1, ]       <- lapply(top[-1, ],       purge_guide_labels)
      bottom[-nrow, ] <- lapply(bottom[-nrow, ], purge_guide_labels)
    }
    if (params$axes$y && params$rmlab$y && !params$independent$y) {
      left[, -1]     <- lapply(left[, -1],     purge_guide_labels)
      right[, -ncol] <- lapply(right[, -ncol], purge_guide_labels)
    }
    list(top = top, bottom = bottom, left = left, right = right)
  },
  finish_panels = function(self, panels, layout, params, theme) {
    panels
  },
  draw_panels = function(
    panels, layout,
    x_scales, y_scales,
    ranges, coord, data, theme, params, self
  ) {
    if ((params$free$x || params$free$y) && !coord$is_free()) {
      abort(paste0(.int$snake_class(coord), "doesn't support free scales."))
    }
    strip <- self$strip
    cols <- which(layout$ROW == 1)
    rows <- which(layout$COL == 1)
    ncol <- max(layout$COL)
    nrow <- max(layout$ROW)
    empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)

    panel_pos <- as.vector(matrix(as.integer(layout$PANEL),
                                  nrow = nrow, ncol = ncol, byrow = TRUE))

    axes <- render_axes(ranges[panel_pos], ranges[panel_pos],
                        coord, theme, transpose = TRUE)
    axes <- self$setup_axes(axes, empty_table, panel_pos, layout, params)


    aspect_ratio <- self$setup_aspect_ratio(coord, params$free, theme, ranges)

    panel_table <- self$setup_panel_table(
      panels, layout, params$space_free, ranges, aspect_ratio, coord$clip, theme
    )
    panel_table <- self$attach_axes(panel_table, axes)

    strip$setup(layout, params, theme, type = "grid")
    panel_table <- strip$incorporate_grid(panel_table, params$switch)

    self$finish_panels(panels = panel_table, layout = layout,
                       params = params, theme = theme)
  }
)

.validate_independent <- function(independent, free, space_free, rmlab) {
  if (independent$x) {
    if (!free$x) {
      abort("Rows cannot be independent if the scales are not free.")
    }
    if (space_free$x) {
      warn("Rows cannot have free space if axes are independent.")
      space_free$x <- FALSE
    }
    if (rmlab$x) {
      warn("x-axes must be labelled if they are independent.")
      rmlab$x <- FALSE
    }
  }
  if (independent$y) {
    if (!free$y) {
      abort("Columns cannot be independent if the scales are not free.")
    }
    if (space_free$y) {
      warn("Columns cannot have free space if axes are independent.")
      space_free$y <- FALSE
    }
    if (rmlab$y) {
      warn("y-axes must be labelled if they are independent.")
      rmlab$y <- FALSE
    }
  }
  list(independent = independent,
       free = free,
       space_free = space_free,
       rmlab = rmlab)
}

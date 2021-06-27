# constructor -------------------------------------------------------------

#' Default strips
#'
#' Strips with the style of vanilla ggplot2.
#'
#' @param clip A `character(1)` that controls wether text labels are clipped to
#'   the background boxes. Can be either `"inherit"` (default), `"on"` or
#'   `"off"`.
#' @param size A `character(1)` stating that the strip margins in different
#'   layers remain `"constant"` or are `"variable"`.
#'
#' @return A `Strip` ggproto object that can be used ggh4x facets.
#' @export
#' @md
#' @family strips
#'
#' @examples
#' # Some dummy data with a long string
#' df <- data.frame(
#'   short = "X",
#'   long  = "A very long string that takes up a lot of space",
#'   value = 1
#' )
#' # Simple plot
#' p <- ggplot(df, aes(value, value)) +
#'   geom_point() +
#'   theme(strip.text.y.right = element_text(angle = 0))
#'
#' # Short titles take up as much space as long titles
#' p + facet_grid2(
#'   vars(short, long),
#'   strip = strip_vanilla(size = "constant")
#' )
#'
#' # Short titles take up less space
#' p + facet_grid2(
#'   vars(short, long),
#'   strip = strip_vanilla(size = "variable")
#' )
strip_vanilla <- function(clip = "inherit", size = "constant") {
  params <- list(
    clip = arg_match0(clip, c("on", "off", "inherit")),
    size = arg_match0(size, c("constant", "variable"))
  )

  ggproto(
    NULL, Strip,
    params = params
  )
}

# ggproto class -----------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
Strip <- ggproto(
  "Strip",

  clip = "inherit",

  elements = list(),

  params = list(),

  strips = list(),

  setup_elements = function(self, theme, type) {
    # Actual strip theme elements
    background <- list(
      "x" = element_render(theme, "strip.background.x"),
      "y" = element_render(theme, "strip.background.y")
    )
    text <- list(
      "x" = list(
        "top"    = calc_element("strip.text.x.top", theme),
        "bottom" = calc_element("strip.text.x.bottom", theme)
      ),
      "y" = list(
        "left"  = calc_element("strip.text.y.left", theme),
        "right" = calc_element("strip.text.y.right", theme)
      )
    )
    # Strip placement theme elements
    inside <- list(
      "x" = calc_element("strip.placement.x", theme) %||% "inside" == "inside",
      "y" = calc_element("strip.placement.y", theme) %||% "inside" == "inside"
    )
    if (type == "wrap") {
      padding <- calc_element("strip.switch.pad.wrap", theme)
    } else {
      padding <- calc_element("strip.switch.pad.grid", theme)
    }
    padding <- convertUnit(padding, "cm")

    list(
      padding = padding,
      background = background,
      text = text,
      inside = inside
    )
  },

  setup = function(self, layout, params, theme, type) {
    self$elements <- self$setup_elements(theme, type)
    if (type == "wrap") {
      # Format labels and render strips
      if (length(params$facets) == 0) {
        labels <- .int$new_data_frame(list("(all)" = "(all)"), n = 1)
      } else {
        labels <- layout[names(params$facets)]
      }
      col_vars <- row_vars <- labels
      layout_x <- layout_y <- layout
    } else {
      col_vars <- !duplicated(layout[names(params$cols)])
      row_vars <- !duplicated(layout[names(params$rows)])
      layout_x <- layout[col_vars,]
      layout_y <- layout[row_vars,]
      col_vars <- layout_x[names(params$cols)]
      row_vars <- layout_y[names(params$rows)]
    }
    attr(row_vars, "facet") <- type
    attr(col_vars, "facet") <- type
    self$get_strips(
      x = structure(col_vars, type = "cols"),
      y = structure(row_vars, type = "rows"),
      params$labeller, theme, params = self$params,
      layout_x = layout_x, layout_y = layout_y
    )
  },

  # Function based on ggplot2::render_strips
  get_strips = function(self, x = NULL, y = NULL, labeller, theme, params,
                        layout_x, layout_y) {
    self$strips <- list(
      x = self$build_strip(x, labeller, theme, TRUE,
                           params, layout_x),
      y = self$build_strip(y, labeller, theme, FALSE,
                           params, layout_y)
    )
  },

  draw_labels = function(labels, element, position, layer_id, size) {
    export <<- list(labels = labels, element = element, position = position,
                    layer_id = layer_id, size = size)
    if (size == "constant") {
      layer_id <- rep(1L, length(layer_id))
    }
    aes <- if (position %in% c("top", "bottom")) "x" else "y"
    labels <- mapply(function(label, elem) {
      grob <- element_grob(elem, label, margin_x = TRUE, margin_y = TRUE)
      if (!inherits(grob, c("titleGrob", "zeroGrob"))) {
        grob <- .int$add_margins(
          gList(grob), grobHeight(grob), grobWidth(grob),
          margin_x = TRUE, margin_y = TRUE
        )
      }
      grob$name <- grobName(grob, paste0("strip.text.", aes))
      grob
    }, label = labels, elem = element, SIMPLIFY = FALSE)

    zeros <- vapply(labels, .int$is.zero, logical(1))
    if (length(labels) == 0 || all(zeros)) {
      return(labels)
    }

    if (aes == "x") {
      height <- lapply(labels[!zeros], function(x) x$heights[2])
      height <- lapply(split(height, layer_id[!zeros]), max_height)
      height <- do.call(unit.c, height)
      width  <- rep(unit(1, "null"), length(height))
    } else {
      width  <- lapply(labels[!zeros], function(x) x$widths[2])
      width  <- lapply(split(width, layer_id[!zeros]), max_width)
      width  <- do.call(unit.c, width)
      height <- rep(unit(1, "null"), length(width))
    }

    # Set all margins equal
    labels[!zeros] <- mapply(function(x, i) {
      w <- width[i]
      h <- height[i]
      x$widths  <- unit.c(x$widths[1],  w,  x$widths[c(-1,  -2)])
      x$heights <- unit.c(x$heights[1], h, x$heights[c(-1, -2)])
      x$vp$parent$layout$widths <- unit.c(
        x$vp$parent$layout$widths[1],
        w,
        x$vp$parent$layout$widths[c(-1, -2)]
      )
      x$vp$parent$layout$heights <- unit.c(
        x$vp$parent$layout$heights[1],
        h,
        x$vp$parent$layout$heights[c(-1, -2)]
      )
      x
    }, x = labels[!zeros], i = layer_id[!zeros], SIMPLIFY = FALSE)

    firsts <- lapply(split(labels[!zeros], layer_id[!zeros]), `[[`, 1)
    if (aes == "x") {
      height <- lapply(firsts, `[[`, "heights")
      height <- unname(do.call(unit.c, lapply(height, sum)))
    } else {
      width  <- lapply(firsts, `[[`, "widths")
      width  <- unname(do.call(unit.c, lapply(width, sum)))
    }
    attr(labels, "width")  <- width
    attr(labels, "height") <- height
    return(labels)
  },

  # Function mostly based on ggplot2:::assemble_strips
  draw_strip = function(self, labels, position, elements,
                        params, layout) {
    aes <- if (position %in% c("top", "bottom")) "x" else "y"
    el <- elements[[c("text", aes, position)]]
    el <- if (inherits(el, "list")) el else list(el)

    bg <- elements[[c("background", aes)]]
    bg <- if (inherits(bg, "list")) bg else list(bg)

    if (is.null(elements$by_layer)) {
      by_layer <- FALSE
    } else {
      by_layer <- elements$by_layer[[aes]]
    }

    index <- as.vector(col(labels))
    if (by_layer) {
      el <- el[pmin(index, length(el))]
      bg <- bg[pmin(index, length(bg))]
    } else {
      el <- rep_len(el, length(labels))
      bg <- rep_len(bg, length(labels))
    }

    strips <- self$draw_labels(labels, el, position, layer_id = index,
                               size = params$size)
    if (length(strips) == 0 || all(vapply(strips, .int$is.zero, logical(1)))) {
      return(strips)
    }
    width  <- attr(strips, "width")
    height <- attr(strips, "height")

    # Combine with background
    strips <- mapply(function(x, bg) {
      x <- gTree(children = gList(bg, x))
      x$name <- grobName(x, "strip")
      x
    }, x = strips, bg = bg, SIMPLIFY = FALSE)

    strips <- matrix(strips, ncol = ncol(labels), nrow = nrow(labels))
    strips <- apply(strips, 1, function(x) {
      if (aes == "x") {
        mat <- matrix(x, ncol = 1)
      } else {
        mat <- matrix(x, nrow = 1)
      }
      gtable_matrix(
        "strip", mat,
        rep_len(width,  ncol(mat)),
        rep_len(height, nrow(mat)),
        clip = params$clip
      )
    })
    .int$new_data_frame(list(
      t = layout$ROW, l = layout$COL,
      b = layout$ROW, r = layout$COL,
      grobs = strips
    ))
  },

  # Function adapted from ggplot2:::build_strip
  build_strip = function(self, data, labeller, theme, horizontal,
                         params, layout) {
    labeller <- match.fun(labeller)
    elem <- self$elements

    if (.int$empty(data)) {
      if (horizontal) {
        return(list(top = NULL, bottom = NULL))
      } else {
        return(list(left = NULL, right = NULL))
      }
    }

    labels <- lapply(labeller(data), cbind)
    labels <- do.call("cbind", labels)
    ncol <- ncol(labels)
    nrow <- ncol(labels)

    if (horizontal) {
      top    <- self$draw_strip(labels, "top",    elem, params, layout)
      bottom <- self$draw_strip(labels, "bottom", elem, params, layout)
      list(top = top, bottom = bottom)
    } else {
      left  <- self$draw_strip(labels, "left", elem, params, layout)
      right <- self$draw_strip(
        labels[, rev(seq_len(ncol)), drop = FALSE],
        "right", elem, params, layout
      )
      list(left = left, right = right)
    }
  },

  # Recieves panels and parameters from facet, then adds
  incorporate_wrap = function(self, panels, position,
                              clip = "off", sizes) {
    # Setup parameters
    strip_padding <- self$elements$padding
    strip_name <- paste0("strip-", substr(position, 1, 1))
    strip   <- unlist(unname(self$strips), recursive = FALSE)[[position]]
    padding <- sizes[[position]]
    padding[as.numeric(padding) != 0] <- strip_padding
    inside <- self$elements$inside

    if (position %in% c("top", "bottom")) {
      if (position == "top") {
        offset <- -2 + inside$x
      } else {
        offset <-  1 - inside$x
      }
      strip_height <- split(unlist(lapply(strip$grobs, height_cm)), strip$t)
      strip_height <- unit(vapply(strip_height, max, numeric(1)), "cm")

      # Add top/bottom strips
      panels <- weave_layout_row(
        panels, strip, offset, strip_height, strip_name, 2, clip
      )
      if (!inside$x) {
        # Apply extra padding
        panels <- .int$weave_tables_row(
          panels, row_shift = offset, row_height = padding
        )
      }
    } else {
      if (position == "left") {
        offset <- -2 + inside$y
      } else {
        offset <-  1 - inside$y
      }
      strip_width <- split(unlist(lapply(strip$grobs, width_cm)), strip$l)
      strip_width <- unit(vapply(strip_width, max, numeric(1)), "cm")

      # Add left/right strips
      panels <- weave_layout_col(
        panels, strip, offset, strip_width, strip_name, 2, clip
      )

      if (!inside$y) {
        # Apply extra padding
        panels <- .int$weave_tables_col(
          panels, col_shift = offset, col_width = padding
        )
      }
    }
    panels
  },

  # Recieves panels and parameters from facet, then adds strips.
  incorporate_grid = function(self, panels, switch) {
    switch_x <- !is.null(switch) && switch %in% c("both", "x")
    switch_y <- !is.null(switch) && switch %in% c("both", "y")
    inside  <- self$elements$inside
    padding <- self$elements$padding
    strips  <- self$strips

    pos_cols <- panel_cols(panels)

    if (switch_x) {
      strip  <- strips$x$bottom$grobs
      table  <- strips$x$bottom[c("t", "b", "l", "r")]
      prefix <- "strip-b-"
    } else {
      strip  <- strips$x$top$grobs
      table  <- strips$x$top[c("t", "b", "l", "r")]
      prefix <- "strip-t-"
    }
    if (!is.null(strip)) {
      stripnames  <- paste0(prefix, seq_along(strip))
      stripheight <- max_height(strip)
      if (inside$x) {
        where  <- if (switch_x) -2 else 1
      } else {
        where  <- 0 - switch_x
        panels <- gtable_add_rows(panels, padding, where)
      }
      panels <- gtable_add_rows(panels, stripheight, where)
      panels <- gtable_add_grob(
        panels, strip, name = stripnames,
        t = where + !switch_x,
        l = pos_cols$l[table$l], r = pos_cols$r[table$r],
        clip = "on", z = 2
      )
    }

    pos_rows <- panel_rows(panels)

    if (switch_y) {
      strip  <- strips$y$left$grobs
      table  <- strips$y$left[c("t", "b", "l", "r")]
      prefix <- "strip-l-"
    } else {
      strip  <- strips$y$right$grobs
      table  <- strips$y$right[c("t", "b", "l", "r")]
      prefix <- "strip-r-"
    }
    if (!is.null(strip)) {
      stripnames <- paste0(prefix, seq_along(strip))
      stripwidth <- max_width(strip)
      if (inside$y) {
        where  <- if (switch_y) 1 else -2
      } else {
        where  <- -1 + switch_y
        panels <- gtable_add_cols(panels, padding, where)
      }
      panels <- gtable_add_cols(panels, stripwidth, where)
      panels <- gtable_add_grob(
        panels, strip, name = stripnames,
        t = pos_rows$t[table$t], b = pos_rows$b[table$b],
        l = where + switch_y,
        clip = "on", z = 2
      )
    }

    panels

  }
)

weave_layout_row <- function(table, table2, row_shift, row_height,
                             name, z = 1, clip = "off") {
  cols <- panel_cols(table)
  rows <- panel_rows(table)

  for (i in rev(seq_along(rows$t))) {
    table <- gtable_add_rows(table, row_height[i], pos = rows$t[i] + row_shift)
  }

  rows <- rows + row(rows) + row_shift

  if (!missing(table2)) {
    table <- gtable_add_grob(
      table, table2$grobs,
      t = rows$t[table2$t],
      b = rows$b[table2$b],
      l = cols$l[table2$l],
      r = cols$r[table2$r],
      clip = clip, z = z,
      name = paste0(name, "-", seq_along(cols$l), "-", seq_along(table2$t))
    )
  }
  table
}

weave_layout_col <- function(table, table2, col_shift, col_width,
                             name, z= 1, clip = "off") {
  cols <- panel_cols(table)
  rows <- panel_rows(table)

  for (i in rev(seq_along(cols$l))) {
    table <- gtable_add_cols(table, col_width[i], pos = cols$l[i] + col_shift)
  }

  cols <- cols + row(cols) + col_shift

  if (!missing(table2)) {
    table <- gtable_add_grob(
      table, table2$grobs,
      t = rows$t[table2$t],
      b = rows$b[table2$b],
      l = cols$l[table2$l],
      r = cols$r[table2$r],
      clip = clip, z= z,
      name = paste0(name, "-", seq_along(rows$t), "-", seq_along(table2$l))
    )
  }
  table
}

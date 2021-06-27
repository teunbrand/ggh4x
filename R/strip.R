
# constructor -------------------------------------------------------------

#' Default strips
#'
#' @param clip
#'
#' @return A `Strip` ggproto object.
#' @export
#'
#' @examples
#' strip_vanilla()
strip_vanilla <- function(clip = "inherit") {
  ggproto(NULL, Strip, clip = clip)
}

# ggproto class -----------------------------------------------------------

Strip <- ggproto(
  "Strip",

  clip = "inherit",

  elements = list(),

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

    self$elements <- list(
      padding = padding,
      background = background,
      text = text,
      inside = inside
    )
  },

  strips = list(),

  setup = function(self, layout, params, theme, type) {
    self$setup_elements(theme, type)
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
      params$labeller, theme, layout_x = layout_x, layout_y = layout_y
    )
  },

  # Function based on ggplot2::render_strips
  get_strips = function(self, x = NULL, y = NULL, labeller, theme,
                        layout_x, layout_y) {
    self$strips <- list(
      x = self$build_strip(x, labeller, theme, TRUE,
                           clip = self$clip, layout_x),
      y = self$build_strip(y, labeller, theme, FALSE,
                           clip = self$clip, layout_y)
    )
  },

  # Function mostly based on ggplot2:::assemble_strips
  draw_strip = function(labels, position, elements, clip = "inherit", layout) {
    aes <- if (position %in% c("top", "bottom")) "x" else "y"
    el <- elements[[c("text", aes, position)]]
    bg <- elements[[c("background", aes)]]
    el_name <- paste("strip.text", aes, position, sep = ".")

    # Render text
    text <- lapply(labels, function(label) {
      grob <- element_grob(el, label, margin_x = TRUE, margin_y = TRUE)
      if (!inherits(grob, c("titleGrob", "zeroGrob"))) {
        # Add margins to non-titlegrobs
        ggplot2:::add_margins(
          gList(grob),  grobHeight(grob), grobWidth(grob),
          margin_x = TRUE, margin_y = TRUE
        )
      }
      grob$name <- grobName(grob, el_name)
      grob
    })
    # Early exit when zero-grobs
    if (length(text) == 0 || .int$is.zero(text[[1]])) {
      text <- text[seq_len(NROW(layout))]
      text <- .int$new_data_frame(list(
        t = layout$ROW, l = layout$COL,
        b = layout$ROW, r = layout$COL,
        grobs = text
      ))
      return(text)
    }

    if (aes == "x") {
      height <- max_height(lapply(text, function(x) x$heights[2]))
      width  <- unit(1, "null")
    } else {
      height <- unit(1, "null")
      width  <- max_width(lapply(text, function(x) x$widths[2]))
    }

    text <- lapply(text, function(x) {
      x$widths  <- unit.c(x$widths[1],  width,  x$widths[c(-1,  -2)])
      x$heights <- unit.c(x$heights[1], height, x$heights[c(-1, -2)])
      x$vp$parent$layout$widths <- unit.c(
        x$vp$parent$layout$widths[1],
        width,
        x$vp$parent$layout$widths[c(-1, -2)]
      )
      x$vp$parent$layout$heights <- unit.c(
        x$vp$parent$layout$heights[1],
        height,
        x$vp$parent$layout$heights[c(-1, -2)]
      )
      x
    })
    if (aes == "x") {
      height <- sum(text[[1]]$heights)
    } else {
      width  <- sum(text[[1]]$widths)
    }

    # Combine with background
    text <- lapply(text, function(x) {
      x <- gTree(children = gList(bg, x))
      x$name <- grobName(x, "strip")
      x
    })

    text <- matrix(text, ncol = ncol(labels), nrow = nrow(labels))
    text <- apply(text, 1, function(x) {
      if (aes == "x") {
        mat <- matrix(x, ncol = 1)
      } else {
        mat <- matrix(x, nrow = 1)
      }
      gtable_matrix(
        "strip", mat,
        rep(width, ncol(mat)),
        rep(height, nrow(mat)),
        clip = clip
      )
    })
    .int$new_data_frame(list(
      t = layout$ROW, l = layout$COL,
      b = layout$ROW, r = layout$COL,
      grobs = text
    ))
  },

  # Function adapted from ggplot2:::build_strip
  build_strip = function(self, data, labeller, theme, horizontal,
                         clip = "inherit", layout) {
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
      top    <- self$draw_strip(labels, "top",    elem, clip, layout)
      bottom <- self$draw_strip(labels, "bottom", elem, clip, layout)
      list(top = top, bottom = bottom)
    } else {
      left  <- self$draw_strip(labels, "left", elem, clip, layout)
      right <- self$draw_strip(
        labels[, rev(seq_len(ncol)), drop = FALSE],
        "right", elem, clip, layout
      )
      list(left = left, right = right)
    }
  },

  incorporate_wrap = function(self, panel_table, position, params, theme,
                              clip = "off", sizes) {
    # Setup parameters
    strip_padding <- self$elements$padding
    strip_pos <- params$strip.position
    strip_name <- paste0("strip-", substr(strip_pos, 1, 1))
    strip <- unlist(unname(self$strips), recursive = FALSE)[[strip_pos]]
    padding <- sizes[[strip_pos]]
    padding[as.numeric(padding) != 0] <- strip_padding
    inside <- self$elements$inside

    if (strip_pos %in% c("top", "bottom")) {
      if (strip_pos == "top") {
        offset  <- if (inside$x) -1 else -2
      } else {
        offset  <- if (inside$x) 0 else 1
      }
      strip_height <- split(unlist(lapply(strip$grobs, height_cm)), strip$t)
      strip_height <- unit(vapply(strip_height, max, numeric(1)), "cm")

      # Add top/bottom strips
      panel_table <- weave_layout_row(
        panel_table, strip, offset, strip_height, strip_name, 2, clip
      )
      if (!inside$x) {
        # Apply extra padding
        panel_table <- .int$weave_tables_row(
          panel_table, row_shift = offset, row_height = padding
        )
      }
    } else {
      if (strip_pos == "left") {
        offset  <- if (inside$y) -1 else -2
      } else {
        offset  <- if (inside$y) 0 else 1
      }
      strip_width <- split(unlist(lapply(strip$grobs, width_cm)), strip$l)
      strip_width <- unit(vapply(strip_width, max, numeric(1)), "cm")

      # Add left/right strips
      panel_table <- weave_layout_col(
        panel_table, strip, offset, strip_width, strip_name, 2, clip
      )

      if (!inside$y) {
        # Apply extra padding
        panel_table <- .int$weave_tables_col(
          panel_table, col_shift = offset, col_width = padding
        )
      }
    }
    panel_table
  },

  incorporate_grid = function(self, panels, switch, theme) {
    switch_x <- !is.null(switch) && switch %in% c("both", "x")
    switch_y <- !is.null(switch) && switch %in% c("both", "y")
    inside_x <- calc_element("strip.placement.x", theme) == "inside"
    inside_y <- calc_element("strip.placement.y", theme) == "inside"
    padding  <- calc_element("strip.switch.pad.grid", theme)
    padding  <- convertUnit(padding, "cm")
    strips <- self$strips

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
      if (inside_x) {
        where  <- if (switch_x) -2 else 1
      } else {
        where  <- if (switch_x) -1 else 0
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
      if (inside_y) {
        where <- if (switch_y) 1 else -2
      } else {
        where <- if (switch_y) 0 else -1
        panels <- gtable_add_rows(panels, padding, where)
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

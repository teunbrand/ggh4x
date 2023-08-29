# constructor -------------------------------------------------------------

#' Default strips
#'
#' Strips with the style of vanilla ggplot2.
#'
#' @param clip A `character(1)` that controls whether text labels are clipped to
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
        "top"    = calc_element("strip.text.x.top",    theme),
        "bottom" = calc_element("strip.text.x.bottom", theme)
      ),
      "y" = list(
        "left"  = calc_element("strip.text.y.left",  theme),
        "right" = calc_element("strip.text.y.right", theme)
      )
    )
    # Strip placement theme elements
    inside <- list(
      "x" = calc_element("strip.placement.x", theme) %||% "inside" == "inside",
      "y" = calc_element("strip.placement.y", theme) %||% "inside" == "inside"
    )
    padding <- calc_element(
      paste0("strip.switch.pad.", if (type == "wrap") "wrap" else "grid"),
      theme
    )
    padding <- convertUnit(padding,  "cm")

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
        labels <- data_frame0("(all)" = "(all)", .size = 1)
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
    if (size == "constant") {
      layer_id <- rep(1L, length(layer_id))
    }
    aes <- if (position %in% c("top", "bottom")) "x" else "y"
    labels <- mapply(function(label, elem) {
      grob <- element_grob(elem, label, margin_x = TRUE, margin_y = TRUE)
      if (!new_guide_system) {
        if (!inherits(grob, c("titleGrob", "zeroGrob"))) {
          grob <- add_margins(
            gList(grob), grobHeight(grob), grobWidth(grob),
            margin_x = TRUE, margin_y = TRUE
          )
        }
      }
      grob$name <- grobName(grob, paste0("strip.text.", aes))
      grob
    }, label = labels, elem = element$el, SIMPLIFY = FALSE)

    zeros <- vapply(labels, is.zero, logical(1))
    if (length(labels) == 0 || all(zeros)) {
      return(labels)
    }

    if (aes == "x") {
      if (!new_guide_system) {
        height <- lapply(labels[!zeros], function(x) x$heights[2])
        height <- lapply(split(height, layer_id[!zeros]), max_height)
        height <- do.call(unit.c, height)
      } else {
        height <- max_height(labels)
      }
      width  <- rep(unit(1, "null"), length(height))
    } else {
      if (!new_guide_system) {
        width <- lapply(labels[!zeros], function(x) x$widths[2])
        width <- lapply(split(width, layer_id[!zeros]), max_width)
        width <- do.call(unit.c, width)
      } else {
        width <- max_width(labels)
      }
      height <- rep(unit(1, "null"), length(width))
    }

    if (!new_guide_system) {
      # Set all margins equal
      idx_w <- c("vp", "parent", "layout", "widths")
      idx_h <- c("vp", "parent", "layout", "heights")
      labels[!zeros] <- mapply(function(x, i) {
        w <- width[i]
        h <- height[i]
        x$widths   <- unit.c(  x$widths[1], w,   x$widths[c(-1, -2)])
        x$heights  <- unit.c( x$heights[1], h,  x$heights[c(-1, -2)])
        x[[idx_w]] <- unit.c(x[[idx_w]][1], w, x[[idx_w]][c(-1, -2)])
        x[[idx_h]] <- unit.c(x[[idx_h]][1], h, x[[idx_h]][c(-1, -2)])
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
    }

    # Combine with background
    labels <- mapply(function(x, bg) {
      x <- gTree(children = gList(bg, x))
      x$name <- grobName(x, "strip")
      x
    }, x = labels, bg = element$bg, SIMPLIFY = FALSE)

    attr(labels, "width")  <- width
    attr(labels, "height") <- height
    return(labels)
  },

  finish_strip = function(strip, width, height, position, layout, dim,
                          clip = "inherit") {
    empty_strips <- length(strip) == 0 ||
      all(vapply(strip, is.zero, logical(1)))
    if (!empty_strips) {
      strip <- matrix(strip, ncol = dim[2], nrow = dim[1])
      horizontal <- position %in% c("top", "bottom")
      if (horizontal)  {
        strip <- apply(strip, 1, matrix, ncol = 1)
      } else {
        strip <- apply(strip, 1, matrix, nrow = 1)
      }
      strip <- lapply(strip, function(mat) {
        gtable_matrix(
          "strip", mat,
          rep(width,  length.out = ncol(mat)),
          rep(height, length.out = nrow(mat)),
          clip = clip
        )
      })
    }
    panel <- as.integer(layout$PANEL)
    data_frame0(
      t = panel, l = panel,
      b = panel, r = panel,
      grobs = strip
    )
  },

  init_strip = function(elements, position, layer_index) {
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

    if (by_layer) {
      el <- el[pmin(layer_index, length(el))]
      bg <- bg[pmin(layer_index, length(bg))]
    } else {
      el <- rep_len(el, length(layer_index))
      bg <- rep_len(bg, length(layer_index))
    }
    return(list(el = el, bg = bg))
  },

  # Function mostly based on ggplot2:::assemble_strips
  assemble_strip = function(self, labels, position, elements,
                            params, layout) {
    index  <- as.vector(col(labels))
    elems  <- self$init_strip(elements, position, index)
    strips <- self$draw_labels(labels, elems, position, layer_id = index,
                               size = params$size)
    width  <- attr(strips, "width")
    height <- attr(strips, "height")

    self$finish_strip(strips, width, height, position, layout,
                      dim = dim(labels), params$clip)
  },

  # Function adapted from ggplot2:::build_strip
  build_strip = function(self, data, labeller, theme, horizontal,
                         params, layout) {

    if (empty(data)) {
      ans <- list(NULL, NULL)
      names(ans) <- if (horizontal) c("top", "bottom") else c("left", "right")
      return(ans)
    }

    labels <- match.fun(labeller)
    labels <- lapply(labels(data), cbind)
    labels <- do.call("cbind", labels)

    elem <- self$elements

    if (horizontal) {
      top    <- self$assemble_strip(labels, "top",    elem, params, layout)
      bottom <- self$assemble_strip(labels, "bottom", elem, params, layout)
      list(top = top, bottom = bottom)
    } else {
      # Labels on the right are inside-out
      revlab <- labels[, rev(seq_len(ncol(labels))), drop = FALSE]
      right <- self$assemble_strip(revlab, "right", elem, params, layout)
      left  <- self$assemble_strip(labels, "left",  elem, params, layout)
      list(left = left, right = right)
    }
  },

  # Receives panels and parameters from facet, then adds
  incorporate_wrap = function(self, panels, position,
                              clip = "off", sizes) {

    # Setup padding
    strip_padding <- self$elements$padding
    padding  <- sizes[[position]]
    padding[as.numeric(padding) != 0] <- strip_padding

    # Setup parameters
    strip <- unlist(unname(self$strips), recursive = FALSE)[[position]]
    inside <- self$elements$inside
    position <- substr(position, 1, 1)
    strip_name <- paste0("strip-", position)
    offset <- switch(
      position,
      "t" = -2 + inside$x,
      "b" =  1 - inside$x,
      "l" = -2 + inside$y,
      "r" =  1 - inside$y
    )


    if (position %in% c("t", "b")) {

      strip_height <- split_heights_cm(strip$grobs, strip$t)

      # Add top/bottom strips
      panels <- weave_panel_rows(
        panels, strip, offset, strip_height, strip_name, 2, clip, position
      )
      if (!inside$x) {
        # Apply extra padding
        panels <- weave_panel_rows(
          panels, row_shift = offset, row_height = padding
        )
      }
    } else {

      strip_width <- split_widths_cm(strip$grobs, strip$l)

      # Add left/right strips
      panels <- weave_panel_cols(
        panels, strip, offset, strip_width, strip_name, 2, clip, position
      )

      if (!inside$y) {
        # Apply extra padding
        panels <- weave_panel_cols(
          panels, col_shift = offset, col_width = padding
        )
      }
    }
    panels
  },

  # Recieves panels and parameters from facet, then adds strips.
  incorporate_grid = function(self, panels, switch) {

    switch_x <- switch %in% c("both", "x")
    switch_y <- switch %in% c("both", "y")
    inside   <- self$elements$inside
    padding  <- self$elements$padding
    strips   <- self$strips

    pos_cols <- panels$layout[grepl("^panel-", panels$layout$name), ,
                              drop = FALSE]

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

    # pos_rows <- panel_rows(panels)
    pos_rows <- panels$layout[grepl("^panel-", panels$layout$name), ,
                              drop = FALSE]

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


# Helpers -----------------------------------------------------------------

assert_strip <- function(strip, arg = deparse(substitute(strip))) {
  is_strip <- inherits(strip, "Strip") && inherits(strip, "ggproto")
  if (!is_strip) {
    cli::cli_abort(
      "The {.arg {arg}} argument is not a valid facet strip specification."
    )
  }
  strip
}

# Constructor -------------------------------------------------------------

strip_tag <- function(
  clip = "inherit",
  order = c("x", "y"),
  just = c(0, 1),
  text_x = NULL,
  text_y = element_text(angle = 0),
  background_x = NULL,
  background_y = NULL,
  by_layer_x = FALSE,
  by_layer_y = FALSE
) {

  params <- list(
    clip  = arg_match0(clip, c("on", "off", "inherit")),
    order = order,
    just  = just
  )

  given_elements = list(
    text_x = validate_element_list(text_x, "element_text"),
    text_y = validate_element_list(text_y, "element_text"),
    background_x = validate_element_list(background_x, "element_rect"),
    background_y = validate_element_list(background_y, "element_rect"),
    by_layer_x = isTRUE(by_layer_x),
    by_layer_y = isTRUE(by_layer_y)
  )

  ggproto(
    NULL, StripTag,
    params = params,
    given_elements = given_elements
  )
}

# Class -------------------------------------------------------------------

StripTag <- ggproto(
  "StripTag", StripThemed,

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
      col_vars <- layout[names(params$cols)]
      row_vars <- layout[names(params$rows)]
      layout_x <- layout_y <- layout
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

  draw_labels = function(labels, element, position, layer_id, size) {
    aes <- if (position %in% c("top", "bottom")) "x" else "y"

    labels <- Map(
      function(label, elem) {
        grob <- element_grob(elem, label, margin_x = TRUE, margin_y = TRUE)
        grob$name <- grobName(grob, paste0("strip.text.", aes))
        grob
      },
      label = labels,
      elem  = element$el
    )

    zeroes <- vapply(labels, is.zero, logical(1))
    if (length(labels) == 0 || all(zeroes)) {
      return(labels)
    }

    height <- lapply(labels[!zeroes], grobHeight)
    width  <- lapply(labels[!zeroes], grobWidth)

    if (aes == "x") {
      height <- lapply(split(height, layer_id[!zeroes]), max_height)
      height <- do.call(unit.c, height)
      width  <- unit(width_cm(width), "cm")
    } else {
      width  <- lapply(split(width, layer_id[!zeroes]), max_width)
      width  <- do.call(unit.c, width)
      height <- unit(height_cm(height), "cm")
    }

    labels <- Map(
      function(label, background) {
        x <- gTree(children = gList(background, label))
        x$name <- grobName(x, "strip")
        x
      },
      label = labels,
      background = element$bg
    )

    attr(labels, "width")  <- width
    attr(labels, "height") <- height
    labels
  },

  finish_strip = function(self, strip, width, height, position,
                          layout, dim, clip = "inherit") {

    empty <- length(strip) == 0 || all(vapply(strip, is.zero, logical(1)))

    if (!empty) {

      just <- self$params$just

      width  <- rep(width_cm(width),   length.out = length(strip))
      height <- rep(height_cm(height), length.out = length(strip))

      idx <- matrix(seq_along(strip), nrow = dim[1], ncol = dim[2])
      if (position %in% c("top", "bottom")) {
        idx <- apply(idx, 1, matrix, ncol = 1, simplify = FALSE)
      } else {
        idx <- apply(idx, 1, matrix, nrow = 1, simplify = FALSE)
      }

      strip <- lapply(idx, function(i) {
        dim <- dim(i)
        m <- matrix(strip[as.vector(i)], dim[1], dim[2])
        w <- apply(matrix( width[as.vector(i)], dim[1], dim[2]), 2, max)
        h <- apply(matrix(height[as.vector(i)], dim[1], dim[2]), 1, max)

        vp_width <- unit(sum(w), "cm")
        vp_height <- unit(sum(h), "cm")
        if (clip == "on") {
          vp_width  <- unit.pmin(vp_width, unit(1, "npc"))
          vp_height <- unit.pmin(vp_height, unit(1, "npc"))
        }
        vp <- viewport(
          x = just[1], y = just[2], just = just,
          width = vp_width, height = vp_height,
          clip = clip
        )
        gt <- gtable_matrix(
          "strip-cells", m, clip = clip,
          widths = unit(w / sum(w), "npc"),
          heights = unit(h / sum(h), "npc"),
          vp = vp
        )
        gt
      })
    }

    panel <- as.integer(layout$PANEL)
    data_frame0(
      t = panel, l = panel,
      b = panel, r = panel,
      grobs = strip
    )
  },

  incorporate_wrap = function(self, panels, position,
                              clip = "off", sizes) {
    strip <- unlist(unname(self$strips), recursive = FALSE)[[position]]
    layout <- panels$layout[grepl("^panel", panels$layout$name), , drop = FALSE]
    t <- layout$t[strip$t]
    l <- layout$l[strip$l]
    panels <- gtable_add_grob(
      panels, strip$grobs, name = paste0("strip-", seq_len(nrow(strip))),
      t = t, l = l, clip = clip
    )
    panels
  },

  incorporate_grid = function(self, panels, switch) {

    strip  <- unlist(unname(self$strips), recursive = FALSE)
    xstrip <- if (switch %in% c("x", "both")) strip$bottom else strip$top
    ystrip <- if (switch %in% c("y", "both")) strip$right  else strip$left

    if (is.null(xstrip) && is.null(ystrip)) {
      return(panels)
    } else if (is.null(xstrip)) {
      strip <- ystrip$grobs
    } else if (is.null(ystrip)) {
      strip <- xstrip$grobs
    } else {
      if (!identical(self$params$order, c("x", "y"))) {
        bind <- function(a, b) rbind(b, a)
      } else {
        bind <- function(a, b) rbind(a, b)
      }
      strip  <- Map(
        function(x, y) {
          vp <- x$vp
          vp$height <- sum(x$heights, y$heights)
          vp$width <- max(x$widths, y$widths)
          new <- bind(x, y)
          new <- editGrob(new, vp = vp)
          new
        },
        x = xstrip$grobs,
        y = ystrip$grobs
      )
    }
    layout <- panels$layout[grepl("^panel", panels$layout$name), , drop = FALSE]
    t <- layout$t[xstrip$t %||% ystrip$t]
    l <- layout$l[xstrip$l %||% ystrip$l]
    panels <- gtable_add_grob(
      panels, strip, name = paste0("strip-", seq_along(strip)),
      t = t, l = l, clip = "on", z = 2
    )
    panels
  }
)

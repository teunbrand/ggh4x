

strip_split <- function(
  clip = "inherit",
  size = "constant",
  nest = TRUE,
  position = c("top", "left"),
  bleed    = FALSE,
  text_x   = NULL,
  text_y   = NULL,
  background_x = NULL,
  background_y = NULL,
  by_layer_x = FALSE,
  by_layer_y = FALSE
) {
  params <- list(
    clip  = arg_match0(clip, c("on", "off", "inherit")),
    size  = arg_match0(size, c("constant", "variable")),
    nest  = isTRUE(nest),
    bleed = isTRUE(bleed),
    position = arg_match(position, values = c("top", "bottom", "left", "right"),
                         multiple = TRUE)
  )

  given_elements <- list(
    text_x       = validate_element_list(text_x, "element_text"),
    text_y       = validate_element_list(text_y, "element_text"),
    background_x = validate_element_list(background_x, "element_rect"),
    background_y = validate_element_list(background_y, "element_rect"),
    by_layer_x   = isTRUE(by_layer_x),
    by_layer_y   = isTRUE(by_layer_y)
  )

  ggproto(
    NULL, StripSplit,
    params = params,
    given_elements = given_elements
  )
}

# ggproto class -----------------------------------------------------------

StripSplit <- ggproto(
  "StripSplit", StripNested,

  setup = function(self, layout, params, theme, type) {
    self$elements <- self$setup_elements(theme, type)

    if (type == "wrap") {
      if (length(params$facets) == 0) {
        labels <- .int$new_data_frame(list("(all)" = "(all)"), n = 1)
      } else {
        labels <- layout[names(params$facets)]
      }
      vars <- labels
    } else {
      var_names <- union(names(params$rows), names(params$cols))
      vars      <- !duplicated(layout[var_names])

      layout <- layout[vars, ]
      vars   <- layout[var_names]

    }
    attr(vars, "facet") <- type

    self$get_strips(
      vars = structure(vars, type = "cols"),
      params$labeller, theme, params = self$params,
      layout = layout
    )
  },

  get_strips = function(
    self, vars,
    labeller, theme, params, layout
  ) {

    positions <- params$position
    style <- attr(vars, "facet")

    if (.int$empty(vars)) {
      ans <- list(x = list(top = NULL, bottom = NULL),
                  y = list(left = NULL, right = NULL))
      self$strips <- ans
      return(invisible())
    }

    labels <- match.fun(labeller)

    elem <- self$elements

    ids <- lapply(seq_len(ncol(vars)), seq_len)
    ids <- lapply(ids, function(i) .int$id(vars[, i, drop = FALSE]))
    ids <- .int$new_data_frame(setNames(ids, colnames(vars)))

    strips <- lapply(
      setNames(nm = c("top", "bottom", "left", "right")),
      function(pos) {
        if (!(pos %in% positions)) {
          return(NULL)
        }
        cn  <- colnames(vars[positions == pos])

        i   <- !duplicated(ids[positions == pos])
        lay <- layout[i, , drop = FALSE]

        out <- cbind(labels(lay[cn]))
        lab <- do.call("cbind", out)

        if (pos == "right") {
          lab <- lab[, rev(seq_len(ncol(lab)))]
        }

        strp <- self$assemble_strip(lab, pos, elem, params, lay)

        if (length(cn) == 1) {
          if (all(strp$t == strp$b)) {
            strp$b <- vapply(split(layout$ROW, ids[[cn]]), max, integer(1))
            strp$b <- layout$PANEL[match(strp$b, layout$ROW)]
          }
          if (all(strp$l == strp$r)) {
            strp$r <- vapply(split(layout$COL, ids[[cn]]), max, integer(1))
            strp$r <- layout$PANEL[match(strp$r, layout$COL)]
          }
        }
        strp
      }
    )

    self$strips <- list(
      x = list(top  = strips$top, bottom = strips$bottom),
      y = list(left = strips$left, right = strips$right)
    )
  },

  incorporate_grid = function(self, panels, switch) {

    inside  <- self$elements$inside
    padding <- self$elements$padding
    strips  <- self$strips

    pos_cols <- panels$layout[grepl("^panel-", panels$layout$name), ,
                              drop = FALSE]

    if (!is.null(strips$x$top)) {
      strip  <- strips$x$top$grobs
      table  <- strips$x$top[c("t", "b", "l", "r")]
      stripnames  <- paste0("strip-t-", seq_along(strip))
      stripheight <- split_heights_cm(strip, table$t)
      where <- pos_cols$t[table$t] - 1
      if (!inside$x) {
        where <- where - 1
        for (i in rev(unique(where))) {
          panels <- gtable_add_rows(panels, padding, i)
        }
        where <- where + match(where, unique(where)) - 1
      }
      for (w in rev(unique(where))) {
        i <- which(where == w)
        panels <- gtable_add_rows(panels, stripheight[i[1]], w)
        panels <- gtable_add_grob(
          panels, strip[i], name = stripnames[i],
          t = where[i] + 1, l = pos_cols$l[table$l][i], r = pos_cols$r[table$r][i],
          clip = "on", z = 2
        )
      }
    }
    pos_cols <- panels$layout[grepl("^panel-", panels$layout$name), ,
                              drop = FALSE]
    if (!is.null(strips$x$bottom)) {
      strip  <- strips$x$bottom$grobs
      table  <- strips$x$bottom[c("t", "b", "l", "r")]
      stripnames  <- paste0("strip-b-", seq_along(strip))
      stripheight <- split_heights_cm(strip, table$t)
      where <- pos_cols$b[table$b]
      if (!inside$x) {
        where <- where + 1
        for (i in rev(unique(where))) {
          panels <- gtable_add_rows(panels, padding, i)
        }
        where <- where + match(where, unique(where))
      }
      for (w in rev(unique(where))) {
        i <- which(where == w)
        panels <- gtable_add_rows(panels, stripheight[i[1]], w)
        panels <- gtable_add_grob(
          panels, strip[i], name = stripnames[i],
          t = where[i] + 1, l = pos_cols$l[table$l][i], r = pos_cols$r[table$r][i],
          clip = "on", z = 2
        )
      }
    }

    pos_rows <- panels$layout[grepl("^panel-", panels$layout$name), ,
                              drop = FALSE]

    if (!is.null(strips$y$left)) {
      strip  <- strips$y$left$grobs
      table  <- strips$y$left[c("t", "b", "l", "r")]
      stripnames <- paste0("strip-l-", seq_along(strip))
      stripwidth <- split_widths_cm(strip, table$l)
      where <- pos_rows$l[table$l] - 2
      if (!inside$y) {
        for (i in unique(where)) {
          panels <- gtable_add_cols(panels, padding, i)
        }
        where <- where + match(where, unique(where)) - 1
      }
      for (w in rev(unique(where))) {
        i <- which(where == w)
        panels <- gtable_add_cols(panels, stripwidth[i[1]], w)
        panels <- gtable_add_grob(
          panels, strip[i], name = stripnames[i],
          t = pos_rows$t[table$t][i], b = pos_rows$b[table$b][i],
          l = where[i] + 1,
          clip = "on", z = 2
        )
      }
    }

    pos_rows <- panels$layout[grepl("^panel-", panels$layout$name), ,
                              drop = FALSE]

    if (!is.null(strips$y$right)) {
      strip  <- strips$y$right$grobs
      table  <- strips$y$right[c("t", "b", "l", "r")]
      stripnames <- paste0("strip-r-", seq_along(strip))
      stripwidth <- split_widths_cm(strip, table$r)
      where <- pos_rows$r[table$r]
      if (!inside$y) {
        where <- where + 1
        for (i in rev(unique(where))) {
          panels <- gtable_add_cols(panels, padding, i)
        }
        where <- where + match(where, unique(where))
      }
      for (w in rev(unique(where))) {
        i <- which(where == w)
        panels <- gtable_add_cols(panels, stripwidth[i[1]], w)
        panels <- gtable_add_grob(
          panels, strip[i], name = stripnames[i],
          t = pos_rows$t[table$t][i], b = pos_rows$b[table$b][i],
          l = where[i] + 1,
          clip = "on", z = 2
        )
      }
    }

    panels
  },

  incorporate_wrap = function(self, panels, position, clip = "off", sizes) {
    self$incorporate_grid(panels, FALSE)
  }
)

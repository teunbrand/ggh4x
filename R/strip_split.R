#' @include strip_nested.R
NULL

# TODO: write tests

# Constructor -------------------------------------------------------------

#' Split strips
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This strip style allows a greater control over where a strip is placed
#' relative to the panel. Different facetting variables are allowed to be
#' placed on different sides.
#'
#' @inheritParams strip_nested
#' @param position A `character` vector stating where the strips of faceting
#'   variables should be placed. Can be some of the following: `"top"`,
#'   `"bottom"`, `"left"` or `"right"`. The length of the `position` argument
#'   must match the length of variables provided to the `facets` argument in
#'   wrap/manual layouts, or those provided to the `rows` and `cols` arguments
#'   in the grid layout.
#' @param bleed A `logical(1)` indicating whether merging of lower-layer
#'   variables is allowed when the higher-layer variables are separate. See
#'   the details of [`strip_nested`] for more info. Note that currently,
#'   `strip_split()` cannot recognise collisions between strips, so changing
#'   to `bleed = TRUE` can have unexpected results.
#'
#' @details Using this style of strip completely overrules the `strip.position`
#'   and `switch` arguments.
#'
#' @return A `StripSplit` ggproto object that can be given as an argument to
#'   facets in ggh4x.
#' @export
#' @md
#' @family strips
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # --- Wrap examples ------
#'
#' # Defaults to 1st (cyl) at top, 2nd (drv) on left
#' p + facet_wrap2(vars(cyl, drv), strip = strip_split())
#'
#' # Change cyl to left, drv to bottom
#' p + facet_wrap2(vars(cyl, drv), strip = strip_split(c("left", "bottom")))
#'
#' # --- Grid examples -----
#'
#' # Display both strips levels on the left
#' p + facet_grid2(vars(drv), vars(cyl),
#'                 strip = strip_split(c("left", "left")))
#'
#' # Separate the strips again
#' p + facet_grid2(vars(cyl, year),
#'                 strip = strip_split(c("bottom", "left")))
#'
#' # Using a dummy variable as a title strip
#' p + facet_grid2(vars(cyl, "year", year),
#'                 strip = strip_split(c("bottom", "left", "left")))
strip_split <- function(
  position = c("top", "left"),
  clip = "inherit",
  size = "constant",
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

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
StripSplit <- ggproto(
  "StripSplit", StripNested,

  setup = function(self, layout, params, theme, type) {
    self$elements <- self$setup_elements(theme, type)

    if (type == "wrap") {
      if (length(params$facets) == 0) {
        labels <- data_frame0("(all)" = "(all)", .size = 1)
      } else {
        labels <- layout[names(params$facets)]
      }
      vars <- labels
    } else {
      # Split strips don't care whether variable is for columns or rows
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
    # Early exit if we have no facetting variables
    if (empty(vars)) {
      ans <- list(x = list(top = NULL, bottom = NULL),
                  y = list(left = NULL, right = NULL))
      self$strips <- ans
      return(invisible())
    }

    # Setup some parameters/functions
    positions <- params$position
    labels    <- match.fun(labeller)
    elem      <- self$elements

    # Check position has correct length
    # TODO: See if we can do this check earlier somewhere
    if (length(positions) != ncol(vars)) {
      rlang::warn(paste0(
        "The `position` argument in `strip_split()` is being recycled ",
        "to match the length of the facetting variables,  as provided in the ",
        "`facets`, `rows`, or `cols` arguments in the facet function."
      ))
      positions <- rep_len(positions, ncol(vars))
    }

    # Construct a data.frame of IDs, it controls the hierarchy in which
    # strips can be drawn.
    ids <- lapply(seq_len(ncol(vars)), seq_len)
    ids <- lapply(ids, function(i) id(vars[, i, drop = FALSE]))
    ids <- new_data_frame(setNames(ids, colnames(vars)))

    # For every side of the panel, make a strip
    strips <- lapply(
      setNames(nm = c("top", "bottom", "left", "right")),
      function(pos) {
        if (!(pos %in% positions)) {
          return(NULL)
        }
        cn  <- colnames(vars[positions == pos])

        # Select relevant bit of the layout
        i   <- !duplicated(ids[positions == pos])
        lay <- layout[i, , drop = FALSE]

        # Format labels
        out <- cbind(labels(lay[cn]))
        lab <- do.call("cbind", out)

        if (pos == "right") {
          lab <- lab[, rev(seq_len(ncol(lab))), drop = FALSE]
        }

        strp <- self$assemble_strip(lab, pos, elem, params, lay)

        # Span strips over multiple panels if there is only a single variable
        if (length(cn) == 1) {
          if (all(strp$t == strp$b)) {
            strp$b <- vapply(split(layout$ROW, ids[[cn]]), max, integer(1))
            strp$b <- as.integer(layout$PANEL)[match(strp$b, layout$ROW)]
          }
          if (all(strp$l == strp$r)) {
            strp$r <- vapply(split(layout$COL, ids[[cn]]), max, integer(1))
            strp$r <- as.integer(layout$PANEL)[match(strp$r, layout$COL)]
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

    # TODO: There must be a smarter way than to add strips 4 times in very
    # similar but slightly different ways.
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
    # The grid way of doing this is probably robust enough to reuse it for
    # wrapped facets
    self$incorporate_grid(panels, FALSE)
  }
)

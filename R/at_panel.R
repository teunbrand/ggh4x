#' Constrain layer to panels
#'
#' This function limits the panels in which a layer is displayed. It can be
#' used to make panel-specific annotations.
#'
#' @param layer A `layer` as returned by [`layer()`][ggplot2::layer].
#'   Alternatively, a bare list of layers.
#' @param expr An `expression` that, when evaluated in the facet's layout
#'   data.frame, yields a `logical` vector parallel to the rows.
#'
#' @details
#' The `expr` argument's expression will be evaluated in the context of the
#' plot's layout. This is an internal `data.frame` structure that isn't
#' ordinarily exposed to users, so it will require some extra knowledge. For
#' most facets, the layout describes the panels with one panel per row. It
#' typically has `COL`, `ROW` and `PANEL` columns that keep track of where a
#' panel goes in a grid-layout of cells. In addition, the layout contains the
#' facetting variables provided to the `facets` or `rows` and `cols` arguments
#' of the facets. For example, if we have a plot facetted on the `var` variable
#' with the levels `A`, `B` and `C`, as 1 row and 3 columns, we might target
#' the second `B` panel iwth any of these expressions: `var == "B"`,
#' `PANEL == 2` or `COL == 2`. We can inspect the layout structure by using
#'   `ggplot_build(p)$layout$layout`, wherein `p` is a plot.
#'
#' @return A modified `layer` which will only show in some panels.
#' @export
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_grid(year ~ drv)
#'
#' anno <- annotate("text", x = 3, y = 40, label = "My text")
#'
#' # Target specific panels
#' p + at_panel(anno, PANEL %in% c(2, 4))
#'
#' # Target a variable
#' p + at_panel(anno, drv == "f")
#'
#' # Or combine variable with position
#' p + at_panel(anno, drv == "f" & ROW == 2)
at_panel <- function(layer, expr) {
  expr <- rlang::enquo(expr)
  if (quo_is_missing(expr)) {
    cli::cli_abort("{.arg expr} must be an expression, it cannot be missing.")
  }

  if (!inherits(layer, "LayerInstance")) {
    # Accept `geom_sf()`, which returns a list with a layer in first spot, and
    # other lists of layers
    if (is_bare_list(layer)) {
      is_layer <- vapply(layer, inherits, logical(1), what = "LayerInstance")
      layer[is_layer] <- lapply(layer[is_layer], at_panel, expr = !!expr)
      return(layer)
    } else {
      cli::cli_abort(
        "{.arg layer} must be a layer, not {.obj_type_friendly {layer}}."
      )
    }
  }

  old_geom <- layer$geom
  new_geom <- ggproto(
    NULL, old_geom,
    draw_layer = function(self, data, params, layout, coord) {
      # Evaluate expression masked by plot layout
      panels <- layout$layout
      keep   <- as.logical(eval_tidy(expr, panels))

      # Select panels to keep
      keep   <- rep_len(keep, nrow(panels))
      panels <- panels$PANEL[keep]

      # Subset and pass to parent
      data <- vec_slice(data, data$PANEL %in% panels)
      ggproto_parent(old_geom, self)$draw_layer(data, params, layout, coord)
    }
  )

  ggproto(NULL, layer, geom = new_geom)
}

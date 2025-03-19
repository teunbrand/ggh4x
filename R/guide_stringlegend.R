# Constructor -------------------------------------------------------------

#' String legend
#'
#' This type of legend shows colour and fill mappings as coloured text. It does
#' not draw keys as `guide_legend()` does.
#'
#' @inheritParams ggplot2::guide_legend
#'
#' @return A `GuideStringlegend` object.
#' @export
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = manufacturer))
#'
#' # String legend can be set in the `guides()` function
#' p + guides(colour = guide_stringlegend(ncol = 2))
#'
#' # The string legend can also be set as argument to the scale
#' p + scale_colour_viridis_d(guide = "stringlegend")
guide_stringlegend <- function(
  title = waiver(),
  theme = NULL,
  position = NULL,
  direction = NULL,
  nrow = NULL,
  ncol = NULL,
  reverse = FALSE,
  order = 0
) {
  new_guide(
    title = title,
    theme = theme,
    direction = direction,
    nrow = nrow, ncol = ncol,
    reverse = reverse,
    order = order,
    position = position,
    available_aes = c("colour", "fill", "family", "fontface"),
    name = "stringlegend",
    super = GuideStringlegend
  )
}

# Class -------------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
GuideStringlegend <- ggproto(
  "GuideStringlegend", GuideLegend,

  get_layer_key = function(params, ...) {
    params
  },

  setup_params = function(params) {
    params <- GuideLegend$setup_params(params)
    params$sizes <- list(widths = 0, heights = 0)
    params
  },

  setup_elements = function(params, elements, theme) {
    theme <- theme + params$theme
    params$theme <- NULL
    elements <- GuideLegend$setup_elements(params, elements, theme)
    elements$spacing_y <- calc_element("legend.key.spacing.y", theme)
    elements$text$margin <- calc_element("legend.text", theme)$margin
    elements$key_height <- elements$key_width <- unit(0, "cm")
    elements
  },

  build_labels = function(key, elements, params) {
    n_labels <- length(key$.label)
    if (n_labels < 1) {
      out <- rep(list(zeroGrob()), nrow(key))
      return(out)
    }
    colour <- key$colour %||% key$fill
    lapply(
      seq_along(key$.label),
      function(i) {
        text <- element_grob(
          elements$text, label = key$.label[i],
          colour = colour[i],
          family = key$family[i],
          face   = key$fontface[i],
          margin_x = TRUE,
          margin_y = TRUE
        )
      }
    )
  },

  build_decor = function(...) zeroGrob()
)

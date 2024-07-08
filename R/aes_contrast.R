#' Get contrast
#'
#' @param ... Provided to force user argument naming etc.
#' @param fill A fill aesthetic from which to determine the colour scale for contrast.
#' @param dark A dark colour. Defaults to `"black"`.
#' @param light A light colour. Defaults to `"white"`.
#'
#' @noRd
#'
#' @examples
#' get_contrast(fill = c("navy", "yellow", "orange"), dark = "black", light = "white")
#'
get_contrast <- function(fill, dark = "black", light = "white") {
  out <- rep(dark, length(fill))
  channel <- farver::get_channel(fill, "l", space = "hcl")
  out[channel < 50] <- light
  out
}

#' An auto-contrast colour aesthetic
#'
#' @description A colour aesthetic for annotation that automatically contrasts with fill. Can be spliced into [ggplot2::aes] with [rlang::!!!].
#'
#' @param ... Provided to force user argument naming etc.
#' @param dark A dark colour.
#' @param light A light colour.
#'
#' @return An aesthetic
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' data.frame(x = c("A", "B", "C"), y = c(3, 4, 5)) |>
#'   ggplot(aes(x = x, y = y, fill = x)) +
#'   geom_col(position = position_dodge2(preserve = "single"), width = 0.75, ) +
#'   scale_fill_manual(values = c("navy", "orange", "hotpink")) +
#'   geom_text(
#'     mapping = aes(label = y, !!!aes_contrast()),
#'     position = position_dodge2(width = 0.75, preserve = "single"),
#'     vjust = 1.33,
#'     show.legend = FALSE,
#'   )
#'
aes_contrast <- function(..., dark = "#121B24FF", light = "#FFFFFFFF") {

  ggplot2::aes(
    colour = ggplot2::after_scale(
      get_contrast(.data$fill, dark = dark, light = light)
    )
  )
}

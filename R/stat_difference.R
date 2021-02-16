# Constructor -------------------------------------------------------------

#' Difference ribbon
#'
#' This makes a ribbon that is filled depending on whether the `max` is
#' higher than `min`. This can be useful for displaying differences
#' between two series.
#'
#' @inherit ggplot2::stat_density
#' @param levels A `character(3)` indicating factor levels for the `fill`
#'   aesthetic for the following cases (1) `max > min` (2) `max < min`
#'   (3) `max == min`. Will be padded with `NA`s when necessary.
#'
#' @return A `Layer` object that can be added to a plot.
#' @export
#'
#' @details The stat may reorder the `group` aesthetic to accommodate two
#' different fills for the signs of differences. The stat takes care to
#' interpolate a series whenever a crossover between `max` and `min` series
#' happens. This makes the ribbon not look stumpy at these crossovers.
#'
#' @eval ggplot2:::rd_aesthetics("geom", "ribbon")
#' @section Computed variables: \describe{
#'  \item{`sign`}{A `factor` with the `levels` attribute set to the `levels`
#'  argument.}
#' }
#'
#' @md
#' @examples
#' set.seed(2021)
#' df <- data.frame(
#'   x = 1:100,
#'   y = cumsum(rnorm(100)),
#'   z = cumsum(rnorm(100))
#' )
#'
#' ggplot(df, aes(x = x)) +
#'   stat_difference(aes(ymin = y, ymax = z), alpha = 0.3) +
#'   geom_line(aes(y = y, colour = "min")) +
#'   geom_line(aes(y = z, colour = "max"))
stat_difference <- function(
  mapping = NULL,
  data = NULL,
  geom = "ribbon",
  position = "identity",
  ...,
  levels = c("+", "-", "0"),
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDifference,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      levels = levels,
      ...
    )
  )
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
StatDifference <- ggproto(
  "StatDifference", Stat,
  required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),
  default_aes = aes(fill = after_stat(sign)),
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(
      data, params, main_is_orthogonal = FALSE, main_is_continuous = TRUE
    )
    if ({len <- length(params$levels)} != 3) {
      if (len < 3) {
        params$levels <- c(params$levels, paste0("NA", seq_len(3 - len)))
      } else {
        params$levels <- params$levels[1:3]
      }
    }
    params
  },
  extra_params = c("na.rm", "orientation", "levels"),
  compute_panel = function(self, data, scales, ...) {
    data <- ggproto_parent(Stat, self)$compute_panel(
      data, scales, ...
    )
    data$group <- cumsum(data$id)
    data$id <- NULL
    data
  },
  compute_group = function(data, scales, levels = c("+", "-", "0"),
                           na.rm = FALSE, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    data$sign <- sign(data$ymax - data$ymin)
    data <- data[order(data$x),]
    data$id <- rle_id(data$sign)

    crossover <- which(c(FALSE, diff(data$id) == 1))
    crossover <- sort(c(crossover, crossover - 1))
    splitter  <- rep(seq_len(length(crossover) / 2), each = 2)
    crossover <- lapply(split(data[crossover, ], splitter), find_isect)

    data <- vctrs::vec_rbind(data, !!!crossover)
    data <- data[order(data$x, data$id), ]
    data$id <- c(TRUE, diff(data$id) > 0)
    data$sign <- factor(data$sign, levels = c("1", "-1", "0"),
                        labels = levels)
    data$flipped_aes <- flipped_aes
    flip_data(data, flipped_aes)
  }
)

# Helpers -----------------------------------------------------------------

rle_id <- function(x) {
  with(rle(x), rep.int(seq_along(values), lengths))
}

# Simplified version of
# https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
# We can simplify a bit because x[3] == x[1] and x[4] == x[2]
find_isect <- function(df) {
  list2env(df, envir = rlang::current_env())
  dx <- x[1] - x[2]
  dy <- ymin[1] - ymin[2]
  t <- (-1 * (ymin[1] - ymax[1]) * dx) / (dx * (ymax[1] - ymax[2]) - dy * dx)
  df$x <- x[1] + t * -dx
  df$ymin <- df$ymax <- ymin[1] + t * -dy
  return(df)
}

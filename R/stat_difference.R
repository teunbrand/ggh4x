# Constructor -------------------------------------------------------------

#' Difference ribbon
#'
#' This makes a ribbon that is filled depending on whether the `max` is
#' higher than `min`. This can be useful for displaying differences
#' between two series.
#'
#' @inheritParams ggplot2::stat_density
#' @param geom Use to override the default connection between
#'   `geom_ribbon()` and `stat_difference()`.
#' @param levels A `character(2)` indicating factor levels for the `fill`
#'   aesthetic for the cases where (1) `max > min` and where (2) `max < min`.
#'
#' @return A `Layer` object that can be added to a plot.
#' @export
#'
#' @details The stat may reorder the `group` aesthetic to accommodate two
#' different fills for the signs of differences. The stat takes care to
#' interpolate a series whenever a crossover between `max` and `min` series
#' happens. This makes the ribbon not look stumpy at these crossovers.
#'
#' @note
#' When there is a run of more than two 0-difference values, the inner values
#' will be ignored.
#'
#' @eval fixup_docs(ggplot2:::rd_aesthetics("geom", "ribbon"))
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
  levels = c("+", "-"),
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatDifference,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm       = na.rm,
      orientation = orientation,
      levels      = levels,
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
    params
  },
  extra_params = c("na.rm", "orientation", "levels"),
  compute_panel = function(self, data, scales, flipped_aes = FALSE, ...) {
    data <- flip_data(data, flipped_aes)
    data <- ggproto_parent(Stat, self)$compute_panel(
      data, scales, ...
    )
    data$group <- cumsum(data$id)
    data$id <- NULL
    data$flipped_aes <- flipped_aes
    flip_data(data, flipped_aes)
  },
  compute_group = function(data, scales, levels = c("+", "-"),
                           na.rm = FALSE, flipped_aes = FALSE) {
    data <- data[order(data$x),]
    y <- data$ymax - data$ymin
    data$sign <- sign(y)
    sign_rle <- vec_unrep(data$sign)

    # Find crossing points
    ends  <- cumsum(sign_rle$times)
    dups  <- ends[-length(ends)]
    cross <- -y[dups] * (data$x[dups + 1] - data$x[dups]) /
      (y[dups + 1] - y[dups]) + data$x[dups]

    # Interpolate at cross points
    x    <- vec_rep_each(cross, 2)
    ymin <- approx(data$x, data$ymin, x)$y
    ymax <- approx(data$x, data$ymax, x)$y

    # Match metadata
    sign  <- vec_rep_each(sign_rle$key, 2)
    sign  <- sign[-c(1, length(sign))]
    id    <- rep(c(0, 1), length(cross))
    ord   <- cumsum(id) + 1
    data_ord <- vec_rep_each(seq_along(sign_rle$times), sign_rle$times)

    new <- data_frame0(
      x    = c(data$x, x),
      ymin = c(data$ymin, ymin),
      ymax = c(data$ymax, ymax),
      ord  = c(data_ord, ord),
      id   = c(1, rep(0, nrow(data) - 1), id), # Will become group later
      sign = c(data$sign, sign)
    )
    new <- vec_slice(new, order(new$ord, new$x))
    new <- vec_slice(new, new$sign != 0)
    new$sign <- factor(new$sign, levels = c("1", "-1"), labels = levels[1:2])
    new$ord <- NULL
    new
  }
)

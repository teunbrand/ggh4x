# Constructor -------------------------------------------------------------

#' Rolling Kernel
#'
#' A rolling kernel moves along one of the axes and assigns weights to
#' datapoints depending on the distance to the kernel's location. It then
#' calculates a weighted average on the y-values of the datapoints, creating a
#' trendline. In contrast to (weighted) rolling averages, the interval between
#' datapoints do not need to be constant.
#'
#' @inheritParams ggplot2::stat_density
#' @param geom Use to override the default geom (\code{"line"}).
#' @param bw A bandwidth, which can be one of the following: \itemize{ \item A
#'   \code{numeric} of length one indicating a measure of kernel width, in data
#'   units. \item A \code{rel} object of length one constructed for setting a
#'   bandwidth relative to the group data range. Can be constructed with the
#'   \code{rel()} function. \item A \code{character} of length one, naming one
#'   of the functions documented in \code{\link[stats:bandwidth]{bw.nrd}()}. }
#' @param kernel One of the following: \itemize{ \item A \code{function} that
#'   takes a vector of distances as first argument, a numeric bandwidth as
#'   second argument and returns relative weights. \item A \code{character} of
#'   length one that can take one of the following values: \describe{
#'   \item{\code{"gaussian"} or \code{"norm"}}{A kernel that follows a normal
#'   distribution with 0 mean and bandwidth as standard deviation.}
#'   \item{\code{"mean"} or \code{"unif"}}{A kernel that follows a uniform
#'   distribution with \eqn{bandwidth * -0.5} and \eqn{bandwidth * 0.5} as
#'   minimum and maximum. This is similar to a simple, unweighted moving
#'   average.} \item{\code{"cauchy"}}{A kernel that follows a Cauchy
#'   distribution with 0 as location and bandwidth as scale parameters. The
#'   Cauchy distribution has fatter tails than the normal distribution.} } }
#' @param n An \code{integer} of length one: how many points to return per
#'   group.
#' @param expand A \code{numeric} of length one: how much to expand the range
#'   for which the rolling kernel is calculated beyond the most extreme
#'   datapoints.
#' @param orientation A \code{character} of length one, either \code{"x"}
#'   (default) or \code{"y"}, setting the axis along which the rolling should
#'   occur.

#' @section Aesthetics: \code{stat_rollingkernel()} understands the following
#'   aesthetics (required aesthetics are in bold)
#'   \itemize{
#'     \item \strong{x}
#'     \item \strong{y}
#'     \item group
#'   }
#'
#' @section Computed variables: \describe{
#'  \item{\code{x}}{A sequence of ordered x positions.}
#'  \item{\code{y}}{The weighted value of the rolling kernel.}
#'  \item{\code{weight}}{The sum of weight strengths at a position.}
#'  \item{\code{scaled}}{The fraction of weight strengths at a position. This is
#'  the same as \code{weight / sum(weight)} by group.}
#' }
#'
#' @return A \emph{Layer} ggproto object.
#' @export
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point() +
#'   stat_rollingkernel()
#'
#' # The (scaled) weights can be used to emphasise data-dense areas
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point() +
#'   stat_rollingkernel(aes(alpha = after_stat(scaled)))
stat_rollingkernel <-
  function(
    mapping = NULL,
    data = NULL,
    geom = "line",
    position = "identity",
    ...,
    bw = "nrd",
    kernel = "gaussian",
    n = 256,
    expand = 0.1,
    na.rm = FALSE,
    orientation = "x",
    show.legend = NA,
    inherit.aes = TRUE
  ) {
    layer(
      data = data,
      mapping = mapping,
      stat = "rollingkernel",
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        bw = bw,
        kernel = kernel,
        n = n,
        expand = expand,
        na.rm = na.rm,
        orientation = orientation,
        ...
      )
    )
  }

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
StatRollingkernel <- ggproto(
  "StatRollingkernel", Stat,
  required_aes = c("x", "y"),
  setup_params = function(data, params, scales) {
    params$flipped_aes <- isTRUE(params$orientation == "y")
    if (is.character(params$kernel)) {
      params$kernel <- switch(
        params$kernel,
        "gaussian" = .kernel_norm,
        "norm" = .kernel_norm,
        "unif" = .kernel_unif,
        "mean" = .kernel_unif,
        "cauchy" = .kernel_cauchy,
        stop("unknown kernel specification")
      )
    }
    params
  },
  extra_params = c("na.rm", "orientation"),
  compute_group = function(data, scales, n = 256,
                           bw = 0.02, expand = 0, kernel = .kernel_norm,
                           flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)

    if (inherits(bw, "rel")) {
      bw <- bw * diff(range(data[["x"]]))
    } else if (is.character(bw)) {
      i <- data[["x"]]
      bw <- switch(
        tolower(bw),
        nrd0 = bw.nrd0(i),
        nrd  = bw.nrd(i),
        ucv  = bw.ucv(i),
        bcv  = bw.bcv(i),
        sj   = ,
        `sj-ste` = bw.SJ(i, method = "ste"),
        `sj-dpi` = bw.SJ(i, method = "dpi"),
        stop("unknown bandwidth rule"))
    }

    data <- data[is.finite(data$x) & is.finite(data$y),]

    # Get panel and data range
    dat_range <- range(data$x, na.rm = TRUE)
    mid <- (dat_range[1] + dat_range[2]) / 2
    expand <- (1 + expand) * (0.5 * diff(dat_range))


    seq_range <- seq(mid - expand, mid + expand, length.out = n)

    krnl <- outer(data$x, seq_range, "-")
    krnl[] <- kernel(krnl, bw)
    krnl <- t(t(krnl) / {weight <- colSums(krnl)})
    krnl <- krnl * data$y
    y <- colSums(krnl)

    data <- data.frame(x = seq_range, y = y,
                       weight = weight, scaled = weight / sum(weight))
    flip_data(data, flipped_aes)
  }
)

# Helpers -----------------------------------------------------------------

# Kernels -----------------------------------------------------------------

.kernel_norm <- function(x, bw) {
  dnorm(x, sd = bw)
}

.kernel_unif <- function(x, bw) {
  dunif(x, min = -0.5 * bw, max = 0.5 * bw)
}

.kernel_cauchy <- function(x, bw) {
  dcauchy(x, scale = bw)
}

# Backup kernels ----------------------------------------------------------

# These just don't seem to work that well for reasons beyond my understanding

# .kernel_triangular <- function(x, bw) {
#   bw <- bw * sqrt(6)
#   ax <- abs(x)
#   ifelse(ax < bw, (1 - ax/bw)/bw, 0)
# }
#
# .kernel_epanechnikov <- function(x, bw) {
#   bw <- bw * sqrt(5)
#   ax <- abs(x)
#   ifelse(ax < bw, 3/4 * (1 - (ax/bw)^2)/bw, 0)
# }
#
# .kernel_biweight <- function(x, bw) {
#   bw <- bw * sqrt(7)
#   ax <- abs(x)
#   ifelse(ax < bw, 15/16 * (1 - (ax/bw)^2)^2/bw, 0)
# }
#
# .kernel_cosine <- function(x, bw) {
#   bw <- bw / sqrt(1/3 - 2/pi^2)
#   ifelse(abs(x) < bw, (1 + cos(pi * x/bw))/(2 * a), 0)
# }
# .kernel_optcosine <- function(x, bw) {
#   bw <- bw / sqrt(1 - 8/pi^2)
#   ifelse(abs(x) < bw, pi/4 * cos(pi * x/(2 * bw))/bw, 0)
# }
#
# .kernel_laplace <- function(x, bw) {
#   (1 / (2 * bw)) * exp(-(abs(x)/bw))
# }

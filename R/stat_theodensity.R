# Main function -----------------------------------------------------------

#' Fitted theoretical density
#'
#' @description Estimates the parameters of a given distribution and evaluates
#'   the probability density function with these parameters. This can be useful
#'   for comparing histograms or kernel density estimates against a theoretical
#'   distribution.
#'
#' @inheritParams ggplot2::geom_density
#' @param distri A `character` of length 1 naming a distribution without
#'   prefix. See details.
#' @param n An `integer` of length 1 with the number of equally spaced
#'   points at which the density function is evaluated. Ignored if distribution
#'   is discrete.
#' @param fix.arg An optional named list giving values of fixed parameters of
#'   the named distribution. Parameters with fixed value are not estimated by
#'   maximum likelihood procedures.
#' @param start.arg A named list giving initial values of parameters for the
#'   named distribution. This argument may be omitted (default) for some
#'   distributions for which reasonable starting values are computed.
#' @param geom Use to override the default geom for `stat_theodensity`.
#'
#' @details Valid `distri` arguments are the names of distributions for
#'   which there exists a density function. The names should be given without a
#'   prefix (typically 'd', 'r', 'q' and 'r'). For example: `"norm"` for
#'   the normal distribution and `"nbinom"` for the negative binomial
#'   distribution. Take a look at [`distributions()`][stats::Distributions] in the
#'   \pkg{stats} package for an overview.
#'
#'   There are a couple of distribution for which there exist no reasonable
#'   starting values, such as the Student t-distribution and the F-distribution.
#'   In these cases, it would probably be wise to provide reasonable starting
#'   values as a named list to the `start.arg` argument. When estimating a
#'   binomial distribution, it would be best to supply the `size` to the
#'   `fix.arg` argument.
#'
#'   By default, the y values are such that the integral of the distribution is
#'   1, which scales well with the defaults of kernel density estimates. When
#'   comparing distributions with absolute count histograms, a sensible choice
#'   for aesthetic mapping would be `aes(y = stat(count) * binwidth)`,
#'   wherein `binwidth` is matched with the bin width of the histogram.
#'
#'   For discrete distributions, the input data are expected to be integers, or
#'   doubles that can be divided by 1 without remainders.
#'
#'   Parameters are estimated using the
#'   [fitdistrplus::fitdist()]`()` function in the
#'   \pkg{fitdistrplus} package using maximum likelihood estimation.
#'   Hypergeometric and multinomial distributions from the \pkg{stats} package
#'   are not supported.
#'
#' @section Computed variables: \describe{ \item{density}{probability density}
#'   \item{count}{density * number of observations - useful for comparing to
#'   histograms} \item{scaled}{density scaled to a maximum of 1} }
#'
#' @seealso [stats::Distributions()]
#'   [fitdistrplus::fitdist()] [ggplot2::geom_density()]
#'   [ggplot2::geom_histogram()]
#'
#' @export
#'
#' @return A *Layer* ggproto object.
#'
#' @examples
#' # A mixture of normal distributions where the standard deviation is
#' # inverse gamma distributed resembles a cauchy distribution.
#' x <- rnorm(2000, 10, 1/rgamma(2000, 2, 0.5))
#' df <- data.frame(x = x)
#'
#' ggplot(df, aes(x)) +
#'   geom_histogram(binwidth = 0.1,
#'                  alpha = 0.3, position = "identity") +
#'   stat_theodensity(aes(y = stat(count) * 0.1, colour = "Normal"),
#'                    distri = "norm", geom = "line") +
#'   stat_theodensity(aes(y = stat(count) * 0.1, colour = "Cauchy"),
#'                    distri = "cauchy", geom = "line") +
#'   coord_cartesian(xlim = c(5, 15))
#'
#' # A negative binomial can be understood as a Poisson-gamma mixture
#' df <- data.frame(x = c(rpois(500, 25),
#'                        rpois(500, rgamma(500, 5, 0.2))),
#'                  cat = rep(c("Poisson", "Poisson-gamma"), each = 500))
#'
#' ggplot(df, aes(x)) +
#'   geom_histogram(binwidth = 1, aes(fill = cat),
#'                  alpha = 0.3, position = "identity") +
#'   stat_theodensity(aes(y = stat(count), colour = cat), distri = "nbinom",
#'                    geom = "step", position = position_nudge(x = -0.5)) +
#'   stat_summary(aes(y = x, colour = cat, x = 1),
#'                fun.data = function(x){data.frame(xintercept = mean(x))},
#'                geom = "vline")
stat_theodensity <- function(
  mapping = NULL, data = NULL,
  geom = "line", position = "identity",
  ...,
  distri = "norm", n = 512,
  fix.arg = NULL,
  start.arg = NULL,
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (!exists(paste0("d", distri), mode = "function")) {
    cli::cli_abort(paste0(
      "The {.arg distri} argument must have a valid density function called",
      "{.fn d{distri}}."
    ))
  }
  if (distri %in% c("multinom", "hyper", "wilcox", "signrank")) {
    cli::cli_abort(
      "{.fn stat_theodensity} does not support the '{distri}' distribution."
    )
  }
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatTheoDensity,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list2(
      distri    = distri,
      n         = n,
      fix.arg   = fix.arg,
      start.arg = start.arg,
      na.rm     = na.rm,
      ...
    )
  )
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
StatTheoDensity <- ggproto(
  "StatTheoDensity",
  StatDensity,
  compute_group = function(
    data, scales, distri = "norm", n = 512, distri_type = "continuous",
    fix.arg = NULL, start.arg = NULL
  ) {
    check_installed("fitdistrplus", "for `stat_theodensity()`.")
    # Data to return upon failure
    nulldata <- data.frame(
      x = NA_real_,
      density = NA_real_,
      ndensity = NA_real_,
      count = NA_real_,
      n = NA_integer_
    )

    dfun <- get(paste0("d", distri), mode = "function")
    x <- data$x
    x <- x[!is.na(x)]
    nx <- length(data$x)

    if (nx < 2) {
      cli::cli_warn("Groups with fewer than two data points have been dropped.")
      return(nulldata)
    }

    range <- scales$x$dimension()
    if (distri_type == "discrete") {
      xseq <- seq(floor(range[1]), ceiling(range[2]), by = 1)
    } else {
      xseq <- seq(range[1], range[2], length.out = n)
    }

    par_est <- suppressWarnings(coef(fitdistrplus::fitdist(x, distri,
                                                           start = start.arg,
                                                           fix.arg = fix.arg)))
    par_est <- c(par_est, unlist(fix.arg))

    if (any(is.na(par_est) | is.nan(par_est) |
            !is.finite(par_est) | is.null(par_est))) {
      cli::cli_warn("Failed to estimate parameters of '{distri}' distribution.")
      return(nulldata)
    }

    dens <- do.call(dfun, c(list(x = xseq), as.list(par_est)))

    data.frame(
      x = xseq,
      density = dens,
      scaled = dens / max(dens, na.rm = TRUE),
      count = dens * nx,
      n = nx
    )
  },
  setup_params = function(data, params) {
    dtype <- class_distri(params$distri)
    if (dtype == "discrete") {
      if (sum(abs(data$x %% 1)) > 0) {
        cli::cli_abort(paste0(
          "A discrete '{params$distri}' distribution cannot be fitted ",
          "to continuous data."
        ))
      }
    }
    params <- c(params, distri_type = dtype)

    # Chi square estimator causes trouble, estimate as gamma with rate = 0.5
    if (params$distri == "chisq") {
      params$distri <- "gamma"
      if (is.null(params$fix.arg)) {
        params$fix.arg <- list(rate = 0.5)
      } else {
        params$fix.arg <- list(shape = params$fix.arg$df / 2, rate = 0.5)
      }
    }
    # Binom does not operate without size
    if (params$distri == "binom") {
      if (is.null(params$fix.arg)) {
        params$fix.arg <- list(size = max(data$x))
        cli::cli_inform(
          "Estimating binomial PMF with {.field size} set to maximum data value."
        )
      }
      params$start.arg <- list(prob = mean(data$x) / max(data$x))
    }
    params
  }
)

# Helper functions --------------------------------------------------------

#' @keywords internal
class_distri <- function(distri) {
  # Is distri discrete?
  discrete_distris <- c("pois", "nbinom", "binom", "geom", "hyper",
                        "signrank", "multinom", "wilcox")
  if (distri %in% discrete_distris) {
    return("discrete")
  }

  # Is distri continuous?
  conti_distris <- c("beta", "cauchy", "chisq", "exp", "f", "gamma",
                     "lnorm", "norm", "t", "unif", "weibull", "logis")
  if (distri %in% conti_distris) {
    return("continuous")
  }

  # Empirical test for functions outside the stats package
  rfun <- dynGet(paste0("r", distri), inherits = TRUE)
  # rfun <- get(paste0("r", distri), mode = "function")
  routput <- tryCatch({
    do.call(rfun, as.list(c(100, rep(1, length(formals(rfun)) - 1))))
  },
  warning = function(cond) {
  },
  error = function(cond) {
    return(NULL)
  })
  if (is.integer(routput) | sum(routput %% 1) == 0) {
    return("discrete")
  } else if (is.numeric(routput)) {
    return("continuous")
  } else {
    cli::cli_abort(paste0(
      "{.fn stat_theodensity} failed to determine if the '{distri}' ",
      "distribution is discrete or continuous."
    ))
  }
}

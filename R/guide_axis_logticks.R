# User function -----------------------------------------------------------

#' Axis guide with ticks for logarithmic breaks
#'
#' This axis guide is probably best described as
#' \code{\link[ggplot2]{annotation_logticks}} but implemented as a guide instead
#' of a geom. The tick marks probably best suit log10 transformations.
#'
#' @inheritParams guide_axis_minor
#' @param prescaled A \code{logical} of length one, indicating wether the data
#'   has been manually rescaled (\code{TRUE}) or the scale takes care of the
#'   transformation (\code{FALSE}).
#' @param base When this is provided, the guide takes this as the base for the
#'   log transformation instead of trying to guess the base. It is recommended
#'   to use this argument if the base is not \code{10}.
#'
#' @details The length of minor ticks can be controlled relative to the length
#'   of major ticks by setting \code{ggh4x.axis.ticks.length.minor} as a
#'   \code{rel} object. Likewise the size of the smallest ticks are controlled
#'   by the \code{ggh4x.axis.ticks.length.minor}, also relative to the major
#'   ticks.
#'
#' @return An \emph{axis_logticks} guide class object.
#' @export
#'
#' @family axis-guides
#'
#' @examples
#' # The guide works well out of the box with log10 scales
#' p <- ggplot(pressure, aes(temperature, pressure)) +
#'   geom_line()
#' p + scale_y_log10(guide = "axis_logticks")
#'
#' # If the data is already transformed, you can set 'prescaled' to TRUE
#' ggplot(pressure, aes(temperature, log10(pressure))) +
#'   geom_line() +
#'   guides(y = guide_axis_logticks(prescaled = TRUE))
#'
#' # The lenghts of the log-ticks are controlled by the theme relative to the
#' # major ticks.
#' p + scale_y_log10(guide = "axis_logticks") +
#'   theme(
#'     axis.ticks.length.y = unit(1, "cm"),
#'     ggh4x.axis.ticks.length.minor = rel(0.55),
#'     ggh4x.axis.ticks.length.mini = rel(0.1)
#'   )
guide_axis_logticks <- function(
  title = waiver(),
  check.overlap = FALSE,
  angle = NULL,
  n.dodge = 1,
  order = 0,
  position = waiver(),
  prescaled = FALSE,
  trunc_lower = NULL,
  trunc_upper = NULL,
  base = waiver()
) {
  check_trunc_arg(trunc_lower, trunc_upper)
  structure(list(
    title = title,
    check.overlap = check.overlap,
    angle = angle,
    n.dodge = n.dodge,
    order = order,
    position = position,
    available_aes = c("x", "y"),
    name = "axis",
    prescaled = prescaled,
    trunc_lower = trunc_lower,
    trunc_upper = trunc_upper,
    base = base
  ), class = c("guide", "axis_logticks", "axis_minor",
               "axis_truncated", "axis"))
}

# Internals ---------------------------------------------------------------

#' @export
#' @noRd
guide_train.axis_logticks <- function(guide, scale, aesthetic = NULL) {
  aesthetic <- aesthetic %||% scale$aesthetics[1]

  # Determine base
  if (inherits(guide$base, "waiver")) {
    if (guide$prescaled) {
      # There is no way to know really, 10 seems reasonable default
      base <- 10
    } else {
      # Guess base
      base <- c(1, 2, exp(1), 10)
      transbase <- scale$scale$trans$transform(base)
      base <- base[head(which(transbase == 1), 1)]
    }
  } else {
    base <- guide$base
  }

  limits <- scale$continuous_range

  # Calculate the breaks
  logbreaks <- make_logbreaks(
    base = base,
    minpow = floor(limits[1]),
    maxpow = ceiling(limits[2])
  )

  # Transform breaks
  minority  <- logbreaks$minority
  if (!guide$prescaled) {
    logbreaks <- scale$scale$trans$transform(logbreaks$value)
  } else {
    logbreaks <- log(logbreaks$value, base)
  }

  # Discard out-of-bounds
  keep <- logbreaks >= limits[1] & logbreaks <= limits[2]
  logbreaks <- logbreaks[keep]
  minority  <- minority[keep]

  major_breaks <- scale$get_breaks()
  breaks <- c(major_breaks, logbreaks)
  is_major <- rep(c(TRUE, FALSE),
                  c(length(major_breaks), length(logbreaks)))


  empty_ticks <- .int$new_data_frame(
    list(aesthetic = numeric(), .value = numeric(0), .label = character())
  )

  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    warning("axis_logticks guide needs appropriate scales: ",
            guide$available_aes)
    guide$key <- empty_ticks
  } else if (length(breaks) == 0) {
    guide$key <- empty_ticks
  } else {
    mapped_breaks <- if (scale$is_discrete()) {
      scale$map(breaks)
    } else {
      breaks
    }
    ticks <- .int$new_data_frame(setNames(list(mapped_breaks),
                                          aesthetic))
    ticks$.value <- breaks
    ticks$.label <- ""
    ticks$.label[is_major] <- scale$get_labels(breaks[is_major])

    ticks$.minority <- c(rep(0, length(major_breaks)), minority)

    guide$key <- ticks[is.finite(ticks[[aesthetic]]), ]
  }
  guide$name <- paste0(guide$name, "_", aesthetic)
  guide$hash <- digest::digest(list(guide$title, guide$key$.value,
                                    guide$key$.label, guide$name))
  guide <- truncate_guide(guide, scale, aesthetic)
  guide
}

# Helpers -----------------------------------------------------------------

# based on the annotation_logticks code
make_logbreaks <- function(base = 10, ticks_per_base = base - 1,
                          minpow = 0, maxpow = minpow + 1) {

  # Number of blocks of tick marks
  reps <- maxpow - minpow

  # For base 10: 1, 2, 3, ..., 7, 8, 9, 1, 2, ...
  ticknums  <- rep(seq(1, base - 1, length.out = ticks_per_base), reps)

  # For base 10: 1, 1, 1, ..., 1, 1, 1, 2, 2, ... (for example)
  powers <- rep(seq(minpow, maxpow - 1), each = ticks_per_base)

  ticks  <- ticknums * base ^ powers
  ticks  <- c(ticks, base ^ maxpow)  # Add the last tick mark

  # Set all ticks to mini
  minority <- rep(2, length(ticks))

  # Get the position within each cycle, 0, 1, 2, ..., 8, 0, 1, 2. ...
  cycleIdx <- ticknums - 1

  # Set the "major" ticks
  minority[cycleIdx == 0] <- 0

  # Where to place the longer tick marks that are between each base
  # For base 10, this will be at each 5
  mediumticks <- floor(ticks_per_base/2)
  minority[ cycleIdx == mediumticks ] <- 1

  tickdf <- .int$new_data_frame(list(value = ticks, minority = minority),
                           n = length(ticks))

  return(tickdf)
}

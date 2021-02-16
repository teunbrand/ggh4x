# Constructor -------------------------------------------------------------

#' @title Run length encoding
#'
#' @description Run length encoding takes a vector of values and calculates the
#' lengths of consequetive repeated values.
#'
#' @inheritParams ggplot2::stat_density
#' @param align A \code{character} of length one that effect the computed
#' \code{start} and \code{end} variables. One of the following:
#' \describe{
#'   \item{\code{"none"}}{Take exact start and end \code{x} values.}
#'   \item{\code{"center"}}{Return start and end \code{x} values in between an
#'   end and the subsequent start.}
#'   \item{\code{"start"}}{Align start values with previous end values.}
#'   \item{\code{"end"}}{Align end values with next start values.}
#' }
#'
#' @details The data is first ordered on the \code{x} aesthetic before run
#' lengths are calculated for the \code{label} aesthetic.
#'
#' @section Aesthetics: \code{stat_rle()} understands the following
#'   aesthetics (required aesthetics are in bold)
#' \itemize{
#'   \item{\strong{x}}
#'   \item{\strong{label}}
#'   \item{group}
#' }
#'
#' @section Computed variables:
#' \describe{
#'   \item{start}{The \code{x} values at the start of every run.}
#'   \item{end}{The \code{x} values at the end of every run.}
#'   \item{start_id}{The index where a run starts.}
#'   \item{end_id}{The index where a run ends.}
#'   \item{run_id}{The index of a run.}
#'   \item{runlength}{The length of a run.}
#'   \item{runvalue}{The value associated with a run.}
#' }
#'
#' @return A \code{ggplot2} layer
#' @export
#' @name stat_rle
#'
#' @examples
#' df <- data.frame(
#'   x = seq(0, 10, length.out = 100),
#'   y = sin(seq(0, 10, length.out = 100)*2)
#' )
#'
#' # Label every run of increasing values
#' ggplot(df) +
#'   stat_rle(aes(x, label = diff(c(0, y)) > 0),
#'            align = "end") +
#'   geom_point(aes(x, y))
#'
#' # Label every run above some threshold
#' ggplot(df) +
#'   stat_rle(aes(x, label = y > 0),
#'            align = "center") +
#'   geom_point(aes(x, y))
#'
#' # Categorising runs, more complicated usage
#' ggplot(df) +
#'   stat_rle(aes(stage(x, after_stat = run_id),
#'                after_stat(runlength),
#'                label = cut(y, c(-1, -0.6, 0.6, 1)),
#'                fill = after_stat(runvalue)),
#'            geom = "col")
stat_rle <- function(
  mapping = NULL,
  data = NULL,
  geom = "rect",
  position = "identity",
  ...,
  align = "none",
  na.rm = FALSE,
  orientation = "x",
  show.legend = NA,
  inherit.aes = TRUE
) {
  align <- match.arg(align, c("none", "centre", "center", "start", "end"))
  if (align == "center") {
    align <- "centre"
  }
  layer(data = data,
        mapping = mapping,
        stat = StatRle,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          na.rm = na.rm,
          orientation = orientation,
          align = align
        ))
}

# ggproto class -----------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
StatRle <- ggproto(
  "StatRle",
  Stat,
  required_aes = c("x", "label"),
  default_aes = aes(xmin = after_stat(start),
                    xmax = after_stat(end),
                    ymin = after_stat(-Inf), ymax = after_stat(Inf),
                    fill = after_stat(runvalue)),
  setup_params = function(data, params) {
    params$flipped_aes <- isTRUE(params$orientation == "y")
    params
  },
  extra_params = c("na.rm", "orientation", "align"),
  compute_layer = function(self, data, params, layout) {
    if ("label" %in% names(data)) {
      data[["label"]] <- protect_finite(data[["label"]])
    }
    ggproto_parent(Stat, self)$compute_layer(data, params, layout)
  },
  compute_group = function(data, flipped_aes = FALSE, align, scales) {
    data$label <- unprotect_finite(data$label)
    data <- data[order(data$x), ]
    n <- nrow(data)

    is_factor <- is.factor(data$label)
    if (is_factor) {
      lvls <- levels(data$label)
    }
    run <- rle(as.character(data$label))

    start_id <- {end_id <- cumsum(run$lengths)} - run$lengths + 1

    if (align == "centre") {
      start <- (data$x[pmax(start_id, 1L)] + data$x[pmax(start_id - 1, 1L)]) / 2
      end <- (data$x[pmin(end_id, n)] + data$x[pmin(end_id + 1, n)]) / 2
    } else if (align == "end") {
      start <- data$x[pmax(start_id - 1, 1L)]
      end <- data$x[end_id]
    } else if (align == "start") {
      start <- data$x[start_id]
      end <- data$x[pmin(end_id + 1, n)]
    } else {
      start <- data$x[start_id]
      end <- data$x[end_id]
    }

    values <- run$values
    if (is_factor) {
      values <- factor(values, levels = lvls)
    }

    list2df(list(
      start = start,
      end = end,
      start_id = start_id,
      end_id = end_id,
      run_id = seq_along(run$values),
      runlength = run$lengths,
      runvalue = run$values
    ))
  }
)

# Helpers -----------------------------------------------------------------

# This is a bit of an ugly solution to let the label variable not be counted as
# a non-finite variable.

#' @export
#' @usage NULL
#' @rdname stat_rle
vec_math.finite_type <- function(.fn, .x, ...) {
  switch(.fn,
         is.finite = !is.na(.x),
         stop("not implemented")
  )
}

protect_finite <- function(x) {
  attrs <- attributes(x)
  oldclass <- class(x)
  attrs <- attrs[setdiff(names(attrs), "class")]
  do.call(new_vctr, c(`.data` = list(x),
                      attrs, oldclass = oldclass,
                      class = "finite_type"))
}

unprotect_finite <- function(x) {
  if (!inherits(x, "finite_type")) {
    return(x)
  }
  class(x) <- attr(x, "oldclass")
  attr(x, "oldclass") <- NULL
  x
}

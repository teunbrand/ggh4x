#' Apply function to position coordinates
#'
#' The function xy stat applies a function to the x- and y-coordinates of a
#' layers positions by group. The \code{stat_centroid()} and
#' \code{stat_midpoint()} functions are convenience wrappers for calculating
#' centroids and midpoints. \code{stat_funxy()} by default leaves the data
#' as-is, but can be supplied functions and arguments.
#'
#' @inheritParams ggplot2::stat_identity
#' @param funx,funy A \code{function} to call on the layer's \code{x} and
#'   \code{y} positions respectively.
#' @param argx,argy A named \code{list} containing arguments to the \code{funx},
#'   and \code{funy} function calls.
#' @param crop_other A \code{logical} of length one; wether the other data
#'   should be fitted to the length of \code{x} and \code{y} (default:
#'   \code{TRUE}). Useful to set to \code{FALSE} when \code{funx} or \code{funy}
#'   calculate summaries of length one that need to be recycled.
#'
#' @details This statistic only makes a minimal attempt at ensuring that the
#'   results from calling both functions are of equal length. Results of length
#'   1 are recycled to match the longest length result.
#'
#' @return A \code{StatFunxy} ggproto object, that can be added to a plot.
#' @export
#'
#' @examples
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species))
#'
#' # Labelling group midpoints
#' p + geom_point() +
#'   stat_midpoint(aes(label = Species, group = Species),
#'                 geom = "text", colour = "black")
#'
#' # Drawing segments to centroids
#' p + geom_point() +
#'   stat_centroid(aes(xend = Sepal.Width, yend = Sepal.Length),
#'                 geom = "segment", crop_other = FALSE)
#'
#' # Drawing intervals
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
#'   geom_point() +
#'   stat_funxy(geom = "path",
#'              funx = median, funy = quantile,
#'              argy = list(probs = c(0.1, 0.9)))
stat_funxy <-
  function(mapping = NULL, data = NULL, geom = "point",
           position = "identity", ..., funx = force, funy = force,
           argx = list(), argy = list(), crop_other = TRUE,
           show.legend = NA, inherit.aes = TRUE) {
    if (!is.function(funx)) {
      stop("The `funx` argument must be a function.", call. = FALSE)
    }
    funx <- force(funx)
    if (!is.function(funy)) {
      stop("The `funy` argument must be a function.", call. = FALSE)
    }
    funy <- force(funy)
    if (!is.list(argx) | !is.list(argy)) {
      stop("The `argx` and `argy` arguments must be lists.", call. = FALSE)
    } else {
      if (length(argx) > 0) {
        if (is.null(names(argx))) {
          stop("The `argx` list must have named elements.", call. = FALSE)
        }
      }
      if (length(argy) > 0) {
        if (is.null(names(argy))) {
          stop("The `argy` list must have named elements.", call. = FALSE)
        }
      }
    }

    layer(
      data = data, mapping = mapping, stat = StatFunxy, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        funx = funx, funy = funy,
        argx = argx, argy = argy,
        crop_other = crop_other,
        ...
      )
    )
  }

#' @rdname stat_funxy
#' @export
stat_centroid <- function(...,
                          funx = mean, funy = mean,
                          argx = list(na.rm = TRUE), argy = list(na.rm = TRUE)) {
  stat_funxy(..., funx = funx, funy = funy, argx = argx, argy = argy)
}

#' @rdname stat_funxy
#' @export
stat_midpoint <- function(...,
                          argx = list(na.rm = TRUE),
                          argy = list(na.rm = TRUE)) {
  fun <- function(x, na.rm = TRUE) {
    sum(range(x, na.rm = na.rm), na.rm = na.rm)/2
  }
  stat_funxy(..., funx = fun, funy = fun, argx = argx, argy = argy)
}

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
#' @importFrom vctrs vec_recycle_common
StatFunxy <- ggproto(
  "StatFunxy", Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales,
                           funx, funy,
                           argx, argy,
                           crop_other = TRUE) {
    # Make list for cheaper operations
    data <- as.list(data)

    # Apply functions
    x <- do.call(funx, c(unname(data["x"]), argx))
    y <- do.call(funy, c(unname(data["y"]), argy))

    # Ensure rest of data is of correct length
    other <- setdiff(names(data), c("x", "y"))
    size <- seq_len(max(length(x), length(y)))
    if (isTRUE(crop_other)) {
      other <- lapply(data[other], `[`, i = size)
    } else {
      other <- data[other]
    }

    # Combine data
    data <- c(other, list(x = x, y = y))
    data <- do.call(vec_recycle_common, data)
    list2df(data)
  }
)

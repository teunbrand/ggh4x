# Main function -----------------------------------------------------------

#' Segregrating overlapping ranges
#'
#' @description One-dimensional ranged data in the x-direction is segregated in
#'   the y-direction such that no overlap in twodimensional space occurs. This
#'   positioning works best when no relevant information is plotted in the
#'   y-direction.
#'
#' @param extend a \code{numeric} of length 1 indicating how far a range should
#'   be extended in total for calculating overlaps. Setting this argument to a
#'   positive number leaves some space between ranges in the same bin.
#' @param stepsize a \code{numeric} of length 1 that determines how much space
#'   is added between bins in the y-direction. A positive value grows the bins
#'   from bottom to top, while a negative value grows the bins from top to
#'   bottom.
#'
#' @export
#'
#' @return A \emph{PositionDisjointRanges} object.
#'
#' @details An object is considered disjoint from a second object when the range
#'   between their \code{xmin} and \code{xmax} coordinates don't overlap.
#'   Objects that overlap are assigned to different bins in the y-direction,
#'   whereby lower bins are filled first. This way, information in the
#'   x-direction is preserved and different objects can be discerned.
#'
#'   Note that this positioning is only particularly useful when y-coordinates
#'   do not encode relevant information. Geoms that pair well with this
#'   positioning are \code{\link[ggplot2:geom_tile]{geom_rect}} and
#'   \code{\link[ggplot2]{geom_tile}}.
#'
#'   This positioning function was inspired by the \code{disjointBins()}
#'   function in the \code{IRanges} package, but has been written such that it
#'   accepts any numeric input next to solely integer input.
#'
#' @seealso The \code{disjointBins} function the Bioconductor IRanges package.
#'
#' @examples
#' # Even though geom_tile() is parametrised by middle-x values, it is
#' # internally converted to xmin, xmax, ymin, ymax parametrisation so the
#' # positioning still works.
#'
#' ggplot() +
#'   geom_tile(aes(x = rnorm(200), y = 0),
#'             width = 0.2, height = 0.9,
#'             position = position_disjoint_ranges(extend = 0.1))
position_disjoint_ranges <- function(extend = 1, stepsize = 1) {
  ggproto(NULL, PositionDisjointRanges, extend = extend, stepsize = stepsize)
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
PositionDisjointRanges <- ggplot2::ggproto(
  "PositionDisjointRanges",
  ggplot2::Position,
  extend   = NULL,
  stepsize = NULL,
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  setup_params = function(self, data) {
    if (is.null(data$xmin) || is.null(data$xmax)) {
      warning("Undefined ranges in the x-direction.
              Please supply 'xmin' and 'xmax'",
              call. = FALSE)
    }
    list(extend = self$extend,
         stepsize = self$stepsize)
  },
  compute_panel = function(data, params, scales) {

    # Simplify groups to ranges
    if (length(unique(data[["group"]])) > 1) {
      group <- data$group
      ranges <- by(data, data$group, function(dat){
        c(min(dat$xmin), max(dat$xmax), dat$group[1])
      })
      ranges <- do.call(rbind, ranges)

      ranges <- setNames(as.data.frame(ranges),
                         c("xmin", "xmax", "group"))
    } else if (all(data[["group"]] == -1)){
      ranges <- cbind(data[, c("xmin", "xmax")],
                      group = row(data)[, 1])
      group <- ranges$group
    } else {
      return(data)
    }

    # Extend and sort ranges
    ranges$xmin <- ranges$xmin - 0.5 * params$extend
    ranges$xmax <- ranges$xmax + 0.5 * params$extend
    ord <- order(ranges$xmin)
    ranges <- ranges[ord, ]

    # Perform disjoint bins operation similar to IRanges::disjointBins(), but
    # generalized to any ranged numeric data, not just integers.
    track_bins <- ranges$xmax[1]
    ranges$bin <- c(1, vapply(tail(seq_along(ord), -1), function(i) {
      dat <- ranges[i, ]
      j <- which(track_bins < dat$xmin)
      if (length(j) > 0) {
        ans  <- j[1]
        # If a bin is available, update bin
        ends <- track_bins
        ends[ans]  <- dat$xmax
        track_bins <<- ends
      } else {
        # Else, make new bin
        track_bins <<- c(track_bins, dat$xmax)
        ans <- length(track_bins)
      }
      return(ans)
    }, integer(1)))

    # Transform
    map <- match(group, ranges$group)
    if (all(c("ymin", "ymax") %in% names(data))) {
      data$ymax <- data$ymax + params$stepsize * (ranges$bin[map] - 1)
      data$ymin <- data$ymin + params$stepsize * (ranges$bin[map] - 1)
    }

    return(data)
  }
)

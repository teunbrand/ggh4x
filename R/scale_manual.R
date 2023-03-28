# External constructors ---------------------------------------------------

#' Manual position scales
#'
#' `scale_x_manual()` and `scale_y_manual()` are hybrid discrete and continuous
#' position scales for the `x` and `y` aesthetics. These accept input like
#' [discrete scales][ggplot2::scale_x_discrete], but may map these discrete
#' values to continuous values that needn't be equally spaced.
#'
#' @param values A `numeric` vector with the same length as unique values.
#'   Alternatively, a function that accepts the limits (unique values) as
#'   determined from the data and returns a `numeric` vector parallel to the
#'   input.
#' @param c_limits Either `NULL` (default) to accept the range of `values` as
#'   the continuous limits, or a `numeric(2)` to set custom continuous limits.
#' @inheritParams ggplot2::discrete_scale
#' @inheritDotParams ggplot2::discrete_scale -aesthetics -palette
#'
#' @details
#' Many thanks to Constantin Ahlmann-Eltze for discussion and suggesting
#' the adoption of this functionality in ggh4x.
#'
#' @return A `<ScaleManualPosition>` object that can be added to a plot.
#' @export
#'
#' @seealso [sep_discrete()]
#'
#' @examples
#' # A boxplot with interactions
#' p <- ggplot(mpg, aes(interaction(year, cyl), displ)) +
#'   geom_boxplot()
#'
#' # Manually setting positions
#' p + scale_x_manual(values = c(1, 2, 4, 6, 7, 9, 10))
#'
#' # Using a function to separate grouped labels
#' p + scale_x_manual(values = sep_discrete())
#'
#' # Expanding the continuous limits
#' p + scale_x_manual(values = sep_discrete(), c_limits = c(NA, 15))
#'
#' # Together with grouped axis
#' p + scale_x_manual(values = sep_discrete(), guide = guide_axis_nested())
scale_x_manual <- function(
  values,
  c_limits = NULL,
  position = "bottom",
  ...
) {
  scale_position_manual(
    c("x", "xmin", "xmax", "xend"),
    values = values,
    c_limits = c_limits,
    position = position,
    ...
  )
}

#' @rdname scale_x_manual
#' @export
scale_y_manual <- function(
  values,
  c_limits = NULL,
  position = "left",
  ...
) {
  scale_position_manual(
    c("y", "ymin", "ymax", "yend"),
    values = values,
    c_limits = c_limits,
    position = position,
    ...
  )
}

# Internal constructor ----------------------------------------------------

scale_position_manual <- function(
  aesthetics = c("x", "xmin", "xmax", "xend"),
  values   = NULL,
  ...,
  limits   = NULL,
  c_limits = NULL,
  breaks   = waiver(),
  expand   = waiver(),
  guide    = waiver(),
  position = "bottom",
  env      = caller_env()
) {
  force(values)

  if (!is.function(values)) {
    if (!is.numeric(values)) {
      cli::cli_abort(
        "The {.arg values} argument must be {.cls numeric}.",
        call = env
      )
    }

    if (is.null(limits) && !is.null(names(values))) {
      limits <- function(x) intersect(x, names(values)) %||% character()
    }

    if (is.vector(values) && is.null(names(values))
        && !inherits(breaks, "waiver") &&
        !is.null(breaks) && !is.function(breaks)) {
      if (length(breaks) <= length(values)) {
        names(values) <- breaks
      } else {
        names(values) <- breaks[1:length(values)]
      }
    }

    pal <- function(limits) {
      if (length(limits) > length(values)) {
        cli::cli_abort(paste0(
          "Insufficient values in manual scale. {length(limits)} needed ",
          "but {length(values)} provided."
        ))
      }
      values[seq_along(limits)]
    }
  } else {
    pal <- values
  }

  if (!is.null(c_limits) && (!is.numeric(c_limits) || length(c_limits) != 2)) {
    cli::cli_abort(paste0(
      "The {.arg c_limits} argument must either be {.code NULL} or a ",
      "{.cls numeric} vector of length 2."
    ))
  }

  sc <- discrete_scale(
    aesthetics,
    "position_d",
    pal,
    ...,
    expand = expand,
    guide  = guide,
    position = position,
    super = ScaleManualPosition
  )
  sc$range_c <- ContinuousRange$new()
  sc$c_limits <- c_limits
  sc
}

# ggproto class -----------------------------------------------------------

ScaleManualPosition <- ggproto(
  "ScaleManualPosition", ScaleDiscretePosition,

  train = function(self, x) {
    if (is.discrete(x)) {
      self$range$train(x, drop = self$drop, na.rm = !self$na.translate)
      range <- range(self$palette(self$get_limits()))
    }
    if (!is.discrete(x)) {
      range <- range(x)
    }
    if (!is.null(self$c_limits)) {
      range <- ifelse(is.na(self$c_limits), range, self$c_limits)
    }
    # Hack for scale expansion
    expand <- self$expand %|W|% expansion(add = 0.6)
    lower  <- expand_range(range, expand[1], expand[2])[1]
    upper  <- expand_range(range, expand[3], expand[4])[2]
    self$range_c$train(c(lower, upper))
  },

  map = function(self, x, limits = self$get_limits()) {
    if (is.discrete(x)) {
      n <- sum(!is.na(limits))
      if (!is.null(self$n.breaks.cache) && self$n.breaks.cache == n) {
        pal <- self$palette.cache
      } else {
        if (!is.null(self$n.breaks.cache)) {
          cli::cli_warn("Cached palette does not match requested.")
        }
        pal <- self$palette(limits)
        self$palette.cache  <- pal
        self$n.breaks.cache <- n
      }

      if (!is_null(names(pal))) {
        idx_nomatch <- is.na(match(names(pal), limits))
        pal[idx_nomatch] <- NA
        pal_match <- pal[match(as.character(x), names(pal))]
        pal_match <- unname(pal_match)
      } else {
        pal_match <- pal[match(as.character(x), limits)]
      }

      if (self$na.translate) {
        pal_match[is.na(x) | is.na(pal_match)] <- self$na.value
      }
      x <- pal_match
    }
    mapped_discrete(x)
  }
)


# Helpers -----------------------------------------------------------------

#' Separator for discrete grouped labels
#'
#' This is a function factory that provides a function to split grouped discrete
#' labels into numerical positions.
#'
#' @param sep A `character(1)` separator to use for splitting. May not contain
#'   regular expressions.
#' @param inv A `logical(1)` whether to invert the layering of groups.
#'
#' @return A `function` that accepts `character` input and returns
#'   `numeric` output.
#' @export
#'
#' @examples
#' # Here, 'bar.qux' belongs to the second group, so has +1 value
#' sep_discrete()(c("foo.bar", "bar.bar", "bar.qux"))
#'
#' # Now, the values are grouped by the groups before the separator
#' sep_discrete(inv = TRUE)(c("foo.bar", "bar.bar", "bar.qux"))
sep_discrete <- function(sep = ".", inv = FALSE) {
  force(sep)
  force(inv)
  function(limits) {
    split <- strsplit(limits, sep, fixed = TRUE)
    lengs <- lengths(split)
    depth <- max(lengs)

    if (!all(lengs == depth)) {
      split <- lapply(split, function(lab) {
        c(lab, rep("", depth - length(lab)))
      })
    }
    split <- do.call(rbind, split)
    if (isTRUE(inv)) {
      split <- split[, rev(seq_len(ncol(split))), drop = FALSE]
    }
    vals  <- apply(split, 2, function(x) {
      unrep <- vec_unrep(x)
      rep(vec_seq_along(unrep$key) - 1L, unrep$times)
    })
    if (nrow(split) == 1) {
      dim(vals) <- c(1, length(vals))
    }
    vals[, 1] <- seq_len(nrow(vals))
    rowSums(vals)
  }
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

mapped_discrete <- function(x = double()) {
  if (is.null(x)) return(NULL)
  vec_assert(as.vector(x), double())
  class(x) <- c("mapped_discrete", "numeric")
  x
}

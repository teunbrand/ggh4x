# Utilities ---------------------------------------------------------------

# Like the try_require in ggplot2
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    return(invisible())
  }

  stop("Package `", package, "` required for `", fun, "`.\n",
       "Please install and try again.", call. = FALSE)
}

# Rlang operator
`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else x
}

# R4.4.0 minimal data.frame constructor
list2df <- function (x = list(), nrow = NULL)
{
  stopifnot(is.list(x), is.null(nrow) || nrow >= 0L)
  if (n <- length(x)) {
    if (is.null(nrow))
      nrow <- max(lengths(x), 0L)
    x <- lapply(x, rep_len, nrow)
  }
  else {
    if (is.null(nrow))
      nrow <- 0L
  }
  if (is.null(names(x)))
    names(x) <- character(n)
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(nrow)
  x
}

seq_range <- function(dat, ...) {
  seq.int(min(dat, na.rm = TRUE), max(dat, na.rm = TRUE), ...)
}

seq_nrow <- function(dat) {
  seq_len(NROW(dat))
}

seq_ncol <- function(dat) {
  seq_len(NCOL(dat))
}

# ggplot internals --------------------------------------------------------

# Function for grabbing internal function of ggplot2 that are also used here
.grab_ggplot_internals <- function() {
  objects <- c(
    ".all_aesthetics",
    "as_facets_list",
    "as_gg_data_frame",
    "axis_label_element_overrides",
    "check_aesthetics",
    "check_labeller",
    "check_subclass",
    "compact",
    "continuous_range",
    "convertInd",
    "df.grid",
    "draw_axis_labels",
    "reshape_add_margins",
    "new_data_frame",
    "defaults", "id",
    "empty",
    "eval_facets",
    "ggname",
    "rename_aes",
    "mapped_aesthetics",
    "make_labels",
    "grid_as_facets_list",
    "is.zero",
    "rbind_dfs",
    "sanitise_dim",
    "set_draw_key",
    "snake_class",
    "ulevels",
    "unique_combs",
    "var_list",
    "validate_mapping",
    "warn_for_guide_position",
    "weave_tables_col",
    "weave_tables_row",
    "wrap_as_facets_list",
    "join_keys",
    ".pt",
    "interleave",
    "justify_grobs",
    "compute_just"
  )
  objects <- setNames(objects, objects)
  out <- lapply(objects, function(i) {
    getFromNamespace(i, "ggplot2")
  })
}

# Store the needed ggplot internals here
.int <- .grab_ggplot_internals()

# Orphan functions --------------------------------------------------------

# Functions that need to be placed somewhere but do not have a proper place
# anywhere else

#' Center limits
#'
#' This a function factory that allows the centering of scales around a certain
#' value while still including all values. Convenient for centering log2 fold
#' change limits around zero.
#'
#' @param around A \code{numeric} of length 1 indicating around which value to
#'   center the limits.
#'
#' @return A \code{function} that takes limits and returns expanded limits
#'   centered at the \code{around} argument.
#' @export
#'
#' @examples
#' center_limits(5)(c(3,8))
#'
#' g <- ggplot(iris,
#'             aes(Sepal.Width, Sepal.Length,
#'                 colour = log2(Petal.Width / Petal.Length))) +
#'   geom_point() +
#'   scale_colour_gradient2(limits = center_limits())
center_limits <- function(around = 0) {
  function(input) {
    c(-1, 1) * max(abs(input - around)) + around
  }
}

#' Passing a subset of data to ggplot2 layers.
#'
#' This is a convenience function to allow layer objects, such as geoms, to take
#' a subset of the data in the main \code{ggplot()} call, without storing a
#' duplicate of the subset in the ggplot object.
#'
#' @param rowtest logical \code{expression} indicating which rows to keep.
#' @param omit a \code{character} column name to exclude.
#'
#' @return A function that takes a \code{data.frame} as argument and returns a
#'   subset of that \code{data.frame} according to \code{rowtest}
#' @export
#'
#' @details \code{ggsubset} is a wrapper around \code{subset.data.frame} where
#'   the \code{subset} argument is set to \code{rowtest} and the \code{select}
#'   argument to \code{-omit}. Since the \code{data} argument in the
#'   \code{layer()} function can take a function with one argument, we can pass
#'   the function returned from \code{ggsubset} as that argument to subset the
#'   data by rows.
#'
#' @seealso See \code{\link[ggplot2]{layer}}, specifically the \code{data}
#'   argument. See \code{\link[base:subset]{subset.data.frame}} for the internal
#'   function.
#'
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point(data = ggsubset(Species == "setosa"))
ggsubset <- function(rowtest = NULL, omit = NULL) {
  rowtest <- substitute(rowtest)
  if (is.null(rowtest)) {
    rowtest <- substitute(TRUE)
  }

  if (!is.null(substitute(omit))) {
    omit <- substitute(-omit)
  } else {
    omit <- TRUE
  }

  function(x) subset.data.frame(x, eval(rowtest), eval(omit))
}


#' Bind together factors
#'
#' Computes a new factor out of combinations of input factors.
#'
#' @param ... The vectors
#' @param drop A \code{logical} of length 1 which when \code{TRUE} will remove
#'   combinations of factors not occurring in the input data.
#' @param sep A \code{character} of length 1 with a string to delimit the new
#'   level labels.
#' @param replaceNA A \code{logical} of length 1: replace \code{NA} values with
#'   empty strings?
#'
#' @details \code{weave_factors()} broadly resembles \code{interaction(...,
#'   lex.order = TRUE)}, with a slightly altered approach to non-factor inputs.
#'   In other words, this function orders the new levels such that the levels of
#'   the first input variable in \code{...} is given priority over the second
#'   input, the second input has priority over the third, etc.
#'
#'   This function treats non-factor inputs as if their levels were
#'   \code{unique(as.character(x))}, wherein \code{x} represents an input.
#'
#' @return A \code{factor} representing combinations of input factors.
#' @export
#'
#' @seealso \code{\link{interaction}}
#'
#' @examples
#' f1 <- c("banana", "apple", "apple", "kiwi")
#' f2 <- factor(c(1, 1:3), labels = c("house", "cat", "dog"))
#'
#' # Notice the difference in level ordering between the following:
#' interaction(f1, f2, drop = TRUE, lex.order = TRUE)
#' interaction(f1, f2, drop = TRUE, lex.order = FALSE)
#' weave_factors(f1, f2)
#'
#' # The difference is in how characters are interpreted
#' # The following are equivalent
#' interaction(f1, f2, drop = TRUE, lex.order = TRUE)
#' weave_factors(as.factor(f1), f2)
weave_factors <- function(..., drop = TRUE, sep = ".", replaceNA = TRUE) {
  args <- list(...)
  nargs <- length(args)
  if (nargs < 1L) {
    return(NULL)
  }
  lengths <- lengths(args)
  if (!all(lengths %in% c(1L, max(lengths)))) {
    stop("All inputs to 'weave_factors' should either be the",
         "same length or length 1", call. = FALSE)
  }
  if (replaceNA) {
    args <- lapply(args, function(x) {
      # Kind of assuming factors don't have NA levels
      if (is.factor(x)) {
        lvls <- levels(x)
        x <- as.integer(x)
        if (anyNA(x)) {
          if ("" %in% lvls) {
            i <- which("" %in% lvls)
          } else {
            i <- length(lvls) + 1
          }
          x[is.na(x)] <- i
        }
        structure(x, levels = c(lvls, ""), class = "factor")
      } else {
        ifelse(is.na(x), "", as.character(x))
      }
    })
  }
  vals <- do.call(paste, c(args, sep = sep))
  unique_vals <- unique(vals)
  unique_lvls <- lapply(args, function(x) {
    levels(x) %||% as.character(unique(x))
  })
  lvls <- do.call(
    expand.grid, c(rev(unique_lvls),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  )
  lvls <- do.call(paste, c(rev(lvls), sep = sep))
  if (drop) {
    lvls <- lvls[lvls %in% unique_vals]
  }
  lvls <- unique(lvls)
  i <- match(vals, lvls)
  structure(i, levels = lvls, class = "factor")
}

.onLoad <- function(...) {
  ggh4x_theme_elements()
}

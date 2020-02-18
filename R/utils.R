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

# Function for grabbing internal function of ggplot2 that are also used here
.grab_ggplot_internals <- function() {
  objects <- c(
    ".all_aesthetics", "as_facets_list", "as_gg_data_frame", "check_aesthetics",
    "check_labeller", "check_subclass", "compact", "continuous_range", "convertInd",
    "df.grid", "draw_axis_labels", "reshape_add_margins", "new_data_frame",
    "defaults", "id",
    "empty", "eval_facets", "ggname", "rename_aes", "mapped_aesthetics",
    "make_labels",
    "grid_as_facets_list", "is.zero", "rbind_dfs", "sanitise_dim", "set_draw_key",
    "snake_class", "ulevels",
    "unique_combs", "var_list", "validate_mapping", "warn_for_guide_position",
    "weave_tables_col", "weave_tables_row", ".pt"
  )
  objects <- setNames(objects, objects)
  out <- lapply(objects, function(i) {
    getFromNamespace(i, "ggplot2")
  })
}

# Store the needed ggplot internals here
.int <- .grab_ggplot_internals()

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
#'   argument. See \code{\link[base]{subset.data.frame}} for the internal
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

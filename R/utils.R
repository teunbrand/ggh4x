
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    return(invisible())
  }

  stop("Package `", package, "` required for `", fun, "`.\n",
       "Please install and try again.", call. = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else x
}

.grab_ggplot_internals <- function() {
  objects <- c(
    ".all_aesthetics", "as_facets_list", "as_gg_data_frame", "check_aesthetics",
    "check_labeller", "check_subclass", "compact", "continuous_range", "convertInd",
    "df.grid", "draw_axis_labels",
    "defaults",
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

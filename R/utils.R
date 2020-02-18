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

# ggplot internals --------------------------------------------------------

# Function for grabbing internal function of ggplot2 that are also used here
.grab_ggplot_internals <- function() {
  objects <- c(
    ".all_aesthetics",
    "as_facets_list",
    "as_gg_data_frame",
    "check_aesthetics",
    "check_labeller",
    "check_subclass",
    "compact",
    "continuous_range",
    "convertInd",
    "df.grid",
    # "draw_axis_labels",
    # "reshape_add_margins",
    "new_data_frame",
    "defaults", "id",
    "empty",
    # "eval_facets",
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
    # "warn_for_guide_position",
    "weave_tables_col",
    "weave_tables_row",
    ".pt"
  )
  objects <- setNames(objects, objects)
  out <- lapply(objects, function(i) {
    getFromNamespace(i, "ggplot2")
  })
}

# Store the needed ggplot internals here
.int <- .grab_ggplot_internals()

# ggplot v3.30 internals --------------------------------------------------

# Needs to be copied from ggplot2 untill v3.30 is on CRAN, then can be moved to
# the .grab_ggplot_internals

reshape_add_margins <- function(df, vars, margins = TRUE) {
  margin_vars <- reshape_margins(vars, margins)

  # Return data frame if no margining necessary
  if (length(margin_vars) == 0) return(df)

  # Prepare data frame for addition of margins
  addAll <- function(x) {
    x <- addNA(x, TRUE)
    factor(x, levels = c(levels(x), "(all)"), exclude = NULL)
  }
  vars <- unique(unlist(margin_vars))
  df[vars] <- lapply(df[vars], addAll)

  rownames(df) <- NULL

  # Loop through all combinations of margin variables, setting
  # those variables to (all)
  margin_dfs <- lapply(margin_vars, function(vars) {
    df[vars] <- rep(list(factor("(all)")), length(vars))
    df
  })

  do.call("rbind", margin_dfs)
}

reshape_margins <- function(vars, margins = NULL) {
  if (is.null(margins) || identical(margins, FALSE)) return(NULL)

  all_vars <- unlist(vars)
  if (isTRUE(margins)) {
    margins <- all_vars
  }

  # Start by grouping margins by dimension
  dims <- lapply(vars, intersect, margins)

  # Next, ensure high-level margins include lower-levels
  dims <- mapply(function(vars, margin) {
    lapply(margin, downto, vars)
  }, vars, dims, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  # Finally, find intersections across all dimensions
  seq_0 <- function(x) c(0, seq_along(x))
  indices <- expand.grid(lapply(dims, seq_0), KEEP.OUT.ATTRS = FALSE)
  # indices <- indices[rowSums(indices) > 0, ]

  lapply(seq_len(nrow(indices)), function(i){
    unlist(mapply("[", dims, indices[i, ], SIMPLIFY = FALSE))
  })
}

.int <- c(.int, list(reshape_add_margins = reshape_add_margins))

eval_facet <- function(facet, data, possible_columns = NULL) {
  # Treat the case when `facet` is a quosure of a symbol specifically
  # to issue a friendlier warning
  if (rlang::quo_is_symbol(facet)) {
    facet <- as.character(rlang::quo_get_expr(facet))

    if (facet %in% names(data)) {
      out <- data[[facet]]
    } else {
      out <- NULL
    }
    return(out)
  }

  # Key idea: use active bindings so that column names missing in this layer
  # but present in others raise a custom error
  env <- rlang::new_environment(data)
  missing_columns <- setdiff(possible_columns, names(data))
  undefined_error <- function(e) stop("Unknown error")
  bindings <- rlang::rep_named(missing_columns, list(undefined_error))
  rlang::env_bind_active(env, !!!bindings)

  # Create a data mask and install a data pronoun manually (see ?new_data_mask)
  mask <- rlang::new_data_mask(env)
  mask$.data <- rlang::as_data_pronoun(mask)

  tryCatch(
    rlang::eval_tidy(facet, mask),
    ggplot2_missing_facet_var = function(e) NULL
  )
}

.int <- c(.int, list(eval_facet = eval_facet))

eval_facets <- function(facets, data, possible_columns = NULL) {
  vars <- .int$compact(lapply(facets, .int$eval_facet, data,
                         possible_columns = possible_columns))
  .int$new_data_frame(tibble::as_tibble(vars))
}

.int <- c(.int, list(eval_facets = eval_facets))

upto <- function(a, b) {
  b[seq_len(match(a, b, nomatch = 0))]
}
downto <- function(a, b) {
  rev(upto(a, rev(b)))
}

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

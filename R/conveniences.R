# Argument distributor ----------------------------------------------------

#' Element list constructors
#'
#' These functions take a vector of arguments and pass on the
#' \ifelse{html}{\out{i<sup>th</sup>}}{\eqn{i^{th}}} item of the vector to an
#' \ifelse{html}{\out{i<sup>th</sup>}}{\eqn{i^{th}}} call of a function. The
#' `elem_list_text` and `elem_list_rect` are convenience functions for
#' constructing lists of [`element_text()`][ggplot2::element_text()] and
#' [`element_rect()`][ggplot2::element_text()] theme elements.
#'
#' @param ... Vectorised arguments to pass on to functions.
#' @param .fun A function to distribute arguments to.
#' @param .cull A `logical(1)` determining if unknown arguments are being
#'   culled.
#'
#' @details `NA`s and `NULL`s will be silently dropped. If you want to pass on a
#'   transparent `fill` or `colour` argument, you should use the more verbose
#'   character `"transparent"` instead. However, you *can* use a `NA` to
#'   indicate that it's argument should not be passed to a function in that
#'   position.
#'
#' @note Whereas the `distribute_args` function might seem amenable for
#'   off-label uses elsewhere (besides constructing lists of theme elements), it
#'   is not intended as such. For example, because valid arguments will be
#'   deduced from the formals of a function, using certain functions can be
#'   troublesome. For example, the `distribute_args` function does not properly
#'   recognise the utility of a `...` argument in a function that it is supposed
#'   to distribute arguments to. This can be a problem for object-oriented
#'   functions: if the methods contain more arguments than the generic itself,
#'   these extra arguments will be silently dropped.
#'
#' @seealso The [`element_text()`][ggplot2::element_text()] and
#'   [`element_rect()`][ggplot2::element_text()] theme elements for a
#'   description of their arguments.
#'
#' @return A `list` of outputs from `fun`.
#' @export
#' @md
#'
#' @examples
#' # Providing arguments for `element_rect()`
#' elem_list_rect(
#'   # The first element_rect will have linetype 1, the second gets 3
#'   linetype = c(1, 3),
#'   # If an argument doesn't exist, it will be silently dropped
#'   nonsense_argument = c("I", "will", "be", "filtered", "out")
#' )
#'
#' # Providing arguments for `element_text()`
#' elem_list_text(
#'   # `NA`s will be skipped
#'   family = c("mono", NA, "sans"),
#'   # Providing a list of more complex arguments. `NULL` will be skipped too.
#'   margin = list(NULL, margin(t = 5))
#' )
#'
#' # Providing arguments to other functions
#' distribute_args(
#'   lineend = c("round", "butt", "square"),
#'   # If you want to pass a vector instead of a scalar, you can use a list
#'   colour = list(c("blue", "red"), "green"),
#'   .fun = element_line
#' )
distribute_args <- function(..., .fun = element_text, .cull = TRUE) {
  # Format arguments
  args <- list(...)
  fun_args <- names(formals(.fun))
  if (.cull) {
    args <- args[intersect(names(args), fun_args)]
  }
  args <- args[lengths(args) > 0]
  if (length(args) == 0) {
    return(.fun())
  }

  # Measure arguments
  nms  <- names(args)
  len  <- length(args)
  lens <- lengths(args)

  # Format args as matrix of arguments
  m <- matrix(list(NULL), len, max(lens))
  i <- rep(seq_len(len), lens)
  j <- unlist(lapply(lens, seq), use.names = FALSE)
  m[cbind(i, j)] <- unlist(lapply(args, as.list),
                           use.names = FALSE, recursive = FALSE)

  # Replace NAs by NULLs
  vectors <- vapply(m, is.vector, logical(1))
  m[vectors] <- lapply(m[vectors], function(x) {
    if (any(is.na(x))) NULL else x
  })

  # Loop over columns, distribute arguments to function
  apply(m, 2, function(arg) {
    arg <- setNames(arg, nms)
    # Drop NULLs
    arg <- arg[lengths(arg) > 0]
    do.call(.fun, arg)
  })
}

#' @export
#' @rdname distribute_args
elem_list_text <- function(...) {
  distribute_args(..., .fun = element_text)
}

#' @export
#' @rdname distribute_args
elem_list_rect <- function(...) {
  distribute_args(..., .fun = element_rect)
}

# Factor weaving ----------------------------------------------------------

#' Bind together factors
#'
#' Computes a new factor out of combinations of input factors.
#'
#' @param ... The vectors
#' @param drop A `logical` of length 1 which when `TRUE` will remove
#'   combinations of factors not occurring in the input data.
#' @param sep A `character` of length 1 with a string to delimit the new
#'   level labels.
#' @param replaceNA A `logical` of length 1: replace `NA` values with
#'   empty strings?
#'
#' @details `weave_factors()` broadly resembles `interaction(...,
#'   lex.order = TRUE)`, with a slightly altered approach to non-factor inputs.
#'   In other words, this function orders the new levels such that the levels of
#'   the first input variable in `...` is given priority over the second
#'   input, the second input has priority over the third, etc.
#'
#'   This function treats non-factor inputs as if their levels were
#'   `unique(as.character(x))`, wherein `x` represents an input.
#'
#' @return A `factor` representing combinations of input factors.
#' @export
#'
#' @seealso [interaction()]
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
    cli::cli_abort(
      "All inputs to {.fn weave_factors} should have the same length, \\
      or length 1."
    )
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

# Limit centering ---------------------------------------------------------

#' Center limits
#'
#' This a function factory that allows the centering of scales around a certain
#' value while still including all values. Convenient for centering log2 fold
#' change limits around zero.
#'
#' @param around A `numeric` of length 1 indicating around which value to
#'   center the limits.
#'
#' @return A `function` that takes limits and returns expanded limits
#'   centered at the `around` argument.
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
  force(around)
  function(input) {
    c(-1, 1) * max(abs(input - around)) + around
  }
}

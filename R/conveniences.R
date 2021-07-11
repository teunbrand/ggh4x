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
distribute_args <- function(..., .fun = element_text) {
  # Format arguments
  args <- list(...)
  fun_args <- names(formals(.fun))
  args <- args[intersect(names(args), fun_args)]
  args <- args[lengths(args) > 0]

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
  m[] <- lapply(m, function(x) {
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





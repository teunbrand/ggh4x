#' Secondary axis helper
#'
#' The purpose of this function is to construct a secondary axis with a
#' projection function.
#'
#' @param data A `data.frame` object.
#' @param primary,secondary An expression that is evaluated in the context of
#'   the `data` argument. These can be symbols for column names or plain
#'   expressions.
#' @param method One of the following: \describe{
#'  \item{`"range"`}{Causes the ranges of `primary` and `secondary` data to
#'  overlap completely.}
#'  \item{`"max"`}{Causes the maxima of `primary` and `secondary` data to
#'  coincide.}
#'  \item{`"fit"`}{Uses the coefficients of `lm(primary ~ secondary)` to make
#'  the axes fit.}
#'  \item{`"ccf"`}{Uses the lag at which maximum cross-correlation occurs to
#'  then align the data by truncation. The aligned data is then passed to the
#'  `"fit"` method.}
#'  \item{`"sortfit"`}{Sorts the both `primary` and `secondary` independently
#'  before passing these on to the `"fit"` method.}
#' }
#' @inheritDotParams ggplot2::sec_axis -trans
#'
#' @details The intent is to run this function before starting a plot. The
#'   output of the function is a secondary axis wherein the `trans` argument of
#'   `sec_axis()` is populated by an appropriate transformation. In addition,
#'   the output also contains a `output$proj()` function that helps transform the
#'   secondary data.
#'
#' @return An `AxisSecondary` ggproto object with a `proj` method for projecting
#'   secondary data.
#' @export
#' @md
#'
#' @examples
#' # Run the secondary axis helper
#' sec <- help_secondary(economics, primary = unemploy, secondary = psavert)
#'
#' # Making primary plot
#' p <- ggplot(economics, aes(date)) +
#'   geom_line(aes(y = unemploy), colour = "blue")
#'
#' # For the secondary data, later we use the `proj` function from the helper
#' p <- p + geom_line(aes(y = sec$proj(psavert)), colour = "red")
#'
#' # We feed the scale the secondary axis
#' p + scale_y_continuous(sec.axis = sec)
#'
#' # Setup cross-correlated data
#' set.seed(42)
#' n <- 100
#' lag <- 20
#' dat <- cumsum(rnorm(n + lag))
#' df <- data.frame(
#'   x = seq_len(n),
#'   y1 = head(dat, n),
#'   y2 = 10 + tail(dat, n) * 5 # offset and scale y2
#' )
#' # Choosing the cross-correlation function method.
#' sec <- help_secondary(df, y1, y2, method = "ccf")
#'
#' ggplot(df, aes(x)) +
#'   geom_line(aes(y = y1), colour = "blue") +
#'   geom_line(aes(y = sec$proj(y2)), colour = "red") +
#'   scale_y_continuous(sec.axis = sec)
help_secondary <- function(
  data = NULL,
  primary = c(0, 1),
  secondary = c(0, 1),
  method = c("range", "max", "fit", "ccf", "sortfit"),
  ...
) {
  primary   <- enquo(primary)
  secondary <- enquo(secondary)

  method <- match.arg(method, c("range", "max", "fit", "ccf", "sortfit"))
  name <- as_label(secondary)

  primary   <- eval_tidy(primary,   data)
  secondary <- eval_tidy(secondary, data)

  help <- switch(
    method,
    "range" = help_sec_range(primary, secondary),
    "max"   = help_sec_max(primary, secondary),
    "fit"   = help_sec_fit(primary, secondary),
    "ccf"   = help_sec_ccf(primary, secondary),
    "sortfit" = help_sec_sortfit(primary, secondary)
  )

  out <- ggproto(
    NULL,
    new_sec_axis(trans = help$reverse, ...),
    proj = help$forward
  )
  if (inherits(out$name, "waiver")) {
    out$name <- name
  }

  return(out)
}

# This is a workaround to bridge the gap between ggplot 3.4.4 and 3.5.0
new_sec_axis <- function(trans = NULL, ...) {
  args <- list2(trans = trans, ...)
  if ("transform" %in% fn_fmls_names(ggplot2::sec_axis)) {
    names(args)[1] <- "transform"
  }
  inject(sec_axis(!!!args))
}

# Methods -----------------------------------------------------------------

help_sec_range <- function(from, to) {
  from   <- range(from, na.rm = TRUE)
  to <- range(to, na.rm = TRUE)

  forward <- function(x) {
    rescale(x, from = to, to = from)
  }
  reverse <- function(x) {
    rescale(x, from = from, to = to)
  }
  list(forward = forward, reverse = reverse)
}

help_sec_max <- function(from, to) {
  from   <- range(from, na.rm = TRUE)
  to <- range(to, na.rm = TRUE)

  forward <- function(x) {
    rescale_max(x, from = to, to = from)
  }
  reverse <- function(x) {
    rescale_max(x, from = from, to = to)
  }
  list(forward = forward, reverse = reverse)
}

help_sec_fit <- function(from, to) {
  if (length(from) != length(to)) {
    cli::cli_abort(
      "The primary and secondary values must have the same length."
    )
  }
  fit <- coef(lm(from ~ to))
  forward <- function(x) {
    fit[1] + x * fit[2]
  }
  reverse <- function(x) {
    (x - fit[1]) / fit[2]
  }
  list(forward = forward, reverse = reverse)
}

help_sec_ccf <- function(from, to) {
  if ({len <- length(from)} != length(to)) {
    cli::cli_abort(
      "The primary and secondary values must have the same length."
    )
  }
  lag <- ccf(from, to, lag.max = len - 1, plot = FALSE)
  lag <- lag$lag[which.max(lag$acf)]
  # No block for 0-lag because data is optimal as-is
  if (sign(lag) == 1) {
    from <- tail(from, -lag)
    to   <- head(to, -lag)
  } else if (sign(lag) == -1) {
    from <- head(from, lag)
    to   <- tail(to, lag)
  }
  help_sec_fit(from = from, to = to)
}

help_sec_sortfit <- function(from, to) {
  help_sec_fit(from = sort(from), to = sort(to))
}

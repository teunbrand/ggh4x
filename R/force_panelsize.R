# Main function -----------------------------------------------------------

#' Force a facetted plot to have specified panel sizes
#'
#' Takes a ggplot and modifies its facet drawing behaviour such that the widths
#' and heights of panels are set by the user.
#'
#' @param rows a \code{numeric} or \code{unit} vector for setting panel heights.
#' @param cols a \code{numeric} or \code{unit} vector for setting panel widths.
#' @param respect a \code{logical} value. If \code{TRUE}, widths and heights
#'   specified in \code{"null" unit}s are proportional. If \code{FALSE},
#'   \code{"null" unit}s in x- and y-direction vary idependently.
#'
#' @details Forcing the panel sizes should in theory work regardless of what
#'   facetting choice was made, as long as this function is called after the
#'   facet specification. Even when no facets are specified, ggplot2 defaults to
#'   the \code{\link[ggplot2]{facet_null}} specification; a single panel.
#'   \code{force_panelsizes} works by wrapping the original panel drawing
#'   function inside a function that modifies the widths and heights of panel
#'   grobs in the original function's output gtable.
#'
#'   When \code{rows} or \code{cols} are \code{numeric} vectors, panel sizes are
#'   defined as ratios i.e. relative \code{"null" unit}s. \code{rows} and
#'   \code{cols} vectors are repeated or shortened to fit the number of panels
#'   in their direction. When \code{rows} or \code{cols} are \code{NULL}, no
#'   changes are made in that direction.
#'
#'   When \code{respect = NULL}, default behaviour specified elsewhere is
#'   inherited.
#'
#'   No attempt is made to guarantee that the plot fits the output device. The
#'   \code{space} argument in \code{\link[ggplot2]{facet_grid}} will be
#'   overruled. When individual panels span multiple rows or columns, this
#'   function may not work as intended.
#' @export
#'
#' @seealso \code{\link[ggplot2]{facet_grid}} \code{\link[ggplot2]{facet_wrap}}
#'   \code{\link[ggplot2]{facet_null}} \code{\link[grid]{unit}}
#'
#' @return A \code{forcedsize} S3 object that can be added to a plot.
#'
#' @examples
#' ggplot(mtcars, aes(disp, mpg)) +
#'   geom_point() +
#'   facet_grid(vs ~ am) +
#'   force_panelsizes(rows = c(2, 1),
#'                    cols = c(2, 1))
force_panelsizes <- function(rows = NULL, cols = NULL, respect = NULL) {
  if (!is.null(rows) & !is.unit(rows)) {
    rows <- unit(rows, "null")
  }
  if (!is.null(cols) & !is.unit(cols)) {
    cols <- unit(cols, "null")
  }

  structure(list(rows = rows, cols = cols, respect = respect),
            class = "forcedsize")
}

# S3 add method -----------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @noRd
#' @export
#' @keywords internal
ggplot_add.forcedsize <- function(object, plot, object_name) {
  # Simply return plot if no changes are needed
  if (is.null(object$rows) & is.null(object$cols) & is.null(object$respect)){
    return(plot)
  }

  # Grab old facet stuffs
  old.facet <- plot$facet
  old.draw_panels <- old.facet$draw_panels
  old.args <- formals(environment(old.draw_panels)$f)
  old.params <- old.facet$params

  # Make new panel drawing function
  new.fun <- function(params){
    # Format old function arguments
    pass_args <- names(formals())
    pass_args <- pass_args[pass_args != "self"]
    pass_args <- lapply(pass_args, as.symbol)

    # Call the old function to make panels
    panel_table <- do.call(old.draw_panels, pass_args)

    # Grab panel positions
    prows <- panel_rows(panel_table)
    pcols <- panel_cols(panel_table)

    # Override row heights
    if (!is.null(params$force.rows)) {
      rowheights <- rep(params$force.rows, length.out = nrow(prows))
      panel_table$heights[prows$t] <- rowheights
    }
    # Override col widths
    if (!is.null(params$force.cols)) {
      colwidths <- rep(params$force.cols, length.out = nrow(pcols))
      panel_table$widths[pcols$l] <- colwidths
    }
    # Override respect
    if (!is.null(params$force.respect)) {
      panel_table$respect <- params$force.respect
    }

    return(panel_table)
  }
  # Force new fun to take old fun's arguments
  formals(new.fun) <- as.list(old.args)

  # Make new params
  new_params <- c(old.params, force = object)

  # Make new facet class
  new_facet <- ggproto(paste0("Forced", class(old.facet)[[1]]),
                       old.facet,
                       draw_panels = new.fun,
                       params = new_params)

  # Pass new facet to plot
  plot$facet <- new_facet
  return(plot)
}

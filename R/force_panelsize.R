# Main function -----------------------------------------------------------

#' Force a facetted plot to have specified panel sizes
#'
#' Takes a ggplot and modifies its facet drawing behaviour such that the widths
#' and heights of panels are set by the user.
#'
#' @param rows,cols a `numeric` or `unit` vector for setting panel heights
#'   (rows) or panel widths (cols).
#' @param respect a `logical` value. If `TRUE`, widths and heights
#'   specified in `"null" unit`s are proportional. If `FALSE`,
#'   `"null" unit`s in x- and y-direction vary independently.
#' @param total_width,total_height an absolute `unit` of length 1 setting the
#'   total width or height of all panels and the decoration between panels.
#'   If not `NULL`, `rows` and `cols` should be `numeric` and not `unit`s.
#'
#' @details Forcing the panel sizes should in theory work regardless of what
#'   facetting choice was made, as long as this function is called after the
#'   facet specification. Even when no facets are specified, ggplot2 defaults to
#'   the [ggplot2::facet_null()] specification; a single panel.
#'   `force_panelsizes` works by wrapping the original panel drawing
#'   function inside a function that modifies the widths and heights of panel
#'   grobs in the original function's output gtable.
#'
#'   When `rows` or `cols` are `numeric` vectors, panel sizes are
#'   defined as ratios i.e. relative `"null" unit`s. `rows` and
#'   `cols` vectors are repeated or shortened to fit the number of panels
#'   in their direction. When `rows` or `cols` are `NULL`, no
#'   changes are made in that direction.
#'
#'   When `respect = NULL`, default behaviour specified elsewhere is
#'   inherited.
#'
#'   No attempt is made to guarantee that the plot fits the output device. The
#'   `space` argument in [ggplot2::facet_grid()] will be
#'   overruled. When individual panels span multiple rows or columns, this
#'   function may not work as intended.
#' @export
#'
#' @seealso [ggplot2::facet_grid()] [ggplot2::facet_wrap()]
#'   [ggplot2::facet_null()] [grid::unit()]
#'
#' @return A `forcedsize` S3 object that can be added to a plot.
#'
#' @examples
#' ggplot(mtcars, aes(disp, mpg)) +
#'   geom_point() +
#'   facet_grid(vs ~ am) +
#'   force_panelsizes(rows = c(2, 1),
#'                    cols = c(2, 1))
force_panelsizes <- function(rows = NULL, cols = NULL, respect = NULL,
                             total_width = NULL, total_height = NULL) {
  if (!is.null(rows) & !is.unit(rows)) {
    rows <- unit(rows, "null")
  }
  if (!is.null(cols) & !is.unit(cols)) {
    cols <- unit(cols, "null")
  }
  if (!is.null(total_width)) {
    if (is.unit(cols) && !is_null_unit(cols)) {
      cli::cli_abort(
        "Cannot set {.arg total_width} when {.arg cols} is not relative."
      )
    }
    if (!is.unit(total_width)) {
      cli::cli_abort("{.arg total_width} must be a {.cls unit} object.")
    }
    arg_match0(unitType(total_width), c("cm", "mm", "inches", "points"))
  }
  if (!is.null(total_height)) {
    if (is.unit(rows) && !is_null_unit(rows)) {
      cli::cli_abort(
        "Cannot set {.arg total_height} when {.arg rows} is not relative."
      )
    }
    if (!is.unit(total_height)) {
      cli::cli_abort("{.arg total_height} must be a {.cls unit} object.")
    }
    arg_match0(unitType(total_height), c("cm", "mm", "inches", "points"))
  }

  structure(list(rows = rows, cols = cols, respect = respect,
                 total_width = total_width, total_height = total_height),
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
  if (sum(lengths(object)) < 1){
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

    # Set total width
    if (!is.null(params$force.total_width)) {
      colwidths <- as.numeric(
        rep(params$force.cols %||% 1, length.out = nrow(pcols))
      )
      extra_width <- setdiff(seq_range(pcols), unique(unlist(pcols)))
      if (length(extra_width) > 1) {
        extra_width <- convertUnit(sum(panel_table$widths[extra_width]), "cm")
      } else {
        extra_width <- unit(0, "cm")
      }
      extra_width <- params$force.total_width - extra_width
      colwidths <- extra_width * colwidths / sum(colwidths)
      panel_table$widths[pcols$l] <- colwidths
      params$force.cols <- NULL
    }

    if (!is.null(params$force.total_height)) {
      rowheights <- as.numeric(
        rep(params$force.rows %||% 1, length.out = nrow(prows))
      )
      extra_height <- setdiff(seq_range(prows), unique(unlist(prows)))
      if (length(extra_height) > 1) {
        extra_height <- convertUnit(sum(panel_table$heights[extra_height]), "cm")
      } else {
        extra_height <- unit(0, "cm")
      }
      extra_height <- params$force.total_height - extra_height
      rowheights <- extra_height * rowheights / sum(rowheights)
      panel_table$heights[prows$t] <- rowheights
      params$force.rows <- NULL
    }

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

is_null_unit <- function(x) {
  if (!is.unit(x)) {
    return(FALSE)
  }
  all(unitType(x) == "null")
}

# Weaving -----------------------------------------------------------------

#' Weave rows and columns in panel tables
#'
#' When drawing a plot with `ggplot2`, the canvas is laid out in a `gtable`
#' object containing panels and additional plot components. This is called the
#' "panel table". This function is to insert new rows and columns into the table
#' relative to some position of the panels.
#'
#' @param table A `gtable` object containing objects in the layout that are
#'   called `"panel"-*`.
#' @param table2 A `data.frame` with columns named `"t"`, `"b"`, `"l"` and `"r"`
#'   containing indices to the panels of the `table` argument, *and* a column
#'   with the name specified in `grob_var` that contains a list of `grob`s.
#' @param row_shift,col_shift An `integer(1)` determining where relative to the
#'   panel a new row or column needs to be inserted.
#' @param row_height,col_width A `unit` object specifying the size of the row
#'   or column to be inserted.
#' @param name A `character(1)`
#' @param z,clip See `[gtable_add_grob()][gtable::gtable_add_grob()]`.
#' @param pos A `character(1)` or `NULL` specifying what position takes
#'   precedence.
#' @param grob_var A `character(1)` with the name of the `grob`-list column.
#'
#' @return This returns the `table` input with additional rows or columns
#'   containing the grobs from the `grob_var`-column in `table2`.
#' @md
#' @noRd
#' @seealso `ggplot2:::weave_tables_col()` and `ggplot2:::weave_tables_row()`.
#'
#' @examples
#' NULL
weave_panel_rows <- function(table, table2, row_shift, row_height,
                              name, z = 1, clip = "off", pos = NULL,
                              grob_var = "grobs") {
  if (is.null(pos)) {
    # As no position is specified, interpret verbatim
    pos <- "t"
    alt <- "b"
  } else {
    # Otherwise, interpret opposite as position too
    alt <- pos
  }

  cols <- panel_cols(table)
  rows <- panel_rows(table)

  for (i in rev(seq_along(rows$t))) {
    table <- gtable_add_rows(table, row_height[i],
                             pos = rows[[pos]][i] + row_shift)
  }

  rows <- rows + row(rows) + row_shift

  if (!missing(table2)) {
    table <- gtable_add_grob(
      table, table2[[grob_var]],
      t = rows[[pos]][table2$t],
      b = rows[[alt]][table2$b],
      l = cols$l[table2$l],
      r = cols$r[table2$r],
      clip = clip, z = z,
      name = paste0(name, "-", seq_along(cols$l), "-", seq_along(table2$t))
    )
  }
  table
}

# See documentation for weave_panel_rows above
weave_panel_cols <- function(table, table2, col_shift, col_width,
                              name, z = 1, clip = "off", pos = NULL,
                              grob_var = "grobs") {
  if (is.null(pos)) {
    # As no position is specified, interpret verbatim
    pos <- "l"
    alt <- "r"
  } else {
    # Otherwise, interpret opposite as position too
    alt <- pos
  }

  cols <- panel_cols(table)
  rows <- panel_rows(table)

  for (i in rev(seq_along(cols$l))) {
    table <- gtable_add_cols(table, col_width[i],
                             pos = cols[[pos]][i] + col_shift)
  }

  cols <- cols + row(cols) + col_shift

  if (!missing(table2)) {
    table <- gtable_add_grob(
      table, table2[[grob_var]],
      t = rows$t[table2$t],
      b = rows$b[table2$b],
      l = cols[[pos]][table2$l],
      r = cols[[alt]][table2$r],
      clip = clip, z= z,
      name = paste0(name, "-", seq_along(rows$t), "-", seq_along(table2$l))
    )
  }
  table
}

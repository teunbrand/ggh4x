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

  # Ensure top-to-bottom order of unique panels
  rows <- panel_rows(table)
  rows <- sort(unique(rows[[pos]]))

  # Keep adding heights bottom-to-top
  for (i in rev(seq_along(rows))) {
    table <- gtable_add_rows(table, row_height[i], pos = rows[i] + row_shift)
  }

  if (!missing(table2)) {
    # Offset shift because we already added the rows
    row_shift <- ifelse(row_shift > -1, 1 + row_shift, row_shift)
    panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
    panels[, c("t", "b")] <- panels[, c("t", "b")] + row_shift

    table <- gtable_add_grob(
      table, table2[[grob_var]],
      t = panels[[pos]][table2$t],
      b = panels[[alt]][table2$b],
      l = panels$l[table2$l],
      r = panels$r[table2$r],
      clip = clip, z = z,
      name = paste0(name, "-", seq_along(table2$l), "-", seq_along(table2$t))
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

  # Ensure left-to-right ordering of unique panels
  cols <- panel_cols(table)
  cols <- sort(unique(cols[[pos]]))

  # Keep adding widths right-to-left
  for (i in rev(seq_along(cols))) {
    table <- gtable_add_cols(table, col_width[i], pos = cols[i] + col_shift)
  }

  if (!missing(table2)) {
    # Offset shift because we already added the columns
    col_shift <- ifelse(col_shift > -1, 1 + col_shift, col_shift)
    panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
    panels[, c("l", "r")] <- panels[, c("l", "r")] + col_shift

    table <- gtable_add_grob(
      table, table2[[grob_var]],
      t = panels$t[table2$t],
      b = panels$b[table2$b],
      l = panels[[pos]][table2$l],
      r = panels[[alt]][table2$r],
      clip = clip, z= z,
      name = paste0(name, "-", seq_along(table2$t), "-", seq_along(table2$l))
    )
  }
  table
}

p <- ggplot(mtcars, aes(disp, drat)) + geom_point()

grab_axis <- function(gt, where = "b") {
  gt$grobs[grepl(paste0("^axis-", where), gt$layout$name)]
}

test_that("facet_grid2 can duplicate axes and remove labels", {
  test <- p + facet_grid2(vs ~ am, axes = "all", remove_labels = "y")
  test <- ggplotGrob(test)

  b <- grab_axis(test, "b")
  btest <- vapply(b, inherits, logical(1), "absoluteGrob")
  expect_length(b, 4)
  expect_true(all(btest))

  l <- grab_axis(test, "l")
  ltest <- vapply(l, inherits, logical(1), "absoluteGrob")
  expect_length(l, 4)
  expect_true(all(ltest))

  b <- vapply(b, function(x){length(x$children[[2]]$grobs)}, integer(1))
  l <- vapply(l, function(x){length(x$children[[2]]$grobs)}, integer(1))

  if (!new_guide_system) {
    expect_equal(b, c(2L, 2L, 2L, 2L))
    expect_equal(l, c(1L, 1L, 2L, 2L))
  } else {
    expect_equal(b, c(3L, 3L, 3L, 3L))
    expect_equal(l, c(1L, 1L, 3L, 3L))
  }
})

test_that("facet_grid2 can have free and independent scales", {
  test <- p + facet_grid2(vs ~ am, scales = "free", independent = "all")
  ctrl <- p + facet_grid2(vs ~ am, scales = "free", independent = "none")

  test <- ggplot_build(test)
  ctrl <- ggplot_build(ctrl)

  test <- test$layout$layout
  ctrl <- ctrl$layout$layout

  expect_equal(test[, c("PANEL", "ROW", "COL", "vs", "am")],
               ctrl[, c("PANEL", "ROW", "COL", "vs", "am")])
  expect_equal(test$SCALE_X, 1:4)
  expect_equal(ctrl$SCALE_X, c(1L, 2L, 1L, 2L))
  expect_equal(test$SCALE_Y, 1:4)
  expect_equal(ctrl$SCALE_Y, c(1L, 1L, 2L, 2L))
})

test_that("facet_grid2 respects aspect ratio", {
  case_null <- p + facet_grid2(~ vs)
  case_asp  <- case_null + theme(aspect.ratio = 2)

  case_null <- ggplotGrob(case_null)
  case_asp  <- ggplotGrob(case_asp)

  panel_col <- panel_cols(case_null)$l
  panel_row <- panel_rows(case_null)$t

  expect_equal(as.character(case_null$widths[panel_col]), c("1null", "1null"))
  expect_equal(as.character(case_asp$widths[panel_col]),  c("1null", "1null"))

  expect_equal(as.character(case_null$heights[panel_row]), "1null")
  expect_equal(as.character(case_asp$heights[panel_row]),  "2null")

  expect_false(case_null$respect)
  expect_true(case_asp$respect)
})

test_that("facet_grid2 can use `render_empty` to omit panels", {

  case_null <- p + facet_grid2(vars(cyl), vars(gear), render_empty = TRUE)
  case_test <- p + facet_grid2(vars(cyl), vars(gear), render_empty = FALSE)

  case_null <- ggplotGrob(case_null)
  case_test <- ggplotGrob(case_test)

  is_panel_null <- grepl("^panel", case_null$layout$name)
  is_panel_test <- grepl("^panel", case_test$layout$name)

  expect_equal(is_panel_null, is_panel_test)

  null_zero <- vapply(case_null$grobs[is_panel_null], inherits, what = "zeroGrob", logical(1))
  expect_true(!any(null_zero))

  test_zero <- vapply(case_test$grobs[is_panel_test], inherits, what = "zeroGrob", logical(1))
  expect_equal(test_zero, c(rep(FALSE, 7), TRUE, FALSE))

})

test_that("facet_grid2 warns about inappropriate arguments", {
  expect_snapshot_error(facet_grid2(vs ~ am, independent = "x"))
  expect_snapshot_error(facet_grid2(vs ~ am, independent = "y"))
  expect_snapshot_warning(
    facet_grid2(vs ~ am, space = "free_x", independent = "x", scales = "free_x")
  )
  expect_snapshot_warning(
    facet_grid2(vs ~ am, independent = "y", space = "free_y", scales = "free_y")
  )
  expect_snapshot_warning(
    facet_grid2(vs ~ am, independent = "x", scales = "free_x",
                remove_labels = "x")
  )
  expect_snapshot_warning(
    facet_grid2(vs ~ am, independent = "y", scales = "free_y",
                remove_labels = "y")
  )
})

test_that("large facet combinations retain row-major panel order", {
  # The theoretical 4^16 combinations exceed the integer limit.
  df <- expand.grid(r = 1:4, c = 1:4)
  row_vars <- paste0("r", 1:8)
  col_vars <- paste0("c", 1:8)
  df[row_vars] <- rep(list(sprintf("r%02d", df$r)), length(row_vars))
  df[col_vars] <- rep(list(sprintf("c%02d", df$c)), length(col_vars))
  df$x <- "x"
  df$y <- sprintf("r%02d_a", df$r)
  df <- rbind(df, transform(df[df$r == 1L, ], y = "r01_b"))
  df <- df[order(-df$r, -df$c), ]

  for (facet in list(facet_grid2, facet_nested)) {
    plot <- ggplot(df, aes(x, y)) + geom_tile() +
      facet(
        rows = vars(!!!rlang::syms(row_vars)),
        cols = vars(!!!rlang::syms(col_vars)),
        scales = "free", space = "free"
      )
    built <- ggplot_build(plot)
    layout <- built$layout$layout
    expect_equal(as.integer(layout$PANEL), (layout$ROW - 1L) * 4L + layout$COL)

    grob <- ggplot_gtable(built)
    heights <- as.numeric(grob$heights[panel_rows(grob)$t])
    expect_equal(heights, c(2.2, 1.2, 1.2, 1.2))
  }
})

test_that("facet IDs can number combinations at the integer boundary", {
  variables <- list(
    structure(c(32768L, 1L), n = 32768L),
    structure(c(65536L, 1L), n = 65536L)
  )
  expect_identical(id(variables), structure(c(2L, 1L), n = 2^31))
})

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

  expect_equal(b, c(2L, 2L, 2L, 2L))
  expect_equal(l, c(1L, 1L, 2L, 2L))
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

test_that("facet_grid2 warns about inappropriate arguments", {
  expr <- substitute(facet_grid2(vs ~ am, independent = "x"))
  expect_error(eval(expr), "Rows cannot be independent")
  expr <- substitute(facet_grid2(vs ~ am, independent = "y"))
  expect_error(eval(expr), "Columns cannot be independent")
  expr <- substitute(facet_grid2(vs ~ am, space = "free_x",
                                 independent = "x", scales = "free_x"))
  expect_warning(eval(expr), "Rows cannot have free space")
  expr <- substitute(facet_grid2(vs ~ am, independent = "y", space = "free_y", scales = "free_y"))
  expect_warning(eval(expr), "Columns cannot have free space")
  expr <- substitute(facet_grid2(vs ~ am, independent = "x", scales = "free_x", remove_labels = "x"))
  expect_warning(eval(expr), "x-axes must be labelled")
  expr <- substitute(facet_grid2(vs ~ am, independent = "y", scales = "free_y", remove_labels = "y"))
  expect_warning(eval(expr), "y-axes must be labelled")
  expr <- substitute(facet_grid2(cols = TRUE))
  expect_error(eval(expr), "should not be logical")
  expr <- substitute(facet_grid2(switch = "bs"))
  expect_error(eval(expr), "Switch must be")
})

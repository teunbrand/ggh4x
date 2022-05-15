base <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point()

grab_axis <- function(plot, where = "b") {
  if (inherits(plot, "ggplot")) {
    gt <- ggplotGrob(plot)
  } else if (inherits(plot, "gtable")) {
    gt <- plot
  }
  grb <- gt$grobs[grep(paste0("axis-", where), gt$layout$name)][[1]]
  grb <- grb$children[vapply(grb$children, inherits, logical(1), "gtable")][[1]]
  return(grb)
}

test_that("guide_axis_scalebar can be placed at every position", {

  p <- base +
    guides(
      x = "axis_scalebar", x.sec = "axis_scalebar",
      y = "axis_scalebar", y.sec = "axis_scalebar"
    )
  gt <- ggplotGrob(p)

  test <- grab_axis(gt, "t")
  expect_s3_class(test, "gtable")
  expect_equal(dim(test), c(2, 1))

  test <- grab_axis(gt, "b")
  expect_s3_class(test, "gtable")
  expect_equal(dim(test), c(2, 1))

  test <- grab_axis(gt, "l")
  expect_s3_class(test, "gtable")
  expect_equal(dim(test), c(1, 2))


  test <- grab_axis(gt, "r")
  expect_s3_class(test, "gtable")
  expect_equal(dim(test), c(1, 2))
})

test_that("NULL breaks return zeroGrob as labels", {
  p <- base + scale_x_continuous(guide = "axis_scalebar", breaks = NULL)
  gt <- ggplotGrob(p)
  gt <- gt$grobs[[which(gt$layout$name == "axis-b")]]$children[[1]]
  expect_s3_class(gt, "zeroGrob")
})

test_that("guide_axis_scalebar has appropriate warnings", {

  g <- guide_axis_scalebar()
  sc <- scale_x_continuous(trans = "log10", breaks = NULL)
  sc$train(c(1, 2))
  sc <- ggplot2:::view_scale_primary(sc)

  expect_warning(guide_train(g, sc), "non-linear transformations")
  sc <- scale_x_continuous()
  sc$train(c(1, 2))
  sc <- ggplot2:::view_scale_primary(sc)
  sc$aesthetics <- "nonsense"
  expect_warning(guide_train(g, sc), "needs appropriate scales")

})


test_that("coord_axis_inside can place axes inside", {

  p <- ggplot(mtcars, aes(scale(mpg), scale(disp))) +
    geom_point() +
    theme_test() +
    theme(panel.border = element_blank(),
          axis.line = element_line())

  test <- p + coord_axes_inside(labels_inside = FALSE)
  test <- ggplotGrob(test)

  axis <- test$grobs[test$layout$name == "axis-b"][[1]]$children
  axis <- axis[names(axis) == "axis"][[1]]

  expect_s3_class(axis$grobs[[1]], "zeroGrob")
  expect_s3_class(axis$grobs[[2]], "titleGrob")

  axis <- test$grobs[test$layout$name == "axis-l"][[1]]$children
  axis <- axis[names(axis) == "axis"][[1]]

  if (new_guide_system) {
    expect_s3_class(axis$grobs[[1]], "zeroGrob")
    expect_s3_class(axis$grobs[[2]], "titleGrob")
  } else {
    expect_s3_class(axis$grobs[[2]], "zeroGrob")
    expect_s3_class(axis$grobs[[1]], "titleGrob")
  }

  test <- p + coord_axes_inside(labels_inside = TRUE)
  test <- ggplotGrob(test)

  axis <- test$grobs[test$layout$name == "axis-b"][[1]]$children
  axis <- axis[names(axis) == "axis"][[1]]

  expect_s3_class(axis$grobs[[1]], "zeroGrob")
  expect_s3_class(axis$grobs[[2]], "zeroGrob")

  axis <- test$grobs[test$layout$name == "axis-l"][[1]]$children
  axis <- axis[names(axis) == "axis"][[1]]

  expect_s3_class(axis$grobs[[2]], "zeroGrob")
  expect_s3_class(axis$grobs[[1]], "zeroGrob")
})

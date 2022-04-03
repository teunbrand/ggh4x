
test_that("coord_axis_inside can place axes inside", {

  p <- ggplot(mtcars, aes(scale(mpg), scale(disp))) +
    geom_point() +
    theme_test() +
    theme(panel.border = element_blank(),
          axis.line = element_line())

  vdiffr::expect_doppelganger(
    "axes inside, labels outside",
    p + coord_axes_inside(labels_inside = FALSE)
  )

  vdiffr::expect_doppelganger(
    "axes inside, labels inside",
    p + coord_axes_inside(labels_inside = TRUE)
  )


})

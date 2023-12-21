test_that("guide_stringlegend returns correct object", {
  xx <- guide_stringlegend(spacing.x = 1, spacing.y = 2, spacing = 3,
                           default.units = "mm")
  expect_s3_class(xx, "guide")
  expect_s3_class(xx, "legend")
  expect_s3_class(xx, "stringlegend")
  expect_equal(xx$label.spacing.x, unit(1, "mm"))
  expect_equal(xx$label.spacing.y, unit(2, "mm"))
})

test_that("guide_stringlegend can be placed in different spots", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
    geom_point()

  test <- p + guides(colour = guide_stringlegend(ncol = 2)) +
    theme(legend.position = "right")
  gt <- ggplotGrob(test)
  box_name <- if (new_guide_system) "guide-box-right" else "guide-box"
  extra <- if (new_guide_system) 2 else 0
  i <- which(gt$layout$name == box_name)
  expect_equal(gt$layout$l[i], 9 + extra)

  test <- p + guides(colour = guide_stringlegend(ncol = 2, byrow = TRUE,
                                                 title.position = "bottom")) +
    theme(legend.position = "bottom")
  box_name <- if (new_guide_system) "guide-box-bottom" else "guide-box"
  gt <- ggplotGrob(test)
  i <- which(gt$layout$name == box_name)
  expect_equal(gt$layout$t[i], 11 + extra)

  test <- p + guides(colour = guide_stringlegend(byrow = TRUE, title.position = "left")) +
    theme(legend.position = "left")
  box_name <- if (new_guide_system) "guide-box-left" else "guide-box"
  gt <- ggplotGrob(test)
  i <- which(gt$layout$name == box_name)
  expect_equal(gt$layout$l[i], 3)

  test <- p + guides(colour = guide_stringlegend(title.position = "right")) +
    theme(legend.position = "top")
  box_name <- if (new_guide_system) "guide-box-top" else "guide-box"
  gt <- ggplotGrob(test)
  i <- which(gt$layout$name == box_name)
  expect_equal(gt$layout$t[i], 5)
})


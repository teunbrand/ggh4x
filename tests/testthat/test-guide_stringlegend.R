test_that("guide_stringlegend returns correct object", {
  xx <- guide_stringlegend(spacing.x = 1, spacing.y = 2, spacing = 3,
                           default.units = "mm")
  expect_is(xx, "guide")
  expect_is(xx, "legend")
  expect_is(xx, "stringlegend")
  expect_equal(xx$label.spacing.x, unit(1, "mm"))
  expect_equal(xx$label.spacing.y, unit(2, "mm"))
})

test_that("guide_stringlegend can be placed in different spots", {
  p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
    geom_point()

  test <- p + guides(colour = guide_stringlegend(ncol = 2)) +
    theme(legend.position = "right")
  gt <- ggplotGrob(test)
  i <- which(gt$layout$name == "guide-box")
  expect_equal(gt$layout$l[i], 9)

  test <- p + guides(colour = guide_stringlegend(ncol = 2, byrow = TRUE,
                                                 title.position = "bottom")) +
    theme(legend.position = "bottom")
  gt <- ggplotGrob(test)
  i <- which(gt$layout$name == "guide-box")
  expect_equal(gt$layout$t[i], 11)

  test <- p + guides(colour = guide_stringlegend(byrow = TRUE, title.position = "left")) +
    theme(legend.position = "left")
  gt <- ggplotGrob(test)
  i <- which(gt$layout$name == "guide-box")
  expect_equal(gt$layout$l[i], 3)

  test <- p + guides(colour = guide_stringlegend(title.position = "right")) +
    theme(legend.position = "top")
  gt <- ggplotGrob(test)
  i <- which(gt$layout$name == "guide-box")
  expect_equal(gt$layout$t[i], 5)
})


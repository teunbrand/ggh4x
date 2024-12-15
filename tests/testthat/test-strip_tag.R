

test_that("strip_tag works as intended", {

  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point()

  gt <- ggplotGrob(
    p + facet_wrap2(~ year + drv, strip = strip_tag(just = c(0.5, 1)))
  )
  grob <- gt$grobs[[which(gt$layout$name == "strip-1")]]
  expect_equal(as.character(grob$widths), "1npc")
  expect_equal(as.character(grob$heights), c("0.5npc", "0.5npc"))

  expect_equal(as.numeric(grob$vp$width), 1.015, tolerance = 1e-3)
  expect_equal(as.numeric(grob$vp$height), 1.214, tolerance = 1e-3)

  grob <- grob$grobs[[1]]
  expect_s3_class(grob$children[[1]], "rect")
  expect_s3_class(grob$children[[2]], "titleGrob")
  expect_equal(grob$children[[2]]$children[[1]]$label, "1999")

})


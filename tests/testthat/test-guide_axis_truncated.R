base <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  theme(axis.line = element_line(colour = "black"))

build <- ggplot_build(base)

grab_axis <- function(plot, where = "b") {
  if (!inherits(plot, "gtable")) {
    gt <- ggplotGrob(plot)
  } else {
    gt <- plot
  }
  grb <- gt$grobs[grep(paste0("axis-", where), gt$layout$name)][[1]]
  grb <- grb$children[vapply(grb$children, inherits, logical(1), "gtable")][[1]]
  return(grb)
}

test_that("guide_axis_truncated construction works", {
  g <- guide_axis_truncated()
  expect_s3_class(g, "axis_truncated")

  # Test error
  expr <- substitute(guide_axis_truncated(trunc_lower = 2, trunc_upper = c(3, 4)))
  expect_error(eval(expr), "Axis truncation must have an equal number")
})

test_that("guide_axis_truncated training gives correct output", {
  scale <- build$layout$panel_params[[1]]$x

  g <- guide_axis_truncated()
  class(g) <- c("guide", "axis")
  g <- guide_train(g, scale, "x")

  test <- truncate_guide(g, scale, "x")
  expect_equal(test$trunc, data.frame(x = 2, xend = 5))

  g$trunc_lower <- 2.5
  g$trunc_upper <- 4.5

  test <- truncate_guide(g, scale, "x")
  expect_equal(test$trunc, data.frame(x = 2.5, xend = 4.5))

  g$trunc_lower <- unit(0.1, "npc")
  g$trunc_upper <- unit(0.9, "npc")

  test <- truncate_guide(g, scale, "x")
  expect_equal(test$trunc, .int$new_data_frame(list(x = unit(0.1, "npc"),
                                                    xend = unit(0.9, "npc"))))

  g$trunc_lower <- NULL
  g$trunc_upper <- NULL

  test <- truncate_guide(g, scale, "x")
  expect_equal(test$trunc, .int$new_data_frame(list(x = unit(0, "npc"),
                                                    xend = unit(1, "npc"))))

  g$trunc_lower <- 2.5
  g$trunc_upper <- c(4, 5)

  expr <- substitute(truncate_guide(g, scale, "x"))
  expect_error(eval(expr), "Axis truncation must have an equal number")

})

test_that("guide_axis_truncated can be placed at every position", {

  g <- guides(
    x = guide_axis_truncated(trunc_lower = unit(0.1, "npc"), trunc_upper = unit(0.9, "npc")),
    x.sec = guide_axis_truncated(),
    y = guide_axis_truncated(trunc_lower = 15, trunc_upper = 30),
    y.sec = guide_axis_truncated()
  )

  gt <- ggplotGrob(base + g)
  expect_s3_class(gt, "gtable")

  left <- gt$grobs[gt$layout$name == "axis-l"][[1]]$children[[1]]
  expect_false(identical(format(left$y), format(unit(c(0, 1), "npc"))))

  right <- gt$grobs[gt$layout$name == "axis-r"][[1]]$children[[1]]
  expect_false(identical(format(right$y), format(unit(c(0, 1), "npc"))))

  top <- gt$grobs[gt$layout$name == "axis-t"][[1]]$children[[1]]
  expect_false(identical(format(top$x), format(unit(c(0, 1), "npc"))))

  bottom <- gt$grobs[gt$layout$name == "axis-b"][[1]]$children[[1]]
  expect_true(identical(format(bottom$x), format(unit(c(0.1, 0.9), "npc"))))
})

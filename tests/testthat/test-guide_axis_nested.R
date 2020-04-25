test_that("guide_axis_nested work on x-axis", {
  g <- ggplot(mpg, aes(interaction(cyl, class), hwy)) +
    geom_boxplot() +
    scale_x_discrete(guide = "axis_nested")

  gt <- ggplotGrob(g)
  grob <- gt$grobs[gt$layout$name == "axis-b"][[1]]
  grob <- grob$children[names(grob$children) == "axis"][[1]]

  classes <- vapply(lapply(grob$grobs, class), `[`, character(1), 1)
  names(classes) <- NULL
  expect_equal(classes, c("polyline", "titleGrob", "polyline", "titleGrob"))

  titles <- grob$grobs[classes == "titleGrob"]
  small <- titles[[1]]$children[[1]]$label
  large <- titles[[2]]$children[[1]]$label
  expect_equal(length(small), nlevels(interaction(mpg$cyl, mpg$class, drop = T)))
  expect_true(all(large %in% unique(mpg$class)))
})

test_that("guide_axis_nested work on y-axis", {
  g <- ggplot(mpg, aes(hwy, interaction(cyl, class))) +
    geom_boxplot() +
    scale_y_discrete(guide = "axis_nested")

  gt <- ggplotGrob(g)
  grob <- gt$grobs[gt$layout$name == "axis-l"][[1]]
  grob <- grob$children[names(grob$children) == "axis"][[1]]

  classes <- vapply(lapply(grob$grobs, class), `[`, character(1), 1)
  names(classes) <- NULL
  expect_equal(classes, c("titleGrob", "polyline", "titleGrob", "polyline"))

  titles <- grob$grobs[classes == "titleGrob"]
  large <- titles[[1]]$children[[1]]$label
  small <- titles[[2]]$children[[1]]$label
  expect_equal(length(small), nlevels(interaction(mpg$cyl, mpg$class, drop = T)))
  expect_true(all(large %in% unique(mpg$class)))
})

test_that("guide_axis_nested work as secundary x-axis", {
  g <- ggplot(mpg, aes(interaction(cyl, class), hwy)) +
    geom_boxplot() +
    scale_x_discrete(position = "top", guide = "axis_nested")

  gt <- ggplotGrob(g)
  grob <- gt$grobs[gt$layout$name == "axis-t"][[1]]
  grob <- grob$children[names(grob$children) == "axis"][[1]]

  classes <- vapply(lapply(grob$grobs, class), `[`, character(1), 1)
  names(classes) <- NULL
  expect_equal(classes, c("titleGrob", "polyline", "titleGrob", "polyline"))

  titles <- grob$grobs[classes == "titleGrob"]
  large <- titles[[1]]$children[[1]]$label
  small <- titles[[2]]$children[[1]]$label
  expect_equal(length(small), nlevels(interaction(mpg$cyl, mpg$class, drop = T)))
  expect_true(all(large %in% unique(mpg$class)))
})

test_that("guide_axis_nested works as secundary y-axis", {
  g <- ggplot(mpg, aes(hwy, interaction(cyl, class))) +
    geom_boxplot() +
    scale_y_discrete(guide = "axis_nested", position = "right")

  gt <- ggplotGrob(g)
  grob <- gt$grobs[gt$layout$name == "axis-r"][[1]]
  grob <- grob$children[names(grob$children) == "axis"][[1]]

  classes <- vapply(lapply(grob$grobs, class), `[`, character(1), 1)
  names(classes) <- NULL
  expect_equal(classes, c("polyline", "titleGrob", "polyline", "titleGrob"))

  titles <- grob$grobs[classes == "titleGrob"]
  small <- titles[[1]]$children[[1]]$label
  large <- titles[[2]]$children[[1]]$label
  expect_equal(length(small), nlevels(interaction(mpg$cyl, mpg$class, drop = T)))
  expect_true(all(large %in% unique(mpg$class)))
})

test_that("guide_axis_nested errors upon misuse", {
  base <- ggplot(mpg, aes(interaction(cyl, class), hwy)) +
    geom_boxplot(aes(fill = class))
  g <- base + scale_fill_discrete(guide = "axis_nested")
  expect_error(ggplotGrob(g), "Guide 'axis' cannot be used for 'fill'.")

  gui <- guide_axis_nested()
  gui$available_aes <- "z"

  g <- base + scale_x_discrete(guide = gui)
  expect_warning(ggplotGrob(g), "axis guide needs appropriate scales: z")
})

test_that("NULL breaks return zeroGrob as labels", {
  g <- ggplot(mpg, aes(interaction(cyl, class), hwy)) +
    geom_boxplot() +
    scale_x_discrete(guide = "axis_nested", breaks = NULL)
  g <- ggplotGrob(g)
  g <- g$grobs[[which(g$layout$name == "axis-b")]]$children[[1]]
  expect_is(g, "zeroGrob")
})




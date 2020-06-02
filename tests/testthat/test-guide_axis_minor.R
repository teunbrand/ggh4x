base <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point()

grab_axis <- function(plot, where = "b") {
  gt <- ggplotGrob(plot)
  grb <- gt$grobs[grep(paste0("axis-", where), gt$layout$name)][[1]]
  grb <- grb$children[vapply(grb$children, inherits, logical(1), "gtable")][[1]]
  return(grb)
}

test_that("guide_axis_minor works on x-scales", {
  test <- base + scale_x_continuous(guide = "axis_minor")
  test <- grab_axis(test, "b")
  # Grab tick positions
  test <- grid::convertX(test$grobs[[1]]$y, "cm", valueOnly = TRUE)
  expect_length(unique(test), 3) # 1 at base, 1 for long ticks, 1 for short

  ctrl <- grab_axis(base, "b")
  ctrl <- grid::convertX(ctrl$grobs[[1]]$y, "cm", valueOnly = TRUE)
  expect_length(unique(ctrl), 2)
})

test_that("guide_axis_minor works on y-scales", {
  test <- base + scale_y_continuous(guide = "axis_minor")
  test <- grab_axis(test, "l")
  # Grab tick positions
  test <- grid::convertX(test$grobs[[2]]$x, "cm", valueOnly = TRUE)
  expect_length(unique(test), 3) # 1 at base, 1 for long ticks, 1 for short

  ctrl <- grab_axis(base, "l")
  ctrl <- grid::convertX(ctrl$grobs[[2]]$x, "cm", valueOnly = TRUE)
  expect_length(unique(ctrl), 2)
})

test_that("guide_axis_minor works on top x-scales", {
  test <- base + scale_x_continuous(guide = "axis_minor", position = "top")
  ctrl <- base + scale_x_continuous(position = "top")
  test <- grab_axis(test, "t")
  # Grab tick positions
  test <- grid::convertX(test$grobs[[2]]$y, "cm", valueOnly = TRUE)
  expect_length(unique(test), 3) # 1 at base, 1 for long ticks, 1 for short

  ctrl <- grab_axis(ctrl, "t")
  ctrl <- grid::convertX(ctrl$grobs[[2]]$y, "cm", valueOnly = TRUE)
  expect_length(unique(ctrl), 2)
})

test_that("guide_axis_minor works on right y-scales", {
  test <- base + scale_y_continuous(guide = "axis_minor", position = "right")
  ctrl <- base + scale_y_continuous(position = "right")
  test <- grab_axis(test, "r")
  # Grab tick positions
  test <- grid::convertX(test$grobs[[1]]$x, "cm", valueOnly = TRUE)
  expect_length(unique(test), 3) # 1 at base, 1 for long ticks, 1 for short

  ctrl <- grab_axis(ctrl, "r")
  ctrl <- grid::convertX(ctrl$grobs[[1]]$x, "cm", valueOnly = TRUE)
  expect_length(unique(ctrl), 2)
})

test_that("minor tick theme element works", {
  ctrl <- base + scale_x_continuous(guide = "axis_minor")
  test <- ctrl + theme(ggh4x.axis.ticks.length.minor = rel(2))

  ctrl <- grab_axis(ctrl, "b")
  test <- grab_axis(test, "b")

  expect_length(unique(ctrl$grobs[[1]]$y), 3)
  expect_length(unique(test$grobs[[1]]$y), 3)
})

test_that("NULL breaks return zeroGrob as labels", {
  g <- base + scale_x_continuous(guide = "axis_minor", breaks = NULL)
  g <- ggplotGrob(g)
  g <- g$grobs[[which(g$layout$name == "axis-b")]]$children[[1]]
  expect_is(g, "zeroGrob")
})

test_that("guide_axis_minor errors upon misuse", {
  g <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
    geom_point(aes(colour = Species)) +
    scale_colour_discrete(guide = "axis_minor")
  expect_error(ggplotGrob(g), "Guide 'axis' cannot be used for 'colour'.")

  gui <- guide_axis_minor()
  gui$available_aes <- "z"

  g <- base + scale_x_continuous(guide = gui)
  expect_warning(ggplotGrob(g), "axis_minor guide needs appropriate scales: z")
})

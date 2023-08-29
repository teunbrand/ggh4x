base <- ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point(na.rm = T)

grab_axis <- function(plot, where = "b", what = NULL) {
  gt <- ggplotGrob(plot)
  grb <- gt$grobs[grep(paste0("axis-", where), gt$layout$name)][[1]]
  grb <- grb$children[vapply(grb$children, inherits, logical(1), "gtable")][[1]]
  if (!is.null(what)) {
    i <- vapply(grb$grobs, function(x){any(class(x) %in% what)}, logical(1))
    grb <- grb$grobs[[head(which(i, 1))]]
  }
  return(grb)
}

test_that("guide_axis_logticks works on the bottom", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  test <- base + scale_x_log10(guide = "axis_logticks")
  ctrl <- base + scale_x_log10(guide = "axis")

  test <- grab_axis(test, "b", "polyline")
  ctrl <- grab_axis(ctrl, "b", "polyline")

  test <- grid::convertX(test$y, "cm", valueOnly = TRUE)
  expect_length(unique(test), 4)

  ctrl <- grid::convertX(ctrl$y, "cm", valueOnly = TRUE)
  expect_length(unique(ctrl), 2)
})

test_that("guide_axis_logticks works on the top", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  test <- base + scale_x_log10(guide = "axis_logticks",
                               position = "top")
  ctrl <- base + scale_x_log10(guide = "axis",
                               position = "top")

  test <- grab_axis(test, "t", "polyline")
  ctrl <- grab_axis(ctrl, "t", "polyline")

  test <- grid::convertX(test$y, "cm", valueOnly = TRUE)
  expect_length(unique(test), 4)

  ctrl <- grid::convertX(ctrl$y, "cm", valueOnly = TRUE)
  expect_length(unique(ctrl), 2)
})

test_that("guide_axis_logticks works on the left", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  test <- base + scale_y_log10(guide = "axis_logticks")
  ctrl <- base + scale_y_log10(guide = "axis")

  test <- grab_axis(test, "l", "polyline")
  ctrl <- grab_axis(ctrl, "l", "polyline")

  test <- grid::convertX(test$x, "cm", valueOnly = TRUE)
  expect_length(unique(test), 4)

  ctrl <- grid::convertX(ctrl$x, "cm", valueOnly = TRUE)
  expect_length(unique(ctrl), 2)
})

test_that("guide_axis_logticks works on the right", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  test <- base + scale_y_log10(guide = "axis_logticks",
                               position = "right")
  ctrl <- base + scale_y_log10(guide = "axis",
                               position = "right")

  test <- grab_axis(test, "r", "polyline")
  ctrl <- grab_axis(ctrl, "r", "polyline")

  test <- grid::convertX(test$x, "cm", valueOnly = TRUE)
  expect_length(unique(test), 4)

  ctrl <- grid::convertX(ctrl$x, "cm", valueOnly = TRUE)
  expect_length(unique(ctrl), 2)
})

test_that("guide_axis_logticks recognises untransformed axis", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  test <- base + scale_y_continuous(guide = "axis_logticks")
  ctrl <- base + scale_y_continuous(guide = "axis")

  test <- grab_axis(test, "l", "polyline")
  ctrl <- grab_axis(ctrl, "l", "polyline")

  expect_length(test$y, 8) # 4 ticks, 1 extra at 1.0
  expect_length(ctrl$y, 6) # 3 ticks
})

test_that("NULL breaks return zeroGrob as labels", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  g <- base + scale_x_continuous(guide = "axis_logticks", breaks = NULL)
  g <- ggplotGrob(g)
  g <- g$grobs[[which(g$layout$name == "axis-b")]]$children[[1]]
  expect_s3_class(g, "zeroGrob")
})

test_that("guide_axis_logticks errors upon misuse", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  g <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
    geom_point(aes(colour = Species)) +
    scale_colour_discrete(guide = "axis_logticks")

  if (!new_guide_system) {
    expect_snapshot_error(ggplotGrob(g))
  } else {
    expect_snapshot_warning(ggplotGrob(g))
  }

  gui <- guide_axis_logticks()
  gui$available_aes <- "z"

  g <- base + scale_x_continuous(guide = gui)
  expect_snapshot_warning(ggplotGrob(g))
})

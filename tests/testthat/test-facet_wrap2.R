
strip_layout <- function(p) {
  data <- ggplot_build(p)
  plot <- data$plot
  layout <- data$layout
  data <- data$data
  theme <- ggplot2:::plot_theme(plot)

  geom_grobs <- Map(function(l, d) { l$draw_geom(d, layout) },
                    plot$layers, data)


  facet <- layout$render(geom_grobs, data, theme, plot$labels)
  layout <- facet$layout
  strip_layout <- layout[grepl("^strip", layout$name), 1:4]
  as.list(strip_layout)
}

p <- ggplot(mtcars, aes(disp, drat)) + geom_point()

test_that("facet_wrap2() builds correct output", {
  wrap <- p + facet_wrap2(~cyl, axes = "all", remove_labels = "y")

  wrap_expected <- list(
    t = c(3, 3, 3),
    l = c(3, 7, 11),
    b = c(3, 3, 3),
    r = c(3, 7, 11)
  )

  expect_equal(strip_layout(wrap), wrap_expected)
})

grab_axis <- function(gt, where = "b") {
  gt$grobs[grepl(paste0("^axis-", where), gt$layout$name)]
}

nchildren <- function(gt, where) {
  axis <- grab_axis(gt, where)
  vapply(axis, function(x){
    if (!length(x$children)) {
      return(0L)
    }
    x <- x$children
    x <- x[names(x) == "axis"][[1]]
    length(x$grobs)
  }, integer(1))
}

test_that("facet_wrap2() can some repeat axes", {
  case <- p + facet_wrap2(am ~ cyl, axes = "x", dir = "v")
  ctrl <- p + facet_wrap2(am ~ cyl, axes = "margins", dir = "v")

  case <- ggplotGrob(case)
  ctrl <- ggplotGrob(ctrl)

  case <- grab_axis(case, "b")
  ctrl <- grab_axis(ctrl, "b")

  case <- vapply(case, ggplot2:::is.zero, logical(1))
  ctrl <- vapply(ctrl, ggplot2:::is.zero, logical(1))

  expect_equal(sum(case), 0L)
  expect_equal(sum(ctrl), 4L)
})

test_that("facet_wrap2 respects aspect ratio", {
  case_null <- p + facet_wrap2(~ vs)
  case_asp  <- case_null + theme(aspect.ratio = 2)

  case_null <- ggplotGrob(case_null)
  case_asp  <- ggplotGrob(case_asp)

  panel_col <- panel_cols(case_null)$l
  panel_row <- panel_rows(case_null)$t

  expect_equal(as.character(case_null$widths[panel_col]), c("1null", "1null"))
  expect_equal(as.character(case_asp$widths[panel_col]),  c("1null", "1null"))

  expect_equal(as.character(case_null$heights[panel_row]), "1null")
  expect_equal(as.character(case_asp$heights[panel_row]),  "2null")

  expect_false(case_null$respect)
  expect_true(case_asp$respect)
})

test_that("facet_wrap2() can remove some labels", {
  case1 <- p + facet_wrap2(am~cyl, axes = "all", remove_labels = "y")
  case2 <- p + facet_wrap2(am~cyl, axes = "all", remove_labels = "x")
  ctrl1 <- p + facet_wrap2(am~cyl, axes = "margins")
  ctrl2 <- p + facet_wrap2(am~cyl, axes = "all", remove_labels = "none")

  # Convert to gtables
  case1 <- ggplotGrob(case1)
  case2 <- ggplotGrob(case2)
  ctrl1 <- ggplotGrob(ctrl1)
  ctrl2 <- ggplotGrob(ctrl2)

  # Compare x-axis
  if (!new_guide_system) {
    expect_equal(nchildren(case1, "b"), rep(2L, 6))
    expect_equal(nchildren(case2, "b"), c(2L, 2L, 2L, 1L, 1L, 1L))
    expect_equal(nchildren(ctrl1, "b"), c(2L, 2L, 2L, 0L, 0L, 0L))
    expect_equal(nchildren(ctrl2, "b"), rep(2L, 6))
  } else {
    expect_equal(nchildren(case1, "b"), rep(3L, 6))
    expect_equal(nchildren(case2, "b"), c(3L, 3L, 3L, 2L, 2L, 2L))
    expect_equal(nchildren(ctrl1, "b"), c(3L, 3L, 3L, 0L, 0L, 0L))
    expect_equal(nchildren(ctrl2, "b"), rep(3L, 6))
  }

  # Compare y-axis
  if (!new_guide_system) {
    expect_equal(nchildren(case1, "l"), c(1L, 1L, 1L, 1L, 2L, 2L))
    expect_equal(nchildren(case2, "l"), rep(2L, 6))
    expect_equal(nchildren(ctrl1, "l"), c(0L, 0L, 0L, 0L, 2L, 2L))
    expect_equal(nchildren(ctrl2, "l"), rep(2L, 6))
  } else {
    expect_equal(nchildren(case1, "l"), c(2L, 2L, 2L, 2L, 3L, 3L))
    expect_equal(nchildren(case2, "l"), rep(3L, 6))
    expect_equal(nchildren(ctrl1, "l"), c(0L, 0L, 0L, 0L, 3L, 3L))
    expect_equal(nchildren(ctrl2, "l"), rep(3L, 6))
  }
})

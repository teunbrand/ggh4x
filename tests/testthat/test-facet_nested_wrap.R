# Setup basic plot --------------------------------------------------------

basic <- ggplot(mpg, aes(displ, hwy)) +
  geom_point()


# Basic tests -------------------------------------------------------------

test_that("facet_nested_wrap can be added to a plot", {
  g <- basic + facet_nested_wrap(vars(cyl, drv))
  expect_is(g$facet, "gg")
  expect_is(g$facet, "Facet")
  expect_is(g$facet, "FacetWrap2")
  expect_is(g$facet, "FacetNestedWrap")
})

test_that("facet_nested_wrap can be build", {
  g <- basic + facet_nested_wrap(vars(cyl, drv), dir = "v")
  g <- ggplot_build(g)
  expect_is(g, "ggplot_built")
  expect_is(g$layout, "gg")
})

test_that("facet_nested_wrap can be interpreted as gtable", {
  test <- basic + facet_nested_wrap(vars(cyl, drv))
  ctrl <- basic + facet_wrap(vars(cyl, drv))

  test <- ggplotGrob(test)
  ctrl <- ggplotGrob(ctrl)

  expect_equal(class(ctrl), class(test))
  expect_is(test, "gtable")
})

default <- basic + facet_nested_wrap(vars(cyl, drv))
default_gtable <- ggplotGrob(default)

test_that("facet_nested_wrap doesn't nest 1-dimensional strips",{
  test <- basic + facet_nested_wrap(vars(cyl))
  ctrl <- basic + facet_wrap(vars(cyl))

  test <- ggplotGrob(test)
  ctrl <- ggplotGrob(ctrl)

  test <- sum(grepl("strip", test$layout$name))
  ctrl <- sum(grepl("strip", ctrl$layout$name))

  expect_equal(test, ctrl)
})

test_that("facet_nested_wrap bleed argument works", {
  test <- basic + facet_nested_wrap(vars(cyl, drv), bleed = TRUE)

  test <- ggplotGrob(test)
  ctrl <- default_gtable

  test <- sum(grepl("strip", test$layout$name))
  ctrl <- sum(grepl("strip", ctrl$layout$name))

  expect_equal(test + 1, ctrl)
})

test_that("facet_nested_wrap nest_line parameter works", {
  test <- basic + facet_nested_wrap(vars(cyl, drv), nest_line = TRUE)

  test <- ggplotGrob(test)
  ctrl <- default_gtable

  test <- test$grobs[test$layout$name == "strip-t-1-3-1"][[1]]
  ctrl <- ctrl$grobs[ctrl$layout$name == "strip-t-1-3-1"][[1]]

  expect_equal(length(test$grobs), length(ctrl$grobs) + 1)
  expect_true("nester" %in% test$layout$name)
  expect_false("nester" %in% ctrl$layout$name)
})

test_that("facet_nested_wrap all strip positions are OK", {
  top <- basic + facet_nested_wrap(vars(cyl, drv), strip.position = "top")
  bottom <- basic + facet_nested_wrap(vars(cyl, drv), strip.position = "bottom")
  left <- basic + facet_nested_wrap(vars(cyl, drv), strip.position = "left", dir = "v")
  right <- basic + facet_nested_wrap(vars(cyl, drv), strip.position = "right", bleed = TRUE)

  tables <- lapply(list(top, bottom, left, right), ggplotGrob)

  nstrips <- vapply(tables, function(gt) {
    sum(grepl("strip", gt$layout$name))
  }, numeric(1))

  expect_equal(nstrips, c(13, 13, 13, 13))
})

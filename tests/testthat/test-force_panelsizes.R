# Setup basic plots -------------------------------------------------------

basic <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point()
colwrap <- basic + facet_wrap(~ Species, ncol = 3)
rowwrap <- basic + facet_wrap(~ Species, nrow = 3)


# Basic tests -------------------------------------------------------------

test_that("force_panelsizes updates facet class correctly", {
  ctrl <- colwrap + force_panelsizes()
  test1 <- colwrap + force_panelsizes(respect = TRUE)
  test2 <- colwrap + force_panelsizes(cols = 1:3)

  expect_is(ctrl$facet, "FacetWrap")
  expect_is(test1$facet, "FacetWrap")
  expect_is(test2$facet, "FacetWrap")

  expect_false(inherits(ctrl$facet, "ForcedFacetWrap"))
  expect_is(test1$facet, "ForcedFacetWrap")
  expect_is(test2$facet, "ForcedFacetWrap")
})

test_that("force_panelsizes can set respect", {
  ctrl <- colwrap + force_panelsizes(respect = FALSE)
  test <- colwrap + force_panelsizes(respect = TRUE)

  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)

  expect_false(ctrl$respect)
  expect_true(test$respect)
})

test_that("force_panelsizes overrides aspect ratio respect", {
  ctrl <- colwrap + theme(aspect.ratio = 1)
  test <- ctrl + force_panelsizes(respect = FALSE)

  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)

  expect_true(ctrl$respect)
  expect_false(test$respect)
})


# Forced size tests -------------------------------------------------------

test_that("force_panelsizes can set column widths", {
  ctrl <- colwrap + theme(aspect.ratio = 1)
  test <- ctrl + force_panelsizes(cols = 1:3)

  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)

  expect_equal(panel_cols(ctrl), panel_cols(test))
  panel_x <- panel_cols(test)$l

  test <- as.numeric(test$widths[panel_x])
  ctrl <- as.numeric(ctrl$widths[panel_x])

  expect_false(all(test == ctrl))
  expect_equal(ctrl, c(1,1,1))
  expect_equal(test, c(1,2,3))
})

test_that("force_panelsizes can set row heights", {
  ctrl <- rowwrap + theme(aspect.ratio = 1)
  test <- ctrl + force_panelsizes(rows = 1:3)

  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)

  expect_equal(panel_rows(ctrl), panel_rows(test))
  panel_y <- panel_rows(test)$t

  test <- as.numeric(test$heights[panel_y])
  ctrl <- as.numeric(ctrl$heights[panel_y])

  expect_false(all(test == ctrl))
  expect_equal(ctrl, c(1,1,1))
  expect_equal(test, c(1,2,3))
})

test_that("force_panelsizes can set rows and columns simultaneously", {
  wrap <- basic + facet_wrap(~ Species, ncol = 2)
  ctrl <- wrap + theme(aspect.ratio = 1)
  test <- ctrl + force_panelsizes(1:2, 1:2)
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)

  expect_equal(panel_rows(ctrl), panel_rows(test))
  expect_equal(panel_cols(ctrl), panel_cols(test))

  panel_x <- panel_cols(ctrl)$l
  panel_y <- panel_rows(ctrl)$t

  ctrl_x <- as.numeric(ctrl$widths[panel_x])
  test_x <- as.numeric(test$widths[panel_x])
  ctrl_y <- as.numeric(ctrl$heights[panel_y])
  test_y <- as.numeric(test$heights[panel_y])

  expect_false(all(ctrl_x == test_x))
  expect_false(all(ctrl_y == test_y))
  expect_equal(ctrl_x, c(1,1))
  expect_equal(ctrl_y, c(1,1))
  expect_equal(test_x, c(1,2))
  expect_equal(test_y, c(1,2))
})


# Unit tests --------------------------------------------------------------

test_that("force_panelsizes can set units on columns", {
  ctrl <- colwrap + theme(aspect.ratio = 1)
  test <- ctrl + force_panelsizes(cols = grid::unit.c(unit(1, "inch"),
                                                      unit(2, "cm"),
                                                      unit(3, "mm")))
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)

  expect_equal(panel_cols(ctrl), panel_cols(test))
  panel_pos <- panel_cols(ctrl)$l

  # Grab metrics
  ctrl <- ctrl$widths[panel_pos]
  test <- test$widths[panel_pos]

  ctrl_num <- as.numeric(ctrl)
  test_num <- as.numeric(test)

  ctrl_units <- attr(ctrl, "unit")
  test_units <- sapply(test, function(x){attr(x, "unit")})

  if (as.numeric(version$major) < 4) {
    expect_identical(
      ctrl, grid::unit.c(unit(1, "null"), unit(1, "null"), unit(1, "null"))
    )
    expect_equivalent(
      list(test[[1]], test[[2]], test[[3]]),
      list(unit(1, "inch"), unit(2, "cm"), unit(3, "mm"))
    )
  } else {
    expect_identical(as.numeric(ctrl), c(1, 1, 1))
    expect_true(all(grid::unitType(ctrl) == c("null", "null", "null")))
    expect_identical(as.numeric(test), c(1, 2, 3))
    expect_true(all(grid::unitType(test) == c("inches", "cm", "mm")))
  }
})

test_that("force_panelsizes can set units on rows", {
  ctrl <- rowwrap + theme(aspect.ratio = 1)
  test <- ctrl + force_panelsizes(rows = grid::unit.c(unit(1, "inch"),
                                                      unit(2, "cm"),
                                                      unit(3, "mm")))
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)

  expect_equal(panel_rows(ctrl), panel_rows(test))
  panel_pos <- panel_rows(ctrl)$t

  # Grab metrics
  ctrl <- ctrl$heights[panel_pos]
  test <- test$heights[panel_pos]

  ctrl_num <- as.numeric(ctrl)
  test_num <- as.numeric(test)

  if (as.numeric(version$major) < 4) {
    expect_identical(
      ctrl, grid::unit.c(unit(1, "null"), unit(1, "null"), unit(1, "null"))
    )
    expect_equivalent(
      list(test[[1]], test[[2]], test[[3]]),
      list(unit(1, "inch"), unit(2, "cm"), unit(3, "mm"))
    )
  } else {
    expect_identical(as.numeric(ctrl), c(1, 1, 1))
    expect_true(all(grid::unitType(ctrl) == c("null", "null", "null")))
    expect_identical(as.numeric(test), c(1, 2, 3))
    expect_true(all(grid::unitType(test) == c("inches", "cm", "mm")))
  }
})

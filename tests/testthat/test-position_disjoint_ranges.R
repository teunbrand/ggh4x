context("test-position_disjoint_ranges")


# Setup data --------------------------------------------------------------

df <- data.frame(xmin = c(1, 4, 7,  13, 19),
                 xmax = c(5, 8, 11, 17, 23),
                 ymin = 1,
                 ymax = 2,
                 group = c(1,2,3,4,3))
base <- ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))


# Basic tests -------------------------------------------------------------

test_that("position_disjoint_ranges repositions disjoint ranges", {

  ctrl <- base + geom_rect()
  test <- base + geom_rect(position = position_disjoint_ranges())

  ctrl <- layer_data(ctrl)
  test <- layer_data(test)

  expect_equal(ctrl[, c("xmin", "xmax")], test[, c("xmin", "xmax")])
  expect_false(identical(ctrl[, c("ymin", "ymax")], test[, c("ymin", "ymax")]))
  expect_equal(test$ymin, c(1,2,1,1,1))
  expect_equal(test$ymax, c(2,3,2,2,2))
})

test_that("position_disjoint_ranges can be used with geom_tile", {
  df2 <- data.frame(x = (df$xmin + df$xmax)/2,
                    y = (df$ymin + df$ymax)/2,
                    width = df$xmax - df$xmin,
                    height = df$ymax - df$ymin)
  base2 <- ggplot(df2, aes(x, y, width = width, height = height))

  ctrl <- base  + geom_rect(position = position_disjoint_ranges())
  test <- base2 + geom_tile(position = position_disjoint_ranges())

  ctrl <- layer_data(ctrl)
  test <- layer_data(test)

  isect <- intersect(names(ctrl), names(test))
  # default fills and sizes differ between geom_rect and geom_tile
  isect <- isect[!(isect %in% c("fill", "size"))]

  expect_equal(ctrl[,isect], test[,isect])
  expect_equal(test$ymin, c(1,2,1,1,1))
  expect_equal(test$ymax, c(2,3,2,2,2))
})

# Group tests -------------------------------------------------------------

test_that("position_disjoint_ranges respects groups", {

  ctrl <- base + geom_rect(aes(group = NULL), position = position_disjoint_ranges())
  test <- base + geom_rect(aes(group = group), position = position_disjoint_ranges())

  ctrl <- layer_data(ctrl)
  test <- layer_data(test)

  expect_equal(ctrl[, c("xmin", "xmax")], test[, c("xmin", "xmax")])
  expect_false(identical(ctrl[, c("ymin", "ymax")], test[, c("ymin", "ymax")]))
  expect_equal(ctrl$ymin, c(1,2,1,1,1))
  expect_equal(ctrl$ymax, c(2,3,2,2,2))
  expect_equal(test$ymin, c(1,2,1,2,1))
  expect_equal(test$ymax, c(2,3,2,3,2))
})

test_that("position_disjoint_ranges distinguishes proper groups from improper groups", {
  ctrl <- base + geom_rect(aes(group = NULL), position = position_disjoint_ranges())
  test <- base + geom_rect(aes(group = 1), position = position_disjoint_ranges())

  ctrl <- layer_data(ctrl)
  test <- layer_data(test)

  expect_equal(ctrl[, c("xmin", "xmax")], test[, c("xmin", "xmax")])
  expect_false(identical(ctrl[, c("ymin", "ymax")], test[, c("ymin", "ymax")]))
  expect_equal(ctrl$ymin, c(1,2,1,1,1))
  expect_equal(ctrl$ymax, c(2,3,2,2,2))
  expect_equal(test$ymin, c(1,1,1,1,1))
  expect_equal(test$ymax, c(2,2,2,2,2))
})

# Arguments Tests ---------------------------------------------------------

test_that("position_disjoint_ranges extends ranges", {
  ctrl <- base + geom_rect(position = position_disjoint_ranges(extend = 0))
  test <- base + geom_rect(position = position_disjoint_ranges(extend = 100))

  ctrl <- layer_data(ctrl)
  test <- layer_data(test)

  expect_equal(ctrl[, c("xmin", "xmax")], test[, c("xmin", "xmax")])
  expect_false(identical(ctrl[, c("ymin", "ymax")], test[, c("ymin", "ymax")]))
  expect_equal(ctrl$ymin, c(1,2,1,1,1))
  expect_equal(ctrl$ymax, c(2,3,2,2,2))
  expect_equal(test$ymin, 1:5)
  expect_equal(test$ymax, 2:6)
})

test_that("position_disjoint_ranges stepsize works", {
  ctrol <- base + geom_rect(position = position_disjoint_ranges(stepsize = 1))
  small <- base + geom_rect(position = position_disjoint_ranges(stepsize = 0.5))
  large <- base + geom_rect(position = position_disjoint_ranges(stepsize = 10))

  exp <- c(1,2,1,1,1)

  ctrol <- layer_data(ctrol)
  small <- layer_data(small)
  large <- layer_data(large)

  expect_equal(ctrol$ymin, exp)
  expect_equal(ctrol$ymin, ctrol$ymax - 1)
  expect_equal(small$ymin, 0.5 * (exp - 1) + 1)
  expect_equal(small$ymin, small$ymax - 1)
  expect_equal(large$ymin, 10 * (exp - 1) + 1)
  expect_equal(large$ymin, large$ymax - 1)
})

test_that("stat_funxy et al. constructors give correct objects", {
  x <- stat_funxy()
  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomPoint")
  expect_s3_class(x$stat, "StatFunxy")

  x <- stat_centroid()
  expect_s3_class(x$stat, "StatFunxy")
  x <- x$stat_params
  expect_equal(x$funx, mean)
  expect_equal(x$funy, mean)
  expect_equal(x$argx, list(na.rm = TRUE))
  expect_equal(x$argy, list(na.rm = TRUE))

  x <- stat_midpoint()
  expect_s3_class(x$stat, "StatFunxy")
})

test_that("stat_centroid calculates centroids", {
  g <- ggplot(iris, aes(Sepal.Width, Sepal.Length, group = Species)) +
    stat_centroid()
  g <- layer_data(g)

  ctrl <- aggregate(iris[,1:4], iris["Species"], mean)
  expect_equal(g$x, ctrl$Sepal.Width)
  expect_equal(g$y, ctrl$Sepal.Length)
})

test_that("stat_midpoint calculates midpoints", {
  g <- ggplot(iris, aes(Sepal.Width, Sepal.Length, group = Species)) +
    stat_midpoint()
  g <- layer_data(g)

  ctrl <- aggregate(iris[,1:4], iris["Species"], function(x){mean(range(x))})
  expect_equal(g$x, ctrl$Sepal.Width)
  expect_equal(g$y, ctrl$Sepal.Length)
})

test_that("stat_funxy throws appropriate errors", {
  xpr <- substitute(stat_funxy(funx = 10))
  expect_error(eval(xpr), "must be a function")
  xpr <- substitute(stat_funxy(funy = 10))
  expect_error(eval(xpr), "must be a function")
  xpr <- substitute(stat_funxy(argx = c(na.rm = TRUE)))
  expect_error(eval(xpr), "must be lists")
  xpr <- substitute(stat_funxy(argx = list(10)))
  expect_error(eval(xpr), "must have named elements")
  xpr <- substitute(stat_funxy(argy = list(10)))
  expect_error(eval(xpr), "must have named elements")
})

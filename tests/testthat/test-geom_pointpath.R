base <- ggplot(pressure, aes(temperature, pressure))

test_that("geom_pointpath adds geom to plot", {
  g <- base + geom_pointpath()
  expect_s3_class(g$layers[[1]]$geom, "GeomPointPath")
  # Check inherits GeomPoint
  expect_s3_class(g$layers[[1]]$geom, "GeomPoint")
})

test_that("geom_pointpath plots can be build", {
  g <- base + geom_pointpath()
  gt <- ggplotGrob(g)
  expect_s3_class(gt, "gtable")
  grob <- layer_grob(g)[[1]]$children
  expect_s3_class(grob[[2]], "points")
  expect_s3_class(grob[[1]], "gapsegments")
})

test_that("geom_pointpath makeContext works", {
  g <- base + geom_pointpath()
  grob <- layer_grob(g)[[1]]$children[[1]]
  expect_s3_class(grob, "gapsegments")
  out <- grid::makeContext(grob)
  expect_s3_class(out, "segments")
})

test_that("geom_pointpath plots can be build in polar coordinates", {
  g <- base + geom_pointpath() + coord_polar()
  gt <- ggplotGrob(g)
  expect_s3_class(gt, "gtable")
  grob <- layer_grob(g)[[1]]$children
  expect_s3_class(grob[[2]], "points")
  expect_s3_class(grob[[1]], "gapsegmentschain")
})

test_that("geom_pointpath makeContext works with polar coordinates", {
  g <- base + geom_pointpath() + coord_polar()
  grob <- layer_grob(g)[[1]]$children[[1]]
  expect_s3_class(grob, "gapsegmentschain")
  out <- grid::makeContext(grob)
  expect_s3_class(out, "polyline")
})

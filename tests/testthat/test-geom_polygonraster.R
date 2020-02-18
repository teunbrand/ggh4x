base <- ggplot(faithfuld, aes(eruptions, waiting, fill = density))

test_that("geom_polygonraster has correct class and inheritance", {
  g <- base + geom_polygonraster()
  g <- g$layers[[1]]$geom
  
  expect_is(g, "GeomPolygonRaster")
  expect_is(g, "GeomRaster")
})

test_that("geom_polygonraster outputs correct grob type", {
  g <- base + geom_polygonraster()
  g <- layer_grob(g)[[1]]
  
  expect_is(g, "polygon")
  expect_is(g, "grob")
})

test_that("geom_polygonraster reparameterises raster", {
  ctrl <- base + geom_raster()
  test <- base + geom_polygonraster()
  
  ctrl <- layer_data(ctrl)
  test <- layer_data(test)
  
  expect_equal(nrow(ctrl) * 4, nrow(test))
})

# Re-base

df <- data.frame(x = row(volcano)[T],
                 y = col(volcano)[T],
                 z = volcano[T])
base <- ggplot(df, aes(x, y, fill = z))

test_that("geom_polygonraster hjust works", {
  test1 <- base + geom_polygonraster(hjust = 0)
  test2 <- base + geom_polygonraster(hjust = 1)
  
  test1 <- layer_data(test1)
  test2 <- layer_data(test2)
  
  expect_identical(test1$y, test2$y)
  expect_identical(test1$x + 1, test2$x)
})

test_that("geom_polygonraster vjust works", {
  test1 <- base + geom_polygonraster(vjust = 0)
  test2 <- base + geom_polygonraster(vjust = 1)
  
  test1 <- layer_data(test1)
  test2 <- layer_data(test2)
  
  expect_identical(test1$y + 1, test2$y)
  expect_identical(test1$x, test2$x)
})

df <- data.frame(x = c(1, 0, 0, 1),
                 y = c(1, 1, 0, 0))

base <- ggplot(df, aes(x, y))

test_that("position_lineartrans can perform identity transformations", {
  M <- matrix(c(1, 0, 0, 1), 2)
  ctrl  <- base + geom_polygon(position = position_identity())
  test1 <- base + geom_polygon(position = position_lineartrans())
  test2 <- base + geom_polygon(position = position_lineartrans(M = M))
  
  ctrl  <- layer_data(ctrl)
  test1 <- layer_data(test1)
  test2 <- layer_data(test2)
  
  expect_identical(test1, test2)
  expect_identical(test1, ctrl)
})

test_that("position_lineartrans can scale data", {
  M <- matrix(c(2,0,0,2), 2)
  ctrl  <- base + geom_polygon(position = position_identity())
  test1 <- base + geom_polygon(position = position_lineartrans(scale = c(2, 2)))
  test2 <- base + geom_polygon(position = position_lineartrans(M = M))
  
  ctrl  <- layer_data(ctrl)
  test1 <- layer_data(test1)
  test2 <- layer_data(test2)
  
  expect_identical(test1, test2)
  expect_identical(test1$x, ctrl$x * 2)
  expect_identical(test1$y, ctrl$y * 2)
})

test_that("position_lineartrans can squeeze data", {
  M <- matrix(c(2, 0, 0, 0.5), 2)
  ctrl  <- base + geom_polygon(position = position_identity())
  test1 <- base + geom_polygon(position = position_lineartrans(scale = c(2, 0.5)))
  test2 <- base + geom_polygon(position = position_lineartrans(M = M))
  
  ctrl  <- layer_data(ctrl)
  test1 <- layer_data(test1)
  test2 <- layer_data(test2)
  
  expect_identical(test1, test2)
  expect_identical(test1$x, ctrl$x * 2)
  expect_identical(test1$y, ctrl$y * 0.5)
})

test_that("position_lineartrans can reflect data", {
  M <- matrix(c(1, 0, 0, -1), 2)
  ctrl  <- base + geom_polygon(position = position_identity())
  test1 <- base + geom_polygon(position = position_lineartrans(scale = c(1, -1)))
  test2 <- base + geom_polygon(position = position_lineartrans(M = M))
  
  ctrl  <- layer_data(ctrl)
  test1 <- layer_data(test1)
  test2 <- layer_data(test2)
  
  expect_identical(test1, test2)
  expect_identical(test1$x, ctrl$x)
  expect_identical(test1$y, ctrl$y * -1)
})

test_that("position_lineartrans can project data", {
  M <- matrix(c(0, 0, 0, 1), 2)
  ctrl  <- base + geom_polygon(position = position_identity())
  test1 <- base + geom_polygon(position = position_lineartrans(scale = c(0, 1)))
  test2 <- base + geom_polygon(position = position_lineartrans(M = M))
  
  ctrl  <- layer_data(ctrl)
  test1 <- layer_data(test1)
  test2 <- layer_data(test2)
  
  expect_identical(test1, test2)
  expect_identical(test1$x, ctrl$x * 0)
  expect_identical(test1$y, ctrl$y)
})

test_that("position_lineartrans can shear data vertically", {
  M <- matrix(c(1, 0.1, 0, 1), 2)
  ctrl  <- base + geom_polygon(position = position_identity())
  test1 <- base + geom_polygon(position = position_lineartrans(shear = c(0.1, 0)))
  test2 <- base + geom_polygon(position = position_lineartrans(M = M))
  
  ctrl  <- layer_data(ctrl)
  test1 <- layer_data(test1)
  test2 <- layer_data(test2)
  
  expect_identical(test1, test2)
  expect_identical(test1$x, ctrl$x)
  expect_identical(test1$y, ctrl$y + c(0.1, 0, 0, 0.1))
})

test_that("position_lineartrans can shear data horizontally", {
  M <- matrix(c(1, 0, 0.5, 1), 2)
  ctrl  <- base + geom_polygon(position = position_identity())
  test1 <- base + geom_polygon(position = position_lineartrans(shear = c(0, 0.5)))
  test2 <- base + geom_polygon(position = position_lineartrans(M = M))
  
  ctrl  <- layer_data(ctrl)
  test1 <- layer_data(test1)
  test2 <- layer_data(test2)
  
  expect_identical(test1, test2)
  expect_identical(test1$x, ctrl$x + c(0.5, 0.5, 0, 0))
  expect_identical(test1$y, ctrl$y)
})

test_that("position_lineartrans can rotate data", {
  M <- matrix(c(0, 1, -1, 0), 2)
  ctrl  <- base + geom_polygon(position = position_identity())
  test1 <- base + geom_polygon(position = position_lineartrans(angle = -90))
  test2 <- base + geom_polygon(position = position_lineartrans(M = M))
  
  ctrl  <- layer_data(ctrl)
  test1 <- layer_data(test1)
  test2 <- layer_data(test2)
  
  expect_equal(test1, test2)
  expect_identical(test2$x, ctrl$x[c(4, 1:3)] * -1)
  expect_identical(test2$y, ctrl$y[c(2:4, 1)])
})
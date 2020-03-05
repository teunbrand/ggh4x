df <- data.frame(
  xmin = c(1, 5),
  xmax = c(2, 7),
  ymin = c(1, 2),
  ymax = c(2, 4),
  fill = c("A", "B")
)

base <- ggplot(df, aes(xmin = xmin, xmax = xmax,
                       ymin = ymin, ymax = ymax,
                       fill = fill)) +
  geom_rect()

test_that("geom_rectmargin can be added to plots", {
  g <- base + geom_rectmargin()
  expect_is(g$layers[[2]]$geom, "GeomRectMargin")

  gt <- ggplotGrob(g)
  gt <- gt$grobs[grepl("panel", gt$layout$name)][[1]]
  gt <- gt$children[[4]]$children

  expect_is(gt[[1]], "rect")
  expect_is(gt[[2]], "rect")
})

test_that("geom_rectmargin recognises sides argument", {
  t <- base + geom_rectmargin(sides = "t")
  b <- base + geom_rectmargin(sides = "b")
  l <- base + geom_rectmargin(sides = "l")
  r <- base + geom_rectmargin(sides = "r")

  t <- layer_grob(t, 2)[[1]]$children[[1]]
  b <- layer_grob(b, 2)[[1]]$children[[1]]
  l <- layer_grob(l, 2)[[1]]$children[[1]]
  r <- layer_grob(r, 2)[[1]]$children[[1]]

  expect_equal(as.numeric(t$y), 1)
  expect_equal(as.numeric(b$y), 0)
  expect_equal(as.numeric(l$x), 0)
  expect_equal(as.numeric(r$x), 1)

  sizes <- c(t$height, b$height,
             r$width, l$width)
  expect_equal(sizes, c(0.03, 0.03, 0.03, 0.03))
})

test_that("geom_rectmargin size can be set", {
  a <- base + geom_rectmargin(length = unit(1, "inch"))
  b <- base + geom_rectmargin(length = unit(5, "mm"))
  a <- layer_grob(a, 2)[[1]]$children[[1]]$height
  b <- layer_grob(b, 2)[[1]]$children[[1]]$height
  expect_identical(a, unit(1, "inch"))
  expect_identical(b, unit(5, "mm"))
})

test_that("coord flip flips rectmargins", {
  a <- base + geom_rectmargin(sides = "b")
  b <- a + coord_flip()
  a <- layer_grob(a, 2)[[1]]$children[[1]]
  b <- layer_grob(b, 2)[[1]]$children[[1]]

  expect_equal(as.numeric(a$width), as.numeric(b$height))
})


# geom_tilemargin ------------------------------------------------------------

df <- data.frame(
  x = c(1, 4),
  y = c(1, 2),
  width = c(2, 1),
  height = c(1, 2),
  fill = c("A", "B")
)

base <- ggplot(df, aes(x, y,
                       width = width, height = height,
                       fill = fill)) +
  geom_tile()

test_that("geom_rectmargin can be added to plots", {
  g <- base + geom_tilemargin()
  expect_is(g$layers[[2]]$geom, "GeomTileMargin")
  expect_is(g$layers[[2]]$geom, "GeomRectMargin")

  gt <- ggplotGrob(g)
  gt <- gt$grobs[grepl("panel", gt$layout$name)][[1]]
  gt <- gt$children[[4]]$children

  expect_is(gt[[1]], "rect")
  expect_is(gt[[2]], "rect")
})

test_that("geom_tilemargin recognises sides argument", {
  t <- base + geom_tilemargin(sides = "t")
  b <- base + geom_tilemargin(sides = "b")
  l <- base + geom_tilemargin(sides = "l")
  r <- base + geom_tilemargin(sides = "r")

  t <- layer_grob(t, 2)[[1]]$children[[1]]
  b <- layer_grob(b, 2)[[1]]$children[[1]]
  l <- layer_grob(l, 2)[[1]]$children[[1]]
  r <- layer_grob(r, 2)[[1]]$children[[1]]

  expect_equal(as.numeric(t$y), 1)
  expect_equal(as.numeric(b$y), 0)
  expect_equal(as.numeric(l$x), 0)
  expect_equal(as.numeric(r$x), 1)

  sizes <- c(t$height, b$height,
             r$width, l$width)
  expect_equal(sizes, c(0.03, 0.03, 0.03, 0.03))
})

test_that("geom_tilemargin size can be set", {
  a <- base + geom_tilemargin(length = unit(1, "inch"))
  b <- base + geom_tilemargin(length = unit(5, "mm"))
  a <- layer_grob(a, 2)[[1]]$children[[1]]$height
  b <- layer_grob(b, 2)[[1]]$children[[1]]$height
  expect_identical(a, unit(1, "inch"))
  expect_identical(b, unit(5, "mm"))
})

test_that("coord flip flips tilemargins", {
  a <- base + geom_tilemargin(sides = "b")
  b <- a + coord_flip()
  a <- layer_grob(a, 2)[[1]]$children[[1]]
  b <- layer_grob(b, 2)[[1]]$children[[1]]

  expect_equal(as.numeric(a$width), as.numeric(b$height))
})

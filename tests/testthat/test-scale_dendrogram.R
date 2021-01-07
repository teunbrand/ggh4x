clus <- hclust(dist(USArrests), "ave")

test_that("scale_xy_dendrogram return correct scale types", {
  plain_x <- scale_x_dendrogram()
  plain_y <- scale_y_dendrogram()

  # Without hclust argument, should simply return discrete position scale
  expect_is(scale_x_dendrogram(), "ScaleDiscretePosition")
  expect_is(scale_y_dendrogram(), "ScaleDiscretePosition")
  expect_false(inherits(plain_x, "ScaleDendrogram"))
  expect_false(inherits(plain_y, "ScaleDendrogram"))

  proper_x <- scale_x_dendrogram(hclust = clus)
  proper_y <- scale_y_dendrogram(hclust = clus)

  # With hclust argument should return dendrogram scale
  expect_is(proper_x, "ScaleDiscretePosition")
  expect_is(proper_y, "ScaleDiscretePosition")
  expect_is(proper_x, "ScaleDendrogram")
  expect_is(proper_y, "ScaleDendrogram")

  # hclust object should be in scale
  expect_identical(proper_x$hclust, clus)
  expect_identical(proper_y$hclust, clus)
})

test_that("scale_xy_dendrogram sets guide correctly", {
  x <- scale_x_dendrogram(hclust = clus)
  y <- scale_y_dendrogram(hclust = clus)

  expect_is(x$guide, "guide")
  expect_is(x$guide, "dendroguide")
  expect_is(x$guide$dendro, "dendro")

  expect_is(y$guide, "guide")
  expect_is(y$guide, "dendroguide")
  expect_is(y$guide$dendro, "dendro")

  # Should not override manual axis
  x <- scale_x_dendrogram(guide = guide_axis())
  y <- scale_y_dendrogram(guide = guide_axis())

  expect_false(inherits(x$guide, "dendroguide"))
  expect_false(inherits(y$guide, "dendroguide"))
})

test_that("scale_xy_dendrogram reorders axis", {
  df <- data.frame(
    x = factor(rownames(USArrests)),
    y = rnorm(nrow(USArrests))
  )

  base <- ggplot(df, aes(x, y)) + geom_point()
  ctrl <- base + scale_x_discrete()
  test <- base + scale_x_dendrogram(hclust = clus)
  ctrl <- layer_data(ctrl)
  test <- layer_data(test)

  expect_equal(ctrl$x[order(clus$order)], test$x)
})

test_that("scale_xy_dendrogram draws dendrogram", {
  df <- data.frame(
    x = rownames(USArrests),
    y = rnorm(nrow(USArrests))
  )

  base <- ggplot(df, aes(x, y)) + geom_point()
  ctrl <- base + scale_x_discrete()
  test <- base + scale_x_dendrogram(hclust = clus)
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  ctrl <- ctrl$grobs[ctrl$layout$name == "axis-b"][[1]]
  test <- test$grobs[test$layout$name == "axis-b"][[1]]
  ctrl <- ctrl$children[[2]]
  test <- test$children[[2]]
  expect_false(identical(ctrl, test))
  expect_true(any(sapply(test$grobs, function(x) {
    startsWith(x$name, "GRID.segments")
  })))
})

test_that("scale_xy_dendrogram position is correct", {
  df <- data.frame(
    x = rownames(USArrests),
    y = rnorm(nrow(USArrests))
  )

  hor <- ggplot(df, aes(x, y)) + geom_point()
  ver <- ggplot(df, aes(y, x)) + geom_point()

  bottom <- hor + scale_x_dendrogram(hclust = clus, position = "bottom")
  top <- hor + scale_x_dendrogram(hclust = clus, position = "top")
  left <- ver + scale_y_dendrogram(hclust = clus, position = "left")
  right <- ver + scale_y_dendrogram(hclust = clus, position = "right")

  plot <- list(bottom = bottom, top = top, left = left, right = right)
  # Pull out the segments grob making up the dendrogram
  grobs <- lapply(setNames(names(plot), names(plot)), function(p) {
    gt <- ggplotGrob(plot[[p]])
    gt <- gt$grobs[gt$layout$name == paste0("axis-", substr(p, 1, 1))][[1]]
    gt <- gt$children[sapply(gt$children, inherits, "gtable")][[1]]
    gt <- gt$grobs[sapply(gt$grobs, inherits, "segments")][[1]]
    gt
  })

  # We check how many segment ends are at the relevant axis, which should equal
  # the number of leaves / samples
  bottom <- sum(unclass(grobs$bottom$y1) == 1)
  top <- sum(unclass(grobs$top$y1) == 0)
  left <- sum(unclass(grobs$left$x1) == 1)
  right <- sum(unclass(grobs$right$x1) == 0)

  expect_identical(bottom, nrow(df))
  expect_identical(top, nrow(df))
  expect_identical(left, nrow(df))
  expect_identical(right, nrow(df))

})

test_that("scale_xy_dendrogram can set labels", {
  df <- data.frame(
    x = factor(rownames(USArrests)),
    y = rnorm(nrow(USArrests))
  )

  base <- ggplot(df, aes(x, y)) + geom_point()
  ctrl <- base + scale_x_dendrogram(hclust = clus)
  # Verbatim
  test1 <- base + scale_x_dendrogram(hclust = clus,
                                     labels = seq_len(nrow(USArrests)))
  # By function
  test2 <- base + scale_x_dendrogram(hclust = clus,
                                     labels = toupper)

  cases <- list(ctrl = ctrl, test1 = test1, test2 = test2)
  cases <- lapply(cases, function(x) {
    x <- ggplotGrob(x)
    x <- x$grobs[x$layout$name == "axis-b"][[1]]$children[["axis"]]
    i <- vapply(x$grobs, inherits, logical(1), "titleGrob")
    x <- x$grobs[[which(i)]]$children[[1]]$label
    x
  })

  expect_identical(cases$test1, as.character(1:50))
  expect_false(identical(cases$ctrl, cases$test2))
  expect_identical(toupper(cases$ctrl), cases$test2)
})

test_that("scale_xy_dendrogram can be draw without labels", {
  df <- data.frame(
    x = factor(rownames(USArrests)),
    y = rnorm(nrow(USArrests))
  )

  base <- ggplot(df, aes(x, y)) + geom_point()
  ctrl <- base + scale_x_dendrogram(hclust = clus)
  test <- base + scale_x_dendrogram(hclust = clus, labels = NULL)

  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)

  ctrl <- ctrl$grobs[[which(ctrl$layout$name == "axis-b")]]$children[[2]][1,1]$grobs[[1]]
  test <- test$grobs[[which(test$layout$name == "axis-b")]]$children[[2]][1,1]$grobs[[1]]

  expect_is(ctrl, "titleGrob")
  expect_is(test, "zeroGrob")

})

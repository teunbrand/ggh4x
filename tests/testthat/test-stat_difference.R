test_that("stat_difference calculates the difference appropriately", {
  df <- data.frame(x = 1:2,
                   min = 1:2, max = 2:1)
  g <- ggplot(df, aes(x, ymin = min, ymax = max)) +
    stat_difference()

  ld <- layer_data(g)
  expect_equal(ld$x, c(1, 1.5, 1.5, 2))
  expect_equal(ld$ymin, c(1, 1.5, 1.5, 2))
  expect_equal(ld$ymax, c(2, 1.5, 1.5, 1))
})

test_that("stat_difference can be flipped", {
  df <- data.frame(y = 1:2,
                   min = 1:2, max = 2:1)
  g <- ggplot(df, aes(y = y, xmin = min, xmax = max)) +
    stat_difference()
  ld <- layer_data(g)
  expect_equal(ld$y, c(1, 1.5, 1.5, 2))
  expect_equal(ld$xmin, c(1, 1.5, 1.5, 2))
  expect_equal(ld$xmax, c(2, 1.5, 1.5, 1))
})

test_that("stat_difference can handle multiple groups", {
  df <- data.frame(
    x = c(1,2,3,4),
    min = c(1,2,1,2),
    max = c(2,1,2,1),
    group = c(1,1,2,2)
  )

  g <- ggplot(df, aes(x = x, ymin = min, ymax = max, group = group)) +
    stat_difference()
  ld <- layer_data(g)
  expect_equal(ld$group, c(1,1,2,2,3,3,4,4))
})

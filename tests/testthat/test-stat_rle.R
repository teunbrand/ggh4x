test_that("stat_rle constructor gives correct object", {
  x <- stat_rle()
  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomRect")
  expect_s3_class(x$stat, "StatRle")
  expect_named(x$stat_params, c("na.rm", "orientation", "align"))
})

test_that("stat_rle calculates runlengths correctly", {
  df <- data.frame(
    x = 1:30,
    y = c(rep(LETTERS[1:5], 5:1), rep(LETTERS[6:10], 1:5)),
    grp = rep(LETTERS[1:2], each = 15),
    stringsAsFactors = FALSE
  )

  g <- ggplot(df) +
    stat_rle(aes(x = x, label = y, group = grp))

  ld <- layer_data(g)
  expect_identical(ld$runlength, c(5:1, 1:5))
  expect_identical(ld$runvalue, LETTERS[1:10])
})

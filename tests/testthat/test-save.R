test_that("save_plot computes correct size", {

  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    guides(x = guide_none(NULL), y = guide_none(NULL)) +
    theme(plot.margin = margin(1, 1, 1, 1, unit = "in"))

  tmp <- tempfile(fileext = ".png")

  f <- suppressMessages(save_plot(tmp, plot = p))
  expect_equal(attr(f, "width"), NA_real_)
  expect_equal(attr(f, "height"), NA_real_)

  unlink(tmp)

  f <- save_plot(tmp, plot = p, width = 10, height = 5)
  expect_equal(attr(f, "width"), 10)
  expect_equal(attr(f, "height"), 5)

  unlink(tmp)

  f <- save_plot(
    tmp, plot = p + force_panelsizes(rows = unit(8, "in"), cols = unit(3, "in"))
  )
  # We add +2 because of margins
  expect_equal(attr(f, "width"), 5)
  expect_equal(attr(f, "height"), 10)

  unlink(tmp)
})

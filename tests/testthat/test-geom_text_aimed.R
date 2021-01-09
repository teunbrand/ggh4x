test_that("geom_text_aimed aims text", {
  tmpfile <- tempfile()
  png(tmpfile) # Angles are off otherwise

  df <- data.frame(
    x = c(1, 1, -1, -1),
    y = c(1, -1, -1, 1),
    xend = 0, yend = 0,
    label = LETTERS[1:4]
  )

  p <- ggplot(df, aes(x, y, xend = xend, yend = yend, label = label)) +
    theme(aspect.ratio = 1)

  grob <- layer_grob(p + geom_text_aimed())[[1]]

  expect_is(grob, "aimed_text")
  expect_equal(grob$rot, c(0, 0, 0, 0))

  rotgrob <- makeContent(grob)
  expect_equal(rotgrob$rot, c(315, 45, 315, 45))

  grob <- layer_grob(p + geom_text_aimed(flip_upsidedown = FALSE))[[1]]
  rotgrob <- makeContent(grob)
  expect_equal(rotgrob$rot, c(315, 45, 135, 225))
  dev.off()
  unlink(tmpfile)
})

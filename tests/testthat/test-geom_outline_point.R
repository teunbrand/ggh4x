test_that("geom_outline_point draws outlines", {

  df <- data.frame(x = 1:2)

  p <- ggplot(df, aes(x, x, colour = factor(x), stroke_colour = factor(x))) +
    geom_outline_point() +
    scale_colour_hue(aesthetics = "stroke_colour", l = 50) +
    theme_test()

  gt <- ggplotGrob(p)

  pnl <- gt$grobs[gt$layout$name == "panel"][[1]]$children
  pnl <- pnl[[which(startsWith(names(pnl), "outline"))]]$children
  pnl <- lapply(pnl, `[[`, "gp")
  expect_length(pnl, 2)

  expect_snapshot(pnl[[1]]$col)
  expect_snapshot(pnl[[2]]$col)

  # Check Key Drawing
  keys <- gt$grobs[gt$layout$name == "guide-box"][[1]]
  keys <- keys$grobs[keys$layout$name == "guides"][[1]]
  keys <- gtable::gtable_filter(keys, "key")
  keys <- gtable::gtable_filter(keys, "bg", invert = TRUE)

  keys <- lapply(keys$grobs, `[[`, "children")
  keys <- unlist(keys, recursive = FALSE)
  keys <- vapply(keys, inherits, logical(1), what = "points")
  expect_true(all(keys))
  expect_length(keys, 4)

})

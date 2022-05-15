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
})

test_that("geom_outline_point draws keys", {

  data <- data.frame(colour = "#F8766D",
                     stroke_colour = "#CB4D42",
                     shape = 16,
                     size = 1.5,
                     fill = NA, alpha = NA, stroke = 0.5)

  key <- draw_key_outline_point(data, list(na.rm = FALSE), size = c(6, 6))
  key <- key$children

  expect_length(key, 2)
  expect_s3_class(key[[1]], "points")
  expect_s3_class(key[[2]], "points")
  expect_equal(key[[2]]$gp$col, '#F8766DFF')
  expect_equal(key[[1]]$gp$col, "#CB4D42FF")
})

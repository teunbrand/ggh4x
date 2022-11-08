
test_that("boxes can be resolved with partial missing information", {

  ans <- list(
    resolve_box(min = 1, max    = 3),
    resolve_box(min = 1, center = 2),
    resolve_box(min = 1, dim    = 2),
    resolve_box(max = 3, center = 2),
    resolve_box(max = 3, dim    = 2),
    resolve_box(dim = 2, center = 2)
  )
  ans <- matrix(unlist(ans), 6, 2, byrow = TRUE)

  expect_equal(ans[, 1], rep(1, nrow(ans)))
  expect_equal(ans[, 2], rep(3, nrow(ans)))
})

test_that("boxes can be resolved with partial missing information", {
  test <- expand.grid(
    xmin  = c(1, NA),
    xmax  = c(3, NA),
    x     = c(2, NA),
    width = c(2, NA)
  )
  ans <- with(test, resolve_box(xmin, xmax, x, width))
  nas <- rowSums(is.na(test))

  expect_equal(
    ans$min,
    ifelse(nas > 2, NA_real_, 1)
  )
  expect_equal(
    ans$max,
    ifelse(nas > 2, NA_real_, 3)
  )
  expect_equal(sum(is.na(ans$min)), 5)
  expect_equal(sum(is.na(ans$max)), 5)
})

test_that("geom_box() builds expected grob", {

  df <- data.frame(xmin = c(NA, 1), x = c(1, 2), xmax = c(2, NA),
                   ymin = c(1, 2), height = 1)
  p <- ggplot(df) +
    geom_box(aes(xmin = xmin, xmax = xmax, x = x,
                 ymin = ymin, height = height), radius = unit(1, "cm"),
             fill = c("blue", "green"))

  lg <- layer_grob(p)[[1]]
  expect_s3_class(lg, "gTree")
  expect_s3_class(lg$children[[1]], "roundrect")
  expect_s3_class(lg$children[[2]], "roundrect")
})

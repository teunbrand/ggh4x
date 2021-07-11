# Construction ------------------------------------------------------------

test_that("facet_manual can be constructed", {
  test <- facet_manual(vars(a), design = "A")
  expect_s3_class(test, c("FacetManual", "FacetWrap2", "FacetWrap"))
})

test_that("facet_manual returns facet_null without vars", {
  test <- facet_manual(vars(), design = "A")
  expect_s3_class(test, c("FacetNull", "Facet"))
})

test_that("facet_manual matches widths/heights to design", {
  test <- facet_manual(vars(a), design = matrix(c(1,1,2,2), 2, 2),
                       widths = 1, height = c(0.5, 2))
  test <- test$params[c("widths", "heights")]
  expect_equal(test$widths, unit(c(1, 1), "null"))
  expect_equal(test$heights, unit(c(0.5, 2), "null"))
})


# Correctness -------------------------------------------------------------

test_that("facet_manual rejects some designs", {
  test <- substitute(validate_design(list(1, "A")))
  expect_error(eval(test), "should be interpretable as a matrix.")

  test <- substitute(validate_design("AA\nB"))
  expect_error(eval(test), "must be rectangular")

  test <- substitute(validate_design(NULL))
  expect_error(eval(test), "Cannot interpret")
})

test_that("facet_manual can build correct plots", {
  design <- "
   A##
   AB#
   #BC
   ##C
  "
  p <- ggplot(mtcars, aes(mpg, wt)) +
    geom_point() +
    facet_manual(vars(cyl), design)
  p <- ggplot_build(p)

  gt <- ggplot_gtable(p)$layout

  # Test panel positions
  panels <- gt[grepl("^panel-", gt$name), , drop = FALSE]
  expect_equal(unlist(panels[1:4], use.names = FALSE),
               c(8, 12, 17, 5, 9, 13, 12, 17, 20, 5, 9, 13))
  # Test axis positions
  axes_b <- gt[grepl("^axis-b-", gt$name), , drop = FALSE]
  expect_equal(unname(panels$b), unname(axes_b$b - 1))
  axes_l <- gt[grepl("^axis-l-", gt$name), , drop = FALSE]
  expect_equal(unname(panels$l), unname(axes_l$l) + 1)
  # Test strip positions
  strips <- gt[grepl("^strip-t-", gt$name), , drop = FALSE]
  expect_equal(unname(panels$t), unname(strips$t) + 1)
})

test_that("facet_manual can assume layouts", {
  design <- matrix(c(3,3,2,1), 2, 2)
  p <- ggplot(mtcars, aes(mpg, wt)) +
    geom_point() +
    facet_manual(vars(cyl), design, strip.position = "right") +
    scale_x_continuous(position = "top") +
    scale_y_continuous(position = "right") +
    theme(strip.placement = "outside")
  p <- ggplot_build(p)

  gtab <- ggplot_gtable(p)
  gt <- gtab$layout

  # Test panel positions
  panels <- gt[grepl("^panel-", gt$name), , drop = FALSE]
  expect_equal(unlist(panels[1:4], use.names = FALSE),
               c(11, 7, 7, 11, 11, 5, 11, 7, 11, 11, 11, 5))
  # Test axis positions
  axes_t <- gt[grepl("^axis-t-", gt$name), , drop = FALSE]
  expect_equal(unname(panels$t), unname(axes_t$t) + 1)
  axes_r <- gt[grepl("^axis-r-", gt$name), , drop = FALSE]
  expect_equal(unname(panels$r), unname(axes_r$r) - 1)
  # Test strip positions
  strips <- gt[grepl("^strip-r-", gt$name), , drop = FALSE]
  # 1 offset for axis, 1 offset for padding, 1 offset for strip
  expect_equal(unname(panels$r), unname(strips$r) - 3)
})

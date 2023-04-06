
test_that("scale_(x/y)_facet Error messages are thrown appropriately", {

  expect_error(
    scale_x_facet(type = "facet"),
    "Cannot circularly define"
  )

  expect_error(
    scale_y_facet(type = "nonsense"),
    "Cannot find a"
  )

  expect_error(
    scale_x_facet(),
    "must be a valid"
  )
})

test_that("scale_(x/y)_facet can be added to a plot", {

  p <- ggplot(mtcars, aes(disp, mpg)) +
    geom_point()

  expect_error(
    p + scale_x_facet(COL == 2),
    "Try adding facets"
  )

  expect_warning(
    p + facet_wrap(~ cyl) + scale_x_facet(COL == 2),
    "Attempting to add facetted x scales, while x scales are not free."
  )

  p <- p + facet_wrap(~ cyl, scales = "free")

  expect_s3_class(p$facet, "FacetWrap")

  p <- p + scale_y_facet(COL == 2, limits = c(0, 40))

  expect_s3_class(p$facet, "FreeScaledFacetWrap")

  expect_s3_class(p$facet$new_y_scales[[1]], "ScaleContinuousPosition")
  expect_length(p$facet$new_y_scales, 1L)
  expect_identical(
    attr(p$facet$new_y_scales, "lhs")[[1]],
    rlang::quo(COL == 2)
  )

  p <- p + scale_y_facet(COL == 1, breaks = 1:40)

  expect_s3_class(p$facet$new_y_scales[[2]], "ScaleContinuousPosition")
  expect_length(p$facet$new_y_scales, 2L)
  expect_length(attr(p$facet$new_y_scales, "lhs"), 2L)
  expect_identical(
    attr(p$facet$new_y_scales, "lhs")[[2]],
    rlang::quo(COL == 1)
  )

  p <- p + scale_x_facet(PANEL == 3, limits = c(0, 500))

  expect_s3_class(p$facet$new_x_scales[[1]], "ScaleContinuousPosition")
  expect_length(p$facet$new_x_scales, 1L)
  expect_length(attr(p$facet$new_x_scales, "lhs"), 1L)

})

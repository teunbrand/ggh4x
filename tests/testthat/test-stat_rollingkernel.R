test_that("kernels give correct weights", {
  x <- c(-1, 0, 1)

  gaus <- .kernel_norm(x, 2)
  cauc <- .kernel_cauchy(x, 2)
  unif <- .kernel_unif(x, 2)

  expect_identical(gaus, dnorm(x, sd = 2))
  expect_identical(cauc, dcauchy(x, scale = 2))
  expect_identical(unif, dunif(x, -1, 1))
  expect_equal(length(unique(unif)), 1)
})

test_that("stat_rollingkernel constructor gives correct object", {
  x <- stat_rollingkernel()
  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomLine")
  expect_s3_class(x$stat, "StatRollingkernel")
  expect_named(x$stat_params, c("bw", "kernel", "n", "expand", "na.rm",
                                "orientation"))
})

test_that("stat_rollingkernel can build a plot", {
  g <- ggplot(mpg, aes(displ, hwy, colour = class)) +
    geom_point() +
    stat_rollingkernel()
  ld <- layer_data(g, 2)
  expect_true(all(c("x", "y", "weight", "scaled") %in% names(ld)))
  expect_length(ld$x, length(unique(mpg$class)) * g$layers[[2]]$stat_params$n)
})

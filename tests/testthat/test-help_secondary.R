test_that("help_secondary does what it is supposed to", {

  sec <- help_secondary()

  expect_s3_class(sec, "AxisSecondary")

  formals <- formals(environment(sec$proj)$f)
  expect_identical(names(formals), "x")
})

test_that("help_sec range transforms correctly", {
  x <- -5:5
  y <- -10:10
  sec <- help_sec_range(x, y)

  expect_identical(sec$forward(range(y)), c(-5, 5))
  expect_identical(sec$reverse(range(x)), c(-10, 10))
})

test_that("help_sec max transforms correctly", {
  x <- 5:10
  y <- 15:20
  sec <- help_sec_max(x, y)

  expect_identical(sec$forward(range(y)), c(7.5, 10))
  expect_identical(sec$reverse(range(x)), c(10, 20))
})

test_that("help_sec sortfit transforms correctly", {
  x <- rnorm(20)
  y <- 2 + sample(x) * 5
  sec <- help_sec_sortfit(x, y)
  fit <- environment(sec$forward)$fit

  expect_equal(unname(fit), c(-0.4, 0.2))

  expect_equal(sort(x), sec$forward(sort(y)))
  expect_equal(sort(y), sec$reverse(sort(x)))
})

test_that("help_sec ccf transforms correctly", {
  z <- seq(0, 4*pi, pi/50)
  x <- sin(z)
  y <- 2 + sin(z + pi/2) * 5
  sec <- help_sec_ccf(x, y)
  fit <- environment(sec$forward)$fit
  expect_equal(unname(fit), c(-0.4, 0.2), tolerance = 0.05)

  # Test reverse for negative lag
  sec <- help_sec_ccf(y, x)
  fit <- environment(sec$forward)$fit
  expect_equal(unname(fit), c(2, 5), tolerance = 0.05)
})

test_that("try_require returns error when package is absent", {
  expect_error(try_require("nonsense", "test"),
               "Please install and try again")
})

test_that("try_require loads package namespace", {

  unloadNamespace("fitdistrplus")
  pkgs <- loadedNamespaces()
  expect_false("fitdistrplus" %in% pkgs)
  expect_false("package:fitdistrplus" %in% search())

  try_require("fitdistrplus", "test")
  pkgs <- loadedNamespaces()
  expect_true("fitdistrplus" %in% pkgs)
  expect_false("package:fitdistrplus" %in% search())
})

test_that("%||% does what it is supposed to do", {
  x <- NULL
  y <- 10
  z <- 5
  expect_equal(x %||% y, 10)
  expect_equal(z %||% y, 5)
})

test_that("function grabber grabs functions", {
  x <- .grab_ggplot_internals()
  classes <- table(sapply(x, class))
  expect_gt(classes["function"], 10)
})

test_that("weave_factors combines factors", {
  f1 <- c("banana", "apple", "apple", "kiwi", NA)
  f2 <- factor(c(1, NA, 1:3), labels = c("house", "cat", "dog"))

  a <- levels(weave_factors(f1, f2))
  expect_identical(a, c("banana.house", "apple.house", "apple.", "kiwi.cat", ".dog"))

  a <- levels(weave_factors(as.factor(f1), f2))
  expect_identical(a, c("apple.house", "apple.", "banana.house", "kiwi.cat", ".dog"))

  a <- weave_factors(f1, f2, dopr = TRUE)
  b <- weave_factors(f1, f2, drop = FALSE)

  expect_length(levels(a), 5)
  expect_length(levels(b), 4*4) # f2 NA becomes empty string level

  a <- levels(weave_factors(f1, f2, replaceNA = FALSE))
  expect_identical(a, c("banana.house", "apple.house", "kiwi.cat", "NA.dog"))

  a <- substitute(weave_factors(f1, f2[1:3]))
  expect_error(eval(a), "same length")

})

# Construction ------------------------------------------------------------

test_that("strip_vanilla can instantiate Strips", {
  test <- strip_vanilla()
  expect_s3_class(test, c("Strip", "ggproto"))
})

test_that("strip_themed can instantiate Strips", {
  test <- strip_themed()
  expect_s3_class(test, c("StripThemed", "Strip", "ggproto"))
})

test_that("strip_nested can instantiate Strips", {
  test <- strip_nested()
  expect_s3_class(test, c("StripNested", "StripThemed", "Strip", "ggproto"))
})

# Correctness -------------------------------------------------------------

# `strip_vanilla()` and `strip_nested()` are assumed to be tested sufficiently
# in `facet_wrap2()`/`facet_grid2()` and `facet_nested_wrap()`/`facet_nested()`
# respectively.
# Therefore, we just need to test `strip_themed` for correctness.

test_that("strip_themed inherits from theme", {
  my_theme <- theme_get() + theme(
    strip.background.x = element_rect(colour = "green", fill = "blue"),
    strip.text.y = element_text(family = "mono", colour = "red")
  )

  strip <- strip_themed(
    background_x = list(NULL, element_rect(colour = "blue"),
                        element_rect(fill = "green"))
  )
  elem <- strip$setup_elements(my_theme, "wrap")
  bg <- lapply(elem$background$x, `[[`, "gp")
  bg <- lapply(bg, unclass)

  # Backgrounds should already have been rendered as grobs
  # First element was NULL, so should be directly from theme
  expect_equal(bg[[1]][c("col", "fill")], list(col = "green", fill = "blue"))
  # Only fill comes from theme, col was specified
  expect_equal(bg[[2]][c("col", "fill")], list(col = "blue", fill = "blue"))
  # Only colour comes from theme, fill was specified
  expect_equal(bg[[3]][c("col", "fill")], list(col = "green", fill = "green"))

  strip <- strip_themed(
    text_y = list(element_blank(), element_text(family = "serif"))
  )
  elem <- strip$setup_elements(my_theme, "wrap")
  txt <- elem$text$y$left
  # Text should still be elements
  # First one was blank
  expect_equal(txt[[1]], element_blank())
  # Second one should have overwritten family but inherited colour
  expect_equal(txt[[2]][c("colour", "family")],
               list(colour = "red", family = "serif"))
  # Third one should not be evaluated until strip is placed
  expect_length(txt, 2)
})

test_that("strip_themed uses by_layer arguments correctly", {
  individ <- strip_themed(
    background_y = elem_list_rect(fill = c("green", "blue")),
    by_layer_y = FALSE
  )

  layered <- strip_themed(
    background_y = elem_list_rect(fill = c("green", "blue")),
    by_layer_y = TRUE
  )

  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point()

  individ <- p +
    facet_wrap2(
      vars("Top layer", drv), strip.position = "right", strip = individ
    )
  layered <- p +
    facet_wrap2(
      vars("Top layer", drv), strip.position = "right", strip = layered
    )
  individ <- ggplotGrob(individ)
  layered <- ggplotGrob(layered)

  # Test individual
  is_strip <- grepl("^strip-r-", individ$layout$name)
  lay <- individ$layout[is_strip,]
  expect_equivalent(lay[c("t", "l")],
                    list(t = c(7, 7, 7), l = c(6, 11, 16)))
  individ <- individ$grobs[is_strip]
  individ <- vapply(individ, function(x) {
    fills <- vapply(x$grobs, function(y) {
      y$children[[grep("^GRID\\.rect", names(y$children))]]$gp$fill
    }, character(1))
  }, character(2))
  expect_equal(as.vector(individ),
               c("green", "blue", "blue", "green", "green", "blue"))

  # Test layered
  is_strip <- grepl("^strip-r-", layered$layout$name)
  lay <- layered$layout[is_strip,]
  expect_equivalent(lay[c("t", "l")],
                    list(t = c(7, 7, 7), l = c(6, 11, 16)))
  layered <- layered$grobs[is_strip]
  layered <- vapply(layered, function(x) {
    fills <- vapply(x$grobs, function(y) {
      y$children[[grep("^GRID\\.rect", names(y$children))]]$gp$fill
    }, character(1))
  }, character(2))
  expect_equal(as.vector(layered),
               rep(c("green", "blue"), 3))
})

# Warnings and errors -----------------------------------------------------

test_that("strip_vanilla rejects faulty arguments", {
  test <- substitute(strip_vanilla(clip = "nonsense"))
  expect_error(eval(test), '`clip` must be one of "on", "off"')
  test <- substitute(strip_vanilla(size = "nonsense"))
  expect_error(eval(test), '`size` must be one of "constant" or "variable"')
})

test_that("strip_themed rejects faulty theme elements", {
  test <- substitute(strip_themed(background_x = "I'm not a theme element"))
  expect_error(eval(test), "should be a list of `element_rect` objects.")
  test <- substitute(strip_themed(text_y = element_line(colour = "blue")))
  expect_error(eval(test), "should be a list of `element_text` objects.")
})

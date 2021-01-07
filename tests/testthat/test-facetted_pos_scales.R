base <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(colour = Species))


# Basic tests -------------------------------------------------------------

test_that("facetted_pos_scales accepts NULL input", {
  x <- facetted_pos_scales(x = NULL, y = NULL)

  expect_equal(
    x,
    structure(list(x = list(NULL),
                   y = list(NULL)),
              class = "facetted_pos_scales")
  )

  # Also: adding this shouldn't modify facet
  ctrl <- base + facet_grid(~ Species, scales = "free")
  test <- ctrl + x

  expect_false(inherits(ctrl$facet, "FreeScaleFacetGrid"))
  expect_false(inherits(test$facet, "FreeScaleFacetGrid"))
})

test_that("facetted_pos_scales accepts a list of scales", {
  x <- facetted_pos_scales(x = list(scale_x_continuous(),
                                    scale_x_continuous()))

  expect_equal(names(x), c("x", "y"))
  expect_false("lhs" %in% names(attributes(x$x)))
  expect_null(x$y[[1]])
  expect_equal(unname(lengths(x)), c(2, 1))
  expect_is(x$x[[1]], "ScaleContinuous")
  expect_is(x$x[[2]], "ScaleContinuous")

})

test_that("facetted_pos_scales accepts formula input", {
  x <- facetted_pos_scales(x = list(Species == "setosa" ~ scale_x_continuous()))

  expect_equal(names(x), c("x", "y"))
  expect_true("lhs" %in% names(attributes(x$x)))
  expect_null(x$y[[1]])
  expect_equal(unname(lengths(x)), c(1, 1))
  expect_is(x$x[[1]], "ScaleContinuous")
})

test_that("facetted_pos_scales add to facet_grid correctly", {
  g <- base + facet_grid(~ Species, scales = "free") +
    facetted_pos_scales(x = list(NULL, scale_x_reverse()))

  facet <- g$facet
  expect_is(facet, "Facet")
  expect_is(facet, "FacetGrid")
  expect_is(facet, "FreeScaledFacetGrid")

  # Check relevant functions are updated
  expect_false(identical(
    body(environment(Facet$init_scales)$f),
    body(environment(facet$init_scales)$f)
  ))
  expect_false(identical(
    body(environment(Facet$train_scales)$f),
    body(environment(facet$train_scales)$f)
  ))
  expect_false(identical(
    body(environment(Facet$finish_data)$f),
    body(environment(facet$finish_data)$f)
  ))
})

test_that("facetted_pos_scales add to facet_wrap correctly", {
  g <- base + facet_wrap(~ Species, scales = "free") +
    facetted_pos_scales(x = list(NULL, scale_x_reverse()))

  facet <- g$facet
  expect_is(facet, "Facet")
  expect_is(facet, "FacetWrap")
  expect_is(facet, "FreeScaledFacetWrap")

  # Check relevant functions are updated
  expect_false(identical(
    body(environment(Facet$init_scales)$f),
    body(environment(facet$init_scales)$f)
  ))
  expect_false(identical(
    body(environment(Facet$train_scales)$f),
    body(environment(facet$train_scales)$f)
  ))
  expect_false(identical(
    body(environment(Facet$finish_data)$f),
    body(environment(facet$finish_data)$f)
  ))
})


# Essence tests -----------------------------------------------------------

test_that("facetted_pos_scales can make transformations on x", {
  a <- base + facet_wrap(~ Species, scales = "free")
  b <- a + facetted_pos_scales(
    x = list(Species == "versicolor" ~ scale_x_reverse())
  )

  a <- layer_data(a)
  b <- layer_data(b)

  # First and third panel are untouched
  expect_equivalent(a[a$PANEL %in% c(1,3), ],
                    b[b$PANEL %in% c(1,3), ])

  # Second panel coordinates are negative
  expect_equal(a[a$PANEL == 2, "x"] * -1,
               b[b$PANEL == 2, "x"])
})

test_that("facetted_pos_scales can make transformation on y", {
  a <- base + facet_grid(Species ~ ., scales = "free")
  b <- a + facetted_pos_scales(y = list(NULL, scale_y_reverse()))

  a <- layer_data(a)
  b <- layer_data(b)

  # First and third panel are untouched
  expect_equivalent(a[a$PANEL %in% c(1,3), ],
                    b[b$PANEL %in% c(1,3), ])

  # Second panel coordinates are negative
  expect_equal(a[a$PANEL == 2, "y"] * -1,
               b[b$PANEL == 2, "y"])
})

test_that("facetted_pos_scales can set limits", {
  a <- base + facet_grid(Species ~ ., scales = "free")
  b <- a + facetted_pos_scales(y = list(NULL,
                                        scale_y_continuous(limits = c(0, 100))))

  a <- ggplotGrob(a)
  b <- ggplotGrob(b)

  a <- a$grobs[grepl("axis-l", a$layout$name)]
  b <- b$grobs[grepl("axis-l", b$layout$name)]

  a <- lapply(a, function(x) {
    x$children$axis$grobs[[1]]$children[[1]]$label
  })
  b <- lapply(b, function(x) {
    x$children$axis$grobs[[1]]$children[[1]]$label
  })

  expect_identical(a[[1]], b[[1]])
  expect_false(identical(a[[2]], b[[2]]))
  expect_identical(a[[3]], b[[3]])

  expect_identical(b[[2]], as.character(seq(0, 100, by = 25)))
})

test_that("facetted_pos_scales can set breaks", {
  a <- base + facet_grid(Species ~ ., scales = "free")
  b <- a + facetted_pos_scales(
    y = list(NULL,
             scale_y_continuous(breaks = range))
  )

  a <- ggplotGrob(a)
  b <- ggplotGrob(b)

  a <- a$grobs[grepl("axis-l", a$layout$name)]
  b <- b$grobs[grepl("axis-l", b$layout$name)]

  a <- lapply(a, function(x) {
    x$children$axis$grobs[[1]]$children[[1]]$label
  })
  b <- lapply(b, function(x) {
    x$children$axis$grobs[[1]]$children[[1]]$label
  })

  expect_identical(a[[1]], b[[1]])
  expect_false(identical(a[[2]], b[[2]]))
  expect_identical(a[[3]], b[[3]])

  expect_true(length(a[[2]]) == 3)
  expect_true(length(b[[2]]) == 2)
})


test_that("facetted_pos_scales can set labels", {
  a <- base + facet_grid(Species ~ ., scales = "free")
  b <- a + facetted_pos_scales(y = list(NULL,
                                        scale_y_continuous(labels = function(x) x*100)))

  a <- ggplotGrob(a)
  b <- ggplotGrob(b)

  a <- a$grobs[grepl("axis-l", a$layout$name)]
  b <- b$grobs[grepl("axis-l", b$layout$name)]

  a <- lapply(a, function(x) {
    x$children$axis$grobs[[1]]$children[[1]]$label
  })
  b <- lapply(b, function(x) {
    x$children$axis$grobs[[1]]$children[[1]]$label
  })

  expect_identical(a[[1]], b[[1]])
  expect_false(identical(a[[2]], b[[2]]))
  expect_identical(a[[3]], b[[3]])

  expect_identical(b[[2]], as.character(c(200, 250, 300)))
})

test_that("facetted_pos_scales can set expand arguments", {
  a <- base + facet_grid(Species ~ ., scales = "free")
  b <- a + facetted_pos_scales(y = list(NULL,
                                        scale_y_continuous(expand = c(10, 0))))

  a <- ggplotGrob(a)
  b <- ggplotGrob(b)

  a <- a$grobs[grepl("axis-l", a$layout$name)]
  b <- b$grobs[grepl("axis-l", b$layout$name)]

  a <- lapply(a, function(x) {
    x$children$axis$grobs[[1]]$children[[1]]$label
  })
  b <- lapply(b, function(x) {
    x$children$axis$grobs[[1]]$children[[1]]$label
  })

  expect_identical(a[[1]], b[[1]])
  expect_false(identical(a[[2]], b[[2]]))
  expect_identical(a[[3]], b[[3]])

  expect_identical(b[[2]], as.character(c(-10, 0, 10)))
})

test_that("facetted_pos_scales can set position arguments", {
  a <- base + facet_grid(Species ~ ., scales = "free")
  b <- a + facetted_pos_scales(y = list(NULL,
                                        scale_y_continuous(position = "right")))

  a <- ggplotGrob(a)
  b <- c <- ggplotGrob(b)

  a <- a$grobs[grepl("axis-l", a$layout$name)]
  b <- b$grobs[grepl("axis-l", b$layout$name)]
  c <- c$grobs[grepl("axis-r", c$layout$name)]

  a <- lapply(a, function(x) {
    x$children$axis$grobs[[1]]$children[[1]]$label
  })
  b <- lapply(b, function(x) {
    x$children$axis$grobs[[1]]$children[[1]]$label
  })
  c <- lapply(c, function(x) {
    x$children$axis$grobs[[2]]$children[[1]]$label
  })

  expect_identical(a[[1]], b[[1]])
  expect_false(identical(a[[2]], b[[2]]))
  expect_identical(a[[3]], b[[3]])
  expect_identical(c[[2]], a[[2]])

  expect_null(c[[1]])
  expect_null(b[[2]])
  expect_null(c[[3]])
})

test_that("facetted_pos_scales can set secondary axis", {
  a <- base + facet_grid(Species ~ ., scales = "free")
  b <- a + facetted_pos_scales(y = list(
    NULL,
    scale_y_continuous(sec.axis = sec_axis(~ .))
  ))

  a <- ggplotGrob(a)
  b <- c <- ggplotGrob(b)

  a <- a$grobs[grepl("axis-l", a$layout$name)]
  b <- b$grobs[grepl("axis-l", b$layout$name)]
  c <- c$grobs[grepl("axis-r", c$layout$name)]

  a <- lapply(a, function(x) {
    x$children$axis$grobs[[1]]$children[[1]]$label
  })
  b <- lapply(b, function(x) {
    x$children$axis$grobs[[1]]$children[[1]]$label
  })
  c <- lapply(c, function(x) {
    x$children$axis$grobs[[2]]$children[[1]]$label
  })

  expect_identical(a[[1]], b[[1]])
  expect_identical(a[[2]], b[[2]])
  expect_identical(a[[3]], b[[3]])
  expect_identical(c[[2]], a[[2]])

  expect_null(c[[1]])
  expect_null(c[[3]])
})

test_that("facetted_pos_scales can handle empty panels", {
  df <- cbind(expand.grid(1:2, 1:2), x = 1:4)
  df <- df[-3,]

  g <- ggplot(df, aes(x, x)) +
    geom_point() +
    facet_grid(Var1 ~ Var2) +
    facetted_pos_scales(y = list(
      scale_y_continuous(),
      scale_y_reverse()
    ))
  expect_silent(print(g))
})


# Warning tests -----------------------------------------------------------

test_that("facetted_pos_scales warns about invalid scales", {

  # Nonsensical input
  test <- substitute(facetted_pos_scales(y = list(
    NULL, scale_y_continuous(), "nonsense")
  ))
  expect_error(eval(test), "Invalid facetted scale")

  # Incompatible aesthetics (x-scale to y-argument)
  test <- substitute(facetted_pos_scales(y = list(
    NULL, scale_x_continuous()
  )))
  expect_error(eval(test), "Invalid facetted scale")
})

test_that("facetted_pos_scales warns about invalid scales in formulas", {
  g <- base + facet_grid(~ Species, scales = "free_y")

  test <- substitute(facetted_pos_scales(y = list(
    Species == "versicolor" ~ "Nonsense"
  )))
  expect_error(eval(test),
               "RHS of formula does not result in appropriate scale.")
})

test_that("facetted_pos_scales warns about unusual facets", {
  g <- base + facet_grid(~ Species, scales = "free_y")
  oldfacet <- g$facet
  altfacet <- ggproto(
    "TestFacet",
    oldfacet,
    finish_data = function(data, layout, x_scales, y_scales, params) {
      print("This function is not identical to the original")
      data
    }
  )
  g$facet <- altfacet

  test <- substitute(g + facetted_pos_scales(y = list(
    NULL, scale_y_reverse()
  )))

  expect_warning(eval(test), "Unknown facet")
})

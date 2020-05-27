test_that("element_part_rect returns correct class", {
  case1 <- element_part_rect(side = "tlbr")
  case2 <- element_part_rect(side ="nonsense")
  case3 <- element_part_rect(side = "tl")

  expect_false(inherits(case1, "element_part_rect"))
  expect_false(inherits(case2, "element_part_rect"))
  expect_true(inherits(case3, "element_part_rect"))
})

test_that("element_part_rect draws grobs correctly", {
  cases <- list(
    l = element_part_rect(side = "l"),
    r = element_part_rect(side = "r"),
    b = element_part_rect(side = "b"),
    t = element_part_rect(side = "t")
  )

  grobs <- lapply(cases, element_grob)
  grobs <- lapply(grobs, function(grob) {
    expect_length(grob$children, 2)
    grob$children[[2]]
  })
})

test_that("element_part_rect can be used in a ggplot", {
  g <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
    geom_point() +
    facet_wrap(~ "Irises!") +
    theme(
      strip.background = element_part_rect(side = "lrb", colour = "black")
    )
  gt <- ggplotGrob(g)

  strip <- gt$grobs[gt$layout$name == "strip-t-1-1"][[1]]
  strip <- strip$grobs[[1]]$children
  strip <- strip[grepl("background", names(strip))][[1]]
  expect_identical(names(strip$children), c("fillgrob", "sidegrob"))
})

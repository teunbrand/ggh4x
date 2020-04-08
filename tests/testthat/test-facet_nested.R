# Setup basic tests -------------------------------------------------------

# Setup nested data
df <- cbind.data.frame(
  iris,
  nester = ifelse(iris$Species == "setosa",
                  "Short Leaves", "Long Leaves")
)

# Setup a basic plot
basic <- ggplot(df, aes(Sepal.Length, Petal.Length)) +
  geom_point()


# Basic tests -------------------------------------------------------------

test_that("facet_nested can be added to a plot", {
  g <- basic + facet_nested()
  expect_is(g$facet, "gg")
  expect_is(g$facet, "Facet")
  expect_is(g$facet, "FacetGrid")
  expect_is(g$facet, "FacetNested")
})

test_that("facet_nested can be build", {
  g <- basic + facet_nested(~ nester + Species)
  g <- ggplot_build(g)
  expect_is(g, "ggplot_built")
  expect_is(g$layout, "gg")
  expect_is(g$layout, "Layout")
  expect_is(g$plot, "gg")
  expect_is(g$plot, "ggplot")
})

test_that("facet_nested can be interpreted as gtable", {
  # Build plots
  test <- basic + facet_nested(~ nester + Species)
  ctrl <- basic + facet_grid(~ nester + Species)

  # Convert to gtables
  test <- ggplotGrob(test)
  ctrl <- ggplotGrob(ctrl)

  # Tests
  expect_equal(class(ctrl), class(test))
  expect_is(test, "gtable")
})

test_that("facet_nested splits up data", {
  # Build plots
  ctrl <- basic + facet_grid(~ nester + Species)
  hori <- basic + facet_nested(~ nester + Species)
  vert <- basic + facet_nested(nester + Species ~ .)

  # Grab data
  ctrl <- layer_data(ctrl)
  hori <- layer_data(hori)
  vert <- layer_data(vert)

  # Test
  expect_equal(hori$PANEL, factor(rep(c(3,1,2), each = 50)))
  expect_equal(hori, ctrl)
  expect_equal(hori, vert)
})

test_that("facet_nested returns helpful error messages", {
  # Upon misspelled formula
  ctrl <- basic + facet_nested(~ nester + Species)
  test <- basic + facet_nested(~ Nester + Species)
  ctrl <- expect_silent(layer_data(ctrl))
  test <- expect_error(layer_data(test), "Plot is missing")

  # Upon invalid switch
  test <- substitute(basic + facet_nested(~ nester + Species,
                                          switch = "Nonsense"))
  expect_error(eval(test), "switch must be either")
})

# Strip nesting tests -----------------------------------------------------

test_that("facet_nested can draw multiple panel and strips", {
  # Build plots
  test <- basic + facet_nested(~ Species)
  ctrl <- basic + facet_grid(~ Species)

  # Grab gtable layout names
  test <- ggplotGrob(test)$layout$name
  ctrl <- ggplotGrob(ctrl)$layout$name

  # Grab metrics
  test_npanels <- sum(grepl("panel", test))
  test_nstrips <- sum(grepl("strip", test))
  ctrl_npanels <- sum(grepl("panel", ctrl))
  ctrl_nstrips <- sum(grepl("strip", ctrl))

  # Test
  expect_equal(test_npanels, ctrl_npanels)
  expect_equal(test_nstrips, ctrl_nstrips)
  expect_equal(test_npanels, 3)
  expect_equal(test_nstrips, 3)
})

test_that("facet_nested can nest strips", {
  # Build plots
  test <- basic + facet_nested(~ nester + Species)
  ctrl <- basic + facet_grid(~ nester + Species)

  # Grab gtable layout names
  test <- ggplotGrob(test)$layout$name
  ctrl <- ggplotGrob(ctrl)$layout$name

  # Grab metrics
  test_npanels <- sum(grepl("panel", test))
  test_nstrips <- sum(grepl("strip", test))
  ctrl_npanels <- sum(grepl("panel", ctrl))
  ctrl_nstrips <- sum(grepl("strip", ctrl))

  # Test
  expect_equal(test_npanels, ctrl_npanels)
  expect_equal(test_npanels, 3)
  expect_equal(test_nstrips, 5)
  expect_equal(ctrl_nstrips, 3)
})

# Nesting line tests ------------------------------------------------------

test_that("facet_nested can draw nesting lines horizontally", {
  # Build gtable
  g <- basic + facet_nested(~ nester + Species, nest_line = TRUE)
  g <- ggplotGrob(g)
  strp <- g$grobs[g$layout$name == "strip-t-1-1"][[1]]

  # Grab metrics
  is_indicator <- grepl("nester", strp$layout$name)
  panel_xpos <- panel_cols(g)$l
  nestr_xpos <- strp$layout[is_indicator, c("l", "r")]

  # Test
  expect_equal(sum(is_indicator), 1)
})

test_that("facet_nested can draw nesting lines vertically", {
  # Build gtable
  g <- basic + facet_nested(nester + Species ~., nest_line = TRUE)
  g <- ggplotGrob(g)
  strp <- g$grobs[g$layout$name == "strip-r-1-2"][[1]]

  # Grab metrics
  is_indicator <- grepl("nester", strp$layout$name)
  panel_ypos <- panel_rows(g)$t
  nestr_ypos <- strp$layout[is_indicator, c("t", "b")]

  # Test
  expect_equal(sum(is_indicator), 1)
})

test_that("facet_nested line resection works", {
  # Build gtable
  test <- basic + facet_nested(~ nester + Species,
                               nest_line = TRUE,
                               resect = grid::unit(10, "mm"))
  ctrl <- basic + facet_nested(~ nester + Species,
                               nest_line = TRUE,
                               resect = grid::unit(0, "mm"))
  test <- ggplotGrob(test)
  ctrl <- ggplotGrob(ctrl)
  test <- test$grobs[test$layout$name == "strip-t-1-1"][[1]]
  ctrl <- ctrl$grobs[ctrl$layout$name == "strip-t-1-1"][[1]]

  # Grab metrics
  test <- test$grobs[[grep("nester", test$layout$name)]]
  ctrl <- ctrl$grobs[[grep("nester", ctrl$layout$name)]]

  test_width <- grid::convertWidth(test$x, "mm", valueOnly = TRUE)
  ctrl_width <- grid::convertWidth(ctrl$x, "mm", valueOnly = TRUE)

  # Tests
  expect_false(any(test_width == ctrl_width))
  expect_equal(test$x[1], unit(0, "npc") +  1 * unit(10, "mm"))
  expect_equal(test$x[2], unit(1, "npc") + -1 * unit(10, "mm"))
})




# Setup bleed tests -------------------------------------------------------

df <- data.frame(outer = c(1,2,2),
                 inner = c(3,3,4),
                 x = 0, y = 0)

bleed <- ggplot(df, aes(x, y)) +
  geom_point()


# Bleed tests -------------------------------------------------------------

test_that("facet_nested can bleed horizontally", {
  # Setup gtable layouts
  ctrl <- bleed + facet_nested(~ outer + inner, bleed = FALSE)
  test <- bleed + facet_nested(~ outer + inner, bleed = TRUE)

  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  ctrl <- ctrl$layout[grepl("strip", ctrl$layout$name),]
  test <- test$layout[grepl("strip", test$layout$name),]

  # Grab metrics
  ctrl_nstrips <- nrow(ctrl)
  test_nstrips <- nrow(test)

  expect_false(ctrl_nstrips == test_nstrips)
  expect_gt(ctrl_nstrips, test_nstrips)
  expect_equal(test_nstrips, 4)
  expect_equal(ctrl_nstrips, 5)
})

test_that("facet_nested horizontal bleeding works", {
  # Setup gtable layouts
  ctrl <- bleed + facet_nested(~ outer + inner, bleed = FALSE)
  test <- bleed + facet_nested(~ outer + inner, bleed = TRUE)

  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  ctrl <- ctrl$layout[grepl("strip", ctrl$layout$name),]
  test <- test$layout[grepl("strip", test$layout$name),]

  # Grab metrics
  ctrl_nstrips <- nrow(ctrl)
  test_nstrips <- nrow(test)

  # Top and bottom positions should be the same
  expect_equal(test$t, test$b)
  expect_equal(ctrl$t, ctrl$b)

  # Left and right positions should differ based on bleeding
  expect_equal(sum(test$l == test$r), 2)
  expect_equal(sum(ctrl$l == ctrl$r), 4)

  # Test unequal strips
  expect_equal(which(test$l != test$r), c(2, 3))
  expect_lt(test$l[2], test$r[2])
  expect_lt(test$l[3], test$r[3])

  expect_equal(which(ctrl$l != ctrl$r), 3)
  expect_lt(ctrl$l[3], ctrl$r[3])
})

test_that("facet_nested can bleed vertically", {
  # Setup gtable layouts
  ctrl <- bleed + facet_nested(outer + inner ~ ., bleed = FALSE)
  test <- bleed + facet_nested(outer + inner ~ ., bleed = TRUE)

  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  ctrl <- ctrl$layout[grepl("strip", ctrl$layout$name),]
  test <- test$layout[grepl("strip", test$layout$name),]

  # Grab metrics
  ctrl_nstrips <- nrow(ctrl)
  test_nstrips <- nrow(test)

  expect_false(ctrl_nstrips == test_nstrips)
  expect_gt(ctrl_nstrips, test_nstrips)
  expect_equal(test_nstrips, 4)
  expect_equal(ctrl_nstrips, 5)
})

test_that("facet_nested vertical bleeding works", {
  # Setup gtable layouts
  ctrl <- bleed + facet_nested(outer + inner ~ ., bleed = FALSE)
  test <- bleed + facet_nested(outer + inner ~ ., bleed = TRUE)

  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  ctrl <- ctrl$layout[grepl("strip", ctrl$layout$name),]
  test <- test$layout[grepl("strip", test$layout$name),]

  # Left and right positions should be the same
  expect_equal(test$l, test$r)
  expect_equal(ctrl$l, ctrl$r)

  # Left and right positions should differ based on bleeding
  expect_equal(sum(test$t == test$b), 2)
  expect_equal(sum(ctrl$t == ctrl$b), 4)

  # Test unequal strips
  expect_equal(which(test$t != test$b), c(1, 3))
  expect_lt(test$t[1], test$b[1])
  expect_lt(test$t[3], test$b[3])

  expect_equal(which(ctrl$t != ctrl$b), 4)
  expect_lt(ctrl$t[4], ctrl$b[4])
})


# Miscellaneous tests -----------------------------------------------------

test_that("facet_nested handles combined datasets with missing inner variables", {
  df1 <- data.frame(outer = 1,
                    inner = LETTERS[1:2],
                    x = 0, y = 0)
  df2 <- data.frame(outer = 2,
                    x = 0, y = 0)

  g <- ggplot() +
    geom_point(data = df1, aes(x, y)) +
    geom_point(data = df2, aes(x, y))

  test <- ggplotGrob(g + facet_nested(~ outer + inner))
  ctrl <- ggplotGrob(g + facet_grid(~ outer + inner))
  strp_test <- test$grobs[grepl("strip", test$layout$name)]
  strp_ctrl <- ctrl$grobs[grepl("strip", ctrl$layout$name)]
  test_is_strip <- grepl("strip", strp_test$layout$name)
  ctrl_is_strip <- grepl("strip", strp_ctrl$layout$name)

  test_striplabels <- sapply(strp_test, function(strip){
    titles <- sapply(strip$grobs, function(grob){
      title <- grob$children[[2]]$children[[1]]$label
    })
  })

  ctrl_striplabels <- sapply(strp_ctrl, function(strip){
    titles <- sapply(strip$grobs, function(grob){
      title <- grob$children[[2]]$children[[1]]$label
    })
  })
  ctrl_striplabels <- as.vector(ctrl_striplabels)

  expect_false(length(test_striplabels) == length(ctrl_striplabels))
  expect_equal(length(ctrl_striplabels) - length(test_striplabels), 3)
  expect_equal(ctrl_striplabels, c("1", "A", "1", "B", "2", "A", "2", 'B'))
  expect_equal(test_striplabels, c("1", "A", "B", "2", ""))
})

context("test-ggsubset")

test_that("ggsubset returns a function", {
  f <- ggsubset()
  expect_is(f, "function")
})

test_that("ggsubset returns a function that can be used on a data.frame", {
  f <- ggsubset(Species == "setosa")
  results <- f(iris)
  expect_is(results, "data.frame")
})

test_that("ggsubset function correctly subsets", {
  f <- ggsubset(Species == "setosa")
  results <- f(iris)
  species <- as.character(unique(results$Species))
  expect_equal(species, "setosa")
})

test_that("ggsubset handles compound logic", {
  f <- ggsubset(Species == "setosa" | Species == "versicolor")
  results <- f(iris)
  species <- as.character(unique(results$Species))
  expect_equal(species, c("setosa", "versicolor"))
})

test_that("ggsubset correctly omits columns", {
  f <- ggsubset(omit = Species)
  results <- f(iris)
  expect_false("Species" %in% colnames(results))
})

test_that("ggsubset correctly omits multiple columns", {
  f <- ggsubset(omit = c(Species, Sepal.Length))
  results <- f(iris)
  expect_false(any(c("Species", "Sepal.Length") %in% colnames(results)))
})

test_that("ggsubset can subset and omit simultaneously", {
  f <- ggsubset(Species == "setosa", omit = Sepal.Length)
  results <- f(iris)
  species <- as.character(unique(results$Species))
  expect_equal(species, "setosa")
  expect_false("Sepal.Length" %in% colnames(results))
})

test_that("ggsubset can subset and omit on same column", {
  f <- ggsubset(Species == "setosa", omit = Species)
  results <- f(iris)
  expect_false("Species" %in% colnames(results))
  expect_equal(nrow(results), 50)
})

test_that("ggsubset can be used in a ggplot", {
  plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width))

  ctrl <- plot + geom_point(aes(colour = Species))

  test <- plot +
    geom_point(data = ggsubset(Species == "setosa"), colour = "red") +
    geom_point(data = ggsubset(Species == "versicolor"), colour = "blue") +
    geom_point(data = ggsubset(Species == "virginica"), colour = "green")

  expect_equal(length(ctrl$layers), 1)
  expect_equal(length(test$layers), 3)

  ctrl <- layer_data(ctrl)
  test <- rbind(layer_data(test, 1),
                layer_data(test, 2),
                layer_data(test, 3))

  expect_equal(nrow(ctrl), nrow(test))

  ctrl_tab <- table(ctrl$colour, iris$Species)
  test_tab <- table(test$colour, iris$Species)

  expect_equivalent(ctrl_tab, test_tab)
  expect_equivalent(colSums(ctrl_tab), c(50, 50, 50))
  expect_equivalent(rowSums(ctrl_tab), c(50, 50, 50))
  expect_equivalent(colSums(test_tab), c(50, 50, 50))
  expect_equivalent(rowSums(test_tab), c(50, 50, 50))
})

test_that("ggsubset returns a function", {
  suppressWarnings({
    f <- ggsubset()
  }, classes = "lifecycle_warning_deprecated")
  expect_type(f, "closure")
})

test_that("ggsubset returns a function that can be used on a data.frame", {
  suppressWarnings({
    f <- ggsubset(Species == "setosa")
  }, classes = "lifecycle_warning_deprecated")
  results <- f(iris)
  expect_s3_class(results, "data.frame")
})

test_that("ggsubset function correctly subsets", {
  suppressWarnings({
    f <- ggsubset(Species == "setosa")
  }, classes = "lifecycle_warning_deprecated")
  results <- f(iris)
  species <- as.character(unique(results$Species))
  expect_equal(species, "setosa")
})

test_that("ggsubset handles compound logic", {
  suppressWarnings({
    f <- ggsubset(Species == "setosa" | Species == "versicolor")
  }, classes = "lifecycle_warning_deprecated")
  results <- f(iris)
  species <- as.character(unique(results$Species))
  expect_equal(species, c("setosa", "versicolor"))
})

test_that("ggsubset correctly omits columns", {
  suppressWarnings({
    f <- ggsubset(omit = Species)
  }, classes = "lifecycle_warning_deprecated")
  results <- f(iris)
  expect_false("Species" %in% colnames(results))
})

test_that("ggsubset correctly omits multiple columns", {
  suppressWarnings({
    f <- ggsubset(omit = c(Species, Sepal.Length))
  }, classes = "lifecycle_warning_deprecated")
  results <- f(iris)
  expect_false(any(c("Species", "Sepal.Length") %in% colnames(results)))
})

test_that("ggsubset can subset and omit simultaneously", {
  suppressWarnings({
    f <- ggsubset(Species == "setosa", omit = Sepal.Length)
  }, classes = "lifecycle_warning_deprecated")
  results <- f(iris)
  species <- as.character(unique(results$Species))
  expect_equal(species, "setosa")
  expect_false("Sepal.Length" %in% colnames(results))
})

test_that("ggsubset can subset and omit on same column", {
  suppressWarnings({
    f <- ggsubset(Species == "setosa", omit = Species)
  }, classes = "lifecycle_warning_deprecated")
  results <- f(iris)
  expect_false("Species" %in% colnames(results))
  expect_equal(nrow(results), 50)
})

test_that("ggsubset can be used in a ggplot", {
  plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width))

  ctrl <- plot + geom_point(aes(colour = Species))

  suppressWarnings({
    test <- plot +
      geom_point(data = ggsubset(Species == "setosa"),     colour = "red") +
      geom_point(data = ggsubset(Species == "versicolor"), colour = "blue") +
      geom_point(data = ggsubset(Species == "virginica"),  colour = "green")
  }, classes = "lifecycle_warning_deprecated")

  expect_equal(length(ctrl$layers), 1)
  expect_equal(length(test$layers), 3)

  ctrl <- layer_data(ctrl)
  test <- rbind(layer_data(test, 1),
                layer_data(test, 2),
                layer_data(test, 3))

  expect_equal(nrow(ctrl), nrow(test))

  ctrl_tab <- table(ctrl$colour, iris$Species)
  test_tab <- table(test$colour, iris$Species)

  expect_equal(ctrl_tab, test_tab, ignore_attr = TRUE)
  expect_equal(colSums(ctrl_tab), c(50, 50, 50), ignore_attr = TRUE)
  expect_equal(rowSums(ctrl_tab), c(50, 50, 50), ignore_attr = TRUE)
  expect_equal(colSums(test_tab), c(50, 50, 50), ignore_attr = TRUE)
  expect_equal(rowSums(test_tab), c(50, 50, 50), ignore_attr = TRUE)
})

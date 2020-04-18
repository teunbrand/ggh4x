test_that("theme elements are loaded by default", {
  tree <- get_element_tree()
  expect_true(sum(grepl("ggh4x.axis", names(tree))) > 5)
})


test_that("theme elements can be removed", {
  reset_theme_settings()
  tree <- get_element_tree()
  expect_equal(sum(grepl("ggh4x.axis", names(tree))), 0)
})

test_that("theme elements can be set again", {
  reset_theme_settings()
  tree <- get_element_tree()
  expect_equal(sum(grepl("ggh4x.axis", names(tree))), 0)
  ggh4x_theme_elements()
  tree <- get_element_tree()
  expect_true(sum(grepl("ggh4x.axis", names(tree))) > 5)
})

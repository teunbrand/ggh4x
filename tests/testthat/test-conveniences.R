test_that("elem_list_rect constructs a list of rect elements", {
  x <- elem_list_rect(colour = list(c("green", "blue"), "red"),
                      nonense_argument = "Hell, no")

  expect_equal(
    x,
    list(
      element_rect(colour = c("green", "blue")),
      element_rect(colour = "red")
    )
  )

})

test_that("elem_list_text constructs a list of text elements", {
  x <- elem_list_text(colour = c("green", "blue"),
                      margin = list(NULL, margin(t = 5)),
                      nonense_argument = "Hell, no")

  expect_equal(
    x,
    list(
      element_text(colour = "green"),
      element_text(colour = "blue", margin = margin(t = 5))
    )
  )

})

test_that("old guides throw deprecation messages", {
  lifecycle::expect_deprecated(guide_axis_manual())
  lifecycle::expect_deprecated(guide_axis_truncated())
  lifecycle::expect_deprecated(guide_axis_color())
  lifecycle::expect_deprecated(guide_axis_colour())
  lifecycle::expect_deprecated(guide_axis_minor())
  lifecycle::expect_deprecated(guide_axis_nested())
  lifecycle::expect_deprecated(guide_axis_scalebar())
})

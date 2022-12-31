test_that("as_orientation works", {
  expect_identical(as_orientation(6 / 4), "landscape")
  expect_identical(as_orientation(4 / 6), "portrait")
  expect_identical(as_orientation(4 / 4), "square")
  expect_identical(as_orientation(data.frame(
    "width" = c(6, 4, 4),
    "height" = c(4, 6, 4)
  )), c("landscape", "portrait", "square"))

  expect_identical(as_orientation(1.09 / 1), "square")
  expect_identical(as_orientation(1.08 / 1, tolerance = 0.07), "landscape")
})

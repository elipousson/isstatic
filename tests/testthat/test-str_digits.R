test_that("str_digits works", {
  expect_identical(
    str_pad_digits(1, pad = NULL),
    1
  )
  expect_identical(
    str_pad_digits(1),
    "1"
  )
  expect_identical(
    str_pad_digits(1, width = 2),
    "01"
  )
  expect_identical(
    str_replace_digits("file_01", 2, width = 2),
    "file_02"
  )
  expect_identical(
    str_increment_digits("file_1"),
    "file_2"
  )
})

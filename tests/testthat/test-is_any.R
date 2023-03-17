test_that("is_any works", {
  expect_true(is_any(list("A", "B", 1), is.character))
  expect_true(is_any_na(list("A", NA)))
  expect_true(is_any_null(list("A", NULL)))
  expect_true(is_none(list("A", "B"), is.numeric))
  expect_true(is_none_null(list("A", "B")))
})

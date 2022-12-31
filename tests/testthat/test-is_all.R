test_that("is_all works", {
  expect_true(is_all(list("A", "B"), is.character))
  expect_false(is_all_null(list("A", NULL)))
  expect_false(is_all_na(list("A", NA)))
})

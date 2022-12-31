test_that("is_in works", {
  expect_true(is_any_in("A", c("A", "B", "C")))
  expect_false(is_none_in("A", c("A", "B", "C")))
  expect_false(is_all_in(c("A", "D"), c("A", "B", "C")))
})

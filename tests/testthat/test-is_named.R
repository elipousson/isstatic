test_that("is_named works", {
  expect_true(is_named(list("A" = 1)))
  expect_true(has_all_names(list("A" = 1, "B" = 2), c("A", "B")))
  expect_false(has_all_names(list("A" = 1), c("A", "B")))
  expect_true(has_any_names(list("A" = 1), c("A", "B")))
  expect_false(has_all_names(list("A" = 1), NA))
  expect_false(has_any_names(NA, NA))
})

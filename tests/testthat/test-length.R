test_that("has_same_len works", {
  expect_true(has_same_len(c(1, 2, 3), c("A", "B", "C")))
})

test_that("has_len_between works", {
  expect_true(has_len_between(rep("A", 5), 1, 10))
  expect_false(has_len_between("A", 5, 10))
})

test_that("has_min_length works", {
  expect_true(has_min_length(c("A", "B"), 2))
  expect_false(has_min_length("A", 2))
})

test_that("has_max_length works", {
  expect_true(has_max_length("A", 1))
  expect_false(has_max_length(c("A", "B"), 1))
})

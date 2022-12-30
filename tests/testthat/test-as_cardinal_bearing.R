test_that("as_cardinal_bearing works", {
  expect_identical(
    as_cardinal_bearing(c(5, 95)), c("N" = 0, "E" = 90)
  )
  expect_identical(
    as_cardinal_bearing(data.frame("bearing" = c(5, 95))),
    data.frame(
        "bearing" = c(5, 95),
        "cardinal_bearing" = c(0, 90)
      )
  )
})

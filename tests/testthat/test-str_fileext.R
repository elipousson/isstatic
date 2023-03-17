test_that("str_filext works", {
  expect_identical(
    str_add_fileext("image"),
    "image"
  )
  expect_identical(
    str_add_fileext("image", "jpeg"),
    "image.jpeg"
  )
  expect_identical(
    str_add_fileext("image.png", "jpeg"),
    "image.jpeg"
  )
  expect_identical(
    str_remove_fileext(c("file.txt", "word.docx")),
    c("file", "word")
  )
  expect_identical(
    str_remove_fileext(c("file.txt", "word.docx"), "docx"),
    c("file.txt", "word")
  )
  expect_identical(
    str_remove_fileext(c("file1.pdf", "file2")),
    c("file1", "file2")
  )
  expect_identical(
    str_extract_fileext(c("file1.pdf", "file2")),
    c("pdf", NA_character_)
  )
  expect_identical(
    str_extract_fileext(c("image1.png", "image2.jpeg"), "jpeg"),
    c(NA_character_, "jpeg")
  )
})

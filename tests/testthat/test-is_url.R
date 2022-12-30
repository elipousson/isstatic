test_that("is_url works", {
  expect_true(is_url("https://www.google.com"))
  expect_true(is_esri_url("https://sampleserver6.arcgisonline.com/arcgis/rest/services/LocalGovernment/Recreation/FeatureServer/0"))
  expect_true(is_gsheet_url("https://docs.google.com/spreadsheets/d/1O0q3l-QFqgFvJMFw8Raihv6zt8uuIrKkR2NHThUnJT8"))
  expect_true(is_gist_url("https://gist.github.com/elipousson/24605cafcb0a9694daa7dbf1ed8a2b6f"))
  expect_true(is_gmap_url("https://www.google.com/maps/d/u/0/viewer?mid=1CGR52gV1yS1zCvBhhvzVftus59Y"))
})

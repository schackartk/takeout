test_that("good error on bad file", {
  expect_error(json_to_tibble("foo.json"),regexp = "'foo.json' does not*")
})

test_that("good error on bad file", {
  expect_error(json_to_tibble("foo.json"), regexp = "'foo.json' does not*")
})

test_that("works with good location history file", {
  expect_equal(ncol(json_to_tibble("sample_location_history.json")), 4)
  expect_equal(nrow(json_to_tibble("sample_location_history.json")), 3)
})

context("test-get_dataset")

test_that("get_dataset main function", {
  expect_equal(get_dataset("yle_2011"), get_YLE_2011_data())
  expect_equal(get_dataset("yle_2015"), get_YLE_2015_data())
})

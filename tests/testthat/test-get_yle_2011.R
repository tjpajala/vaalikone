context("test-get_yle_2011")

test_that("Function returns a tibble with at least one row.", {
  df <- get_YLE_2015_data(filter_precinct=c("01 Helsingin vaalipiiri"))
  expect_gt(nrow(df),0)
  expect_equal(class(df)[1],"tbl_df")
})

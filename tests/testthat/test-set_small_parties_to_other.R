context("test-set_small_parties_to_other")

test_that("subbing parties with 'Other' works", {
  dataset_name <- "yle_2011"
  data <- get_dataset(dataset_name)
  party_col <- .get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
  parties <- set_small_parties_to_other(data, party_col)  
  expect_equal(sum(parties=="Other"), 26)
  expect_equal(length(parties), nrow(data))
})

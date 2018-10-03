context("test-prepare_data")

test_that("multiplication works", {
  dataset_name <- "yle_2011"
  data <- get_dataset(dataset_name)
  q_col=get_data_cols(dataset_name, data)
  party_col <- .get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
  data <- prepare_data(data, q_col, party_col)
  nullrows <- data[rowSums(is.na(data[,q_col]))!=0,]
  
  expect_true(is.factor(data[[party_col]]))
  expect_equal(nrow(nullrows), 0)
})

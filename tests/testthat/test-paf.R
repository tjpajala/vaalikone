context("test-paf")

test_that("multiplication works", {
  dataset_name <- "yle_2011"
  data <- get_dataset(dataset_name)
  q_col=get_data_cols(dataset_name, data)
  party_col <- .get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
  data <- prepare_data(data, q_col, party_col)
  
  qdata <- select(data, one_of(party_col, q_col))
  fa <- PAF(qdata, nfactors = 2, vss=FALSE, cols=q_col)
  
  expect_equivalent(select(fa$scores,c(-PA1,-PA2)), qdata)
  expect_equal(colnames(fa$loadings),c("PA1","PA2"))
  expect_equal(fa$det, 0.0002640543, tolerance=0.00001)
})

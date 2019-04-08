context("test-paf")
dataset_name <- "yle_2011"
data <- get_dataset(dataset_name,filter=c("01 Helsingin vaalipiiri"))
q_col=get_data_cols(dataset_name, data)
party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
data <- prepare_data(data, q_col, party_col,limit=10)
qdata <- dplyr::select(data, dplyr::one_of(party_col, q_col))

test_that("PAF works", {
  fa <- PAF(qdata, nfactors = 2, vss=FALSE, cols=q_col)
  
  expect_equivalent(dplyr::select(fa$scores,c(-PA1,-PA2)), qdata)
  expect_equal(colnames(fa$loadings),c("PA1","PA2"))
  expect_equal(fa$det, 0.0002640543, tolerance=0.00001)
})

test_that("VSS can be printed", {
  fa <- PAF(qdata, nfactors = 2, vss=TRUE, cols=q_col)
  expect_equivalent(dplyr::select(fa$scores,c(-PA1,-PA2)), qdata)
})
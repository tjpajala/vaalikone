context("test-removeqname")

test_that("remove question name", {
  dataset_name <- "yle_2011"
  data <- get_dataset(dataset_name)
  q_col=get_data_cols(dataset_name, data)
  party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
  data <- prepare_data(data, q_col, party_col)
  
  qdata <- dplyr::select(data, dplyr::one_of(party_col, q_col))
  colnames(qdata) <- c(party_col,paste("q",1:length(q_col),sep=""))
  qdata_bck <- qdata
  
  qdata <- removeQname(qdata, "q1")
  expect_equal(colnames(qdata), colnames(dplyr::select(qdata_bck, -q1)))
  expect_error(removeQname(qdata, "wrong_column"))
  
})

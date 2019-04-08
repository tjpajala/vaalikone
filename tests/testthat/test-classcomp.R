context("test-classcomp")

test_that("classifier comparison", {
  dataset_name <- "yle_2011"
  data <- get_dataset(dataset_name,filter=c("01 Helsingin vaalipiiri"))
  q_col=get_data_cols(dataset_name, data)
  party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
  data <- prepare_data(data, q_col, party_col,limit=10)
  
  qdata <- dplyr::select(data, dplyr::one_of(party_col, q_col))
  colnames(qdata) <- c(party_col,paste("q",1:length(q_col),sep=""))
  modelname <- "rf"
  res <- classComp(qdata, 3, model=modelname, party_col = party_col)
  
  expect_equal(nrow(res), 1)
  expect_equal(rownames(res)[1], modelname)
})

context("test-analyze_removed_questions")

test_that("Test question removal analysis.",{
  dataset_name <- "yle_2011"
  data <- get_dataset(dataset_name)
  q_col=get_data_cols(dataset_name, data)
  party_col <- .get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
  data <- prepare_data(data, q_col, party_col)
  
  qdata <- select(data, one_of(party_col, q_col))
  colnames(qdata) <- c(party_col,paste("q",1:length(q_col),sep=""))
  
  imp_num<-1:2
  result <- analyze_removed_questions(data = qdata, imp_num = imp_num, party_col = party_col)
  expect_equal(nrow(result), 2)
  expect_true(all(result$removed==c(1,2)))
})

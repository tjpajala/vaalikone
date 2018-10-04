# elecmachine

The goal of elecmachine is providing scripts that help to pull the Election Machine (vaalikone) data from HS (2015) and YLE (2011, 2015). The package includes scripts for analyzing the answers of candidates, doing principal axis factoring, and estimating the predictive value of different questions for party prediction.

## Installation

You can install the released version of elecmachine from [github](https://github.com/tjpajala/vaalikone) with:

``` r
devtools::install_github("tjpajala/vaalikone")
```

## Example

This is a basic example which shows you how to get the YLE 2015 data set, and perform some
basic analysis on it:

``` r
dataset_name <- "yle_2015" #options: hs_2015, yle_2011
data <- get_dataset(name=dataset_name)
party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
q_cols=get_data_cols(dataset_name = dataset_name, data=data)
data <- prepare_data(data, q_cols, party_col)

#make a dataframe with just party and question data
qdata <- select(data, one_of(party_col, q_cols))
colnames(qdata) <- c(party_col,paste("q",1:length(q_cols),sep=""))

#run principal axis factoring
fa <- PAF(qdata, nfactors=2, vss=TRUE, cols=q_cols)
#plot candidates on the factor plot
FA_ggplot(fa,flip=20,colname_party = party_col)

#analyze information value of questions
res <- analyze_removed_questions(qdata, imp_num=imp_num[1:(length(imp_num)-1)], party_col=party_col)
#plot class error by removed question
error_ggplot(res)
```

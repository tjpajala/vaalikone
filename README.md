# elecmachine

The goal of elecmachine is providing scripts that help to pull the Election Machine (vaalikone) data from HS (2015) and YLE (2011, 2015). The package includes scripts for analyzing the answers of candidates, doing principal axis factoring, and estimating the predictive value of different questions for party prediction.

## Installation

You can install the released version of elecmachine from [github](https://github.com/tjpajala/vaalikone) with:

``` r
devtools::install_github("tjpajala/vaalikone")
library(elecmachine)
```

## Example

This is a basic example which shows you how to get the YLE 2015 data set, and perform some
basic analysis on it:

``` r
dataset_name <- "yle_2015" #options: hs_2015, yle_2011, yle_2019
data <- get_dataset(name=dataset_name,filter_precinct=c("01 Helsingin vaalipiiri"))
party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party","Party"))
q_cols=get_data_cols(dataset_name = dataset_name, data=data)
#combine all parties with less than 10 members
data <- prepare_data(data, q_cols, party_col, limit=10)

#make a dataframe with just party and question data
qdata <- select(data, one_of(party_col, q_cols))

#run principal axis factoring
fa <- PAF(qdata, nfactors=2, vss=TRUE, cols=q_cols)
#plot candidates on the factor plot
FA_ggplot(fa,flip=20,colname_party = party_col, encircle=FALSE)
```
![Factor plot](/figs/fa_plot.png)
``` r
#analyze information value of questions
colnames(qdata) <- c(party_col,paste("q",1:length(q_cols),sep=""))
rf<-randomForest::randomForest(as.formula(paste0(party_col,"~.")),data=qdata,importance=TRUE)
ord<-order(rf$importance[,"MeanDecreaseGini"])
imp<-rf$importance[,"MeanDecreaseGini"]
imp_num<-as.numeric(sub("q","",names(imp)[ord]))
res <- analyze_removed_questions(qdata, imp_num=imp_num[1:(length(imp_num)-1)], party_col=party_col)
#plot class error by removed question
error_ggplot(res)
```
![Class prediction error plot](/figs/class_error_by_question.png)
```r
#plot distribution of answers for a question
plot_single_question(data, 5, q_cols, jitter=TRUE)
```
![Distribution of candidates, jittered](/figs/question_jitter.png)
```r
#table of question variance across parties (and total)
table_question_variance(data, q_cols)
```
```
#A tibble: 14 x 39
   puol…    q1    q2    q3    q4    q5    q6    q7    q8    q9   q10   q11   q12   q13   q14   q15   q16   q17
   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
 1 IP    1.36  2.62  1.98   2.07  1.36  1.72 1.98  1.76  0.995 1.63  0     2.25  1.98  1.61  2.73  0.555  2.69
 2 KD    1.67  1.58  2.15   1.44  1.80  2.32 2     1.39  0.690 1.27  2.02  1.81  2.15  2.15  1.84  0.766  1.94
 3 KESK  1.75  1.93  2.39   1.66  1.33  1.31 2.36  2.01  2.19  1.49  2.19  0.614 2.05  0.990 0.648 1.56   2.03
 4 KOK   0.658 2.74  1.84   1.99  1.40  2.05 2.05  2.62  1.96  1.92  2.16  0.970 1.90  1.59  1.61  1.75   2.37
 5 PIR   2.07  0     1.54   2.4   1.41  1.17 0.924 2.54  1.98  1.92  2.43  2.12  2.10  2.46  1.54  1.27   1.64
 6 PS    1.55  2.53  2.16   2.79  2.73  2.53 0.590 3.15  1.5   2.16  1.81  1.26  2.75  1.76  2.16  1.3    1.91
 7 RKP   1.48  2.62  1.48   1.27  2.37  1.99 2.12  1.84  2.06  1.40  3.26  1.91  0.853 2.25  1.40  1.31   1.75
 8 SDP   2.06  0.833 1.79   2.69  2.36  1.9  2.23  1.43  2.25  2.15  1.89  1.56  1.99  1.09  1.1   2.09   2.26
 9 SKP   1.27  2.81  2.58   1.27  1.27  1.74 0.692 1.27  1.14  0     0     1.53  1.73  2.19  1.58  1.26   2.40
10 STP   1.8   2.7   1.7    3.3   1.7   3.2  1.8   2.3   1.7   2.7   1.8   3.2   2.7   2.7   3.8   0.8    1.2 
11 VAS   1.46  2.15  1.46   1.16  1.16  1.79 0.429 0.814 2.29  0.429 0.429 1.95  0.814 1.63  1.71  1.66   1.86
12 VIHR  2.28  2.44  0.779  2.98  1.52  2.06 2.14  2.26  1.48  2.21  2.47  1.11  1.11  0.727 2.18  1.52   2.55
13 Other 1.24  2.57  2.48   3.67  2     3.95 1.29  3.14  1.95  0.143 1.57  2.14  2     3     2.57  2.29   2   
14 all … 1.86  2.75  2.69   2.87  1.90  2.41 1.94  2.30  1.72  1.97  2.08  2.03  2.02  1.70  1.94  1.60   2.30
# ... with 21 more variables: q18 <dbl>, q19 <dbl>, q20 <dbl>, q21 <dbl>, q22 <dbl>, q23 <dbl>, q24 <dbl>,
#   q25 <dbl>, q26 <dbl>, q27 <dbl>, q28 <dbl>, q29 <dbl>, q30 <dbl>, q31 <dbl>, q32 <dbl>, q33 <dbl>,
#   q34 <dbl>, q35 <dbl>, q36 <dbl>, q37 <dbl>, q38 <dbl>
```
The same table can be turned into a tile plot:
```r
#plot of question variance across parties (and total)
plot_question_variance(data, q_cols)
```
![Distribution of candidates, jittered](/figs/question_variance.png)
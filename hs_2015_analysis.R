#pull functions from other files
source("functions.R")

library(dplyr)
library(readr)
col_define <- cols(
  X1=col_skip(),
  id=col_integer(),
  name=col_character(),
  district=col_character(),
  party=col_character(),
  age=col_integer(),
  gender=col_character(),
  www=col_skip(),
  facebook=col_skip(),
  twitter=col_skip(),
  education=col_character(),
  votes=col_integer(),
  lambda=col_double(),
  elected=col_logical(),
  q1=col_integer(),
  q2=col_integer(),
  q3=col_integer(),
  q4=col_integer(),
  q5=col_integer(),
  q6=col_integer(),
  q7=col_integer(),
  q8=col_integer(),
  q9=col_integer(),
  q10=col_integer(),
  q11=col_integer(),
  q12=col_integer(),
  q13=col_integer(),
  q14=col_integer(),
  q15=col_integer(),
  q16=col_integer(),
  q17=col_integer(),
  q18=col_integer(),
  q19=col_integer(),
  q20=col_integer(),
  q21=col_integer(),
  q22=col_integer(),
  q23=col_integer(),
  q24=col_integer(),
  q25=col_integer(),
  q26=col_integer(),
  q27=col_integer(),
  q28=col_integer(),
  q29=col_integer(),
  q30=col_integer(),
  incumbency=col_logical(),
  vec_social=col_integer(),
  media=col_integer()
)

data <- read_csv2("./data/candidates_helsinki_2015.csv", col_names = TRUE, col_types = col_define, na=c("","NULL"))
data$district <- as.factor(data$district)
data$party <- set_small_parties_to_other(data, colname_party="party")
data$gender <- as.factor(data$gender)

fa <- PAF(data, nfactors=2, vss=TRUE, cols=names(select(data, q1:q30)))

#baseR version
#FAplot(fa$scores, centers = FALSE, add=FALSE, pch=21, flip=0)

#ggplot version
FA_ggplot(fa,flip=20)
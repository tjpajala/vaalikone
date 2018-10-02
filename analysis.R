library(dplyr)
library(readr)
library(reshape2)
#pull functions from other files
source("functions.R")
set.seed(123)

#dataset sources:
#yle_2015: http://data.yle.fi/dokumentit/Eduskuntavaalit2015/vastaukset_avoimena_datana.csv
#yle_2011: https://yle.fi/aihe/artikkeli/2011/05/16/vuoden-2011-vaalikonetiedot-nyt-avoimena-datana
#hs_2015:  https://www.hs.fi/politiikka/art-2000002801942.html
dataset_name <- "hs_2015" #options: hs_2015, yle_2011
data <- get_dataset(name=dataset_name)
q_cols=get_data_cols(dataset_name = dataset_name, data=data)

party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
data[[party_col]] <- sub_parties_for_shortcodes(data[[party_col]])
data[[party_col]] <- set_small_parties_to_other(data,colname_party = party_col)
id_col <- get_functional_column_name(data, c("ID","id","Id"))

#turn string answers to numeric if necessary
for(col in q_cols){
  data[,col] <- as.numeric(as.factor(data[[col]]))
}

#drop candidates who have not answered some questions
data <- data[rowSums(is.na(data[,q_cols]))==0,]

#retain only party and q_cols for PAF
data_fa <- data %>% select(., one_of(party_col, q_cols))
fa <- PAF(data_fa, nfactors=2, vss=TRUE, cols=q_cols)

#baseR version
FAplot(fa$scores, party_col, centers = FALSE, add=FALSE, pch=21, flip=0)

#ggplot version
FA_ggplot(fa,flip=20,colname_party = party_col)

#save question text to another vector, replace dataframe header with q#
qdata <- select(data, one_of(party_col, q_cols))
q_text <- q_cols
colnames(qdata) <- c(party_col,paste("q",1:length(q_cols),sep=""))


removeQnum(qdata,1)
res <- classComp(qdata,10,1,justRF = T, party_col = party_col)

rf<-randomForest(as.formula(paste0(party_col,"~.")),data=qdata,importance=TRUE)
classComp(qdata,k=10,repeats=1,justRF=TRUE, party_col = party_col)

ord<-order(rf$importance[,"MeanDecreaseGini"])
imp<-rf$importance[,"MeanDecreaseGini"]
imp_num<-as.numeric(sub("q","",names(imp)[ord]))
classifiers<-data.frame("removed"=0,"acc"=0,"Gini"=0)
q_loadings<-list()
d2<-qdata

ptm <- proc.time()
for(x in imp_num[1:29]){
  print(x)
  d2<-removeQname(d2,names(imp)[x])
  #paf<-PAF(d2,nfactors=2,vss=T)
  #q_loadings<-append(q_loadings,list(paf$loadings))
  clComp<-classComp(d2,k=10,repeats=1,model="rf", party_col = party_col)
  classifiers<-rbind(classifiers,c("removed"=x,clComp["Random Forest","Accuracy"],imp[x]))
}

classifiers<-classifiers[-1,]
proc.time()-ptm

#plot accuracy by removed questions
plot(classifiers$acc,type="l",xlab="Number of removed questions",ylab="Accuracy")



res <- analyze_removed_questions(qdata, imp_num=imp_num, party_col=party_col)
error_ggplot(res)



if(!require(optimx)){
  install.packages("optimx")
}
library(optimx)

if(!require(tidyverse)){
  install.packages("tidyverse")
}
library(tidyverse)

if(!require(psych)){
  install.packages("psych")
}
library(psych)
if(!require(ggplot2)){
  install.packages("ggplot2")
}
library(ggplot2)
if(!require(randomForest)){
  install.packages("randomForest")
}
library(randomForest)
if(!require(caret)){
  install.packages("caret")
}
library(caret)

get_YLE_2015_data <- function(){
  data <- read_csv2("./data/ehdokasdata_2015_yle.csv")
  #replace spaces with _ because tidy evaluation doesn't like spaces
  names(data) <- str_replace_all(names(data), " ", "_")
  data$vaalipiiri <- as.factor(data$vaalipiiri)
  #include only Helsinki
  data <- filter(data, vaalipiiri=="01 Helsingin vaalipiiri")
  #drop columns with all NAs
  data <- data[,colSums(!is.na(data))>0]
  
  return(data)
}


get_YLE_2011_data <- function(){
  data <- read_csv("./data/ehdokasdata_eduskunta11_yle.csv", col_types = paste0(rep("c",108),sep="",collapse=""))
  names(data) <- str_replace_all(names(data), " ", "_")
  data$Vaalipiiri <- as.factor(data$Vaalipiiri)
  #include only Helsinki
  data <- filter(data, Vaalipiiri=="01 Helsingin vaalipiiri")
  #drop columns with all NAs
  data <- data[,colSums(!is.na(data))>0]
  
  return(data)
}

get_HS_2015_data <- function(){
  
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
  
  return(data)
}

get_functional_column_name <- function(data, alternative_spellings){
  for (alt in alternative_spellings){
    if(sum(str_detect(names(data), paste0(c("^",alt),collapse="")))==1){
      col <- alt
    }
  }

  return(col)
}

get_dataset <- function(name){
  data <- switch (name,
          hs_2015=get_HS_2015_data(),
          yle_2011=get_YLE_2011_data(),
          yle_2015=get_YLE_2015_data()
          )
  
  return(data)
}

get_data_cols <- function(dataset_name,data){
  data_cols <- switch (dataset_name,
                       hs_2015 = names(select(data, q1:q30)),
                       yle_2015 = str_subset(names(data), "[:digit:]+\\|[:upper:]"),
                       yle_2011 = str_subset(names(data), "[:digit:]+\\.")
  )
  return(data_cols)
}

set_small_parties_to_other <- function(data, colname_party="party"){
  #'Combine small parties to group "Other"
  #'
  #'Takes all parties with less than \code{nlimit} members, and changes their party
  #'to "Other".
  #'
  #' @param data Data set to be used (either tbl or dataframe)
  #' @param colname_party Column name that has the party information (default is "party")
  #' 
  #' @return Returns the party column
  #' 
  #' @examples
  #' df$party <- set_small_parties_to_other(df, "party")
  #' 
  #' @export  
  data[[colname_party]] <- factor(data[[colname_party]])
  var_unquo <- sym(colname_party)
  big_parties<-data[,colname_party] %>% count(!!var_unquo) %>% filter(n>10) %>% pull(!!var_unquo)
  data[,colname_party] <- fct_other(data[[colname_party]],keep=big_parties,other_level = "Other")
  return(data[[colname_party]])
}


PAF<-function(data,nfactors,vss,cols){
  #FUNCTION for doing Principal Axis Factoring for a given data
  #' Principal Axis Factoring for election machine answers
  #' 
  #' Takes the answers in the election machine, and completes
  #' Principal Axis Factoring (PAF) for them. Returns factor
  #' scores, correlations, the determinant, KMO score and 
  #' factor loadings.
  #' 
  #' @param data The data set to be used.
  #' @param nfactors How many factors to use in the analysis.
  #' @param vss Should a VSS analysis be run to analyze the amount of factors?
  #' @param cols List of column names that include the data to be analysed.
  #' 
  #' @return A list that includes the factor original data augmented with scores, correlations, determinant of the 
  #' correlation matrix, KMO score, and factor loadings. See \code{\link[psych]{fa} for more
  #' information.}
  #' 
  #' @example paf <- PAF(data, 3, FALSE, cols=names(select(data,q1:q30)))
  #' @export

  #bivariate correlations
  bcor<-cor(data[,cols])
  det(bcor)
  
  #KMO test & anti-image
  kmo<-KMO(data[,cols])
  
  if(vss==TRUE){
    vss(bcor,10,rotate="varimax",fm="pa",n.obs=nrow(data))
    scree(bcor,factors=F,pc=T,hline=1,main=paste("Scree with",length(cols),"questions"))
  }
  #FA with nfactors factors
  fact<-fa(bcor,nfactors=nfactors,fm="pa",n.obs=nrow(data),rotate="varimax",scores=TRUE,SMC=FALSE)
  #print(fact)
  f2<-factor.scores(data[,cols],fact$loadings)
  
  #save F2 scores to data
  fa_ans<-data.frame(cbind(data,f2$scores))
  
  return(list("scores"=fa_ans,"corr"=bcor,"det"=det(bcor),"KMO"=kmo,"loadings"=fact$loadings))
}

rot<-function(x,y,flip){
  #'Rotate the factor plot
  #'
  #'Takes the coordinates of calculated factors and rotates them by \code{flip} degrees. This is useful for
  #'aligning the plot in the usual left-right direction.
  #'
  #'@param x First factor coordinates
  #'@param y Second factor coordinates
  #'@param flip Degrees of rotation
  #'
  #'@return A two-column matrix with the rotated coordinates
  #'
  #'
  len<-sqrt(x^2+y^2)
  x2<-x*cospi(flip/180)-y*sinpi(flip/180)
  y2<-y*cospi(flip/180)+x*sinpi(flip/180)
  return(cbind(x2,y2))
}




FAplot<-function(fa_ans,party_col,centers=FALSE,add=FALSE,pch=21,flip=0,...){
  #'Plot party candidates on a 2D factor plot
  #'
  #'Takes candidate answers and their factor scores, and plots them on a 2D plot (that can be rotated), 
  #'coloring candidates by party.
  #'
  #'@param fa_ans The data set and factor scores together. Use \code{\link{PAF}} to get this.
  #'@param party_col Column name that represents party (as string).
  #'@param centers Boolen to control for drawing party centers.
  #'@param add Do you want to add points to a previous plot, or draw a new one?
  #'@param pch Which plot symbol to use.
  #'@param flip Degrees of rotation for the plot, can be used to align left-right axis in the usual political direction
  #'@param ... Any additional arguments for the plot function
  #'
  #'@example FAplot(PAF(data, 2, FALSE, names(select(data, q1:q30))), centers=FALSE, add=FALSE)
  #'@export
  parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SEN","SKP","STP","VAS","VIHR","VP")
  colp=c("blue","red1","purple","darkgreen","darkblue","grey","red1","blue","brown","orange","yellow3","red2","red1","pink2","red1","darkred","green","red1")
  
  #plot FA with two factors
  if(centers==TRUE){
    
    fa_ans_flipped<-fa_ans
    bck<-as.character("white")
    bck<-colp[match(fa_ans_flipped[,party_col],parties,nomatch=0)]
    parties_legend<-unique(fa_ans_flipped[,party_col])
    col_legend<-colp[match(parties_legend,parties,nomatch=0)]
    
    len<-fa_ans_flipped[,"PA1"]^2+fa_ans_flipped[,"PA2"]^2
    rotated<-rot(fa_ans_flipped[,"PA2"],fa_ans_flipped[,"PA1"],flip = flip)
    fa_ans_flipped[,"PA1"]<-rotated[,2]
    fa_ans_flipped[,"PA2"]<-rotated[,1]
    if(add==TRUE){
      points(fa_ans_flipped[,"PA2"],fa_ans_flipped[,"PA1"],col=bck,pch=pch,bg=bck,xlim=range(-3,3),ylim=range(-3,3),cex=2,...)
      legend("topleft",legend=parties_legend,border=col_legend,fill=col_legend,cex=0.7)
    } else{
      plot(fa_ans_flipped[,"PA2"],fa_ans_flipped[,"PA1"],col=bck,pch=pch,bg=bck,xlim=range(-3,3),...)
      
    }
    
  } else{
    fa_ans_flipped<-fa_ans
    bck<-as.character("white")
    bck<-colp[match(fa_ans_flipped[,party_col],parties,nomatch=0)]
    parties_legend<-unique(fa_ans_flipped[,party_col])
    col_legend<-colp[match(parties_legend,parties,nomatch=0)]
    rotated<-rot(fa_ans_flipped[,"PA2"],fa_ans_flipped[,"PA1"],flip = flip)
    fa_ans_flipped[,"PA1"]<-rotated[,2]
    fa_ans_flipped[,"PA2"]<-rotated[,1]
    plot(fa_ans_flipped[,"PA2"],fa_ans_flipped[,"PA1"],col=bck,pch=pch,bg=bck,xlim=range(-3,3),ylim=range(-3,3),...)
    legend("topleft",legend=parties_legend,border=col_legend,fill=col_legend)  
  }
}


FA_ggplot <- function(fa, flip=20, colname_party="party") {
  #' Plot factor plot with ggplot
  #' 
  #' Plots points on a 2D coordinate scatter plot, based on their factor scores.
  #' 
  #' @param fa The data frame including factor scores.
  #' @param flip Degrees to rotate axes (so that leftists are on the left, etc.) Default value 20 (works for HS data).
  #' 
  #' @example FA_ggplot(PAF(data, 2, FALSE, names(select(data, q1:q30))),flip=20)
  #' @export
  var_unquo <- sym(colname_party)
  plt_data <- rot(fa$scores$PA1, fa$scores$PA2, flip=flip)
  fa$scores$PA1 <- plt_data[,1]
  fa$scores$PA2 <- plt_data[,2]
  parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SEN","SKP","STP","VAS","VIHR","VP")
  colp=c("blue","red1","purple","darkgreen","darkblue","grey","red1","blue","brown","orange","yellow3","red2","red1","pink2","red1","darkred","green","red1")
  names(colp) <- parties
  ggplot(fa$scores,aes(x=PA1, y=PA2, color=!!var_unquo))+geom_point()+
    scale_color_manual(values=colp)+coord_flip()+theme_classic()
}

sub_parties_for_shortcodes <- function(datacol){
  #'Substitute Finnish Party names with short versions.
  #'
  #'Takes all party names and substitutes their short versions.
  #'
  #'@param datacol Vector of parties to be replaced.
  #'
  #'@example data$party <- sub_parties_for_shortcodes(datacol=dataparty)
  #'
  
  #get rid of extra party names inside brackets inside the party columns
  datacol <- str_replace(datacol," [:punct:].+[:punct:]","")
  short_parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SKP","STP","VAS","VIHR","VP", "RKP")
  parties <- c("Itsenäisyyspuolue","Köyhien Asialla","Suomen Kristillisdemokraatit","Suomen Keskusta","Kansallinen Kokoomus","Other","Kommunistinen Työväenpuolue","Muutos 2011","Piraattipuolue","Perussuomalaiset","Suomen ruotsalainen kansanpuolue","Suomen Sosialidemokraattinen Puolue","Suomen Kommunistinen Puolue","Suomen Työväenpuolue STP","Vasemmistoliitto","Vihreä liitto","Vapauspuolue","Ruotsalainen kansanpuolue")
  names(short_parties) <- parties
  parties_c <- str_c("^",parties,collapse = "|")
  party_repl <- function(p){
    return(as.character(short_parties[p]))
    #return(p)
  }
  str_replace_all(datacol, parties_c,party_repl)
}

classComp<-function(data,k,repeats,model,party_col){
  #'Predict party membership with caret
  #'
  #'Perform k-fold CV and hyperparameter optimization with caret to predict party affiliation based on 
  #'election machine questions.
  #'
  #'@param data The data set to be used: should only include party column and predictors!
  #'@param k Selected k for cross-validation.
  #'@param repeats Number of repeats for CV (increases stability).
  #'@param model The caret model to be used in prediction.
  #'@param party_col Column name that includes party affiliation.
  #'
  #'@return Data frame that includes optimized parameters, accuracy and kappa information.
  #'
  #'@example res <- classComp(data, 10, 1, "rf", "party")
  #'@export
  controlCV<-trainControl(method="cv",number=k,repeats=repeats)
  f <- as.formula(paste0(party_col,"~."))
  #run model
  rf<-train(f,data=data,method=model,metric="Accuracy",trControl=controlCV)
  res<-data.frame("par"=unlist(rf$bestTune),rf$results[which(rf$results[,1]==unlist(rf$bestTune[1])),2:5])
  rownames(res)<-c(model)
  
  return(res)
}

#FUNCTION for removing questions from data pool
#DEPRECATED
removeQnum<-function(data,n){
  qtext<-names(data)[n]
  if(!qtext%in%names(data)) stop("Question number outside bounds!")
  var.out.bool<-!names(data) %in% qtext
  temp<-data
  #temp<-subset(data,select=-qtext)
  temp<-data[,var.out.bool]
  return(temp)
}

removeQname <- function(data,qname){
  #'Remove election machine question with name \code{qname}.
  #'
  #'@param data Dataset
  #'@param qname The name of question to be removed
  #'
  #'@return Dataset without the removed question.
  #'@export
  var_unquo <- qname
  return(select(data,-var_unquo))
}


analyze_removed_questions <- function(data, imp_num, party_col){
  #'Analyze effect of removal of questions on accuracy with Random Forest.
  #'
  #'Removes questions based on their importance value (starting from least important),
  #'runs Random Forest, and analyzes accuracy.
  #'
  #'@param data The dataset with questions and party affiliation.
  #'@param imp_num Question numbers from least important to most important as vector.
  #'@param party_col Column name of party affiliation.
  #'
  #'@return Data frame with class error by removed question.
  #'
  #'@export
  #partywise error rate with removal
  d2<-data
  f <- as.formula(paste0(party_col,"~q1"))
  nam<-colnames(randomForest(f,data=data)$confusion)
  nam<-nam[1:(length(nam)-1)]
  res<-as.data.frame(matrix(0,1,length(nam)+1))
  colnames(res)<-c("removed",nam)
  ptm <- proc.time()
  for(x in imp_num[1:29]){
    print(x)
    d2<-removeQname(d2,paste0("q",x,collapse = ""))
    f <- as.formula(paste0(party_col,"~."))
    rf<-randomForest(f,data=d2,importance=TRUE,trControl=trainControl(method="cv",number=10,repeats=1))
    rconf<-rf$confusion[,"class.error"]
    pres<-as.data.frame(matrix(0,1,length(nam)))
    colnames(pres)<-nam
    pres<-sapply(1:length(nam),function(x) rconf[nam[x]])
    res<-rbind(res,c("removed"=x,pres))
    
  }
  
  res<-res[-1,]
  
  print(proc.time()-ptm)
  return(res)
}

error_ggplot <- function(res){
  #'Plot classwise error by removed question with ggplot.
  #'
  #'@param res Data frame returned by \code{\link{analyze_removed_questions}}
  #'@export
  res$n <- seq.int(nrow(res))
  res_plot <- melt(res,id.vars="n",variable.name = "party",value.name = "error")
  res_plot <- res_plot[res_plot$party!="removed",]
  parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SEN","SKP","STP","VAS","VIHR","VP")
  colp=c("blue","red1","purple","darkgreen","darkblue","grey","red1","blue","brown","orange","yellow3","red2","red1","pink2","red1","darkred","green","red1")
  names(colp) <- parties
  ggplot(res_plot,aes(x=n, y=error, color=party))+geom_line()+theme_minimal()+
    stat_summary(fun.y=mean, geom="line", linetype="dashed", colour="black")+
    scale_color_manual(values=colp)+
    geom_text(data=subset(res_plot,n==max(res_plot$n)),aes(x=n,y=error,label=party,color=party),nudge_x = 1, nudge_y = 0, show.legend = FALSE)
}
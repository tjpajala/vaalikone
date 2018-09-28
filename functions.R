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
  data[,colname_party] <- fct_other(data$party,keep=big_parties,other_level = "Other")
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




FAplot<-function(fa_ans,centers=FALSE,add=FALSE,pch=21,flip=0,...){
  #'Plot party candidates on a 2D factor plot
  #'
  #'Takes candidate answers and their factor scores, and plots them on a 2D plot (that can be rotated), 
  #'coloring candidates by party.
  #'
  #'@param fa_ans The data set and factor scores together. Use \code{\link{PAF}} to get this.
  #'@param centers Boolen to control for drawing party centers.
  #'@param add Do you want to add points to a previous plot, or draw a new one?
  #'@param pch Which plot symbol to use.
  #'@param flip Degrees of rotation for the plot, can be used to align left-right axis in the usual political direction
  #'@param ... Any additional arguments for the plot function
  #'
  #'@example FAplot(PAF(data, 2, FALSE, names(select(data, q1:q30))), centers=FALSE, add=FALSE)
  #'@export
  parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SEN","SKP","STP","VAS","VIHR","VP")
  colp=c("blue","red1","purple","darkgreen","darkblue","grey","red1","blue","brown","orange","yellow","red2","red1","pink2","red1","darkred","green","red1")
  
  #plot FA with two factors
  if(centers==TRUE){
    
    fa_ans_flipped<-fa_ans
    bck<-as.character("white")
    bck<-colp[match(fa_ans_flipped$party,parties,nomatch=0)]
    parties_legend<-unique(fa_ans_flipped$party)
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
    bck<-colp[match(fa_ans_flipped$party,parties,nomatch=0)]
    parties_legend<-unique(fa_ans_flipped$party)
    col_legend<-colp[match(parties_legend,parties,nomatch=0)]
    rotated<-rot(fa_ans_flipped[,"PA2"],fa_ans_flipped[,"PA1"],flip = flip)
    fa_ans_flipped[,"PA1"]<-rotated[,2]
    fa_ans_flipped[,"PA2"]<-rotated[,1]
    plot(fa_ans_flipped[,"PA2"],fa_ans_flipped[,"PA1"],col=bck,pch=pch,bg=bck,xlim=range(-3,3),ylim=range(-3,3),...)
    legend("topleft",legend=parties_legend,border=col_legend,fill=col_legend)  
  }
}


FA_ggplot <- function(fa, flip=20) {
  #' Plot factor plot with ggplot
  #' 
  #' Plots points on a 2D coordinate scatter plot, based on their factor scores.
  #' 
  #' @param fa The data frame including factor scores.
  #' @param flip Degrees to rotate axes (so that leftists are on the left, etc.) Default value 20 (works for HS data).
  #' 
  #' @example FA_ggplot(PAF(data, 2, FALSE, names(select(data, q1:q30))),flip=20)
  #' @export
  plt_data <- rot(fa$scores$PA1, fa$scores$PA2, flip=flip)
  fa$scores$PA1 <- plt_data[,1]
  fa$scores$PA2 <- plt_data[,2]
  parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SEN","SKP","STP","VAS","VIHR","VP")
  colp=c("blue","red1","purple","darkgreen","darkblue","grey","red1","blue","brown","orange","yellow","red2","red1","pink2","red1","darkred","green","red1")
  names(colp) <- parties
  ggplot(fa$scores,aes(x=PA1, y=PA2, color=party))+geom_point()+
    scale_color_manual(values=colp)+coord_flip()+theme_classic()
}



classComp<-function(data,k,repeats,justRF){

  controlCV<-trainControl(method="cv",number=k,repeats=repeats)

  #Random Forest
  rf<-train(party~.,data=data,method="rf",metric="Accuracy",trControl=controlCV)
  res<-data.frame("par"=unlist(rf$bestTune),rf$results[which(rf$results[,1]==unlist(rf$bestTune[1])),2:5])
  rownames(res)<-c("Random Forest")
  
  return(res)
}

#FUNCTION for removing questions from data pool
removeQnum<-function(data,n){
  qtext<-paste("q",n,sep="")
  if(!qtext%in%names(data)) stop("Question number outside bounds!")
  var.out.bool<-!names(data) %in% qtext
  temp<-data
  #temp<-subset(data,select=-qtext)
  temp<-data[,var.out.bool]
  return(temp)
}


analyze_removed_questions <- function(data, imp_num){
  #partywise error rate with removal
  d2<-data
  nam<-colnames(randomForest(party~q1,data=data)$confusion)
  nam<-nam[1:(length(nam)-1)]
  res<-as.data.frame(matrix(0,1,length(nam)+1))
  colnames(res)<-c("removed",nam)
  ptm <- proc.time()
  for(x in imp_num[1:29]){
    print(x)
    d2<-removeQnum(d2,x)
    rf<-randomForest(party~.,data=d2,importance=TRUE,trControl=trainControl(method="cv",number=10,repeats=1))
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
  res$n <- seq.int(nrow(res))
  res_plot <- melt(res,id.vars="n",variable.name = "party",value.name = "error")
  res_plot <- res_plot[res_plot$party!="removed",]
  parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SEN","SKP","STP","VAS","VIHR","VP")
  colp=c("blue","red1","purple","darkgreen","darkblue","grey","red1","blue","brown","orange","yellow","red2","red1","pink2","red1","darkred","green","red1")
  names(colp) <- parties
  ggplot(res_plot,aes(x=n, y=error, color=party))+geom_line()+theme_minimal()+
    stat_summary(fun.y=mean, geom="line", linetype="dashed", colour="black")+
    scale_color_manual(values=colp)
}
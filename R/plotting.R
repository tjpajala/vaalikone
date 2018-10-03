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
rot<-function(x,y,flip){
  len<-sqrt(x^2+y^2)
  x2<-x*cospi(flip/180)-y*sinpi(flip/180)
  y2<-y*cospi(flip/180)+x*sinpi(flip/180)
  return(cbind(x2,y2))
}



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
#'@usage FAplot(PAF(data, 2, FALSE, names(select(data, q1:q30))), centers=FALSE, add=FALSE)
#'@export
FAplot<-function(fa_ans,party_col,centers=FALSE,add=FALSE,pch=21,flip=0,...){
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

#' Plot factor plot with ggplot
#' 
#' Plots points on a 2D coordinate scatter plot, based on their factor scores.
#' 
#' @param fa The data frame including factor scores.
#' @param flip Degrees to rotate axes (so that leftists are on the left, etc.) Default value 20 (works for HS data).
#' 
#' @usage FA_ggplot(PAF(data, 2, FALSE, names(select(data, q1:q30))),flip=20)
#' @export
FA_ggplot <- function(fa, flip=20, colname_party="party") {
  var_unquo <- rlang::sym(colname_party)
  plt_data <- rot(fa$scores$PA1, fa$scores$PA2, flip=flip)
  fa$scores$PA1 <- plt_data[,1]
  fa$scores$PA2 <- plt_data[,2]
  parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SEN","SKP","STP","VAS","VIHR","VP")
  colp=c("blue","red1","purple","darkgreen","darkblue","grey","red1","blue","brown","orange","yellow3","red2","red1","pink2","red1","darkred","green","red1")
  names(colp) <- parties
  ggplot2::ggplot(fa$scores,aes(x=PA1, y=PA2, color=!!var_unquo))+ggplot2::geom_point()+
    ggplot2::scale_color_manual(values=colp)+ggplot2::coord_flip()+ggplot2::theme_classic()
}

#'Plot classwise error by removed question with ggplot.
#'
#'@param res Data frame returned by \code{\link{analyze_removed_questions}}
#'@export
error_ggplot <- function(res){
  res$n <- seq.int(nrow(res))
  res_plot <- reshape2::melt(res,id.vars="n",variable.name = "party",value.name = "error")
  res_plot <- res_plot[res_plot$party!="removed",]
  parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SEN","SKP","STP","VAS","VIHR","VP")
  colp=c("blue","red1","purple","darkgreen","darkblue","grey","red1","blue","brown","orange","yellow3","red2","red1","pink2","red1","darkred","green","red1")
  names(colp) <- parties
  ggplot2::ggplot(res_plot,ggplot2::aes(x=n, y=error, color=party))+ggplot2::geom_line()+ggplot2::theme_minimal()+
    ggplot2::stat_summary(fun.y=mean, geom="line", linetype="dashed", colour="black")+
    ggplot2::scale_color_manual(values=colp)+
    ggplot2::geom_text(data=dplyr::subset(res_plot,n==max(res_plot$n)),ggplot2::aes(x=n,y=error,label=party,color=party),nudge_x = 1, nudge_y = 0, show.legend = FALSE)
}
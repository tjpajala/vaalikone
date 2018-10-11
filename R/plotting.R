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
#' @param colname_party The column name that denotes party
#' @param encircle Should candidates of each party be encircled with a polygon line? (Default: FALSE, because messy with >10 parties.)
#' @usage FA_ggplot(PAF(data, 2, FALSE, names(select(data, q1:q30))),flip=20)
#' @export
FA_ggplot <- function(fa, flip=20, colname_party=get_functional_column_name(fa, c("Party","party","puolue","Puolue")), encircle=FALSE) {
  var_unquo <- rlang::sym(colname_party)
  plt_data <- rot(fa$scores$PA1, fa$scores$PA2, flip=flip)
  fa$scores$PA1 <- plt_data[,1]
  fa$scores$PA2 <- plt_data[,2]
  parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SEN","SKP","STP","VAS","VIHR","VP")
  colp=c("blue","red1","purple","darkgreen","darkblue","grey","red1","blue","brown","orange","yellow3","red2","red1","pink2","red1","darkred","green","red1")
  names(colp) <- parties
  gg <- ggplot2::ggplot(fa$scores,ggplot2::aes(x=PA1, y=PA2, color=!!var_unquo))+ggplot2::geom_point()+
    ggplot2::scale_color_manual(values=colp)+ggplot2::coord_flip()+ggplot2::theme_classic()
  if(!encircle){
    print(gg)
  } else {
    print(gg + ggalt::geom_encircle(data=fa$scores, s_shape=0.5, expand=0, aes(x=PA1,y=PA2,color=!!var_unquo, group=!!var_unquo), fill=NA))
  }
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
    ggplot2::geom_text(data=subset(res_plot,n==max(res_plot$n)),ggplot2::aes(x=n,y=error,label=party,color=party),nudge_x = 1, nudge_y = 0, show.legend = FALSE)
}

#'Violin plot for distributions of candidates across questions
#'
#'Violin plots that show how candidates are distributed across questions (optionally across parties).
#'
#'@param data Data set to be used
#'@param q_cols The columns defining questions
#'
#'@usage
#'@export
#@param partywise Logical. Do you want the plots per party? (Optional.) 
plot_for_all_questions <- function(data, q_cols){
  parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SEN","SKP","STP","VAS","VIHR","VP")
  colp=c("blue","red1","purple","darkgreen","darkblue","grey","red1","blue","brown","orange","yellow3","red2","red1","pink2","red1","darkred","green","red1")
  names(colp) <- parties
  party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
  party_col_plot <- sym(party_col)
  df <- dplyr::select(data, dplyr::one_of(party_col,q_cols))
  ggplot2::ggplot(df, ggplot2::aes(x=question, y=value))+
    geom_raster(stat="sum",aes(fill=..prop..))+scale_fill_distiller(type="seq", palette="BuGn",direction = 1)+
    coord_fixed(ratio=2, ylim = c(0.6,5.4))+
    theme_minimal()
}

#'Plot for a single question and all parties
#'
#'Plot showing distribution of candidates of different parties for a single question
#'
#'@param data Dataset.
#'@param q_num Question to be analyzed.
#'@param q_cols The columns defining questions
#'@export
plot_single_question <- function(data, q_num, q_cols, jitter=TRUE){
  parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SEN","SKP","STP","VAS","VIHR","VP")
  colp=c("blue","red1","purple","darkgreen","darkblue","grey","red1","blue","brown","orange","yellow3","red2","red1","pink2","red1","darkred","green","red1")
  names(colp) <- parties
  party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
  q_col <- paste0("q",q_num)
  df <- dplyr::select(data, dplyr::one_of(party_col,q_cols))
  colnames(df) <- c(party_col,paste("q",1:length(q_cols),sep=""))
  df <- dplyr::select(df, one_of(party_col, q_col))
  df <- reshape2::melt(df, id.vars=party_col, measure.vars=q_col, variable.name="question")
  party_col_plot <- sym(party_col)

  p <- ggplot2::ggplot(df, aes(x=!!party_col_plot, y=value, fill=!!party_col_plot))+
  stat_sum(geom="tile", aes(alpha=..prop..))+coord_fixed()+guides(alpha="legend", size="none")+
  ggplot2::scale_alpha_continuous(breaks=seq(0.2,1,0.2),labels=seq(0.2,1,0.2))+
  ggplot2::theme_minimal()+ggplot2::scale_color_manual(values=colp, aesthetics = c("colour","fill"))+
  ggtitle(label = paste0(q_col,": ",q_cols[q_num]))+theme(legend.position = "bottom") 
  
  ifelse(jitter, print(p+geom_jitter(height = 0.2, width = 0.2,color="black")), print(p))

}

#'Produce table about question variance
#'
#'Produces a table showcasing within-party and between-party variance for all the questions.
#'
#'@param data Dataset.
#'@param q_cols Question columns
#'@param cols_to_analyze (Optional.) Only a subset of q_cols, if that's all you want to look at. Default is all columns.
#'@param functions_to_use (Optional.) What functions to analyze across parties? Default is "var".
#'@return A table in tbl format.
#'@usage table_question_variance(data, q_cols)
#'@export
table_question_variance <- function(data, q_cols,cols_to_analyze=q_cols, functions_to_use=c("var")){
  party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
  party_col_sym <- sym(party_col)
  df <- dplyr::select(data, one_of(party_col, q_cols))
  colnames(df) <- c(party_col,paste("q",1:length(q_cols),sep=""))
  qtext <- q_cols
  names(qtext) <- paste("q",1:length(q_cols),sep="")
  q_idx <- paste("q",1:length(q_cols),sep="")[(q_cols %in% cols_to_analyze)]
  ret <- group_by(df, !!party_col_sym) %>% summarise_at(.vars=q_idx,.funs=(functions_to_use))
  total <- df %>% summarise_at(.vars=q_idx,.funs=(functions_to_use))
  ret <- dplyr::bind_rows(ret, total)
  ret[[party_col]] <- as.character(ret[[party_col]])
  ret[nrow(ret),party_col] <- "all parties"
  return(ret)
}

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

#'Get color scheme for parties
#'
#'Returns the defined color scheme for parties.
#'
#'@export
get_colors <- function(){
  parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SEN","SKP","STP","VAS","VIHR","VP","FP","STL","KP","ST","LIB","EOP","LN","SIT","SKE","SSP","PSY")
  colp=c("blue","red1","purple","darkgreen","darkblue","grey","red1","blue","brown","orange","yellow3","red2","red1","pink2","red1","darkred","green","red1","hotpink","olivedrab3","slateblue4","royalblue","turquoise4","gold2","magenta4","wheat4","skyblue1","grey3","grey5")
  names(colp)<-parties
  return(colp)
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
  colors <- get_colors()
  parties<-names(colors)
  colp=colors
  
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
  colors <- get_colors()
  parties<-names(colors)
  colp=colors
  gg <- ggplot2::ggplot(fa$scores,ggplot2::aes(x=PA1, y=PA2, color=!!var_unquo))+ggplot2::geom_point()+
    ggplot2::scale_color_manual(values=colp)+ggplot2::coord_flip()+ggplot2::theme_classic()
  if(!encircle){
    print(gg)
  } else {
    print(gg + ggalt::geom_encircle(data=fa$scores, s_shape=0.5, expand=0, ggplot2::aes(x=PA1,y=PA2,color=!!var_unquo, group=!!var_unquo), fill=NA))
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
  colors <- get_colors()
  parties<-names(colors)
  colp=colors
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
#'@usage plot_for_all_questions(data, q_cols)
#'@export
#@param partywise Logical. Do you want the plots per party? (Optional.) 
plot_for_all_questions <- function(data, q_cols){
  colors <- get_colors()
  parties<-names(colors)
  colp=colors
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
#'Plots the distribution of candidates of different parties for a single question, as a combination
#'of tile and jitter plots.
#'@param data Dataset.
#'@param q_num Question to be analyzed.
#'@param q_cols The columns defining questions
#'@usage plot_single_question(data, 5, q_cols, jitter=TRUE)
#'@export
plot_single_question <- function(data, q_num, q_cols, jitter=TRUE){
  colors <- get_colors()
  parties<-names(colors)
  colp=colors
  party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
  q_col <- paste0("q",q_num)
  df <- dplyr::select(data, dplyr::one_of(party_col,q_cols))
  colnames(df) <- c(party_col,paste("q",1:length(q_cols),sep=""))
  df <- dplyr::select(df, dplyr::one_of(party_col, q_col))
  df <- reshape2::melt(df, id.vars=party_col, measure.vars=q_col, variable.name="question")
  party_col_plot <- sym(party_col)

  p <- ggplot2::ggplot(df, ggplot2::aes(x=!!party_col_plot, y=value, fill=!!party_col_plot))+
  ggplot2::stat_sum(geom="tile", ggplot2::aes(alpha=..prop..))+ggplot2::coord_fixed()+
    ggplot2::guides(alpha="legend", size="none")+
  ggplot2::scale_alpha_continuous(breaks=seq(0.2,1,0.2),labels=seq(0.2,1,0.2))+
  ggplot2::theme_minimal()+ggplot2::scale_color_manual(values=colp, aesthetics = c("colour","fill"))+
  ggtitle(label = paste0(q_col,": ",q_cols[q_num]))+ggplot2::theme(legend.position = "bottom") 
  
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
  party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party","Party"))
  party_col_sym <- sym(party_col)
  df <- dplyr::select(data, dplyr::one_of(party_col, q_cols))
  colnames(df) <- c(party_col,paste("q",1:length(q_cols),sep=""))
  qtext <- q_cols
  names(qtext) <- paste("q",1:length(q_cols),sep="")
  q_idx <- paste("q",1:length(q_cols),sep="")[(q_cols %in% cols_to_analyze)]
  ret <- dplyr::group_by(df, !!party_col_sym) %>% dplyr::summarise_at(.vars=q_idx,.funs=(functions_to_use))
  total <- df %>% dplyr::summarise_at(.vars=q_idx,.funs=(functions_to_use))
  ret <- dplyr::bind_rows(ret, total)
  ret[[party_col]] <- as.character(ret[[party_col]])
  ret[nrow(ret),party_col] <- "all parties"
  return(ret)
}

#'Plot question variance as a tile plot
#'
#'Analyzes question variance as in \code{\link{table_question_variance}} and plots the result as a tile plot.
#'@param data Dataset.
#'@param q_cols Question columns
#'@param cols_to_analyze (Optional.) Only a subset of q_cols, if that's all you want to look at. Default is all columns.
#'@param function_to_use (Optional.) What function to analyze across parties? Default is "var". Has to be length 1
#'@param palette (Optional.) Colour palette for the plot. Default: "BuGn".
#'@usage plot_question_variance(data, q_cols, cols_to_analyze=q_cols[1:5], functions_to_use="var")
#'@export
plot_question_variance <- function(data, q_cols,cols_to_analyze=q_cols, function_to_use="var", palette="BuGn"){
  if(length(function_to_use)>1){
    stop("invalid argument: functions_to_use has to be length 1.")
  }
  res <- table_question_variance(data, q_cols,cols_to_analyze=q_cols, functions_to_use=c(function_to_use))
  res <- reshape2::melt(res,id.vars = party_col)
  party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party"))
  party_col_sym <- sym(party_col)
  ggplot2::ggplot(res,aes(x=variable,y=!!party_col_sym))+ggplot2::geom_tile(aes(fill=value))+
    ggplot2::scale_fill_distiller(type="seq", palette="BuGn",direction = -1)
}


#'Get text for the questions with most or least variance
#'
#'Provides the question text for the \code{nmost} questions with either most(\code{direction=1}) or
#'least (\code{direction=-1}) variance.
#'
#'@param data Dataset
#'@param q_cols Question columns
#'@param nmost (Optional.) How many questions to return? Default: 5
#'@param direction (Optional.) Either most(\code{direction=1}) or least (\code{direction=-1}) variance. Default: -1
#'@param function_to_use (Optional.) What function to use for analysis of variance? Default: "var"
#'@usage get_text_for_variance_nmost(data, q_cols, 5, -1, "var")
#'@export
get_text_for_variance_nmost <- function(data, q_cols, nmost=5, direction=-1, function_to_use="var"){
  if(!(direction %in% c(1,-1))){
    stop("invalid argument: direction has to be either 1 or -1")
  }
  if(length(function_to_use)>1){
    stop("invalid argument: functions_to_use has to be length 1.")
  }
  res <- table_question_variance(data, q_cols,cols_to_analyze=q_cols, functions_to_use=c(function_to_use))
  res <- reshape2::melt(res,id.vars = party_col)
  selected_q <- res %>% 
    dplyr::filter(!!party_col_sym=="all parties") %>% 
    dplyr::top_n(direction*nmost,value) %>% 
    dplyr::select(variable) %>% 
    dplyr::pull()
  qtext <- q_cols
  names(qtext) <- paste("q",1:length(q_cols),sep="")
  return(qtext[selected_q])
}
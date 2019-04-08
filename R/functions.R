#' Get the YLE 2015 candidate data
#' 
#' Fetches and preprocesses the YLE 2015 candidate data.
#'@param filter_precinct Vector of precincts to include. Defaults to "01 Helsingin vaalipiiri".
#' @return Tbl dataframe.
#' @export
get_YLE_2015_data <- function(filter_precinct=NULL){
  data <- readr::read_csv2("http://data.yle.fi/dokumentit/Eduskuntavaalit2015/vastaukset_avoimena_datana.csv")
  #replace spaces with _ because tidy evaluation does not like spaces
  names(data) <- stringr::str_replace_all(names(data), " ", "_")
  data$vaalipiiri <- as.factor(data$vaalipiiri)
  #include only Helsinki?
  if(!is.null(filter_precinct)){
    data <- dplyr::filter(data, vaalipiiri%in%filter_precinct)
  }
  #drop columns with all NAs
  data <- data[,colSums(!is.na(data))>0]
  return(data)
}


#'Get the YLE 2011 candidate data
#'
#'Fetches and preprocesses the YLE 2011 candidate data.
#'#'@param filter_precinct Vector of precincts to include. Defaults to NULL.
#'
#'@return Tbl dataframe.
#'@export
get_YLE_2011_data <- function(filter_precinct=NULL){
  data <- readr::read_csv("https://docs.google.com/spreadsheets/d/1yOLYmnWXtIutqpojnvktnDpBdAxtNzcsc5MLlbAxNfg/gviz/tq?tqx=out:csv",
                          col_types = paste0(rep("c",108),sep="",collapse=""))
  names(data) <- stringr::str_replace_all(names(data), " ", "_")
  data$Vaalipiiri <- as.factor(data$Vaalipiiri)
  #include only Helsinki?
  if(!is.null(filter_precinct)){
    data <- dplyr::filter(data, Vaalipiiri%in%filter_precinct)
  }  #drop columns with all NAs
  data <- data[,colSums(!is.na(data))>0]
  
  return(data)
}

#'Get the YLE 2019 candidate data
#'
#'Fetches and preprocesses the YLE 2019 candidate data.
#'@param filter_precinct Vector of precincts to include. Defaults to "Helsingin vaalipiiri".
#'@return Tbl dataframe.
#'@export
get_YLE_2019_data <- function(filter_precinct=NULL){
  temp <- tempfile()
  download.file("https://vaalit.beta.yle.fi/avoindata/avoin_data_eduskuntavaalit_2019.zip",temp)
  temp <- unzip(temp)
  data <- readr::read_csv(temp[1], col_types = paste0(rep("c",211),sep="",collapse=""))
  names(data)<-stringr::str_replace_all(names(data),"[:punct:]",".")
  names(data) <- stringr::str_replace_all(names(data), " ", "_")
  data$vaalipiiri <- as.factor(data$vaalipiiri)
  #include only Helsinki
  if(!is.null(filter_precinct)){
    data <- dplyr::filter(data, vaalipiiri%in%filter_precinct)
  }
  #drop columns with all NAs
  data <- data[,colSums(!is.na(data))>0]
  
  return(data)
}

#'Get the HS data
#'
#'Loads the HS data set and does a little preprocessing. The dataset is not supplied with the package,
#'because it is not public.
#'
#'@return Tbl dataframe.
.get_HS_2015_data <- function(){
  
  col_define <- readr::cols(
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
  
  data <- readr::read_csv2("./data/candidates_helsinki_2015.csv", col_names = TRUE, col_types = col_define, na=c("","NULL"))
  data$district <- as.factor(data$district)
  data$party <- set_small_parties_to_other(data, colname_party="party")
  data$gender <- as.factor(data$gender)
  
  return(data)
}

#'Get a column name with one of the alternative spellings
#'
#'Looks at columns to check if there is one with a name specified in \{alternative_spellings}. If such a 
#'column exists, returns the column name. In this package, used to get the party column name (this varies
#'between data sets.)
#'
#'@param data Data set to look into.
#'@param alternative_spellings Possible alternative spellings of the column name to look for.
#'
#'@return The name of the column  that was found.
#'@note Behavior not specified if many columns of the alternative_spellings exist.
#'@export
get_functional_column_name <- function(data, alternative_spellings){
  for (alt in alternative_spellings){
    if(sum(stringr::str_detect(names(data), paste0(c("^",alt),collapse="")))==1){
      col <- alt
    }
  }

  return(col)
}


#'Get the required data set
#'
#'Convenience function for loading the required dataset.
#'
#'@param name Dataset name (options: "yle_2011", "yle_2015", "hs_2015", "yle_2019")
#'
#'@return Tbl dataframe.
#'@export
get_dataset <- function(name,filter_precinct=NULL){
  if(name=="hs_2015"){
    stop("HS dataset not implemented yet.")
  }
  data <- switch (name,
          hs_2015=.get_HS_2015_data(),
          yle_2011=get_YLE_2011_data(filter_precinct=filter_precinct),
          yle_2015=get_YLE_2015_data(filter_precinct=filter_precinct),
          yle_2019=get_YLE_2019_data(filter_precinct=filter_precinct)
          )
  
  return(data)
}

#'Get the question columns for the specified data set
#'
#'@param dataset_name Name of the dataset ('yle_2011', 'yle_2015', 'yle_2019 or 'hs_2015')
#'
#'@usage q_cols <- get_data_cols('yle_2011', data)
#'
#'@return List of columns that represent the questions.
#'@export
get_data_cols <- function(dataset_name,data){
  yle_2019_q_list <- c(
     "Suomen_pitää_olla_edelläkävijä_ilmastonmuutoksen_vastaisessa_taistelussa._vaikka_se_aiheuttaisi_suomalaisille_kustannuksia.",                  
     "Suomen_ei_pidä_kiirehtiä_kieltämään_uusien_bensa._ja_dieselautojen_myyntiä.",                                                                  
     "Valtion_pitää_ohjata_suomalaiset_syömään_vähemmän_lihaa_esimerkiksi_verotuksen_avulla.",                                                       
     "Metsiä_hakataan_Suomessa_liikaa.",                                                                                                             
     "Kun_valtion_menoja_ja_tuloja_tasapainotetaan._se_on_tehtävä_mieluummin_menoja_karsimalla_kuin_veroja_kiristämällä.",                           
     "Sosiaaliturvaa_tulee_kehittää_niin._että_osa_nykyisistä_tuista_korvataan_kaikille_työikäisille_maksettavalla._vastikkeettomalla_perustulolla.",
     "Euron_ulkopuolella_Suomi_pärjäisi_paremmin.",                                                                                                  
     "Sosiaali._ja_terveyspalvelut_on_tuotettava_ensisijaisesti_julkisina_palveluina.",                                                              
     "Vanhustenhoidon_ulkoistamista_yksityisille_toimijoille_tulee_lisätä.",                                                                         
     "Parantumattomasti_sairaalla_on_oltava_oikeus_eutanasiaan.",                                                                                    
     "Sukupuolen_korjaamisen_tulee_olla_mahdollista_myös_alle_18.vuotiaille.",                                                                       
     "Viinit_ja_vahvat_oluet_pitää_saada_ruokakauppoihin.",                                                                                          
     "Perhevapaita_pitää_uudistaa_niin._että_vapaat_jakautuvat_tasan_vanhempien_kesken.",                                                            
     "Oppivelvollisuus_pitää_ulottaa_myös_ammatilliseen_koulutukseen_ja_lukioon.",                                                                   
     "Koulujen_kesälomia_tulee_siirtää_kahdella_viikolla_niin._että_ne_alkavat_kesäkuun_puolivälissä_ja_päättyvät_elokuun_lopulla.",                 
     "Korkeakoulujen_määrää_pitää_vähentää_ja_vapautuneet_voimavarat_käyttää_huippuopetukseen_ja_.tutkimukseen.",                                    
     "Maahanmuuttajien_määrän_kasvu_on_lisännyt_turvattomuutta_Suomessa.",                                                                           
     "Sosiaali._ja_terveyspalveluiden_rahoittaminen_vaatii_työperäisen_maahanmuuton_merkittävää_lisäämistä.",                                        
     "Nato.jäsenyys_vahvistaisi_Suomen_turvallisuuspoliittista_asemaa.",                                                                             
     "Vihapuhe_tulee_määritellä_ja_asettaa_rangaistavaksi_rikoslaissa.",                                                                             
     "Perinteiset_arvot_ovat_hyvän_elämän_perusta.",                                                                                                 
     "Suomessa_tarvitaan_nyt_koviakin_keinoja_järjestyksen_ja_tavallisten_ihmisten_puolustamiseksi.",                                                
   "On_oikein._että_yhteiskunnassa_jotkut_ryhmät_ovat_paremmassa_asemassa_kuin_toiset.",                                                           
     "Suomen_lakien_pitäisi_nykyistä_vapaammin_antaa_ihmisten_tehdä_omat_ratkaisunsa_ja_kantaa_niiden_seuraukset.",                                  
     "Poliitikon_velvollisuus_on_ennen_kaikkea_ajaa_omien_äänestäjiensä_etuja." 
  )
  data_cols <- switch (dataset_name,
                       hs_2015 = names(dplyr::select(data, q1:q30)),
                       yle_2015 = stringr::str_subset(names(data), "X?[:digit:]+[\\|.][:upper:]"),
                       yle_2011 = stringr::str_subset(names(data), "X?[:digit:]+[\\|\\.]."),
                       yle_2019 = stringr::str_replace_all(yle_2019_q_list," ","_")
  )
  return(data_cols)
}

#'Prepare data for analysis
#'
#'Prepares data for analysis by combining small parties, factoring the party column, 
#'and ensuring that question columns are numeric.
#'
#'@param data Dataset.
#'@param q_cols Question columns. See \code{\link{get_data_cols}}.
#'@param limit Candidate limit for setting party column to other.
#'@return Dataset with subbed and factored party columns, and numeric question cols.
#'
#'@usage data <- prepare_data(data, q_cols, party_col)
#'@export
prepare_data <- function(data, q_cols, party_col,limit){
  data[[party_col]] <- sub_parties_for_shortcodes(data[[party_col]])
  data[[party_col]] <- set_small_parties_to_other(data,colname_party = party_col, limit = limit)
  data[[party_col]] <- factor(data[[party_col]])
  
  #turn string answers to numeric if necessary
  for(col in q_cols){
    data[,col] <- as.numeric(as.factor(data[[col]]))
  }
  
  #drop candidates who have not answered some questions
  data <- data[rowSums(is.na(data[,q_cols]))==0,]
  
  return(data)
}
#'Combine small parties to group "Other"
#'
#'Takes all parties with less than \code{nlimit} members, and changes their party
#'to "Other".
#'
#' @param data Data set to be used (either tbl or dataframe)
#' @param colname_party Column name that has the party information (default is "party")
#' @param limit Candidate limit for setting party membership to "Other". Default: 10
#' @return Returns the party column
#' 
#' @usage
#' df$party <- set_small_parties_to_other(df, "party")
#' 
#' @export  
set_small_parties_to_other <- function(data, colname_party="party",limit=10){
  data[[colname_party]] <- factor(data[[colname_party]])
  var_unquo <- rlang::sym(colname_party)
  big_parties<-data[,colname_party] %>% dplyr::count(!!var_unquo) %>% dplyr::filter(n>limit) %>% dplyr::pull(!!var_unquo)
  data[,colname_party] <- forcats::fct_other(data[[colname_party]],keep=big_parties,other_level = "Other")
  return(data[[colname_party]])
}


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
#' @usage paf <- PAF(data, 3, FALSE, cols=names(select(data,q1:q30)))
#' @export
PAF<-function(data,nfactors,vss,cols){
  #bivariate correlations
  bcor<-stats::cor(data[,cols])
  det(bcor)
  
  #KMO test & anti-image
  kmo<-psych::KMO(data[,cols])
  
  if(vss==TRUE){
    psych::vss(bcor,10,rotate="varimax",fm="pa",n.obs=nrow(data))
    psych::scree(bcor,factors=F,pc=T,hline=1,main=paste("Scree with",length(cols),"questions"))
  }
  #FA with nfactors factors
  fact<-psych::fa(bcor,nfactors=nfactors,fm="pa",n.obs=nrow(data),rotate="varimax",scores=TRUE,SMC=FALSE)
  #print(fact)
  f2<-psych::factor.scores(data[,cols],fact$loadings)
  
  #save F2 scores to data
  fa_ans<-data.frame(cbind(data,f2$scores))
  
  return(list("scores"=fa_ans,"corr"=bcor,"det"=det(bcor),"KMO"=kmo,"loadings"=fact$loadings))
}


#'Substitute Finnish Party names with short versions.
#'
#'Takes all party names and substitutes their short versions.
#'
#'@param datacol Vector of parties to be replaced.
#'
#'@usage data$party <- sub_parties_for_shortcodes(datacol=dataparty)
#'@export
sub_parties_for_shortcodes <- function(datacol){
  #get rid of extra party names inside brackets inside the party columns
  datacol <- stringr::str_replace(datacol," [:punct:].+[:punct:]","")
  short_parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SKP","STP","VAS","VIHR","VP", "RKP")
  parties <- c("Itsenäisyyspuolue","Köyhien Asialla","Suomen Kristillisdemokraatit","Suomen Keskusta","Kansallinen Kokoomus","Other","Kommunistinen Työväenpuolue","Muutos 2011","Piraattipuolue","Perussuomalaiset","Suomen ruotsalainen kansanpuolue","Suomen Sosialidemokraattinen Puolue","Suomen Kommunistinen Puolue","Suomen Työväenpuolue STP","Vasemmistoliitto","Vihreä liitto","Vapauspuolue","Ruotsalainen kansanpuolue")
  names(short_parties) <- parties
  parties_c <- stringr::str_c("^",parties,collapse = "|")
  party_repl <- function(p){
    return(as.character(short_parties[p]))
    #return(p)
  }
  stringr::str_replace_all(datacol, parties_c,party_repl)
}


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
#'@usage res <- classComp(data, 10, 1, "rf", "party")
#'@export
classComp<-function(data,k,model,party_col,...){
  controlCV<-caret::trainControl(method="cv",number=k,...)
  f <- as.formula(paste0(party_col,"~."))
  #run model
  rf<-caret::train(f,data=data,method=model,metric="Accuracy",trControl=controlCV)
  res<-data.frame("par"=unlist(rf$bestTune),rf$results[which(rf$results[,1]==unlist(rf$bestTune[1])),2:5])
  rownames(res)<-c(model)
  
  return(res)
}


#'Remove election machine question with name \code{qname}.
#'
#'@param data Dataset
#'@param qname The name of question to be removed
#'
#'@return Dataset without the removed question.
#'@export
removeQname <- function(data,qname){
  if(!qname%in%colnames(data)){
    stop(sprintf("%s not in column names", qname))
  }
  var_unquo <- qname
  return(dplyr::select(data,-var_unquo))
}

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
analyze_removed_questions <- function(data, imp_num, party_col){
  d2<-data
  f <- as.formula(paste0(party_col,"~q1"))
  nam<-colnames(randomForest::randomForest(f,data=d2)$confusion)
  nam<-nam[1:(length(nam)-1)]
  res<-as.data.frame(matrix(0,1,length(nam)+1))
  colnames(res)<-c("removed",nam)
  #ptm <- proc.time()
  for(x in imp_num){
    print(x)
    d2<-removeQname(d2,paste0("q",x,collapse = ""))
    f <- as.formula(paste0(party_col,"~."))
    rf<-randomForest::randomForest(f,data=d2,importance=TRUE)
    rconf<-rf$confusion[,"class.error"]
    pres<-as.data.frame(matrix(0,1,length(nam)))
    colnames(pres)<-nam
    pres<-sapply(1:length(nam),function(x) rconf[nam[x]])
    res<-rbind(res,c("removed"=x,pres))
    
  }
  
  res<-res[-1,]
  
  #print(proc.time()-ptm)
  return(res)
}

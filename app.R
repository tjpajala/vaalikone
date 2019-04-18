
source_packages <- c("shiny","dplyr","psych","DT","ggplot2","devtools","corrplot")
for(p in source_packages){
  if(!require(p, character.only = T, quietly = F)){
    install.packages(p, character.only = T)
    library(p, character.only = T)
  }
}
source("./R/functions.R")
source("./R/plotting.R")


#data setup code
setup_data <- function(dataset_name,filter_precinct = c("01 Helsingin vaalipiiri"), online_load=FALSE){
  #dataset_name <- "yle_2011" #option: yle_2011, hs_2015, yle_2019 (not public)
  if(dataset_name=="hs_2015"){
    stop("HS dataset not implemented, it is not public. Sorry!")
  }
  if(online_load){
    #online loading
    data <- get_dataset(name=dataset_name,filter_precinct = filter_precinct)
  } else {
    #offline loading
    data <- readRDS(paste0("./data/",dataset_name,".rds"))
  }
  
  party_col <- get_functional_column_name(data,alternative_spellings = c("puolue","Puolue","party","Party"))
  q_cols=get_data_cols(dataset_name = dataset_name, data=data)
  if(dataset_name=="yle_2011"){
    nums <- as.numeric(stringr::str_sub(q_cols,1,3))
    idx <- sapply(1:length(nums), function(x) {is.na(x) | x<31})
    q_cols <- q_cols[idx]
    #q_cols <- q_cols[q_cols!="34._Mielestäni_seuraavassa_hallituksessa_on_oltava_mukana:"]
    #q_cols <- q_cols[q_cols!="35._Suosikkini_tulevan_hallituksen_pääministeriksi_on_(vain_yksi):"]
    
  }
  if(dataset_name=="yle_2019"){
    data<- data %>% filter(rowSums(data[,q_cols]==3)!=25)
  }
  data <- prepare_data(data, q_cols, party_col,limit=10)
  
  
  qdata <- dplyr::select(data, dplyr::one_of(party_col, q_cols))
  #run principal axis factoring
  fa <- PAF(qdata, nfactors=2, vss=FALSE, cols=q_cols)
  return(fa)
}
yle_2011 <- setup_data("yle_2011",filter_precinct = NULL, online_load = F)
yle_2015 <- setup_data("yle_2015",filter_precinct = NULL, online_load = F)
yle_2019 <- setup_data("yle_2019",filter_precinct = NULL, online_load = F)
#yle_2011 <- setup_data("yle_2011",filter_precinct = NULL, online_load = T)
#yle_2015 <- setup_data("yle_2015",filter_precinct = NULL, online_load = T)
#yle_2019 <- setup_data("yle_2019",filter_precinct = NULL, online_load = T)
alt_party_spellings <- c("puolue","Puolue","party","Party")

calculate_distances_one_voter<-function(voter,cand_data,dataset_name,distance_metric,
                                        limits=get_questions_and_answer_alternatives(cand_data,dataset_name)$answers_limits){
  if(is.null(voter)){
    voter <- sample(c(limits$min,limits$max),size=ncol(cand_data),replace = T)
  }
  
  if(ncol(cand_data)!=length(voter)){
    stop("Nonmatching dimensions in cand_data and voter!")
  }
  if(distance_metric=="L1"){
    q_dist <- abs(sweep(cand_data,2,voter,FUN="-"))
    dist_limits <-list("min"=0,"max"=(limits$max-1)*length(voter))
    cand_scores <- 100 - (rowSums(q_dist)/dist_limits$max)*100
  }
  if(distance_metric=="L2"){
    q_dist <- (sweep(cand_data,2,voter,FUN="-"))^2
    dist_limits <-list("min"=0,"max"=sqrt((limits$max-1)^2*length(voter)))
    cand_scores <- 100 - (sqrt(rowSums(q_dist))/dist_limits$max)*100
  }
  
  return(cand_scores)
}

get_questions_and_answer_alternatives<-function(data,dataset_name){
  q_cols=get_data_cols(dataset_name = dataset_name, data=data)
  if(dataset_name=="yle_2011"){
    q_cols <- q_cols[q_cols!="34._Mielestäni_seuraavassa_hallituksessa_on_oltava_mukana:"]
    q_cols <- q_cols[q_cols!="35._Suosikkini_tulevan_hallituksen_pääministeriksi_on_(vain_yksi):"]
  }
  answers_limits <- list("min"=min(data[,q_cols]), "max"=max(data[,q_cols]))
  return(list("questions"=q_cols, "answers_limits"=answers_limits))
}

calculate_distances_all_voters <- function(voters, data, dataset_name,distance_metric){
  q_cols=get_data_cols(dataset_name = dataset_name, data=data)
  if(dataset_name=="yle_2011"){
    q_cols <- q_cols[q_cols!="34._Mielestäni_seuraavassa_hallituksessa_on_oltava_mukana:"]
    q_cols <- q_cols[q_cols!="35._Suosikkini_tulevan_hallituksen_pääministeriksi_on_(vain_yksi):"]
  }
  cand_data <- data.matrix(data[,q_cols])
  limits <- get_questions_and_answer_alternatives(data,dataset_name)$answers_limits
  if(!exists(paste0(quote(voters)))||is.null(voters)){
    voters <- sapply(c(1:1000), function(x) sample(c(limits$min,limits$max),size=length(q_cols),replace = T))
  }
  voters <- data.matrix(voters)
  dist<-sapply(c(1:ncol(voters)), function(x) calculate_distances_one_voter(voters[,x],cand_data,dataset_name,distance_metric,limits))
  return(dist)  
}


calculate_party_centers <- function(data, dataset_name, voter, metric, distance_metric){
  need(voter, "voter is missing in party center calculation")
  party_col <- get_functional_column_name(data, alt_party_spellings)
  q_cols <- get_data_cols(dataset_name = dataset_name, data=data)
  sym_party <- sym(party_col)
  if(metric=="ehdokkaiden_keskiarvo" ||metric=="etaisyyksien_keskiarvo"){
    centers <- data %>% dplyr::group_by(!!sym_party) %>% dplyr::select(dplyr::one_of(party_col,q_cols)) %>% 
      dplyr::summarise_each(mean)
    return(centers)
  }
  if(metric=="ehdokkaiden_mediaani" || metric=="etaisyyksien_mediaani"){
    centers <- data %>% dplyr::group_by(!!sym_party) %>% dplyr::select(dplyr::one_of(party_col,q_cols)) %>% 
      dplyr::summarise_each(median)
    return(centers)
  }
}

calculate_party_distances <- function(data, dataset_name, voter, metric, distance_metric){
  #shiny::need(voter,message = "voter is missing")
  party_col <- get_functional_column_name(data, alt_party_spellings)
  q_cols <- get_data_cols(dataset_name = dataset_name, data=data)
  sym_party <- sym(party_col)
  limits <- get_questions_and_answer_alternatives(data,dataset_name)$answers_limits
  percentage_round_digits <- 2
  #if(!shortcodes){
  #  data[[party_col]]<-shortcodes_to_parties(data[[party_col]])
  #}
  
  if(metric=="ehdokkaiden_keskiarvo"){
    #print(length(voter))
    centers <- calculate_party_centers(data=data, dataset_name=dataset_name, voter=voter, metric=metric, 
                                       distance_metric=distance_metric)
    #print(dim(centers[,q_cols]))
    #print(ncol(centers[,q_cols]) == length(voter))
    if(ncol(centers[,q_cols]) != length(voter)){
      print(dim(centers[,q_cols]))
      print(str(voter))
      stop("AAARGH!")
    }
    dist <- calculate_distances_one_voter(voter=voter,cand_data=centers[,q_cols],dataset_name=dataset_name,
                                          distance_metric=distance_metric,limits=limits)
    #dist is already scaled
    centers["match"]<-round(dist,percentage_round_digits)
    return(centers[,c(party_col,"match")])
  }
  if(metric=="ehdokkaiden_mediaani"){
    centers <- calculate_party_centers(data, dataset_name, voter, metric, distance_metric)
    if(ncol(centers[,q_cols])!=length(voter)){
      print(dim(centers[,q_cols]))
      print(str(voter))
      stop("AAARGH! vol.2")
    }
    dist <- calculate_distances_one_voter(voter=voter,cand_data=centers[,q_cols],dataset_name=dataset_name,
                                          distance_metric=distance_metric,limits=limits)
    #dist is already scaled
    centers["match"]<-round(dist,percentage_round_digits)
    return(centers[,c(party_col,"match")])
    
  }
  if(metric=="etaisyyksien_keskiarvo"){
    dist <- calculate_distances_one_voter(voter=voter,cand_data=data[,q_cols],dataset_name=dataset_name,
                                          distance_metric=distance_metric,limits=limits)
    data["match"]<-dist
    centers <- data %>% dplyr::group_by(!!sym_party) %>% dplyr::select(dplyr::one_of(party_col,"match")) %>% 
      dplyr::summarise_each(mean)
    #centers is already scaled to max_dist
    centers[,"match"]<-round(centers[,"match"],percentage_round_digits)
    return(centers)
  }
  if(metric=="etaisyyksien_mediaani"){
    dist <- calculate_distances_one_voter(voter=voter,cand_data=data[,q_cols],dataset_name=dataset_name,
                                          distance_metric=distance_metric,limits=limits)
    data["match"]<-dist
    centers <- data %>% dplyr::group_by(!!sym_party) %>% dplyr::select(dplyr::one_of(party_col,"match")) %>% 
      dplyr::summarise_each(median)
    #centers is already scaled to max_dist
    centers[,"match"]<-round(centers[,"match"],percentage_round_digits)
    return(centers)
  }
}

get_rotation<-function(dataset_name){
  if(dataset_name=="yle_2015"){
    return(180)
  }
  if(dataset_name=="yle_2011"){
    return(180)
  }
  if(dataset_name=="yle_2019"){
    return(90)
  }
}

get_data <- function(dataset_name){
  if(dataset_name=="yle_2015"){
    return(yle_2015)
  }
  if(dataset_name=="yle_2011"){
    return(yle_2011)
  }
  if(dataset_name=="yle_2019"){
    return(yle_2019)
  }
}

plot_party_centers<-function(data_full,dataset_name, voter, metric, distance_metric){
  party_col <- get_functional_column_name(data_full$scores,alt_party_spellings)
  party_col <- sym(party_col)
  centers<-list("locations"=calculate_party_centers(data=data_full$scores,dataset_name=dataset_name, voter=voter, metric=metric, distance_metric=distance_metric))
  f2 <- predict.psych(data_full$fa,centers$locations[,2:(ncol(centers$locations))],data_full$scores[,2:(ncol(centers$locations))])
  f3 <- predict.psych(data_full$fa, voter, data_full$scores[,2:(ncol(centers$locations))])    
  #f2<-rot(f2[,"PA1"],f2[,"PA2"],get_rotation(dataset_name))
  f3<-rot(f3[,"PA1"],f3[,"PA2"],get_rotation(dataset_name))
  #colnames(f2)<-c("PA1","PA2")
  colnames(f3)<-c("PA1","PA2")
  centers$locations["PA1"] <- f2[,"PA1"]
  centers$locations["PA2"] <- f2[,"PA2"]
  data_full$scores["type"]<-"candidate"
  centers$locations["type"]<-"center"
  d<-rbind(data_full$scores[,c(rlang::as_name(party_col),"PA1","PA2","type")],
           centers$locations[,c(rlang::as_name(party_col),"PA1","PA2","type")])
  rotated <- rot(d[,"PA1"],d[,"PA2"],get_rotation(dataset_name))
  colnames(rotated)<-c("PA1","PA2")
  d[,c("PA1","PA2")]<-rotated[,c("PA1","PA2")]
  #d <- rbind(d, c(NULL, f3[,c("PA1","PA2")], as.factor("voter")))
  colp<- get_colors()
  voter_df <- data.frame("PA1"=f3[,"PA1"],"PA2"=f3[,"PA2"], "puolue"="Other","type"="voter")
  ggplot2::ggplot(d, ggplot2::aes(x = PA1, y = PA2,pch=type,size=type,alpha=type, color=!!party_col)) + 
    ggplot2::geom_point() + 
    geom_point(data=voter_df,color="black")+
    geom_text(data = d[d$type=="center",], aes(label=!!party_col), nudge_y = -0.15, show.legend = F)+
    geom_text(data = voter_df, label="voter", color="black", nudge_y = -0.15, show.legend = F)+
    ggplot2::scale_color_manual(values = colp) + ggplot2::scale_shape_manual(values=c(16, 17, 13))+
    ggplot2::scale_size_manual(values=c(1.5,4,4))+ ggplot2::scale_alpha_manual(values=c(0.3,1,1))+
    ggplot2::theme_classic()
  
}

#cands_sim_2200 <- dplyr::sample_n(yle_2015$scores,size=2200,replace = T)
#limits <- get_questions_and_answer_alternatives(data,"yle_2015")$answers_limits
#voters <- sapply(c(1:10000), function(x) sample(c(limits$min,limits$max),size=length(get_data_cols("yle_2015",cands_sim_2200)),replace = T))
#microbenchmark(calculate_distances_all_voters(voters=voters,data=cands_sim_2200, dataset_name = "yle_2015",distance_metric = "L2")
#, times=5)
#t_1k<-microbenchmark(calculate_distances_all_voters(voters=NULL,data=cands_sim_2200, dataset_name = "yle_2015",distance_metric = "L2")
#                     , times=5)




get_party_name<-function(dataset_name){
  if(dataset_name=="hs_2015"){
    return("Party")
  }
  if(dataset_name=="yle_2015"){
    return("puolue")
  }
  if(dataset_name=="yle_2011"){
    return("Puolue")
  }
  if(dataset_name=="yle_2019"){
    return("puolue")
  }
  
}

questions_to_taglist<-function(data, dataset_name){
  questions <- get_questions_and_answer_alternatives(data, dataset_name)
  #elem_list <- tagList()
  #for(q in 1:length(questions$questions)){
  #  elem_list<-tagAppendChild(elem_list,radioButtons(paste0(q,"_q"),questions$questions[q],
  #                                                   choices = c(1:5),selected = 3))
  #}
  elem_list <- lapply(1:length(questions$questions), function(q) {
    #questions$questions[q]
    radioButtons(inputId=paste0(q,"_q"),label=stringr::str_replace_all(questions$questions[q],"_"," "),
                 choiceValues = list(1,2,3,4,5),choiceNames=list("--","-","0","+","++"),selected = 3,inline = T)
  })
  do.call(tagList, elem_list)
  return(elem_list)
}

get_voter_answers <- function(input,data){
  req(input$"1_q")#,"1_q element is missing at this point in get_voter_answers")
  dataset_name <- input$data_select
  questions <- get_questions_and_answer_alternatives(data, dataset_name)
  
  ans <- sapply(1:length(questions$questions),function(q) input[[paste0(q,"_q")]],simplify = T)
  return(as.numeric(unlist(ans)))
}

combine_cands_and_scores <- function(cand_data, dist_scores){
  cand_data["dist"]<-dist_scores
  return(cand_data)
}

confusion_plot<-function(confusionmatrix, margin=1, order="alphabet"){
  corrplot::corrplot(prop.table(confusionmatrix, margin), 
                     method="shade", 
                     is.corr=FALSE, 
                     addCoef.col="cyan",
                     cl.pos="n", 
                     addCoefasPercent=TRUE,
                     col=colorRampPalette(c("white","white","black"),1)(80),
                     order=order)
} 

get_stored_cf_matrix <- function(dataset_name, metric, distance_metric){
  return(readRDS(paste0("./data/cf_matrix/",dataset_name,"_",metric,"_",distance_metric,".rds")))
}


store_all_cf_matrix<-function(){
  metrics<-c("ehdokkaiden_keskiarvo","ehdokkaiden_mediaani","etaisyyksien_keskiarvo","etaisyyksien_mediaani")
  distance_metrics<-c("L1","L2")
  datasets<-c("yle_2011","yle_2015","yle_2019")
  params <- expand.grid(datasets,metrics,distance_metrics,stringsAsFactors = F)
  colnames(params)<-c("dataset_name","metric","distance_metric")
  for(i in 1:nrow(params)){
    pr <- params[i,]
    print(pr)
    conf<-confusion(data=get_data(as.character(pr$dataset_name))$scores,dataset_name=as.character(pr$dataset_name),metric=as.character(pr$metric),distance_metric=as.character(pr$distance_metric))
    saveRDS(conf,paste0("./data/cf_matrix/",pr$dataset_name,"_",pr$metric,"_",pr$distance_metric,".rds"))
  }
}

#matrix(runif(21*21)*100,21,21)
confusion <- function(data, dataset_name, metric, distance_metric){
  q_cols <- get_data_cols(dataset_name = dataset_name, data=data)
  party_col <- get_functional_column_name(data,alt_party_spellings)
  d <- t(sapply(1:nrow(data), function(x) calculate_party_distances(data = data, dataset_name = dataset_name, voter = as.numeric(data[x,q_cols]),metric = metric, distance_metric = distance_metric)$match))
  #print(dim(d))
  #center_ops <- calculate_party_centers(data,dataset_name = dataset_name,voter = voter,metric = metric,distance_metric = distance_metric)
  #d<-t(calculate_distances_all_voters(voters = t(data[,q_cols]),data = center_ops[,q_cols],dataset_name = dataset_name,distance_metric = distance_metric))
  best <- sapply(1:nrow(d),function(x) which.max(d[x,]))
  pred <- levels(data[[party_col]])[best]
  return(table(data[[party_col]],factor(pred,levels = levels(data[[party_col]]))))
}

accuracy <- function(confusionmatrix) sum(diag(confusionmatrix))/sum(confusionmatrix)

# Define UI ----
ui <- fluidPage(
  titlePanel("Vaalikoneen etäisyysmitta ja puolue-etäisyyden määritelmä"),
  sidebarLayout(
    position = "right",
    sidebarPanel("", style = "overflow-y:scroll; max-height: 800px", width = 4,
                 selectInput("data_select","Dataset",
                             choices = list("YLE 2011"="yle_2011",
                                            "YLE 2015"="yle_2015",
                                            "YLE 2019"="yle_2019"),
                             selected = "yle_2019"),
                 selectInput("dist_select","Etäisyysmitta",
                             choices = list("L1"="L1","L2"="L2")),
                 selectInput("party_mean_select","Puolue-etäisyyden määritelmä",
                             choices = list("Ehdokkaiden keskiarvo"="ehdokkaiden_keskiarvo","Ehdokkaiden mediaani"="ehdokkaiden_mediaani",
                                            "Etäisyyksien keskiarvo"="etaisyyksien_keskiarvo","Etäisyyksien mediaani"="etaisyyksien_mediaani")),
                 uiOutput("questionControls")),
    mainPanel("",
              textOutput("dataset"), 
              br(),
              tabsetPanel(
                tabPanel("Faktorit", plotOutput("fa_plot")), 
                tabPanel("Luokittelu", 
                         sidebarLayout(
                           position="left", sidebarPanel(
                             fluidRow("Taulukko näyttää valitsemallasi etäisyys- ja 
keskipistemäärityksillä, miten vaalikoneeseen vastanneiden ehdokkaiden puoluevastaavuudet jakaantuvat. 
Rivi kertoo ehdokkaan oikean puolueen ja sarake puolueen, jonka algoritmi antaisi ehdokkaalle itselleen. 
Luvut ovat osuuksia prosentteina. Jos algoritmi toimisi täydellisesti, olisi lävistäjällä 100% ja muualla nolla. 
                                      (Vastauksesi vaalikonekysymyksiin eivät vaikuta tähän taulukkoon.)"),
                             br(), 
                             strong(textOutput("accuracy")),
                             width=4), 
                           mainPanel(plotOutput("confusion"))
                         ) 
                )
              ),
              hr(),
              dataTableOutput("closest_candidates"),
              dataTableOutput("parties_dist"))
    
  )
)

# Define server logic ----
server <- function(input, output) {
  
  reactive_data <- reactive({get_data(input$data_select)})
  
  qControls <- reactive(questions_to_taglist(reactive_data()$scores,input$data_select))
  output$questionControls <- renderUI({
    #give list of elements inside tagList to render many
    qControls()
  })
  
  reactive_voter <- reactive({
    shiny::req(input[["1_q"]])
    get_voter_answers(input=input,data=reactive_data()$scores)
  })
  
  output$dataset <- renderText({"Tällä voit kokeilla erilaisten etäisyysmittojen sekä 
    puolue-etäisyyden määritelmien vaikutusta sinulle suositeltavan puolueen tulokseen. Käytössä on 
    kolme datasettiä: kaikki YLEn kolmen edellisen eduskuntavaalin tulokset. Oikeasta reunasta
    voit valita haluamasi laskenta-algoritmin ominaisuudet, sekä vastata kyseisen vuoden vaalikoneen kysymyksiin."})
  #output$dataset <- renderText({paste("Data: ", input$data_select, ", etäisyysmitalla ",input$dist_select,
  #                                    ", puolueen keskipiste: ",input$party_mean_select, "\n",
  #                                    "äänestäjä: ", paste0(reactive_voter(),collapse = ", "),
  #                                    ", kysymyksiä: ",length(reactive_voter()))})
  #output$fa_plot <- renderPlot({FA_ggplot(get_data(input$data_select),flip=20, 
  #                                                     colname_party=get_party_name(input$data_select),
  #                                                     encircle = F)})
  output$fa_plot <- renderPlot({plot_party_centers(data_full=reactive_data(),
                                                   dataset_name=input$data_select, 
                                                   voter=reactive_voter(), 
                                                   metric=input$party_mean_select, 
                                                   distance_metric=input$dist_select)})
  
  reactive_cfmatrix <- reactive({get_stored_cf_matrix(dataset_name = input$data_select,metric = input$party_mean_select,distance_metric = input$dist_select)})

  output$confusion <- renderPlot({confusion_plot(confusionmatrix=reactive_cfmatrix(), 
                                                 margin=1, order="alphabet")})
  
  output$accuracy <- renderText({paste("Kokonaistarkkuus: ", round(100*accuracy(reactive_cfmatrix()), 2), "%")})
  
  output$parties_dist <- DT::renderDataTable({
    p <- DT::datatable(calculate_party_distances(data=reactive_data()$scores, 
                                                 dataset_name=input$data_select, 
                                                 voter=reactive_voter(), 
                                                 metric=input$party_mean_select, 
                                                 distance_metric=input$dist_select) %>%
                         dplyr::mutate(puolue2:=paste0(shortcodes_to_parties(!!sym(get_party_name(input$data_select)))," (",!!sym(get_party_name(input$data_select)),")")) %>% 
                         dplyr::select(puolue2,match),
                       options = list(
                         order=list(1,'desc'),
                         columns=list(#list(title=""),
                           list(title="puolue"),
                           list(title="sopivuus %")),
                         paging=FALSE
                       ),
                       rownames=FALSE, 
                       selection="none")
    p <- p %>% DT::formatStyle("match",
                               background = DT::styleColorBar(c(0, 100), 'lightblue'),
                               backgroundSize = '98% 88%',
                               backgroundRepeat = 'no-repeat',
                               backgroundPosition = 'center')
    return(p)
  })
  
  
  #output$parties_dist <- DT::renderDataTable({calculate_party_distances(data=reactive_data()$scores, 
  #                                                                      dataset_name=input$data_select, 
  #                                                                      voter=reactive_voter(), 
  #                                                                      metric=input$party_mean_select, 
  #                                                                      distance_metric=input$dist_select) %>%
  #    dplyr::mutate(puolue2:=paste0(shortcodes_to_parties(!!sym(get_party_name(input$data_select)))," (",!!sym(get_party_name(input$data_select)),")")) %>% 
  #    dplyr::select(puolue2,match)},
  #                                           options = list(
  #                                             order=list(1,'desc'),
  #                                             columns=list(#list(title=""),
  #                                                          list(title="puolue"),
  #                                                          list(title="sopivuus %")),
  #                                             paging=FALSE
  #                                             ),rownames=FALSE)
}

# Run the app ----
shinyApp(ui = ui, server = server)

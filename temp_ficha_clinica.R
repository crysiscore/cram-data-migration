#' composeFichaClinica ->  criar uma string json com informacao para criar a ficha clinica de um paciente no openmrs
#' @param   pat.nid  nid do cram
#' @examples 
#' ficha_resumo  <- composeFichaResumo(df,1234)
#' 


composeFichaResumo <- function(df.visits,pat.nid,openmrs.pat.uuid) {
  
  pat <- filter(df.visits, nid==pat.nid) %>% arrange(datvisit)
  
  
  if(nrow(pat)!=0){
    
    #visit_atributes
    visit_date       <- pat$datvisit[index]
    next_visit_date  <- pat$datnext[index]
    patient        <- openmrs.pat.uuid
    #encounter_date_datime <- paste0(visit_date," 11:37:31" )
    encounter_date_datime <- visit_date
    obs_date_time <- visit_date
    age <- pat$agev[index]      
    
    #Whostage
    #TODO estadio OMS: 0 - unknown
    if(i==1){
      estadio_oms <- pat$whof[index]
    } else {
      
      estadio_oms <- pat$whop[index]
    }

    if (estadio_oms==0){
      obs_estadio <- ""
    } else if(estadio_oms==1){
      estadio_oms <- 'e1d9055e-1d5f-11e0-b929-000c29ad1d07'
      obs_estadio <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                             "\"concept\":\"",concept_fc_estadio_oms ,"\"," ,
                             "\"obsDatetime\":\"", obs_date_time,"\"," ,
                             "\"value\":\"", estadio_oms,"\"" ,
                             "}")
      
    } else if(estadio_oms==2){
      estadio_oms <- 'e1d9066c-1d5f-11e0-b929-000c29ad1d07'
      obs_estadio <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                             "\"concept\":\"",concept_fc_estadio_oms ,"\"," ,
                             "\"obsDatetime\":\"", obs_date_time,"\"," ,
                             "\"value\":\"", estadio_oms,"\"" ,
                             "}")
      
    } else if(estadio_oms==3){
      estadio_oms <- 'e1d9077a-1d5f-11e0-b929-000c29ad1d07'
      obs_estadio <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                             "\"concept\":\"",concept_fc_estadio_oms ,"\"," ,
                             "\"obsDatetime\":\"", obs_date_time,"\"," ,
                             "\"value\":\"", estadio_oms,"\"" ,
                             "}")
    } else if(estadio_oms==4){
      estadio_oms <- 'e1d90888-1d5f-11e0-b929-000c29ad1d07'
      obs_estadio <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                             "\"concept\":\"",concept_fc_estadio_oms ,"\"," ,
                             "\"obsDatetime\":\"", obs_date_time,"\"," ,
                             "\"value\":\"", estadio_oms,"\"" ,
                             "}")
    } else {
      obs_estadio <- ""
    }

    # Peso
    weight <- pat$weight[index]
    if(is.na(weight) | weight ==""){
      obs_weight <-""
    } else {
      
      obs_weight  <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                          "\"concept\":\"", concept_fc_weight,"\"," ,
                          "\"obsDatetime\":\"", obs_date_time,"\"," ,
                          "\"value\":\"", weight,"\"" ,
                          "}")
      
    }
    #Altura
    height <- pat$height[index]
    if(is.na(height) | height ==""){
      obs_height <-""
    } else {
      
      obs_height  <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                             "\"concept\":\"", concept_fc_height,"\"," ,
                             "\"obsDatetime\":\"", obs_date_time,"\"," ,
                             "\"value\":\"", height,"\"" ,
                             "}")
      
    }
    
    #Profilaxias  INH
    prof_ihn <- pat$inh[index]
    if(is.na(prof_ihn) | prof_ihn ==""){
      obs_prof_ihn <-""
    } else {
      
      obs_prof_ihn <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                             "\"concept\":\"", concept_fc_prof_inh,"\"," ,
                             "\"obsDatetime\":\"", obs_date_time,"\"," ,
                             "\"value\":\"", value_coded_continua,"\"" ,
                             "}")
      
    }
    
    #Profilaxias CTZ 
    prof_ctz<- pat$cotri[index]
    if(is.na(prof_ctz) | prof_ctz ==""){
      obs_prof_ctz <-""
    } else {
      
      obs_prof_ctz <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                              "\"concept\":\"", concept_fc_pof_ctz,"\"," ,
                              "\"obsDatetime\":\"", obs_date_time,"\"," ,
                              "\"value\":\"", value_coded_continua,"\"" ,
                              "}")
    }
    
    
    #carga viral
    carga_viral    <- pat$hivload[index]
    if(is.na(carga_viral) | carga_viral==""){
      obs_carga_viral <-""
    } else {
      if(carga_viral==99) #Indectectavel/qualitativa
      {
        obs_carga_viral <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                                   "\"concept\":\"", concept_fc_hivload_qualitativa,"\"," ,
                                   "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                   "\"value\":\"", value_coded_hivload_nivel_baixo,"\"" ,
                                   "}")
        
      } else {  # Numerica
        
        obs_carga_viral <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                                   "\"concept\":\"", concept_fc_carga_viral,"\"," ,
                                   "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                   "\"value\":\"", carga_viral,"\"" ,
                                   "}")
      }
    
    }
    # CD4
    cd4_nr         <- pat$lccd4[index]
    if(is.na(cd4_nr) | cd4_nr ==""){
      obs_cd4 <-""
    } else {
      
      obs_cd4  <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                          "\"concept\":\"", concept_fc_cd4,"\"," ,
                          "\"obsDatetime\":\"", obs_date_time,"\"," ,
                          "\"value\":\"", cd4_nr,"\"" ,
                          "}")
      
    }
    #regimet
    regime         <- pat$arv[index]
    if(is.na(regime) |   regime ==""){
      obs_regime <- ""
    }  else {
      regime <- openmrsGetRegimeUuid(regime)
      obs_regime <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                            "\"concept\":\"", concept_fc_regime,"\"," ,
                            "\"obsDatetime\":\"", obs_date_time,"\"," ,
                            "\"value\":\"", regime,"\"" ,
                            "}")
    }
    #Hemoglobina
    
    hemoglb <- pat$hemoglb[index]
    if(is.na(hemoglb) | hemoglb ==""){
      obs_hemoglb <-""
    } else {
      
      obs_hemoglb  <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                          "\"concept\":\"", concept_fc_hemogl,"\"," ,
                          "\"obsDatetime\":\"", obs_date_time,"\"," ,
                          "\"value\":\"", hemoglb,"\"" ,
                          "}")
      
    }
    #ALT
    
    alt <- pat$alat[index]
    if(is.na(alt) | alt ==""){
      obs_alt <-""
    } else {
      
      obs_alt  <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                              "\"concept\":\"", concept_fc_alt,"\"," ,
                              "\"obsDatetime\":\"", obs_date_time,"\"," ,
                              "\"value\":\"", alt,"\"" ,
                              "}")
      
    }
    
    #Creatinina
    creat <- pat$creatui[index]
    if(is.na(creat) | creat ==""){
      obs_creat <-""
    } else {
      
      obs_creat  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                "\"concept\":\"", concept_fc_creat,"\"," ,
                                "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                "\"value\":\"", creat,"\"" ,
                              "}")
    }
    
    # Linhas terapeuticas
    # 
      tmp_linha <-checkLinhaTerapeutica(pat$nid[index])
      if(length(t)>1){ # primeira linha
      
          obs_linhat  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                             "\"concept\":\"", concept_fc_linhat,"\"," ,
                             "\"obsDatetime\":\"", obs_date_time,"\"," ,
                             "\"value\":\"", tmp_linha,"\"" ,
                             "}")
      }
      else{
        
        
        if(as.Date(tmp_linha[2]) < as.Date(visit_date)){
          
          obs_linhat  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                "\"concept\":\"", concept_fc_linhat,"\"," ,
                                "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                "\"value\":\"", value_coded_primeira_linha,"\"" ,
                                "}")
          
        } else {
          
          obs_linhat  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                "\"concept\":\"", concept_fc_linhat,"\"," ,
                                "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                "\"value\":\"", tmp_linha[1],"\"" ,
                                "}")
          
        }
      }
      
  # TB info
     tratamento_tb <- checkTuberculoseInfo(pat.nid,visit_date,next_visit_date)
     if(is.na(tratamento_tb) ){
      obs_tratamento_tb <- ""
       
     } else {
       
       obs_tratamento_tb  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                            "\"concept\":\"", concept_fc_tratamento_tb,"\"," ,
                            "\"obsDatetime\":\"", obs_date_time,"\"," ,
                            "\"value\":\"", tratamento_tb,"\"" ,
                            "}")
       
     }
      
    # these encounter details and obs are never empty 

     if(age <15){
       encounter_details_ped <- paste0( "\"encounterDatetime\":\"",encounter_date_datime, "\" ," ,
                                        "\"patient\":\"",          patient , "\" ," ,
                                        "\"form\":\"",             form_ficha_clinica , "\" ," ,
                                        "\"encounterType\":\"",    encounter_type_ficha_clinica_ped,"\" , " ,  
                                        "\"location\":\"",         default_location, "\", " ,
                                        "\"encounterProviders\":[{ \"provider\":\"", generic_provider  ,"\"," ,
                                        "\"encounterRole\":\"" , encounter_provider_role,"\"" , "}] ,",
                                        "\"obs\":[ { \"person\":\"",  patient  ,"\"," ,
                                        "\"concept\":\"", concept_fc_consul_prox,"\"," ,
                                        "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                        "\"value\":\"", next_visit_date,"\"" ,
                                        "}" )
       
       
       
       joined_encounter_obs <- paste0(encounter_details_ped,obs_carga_viral,obs_cd4,obs_estadio,
                                      obs_hemoglb,obs_creat,obs_alt,obs_prof_ctz,obs_prof_ihn,obs_weight,obs_linhat,obs_tratamento_tb,obs_height, obs_regime)
       
       encounter <- paste0("{ ", joined_encounter_obs, " ] }")
       encounter
       
     } else {
       encounter_details_adult <- paste0( "\"encounterDatetime\":\"",encounter_date_datime, "\" ," ,
                                        "\"patient\":\"",          patient , "\" ," ,
                                        "\"form\":\"",             form_ficha_clinica , "\" ," ,
                                        "\"encounterType\":\"",    encounter_type_ficha_clinica_adulto,"\" , " ,  
                                        "\"location\":\"",         default_location, "\", " ,
                                        "\"encounterProviders\":[{ \"provider\":\"", generic_provider  ,"\"," ,
                                        "\"encounterRole\":\"" , encounter_provider_role,"\"" , "}] ,",
                                        "\"obs\":[ { \"person\":\"",  patient  ,"\"," ,
                                        "\"concept\":\"", concept_fc_consul_prox,"\"," ,
                                        "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                        "\"value\":\"", next_visit_date,"\"" ,
                                        "}" )
       
       
       
       joined_encounter_obs <- paste0(encounter_details_adult,obs_carga_viral,obs_cd4,obs_estadio,
                                      obs_hemoglb,obs_creat,obs_alt,obs_prof_ctz,obs_prof_ihn,obs_weight,obs_linhat,obs_tratamento_tb,obs_height, obs_regime)
       
       encounter <- paste0("{ ", joined_encounter_obs, " ] }")
       encounter
       
     }
  }
  
}
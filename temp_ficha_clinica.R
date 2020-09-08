#' composeFichaClinica ->  criar uma string json com informacao para criar a ficha clinica de um paciente no openmrs
#' @param   pat.nid  nid do cram
#' @examples 
#' ficha_resumo  <- composeFichaResumo(df,1234)
#' 


composeFichaResumo <- function(df.visits,pat.nid,openmrs.pat.uuid) {
  
  pat <- filter(df.visits, nid==pat.nid) %>% arrange(datvisit)
  
  
  if(nrow(pat)!=0){
    
    #visit_atributes
    visit_date       <- pat$datvisit[i]
    next_visit_date  <- pat$datnext[i]
    patient        <- openmrs.pat.uuid
    #encounter_date_datime <- paste0(visit_date," 11:37:31" )
    encounter_date_datime <- visit_date
    obs_date_time <- visit_date
    age <- pat$agev[i]      
    
    #Whostage
    #TODO estadio OMS: 0 - unknown
    if(i==1){
      estadio_oms <- pat$whof[i]
    } else {
      
      estadio_oms <- pat$whop[i]
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
    weight <- pat$weight[i]
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
    height <- pat$height[i]
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
    prof_ihn <- pat$inh[i]
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
    prof_ctz<- pat$cotri[i]
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
    carga_viral    <- pat$hivload[i]
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
    cd4_nr         <- pat$lccd4[i]
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
    regime         <- pat$arv[i]
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
    
    hemoglb <- pat$hemoglb[i]
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
    
    alt <- pat$alat[i]
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
    creat <- pat$creatui[i]
    if(is.na(creat) | creat ==""){
      obs_creat <-""
    } else {
      
      obs_creat  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                "\"concept\":\"", concept_fc_creat,"\"," ,
                                "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                "\"value\":\"", creat,"\"" ,
                              "}")
    }
    
    # these encounter details and obs are never empty 
    
    if(age < 15) {
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
                                     obs_hemoglb,obs_creat,obs_alt,obs_prof_ctz,obs_prof_ihn,obs_weight,obs_height, obs_regime)
      
      encounter <- paste0("{ ", joined_encounter_obs, " ] }")
      return(encounter)
      
      
      
    }
    
    
  } else {
    return(NA)
  }
  
  
}


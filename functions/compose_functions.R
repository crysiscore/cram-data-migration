
#' composePatient ->  criar uma string json com informacao para cirar um paciente no openmrs
#' @param   df  datafram com info dos pacientes
#' @param   index  row number
#' @examples 
#' patient  <- composePatient(df_cram,1)
#' 

composePatient <- function(df, index){
  #********************************************* Names
  given_name <- df$given_name[index]
  middle_name <- df$middle_name[index]
  family_name <- df$family_name[index]
  array_names <-""
  if(is.na(family_name)){ # middle name is also empty
    
    array_names <- paste0(" \"names\":[{\"givenName\": ",  "\"" ,  given_name,  "\" }],")
    display = given_name
  } else if(is.na(middle_name)){
    
    array_names <- paste0(" \"names\":[{\"givenName\": ",  "\"" ,  given_name,  "\"" ,",\"familyName\":\"",family_name, "\""," }],")
    display =paste0(given_name, " ", family_name)
  } else {
    
    array_names <- paste0(" \"names\":[{\"givenName\": ",  "\"" ,  given_name,  "\"" ,",\"familyName\":\"",family_name, "\"", ",\"middleName\":\"",middle_name, "\""," }],")
    display =paste0(given_name, " ", middle_name , " ",family_name)
  }
  #********************************************* Addresses
  country="Moçambique"
  stateProvince <- "Maputo"
  address5 <- patient_admissions$bairro[index]
  addresses <- NA
  if(!is.na(address5)){
    
    address5 <-iconv(address5, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    addresses <- paste0(" \"addresses\":[{  \"address5\":\""     , address5,"\"," ,
                        "\"country\":\""      , country,"\"," ,
                        "\"stateProvince\":\"", stateProvince,"\"",
                        "}]" ) 
    
  } else {
    
    addresses <- paste0(" \"addresses\":[{ \"country\":\""      , country,"\"," ,
                        "\"stateProvince\":\"", stateProvince,"\"",
                        "}]" ) 
    
  }
  
  
  #****************************** other attributes
  uuid <- patient_admissions$uuid[index]
  gender <- patient_admissions$sexo[index]
  #age <- patient_admissions$idade[index] ( we have datbirth var)
  birth_date <- as.character(patient_admissions$datbirth[index])
  nid_cram <-patient_admissions$nid[index]
  telephone <- patient_admissions$telefone[index]

  
  if(!is.na(telephone)){
    attributes <- paste0("\"attributes\": [{\"attributeType\":\"",attribute_telephone_uuid ,"\"," ,
                         "\"value\":\"", telephone, "\"" , " }]" )   
    
    person <- paste0(" \"person\": { \"uuid\":\"", uuid,"\"," ,
                     "\"birthdate\":\"", birth_date,"\" ," ,
                     "\"gender\":\"", gender,"\" ," ,
                     array_names,
                     addresses, ",",
                     attributes,
                     "} }" ) 
    
    
    #TODO - mudar nidcram por patient_id 
    pat<- paste0( "{\"display\":\"",nid_cram ," - ",display,"\"," ,
                  "\"identifiers\": [{\"identifier\": \"",    nid_cram,"\",",
                  "\"identifierType\":\"",nid_cram_identifier_type,"\"," ,  
                  "\"location\":\"",      default_location,"\"," ,
                  "\"preferred\": true", "}]," ) 
    
  } else {
    
    
    person <- paste0(" \"person\": { \"uuid\":\"", uuid,"\"," ,
                     "\"birthdate\":\"", birth_date,"\" ," ,
                     "\"gender\":\"", gender,"\" ," ,
                     array_names,
                     addresses,
                     "} }" ) 
    
    
    #TODO - mudar nidcram por patient_id 
    pat<- paste0( "{\"display\":\"",nid_cram ," - ",display,"\"," ,
                  "\"identifiers\": [{\"identifier\": \"",    nid_cram,"\",",
                  "\"identifierType\":\"",nid_cram_identifier_type,"\"," ,  
                  "\"location\":\"",      default_location,"\"," ,
                  "\"preferred\": true", "}]," ) 
    
    
  }
  
  
  json_patient <- str_c(pat,person)
  
  json_patient
  
}




#' composeFichaResumo ->  criar uma string json com informacao para criar a ficha resumo de um paciente no openmrs
#' @param   pat.nid  nid do cram
#' @examples 
#' ficha_resumo  <- composeFichaResumo(df,1234)
#' 


composeFichaResumo <- function(df.visits,pat.nid,openmrs.pat.uuid) {
  
  pat <- filter(df.visits, nid==pat.nid) %>% arrange(datvisit)
  if(nrow(pat)!=0){
    
    #visit_atributes
    visit_date <- pat$datvisit[1]
    patient <- openmrs.pat.uuid
    data_teste          <- pat$hivdate[1]
    data_inicio_tarv    <- pat$hivdate[1]
    #encounter_date_datime <- paste0(visit_date," 11:37:31" )
    encounter_date_datime <- visit_date
    #obs_date_time <- paste0(visit_date,"T11:37:31.000+0000")
    obs_date_time <- visit_date
    
    #Ficha resumo attributes
    data_abertura_ficha <- pat$datvisit[1]
    
    tipo_teste_hiv      <- pat$hivtest[1]
    if(tipo_teste_hiv=="TR"){
      tipo_teste_hiv <- value_coded_tr
    } else {
      tipo_teste_hiv <- value_coded_pcr
    }
    
    profissao           <- pat$prof[1]
    if (profissao=="."){
      obs_prof <- "" #TODO um bloco para tratar valores NA
    } else {
      profissao <-  iconv(profissao, from = 'UTF-8', to = 'ASCII//TRANSLIT')
      obs_prof <-paste0(", { \"person\":\"",  patient  ,"\"," ,
                        "\"concept\":\"", concept_fr_profissao,"\"," ,
                        "\"obsDatetime\":\"", obs_date_time,"\"," ,
                        "\"value\":\"", profissao,"\"" ,
                        "}")
    }
    
    us_proveniencia     <- pat$entry[1]
    if(us_proveniencia=="."){
      
      obs_transferido_with_us_transf <- paste0(   ", { \"person\":\"",  patient  ,"\"," ,
                                                  "\"concept\":\"", concept_fr_transferido_outra_us,"\"," ,
                                                  "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                                  "\"value\":\"", value_coded_yes,"\"" ,
                                                  "}" )   
    } else {
      us_proveniencia <- iconv(us_proveniencia, from = 'UTF-8', to = 'ASCII//TRANSLIT')
      obs_transferido_with_us_transf <- paste0(   ", { \"person\":\"",  patient  ,"\"," ,
                                                  "\"concept\":\"", concept_fr_transferido_outra_us,"\"," ,
                                                  "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                                  "\"value\":\"", value_coded_yes,"\"," ,
                                                  "\"comment\":\"", us_proveniencia,"\"" ,
                                                  "}")   
    }
    
    
    #TODO estadio OMS: 0 - unknown
    estadio_oms <- pat$whop[1]
    if (estadio_oms==0){
      obs_estadio <- ""
    } else if(estadio_oms==1){
      estadio_oms <- 'e1d9055e-1d5f-11e0-b929-000c29ad1d07'
      obs_estadio <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                             "\"concept\":\"",concept_fr_estadio ,"\"," ,
                             "\"obsDatetime\":\"", obs_date_time,"\"," ,
                             "\"value\":\"", estadio_oms,"\"" ,
                             "}")
      
    } else if(estadio_oms==2){
      estadio_oms <- 'e1d9066c-1d5f-11e0-b929-000c29ad1d07'
      obs_estadio <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                             "\"concept\":\"",concept_fr_estadio ,"\"," ,
                             "\"obsDatetime\":\"", obs_date_time,"\"," ,
                             "\"value\":\"", estadio_oms,"\"" ,
                             "}")
      
    } else if(estadio_oms==3){
      estadio_oms <- 'e1d9077a-1d5f-11e0-b929-000c29ad1d07'
      obs_estadio <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                             "\"concept\":\"",concept_fr_estadio ,"\"," ,
                             "\"obsDatetime\":\"", obs_date_time,"\"," ,
                             "\"value\":\"", estadio_oms,"\"" ,
                             "}")
    } else if(estadio_oms==4){
      estadio_oms <- 'e1d90888-1d5f-11e0-b929-000c29ad1d07'
      obs_estadio <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                             "\"concept\":\"",concept_fr_estadio ,"\"," ,
                             "\"obsDatetime\":\"", obs_date_time,"\"," ,
                             "\"value\":\"", estadio_oms,"\"" ,
                             "}")
    } else {
      obs_estadio <- ""
    }
    
    
    cd4_nr         <- pat$lccd4[1]
    if(is.na(cd4_nr) | cd4_nr ==""){
      obs_cd4 <-""
    } else {
      
      obs_cd4  <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                          "\"concept\":\"", concept_fr_cd4,"\"," ,
                          "\"obsDatetime\":\"", obs_date_time,"\"," ,
                          "\"value\":\"", cd4_nr,"\"" ,
                          "}")
      
    }
    
    
    carga_viral    <- pat$hivload[1]
    if(is.na(carga_viral) | carga_viral==""){
      obs_carga_viral <-""
    } else {
      
      obs_carga_viral <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                                 "\"concept\":\"", concept_fr_carga_viral,"\"," ,
                                 "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                 "\"value\":\"", carga_viral,"\"" ,
                                 "}")
    }
    
    regime         <- pat$arv[1]
    if(is.na(regime) |   regime ==""){
      
      
      obs_regime <- ""
      
    }  else {
      regime <- openmrsGetRegimeUuid(regime)
      obs_regime <- paste0( ", { \"person\":\"",  patient  ,"\"," ,
                            "\"concept\":\"", concept_fr_regime,"\"," ,
                            "\"obsDatetime\":\"", obs_date_time,"\"," ,
                            "\"value\":\"", regime,"\"" ,
                            "}")
    }
    
    
    
    
    
    

    # these encounter details and obs are never empty
    encounter_details <- paste0( "\"encounterDatetime\":\"",encounter_date_datime, "\" ," ,
                                 "\"patient\":\"",          patient , "\" ," ,
                                 "\"form\":\"",             form_ficha_resumo , "\" ," ,
                                 "\"encounterType\":\"",    encounter_type_ficha_resumo,"\" , " ,  
                                 "\"location\":\"",         default_location, "\", " ,
                                 "\"encounterProviders\":[{ \"provider\":\"", generic_provider  ,"\"," ,
                                 "\"encounterRole\":\"" , encounter_provider_role,"\"" , "}] ,",
                                 "\"obs\":[ { \"person\":\"",  patient  ,"\"," ,
                                 "\"concept\":\"", concept_fr_data_abertura_ficha,"\"," ,
                                 "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                 "\"value\":\"", data_abertura_ficha,"\"" ,
                                 "}", ",",  
                                 "{  \"person\":\"",  patient  ,"\"," ,
                                 "\"concept\":\"", concept_fr_tipo_teste_hiv,"\"," ,
                                 "\"obsDatetime\":\"", data_teste,"\"," ,
                                 "\"value\":\"", tipo_teste_hiv,"\"" ,
                                 "}", "," ,
                                 "{  \"person\":\"",  patient  ,"\"," ,
                                 "\"concept\":\"", concept_fr_em_tarv,"\"," ,
                                 "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                 "\"value\":\"", value_coded_em_tarv,"\"" ,
                                 "}", "," ,
                                 "{  \"person\":\"",  patient  ,"\"," ,
                                 "\"concept\":\"", concept_fr_data_inicio_tarv,"\"," ,
                                 "\"obsDatetime\":\"", obs_date_time, "\"," ,
                                 "\"value\":\"", data_inicio_tarv,"\"" ,
                                 "}" )
    
    
    
    joined_encounter_obs <- paste0(encounter_details,obs_carga_viral,obs_cd4,obs_estadio,obs_prof,obs_regime ,obs_transferido_with_us_transf)
    
    encounter <- paste0("{ ", joined_encounter_obs, " ] }")
    return(encounter)
    
    
  } else {
    return(NA)
  }
 
  
  }




#' composeFichaClinica ->  criar uma string json com informacao para criar a ficha clinica de um paciente no openmrs
#' @param   df.visits  visita
#' @param  pat.uuid uuid do paciente no openmrs
#' @examples 
#' ficha_resumo  <- composeFichaClinica(df,123e2354-3245-2325d-23sdvdfs3)
#' 


composeFichaClinica<- function(df.visits,openmrs.pat.uuid) {
  
  index=1 # 1 line = 1 visit
  pat <- df.visits
  pat.nid <- df.visits$nid[index]
  
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
    tmp_linha <- checkLinhaTerapeutica(pat$nid[index])
    if(length(tmp_linha)==1){ # primeira linha
      
      obs_linhat  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                            "\"concept\":\"", concept_fc_linhat,"\"," ,
                            "\"obsDatetime\":\"", obs_date_time,"\"," ,
                            "\"value\":\"", tmp_linha,"\"" ,
                            "}")
    } else if (is.na(tmp_linha[2])){
      
      obs_linhat  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                            "\"concept\":\"", concept_fc_linhat,"\"," ,
                            "\"obsDatetime\":\"", obs_date_time,"\"," ,
                            "\"value\":\"", tmp_linha[1],"\"" ,
                            "}")
      
    }  else if(as.Date(tmp_linha[2]) < as.Date(visit_date)){
      
      obs_linhat  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                            "\"concept\":\"", concept_fc_linhat,"\"," ,
                            "\"obsDatetime\":\"", obs_date_time,"\"," ,
                            "\"value\":\"", value_coded_primeira_linha,"\"" ,
                            "}")
      
    }   else {
      
      obs_linhat  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                            "\"concept\":\"", concept_fc_linhat,"\"," ,
                            "\"obsDatetime\":\"", obs_date_time,"\"," ,
                            "\"value\":\"", tmp_linha[1],"\"" ,
                            "}")
      
    }
    
    
    # TB estado (Inicio/Continua/Fim)
    
    tratamento_tb <- checkTuberculoseInfo(pat.nid,visit_date,next_visit_date)
    
    if(is.na(tratamento_tb) ){
      obs_tratamento_tb <- ""
      
    }
    else {
      
      obs_tratamento_tb  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                   "\"concept\":\"", concept_fc_tratamento_tb,"\"," ,
                                   "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                   "\"value\":\"", tratamento_tb,"\"" ,
                                   "}")
      
    }
    
    # TB LAM
    lab_tb_lam <- check_lab_tb_lam(pat.nid,visit_date,next_visit_date)
    if(is.na(lab_tb_lam) ){
      obs_lab_tb_lam <- ""
      
    } 
    else {
      
      obs_lab_tb_lam  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                "\"concept\":\"", concept_fc_lab_lam,"\"," ,
                                "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                "\"value\":\"", lab_tb_lam,"\"" ,
                                "}")
      
    }
    
    # TB CRAG
    lab_tb_crag <- check_lab_tb_crag(pat.nid,visit_date,next_visit_date)
    if(is.na(lab_tb_crag) ){
      obs_lab_tb_crag <- ""
      
    }
    else {
      
      obs_lab_tb_crag  <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                 "\"concept\":\"", concept_fc_lab_crag,"\"," ,
                                 "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                 "\"value\":\"", lab_tb_crag,"\"" ,
                                 "}")
      
    }  
    #
    
    
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
                                     obs_hemoglb,obs_creat,obs_alt,obs_prof_ctz,obs_prof_ihn,obs_weight,
                                     obs_linhat,obs_tratamento_tb,
                                     obs_lab_tb_crag,obs_lab_tb_lam,obs_height, obs_regime)
      
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
                                     obs_hemoglb,obs_creat,obs_alt,obs_prof_ctz,obs_prof_ihn,obs_weight,
                                     obs_linhat,obs_tratamento_tb,
                                     obs_lab_tb_crag,obs_lab_tb_lam,obs_height, obs_regime)
      
      encounter <- paste0("{ ", joined_encounter_obs, " ] }")
      encounter
      
    }
  }
  
}




#' composeFila ->  criar uma string json com informacao para criar a fila de um paciente no openmrs
#' @param   df.visits  visita
#' @param  pat.uuid uuid do paciente no openmrs
#' @examples 
#' fila  <- composeFila(df,123e2354-3245-2325d-23sdvdfs3)
#' 


composeFila<- function(df.visits,openmrs.pat.uuid) {
  
  index=1 # 1 line = 1 visit
  pat <- df.visits
  
  #visit_atributes
  visit_date       <- pat$datvisit[index]
  next_visit_date  <- pat$datnext[index]
  
  encounter_date_datime <- visit_date
  #openmrs.pat.uuid = created_patients$openmrs_status[which(created_patients$nid==23465)]
  nidUuid = openmrs.pat.uuid
  
  #regimet
  regime         <- pat$arv[index]
  if(is.na(regime) |   regime ==""){
    obs_regime <- ""
  }  else {
    regime <- openmrsGetRegimeUuid(regime)
    
  }
  
  #encounter_date_datime <- paste0(visit_date," 11:37:31" )
  encounter_date_datime <- visit_date
  obs_date_time <- visit_date
  
  obs_next_pickup_date <- paste0( ", { \"person\":\"",  nidUuid  ,"\"," ,
                                  "\"concept\":\"", returnVisitUuid,"\"," ,
                                  "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                  "\"comment\":\"CRAM\" ," ,
                                  "\"value\":\"", next_visit_date,"\"" ,
                                  "}")
  
  
  
  encounter_details <- paste0( "\"encounterDatetime\":\"",encounter_date_datime, "\" ," ,
                               "\"patient\":\"",          nidUuid , "\" ," ,
                               "\"form\":\"",             form_fila_uuid , "\" ," ,
                               "\"encounterType\":\"",    encounter_type_fila,"\" , " ,  
                               "\"location\":\"",         default_location, "\", " ,
                               "\"encounterProviders\":[{ \"provider\":\"", generic_provider  ,"\"," ,
                               "\"encounterRole\":\"" , encounter_provider_role,"\"" , "}] ,",
                               "\"obs\":[ { \"person\":\"",  nidUuid  ,"\"," ,
                               "\"concept\":\"", concept_fila_regime,"\"," ,
                               "\"obsDatetime\":\"", obs_date_time,"\"," ,
                               "\"comment\":\"CRAM\" ," ,
                               "\"value\":\"", regime,"\"" ,
                               "}" )
  
  joined_encounter_obs <- paste0(encounter_details,obs_next_pickup_date)
  
  encounter <- paste0("{ ", joined_encounter_obs, " ] }")
  return(encounter)
  
}



composeLab <- function(patient.uuid,df.lab ){
  
  index=1 
  patient <- patient.uuid
  
  if(nrow(df.lab)>0){
    
    # there are only 27 listed lab tests in the file
    for ( i in 1:27){
      
      examen       <- df.lab[[paste0("examen",i)]][index]
      
      if(!is.na(examen)) {  # no more lab requests
        
        encounter_datetime <- as.character(as.Date(examen))
        
        alat     <- df.lab[[paste0("alat",i)]][index]      # Alanina Aminotransferase 
        hbsag    <- df.lab[[paste0("hbsag",i)]][index]    # Hepatites b antigen s
        creatui  <- df.lab[[paste0("creatui",i)]][index]  # Creatinine umol/L
        lc       <- df.lab[[paste0("lc",i)]][index]       # Linfocitos
        lccd4    <- df.lab[[paste0("lccd4",i)]][index]    # Cd4 Numerico
        lccd4tlc <- df.lab[[paste0("lccd4tlc",i)]][index] # Cd4 Percentual
        hivload  <- df.lab[[paste0("hivload",i)]][index]  # Carga viral
        hemoglb  <- df.lab[[paste0("hemoglb",i)]][index]  # Hemoglobina
        
        if(!is.na(alat)){
          
          obs_alat <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                             "\"concept\":\"", concept_lab_alat,"\"," ,
                             "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                             "\"value\":\"", alat,"\"" ,
                             "}")
        } else {
          
          obs_alat <- ""
        }
        
        if(!is.na(creatui)){
          
          obs_creatui <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                "\"concept\":\"", concept_lab_creatinine,"\"," ,
                                "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                "\"value\":\"", creatui,"\"" ,
                                "}")
        } else {
          
          obs_creatui <- ""
        }
        
        
        if(!is.na(lc) ){
          obs_linfocitos <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                   "\"concept\":\"", concept_lab_linfocitos,"\"," ,
                                   "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                   "\"value\":\"",lc,"\"" ,
                                   "}")
        } else {
          
          obs_linfocitos <- ""
        }
        
        if(!is.na(lccd4) ){
          
          
          obs_cd4_numeric <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                    "\"concept\":\"", concept_lab_cd4_numeric,"\"," ,
                                    "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                    "\"value\":\"", lccd4,"\"" ,
                                    "}")
        } else {
          
          obs_cd4_numeric <- ""
        }
        
        
        
        if(!is.na(lccd4tlc)  ){
          
          obs_cd4_perc <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                 "\"concept\":\"", concept_lab_cd4_percent,"\"," ,
                                 "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                 "\"value\":\"", lccd4tlc,"\"" ,
                                 "}")
          
        } else {
          
          obs_cd4_perc <- ""
        }
        
        
        
        
        if(!is.na(hivload) ){
          
          
          obs_hiv_load <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                 "\"concept\":\"", concept_lab_carga_viral,"\"," ,
                                 "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                 "\"value\":\"", hivload,"\"" ,
                                 "}")
          
        } else {
          
          obs_hiv_load <- ""
        }
        
        
        if(!is.na(hemoglb) ){
          
          
          obs_hemoglb <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                "\"concept\":\"", concept_lab_homogl,"\"," ,
                                "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                "\"value\":\"", hemoglb,"\"" ,
                                "}")
          
          
        } else {
          
          obs_hemoglb <- ""
          
        }
        
        # these encounter details and obs are never empty
        encounter_details <- paste0( "\"encounterDatetime\":\"",encounter_datetime, "\" ," ,
                                     "\"patient\":\"",          patient , "\" ," ,
                                     "\"form\":\"",             form_lab , "\" ," ,
                                     "\"encounterType\":\"",    encounter_type_lab,"\" , " ,  
                                     "\"location\":\"",         default_location, "\", " ,
                                     "\"encounterProviders\":[{ \"provider\":\"", generic_provider  ,"\"," ,
                                     "\"encounterRole\":\"" , encounter_provider_role,"\"" , "}] ,",
                                     "\"obs\":[ { \"person\":\"",  patient  ,"\"," ,
                                     "\"concept\":\"", concept_lab_data_exame,"\"," ,
                                     "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                     "\"value\":\"", examen,"\"" ,
                                     "}" ) 
        
        
        joined_encounter_obs <- paste0(encounter_details,obs_alat,obs_creatui,obs_linfocitos,obs_cd4_numeric,obs_cd4_perc ,obs_hiv_load,obs_hemoglb)
        encounter <- paste0("{ ", joined_encounter_obs, " ] }")
        print(encounter)
        
      }
      
      
    }
    
  }
  
  
}


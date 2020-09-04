
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
    
    
    
    
    
    
    #encounter_date_datime <- paste0(visit_date," 11:37:31" )
    encounter_date_datime <- visit_date
    #obs_date_time <- paste0(visit_date,"T11:37:31.000+0000")
    obs_date_time <- visit_date
    
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





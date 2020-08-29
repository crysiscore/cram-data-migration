library(dplyr)
library(plyr)
library(stringi)
library(tibble)
library(properties)
library(httr)
library(stringr)
library(uuid)



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
     age <- patient_admissions$idade[index]
     nid_cram <-patient_admissions$nid[index]
     nid_cram_identifier_type="8746cc9e-b2d3-4b0b-83a6-580e868e373f"
     default_location ="0dc2d9c3-91ff-4a87-b2d1-84d2955bd9cb"
     
      person <- paste0(" \"person\": { \"uuid\":\"", uuid,"\"," ,
                                     "\"age\":", age," ," ,  
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
                      
    json_patient <- str_c(pat,person)
   
    json_patient
  
}



#' ReadJdbcProperties ->  carrega os paramentros de conexao no ficheiro jdbc.properties
#' @param file  path to file
#' @return  vec [urlBase,urlBaseReportingRest,location,hibernateOpenMRSConnectionUrl,hibernateOpenMRSPassword,hibernateOpenMRSUsername]
#' @examples 
#' properties  <- readJdbcProperties()
#' 

readJdbcProperties <- function(file='config/jdbc.properties') {

  vec <- as.data.frame(read.properties(file = file ))
  return(vec)
}

#' checkPatientUuidExistsOpenMRS ->  verifica se existe um paciente no openmrs com um det uuid
#' @param jdbc.properties  [urlBase,urlBaseReportingRest,location,hibernateOpenMRSConnectionUrl,hibernateOpenMRSPassword,hibernateOpenMRSUsername]
#' @param patient.uuid patient.uuid 
#' @return  TRUE/FALSE 
#' @examples 
#' status  <- checkPatientUuidExistsOpenMRS(jdbc.properties, "cb90174b-81e9-43e4-9b4d-dc09d2966efa")
#' 


checkPatientUuidExistsOpenMRS <- function(patient.uuid) {
  
  jdbc.properties <- readJdbcProperties()
  
  # url da API
  base.url.rest <- as.character(jdbc.properties$urlBaseReportingRest)
  base.url      <-  as.character(jdbc.properties$urlBase)
  status        <- TRUE
  
  url.check.patient <- paste0(base.url,'patient/',patient.uuid)
  
  status <- content(GET(url.check.patient, authenticate('admin', 'eSaude123')), as = "parsed")
  
  if("error" %in% names(status)){
    if(status$error$message =="Object with given uuid doesn't exist" ){
     print("Object with given uuid doesn't exist" )
    }

    return(FALSE)
    
  } else{
    
    return(TRUE)
    
  }

}


#' apiCreateOpenmrsPatient() -> cria um paciente no openmrs usando a restAPI
#' @param file  patg to file
#' @return  vec [urlBase,urlBaseReportingRest,location,hibernateOpenMRSConnectionUrl,hibernateOpenMRSPassword,hibernateOpenMRSUsername]
#' @examples 
#' user_admin  <- ReadJdbcProperties(file)
#' 

apiCreateOpenmrsPatient <- function(patient){
  
  #API connection properties
  jdbc.properties <- readJdbcProperties()
 
   # url da API
  base.url <-  as.character(jdbc.properties$urlBase)
  base.url <- str_c(base.url,'patient')

  # send patient to openmrs
  status <- POST(url = base.url, body = patient, config=authenticate('admin', 'eSaude123'),  add_headers("Content-Type"="application/json") )

  if(as.integer(status$status_code)==201 | as.integer(status$status_code) == 204) {
    print('Patient created')
    return(TRUE)
  } else {
    return(FALSE)
  }
}




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
#' @param json.ficharesumo  json formated string 
#' @return  vec [urlBase,urlBaseReportingRest,location,hibernateOpenMRSConnectionUrl,hibernateOpenMRSPassword,hibernateOpenMRSUsername]
#' @examples 
#' user_admin  <- ReadJdbcProperties(file)
#' 

apiCreateOpenmrsPatient <- function(json.patient){
  
  #API connection properties
  jdbc.properties <- readJdbcProperties()
 
   # url da API
  base.url <-  as.character(jdbc.properties$urlBase)
  base.url <- str_c(base.url,'patient')

  # send patient to openmrs
  status <- POST(url = base.url, body = json.patient, config=authenticate('admin', 'eSaude123'),  add_headers("Content-Type"="application/json") )
  status
 
}


#' apiCreateOpenmrsFichaResumo() -> cria ficha resumo de um paciente no openmrs usando a restAPI
#' @param json.ficharesumo  json formated string 
#' @return  vec [urlBase,urlBaseReportingRest,location,hibernateOpenMRSConnectionUrl,hibernateOpenMRSPassword,hibernateOpenMRSUsername]
#' @examples 
#' user_admin  <- ReadJdbcProperties(file)
#' 

apiCreateOpenmrsFichaResumo <- function(json.ficharesumo){
  
  #API connection properties
  jdbc.properties <- readJdbcProperties()
  
  # url da API
  base.url <-  as.character(jdbc.properties$urlBase)
  base.url <- str_c(base.url,'encounter')
  
  # send patient to openmrs
  status <- POST(url = base.url, body = json.ficharesumo, config=authenticate('admin', 'eSaude123'),  add_headers("Content-Type"="application/json") )
  status
  
}




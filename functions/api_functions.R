
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
  
  status <- content(GET(url.check.patient, authenticate(migration_user, migration_passwd)), as = "parsed")
  
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
#' @return   json status object
#' @examples 
#' status  <- apiCreateOpenmrsPatient(json.string)
#' 

apiCreateOpenmrsPatient <- function(json.patient){
  
  #API connection properties
  jdbc.properties <- readJdbcProperties()
 
   # url da API
  base.url <-  as.character(jdbc.properties$urlBase)
  base.url <- str_c(base.url,'patient')

  # send patient to openmrs
  status <- POST(url = base.url, body = json.patient, config=authenticate(migration_user, migration_passwd),  add_headers("Content-Type"="application/json") )
  status
 
}


#' apiCreateOpenmrsFichaResumo() -> cria ficha resumo de um paciente no openmrs usando a restAPI
#' @param json.ficharesumo  json formated string 
#' @return  json status object
#' @examples 
#' status  <- apiCreateOpenmrsFichaResumo(json.string)
#' 

apiCreateOpenmrsFichaResumo <- function(json.ficharesumo){
  
  #API connection properties
  jdbc.properties <- readJdbcProperties()
  
  # url da API
  base.url <-  as.character(jdbc.properties$urlBase)
  base.url <- str_c(base.url,'encounter')
  
  # send patient to openmrs
  status <- POST(url = base.url, body = json.ficharesumo, config=authenticate(migration_user, migration_passwd),  add_headers("Content-Type"="application/json") )
  status
  
}


#' apiCreateOpenmrsFichaClinica() -> cria ficha clinica de um paciente no openmrs usando a restAPI
#' @param json.ficha.clinica  json formated string 
#' @return   json status object
#' @examples 
#' status  <- apiCreateOpenmrsFichaClinica(json.string)
#' 

apiCreateOpenmrsFichaClinica <- function(json.ficha.clinica){
  
  #API connection properties
  jdbc.properties <- readJdbcProperties()
  
  # url da API
  base.url <-  as.character(jdbc.properties$urlBase)
  base.url <- str_c(base.url,'encounter')
  
  # send patient to openmrs
  status <- POST(url = base.url, body = json.ficha.clinica, config=authenticate(migration_user, migration_passwd),  add_headers("Content-Type"="application/json") )
  status
  
}



#' json.fila() -> cria fila de um paciente no openmrs usando a restAPI
#' @param json.fila json formated string 
#' @return   json status object
#' @examples 
#' status  <- apiCreateOpenmrsFila(json.string)
#' 

apiCreateOpenmrsFila <- function(json.fila){
  
  #API connection properties
  jdbc.properties <- readJdbcProperties()
  
  # url da API
  base.url <-  as.character(jdbc.properties$urlBase)
  base.url <- str_c(base.url,'encounter')
  
  # send patient to openmrs
  status <- POST(url = base.url,
                 body = json.fila, config=authenticate(migration_user, migration_passwd),
                 add_headers("Content-Type"="application/json") )
  status
  
}



apiCreateProgramEnrollment <- function(json.enrollment){
  

  #API connection properties
  jdbc.properties <- readJdbcProperties()
  
  # url da API
  base.url <-  as.character(jdbc.properties$urlBase)
  base.url <- str_c(base.url,'programenrollment')
  
  # send patient to openmrs
  status <- POST(url = base.url,
                 body = json.enrollment, config=authenticate(migration_user, migration_passwd),
                 add_headers("Content-Type"="application/json") )
  content(status)
  
}




#' apiCreateOpenmrsLab() -> cria ficha de lab de um paciente no openmrs usando a restAPI
#' @param json.lab formated string 
#' @return   json status object
#' @examples 
#' status  <- apiCreateOpenmrsLab(json.lab)
#' 

apiCreateOpenmrsLab <- function(json.lab){
  
  #API connection properties
  jdbc.properties <- readJdbcProperties()
  
  # url da API
  base.url <-  as.character(jdbc.properties$urlBase)
  base.url <- str_c(base.url,'encounter')
  
  # send patient to openmrs
  status <- POST(url = base.url,
                 body = json.lab, config=authenticate(migration_user, migration_passwd),
                 add_headers("Content-Type"="application/json") )
  status
  
}


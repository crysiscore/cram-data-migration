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


#' not_all_na () -> Drop all NA columns in df
#' 
not_all_na <- function(x) {!all(is.na(x))}


#' openmrsGetRegimeUuid() -> retorna uuid dum regime com base no esquema
#' @param esquema  esquema terapeutico
#' @return  uuid
#' @examples 
#' uuid  <- openmrsGetRegimeUuid("TDF+3TC+EFV")
#' 

openmrsGetRegimeUuid <- function (regt) {
  
  if (regt== "OUTRO" ) {
    arv='e1e59e0e-1d5f-11e0-b929-000c29ad1d07'
  }
  else if (regt=="TDF+3TC+DTG"){
    arv='e3f6bb60-e2cf-46cb-a9da-27d634ba8607'
  }
  else if (regt=="TDF+3TC+ATV/r"){
    arv='7bf5a88d-6db6-4899-a01a-bfd14ce77b53'
  } 
  else if (regt=="TDF+3TC+RAL+DRV/r"){
    arv='46b8a2ab-36a7-4072-ab62-0af9b3b58e72'
  } 
  else if (regt=="TDF+3TC+LPV/r"){
    arv='f808f602-bc43-4070-9390-c2ec3fd0bee2'
  }  
  else if (regt=="ABC+3TC+ATV/r"){
    arv= 'e8b741b3-463c-46b1-8423-a16f736af8d4'
  }   
  else if (regt=="TDF+3TC+EFV"){
    arv='9dc17c1b-7b6d-488e-a38d-505a7b65ec82'
  }      
  else if (regt=="AZT+3TC+NVP"){
    arv='e1dd2f44-1d5f-11e0-b929-000c29ad1d07'
  }  
  else if (regt=="AZT+3TC+DTG"){
    arv='9cb63f72-4c08-4543-878a-537dcabe5670'
  }  
  else if (regt=="TDF+3TC+ATV/r+RAL"){
    arv='c22c4431-a27e-4054-92bb-a1563482003e'
  }
  else if (regt=="AZT+3TC+ABC"){
    arv='3e7f46c7-a971-4c0c-82aa-a65589fd518e'
  }    
  else if (regt=="ABC+3TC+DTG"){
    arv='af15246d-30b8-4aff-8391-ca2b58e2c88b'
  }  
  
  else if (regt=="AZT+3TC+EFV"){
    arv='e1de19fe-1d5f-11e0-b929-000c29ad1d07'
  }      
  else if (regt=="ABC+3TC+EFV"){
    arv='78419317-cdda-42e9-92a3-13cb0cbf0020'
  }   
  else if (regt=="AZT+3TC+LPV/r"){
    arv='28b28521-b6cd-454e-9ec5-f2c6c9c58468'
  } 
  else if (regt=="ABC+3TC+ATV/r+RAL"){
    arv='7d18d08c-d1a8-4dc8-8d65-02197b42acf7'
  } 
  else if (regt=="ABC+3TC+LPV/r"){
    arv='cf05347e-063c-4896-91a4-097741cf6be6'
  } 
  else if (regt=="ABC+3TC+NVP"){
    arv='e11be52e-0da1-4d32-ab5c-e0feb9b6abd6'
  } 
  else if (regt=="AZT+3TC+RAL"){
    arv='c4a56680-ac6e-4538-8126-e3097b7b4789'
  } else {
    arv='e1e59e0e-1d5f-11e0-b929-000c29ad1d07'
  } 
  
  return(arv)
}



#' createLogsDataFrame() -> cria um dataframe de logs para cada interaccao com API OpenMRS
#' @param nrows  numero de linhas
#' @return  df
#' @examples 
#' df_patient_logs  <- createLogsDataFrame(nrow(patient_admissions))
#' 

createLogsDataFrame <- function(nrows){
  col_names <- c("nid", "api_status_code", "message", "detail")
  df_patient_logs = as.data.frame(matrix(data = NA,nrow = nrows, ncol = length(col_names)))
  colnames(df_patient_logs) = col_names
  df_patient_logs
}

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
  
  if (regt == "OUTRO" ) {
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
    arv='f8c5d365-7636-4449-9acd-c83c4fd2ea01'
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
  } 
  else if (regt=="AZT+3TC+RAL+DRV/r"){
    arv='c30f826d-9433-4e63-ac91-dbbc158686ff'
  }
  else if (regt=="3TC+RAL+DRV/r"){
    arv='f32e013c-38c8-483a-8012-88596e8b13da'
  }
  else if (regt=="ABC+3TC+RAL+DRV/r"){
    arv='46b8a2ab-36a7-4072-ab62-0af9b3b58e72'
  } 
  else if (regt=="TDF+3TC+RAL+DRV/r"){
    arv='c4a56680-ac6e-4538-8126-e3097b7b4789'
  }
  else if (regt=="AZT+3TC+DRV/r"){
    arv='8f9c4e58-0d70-4b09-a109-e776d325e9fd'
  }
  else if (regt=="AZT+3TC+ATV/r"){
    arv='ba25f2b5-4216-4605-9e6b-1f591033dc3e'
  }
  else if (regt=="TDF+3TC+LPV/r+RTV"){
    arv='d5f9d629-67bc-4f60-9e12-629e6d2c1d08'
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


#' checklinhaTerapeutica() -> Retorna informacao da linha terapeutica
#' @param   pat.nid nid do paciente
#' @return  vector com linha terap e a data de incio da linha: array [linhat,data_inicio]
#' @examples 
#' linhatt  <- checklinhaTerapeutica(11113)
#' 
checkLinhaTerapeutica <- function(pat.nid){
  
   temp_free_var <- filter(patient_free_var,nid==pat.nid)

   var_2_linha <- temp_free_var$varpt1[1]
   var_2_linha_opt <- temp_free_var$varpt2[1]
   var_3_linha <- temp_free_var$varpt3[1]
   
   data_sec_linha <- substr(as.character(temp_free_var$datscnline[1]),start = 1,stop = 10)
   data_sec_linha_opt <-substr(as.character(temp_free_var$datscnlineop[1]),start = 1,stop = 10)
   data_ter_linha <- substr(as.character(temp_free_var$dattrdline[1]),start = 1,stop = 10)
   
   if(!is.na(var_2_linha)) {
     
     return(c(value_coded_segunda_linha,data_sec_linha))
     
   } else if(!is.na(var_2_linha_opt)){
     
       return(c(value_coded_segunda_linha,data_sec_linha))
     
   } else if(!is.na(data_ter_linha)){
     
      return(c(value_coded_terceira_linha,data_ter_linha))
     
   } else {
     
      return(value_coded_primeira_linha)
   }
   
   
   
}




#' checkTuberculoseInfo() -> Retorna informacao de tuberculose
#' @param   pat.nid nid do paciente
#' @return  vector com linha terap e a data de incio da linha: array [linhat,data_inicio]
#' @examples 
#' linhatt  <- checkTuberculoseInfo(11113)
#' 
#' 
checkTuberculoseInfo <- function(pat.nid, data.consult,data.prox.consul){
  
  temp_tb <- filter( tb_patient ,nid==pat.nid) 
  if(nrow(temp_tb)>0) {
    ttfrom  <- temp_tb$tttfrom[1]
    ttto    <- temp_tb$tttto[1]
    outcome <- temp_tb$outcome[1]
    tb_type <- temp_tb$tbtyp[1]
    
    if( outcome=="Completed Treatment"){
      return(NA)
    } else if (!is.na(ttto)) {
      return(NA)
    } else {
      
      if(as.Date(ttfrom) < as.Date("2020-01-21") & tb_type != "Not specified"){ # nao vamos registar (nao se conhece o desfecho)
        return(NA)
      } else if(as.Date(ttfrom) >= as.Date(data.consult) & as.Date(ttfrom) < as.Date(data.prox.consul)  ) {
        return(value_coded_inicia)
      } else if(as.Date(ttfrom) < as.Date(data.consult)){
        return(value_coded_continua)
      } else if(as.Date(ttfrom) > as.Date(data.consult)){
        return(NA)
      }
      
      
    }
    
  } else { return(NA) }

  
}
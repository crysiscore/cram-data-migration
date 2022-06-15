
require(RPostgreSQL)
require(properties)
# iDART Stuff - Configuracoes de variaveis de conexao 
postgres.user ='postgres'
postgres.password='postgres'
postgres.db.name='altomae'
postgres.host='172.18.0.3'
postgres.port=5432
# Objecto de connexao com a bd openmrs postgreSQL
con_local <-  dbConnect(PostgreSQL(),user = postgres.user,password = postgres.password, dbname = postgres.db.name,host = postgres.host)




############################################################################################################################################


#' Busca o nos dados de uma table postgres
#' 
#' @param postgres.con objecto de conexao com mysql    
#' @return df
#' @examples
#' df_regimes = getIdartDrug(postgres.con)
getIdartData <- function (postgres.con, sql.query){
  
  df <- dbGetQuery(postgres.con,sql.query)
  df
}


nids_activos

st <-""

for (r in 1:length(nids_activos)) {
  
  st<- paste0(st,", '", nids_activos[r], "'  ")
  
}

st = paste0('(',substr(st, 3, nchar(st)),')')

patient <- getIdartData(con_local,paste0(' select * from patient where patientid in ',st ," or patientid like '%CRAM%' or patientid like '%cram%';"))




vec_pat <- ""

for (t in 1:nrow(patient)) {
  
  vec_pat<- paste0(vec_pat,", '", patient$patientid[t], "'  ")
  
}



vec_pat = paste0('(',substr(vec_pat, 3, nchar(vec_pat)),')')


query_lev <- paste0(" SELECT pdt.id, patientid, dispensedate::date,      drugname, patientfirstname, packageddrug, patientlastname, dateexpectedstring, 
                       reg.regimeesquema, reg.regimenomeespecificado
                       FROM public.packagedruginfotmp pdt inner join packageddrugs pd on pd.id =pdt.packageddrug
                       inner join package pa on pa.id =pd.parentpackage 
                       inner join prescription p on p.id = pa.prescription inner join
                       regimeterapeutico reg on reg.regimeid=p.regimeid where pdt.patientid in ", vec_pat , 
                    "  order by  dispensedate desc , patientid ;")

levantamentos <- getIdartData(con_local, query_lev)


levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, ' ', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, 'CRAM', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, ' M ', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, ' SONIA', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, '/11AM ', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, '/12 ', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, ' ', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, '/14', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, '110106011/', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, '/14 ', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, '/11AM', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, '/12', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, '/13', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, '/', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, 'M', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, 'PD', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, 'SONIA', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, 'E', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, 'SONIA', '')
levantamentos$patientid <- stringr::str_replace(levantamentos$patientid, ',', '')


nids_criados <- created_patients$nid


tmp = dplyr::filter(levantamentos, patientid %in% nids_criados )

# outro regime
tmp$regimenomeespecificado[which(tmp$regimenomeespecificado=='') ] <-'e1e59e0e-1d5f-11e0-b929-000c29ad1d07'
tmp$dateexpectedstring  <- as.Date(tmp$dateexpectedstring ,   "%d %B %Y")

tmp$uuidopenmrs <- ""

for (it in 1:nrow(tmp) ) {
   
     patid <- tmp$patientid[it]
     
      df_tmp <- filter(created_patients, nid==patid)
      if(nrow(df_tmp)==1){
        tmp$uuidopenmrs[it] <- df_tmp$openmrs_status[1]
      } 
      else if (nrow(df_tmp)==2){
        print(" Nid duplicado")
      }  else { print(" Nid not found")}
}


#Migrate Filas


vec_nids_fila <- unique(tmp$patientid)
df_fila_logs <- createLogsDataFrame(nrow(tmp))
df_fila_logs <- df_fila_logs[1:1,]
df_fila_logs_tmp  <- df_fila_logs


for (var in 1:length(vec_nids_fila)) {
    
   nid <-nids_criados[var]
   df_tmp <- dplyr::filter(tmp, patientid ==nid)
   vec_dates <- unique(df_tmp$dispensedate)
   vec_dates <- sort(vec_dates, decreasing = FALSE)
   patuuid <- df_tmp$uuidopenmrs[1]
   
     for (v in 1:length(vec_dates)) {
         dat <- vec_dates[v]
         df_temp_fila <-  dplyr::filter(df_tmp, dispensedate == dat & patientid==nid )
         if(nrow(df_temp_fila)>0){
            index=1
            vis_date <- df_temp_fila$dispensedate[index]
            next_vist <-  df_temp_fila$dateexpectedstring[index]
            regime <- df_temp_fila$regimenomeespecificado[index]
            
            
            #Cria fila associado a visita
            json_fila <- composeFilaV2(uuid = patuuid,visit.date = vis_date,next.visit.date = next_vist,regimeuuid =  regime)
            status_fila <- apiCreateOpenmrsFila(json_fila)
            
            if(as.integer(status_fila$status_code)==201 | as.integer(status_fila$status_code) == 204){
              df_fila_logs_tmp$api_status_code[1] <- as.integer(status$status_code)
              df_fila_logs_tmp$nid[1] <- nid
              df_fila_logs_tmp$message[1] <- "sucess"
              df_fila_logs <- bind_rows(df_fila_logs_tmp,df_fila_logs)
              
            } else{
              
              content <- content(status_fila)
              df_fila_logs_tmp$api_status_code[1] <- as.integer(status_fila$status_code)
              df_fila_logs_tmp$message[1] <- content$error$message
              if(content$error$message=="Invalid Submission"){
                if(length(content$error$globalErrors)>0){
                  df_fila_logs_tmp$nid[1] <-nid
                  df_fila_logs_tmp$detail[1] <- content$error$globalErrors[[1]]$message
                }
                
                }
              else if(grepl(pattern = "Could not read JSON:",x = content$error$message,ignore.case = TRUE)){
                
                df_fila_logs_tmp$detail[1] <- content$error$detail
              }
              else {
                df_fila_logs_tmp$detail[1] <- content$error$detail
              }
              
              df_fila_logs <- bind_rows(df_fila_logs,df_fila_logs_tmp)
              
            }
           
         }
     }
   

}


  


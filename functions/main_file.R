library(writexl)
library(stringr)
library(stringi)
library(dplyr)
library(tibble)
library(plyr)
library(properties)
library(httr)
library(uuid)
library( jsonlite)

#Set working dir
wd <- '~/Git/cram/'
setwd(wd)

#carrega as funcoes 
source('functions/generic_functions.R')
source('concepts.R')
source('functions/api_functions.R')
source('functions/compose_functions.R')

#import fuchia long dataset
patient_visits <- read.csv(file = 'data/patientlong.csv',stringsAsFactors = FALSE)


#Merge 
#Filtrar os activos
patient_visits <- filter(patient_visits, outcome=="on treatment")

#remover todas colunas vazias do df das visitas
patient_visits <- patient_visits %>% select_if(not_all_na)
patient_visits$nid <- as.numeric(patient_visits$nid)
#import tb data
tb_patient <- readxl::read_xlsx(path = 'data/tb_export.xlsx',col_names = TRUE,progress = TRUE)
tb_patient$nid <- as.numeric(tb_patient$nid)
tb_patient <- filter(tb_patient, nid %in% patient_visits$nid)


#import free variables
patient_free_var <- readxl::read_xls(path = 'data/patient.xls',col_names = TRUE,na = ".")
patient_free_var$nid <- as.numeric(patient_free_var$nid)
patient_free_var <- filter(patient_free_var, nid %in% patient_visits$nid)


#import free variables crag , hepatite, lam, varfu6
patient_free_var_hepatite <- readxl::read_xls(path = 'data/hepC.xls',col_names = TRUE,na = ".")
patient_free_var_hepatite$nid <- as.numeric(patient_free_var_hepatite$nid)
patient_free_var_hepatite <- filter(patient_free_var_hepatite, nid %in% patient_visits$nid)

patient_free_var_lam<- readxl::read_xls(path = 'data/LAM.xls',col_names = TRUE,na = ".")
patient_free_var_lam$nid <- as.numeric(patient_free_var_lam$nid)
patient_free_var_lam <- filter(patient_free_var_lam, nid %in% patient_visits$nid)

patient_free_var_crag<- readxl::read_xls(path = 'data/cripto.xls',col_names = TRUE,na = ".")
patient_free_var_crag$nid <- as.numeric(patient_free_var_crag$nid)
patient_free_var_crag <- filter(patient_free_var_crag, nid %in% patient_visits$nid)


# corrigir algumas 
source('bug_fixes.R')


# Patient Admissions
patient_admissions <- readxl::read_xls(path = 'data/cram_admissions.xls',sheet = 1,col_names = TRUE)
nids_activos <- unique(patient_visits$nid)
patient_admissions <- patient_admissions %>% filter(nid %in% nids_activos)

# separar os nomes (given_name, middle_name,family_name)
patient_admissions <- add_column(patient_admissions, .after = "nome_apelido",given_name="",middle_name="",family_name="" , birthdate="")
patient_admissions <- add_column(patient_admissions,.after = "idade",age="",datbirth="")


# preenche a data de bascimento apartir de patient_visists

for (v in 1:nrow(patient_admissions)) {
   
   nid_pat <- patient_admissions$nid[v]
   pat <- filter(patient_visits, nid==nid_pat) %>% arrange(datvisit) 
   b_date <- pat$birth[1]
   agev <-pat$agev[1]
   datb <- pat$datbirth[1]
   patient_admissions$birthdate[v] <- b_date
   patient_admissions$age[v] <-agev
   patient_admissions$datbirth[v] <-datb
   
}

patient_visits$birth    <- as.Date(patient_visits$birth,   "%d%B%Y")
patient_visits$datbirth    <- as.Date(patient_visits$datbirth,   "%d%B%Y")
patient_admissions$birthdate <- as.Date(patient_admissions$birthdate ,   "%d%B%Y")
patient_admissions$datbirth <- as.Date(patient_admissions$datbirth ,   "%d%B%Y")

# Excluir pacientes com nomes vazios (temp)
# wout_names         <- filter(patient_admissions,is.na(nome_apelido))
patient_admissions <- filter(patient_admissions, ! nid %in% wout_names$nid )
patient_visits <- filter(patient_visits, ! nid %in% wout_names$nid ) 

# wout_admissions    <-  filter(patient_visits,! nid %in% patient_admissions$nid )  %>% select(keypatie,origin,prof,nid,gender,age,birth,agedate,hiv,anadate,outcome)
# wout_admissions    <- wout_admissions[!duplicated(wout_admissions$nid),]

patient_visits <- filter(patient_visits, ! nid %in% wout_admissions$nid ) 
# writexl::write_xlsx(x = wout_names,path = 'data/pacientes_sem_nomes.xls')
# writexl::write_xlsx(x = wout_admissions,path = 'data/pacientes_activos_nao_existe_admissao.xls')

for (v in 1:nrow(patient_admissions)) {
   name <- patient_admissions$nome_apelido[v]
   
   f_index <- stri_locate_first(name, regex = ' ')[[1]]
   s_index <- stri_locate_last(name, regex = ' ')[[1]]
   if(!is.na(f_index)){ # sem nomes, skip
     if(f_index==s_index){
       # apenas 2 nomes
       g_name <-  substr(name, 0, f_index -1 )
       f_name <-  substr(name, f_index +1, nchar(name) )
       
       patient_admissions$given_name[v] <- g_name
       
       if(nchar(f_name)!=0){
         patient_admissions$family_name[v] <- f_name
         
       }
       
     } else {  # 3 nomes
       
       
       g_name <-  substr(name, 0, f_index -1 )
       temp_name <-  substr( name, f_index +1, nchar(name) )
       
       t_index <- stri_locate_first(temp_name, regex = ' ')[[1]]
       
       m_name <-  substr( temp_name, 0, t_index -1 )
       
       f_name <-  substr(temp_name, t_index +1, nchar(name) )
 


 
       patient_admissions$given_name[v] <- g_name
       patient_admissions$middle_name[v] <- m_name
       patient_admissions$family_name[v] <- f_name
     }
     
   }


  
}


patient_admissions <- patient_admissions[ , -which(names(patient_admissions) %in% c("nome_apelido"))]

# uniformizar o sexo
patient_admissions$sexo [which(patient_admissions$sexo=="Mas")] <- "M"
patient_admissions$sexo [which(patient_admissions$sexo=="Masc")] <- "M"
patient_admissions$sexo [which(patient_admissions$sexo=="MASC")] <- "M"
patient_admissions$sexo [which(patient_admissions$sexo=="fem")] <- "F"
patient_admissions$sexo [which(patient_admissions$sexo=="Fem")] <- "F"
patient_admissions$sexo [which(patient_admissions$sexo=="FEM")] <- "F"

### generate person uuids
patient_admissions$uuid <- ""
for(i in 1:nrow(patient_admissions)){
  patient_admissions$uuid[i] <- UUIDgenerate(use.time = TRUE, n = 1L)
}

#Add status column
patient_admissions$openmrs_status <- ""



df_patient_logs  <- createLogsDataFrame(nrow(patient_admissions))

# Migrate patients
for (i in 1:nrow(patient_admissions) ) {
  
   nid <- patient_visits$nid[i]
   uuid <- patient_admissions$uuid[i]
   df_patient_logs$nid[i] <- nid
   json_patient <- composePatient(patient_admissions,i)
   status <- apiCreateOpenmrsPatient(json_patient)
   
   if(as.integer(status$status_code)==201 | as.integer(status$status_code) == 204) {
      df_patient_logs$api_status_code[i] <- as.integer(status$status_code)
      df_patient_logs$message[i] <- "sucess"
      content <- content(status)
      patient_admissions$openmrs_status[i] <- content$person$uuid
      #TODO for each patient create ficha resumo
   } else {
      # TODO: handle failure
      content <- content(status)
      df_patient_logs$api_status_code[i] <- as.integer(status$status_code)
      df_patient_logs$message[i] <- content$error$message
      if(content$error$message=="Invalid Submission"){
         df_patient_logs$detail[i] <- content$error$globalErrors[[1]]$message
      } else {
         df_patient_logs$detail[i] <- content$error$detail
      }
      
   }
   
}

# Create ficha resumo patients
created_patients <- patient_admissions %>% filter(openmrs_status!="")  

# Migrate ficha resumo
df_ficha_resumo_logs <- createLogsDataFrame(nrow(created_patients))
for (i in 1:nrow(created_patients) ) {
   
   nid <- created_patients$nid[i]
   uuid <- created_patients$openmrs_status[i]
   df_ficha_resumo_logs$nid[i] <- nid
   json_ficha_resumo <- composeFichaResumo(df.visits = patient_visits,pat.nid = nid,openmrs.pat.uuid =uuid )
   if(!is.na(json_ficha_resumo)){
      status <- apiCreateOpenmrsFichaResumo(json_ficha_resumo)
      if(as.integer(status$status_code)==201 | as.integer(status$status_code) == 204) {
         df_ficha_resumo_logs$api_status_code[i] <- as.integer(status$status_code)
         df_ficha_resumo_logs$message[i] <- "sucess"
         #TODO for each patient create ficha resumo
      } else {
         # TODO: handle failure
         content <- content(status)
         df_ficha_resumo_logs$api_status_code[i] <- as.integer(status$status_code)
         df_ficha_resumo_logs$message[i] <- content$error$message
         if(content$error$message=="Invalid Submission"){
            df_ficha_resumo_logs$detail[i] <- content$error$globalErrors[[1]]$message
         }else if(grepl(pattern = "Could not read JSON:",x = content$error$message,ignore.case = TRUE)){
            
            df_ficha_resumo_logs$detail[i] <- content$error$detail
         }
           else {
            df_ficha_resumo_logs$detail[i] <- content$error$detail
         }
         
      }
      
   } else {
      df_ficha_resumo_logs$message[i]<- "failed"
      df_ficha_resumo_logs$detail[i]<- "paciente sem info. clinica"
   }
 
   
   
   
}

#Migrate fichas clinicas
df_ficha_clinica_logs <- createLogsDataFrame(nrow(created_patients))
df_ficha_clinica_logs <- df_ficha_clinica_logs[1:1,]
df_ficha_clinica_logs_tmp  <- df_ficha_clinica_logs

for (pat_index in 1:nrow(created_patients) ) {
   
   pat.nid <- created_patients$nid[pat_index]
   uuid <- created_patients$openmrs_status[pat_index]
   df_ficha_clinica_logs_tmp$nid[1] <- pat.nid
   
   df_visits <- filter(patient_visits, nid==pat.nid) %>% arrange(datvisit)
   
   for (vis_index in 1:nrow(df_visits) ) {
      
      visit <- df_visits[vis_index,]
      json.ficha.clinica <- composeFichaClinica(df.visits = visit,openmrs.pat.uuid =uuid )
      status <- apiCreateOpenmrsFichaClinica(json.ficha.clinica)
      
      if(as.integer(status$status_code)==201 | as.integer(status$status_code) == 204) {
         df_ficha_clinica_logs_tmp$api_status_code[1] <- as.integer(status$status_code)
         df_ficha_clinica_logs_tmp$message[1] <- "sucess"
         df_ficha_clinica_logs <- bind_rows(df_ficha_clinica_logs,df_ficha_clinica_logs_tmp)
         
      } else {
     
         content <- content(status)
         df_ficha_clinica_logs_tmp$api_status_code[1] <- as.integer(status$status_code)
         df_ficha_clinica_logs_tmp$message[1] <- content$error$message
         if(content$error$message=="Invalid Submission"){
            df_ficha_clinica_logs_tmp$detail[1] <- content$error$globalErrors[[1]]$message
         }else if(grepl(pattern = "Could not read JSON:",x = content$error$message,ignore.case = TRUE)){
            
            df_ficha_clinica_logs_tmp$detail[1] <- content$error$detail
         }
         else {
            df_ficha_clinica_logs_tmp$detail[1] <- content$error$detail
         }
         
         df_ficha_clinica_logs <- bind_rows(df_ficha_clinica_logs,df_ficha_clinica_logs_tmp)
         
      }
      
   }
   
}

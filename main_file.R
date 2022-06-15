library(writexl)
library(stringr)
library(stringi)
library(dplyr)
library(tibble)
library(plyr)
library(properties)
library(httr)
library(uuid)
library(jsonlite)

#Set working dir
wd <- '~/Git/cram/'
setwd(wd)

# carrega as funcoes 
source('functions/generic_functions.R')
source('concepts.R')
source('functions/api_functions.R')
source('functions/compose_functions.R')

# Import fuchia long dataset
# patient_data <- read.csv(file = 'data/patientlong.csv',stringsAsFactors = FALSE, na.strings=c("."))
patient_data <- read.csv(file = 'data/long_export.csv',stringsAsFactors = FALSE, na.strings=c("."))
#patient_data <- read.csv(file = 'data/', stringsAsFactors = FALSE, na.strings=c("."))

# Filtrar os activos
nids_activos <- filter(patient_data, outcome=="on treatment") %>% select(nid)
nids <- unique(as.numeric(nids_activos$nid))
patient_visits <- filter(patient_data, nid  %in% nids)

# Remover todas colunas vazias do df das visitas
patient_visits <- patient_visits %>% select_if(not_all_na)
patient_visits$nid <- as.numeric(patient_visits$nid)

#import tb data
tb_patient <- readxl::read_xlsx(path = 'data/tb_export.xlsx',col_names = TRUE,progress = TRUE)
tb_patient$nid <- as.numeric(tb_patient$nid)
tb_patient <- filter(tb_patient, nid %in% nids)

#import free variables
patient_free_var <- readxl::read_xls(path = 'data/patient.xls',col_names = TRUE,na = ".")
#patient_free_var <- readxl::read_xlsx(path = 'data/patient.xlsx',col_names = TRUE,na = ".")
patient_free_var$nid <- as.numeric(patient_free_var$nid)
patient_free_var <- filter(patient_free_var, nid %in% nids)

#lab data
#lab <- readxl::read_xls(path = 'data/blood_export.xls',col_names = TRUE)
lab <- read.csv(file = 'data/blood_export.csv',stringsAsFactors = FALSE,header = TRUE)
#lab<- readxl::read_xlsx(path = 'data/blood_export.xlsx',col_names = TRUE,na = ".")

#import free variables crag , hepatite, lam, varfu6
patient_free_var_hepatite <- readxl::read_xls(path = 'data/hepC.xls',col_names = TRUE,na = ".")
patient_free_var_hepatite$nid <- as.numeric(patient_free_var_hepatite$nid)
patient_free_var_hepatite <- filter(patient_free_var_hepatite, nid %in% nids)


patient_free_var_lam<- readxl::read_xls(path = 'data/LAM.xls',col_names = TRUE,na = ".")
patient_free_var_lam$nid <- as.numeric(patient_free_var_lam$nid)
patient_free_var_lam <- filter(patient_free_var_lam, nid %in% nids)

patient_free_var_crag<- readxl::read_xls(path = 'data/cripto.xls',col_names = TRUE,na = ".")
patient_free_var_crag$nid <- as.numeric(patient_free_var_crag$nid)
patient_free_var_crag <- filter(patient_free_var_crag, nid %in% nids)


# Mapeamento de regimes Terapeuticos , formatacao de datas e codificacao corecta de caracteres
source('bug_fixes.R')


# Patient Admissions
patient_admissions <- readxl::read_xlsx(path = 'data/cram_admissions.xlsx',sheet = 1,col_names = TRUE)
names(patient_admissions)[2] <- "nid"
names(patient_admissions)[3] <- "nid_misau"
names(patient_admissions)[7] <- "nome_apelido"
names(patient_admissions)[8] <- "idade"
names(patient_admissions)[9] <- "sexo"
names(patient_admissions)[10] <- "bairro"
names(patient_admissions)[11] <- "telefone"

# NIDS MISAU
nid_misau_cram <- readxl::read_xlsx(path = 'data/NID_MISAU_CRAM.xlsx',sheet = 1,col_names = TRUE)
names(nid_misau_cram)[1] <- "cram_id"
names(nid_misau_cram)[2] <- "nid_misau"
names(nid_misau_cram)[6] <- "telefone"

nids_activos <- unique(nids)
patient_admissions <- patient_admissions %>% filter(nid %in% nids_activos)

# separar os nomes (given_name, middle_name,family_name)
patient_admissions <- add_column(patient_admissions, .after = "nome_apelido",given_name="",middle_name="",family_name="" , birthdate="")
patient_admissions <- add_column(patient_admissions,.after = "idade",age="",datbirth="")


# preenche a data de nascimento apartir de patient_visists

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


patient_visits$birth    <- as.Date(patient_visits$birth,   "%m/%d/%Y")
patient_visits$datbirth    <- as.Date(patient_visits$datbirth,   "%m/%d/%Y")
patient_admissions$birthdate <- as.Date(patient_admissions$birthdate ,   "%m/%d/%Y")
patient_admissions$datbirth <- as.Date(patient_admissions$datbirth ,   "%m/%d/%Y")

# Excluir pacientes com nomes vazios (temp)
wout_names         <- filter(patient_admissions,is.na(nome_apelido))
patient_admissions <- filter(patient_admissions, ! nid %in% wout_names$nid )
active_wout_name <- filter(patient_visits,  nid %in% wout_names$nid ) 

patient_visits     <- filter(patient_visits, ! nid %in% wout_names$nid ) 

wout_admissions    <- filter(patient_visits,! nid %in% patient_admissions$nid )  %>%
                           select( keypatie,origin,prof,nid,gender,age,birth,agedate,hiv,outcome)

wout_admissions    <- wout_admissions[!duplicated(wout_admissions$nid),]

patient_visits     <- filter(patient_visits, ! nid %in% wout_admissions$nid ) 

writexl::write_xlsx(x = wout_names,path = 'data/pacientes_sem_nomes.xls')
writexl::write_xlsx(x = wout_admissions,path = 'data/pacientes_activos_nao_existe_admissao.xls')

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


patient_admissions <- patient_admissions %>% left_join(nid_misau_cram, by =  c("nid" = "cram_id")) %>% 
   select(nid,nid_misau.y,given_name , middle_name ,family_name ,sexo,age,
          datbirth,telefone.x,bairro,Referidode ,openmrs_status,uuid)

names(patient_admissions)[2] <- "nid_misau"
names(patient_admissions)[9] <- "telefone"

# Pacientes com data errada de nascimento (1 pacientes de 94 anos 1927-01-01)
patient_admissions$datbirth[which(patient_admissions$datbirth > '2021-11-12')] <- as.Date('1927-01-01')
# Paciente sem nome
patient_admissions$given_name[966]<- "Augusto"
patient_admissions$middle_name[966]<- "Alfredo"
patient_admissions$given_name[1056]<- "Filomena"

df_patient_logs  <- createLogsDataFrame(nrow(patient_admissions))

# Migrate patients nrow(patient_admissions) 
# TODO
for (i in 1:nrow(patient_admissions)  ){
  
   nid <- patient_admissions$nid[i]
   uuid <- patient_admissions$uuid[i]
   df_patient_logs$nid[i] <- nid
   json_patient <- composePatient(patient_admissions,i)
   status       <- apiCreateOpenmrsPatient(json_patient)
   
   if(as.integer(status$status_code)==201 | as.integer(status$status_code) == 204) {
      df_patient_logs$api_status_code[i] <- as.integer(status$status_code)
      df_patient_logs$message[i] <- "sucess"
      content <- content(status)
      patient_admissions$openmrs_status[i] <- content$person$uuid

   } 
   else if(as.integer(status$status_code)==403){
      df_patient_logs$api_status_code[i] <- as.integer(status$status_code)
      df_patient_logs$message[i] <- "HTTP 403 Forbidden"
      df_patient_logs$detail[i]  <- "HTTP 403 Forbidden"
      
   } 
   else {
      # TODO: handle failure
      content <- content(status)
      df_patient_logs$api_status_code[i] <- as.integer(status$status_code)
      df_patient_logs$message[i] <- content$error$message
      df_patient_logs$detail[i] <- content$error$code
         if(content$error$message=="Invalid Submission"){
         if(grepl(pattern = 'sendo usado por um outro paciente', x = content$error$globalErrors[[1]]$message, ignore.case =TRUE)){
            patient_admissions$nid_misau[i] <- NA
            json_patient <- composePatient(patient_admissions,i)
            status       <- apiCreateOpenmrsPatient(json_patient)
            if(as.integer(status$status_code)==201 | as.integer(status$status_code) == 204) {
               df_patient_logs$api_status_code[i] <- as.integer(status$status_code)
               df_patient_logs$message[i] <- "sucess"
               content <- content(status)
               patient_admissions$openmrs_status[i] <- content$person$uuid
               df_patient_logs$detail[i] <- "Paciente com NID existente no OpenMRS"
               
            } 
            
         }else {
            print("-- ----------------------------------------------------------------")
            print(paste0("NID: " ,nid, " Nao foi inserido no OpenMRS. O erro foi gravado na pasta errors"))
            print(paste0("NID: " ,nid))
            save( content, file =paste0("errors/",nid,"_content_error_rest_api_submission.RData") )
            
         }

         
      } else {
         df_patient_logs$detail[i] <- content$error$detail
      }
      
   }
   
}


# Create ficha resumo patients
created_patients <- patient_admissions %>% filter(openmrs_status!="")  


# pacientes nao registados
not_registered_patients <- df_patient_logs %>% filter(!is.na(detail))  
write_xlsx(x = not_registered_patients,path = 'data/not_registered_patients.xlsx')

# Migrate ficha resumo
df_ficha_resumo_logs <- createLogsDataFrame(nrow(created_patients))

for(i in 1:nrow(created_patients)) {
   
   nid <- created_patients$nid[i]
   uuid <- created_patients$openmrs_status[i]
   df_ficha_resumo_logs$nid[i] <- nid
   
   json_ficha_resumo <- composeFichaResumo(df.visits = patient_visits,pat.nid = nid,openmrs.pat.uuid =uuid )
   if(!is.na(json_ficha_resumo)){
      status <- apiCreateOpenmrsFichaResumo(json_ficha_resumo)
      
      
      if(as.integer(status$status_code)==201 | as.integer(status$status_code) == 204) {
         df_ficha_resumo_logs$api_status_code[i] <- as.integer(status$status_code)
         df_ficha_resumo_logs$message[i] <- "sucess"
         tmp_nid <- nid
         tmp_pat <- subset(patient_visits, nid ==tmp_nid,)
         tmp_pat <- arrange(tmp_pat,datvisit)
         #visit_atributes
         date_enrolled <- tmp_pat$datvisit[1]
         
        
         # desactualizado
         # enrrollment_details <- paste0( "\"patient\":\"",        uuid, "\" ," ,
         #                                "\"program\":\"",        program_tarv_uuid , "\" ," ,
         #                                "\"dateEnrolled\":\"",   date_enrolled , "\" ," ,
         #                                "\"location\":\"",       default_location, "\" ") 
         
         json_enrollment <-  paste0("{", "\"patient\" : \"",uuid,"\" ,",
                                    "\"program\":\"",     program_tarv_uuid , "\" ," ,
                                    "\"dateEnrolled\":\"",   date_enrolled , "\" ," ,
                                    "\"dateCompleted\": null, " ,
                                    "\"location\":\"",       default_location, "\" ," ,
                                    "\"voided\": false, ",
                                    "\"outcome\": null,",
                                    "\"states\": [{ \"state\": { \"uuid\":\"ef06e6df-6026-4d5a-88f9-b2c3e0495dc8\", \"retired\": false ,\"concept\":  \"e1da7d3a-1d5f-11e0-b929-000c29ad1d07\" }, \"startDate\": \"" ,   date_enrolled ,"\" ," , 
                                    "\"endDate\": null }] ", "}" )
         
         status_enrollment <- apiCreateProgramEnrollment(json_enrollment)
         
         #TODO for each patient create ficha resumo
      }
      else if(as.integer(status$status_code)==403){
         df_ficha_resumo_logs$api_status_code[i] <- as.integer(status$status_code)
         df_ficha_resumo_logs$message[i] <- "HTTP 403 Forbidden"
         df_ficha_resumo_logs$detail[i]  <- ""
         
      } 
      else {
         # TODO: handle failure
         content <- content(status)
         df_ficha_resumo_logs$api_status_code[i] <- as.integer(status$status_code)
         df_ficha_resumo_logs$message[i] <- content$error$message
         if(content$error$message=="Invalid Submission"){
            df_ficha_resumo_logs$detail[i] <- content$error$code
            print("-- ----------------------------------------------------------------")
            print(paste0("NID: " ,nid, " Ficha resumo nao foi inserido no OpenMRS. O erro foi gravado na pasta errors"))
            print(paste0("NID: " ,nid))
            save( content, file =paste0("errors/",nid,"_ficha_resumo_error_rest_api_submission.RData") )
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
   df_fila_logs_tmp <- df_ficha_clinica_logs
   df_fila_logs <- df_ficha_clinica_logs

      
for(pat_index in 1:nrow(created_patients)) {

      pat.nid <- created_patients$nid[pat_index]
      uuid <- created_patients$openmrs_status[pat_index]
      df_ficha_clinica_logs_tmp$nid[1] <- pat.nid
      df_fila_logs_tmp$nid[1] <- pat.nid
      df_visits <- filter(patient_visits, nid==pat.nid) %>% arrange(datvisit)
      
      for (vis_index in 1:nrow(df_visits) ) {
         
         visit <- df_visits[vis_index,]
         json.ficha.clinica <- composeFichaClinica(df.visits = visit,openmrs.pat.uuid =uuid )
         
       
         status <- apiCreateOpenmrsFichaClinica(json.ficha.clinica)
         
         if(as.integer(status$status_code)==201 | as.integer(status$status_code) == 204) {
            df_ficha_clinica_logs_tmp$api_status_code[1] <- as.integer(status$status_code)
            df_ficha_clinica_logs_tmp$message[1] <- "sucess"
            df_ficha_clinica_logs <- bind_rows(df_ficha_clinica_logs,df_ficha_clinica_logs_tmp)
        
            # Os dados dos levantamentos serao enviados pelo iDART
            # vis_date <- as.Date(visit$datvisit)
            # next_vist <- as.Date(visit$datnext)
            # if(as.numeric(next_vist - vis_date) > 15){ 
            #    ################# Fila
            #    #Cria fila associado a visita
            #    json_fila <- composeFila(df.visits = visit,openmrs.pat.uuid =uuid )
            #    status_fila <- apiCreateOpenmrsFila(json_fila)
            #    if(as.integer(status_fila$status_code)==201 | as.integer(status_fila$status_code) == 204){
            #       df_fila_logs_tmp$api_status_code[1] <- as.integer(status$status_code)
            #       df_fila_logs_tmp$message[1] <- "sucess"
            #       df_fila_logs <- bind_rows(df_fila_logs_tmp,df_fila_logs)
            #    } else{
            #       
            #       content <- content(status_fila)
            #       df_fila_logs_tmp$api_status_code[1] <- as.integer(status_fila$status_code)
            #       df_fila_logs_tmp$message[1] <- content$error$message
            #       if(content$error$message=="Invalid Submission"){
            #          df_fila_logs_tmp$detail[1] <- content$error$globalErrors[[1]]$message
            #       }else if(grepl(pattern = "Could not read JSON:",x = content$error$message,ignore.case = TRUE)){
            #          
            #          df_fila_logs_tmp$detail[1] <- content$error$detail
            #       }
            #       else {
            #          df_fila_logs_tmp$detail[1] <- content$error$detail
            #       }
            #       
            #       df_fila_logs <- bind_rows(df_fila_logs,df_fila_logs_tmp)
            #       
            #    }
            #    
            # } else {
            #    
            #    # skip fila criation
            #    print(paste0(pat.nid, " - Skipping creation of fila due to short visit dates"))
            # }
            
      
         } 
         else {
        
            content <- content(status)
            df_ficha_clinica_logs_tmp$api_status_code[1] <- as.integer(status$status_code)
            df_ficha_clinica_logs_tmp$message[1] <- content$error$message
            if(content$error$message=="Invalid Submission"){
               if(length(content$error$globalErrors)>0){
                  df_ficha_clinica_logs_tmp$detail[1] <- content$error$globalErrors[[1]]$message
               }

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
   
   
   
#Migrate Lab data
   
   df_lab_logs <- createLogsDataFrame(nrow(created_patients))
   df_lab_logs <- df_lab_logs[1:1,]
   df_lab_logs_tmp  <- df_lab_logs

  for (j in 1:nrow(created_patients) ){
      
      pat.nid <- created_patients$nid[j]
      patient <- created_patients$openmrs_status[j]
      index =1
      df.lab <- subset(lab, nid ==pat.nid, )
      
      if(nrow(df.lab)>0){
         
         # there are only 47 listed lab tests in the file
         for ( i in 1:44){
            # Depricated 
            # date format changed when lab file was imported in csv format
            #examen       <-  df.lab[[paste0("examen",i)]][index]
            if( !is.null(df.lab[[paste0("examen",i)]][index])) {
               
               examen       <-  as.Date(df.lab[[paste0("examen",i)]][index],format =  '%d%b%Y')
               
               if(  !is.na(examen) ) {  # no more lab requests
                  
                  encounter_datetime <- as.character(examen)
                  
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
                                        "\"concept\":\"", concept_lab_group_member_bioquimica,"\"," ,
                                        "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                        "\"groupMembers\":  [{  \"concept\":\"", concept_lab_alat,"\"," ,
                                        "\"person\":\"",  patient  ,"\"," ,
                                        "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                        "\"value\":\"", alat,"\" }]",
                                        "}")
                     
                     
                  } else {
                     
                     obs_alat <- ""
                  }
                  
                  if(!is.na(creatui)){
                     obs_creatui <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                           "\"concept\":\"", concept_lab_group_member_bioquimica,"\"," ,
                                           "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                           "\"groupMembers\":  [{  \"concept\":\"", concept_lab_creatinine,"\"," ,
                                           "\"person\":\"",  patient  ,"\"," ,
                                           "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                           "\"value\":\"", creatui,"\" }]",
                                           "}")
                     
                  } else {
                     
                     obs_creatui <- ""
                  }
                  
                  
                  if(!is.na(lc) ){
                     
                     
                     obs_linfocitos <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                              "\"concept\":\"", concept_lab_group_member_hemograma,"\"," ,
                                              "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                              "\"groupMembers\":  [{  \"concept\":\"", concept_lab_linfocitos,"\"," ,
                                              "\"person\":\"",  patient  ,"\"," ,
                                              "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                              "\"value\":\"", lc,"\" }]",
                                              "}")
                  } else {
                     
                     obs_linfocitos <- ""
                  }
                  
                  if(!is.na(lccd4) & lccd4 !=99 ){
                     
                     
                     obs_cd4_numeric <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                               "\"concept\":\"", concept_lab_group_member_imunologia,"\"," ,
                                               "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                               "\"groupMembers\":  [{  \"concept\":\"", concept_lab_cd4_numeric,"\"," ,
                                               "\"person\":\"",  patient  ,"\"," ,
                                               "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                               "\"value\":\"", lccd4,"\" }]",
                                               "}")
                  } else {
                     
                     obs_cd4_numeric <- ""
                  }
                  
                  
                  
                  if(!is.na(lccd4tlc)  ){
                     
                     obs_cd4_perc <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                            "\"concept\":\"", concept_lab_group_member_imunologia,"\"," ,
                                            "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                            "\"groupMembers\":  [{  \"concept\":\"", concept_lab_cd4_percent,"\"," ,
                                            "\"person\":\"",  patient  ,"\"," ,
                                            "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                            "\"value\":\"", lccd4tlc,"\" }]",
                                            "}")
                     
                     
                     
                  } else {
                     
                     obs_cd4_perc <- ""
                  }
                  
                  
                  
                  
                  if(!is.na(hivload) & hivload != 99){
                     
                     
                     obs_hiv_load <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                            "\"concept\":\"", concept_lab_group_member_testagem_virologia,"\"," ,
                                            "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                            "\"groupMembers\":  [{  \"concept\":\"", concept_lab_carga_viral,"\"," ,
                                            "\"person\":\"",  patient  ,"\"," ,
                                            "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                            "\"value\":\"", hivload,"\" }]",
                                            "}")
                     
                     
                  } else {
                     
                     obs_hiv_load <- ""
                  }
                  
                  
                  if(!is.na(hemoglb) ){
                     
                     obs_hemoglb <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                           "\"concept\":\"", concept_lab_group_member_hemograma,"\"," ,
                                           "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                           "\"groupMembers\":  [{  \"concept\":\"", concept_lab_homogl,"\"," ,
                                           "\"person\":\"",  patient  ,"\"," ,
                                           "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                           "\"value\":\"", hemoglb,"\" }]",
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
                                               "\"concept\":\"", concept_lab_data_pedido_exame,"\"," ,
                                               "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                               "\"value\":\"", examen,"\"" ,
                                               "} ," ,
                                               "{ \"person\":\"",  patient  ,"\"," ,
                                               "\"concept\":\"", concept_lab_data_colheita_amostra,"\"," ,
                                               "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                               "\"value\":\"", examen,"\"" ,
                                               "} "
                  ) 
                  
                  
                  joined_encounter_obs <- paste0(encounter_details,obs_alat,obs_creatui,obs_linfocitos,obs_cd4_numeric,obs_cd4_perc ,obs_hiv_load,obs_hemoglb)
                  encounter_lab <- paste0("{ ", joined_encounter_obs, " ] }")
                  
                  status_lab<- apiCreateOpenmrsLab(encounter_lab)
                  
                  if(as.integer(status_lab$status_code)==201 | as.integer(status_lab$status_code) == 204){
                     df_lab_logs_tmp$api_status_code[1] <- as.integer(status$status_code)
                     df_lab_logs_tmp$message[1] <- "sucess"
                     df_lab_logs <- bind_rows(df_lab_logs_tmp,df_lab_logs)
                  } else{
                     
                     content <- content(status_lab)
                     df_lab_logs_tmp$api_status_code[1] <- as.integer(status_lab$status_code)
                     df_lab_logs_tmp$message[1] <- content$error$message
                     if(content$error$message=="Invalid Submission"){
                        df_lab_logs_tmp$detail[1] <- content$error$globalErrors[[1]]$message
                     }else if(grepl(pattern = "Could not read JSON:",x = content$error$message,ignore.case = TRUE)){
                        
                        df_lab_logs_tmp$detail[1] <- content$error$detail
                     }
                     else {
                        df_lab_logs_tmp$detail[1] <- content$error$detail
                     }
                     
                     df_lab_logs <- bind_rows(df_lab_logs,df_lab_logs_tmp)
                     print("##########################################  LAB ERROR ##############################################")
                     print(encounter_lab)
                     
                     
                  }
                  
                  
               }
            }

         
      }
      
      
   }
   
   }
   
   
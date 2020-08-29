library(writexl)
library(stringr)
library(stringi)
library(dplyr)
library(tibble)

#Set working dir
wd <- '~/Git/cram/'
setwd(wd)

patient_admissions <- readxl::read_xls(path = 'data/cram_admissions.xls',sheet = 1,col_names = TRUE)

# separar os nomes (given_name, middle_name,family_name)
patient_admissions <- add_column(patient_admissions, .after = "nome_apelido",given_name="",middle_name="",family_name="" )

# excluir pacientes com nomes vazios (temp)
patient_admissions <- filter(patient_admissions,!is.na(nome_apelido))

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

patient_admissions$nome_apelido <- NULL

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


#'  "names": [
#'    {
#'      "givenName": "Mohit",
#'      "familyName": "Kumar"
#'    }
#'    ],
#'  "gender": "M",
#'  "birthdate": "1997-09-02",
#'  "addresses": [
#'    {
#'      "address1": "30, Vivekananda Layout, Munnekolal,Marathahalli",
#'      "cityVillage": "Bengaluru",
#'      "country": "India",
#'      "postalCode": "560037"
#'    }



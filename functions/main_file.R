library(writexl)
library(stringr)
library(stringi)
library(dplyr)
library(tibble)
library(uuid)
#Set working dir
wd <- '~/Git/cram/'
setwd(wd)


#Global variables
default_location ="0dc2d9c3-91ff-4a87-b2d1-84d2955bd9cb"


patient_admissions <- readxl::read_xls(path = 'data/cram_admissions.xls',sheet = 1,col_names = TRUE)
patient_visits <- read.csv(file = 'data/patientlong.csv',stringsAsFactors = FALSE)


#remover todas colunas vazias do df das visitas
patient_visits <- patient_visits %>% select_if(not_all_na)

# corrigir problema com caracteres especiais na variavel origin, entry
Encoding(patient_visits$origin) <- "latin1"
patient_visits$origin <- iconv(patient_visits$origin, "latin1", "UTF-8",sub='')

Encoding(patient_visits$entry) <- "latin1"
patient_visits$entry <- iconv(patient_visits$entry, "latin1", "UTF-8",sub='')


# converter datas string para datas
patient_visits$datvisit <- as.Date(patient_visits$datvisit,"%d%B%Y")
patient_visits$datnext  <- as.Date(patient_visits$datnext, "%d%B%Y")
patient_visits$birth    <- as.Date(patient_visits$birth,   "%d%B%Y")
patient_visits$hivdate    <- as.Date(patient_visits$hivdate,   "%d%B%Y")

# uniformizar TESTE HIV
patient_visits$hivtest [which(patient_visits$hivtest=="Serology")] <- "TR"
patient_visits$hivtest [which(patient_visits$hivtest=="Not specified")] <- "TR"

# uniformizar regime Terapeutico
# Artemisa:
# AA1- ARV not specified
# AA2- DTG combinado com outra droga
# AA3- DTG-booster
# AA4- Other ARV
# AA5- Other ARV
# AA4 e o AA5 PERMITE CRIAR UMA NOVA COMBINAçÃO DE arv que venha a ser usada

patient_visits$arv <- gsub( pattern = 'AA2',   replacement = 'DTG',x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = 'AA3',   replacement = 'DTG',x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = 'EFV600',replacement = 'EFV',x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = 'D4T30', replacement = 'D4T',x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = 'D4T40', replacement = 'D4T',x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = 'EFV800',replacement = 'EFV',x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = '3TCp',  replacement = '3TC',x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = 'AZTp',  replacement = 'AZT',x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = 'EFVp',  replacement = 'EFV',x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = 'LPV/rp',replacement = 'LPV/r',x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = 'NVPp',  replacement = 'NVP',x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = 'D4Tp',  replacement = 'D4T',     x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = 'ABCp',  replacement = 'ABC', x =  patient_visits$arv)
patient_visits$arv <- gsub( pattern = 'NVPp',  replacement = 'NVP',     x =  patient_visits$arv)






# separar os nomes (given_name, middle_name,family_name)
patient_admissions <- add_column(patient_admissions, .after = "nome_apelido",given_name="",middle_name="",family_name="" )

# excluir pacientes com nomes vazios (temp)
#patient_admissions <- filter(patient_admissions,!is.na(nome_apelido))

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



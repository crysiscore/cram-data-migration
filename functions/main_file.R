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
generic_provider ="7013d271-1bc2-4a50-bed6-8932044bc18f"

patient_admissions <- readxl::read_xls(path = 'data/cram_admissions.xls',sheet = 1,col_names = TRUE)
patient_visits <- read.csv(file = 'data/patientlong.csv',stringsAsFactors = FALSE)

# filtrar os activos
patient_visits <- filter(patient_visits, outcome=="on treatment")


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


patient_visits$arv[which(patient_visits$arv=="3TC+ABC+DRV+RAL+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG+TDF")] <- "TDF+3TC+DTG"
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ/r+TDF")] <- "TDF+3TC+ATZ/r"
patient_visits$arv[which(patient_visits$arv=="3TC+DRV+RAL+RTV+TDF")] <- "TDF+3TC+RAL+DRV/r"
patient_visits$arv[which(patient_visits$arv=="3TC+LPV/r+TDF")] <- "TDF+3TC+LPV/r"
patient_visits$arv[which(patient_visits$arv=="3TC+ABC+ATZ/r")] <- "ABC+3TC+ATV/r"
patient_visits$arv[which(patient_visits$arv=="3TC+EFV+TDF")] <- "TDF+3TC+EFV"
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ/r+DRV+RAL+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+AZT+NVP")] <- "AZT+3TC+NVP"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG+AZT")] <- "AZT+3TC+DTG"
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ+RAL+RTV+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ABC+ATZ/r+AZT")] <- "AZT+3TC+ABC"
patient_visits$arv[which(patient_visits$arv=="3TC+AZT+DRV+RAL+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ/r+AZT")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="DRV+RAL+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG+ABC")] <- "ABC+3TC+DTG"

patient_visits$arv[which(patient_visits$arv=="3TC+DTG+ATZ/r+AZT")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="DRV+RAL+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+DRV+RAL+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="DRV+ETV+RAL+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ+TDF")] <- ""
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ+TDF")] <- "TDF+3TC+ATV/r"
patient_visits$arv[which(patient_visits$arv=="3TC+AZT+EFV")] <- "AZT+3TC+EFV"
patient_visits$arv[which(patient_visits$arv=="ATZ/r+RAL")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ABC+EFV")] <- "ABC+3TC+EFV"
patient_visits$arv[which(patient_visits$arv=="3TC+AZT+LPV/r")] <- "AZT+3TC+LPV/r"
patient_visits$arv[which(patient_visits$arv=="3TC+ABC+ATZ/r+RAL")] <- "ABC+3TC+ATV/r+RAL"
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ/r+RAL")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+DRV+RTV+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ABC+DRV+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ/r+RTV+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG+ABC+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ABC+LPV/r")] <- "ABC+3TC+LPV/r"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+LPV/r+RTV+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG+EFV")] <- "TDF+3TC+DTG"
patient_visits$arv[which(patient_visits$arv=="3TC+AZT+LPV/r+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ABC+LPV/r+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ/r+LPV/r")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ABC+ATZ/r+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ+RTV+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ABC+RAL")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="ABC+DRV+RAL+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="ABC+ATZ/r+AZT")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ/r+RTV")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ/r+LPV/r+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ABC+ATZ")] <- "AB"
patient_visits$arv[which(patient_visits$arv=="3TC+ABC+ATZ")] <- "ABC+3TC+ATV/r"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG+ABC+RAL")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="ATZ/r+AZT+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ABC+NVP")] <- "ABC+3TC+NVP"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG+DRV+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG+ATZ")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG+EFV+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG+DTG+AZT")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="DTG+AZT+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="DTG+AZT+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+DTG+DTG+TDF")] <- "TDF+3TC+DTG"
patient_visits$arv[which(patient_visits$arv=="DTG+EFV+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+AZT+RAL")] <- "AZT+3TC+RAL"

patient_visits$arv[which(patient_visits$arv=="AZT+EFV+LPV/r")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="DTG+FDC7 (AZT-3TC)")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="3TC+ATZ/r+RAL+TDF")] <- "TDF+3TC+ATV/r+RAL"
patient_visits$arv[which(patient_visits$arv=="DTG+EFV+TDF")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="DTG+EFV+TDF")] <- "OUTRO"

patient_visits$arv[which(grepl(pattern = "AA1",x =patient_visits$arv,ignore.case = TRUE ))] <- "OUTRO"
patient_visits$arv[which(grepl(pattern = "D4T",x =patient_visits$arv,ignore.case = TRUE ))] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="AB")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv==".")] <- "OUTRO"
patient_visits$arv[which(patient_visits$arv=="")] <- "OUTRO"






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



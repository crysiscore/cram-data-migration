#Data checks

#Patients without hiv start date
sem_data_inicio_tarv <-   subset(patient_data, is.na(hivdate) ,)

# check if there is a patient without birthdate
wout_birth_date <- subset(patient_admissions,is.na(patient_admissions$datbirth) , )

## patients without names
## Excluir pacientes com nomes vazios (temp)
wout_names         <- filter(patient_admissions,is.na(nome_apelido))
patient_admissions <- filter(patient_admissions, ! nid %in% wout_names$nid )
patient_data <- filter(patient_data, ! nid %in% wout_names$nid ) 

### active patients  without demographic data
wout_admissions    <-  filter(patient_data,! nid %in% patient_admissions$nid )  %>% select(keypatie,origin,prof,nid,gender,age,birth,agedate,hiv,anadate,outcome)
wout_admissions    <- wout_admissions[!duplicated(wout_admissions$nid),]

## duplicated nids
dups_by_nid <- patient_admissions[duplicated(nid),]

# write to excell
writexl::write_xlsx(x = wout_names,path = 'data/pacientes_sem_nomes.xls')
writexl::write_xlsx(x = wout_admissions,path = 'data/pacientes_activos_nao_existe_admissao.xls')
write_xlsx(x = dups_by_nid,path = 'data/duplicated_patients.xlsx')

# fix gender issues
# uniformizar o sexo
patient_admissions$sexo [which(patient_admissions$sexo=="Mas")] <- "M"
patient_admissions$sexo [which(patient_admissions$sexo=="Masc")] <- "M"
patient_admissions$sexo [which(patient_admissions$sexo=="MASC")] <- "M"
patient_admissions$sexo [which(patient_admissions$sexo=="fem")] <- "F"
patient_admissions$sexo [which(patient_admissions$sexo=="Fem")] <- "F"
patient_admissions$sexo [which(patient_admissions$sexo=="FEM")] <- "F"




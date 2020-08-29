# 1 Lista de todos os pacientes activos para cruzar com o ficheiro das admissoe#

#2 extrair lista de pacientes sem idade ou data de nascimeno
wout_birthdate <- patient_admissions %>% filter(is.na(idade) )
patient_admissions %>% filter(is.na(nid) )



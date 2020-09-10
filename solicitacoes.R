# ERROS
# 2 - pacientes activos com nids duplicados
 pacientes_nao_criados <- filter(df_patient_logs, message != "sucess")
 
# fichas-resumo_nao_criadas
 fichas_resumo_nao_criadas <- filter(df_ficha_resumo_logs,message != "sucess")
 fichas_clinicas_nao_criadas <- filter(df_ficha_clinica_logs,message != "sucess")
 
 # duplicados
 duplicados <- patient_admissions[duplicated(patient_admissions$nid ),]
 duplicados <- patient_admissions[patient_admissions$nid %in% duplicados$nid,]
 writexl::write_xlsx(x = duplicados,path = '/data/cram_nids_duplicados.xls',format_headers = TRUE)
 
 
 # estadios para pacientes com 1 consulta
 
# RECOMENDACOES - REGIMES ESPECIFICOS DO CRAM NAO ESTAO PADRONIZADOS NO OPENMRS
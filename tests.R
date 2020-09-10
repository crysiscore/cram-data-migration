for (pat_index in 1:nrow(created_patients) ) {
  
  pat.nid <- created_patients$nid[pat_index]
  uuid <- created_patients$openmrs_status[pat_index]
  
  df_visits <- filter(patient_visits, nid==pat.nid) %>% arrange(datvisit)
  
  for (vis_index in 1:nrow(df_visits) ) {
    visit <- df_visits[vis_index,]
    json.ficha.clinica <- composeFichaClinica(df.visits = visit,openmrs.pat.uuid =uuid )
    
    if(!validate(json.ficha.clinica)){
      print(paste0("nid: ",nid," - problem in json format"))
    }
    
  }
  
}
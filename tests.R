##  Duplicados no iDART e OpenMRS
getCramUuid <- function(connection)  {
  rs  <-    dbSendQuery(
    connection,
    "SELECT  pid.identifier, p.uuid
FROM patient_identifier pid INNER JOIN person p
WHERE pid.patient_id=p.person_id AND p.voided=0 AND pid.voided=0
AND pid.identifier_type=15"
  )
  
  data <- fetch(rs, n = -1)
  RMySQL::dbClearResult(rs)
  return(data)
}


lab <- readxl::read_xls(path = 'data/bloodexport.xls',col_names = TRUE)

df.lab <- filter(lab, nid==pat.nid) 



composeLab <- function(patient.uuid,df.lab ){
  

  patient <- patient.uuid
  index=1 
  if(nrow(df.lab)>0){
    
    # there are only 27 listed lab tests in the file
    for ( i in 1:27){
      
      examen       <- df.lab[[paste0("examen",i)]][index]

      if(!is.na(examen)) {  # no more lab requests
        
        encounter_datetime <- as.character(as.Date(examen))
      
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
                            "\"concept\":\"", concept_lab_alat,"\"," ,
                            "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                            "\"obsGroup\": \"",concept_lab_group_member_bioquimica ,"\"," ,
                            "\"value\":\"", alat,"\"" ,
                            "}")
          
          
          obs_cd4_numeric <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                    "\"concept\":\"", concept_lab_group_member_imunologia,"\"," ,
                                    "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                    "\"groupMembers\":  [{  \"concept\":\"", concept_lab_cd4_numeric,"\"," ,
                                    "\"person\":\"",  patient  ,"\"," ,
                                    "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                    "\"value\":\"", lccd4,"\" }]",
                                    "}")
        } else {
          
          obs_alat <- ""
        }
       
        if(!is.na(creatui)){
          
          obs_creatui <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                              "\"concept\":\"", concept_lab_creatinine,"\"," ,
                              "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                              "\"value\":\"", creatui,"\"" ,
                              "}")
        } else {
          
          obs_creatui <- ""
        }
        

        if(!is.na(lc) ){
          obs_linfocitos <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                                 "\"concept\":\"", concept_lab_linfocitos,"\"," ,
                                 "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                 "\"value\":\"",lc,"\"" ,
                                 "}")
        } else {
          
          obs_linfocitos <- ""
        }
        
        if(!is.na(lccd4) ){
 
          
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
                               "\"concept\":\"", concept_lab_cd4_percent,"\"," ,
                               "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                               "\"value\":\"", lccd4tlc,"\"" ,
                               "}")
          
        } else {
          
          obs_cd4_perc <- ""
        }
        
        

        
        if(!is.na(hivload) ){
          
          
          obs_hiv_load <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                               "\"concept\":\"", concept_lab_carga_viral,"\"," ,
                               "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                               "\"value\":\"", hivload,"\"" ,
                               "}")
          
        } else {
          
          obs_hiv_load <- ""
        }
        
        
        if(!is.na(hemoglb) ){
          
          
          obs_hemoglb <- paste0(", { \"person\":\"",  patient  ,"\"," ,
                              "\"concept\":\"", concept_lab_homogl,"\"," ,
                              "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                              "\"value\":\"", hemoglb,"\"" ,
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
                                     "\"concept\":\"", concept_lab_data_exame,"\"," ,
                                     "\"obsDatetime\":\"", encounter_datetime,"\"," ,
                                     "\"value\":\"", examen,"\"" ,
                                     "}" ) 

        
        joined_encounter_obs <- paste0(encounter_details,obs_alat,obs_creatui,obs_linfocitos,obs_cd4_numeric,obs_cd4_perc ,obs_hiv_load,obs_hemoglb)
        encounter_lab <- paste0("{ ", joined_encounter_obs, " ] }")
        validate(encounter_lab)
        
        status_lab<- apiCreateOpenmrsLab(encounter_lab)
        content(status_lab)
        
        
      }
   
      
    }
   
  }
  
  
}
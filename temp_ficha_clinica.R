
composeFila<- function(df.visits,openmrs.pat.uuid) {
  index=1 # 1 line = 1 visit
  pat <- df.visits

  #visit_atributes
  visit_date       <- pat$datvisit[index]
  next_visit_date  <- pat$datnext[index]

  encounter_date_datime <- visit_date
  #openmrs.pat.uuid = created_patients$openmrs_status[which(created_patients$nid==23465)]
  nidUuid = openmrs.pat.uuid
  
  #regimet
  regime         <- pat$arv[index]
  if(is.na(regime) |   regime ==""){
    obs_regime <- ""
  }  else {
    regime <- openmrsGetRegimeUuid(regime)
   
  }
  
    #encounter_date_datime <- paste0(visit_date," 11:37:31" )
    encounter_date_datime <- visit_date
    obs_date_time <- visit_date
    
    obs_next_pickup_date <- paste0( ", { \"person\":\"",  nidUuid  ,"\"," ,
                                        "\"concept\":\"", returnVisitUuid,"\"," ,
                                        "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                        "\"comment\":\"CRAM\" ," ,
                                        "\"value\":\"", next_visit_date,"\"" ,
                                        "}")
    
  
    
    encounter_details <- paste0( "\"encounterDatetime\":\"",encounter_date_datime, "\" ," ,
                                     "\"patient\":\"",          nidUuid , "\" ," ,
                                     "\"form\":\"",             form_fila_uuid , "\" ," ,
                                     "\"encounterType\":\"",    encounter_type_fila,"\" , " ,  
                                     "\"location\":\"",         default_location, "\", " ,
                                     "\"encounterProviders\":[{ \"provider\":\"", generic_provider  ,"\"," ,
                                     "\"encounterRole\":\"" , encounter_provider_role,"\"" , "}] ,",
                                     "\"obs\":[ { \"person\":\"",  nidUuid  ,"\"," ,
                                                 "\"concept\":\"", concept_fila_regime,"\"," ,
                                                 "\"obsDatetime\":\"", obs_date_time,"\"," ,
                                                 "\"comment\":\"CRAM\" ," ,
                                                 "\"value\":\"", regime,"\"" ,
                                                 "}" )
                
    joined_encounter_obs <- paste0(encounter_details,obs_next_pickup_date)
    
    encounter <- paste0("{ ", joined_encounter_obs, " ] }")
    return(encounter)

}

program_tarv_uuid <- 'efe2481f-9e75-4515-8d5a-86bfde2b5ad3'
concept_transferido_de <- 'e1da7d3a-1d5f-11e0-b929-000c29ad1d07'
programa_tarv_work_flow <- '7f3af436-5c3a-447c-9012-42bb314e03db'
date_enrolled="2020-06-24"

nidUuid <-'9d50a116-f6d1-11ea-b4b3-0242c73e7dc1'

encounter_details <- paste0( "\"patient\":\"",        nidUuid, "\" ," ,
                             "\"program\":\"",        program_tarv_uuid , "\" ," ,
                             "\"dateEnrolled\":\"",   date_enrolled , "\" ," ,
                             "\"location\":\"",         default_location, "\", " ,
                             "\"workflows\":[{ \"person\":\"",  nidUuid  ,"\"," ,
                             "\"concept\":\"", programa_tarv_work_flow,"\"" , " , " ,
                             "\"states\":[{ \"person\":\"",  nidUuid  ,"\"," ,
                             "\"concept\":\"", concept_transferido_de,"\"," ,
                             "\"startDate\":\"", date_enrolled,"\" " , "}]" , "}]")



                            
encounter_details <- paste0( "\"patient\":\"",        nidUuid, "\" ," ,
                             "\"program\":\"",        program_tarv_uuid , "\" ," ,
                             "\"dateEnrolled\":\"",   date_enrolled , "\" ," ,
                             "\"location\":\"",       default_location, "\" ,",
                             "\"states\":[{ \"concept\" : \"", concept_transferido_de,"\" ," ,
                                             "\"startDate\":\"", date_enrolled,"\" }]  " ) 

paste0("{ \"states\": [{ \"state\": {\"uuid\" : \"" , concept_transferido_de
, "\"}, \"startDate\": \"" , date_enrolled , "\"}]}")
{ \"states\": [{ \"uuid\": \"" + existingPatientState.getUuid() + "\", \"startDate\": \""
  + stateStartDate + "\"}]}"
  

encounter <- paste0( "{ ", encounter_details, "  }" )
validate(encounter)
json.enrollment <- encounter

status <- POST(url = base.url,
               body = json.enrollment, config=authenticate('admin', 'eSaude123'),
               add_headers("Content-Type"="application/json") )
content(status)

wf_details <-        paste0( "\"patient\":\"",        nidUuid, "\" ," ,
                             "\"concept\":\"", programa_tarv_work_flow,"\" ," ,
                             "\"states\":[{ \"concept\":\"", concept_transferido_de,"\" ," ,
                             "\"startDate\":\"", date_enrolled,"\" }]  " ) 


base.url <-  as.character(jdbc.properties$urlBase)
base.url <- str_c(base.url,'workflow')

encounter <- paste0("{ ", wf_details, "  }")

json.enrollment <- encounter
status <- POST(url = base.url,
               body = json.enrollment, config=authenticate('admin', 'eSaude123'),
               add_headers("Content-Type"="application/json") )

content(status)




encounter_details <- paste0( "\"patient\":\"",        nidUuid, "\" ," ,
                             "\"program\":{ \"uuid\":\"",  program_tarv_uuid , "\" ," ,
                                           "\"allWorkflows\":[{ \"person\":\"",  nidUuid  ,"\"," ,
                                           "\"concept\":\"", programa_tarv_work_flow,"\" ," ,
                                           "\"states\":[{ \"person\":\"",  nidUuid  ,"\" ," ,
                                           "\"concept\":\"", concept_transferido_de,"\" ," ,
                                           "\"startDate\":\"", date_enrolled,"\" }] }] } ," ,
                             "\"dateEnrolled\":\"",   date_enrolled , "\" ," ,
                             "\"location\":\"",       default_location, "\" ") 


base.url <-  as.character(jdbc.properties$urlBase)
base.url <- str_c(base.url,'workflow')
encounter <- paste0("{ ", encounter_details, "  }")
json.enrollment <- encounter
status <- POST(url = base.url,
               body = json.enrollment, config=authenticate('admin', 'eSaude123'),
               add_headers("Content-Type"="application/json") )
content(status)

apiCreateProgramEnrollment <- function(json.enrollment){
  
  encounter_details <- paste0( "\"patient\":\"",        nidUuid, "\" ," ,
                               "\"program\":\"",        program_tarv_uuid , "\" ," ,
                               "\"dateEnrolled\":\"",   date_enrolled , "\" ," ,
                               "\"location\":\"",       default_location, "\" ") 
  #API connection properties
  jdbc.properties <- readJdbcProperties()
  
  # url da API
  base.url <-  as.character(jdbc.properties$urlBase)
  base.url <- str_c(base.url,'programenrollment')
  
  # send patient to openmrs
  status <- POST(url = base.url,
                 body = json.enrollment, config=authenticate('admin', 'eSaude123'),
                 add_headers("Content-Type"="application/json") )
  content(status)
  
}

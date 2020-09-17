
enrrollment_details <- paste0("{", "\"patient\" : \"",uuid,"\" ,",
                              "\"program\":\"",     program_tarv_uuid , "\" ," ,
                              "\"dateEnrolled\":\"",   date_enrolled , "\" ," ,
                              "\"dateCompleted\": null, " ,
                              "\"location\":\"",       default_location, "\" ," ,
                              "\"voided\": false, ",
                              "\"outcome\": null,",
                              "\"states\": [{ \"state\": { \"uuid\":\"ef06e6df-6026-4d5a-88f9-b2c3e0495dc8\", \"retired\": false ,\"concept\":  \"e1da7d3a-1d5f-11e0-b929-000c29ad1d07\" }, \"startDate\": \"" ,   date_enrolled ,"\" ," , 
                              "\"endDate\": null }] ", "}" )



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

#' composeFichaResumo ->  criar uma string json com informacao para criar a ficha resumo de um paciente no openmrs
#' @param   pat.nid  nid do cram
#' @examples 
#' ficha_resumo  <- composeFichaResumo(df,1234)
#' 

composeFichaResumo <- function(df.visits,pat.nid,openmrs.pat.uuid) {
  
  
  pat <- filter(patient_visits, nid==pat.nid) %>% arrange(datvisit)
  
  #Ficha resumo attributes
  data_abertura_ficha <- pat$datvisit[1]
  profissao           <- pat$prof[1]
  location            <- default_location
  tipo_teste_hiv      <- pat$hivtest[1]
  data_teste          <- pat$hivdate[1]
  us_proveniencia     <- pat$entry[1]
  em_tarv             <- "yes"
  data_inicio_tarv    <- pat$hivdate[1]
  
  
  #TODO estadio OMS: 0 - unknown
  estadio_oms <- pat$whop[1]
  regime      <- pat$arv[1]
  cd4_nr      <- pat$lccd4[1]
  
  #visit_atributes
  visit_date <- pat$datvisit[1]
  patient <- openmrs.pat.uuid
  
  # check empty  variables (required var)
  if(is.na(visit_date) | is.na(data_inicio_tarv)){
    stop(paste0("Error: Cant create ficha resumo for patient:",openmrs.pat.uuid, "either visit_date or data_inicio_tarv is null."))
  }else {
    
    
    # "obs" :[
    #   {
    #     
    #     "concept" : "bae8ffd2-ee79-494c-abc7-2f2a174f802a",
    #     "value" : "111"
    #   },
    #   
    #   {
    #     
    #     "concept" : "c201b543-00a3-4f45-b4ee-684e7fe56782",
    #     "value" : "Test"
    #   }
    #   ]
    # POST /obs 
    # {
    #   "person": "070f0120-0283-4858-885d-a20d967729cf",
    #   "concept": "5089AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
    #   "obsDatetime": "2019-11-14T07:37:31.000+0000",
    #   "value": 70
    # }
    #ramdom time37
    encounter_date_datime <- paste0(visit_date," 11:37:31" )
    obs_date_time <- paste0(visit_date,"T11:37:31.000+0000")
    
    encounter_details <- paste0( "\"encounterDatetime\":\"",encounter_date_datime, "\" ," ,
                                 "\"patient\":\"",          patient , "\" ," ,
                                 "\"form\":\"",             form_ficha_resumo , "\" ," ,
                                 "\"encounterType\":\"",    encounter_type_ficha_resumo,"\" , " ,  
                                 "\"location\":\"",         default_location, "\", " ,
                                 "\"encounterProviders\":[{ \"provider\":\"", generic_provider  ,"\"," ,
                                                           "\"encounterRole\":\"" , encounter_provider_role,"\"" , "}] ,",
                                 "\"obs\":[ {  \"person\":\"",  patient  ,"\"," ,
                                              "\"concept\":\"", concept_fr_data_abertura_ficha,"\"" ,
                                              "\"obsDateTime\":\"", obs_date_time,"\"" ,
                                              "\"valueDateTime\":\"", encounter_date_datime,"\"" ,
                                           "}",
                                           "{  \"person\":\"",  patient  ,"\"," ,
                                              "\"concept\":\"", concept_fr_profissao,"\"" ,
                                              "\"obsDateTime\":\"", obs_date_time,"\"" ,
                                              "\"value\":\"", profissao,"\"" ,
                                           "}",
                                          
                                 
                                 
                                 ]")
    
  }

  
  }

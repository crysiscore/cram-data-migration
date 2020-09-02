#' composeFichaResumo ->  criar uma string json com informacao para criar a ficha resumo de um paciente no openmrs
#' @param   pat.nid  nid do cram
#' @examples 
#' ficha_resumo  <- composeFichaResumo(df,1234)
#' 

composeFichaResumo <- function(df.visits,pat.nid){
  
  
  pat <- filter(patient_visits, nid==pat.nid) %>% arrange(datvisit)
  
  #Ficha resumo attributes
  data_abertura_ficha <- pat$datvisit[1]
  profissao <- pat$prof[1]
  location <- default_location
  tipo_teste_hiv <- pat$hivtest[1]
  data_teste <-   pat$hivdate[1]
  us_proveniencia <- pat$entry[1]
  em_tarv <- "yes"
  data_inicio_tarv <- pat$hivdate[1]
  #TODO estadio oms- pendente da descoficacao das variaveis who
  estadio_oms <-""
  provider <-generic_provider
  
  
}

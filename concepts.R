#Global variables
# change this parameters to match openmrs uuids
default_location = "ca55e5d1-91ec-477b-a6c5-7574142c5086"   # change here
nid_cram_identifier_type <- '1c72703d-fb55-439e-af4f-ef39a1049e19' #NID CRAM UUID ( change this to match openmrs uuid)

generic_provider = "7013d271-1bc2-4a50-bed6-8932044bc18f"
encounter_type_ficha_resumo = 'e422ecf9-75dd-4367-b21e-54bccabc4763'
encounter_type_ficha_clinica_adulto = 'e278f956-1d5f-11e0-b929-000c29ad1d07'
encounter_type_ficha_clinica_ped = 'e278fce4-1d5f-11e0-b929-000c29ad1d07'
encounter_type_fila      <-  'e279133c-1d5f-11e0-b929-000c29ad1d07'      # FILA
encounter_type_lab      <- 'e2790f68-1d5f-11e0-b929-000c29ad1d07'
encounter_provider_role  <- 'a0b03050-c99b-11e0-9572-0800200c9a66'
form_ficha_resumo        <- '05496c70-845c-40b1-9d28-070f67b3f7da'
form_ficha_clinica       <- '3c2d563a-5d37-4735-a125-d3943a3de30a'
form_fila_uuid           <- '49857ace-1a92-4980-8313-1067714df151'
form_lab                  <- '8377e4ff-d0fe-44a5-81c3-74c9040fd5f8'

attribute_telephone_uuid <- "e2e3fd64-1d5f-11e0-b929-000c29ad1d07"

program_tarv_uuid <- 'efe2481f-9e75-4515-8d5a-86bfde2b5ad3'
concept_transferido_de <- 'e1da7d3a-1d5f-11e0-b929-000c29ad1d07'
programa_tarv_work_flow <- '7f3af436-5c3a-447c-9012-42bb314e03db'



#value codes
value_coded_pcr                 <- 'e1d800dc-1d5f-11e0-b929-000c29ad1d07' 
value_coded_tr                  <- 'e1d7f61e-1d5f-11e0-b929-000c29ad1d07'
value_coded_yes                 <- 'e1d81b62-1d5f-11e0-b929-000c29ad1d07'
value_coded_em_tarv             <- '7f3af436-5c3a-447c-9012-42bb314e03db'
value_coded_inicia              <- 'e1d9ef28-1d5f-11e0-b929-000c29ad1d07'
value_coded_continua            <- 'e1d9f036-1d5f-11e0-b929-000c29ad1d07'
value_coded_fim                 <- 'e1d9facc-1d5f-11e0-b929-000c29ad1d07'
value_coded_hivload_nivel_baixo <- 'e1da2812-1d5f-11e0-b929-000c29ad1d07'
value_coded_primeira_linha      <- 'a6bbe1ac-5243-40e4-98cb-7d4a1467dfbe'
value_coded_segunda_linha       <- '7f367983-9911-4f8c-bbfc-a85678801f64'
value_coded_terceira_linha      <- 'ade7656f-0ce3-461b-b7d8-121932dcd6a2'
value_coded_positivo            <- 'e1d47386-1d5f-11e0-b929-000c29ad1d07'
value_coded_negativo            <- 'e1d446cc-1d5f-11e0-b929-000c29ad1d07'

#Ficha Resumo concepts
concept_fr_data_abertura_ficha  <- '68d65d0c-8fee-456b-8e95-caed990351e1'
concept_fr_profissao            <- 'e1dc07c2-1d5f-11e0-b929-000c29ad1d07'
concept_fr_transferido_outra_us <- 'e1da7d3a-1d5f-11e0-b929-000c29ad1d07'
concept_fr_us_provenciencia     <- 'e1da7d3a-1d5f-11e0-b929-000c29ad1d07'
concept_fr_tipo_teste_hiv       <- 'c7ac119c-d59d-474d-8334-0c5bdd2e9863'
concept_fr_data_inicio_tarv     <- 'e1d8f690-1d5f-11e0-b929-000c29ad1d07'
concept_fr_em_tarv              <- '4f139c0a-9843-4dc4-b3e5-5aa00812e605'
concept_fr_regime               <- '62cd383d-165d-4b2a-96c1-a557d5c2bb6f'
concept_fr_estadio              <- 'e1e53c02-1d5f-11e0-b929-000c29ad1d07'
concept_fr_cd4                  <- '596e9d7c-1a17-4177-b4fb-76341dbee9dd'
concept_fr_carga_viral          <- 'e1d6247e-1d5f-11e0-b929-000c29ad1d07'



#Ficha Clinica concepts
concept_fc_weight               <- 'e1e2e826-1d5f-11e0-b929-000c29ad1d07'
concept_fc_height               <- 'e1e2e934-1d5f-11e0-b929-000c29ad1d07'
concept_fc_prof_inh             <- 'be4a76ca-662a-4c39-903b-71983f5f67c9'
concept_fc_pof_ctz              <- '2616b3c9-9a99-4b9a-b673-10871f4a4c71'
concept_fc_hivload_qualitativa  <- 'e1da2704-1d5f-11e0-b929-000c29ad1d07'
concept_fc_cd4                  <- 'e1dd5ab4-1d5f-11e0-b929-000c29ad1d07'
concept_fc_regime               <- 'e1d83d4a-1d5f-11e0-b929-000c29ad1d07'
concept_fc_estadio_oms          <- 'e1e53c02-1d5f-11e0-b929-000c29ad1d07'
concept_fc_carga_viral          <- 'e1d6247e-1d5f-11e0-b929-000c29ad1d07'
concept_fc_hemogl               <- 'e1dd574e-1d5f-11e0-b929-000c29ad1d07'
concept_fc_alt                  <- 'e1dd5942-1d5f-11e0-b929-000c29ad1d07'
concept_fc_creat                <- 'e1d7e19c-1d5f-11e0-b929-000c29ad1d07'
concept_fc_consul_prox          <- 'e1dae630-1d5f-11e0-b929-000c29ad1d07'
concept_fc_linhat               <- 'fdff0637-b36f-4dce-90c7-fe9f1ec586f0' 
concept_fc_tratamento_tb        <- 'e1d9fbda-1d5f-11e0-b929-000c29ad1d07'
concept_fc_lab_lam              <- 'ef139cb2-97c1-4c0f-9189-5e0711a45b8f'
concept_fc_lab_crag             <- 'f3883e12-4883-461b-8440-2a02f3312a84'
concept_fc_lab_hepatite         <- 'ef139cb2-97c1-4c0f-9189-5e0711a45b8f'

#Fila Concepts
concept_fila_regime                      <- "e1d83e4e-1d5f-11e0-b929-000c29ad1d07"
dispensedAmountUuid             <- "e1de2ca0-1d5f-11e0-b929-000c29ad1d07"
encounterType_fila                        <- "49857ace-1a92-4980-8313-1067714df151"
dosageUuid                      <- "e1de28ae-1d5f-11e0-b929-000c29ad1d07"
returnVisitUuid                 <- "e1e2efd8-1d5f-11e0-b929-000c29ad1d07"

#Lab concepts
concept_lab_alat <- 'e1d43c36-1d5f-11e0-b929-000c29ad1d07'
concept_lab_hepatite <-'' #not used
concept_lab_creatinine <- 'e1d5d21c-1d5f-11e0-b929-000c29ad1d07'
concept_lab_linfocitos <- 'e1d6dce8-1d5f-11e0-b929-000c29ad1d07'
concept_lab_cd4_numeric <- 'e1e68f26-1d5f-11e0-b929-000c29ad1d07'
concept_lab_cd4_percent <- 'e1d48fba-1d5f-11e0-b929-000c29ad1d07'
concept_lab_carga_viral <- 'e1d6247e-1d5f-11e0-b929-000c29ad1d07'
concept_lab_homogl <- 'e1cdbe88-1d5f-11e0-b929-000c29ad1d07'
concept_lab_data_exame <- 'f85e3f84-a255-412a-aa43-40174f69c305'
concept_lab_data_pedido_exame <- '892a98b2-9c98-4813-b4e5-0b434d14404d'
concept_lab_data_colheita_amostra <- 'f85e3f84-a255-412a-aa43-40174f69c305'
concept_lab_group_member_testagem_virologia <- 'e1dd03b6-1d5f-11e0-b929-000c29ad1d07'
concept_lab_group_member_bioquimica<- 'e1dd186a-1d5f-11e0-b929-000c29ad1d07'
concept_lab_group_member_imunologia <- 'e1dd2382-1d5f-11e0-b929-000c29ad1d07'
concept_lab_group_member_hemograma <- 'e1de3484-1d5f-11e0-b929-000c29ad1d07'
#concept_lab_group_member_imunologia <- 'e1dd2382-1d5f-11e0-b929-000c29ad1d07'

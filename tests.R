

lab <- readxl::read_xls(path = 'data/bloodexport.xls',col_names = TRUE)

df.lab <- filter(lab, nid==pat.nid) 



composeLab <- function(patient.uuid,df.lab ){
  
  index=1 
  pat_lab <- df.lab
  uuid <-patient.uuid
   

  if(nrow(pat_lab)!=0){
    
    # there are only 27 listed lab testts in the file
    for ( i in 1:27){
      
      examen       <- lab[[paste0("examen",i)]][index]

      if(!is.na(examen)) # no more lab requests {
        encounter_datetime <-examen
      
        alat     <- lab[[paste0("alat",i)]][index] 
        hbsag    <- lab[[paste0("hbsag",i)]][index]    #Hepatites b antigen s
        creatui  <- lab[[paste0("creatui",i)]][index]  #Creatinine umol/L
        lc       <- lab[[paste0("lc",i)]][index]       #Linfocitos
        lccd4    <- lab[[paste0("lccd4",i)]][index]    #cd4 Numerico
        lccd4tlc <- lab[[paste0("lccd4tlc",i)]][index] #cd4 Percentual
        hivload     <- lab[[paste0("hivload",i)]][index]
        hemoglb     <- lab[[paste0("hemoglb",i)]][index]

  
      }
   
      
    }
   
    
    } 
}
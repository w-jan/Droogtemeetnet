source("C:/R/Projecten/Watina/src/invoerMenyanthes_gauge.R")
library(tidyverse)
# getwd()

normalizePath(file.path("..", "Watina", "data","tbl_opnamen6230.csv" ))
brondata <- normalizePath(file.path("..","..","repositories",  "droogtemeetnet", "data","local", "TubesToInspect_202005.csv" ))
bestand <- normalizePath(file.path("..", "..", "Repositories", "habnorm", "data", "interim", "6410" , "6410_peilbuizen.csv" ))
brondata <- normalizePath(file.path("..","Watina", "data","tbl_opnamen6230.csv" ))
tubes_alles <- read_csv(brondata)


peilen <- hydromonitor_peilmeting_csv_formaat(lijstmeetpunten =  tubes_alles %>% select(loc_code) %>% filter(loc_code == "KBRP040"), 
                                              ookdefinities = TRUE,
                                              oppervlaktewater = FALSE
)
debugonce(hydromonitor_peilmeting_csv_formaat)
peilen <- hydromonitor_peilmeting_csv_formaat(gebied = "BTE", 
                                              ookdefinities = TRUE,
                                              oppervlaktewater = FALSE
)

techecken <- wt_nogtevalideren_peilmetingen %>% 
  count(MeetpuntCode) 
  
#voor een gebied
gebiedscode <- "BTE"
write_delim(as.data.frame(peilen), 
            paste0("G:/Mijn Drive/Habnorm/Tijdreeksanalyse/Menyanthes_Peilgegevens/Hydromonitor_peilmetingen_",tolower(gebiedscode),".csv"), 
            col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")

#voor een meetpuntenlijst
write_delim(as.data.frame(peilen), 
            normalizePath(file.path("..","..","repositories",  "droogtemeetnet", "data","local","tijdreeksen","importbestanden_menyanthes", "Hydromonitor_peilmetingen_202005_kbrp040.csv" )), 
            col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")


C:\R\Repositories\droogtemeetnet\data\local\tijdreeksen\importbestanden_menyanthes
peilen_opp <- hydromonitor_peilmeting_csv_formaat(gebiedscode,
                                                  ookdefinities = TRUE,
                                              oppervlaktewater = TRUE
)

write_delim(as.data.frame(peilen), 
            paste0("./data/local/tijdreeksen/importbestanden_menyanthes/Hydromonitor_peilmetingen_heide_voorbeeld",".csv"), 
            col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")

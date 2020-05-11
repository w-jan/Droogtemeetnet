source("C:/R/Projecten/Watina/src/invoerMenyanthes_gauge.R")
library(tidyverse)
getwd()
normalizePath(file.path("..", "..","Projecten","Watina", "data","Menyanthes_heide.csv" ))

gebiedscode <- "DYL"
debugonce(hydromonitor_peilmeting_csv_formaat)
brondata <- normalizePath(file.path("data", "Menyanthes_heide.csv" ))
tubes_alles <- read_csv(brondata)
tubes_1B_nolgl3 <- read_csv(normalizePath(file.path("data","local", "tubes_1B_nolgl3.csv" )))

tubes_alles_cat2 <- read_vc(file.path("data", "tubes_cat2_run1" ))
tubes_alles_cat3 <- read_vc(file.path("data", "tubes_cat3_run1" ))

tubes_alles <- bind_rows(tubes_alles_cat2 %>% select(loc_code), tubes_alles_cat3 %>% select(loc_code))


write_csv(tubes_alles, file.path("data", "tubes_alles_20191112.csv" ))
tubes_add <- data.frame(Watinacode = c("MATP004", "ZWAP309"))
tubes_add <- tubes_add %>% 
  mutate (watinacode = as.character(Watinacode))

wt_nogtevalideren_peilmetingen %>% 
  count(PeilpuntCode) %>% 
  arrange(desc(n))
peilen <- hydromonitor_peilmeting_csv_formaat(lijstmeetpunten =  tubes_alles %>% filter(watina_meetpunt =="ZWAP307"), 
                                              ookdefinities = TRUE,
                                              oppervlaktewater = FALSE
)

peilen <- hydromonitor_peilmeting_csv_formaat(lijstmeetpunten =  pb6410_15m, 
                                              ookdefinities = TRUE,
                                              oppervlaktewater = FALSE
)

techecken <- wt_nogtevalideren_peilmetingen %>% 
  count(MeetpuntCode) 
  
#voor een gebied
write_delim(as.data.frame(peilen), 
            paste0("G:/Mijn Drive/Habnorm/Tijdreeksanalyse/Menyanthes_Peilgegevens/Hydromonitor_peilmetingen_",tolower(gebiedscode),".csv"), 
            col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")

#voor een meetpuntenlijst
write_delim(as.data.frame(peilen), 
            paste0("G:/Mijn Drive/Habnorm/Tijdreeksanalyse/Menyanthes_Peilgegevens/Hydromonitor_peilmetingen_6410_20191127.csv"), 
            col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")


peilen_opp <- hydromonitor_peilmeting_csv_formaat(gebiedscode,
                                                  ookdefinities = TRUE
                                                  
                                                                              ,
                                              oppervlaktewater = TRUE
)

write_delim(as.data.frame(peilen), 
            paste0("./data/local/tijdreeksen/importbestanden_menyanthes/Hydromonitor_peilmetingen_heide_voorbeeld",".csv"), 
            col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")

source("G:/Mijn Drive/PRJ_GRONDWATERGEGEVENS/Tijdreeksanalyse/VoerVoorMenyanthes.R")
source("C:/R/Projecten/Watina/src/invoerMenyanthes_gauge.R")
#opgaven van een gebiedscode
localpath_output <- "./data/local/tijdreeksen/importbestanden_menyanthes/"

#je hebt de keuze tussen ophalen van alle grond- of oppervlaktewaterpeilen van een gebied
#of van de peilen voor een opgegeven lijst van meetpunten (watinacode van 7 karakters)


#ophalen grondwaterpeilen voor een gebied
gebiedscode <- "OSK"
peilen <- hydromonitor_peilmeting_csv_formaat(gebiedscode, 
                                              ookdefinities = TRUE,
                                              oppervlaktewater = FALSE
                                              )
#wegschrijven grondwaterpeilen in hydromonitor-formaat
write_delim(as.data.frame(peilen), 
            paste0(localpath_output, "Hydromonitor_peilmetingen_",tolower(gebiedscode),".csv"), 
            col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")


#ophalen oppervlaktewaterpeilen voor een gebied
peilen_opp <- hydromonitor_peilmeting_csv_formaat(gebiedscode,
                                                  ookdefinities = TRUE,
                                                  oppervlaktewater = TRUE
                                                  )

#wegschrijven oppervlaktewaterpeilen in hydromonitor-formaat
write_delim(as.data.frame(peilen_opp), 
            paste0(localpath_output,"Hydromonitor_peilmetingen_",tolower(gebiedscode),"_opp.csv"), 
            col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")


#ophalen grondwaterpeilen voor een lijst meetpunten
getwd()
tubes_group4 <- git2rdata::read_vc(file.path(".", "data", "tubes_group4"))
techecken <- readRDS("test.Rds")
techecken_groep <- techecken %>% 
  count(MeetpuntCode)
meetpunten <- tubes_group4 %>% 
  filter (stringr::str_sub(loc_code,1,3) == "LDO")
peilen <- hydromonitor_peilmeting_csv_formaat(lijstmeetpunten = meetpunten, 
                                              ookdefinities = TRUE,
                                              oppervlaktewater = FALSE
)
#wegschrijven grondwaterpeilen in hydromonitor-formaat
write_delim(as.data.frame(peilen), 
            paste0(localpath_output, "Hydromonitor_peilmetingen_ldo.csv"), 
            col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")
write_delim(as.data.frame(peilen), 
            paste0(localpath_output, "Hydromonitor_peilmetingen_tubes_group4.csv"), 
            col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")

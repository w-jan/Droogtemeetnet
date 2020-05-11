source("G:/Mijn Drive/PRJ_GRONDWATERGEGEVENS/Tijdreeksanalyse/VoerVoorMenyanthes.R")

#opgaven van een gebiedscode
localpath_output <- "./data/local/tijdreeksen/importbestanden_menyanthes/"

#je hebt de keuze tussen ophalen van alle grond- of oppervlaktewaterpeilen van een gebied
#of van de peilen voor een opgegeven lijst van meetpunten (watinacode van 7 karakters)


#ophalen grondwaterpeilen voor een gebied
gebiedscode <- "DYL"
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
meetpunten <- tubes_group3
peilen <- hydromonitor_peilmeting_csv_formaat(lijstmeetpunten = meetpunten, 
                                              ookdefinities = TRUE,
                                              oppervlaktewater = FALSE
)
#wegschrijven grondwaterpeilen in hydromonitor-formaat
write_delim(as.data.frame(peilen), 
            paste0(localpath_output, "Hydromonitor_peilmetingen_tubes_group3.csv"), 
            col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")

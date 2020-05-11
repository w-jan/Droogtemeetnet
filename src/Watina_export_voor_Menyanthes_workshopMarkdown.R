source("G:/Mijn Drive/PRJ_GRONDWATERGEGEVENS/Tijdreeksanalyse/VoerVoorMenyanthes.R")
#source("C:/R/Projecten/Watina/src/invoerMenyanthes_gauge.R")
library(git2rdata)
library(tidyverse)
library(ggplot2)
#opgaven van een gebiedscode
localpath_output <- "./data/local/tijdreeksen/importbestanden_menyanthes/"

#je hebt de keuze tussen ophalen van alle grond- of oppervlaktewaterpeilen van een gebied
#of van de peilen voor een opgegeven lijst van meetpunten (watinacode van 7 karakters)


#ophalen grondwaterpeilen voor een gebied
gebiedscode <- "SIL"

#ophalen grondwaterpeilmetingen uit de watina-databank en transformeren in het hydromonitor-formaat, een formaat dat kan ingelezen worden in Menyanthes
peilen_export <- hydromonitor_peilmeting_csv_formaat(gebiedscode, 
                                              ookdefinities = TRUE,
                                              oppervlaktewater = FALSE
                                              )

#ophalen grondwaterpeilmetingen uit de watina-databank om deze te kunnen analyseren
peilen <- import_watina_peilmetingen(gebiedscode, ookdefinities = TRUE, oppervlaktewater = FALSE)

write_vc(peilen, "peilen_workshop", sorting = c("Name", "DateTime","ManualHead"), strict= FALSE )
#blijkbaar zitten er enkele dubbels in

# backup
peilen.bu <- peilen
peilen <- peilen.bu

#veldnamen aanpassen
peilen <- peilen %>%
  rename(meetpunt = Name,
         filternr = FilterNo,
         dag = DateTime,
         logger_head = LoggerHead,
         manual_head = ManualHead)

# conversie veld DateTime (tekst) naar een datum-veld, meetpunt naar een factor
peilen <- peilen %>% 
  mutate(dag = lubridate::dmy_hm(dag),
         meetpunt =  factor(meetpunt)) %>% 
  distinct(meetpunt, filternr, dag, logger_head, manual_head)


#samenvattende tabel
summary(peilen)

#velden filternr en logger_head kunnen verwijderd worden
peilen <- peilen %>% 
  select(-filternr, -logger_head)

#overzichtstabel
peilen.overzicht <- peilen %>% 
  group_by(meetpunt) %>% 
  summarise(aantal_metingen = n(),
            head.min = min(manual_head),
            head.q10 = quantile(manual_head,0.10),            
            head.q25 = quantile(manual_head,0.25),
            head.median = median(manual_head),
            head.mean = mean(manual_head),
            head.q75 = quantile(manual_head, 0.75),
            head.q90 = quantile(manual_head,0.90),
            head.max = max(manual_head)
            )
  
#grafiek met een tijdreeks van één meetpunt
keuze_meetpunt <- "SILP001"
peilen.meetpunt <- peilen %>% 
  filter(meetpunt == keuze_meetpunt)
peil.grafiek <- ggplot(peilen.meetpunt, aes(x= dag, y = manual_head)) + 
  geom_line()
peil.grafiek

#boxplot van de meetpunten

peilen.boxplot <- ggplot(peilen, aes(x = meetpunt, y = manual_head))+
  geom_boxplot()
peilen.boxplot
#dit trekt op niet veel, omdat er voor sommige meetpunten de meetwaarden tov maaiveld werden uitgedrukt (door gebrek aan een positiebepaling) en voor andere meetpunten ze in mTAW zijn uitgedrukt: twee verschillende boxplots maken

peilen.boxplot.mv <- ggplot(peilen %>% 
                              filter(stringr::str_sub(as.character(meetpunt), 7,7) %in% c("1","5","6")
                                     ) , aes(x = meetpunt, y = manual_head))+
  geom_boxplot() + labs(y = "grondwaterpeil tov maaiveld (m)")
peilen.boxplot.mv

peilen.boxplot.taw <- ggplot(peilen %>% 
                              filter(stringr::str_sub(as.character(meetpunt), 7,7) %in% c("2","3","4")
                              ) , aes(x = meetpunt, y = manual_head))+
  geom_boxplot() + labs(y = "grondwaterpeil (mTAW)")
peilen.boxplot.taw

#wegschrijven grondwaterpeilen in hydromonitor-formaat
write_delim(as.data.frame(peilen_export), 
            paste0(localpath_output, "Hydromonitor_peilmetingen_",tolower(gebiedscode),".csv"), 
            col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")


#optioneel voor workshop
# 
# #ophalen oppervlaktewaterpeilen voor een gebied
# peilen_opp <- hydromonitor_peilmeting_csv_formaat(gebiedscode,
#                                                   ookdefinities = TRUE,
#                                                   oppervlaktewater = TRUE
#                                                   )
# 
# #wegschrijven oppervlaktewaterpeilen in hydromonitor-formaat
# write_delim(as.data.frame(peilen_opp), 
#             paste0(localpath_output,"Hydromonitor_peilmetingen_",tolower(gebiedscode),"_opp.csv"), 
#             col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")
# 
# 
# #ophalen grondwaterpeilen voor een lijst meetpunten
# getwd()
# tubes_group4 <- git2rdata::read_vc(file.path(".", "data", "tubes_group4"))
# techecken <- readRDS("test.Rds")
# techecken_groep <- techecken %>% 
#   count(MeetpuntCode)
# meetpunten <- tubes_group4 %>% 
#   filter (stringr::str_sub(loc_code,1,3) == "LDO")
# peilen <- hydromonitor_peilmeting_csv_formaat(lijstmeetpunten = meetpunten, 
#                                               ookdefinities = TRUE,
#                                               oppervlaktewater = FALSE
# )
# #wegschrijven grondwaterpeilen in hydromonitor-formaat
# write_delim(as.data.frame(peilen), 
#             paste0(localpath_output, "Hydromonitor_peilmetingen_ldo.csv"), 
#             col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")
# write_delim(as.data.frame(peilen), 
#             paste0(localpath_output, "Hydromonitor_peilmetingen_tubes_group4.csv"), 
#             col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")

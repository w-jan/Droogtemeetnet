options(stringsAsFactors = FALSE)
# library(plyr)

library(stringr)
library(knitr)
library(sf)
library(units)
library(git2rdata)
library(lubridate)

# install watina-package branch develop_fv
# remotes::install_github("inbo/watina",
#                         ref = "develop_fv")
library(watina)
# install n2khab-package
# remotes::install_github("inbo/n2khab",
#                         build_opts = c("--no-resave-data", "--no-manual"))
library(n2khab)
library(raster)
library(tmap)
library(tidyverse)
library(kableExtra)
library(knitr)
library(googledrive)
library(RSQLite) 

#gewenste grootte van het meetnet
tot_n_tub <- 100

#aantal stratificatielagen
aantal_strat <- 5

#verdeling meetpunten over de stratificatielagen
minaantal_tub_group <- as.integer(tot_n_tub/aantal_strat)

#kwaliteitscriteria meetreeksen
#minimale lengte van de tijdreeks 
minlength <- 5
#maximale duur van een onderbreking van de tijdreeks 
maxgap <- 2
#binnen een tijdreeks minimaal aantal meetjaren waarvoor een lg3 kan berekend worden
minnryears <- 5

#aanvullende criteria om meetreeksen te vergelijken
toelaatbare_spreiding_jaren <- 5
toelaatbaar_verschil_lengte_tijdreeks <- 5

#zoekstraal rond peilbuizen (buffer)
bufferpb <- 3 

#peilbuizen niet zeker niet kunnen opgenomen worden, bijv. geen toelating, omdat het te dicht bij een ander geselecteerd punt ligt
#uitgesloten_tubes <- c("MOSP001", "HALP005", "BUIP027")
uitgesloten_tubes <- "leeg"


#inladen gegevens (niet verplicht, enkel nodig bij updates van de brondata)
gw_types <- read_scheme_types(lang = "nl") %>%
  filter(scheme == "GW_05.1_terr") %>%
  arrange(typegroup) %>%
  mutate(groupnr = as.integer(str_sub(typegroup, -1))) %>% 
  dplyr::select(type, groupnr, typegroup_name)
output_vc <- write_vc(gw_types, file.path(".","data","gw_types"), sorting = c("type"), strict =  FALSE)

types <- read_types(lang = "nl")
output_vc <- write_vc(types, file.path(".","data","types"), sorting = c("type"), strict =  FALSE)


rm(output_vc)

habmap_terr <- read_habitatmap_terr()

habmap_polygons <- habmap_terr$habitatmap_terr_polygons

habmap_types <- habmap_terr$habitatmap_terr_types
# habfile <- "20_processed/habitatmap_terr/habitatmap_terr.gpkg"
# 
# habmap_stdized <- read_habitatmap_stdized(file = habfile)
# 
# habmap_polygons <- habmap_stdized$habitatmap_polygons
# 
# habmap_patches <- habmap_stdized$habitatmap_patches

habmap_types <- habmap_types %>%
  mutate( polygon_id = as.factor(.data$polygon_id),
          certain = .data$certain == 1,
          type = factor(.data$type,
                        levels = levels(types$type)
          )
  )

# Verspreiding van de verdrogingsgevoelige typen in Vlaanderen volgens de habitatkaart.
habmap_types_gw <- habmap_types %>% 
  inner_join(gw_types, by = c("type" = "type"))

habmap_polygons_gw <- 
  habmap_polygons %>% 
  inner_join(habmap_types_gw %>% 
               dplyr::select(-code_orig), 
             by = "polygon_id")



#wegschrijven als geopackage
datapath <- "G:/Mijn Drive/PRJ_Meetnet_Droogte/2_Uitvoering/data"
dir.create(file.path(datapath, "GIS/VoorR/"), recursive = TRUE)

st_write(habmap_polygons_gw,
         file.path(datapath, 
                   "GIS/VoorR/habmap_terr_gw.gpkg"), 
         layer = "habitatmap_terr_polygons_gw", 
         driver = "GPKG",
         delete_dsn = TRUE)

con = dbConnect(SQLite(),
                dbname = file.path(
                  datapath, 
                  "GIS/VoorR/habmap_terr_gw.gpkg")
)

dbWriteTable(con, "habitatmap_terr_types_gw", habmap_types_gw)



#GRTS-data
raster_meetnet_poly <- read_GRTSmh_diffres(level = 8, polygon = TRUE)

dbWriteTable(con, "raster_meetnet_poly", raster_meetnet_poly)


#overlay maken hab-kaart en GRTS-rooster
habmap_gw_raster_overlay <- habmap_polygons_gw %>% 
  st_intersection(raster_meetnet_poly)

habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>% 
  mutate(opp = as.integer(st_area(habmap_gw_raster_overlay))) 

#wegschrijven van deze intersectie


dbWriteTable(con, "habmap_gw_raster_overlay", habmap_gw_raster_overlay)
dbDisconnect(con)

#inlezen grts-raster level 1 (hoogste resolutie = kleinste gridcelgrootte)
grts_level1 <- read_GRTSmh(brick = TRUE) %>% 
  raster::subset(1)

#inlezen grts-raster level 9 (resolutie = raster_meetnet_poly), het heeft een gridgrootte van 8192 m, let wel de rastercelgrootte is ook hier 32 bij 32m, dus het aantal rastercellen = grts-raster level 1. 
grts_level9 <- read_GRTSmh(brick = TRUE) %>% 
  raster::subset(9)

writeRaster(grts_level9,
            filename = file.path(datapath, "GIS/VoorR/grts_level9.tif"), 
            format = "GTiff", 
            datatype = "INT4S",
            overwrite = TRUE)

writeRaster(grts_level1,
            filename = file.path(datapath, "GIS/VoorR/grts_level1.tif"), 
            format = "GTiff", 
            datatype = "INT4S",
            overwrite = TRUE)


watina <- connect_watina()
tubes_hab <- get_locs(watina, mask = habmap_gw_raster_overlay, join_mask = TRUE,
                      buffer = bufferpb, loc_type = "P", loc_validity = c("VLD", "ENT"), collect = TRUE)


#beperken van peilbuizen tot de rastercel waar ze effectief in liggen. Door het gebruik van een buffer is het immers mogelijk dat een peilbuis in twee of meer cellen komt te liggen.
tubes_hab_sf <- as_points(tubes_hab)

#overlay van peilbuizen met het raster
tubes_hab_gw_raster_overlay <- tubes_hab_sf %>% 
  distinct(loc_code, x, y) %>% 
  st_intersection(raster_meetnet_poly)

tubes_hab <- tubes_hab %>% 
  semi_join(tubes_hab_gw_raster_overlay, by = c("loc_code","rasterid"))

#save tubes_hab as a git2rdata-object
tubes_hab <- tubes_hab %>% 
  arrange(loc_code, polygon_id, rasterid, patch_id, type)

output_vc <- write_vc(tubes_hab, file.path(".","data","tubes_hab"), sorting = c("loc_code", "polygon_id", "rasterid", "patch_id", "type" ), strict =  FALSE)
rm(output_vc)


tubes_xg3 <- tubes_hab %>% 
  get_xg3(watina, startyear = year(now()) - 18, endyear = 2016, vert_crs = "local",
          truncated =  TRUE, collect = TRUE)

#oplossen van UTF-probleem: strings worden in inbo-SQL-databanken opgeslagen in UTF-16, terwijl hier gewerkt wordt met UTF-8. Dit geeft een probleem bij de kable-functie

#tubes_xg3 <- mutate_if(tubes_xg3, is.character, iconv, to = "UTF-8")

tubes_xg3 <- tubes_xg3 %>% 
  arrange(loc_code, hydroyear)

output_vc <- write_vc(tubes_xg3, file.path(".","data","tubes_xg3"), sorting = c("loc_code", "hydroyear"),
                      strict =  FALSE, root = ".")


DBI::dbDisconnect(watina)

#volgende watina-functie werkt ook zonder databankconnectie, maar omdat deze op sommige pc's niet stabiel werkt, worden de data hier alvast lokaal weggeschreven.

tubes_lg3_eval <- tubes_xg3 %>%
  eval_xg3_series(xg3_type = c("L"),
                  max_gap = maxgap,
                  min_dur = minlength)

tubes_lgl_eval <- tubes_lg3_eval %>%
  filter(ser_nryears >= minnryears)

output_vc <- write_vc(tubes_lgl_eval, file.path(".","data","tubes_lgl_eval"), sorting = c("loc_code", "hydroyear"),
                      strict =  FALSE, root = ".")

rm(output_vc)

#einde inladen



#raster_meetnet_poly <- read_GRTSmh_diffres(level = 8, polygon = TRUE)
drive_download(drive_get(id = "1oHdlUEEZmCDvXDCSXELgtgKNDgLn4E_0"), path = file.path(".","data","local", "raster_meetnet_poly.gpkg"), overwrite = TRUE)
raster_meetnet_poly <- suppressWarnings(read_sf(file.path(".","data","local", "raster_meetnet_poly.gpkg"), "raster_meetnet_poly"))

#oppervlakte van elk hok berekenen
raster_meetnet_poly_opp <- raster_meetnet_poly %>% 
  mutate(opp = as.integer(st_area(raster_meetnet_poly))) %>% 
  st_drop_geometry() %>% 
  dplyr::select(value, opp) %>% 
  group_by(value) %>% 
  summarise(totopp = sum(opp))

#welke crs?
#st_crs(habmap_polygons_gw) #lambert
#plot(raster_meetnet_poly, main = "GRTS-raster level 8")
#st_is_valid(raster_meetnet_poly)
raster_meetnet_poly <- lwgeom::st_make_valid(raster_meetnet_poly)
#st_is_valid(raster_meetnet_poly)

raster_meetnet_poly_tm <- tm_shape(raster_meetnet_poly) + 
  tm_polygons() + tm_layout(title = "GRTS-raster level 8 (8192m)" )

raster_meetnet_poly_tm
#st_crs(raster_meetnet_poly) #lambert


#check op unieke celwaarden
check <- raster_meetnet_poly %>% st_drop_geometry() %>% count(value) %>% filter(n > 1)
#van bepaalde cellen zijn er dus meerdere polygonen, dit zijn rasters die door de gewestgrens verdeeld werden (bijv. streek van Baarle-Nassau) 

raster_meetnet_poly <- raster_meetnet_poly %>% 
  rename(rasterid =  value)



# overlay maken van de habitatkaart (enkel van GT-groepen) en het GTRS-raster (level8)
# habmap_gw_raster_overlay <- habmap_polygons_gw %>% 
#   st_intersection(raster_meetnet_poly)
# 
# habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>% 
#     mutate(opp = as.integer(st_area(habmap_gw_raster_overlay))) 
drive_download(drive_get(id = "1oY7fXj7Kd59w1LFHhu88E9cLLkC5cPJS"), path = file.path(".","data","local", "habmap_gw_raster_overlay.gpkg"), overwrite = TRUE)
habmap_gw_raster_overlay <- suppressWarnings(read_sf(file.path(".","data","local", "habmap_gw_raster_overlay.gpkg"), "habmap_gw_raster_overlay"))

habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>% 
  rename(rasterid =  value)

habmap_gw_raster_overlay <- lwgeom::st_make_valid(habmap_gw_raster_overlay)
#plot(habmap_gw_raster_overlay, main = "Voorkomen van ")

habmap_gw_raster_overlay_tm <- raster_meetnet_poly_tm + 
  tm_shape(habmap_gw_raster_overlay) + 
  tm_fill(col = "groupnr", style = "cat", palette = "BuGn", title = "Grondwatertype") + 
  tm_layout(title = "Voorkomen GT-groepen" )

# interactieve modus, maar vraagt veel computertijd en genereert ook een mega html-bestand
# tmap_mode("view")
# statische modus, dus niet inzoombaar
# tmap_mode("plot")

habmap_gw_raster_overlay_tm


#oppervlakte gw-groep per rastercel
raster_gw_opp <- habmap_gw_raster_overlay %>% 
  st_drop_geometry() %>% 
  group_by(rasterid,groupnr) %>% 
  summarise(opp_gw_cel = sum(opp*phab/100) %>% set_units("m^2") %>% set_units("ha")) %>% 
  ungroup()
# view(raster_gw_opp)

#totale opp van een gw-groep
gw_opp <- raster_gw_opp %>% 
  group_by(groupnr) %>% 
  summarise(opp_gw = sum(opp_gw_cel)) %>% 
  ungroup()





###Berekening gemiddelde benodigde oppervlakte van een GT-groep per meetpunt



#minaantal_tub_group <- as.integer(tot_n_tub/aantal_strat)
min_aantal_tub = data.frame("groupnr" = 1:aantal_strat, minaantal = seq(minaantal_tub_group,minaantal_tub_group,length.out = aantal_strat))

for (group in seq(1,aantal_strat)) {
  #group <- 5
  minaantal_tub_group <- min_aantal_tub[min_aantal_tub$groupnr == group,"minaantal"]
  aantal_tub_group <- 0
  corrafronding <- 1
  gw_opp <- gw_opp %>% 
    left_join(gw_opp %>% 
                filter(groupnr == group) %>% 
                mutate(bewerkt = TRUE) %>% 
                select(groupnr, bewerkt)
              , by = "groupnr")
  
  while (aantal_tub_group < minaantal_tub_group) {
    if (group == 1){
      gw_opp <- gw_opp %>% 
        mutate(minopp = 
                 ifelse(bewerkt == TRUE, round(opp_gw/minaantal_tub_group * corrafronding,0), minopp) %>% set_units("ha")
        )
    } else {
      gw_opp <- gw_opp %>% 
        mutate(minopp = 
                 if_else(bewerkt == TRUE, round(opp_gw/minaantal_tub_group * corrafronding,0), minopp, missing = minopp) %>% set_units("ha")
        )
    }    
    
    gw_opp
    
    #berekening van het aantal meetpunten per cel
    
    aantal_meetpunten_cel_group <- 
      raster_gw_opp %>% 
      inner_join(gw_opp, 
                 by = "groupnr") %>% 
      mutate(gew_aantal_meetptn = as.numeric(opp_gw_cel/minopp)) %>% 
      arrange(desc(gew_aantal_meetptn))
    
    aantal_meetpunten_cel_group
    
    aantal_meetpunten_cel_group <- aantal_meetpunten_cel_group %>% 
      filter(gew_aantal_meetptn >= 0.5, groupnr == group) %>% 
      mutate(gew_aantal_meetptn_afgerond = round(gew_aantal_meetptn + 0.01, digits = 0) )
    
    df <- aantal_meetpunten_cel_group %>% 
      filter(groupnr == group) %>% 
      summarise(aantal = sum(gew_aantal_meetptn_afgerond)) %>% 
      ungroup() %>% 
      summarise(minaantal = min(aantal)) %>% 
      ungroup() 
    
    aantal_tub_group <- as.integer(df[1])
    corrafronding <- corrafronding - 0.01 
    
    
  }
  
  if (group == 1){
    gw_opp <- gw_opp %>% 
      mutate(correctiefactor = 
               ifelse(bewerkt == TRUE, 
                      as.numeric(round(minopp*minaantal_tub_group/opp_gw, 2)), 
                      correctiefactor))
  } else {
    gw_opp <- gw_opp %>% 
      mutate(correctiefactor = 
               if_else(bewerkt == TRUE, 
                       as.numeric(round(minopp*minaantal_tub_group/opp_gw, 2)), 
                       correctiefactor, missing = correctiefactor))
  }
  
  gw_opp <- gw_opp %>% 
    select(-bewerkt)   
  
  if (group == 1){
    aantal_meetpunten_cel <- aantal_meetpunten_cel_group
  } else {
    aantal_meetpunten_cel <- aantal_meetpunten_cel %>% 
      full_join(aantal_meetpunten_cel_group %>% 
                  select(-correctiefactor))
  }
}
# df <- aantal_meetpunten_cel %>% group_by(groupnr)  %>% 
#    summarise(aantal  = sum(gew_aantal_meetptn_afgerond)) %>% 
#    ungroup()
# df


aantal_meetpunten_cel <- aantal_meetpunten_cel %>% 
  select(-bewerkt)
# df <- aantal_meetpunten_cel %>% group_by(groupnr)  %>% 
#    summarise(aantal  = sum(gew_aantal_meetptn_afgerond)) %>% 
#    ungroup()
# df


aantal_meetpunten_cel_overzicht <- aantal_meetpunten_cel %>% 
  group_by(groupnr) %>% 
  summarise("totaal aantal meetptn" = sum(gew_aantal_meetptn_afgerond))


# Twv de ruimtelijke balancering wordt bij voorkeur voor de gw-groepen waar het minimum aantal wordt overschreden, dit aantal tot het minimum teruggebracht door de hokken te selecteren op basis van hun rangorde in het GTRS-raster. Men kan echter ook de koppeling met geschikte watina-meetptn afwachten. 
# We doen hier eerst de aanbevolen GRTS-strategie
# if (df[df$groupnr == 1,"aantal"] > minaantal_tub_group) {}
# 
# sel_raster <- raster_meetnet_poly %>% 
#   st_drop_geometry()  %>% 
#   inner_join(raster_gw_opp, by = "rasterid") %>% 
#   inner_join(aantal_meetpunten_cel_group, by =  c("rasterid", "groupnr")) %>% 
#   inner_join(min_aantal_tub, by = "groupnr") %>% 
#   group_by(groupnr) %>% 
#   distinct(rasterid, minaantal) %>% 
#   top_n(minaantal, rasterid)
# 
# sel_raster_meetnet <- raster_meetnet_poly %>% 
#   inner_join(sel_raster, by = "rasterid")
# 
# plot(sel_raster_meetnet)




#cellen selecteren met een gewenst meetpunt, een cel kan voor meerdere GT-groepen geselecteerd zijn
sel_raster_meetnet <- 
  raster_meetnet_poly %>%
  inner_join(raster_gw_opp, by = "rasterid") %>%
  inner_join(aantal_meetpunten_cel %>% 
               select(rasterid, groupnr, gew_aantal_meetptn_afgerond), 
             by =  c("rasterid", "groupnr")) %>% 
  rename(gew_aantal_meetptn = gew_aantal_meetptn_afgerond)

sel_raster_meetnet <- lwgeom::st_make_valid(sel_raster_meetnet)

#groeperen van gefragmenteerde rastercellen
sel_raster_meetnet <- 
  sel_raster_meetnet  %>% 
  group_by(rasterid, groupnr, opp_gw_cel, gew_aantal_meetptn) %>% 
  summarise (temp = n()) %>% 
  ungroup %>% 
  select(-temp)

#uit voorzorg nog eens de geometrie checken
sel_raster_meetnet <- lwgeom::st_make_valid(sel_raster_meetnet)
#st_is_valid(raster_meetnet_poly)

sel_raster_meetnet_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_raster_meetnet %>% filter(groupnr == 1)) + 
  tm_polygons(col = "red") + tm_layout(title = "GT-groep 1" )

sel_raster_meetnet_tm
# voorbeeld van rasters van GT-groep 1 (permanent nat)
#plot(sel_raster_meetnet %>% filter(groupnr == 1))


sel_raster_meetnet_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_raster_meetnet %>% filter(groupnr == 2)) + 
  tm_polygons(col = "red") + tm_layout(title = "GT-groep 2" )

sel_raster_meetnet_tm


sel_raster_meetnet_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_raster_meetnet %>% filter(groupnr == 3)) + 
  tm_polygons(col = "red") + tm_layout(title = "GT-groep 3" )

sel_raster_meetnet_tm


sel_raster_meetnet_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_raster_meetnet %>% filter(groupnr == 4)) + 
  tm_polygons(col = "red") + tm_layout(title = "GT-groep 4" )

sel_raster_meetnet_tm


sel_raster_meetnet_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_raster_meetnet %>% filter(groupnr == 5)) + 
  tm_polygons(col = "red") + tm_layout(title = "GT-groep 5" )

sel_raster_meetnet_tm


sel_raster_meetnet_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_raster_meetnet) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "alle groepen" )

sel_raster_meetnet_tm

## Selecteren van grondwater-meetpunten


### Opgave van de meetpunten (Watina-databank) die gelegen zijn in een verdrogingsgevoelig type.  


# watina <- connect_watina()

# tubes_hab <- get_locs(watina, mask = habmap_gw_raster_overlay, join_mask = TRUE,
#                       buffer = bufferpb, loc_type = "P", loc_validity = c("VLD", "ENT"), collect = TRUE)
# 
# 
# #beperken van peilbuizen tot de rastercel waar ze effectief in liggen. Door het gebruik van een buffer is het immers mogelijk dat een peilbuis in twee of meer cellen komt te liggen.
# tubes_hab_sf <- as_points(tubes_hab)
# 
# #overlay van peilbuizen met het raster
# tubes_hab_gw_raster_overlay <- tubes_hab_sf %>% 
#   distinct(loc_code, x, y) %>% 
#   st_intersection(raster_meetnet_poly)
# 
# tubes_hab <- tubes_hab %>% 
#   semi_join(tubes_hab_gw_raster_overlay, by = c("loc_code","rasterid"))
# 
# #save tubes_hab as a git2rdata-object
# tubes_hab <- tubes_hab %>% 
#   arrange(loc_code, polygon_id, rasterid, patch_id, type)
# 
# output_vc <- write_vc(tubes_hab, file.path(".","data","tubes_hab"), sorting = c("loc_code", "polygon_id", "rasterid", "patch_id", "type" ), strict =  FALSE)
# rm(output_vc)

# str(tubes_hab)

tubes_hab <- read_vc(file.path(".","data","tubes_hab"))

#peilbuizen uit de lijst verwijderen die niet (meer) mogen meegenomen worden
if (uitgesloten_tubes != "leeg"){
  tubes_hab <- tubes_hab[!(tubes_hab$loc_code %in% uitgesloten_tubes),]
}

# een peilbuis kan meerdere keren voorkomen, namelijk wanneer de pb in een habitat-complex ligt en wanneer de bufferopp meerdere polygonen doorsnijdt. We kunnen ze groeperen als de verschillende eenheden tot eenzelfde gw-groep behoren.
# Alleen de gw-groep met een opp-aandeel van minstens 50% wordt weerhouden. Dit om te vermijden dat indien een rastercel gekozen werd voor een bep. gw-groep, een pb geselecteerd wordt waarvan de kans klein is dat ze die gw-groep representeert.

#oplossen van habitatcomplexen
tubes_hab_groep <- tubes_hab %>%
  group_by(loc_code, polygon_id, rasterid, groupnr) %>%
  summarise(phab_gw = sum(phab),
            aantal =  n()) %>%
  ungroup() %>% 
  filter(phab_gw >= 50)

test <- tubes_hab_groep %>% 
  distinct(loc_code, groupnr) %>% 
  count(loc_code) %>% 
  filter(n > 1)

#oplossen van meerdere polygonen
tubes_hab_multipolyg <- tubes_hab_groep %>% 
  semi_join(tubes_hab_groep %>% 
              distinct(loc_code, groupnr) %>% 
              count(loc_code) %>% 
              filter(n == 1),
            by = "loc_code")

tubes_hab_aggr <- tubes_hab %>%
  select(-patch_id,-phab, -certain, -type, -source.y, -polygon_id, -starts_with("description"), -opp, -source.x) %>% 
  distinct %>% 
  semi_join(tubes_hab_multipolyg, by = c("loc_code", "rasterid", "groupnr" ))



### Opzoeken van peilbuizen in de geselecteerde rastercellen

tubes_in_raster <- tubes_hab_aggr %>% 
  # select(-opp, -description_orig, -source.x, -starts_with("loc_"), loc_code) %>%  
  inner_join(sel_raster_meetnet %>% 
               select(rasterid, groupnr) %>% 
               st_drop_geometry(), by = c("rasterid", "groupnr")) %>% 
  distinct()




### Opzoeken van peilbuizen met een goede tijdreeks binnen de geselecteerde cellen

# watina <- connect_watina()
# minlength <- 5 #jaar
# maxgap <- 2 #jaar
# minnryears <- 5 #jaar

# alle bestaande peilbuizen : tubes_in_raster
# pb met een xg3 (hoeft geen lg3 te zijn) binnen tijdsruimte: tubes_xg3_avail
# pb met min. 1 lg3 binnen tijdsruimte: tubes_lg3_avail
# pb waarvoor een lgl kan berekend worden: tubes_lgl_eval

#voor elke pb de xg3 waarden ophalen (meerdere rec per pb)
# tubes_xg3 <- tubes_in_raster %>% 
#     get_xg3(watina, startyear = year(now()) - 18, endyear = 2016, vert_crs = "local",
#             truncated =  TRUE, collect = TRUE)

tubes_xg3 <- read_vc(file.path(".","data","tubes_xg3"))
tubes_xg3 <- tubes_xg3 %>% 
  inner_join (tubes_in_raster %>% 
                select(loc_code), by = "loc_code")

#oplossen van UTF-probleem: strings worden in inbo-SQL-databanken opgeslagen in UTF-16, terwijl hier gewerkt wordt met UTF-8. Dit geeft een probleem bij de kable-functie

#tubes_xg3 <- mutate_if(tubes_xg3, is.character, iconv, to = "UTF-8")

# tubes_xg3 <- tubes_xg3 %>% 
#   arrange(loc_code, hydroyear)
# 
# output_vc <- write_vc(tubes_xg3, file.path(".","data","tubes_xg3"), sorting = c("loc_code", "hydroyear"),
#          strict =  FALSE, root = ".")
# rm(output_vc)
# 
# DBI::dbDisconnect(watina)


#overzicht per pb hoeveel lg3 er zijn, eerste en laatste jaar 
tubes_xg3_avail <- tubes_xg3 %>% 
  eval_xg3_avail( xg3_type = "L")

#beperken tot pb met een lg3
tubes_lg3_avail <- tubes_xg3_avail %>% 
  filter(nryears > 0)

#pb die voldoen aan minimale voorwaarden voor een lgl (nodige voorwaarden, maar niet noodzakelijk voldoende)
# debugonce(eval_xg3_series)

#volgende code loopt in markdown steeds mank. Daarom buiten markdown uitgevoerd en de data worden dan hier ingelezen (noodoplossing)

# # tubes_lg3_eval <-   tubes_xg3 %>%
# #   eval_xg3_series(xg3_type = c("L"),
# #                   max_gap = maxgap,
# #                   min_dur = minlength)
# 
# 
# #pb die voldoen aan alle voorwaarden voor een lgl
# tubes_lgl_eval <- tubes_lg3_eval %>%
#    filter(ser_nryears >= minnryears)

#write_vc(tubes_lgl_eval, file.path(".","data","local","tubes_lgl_eval"), sorting = c("loc_code"),
#         strict =  FALSE, root = ".")

#DBI::dbDisconnect(watina)

# getwd()
# file.path(".","data","local")
tubes_lgl_eval <- read_vc("tubes_lgl_eval", file.path(getwd(),"data","local"))
tubes_lgl_eval <- tubes_lgl_eval %>% 
  inner_join(tubes_in_raster %>% 
               select(loc_code), by = "loc_code")

  
  ## De rastercellen categoriseren o.b.v. de beschikbaarheid van peilbuizen 

### Koppeling van peilbuizen, met de kwaliteit van hun tijdreeksen, aan de geselecteerde rastercellen


#rastercellen met een pb
sel_raster_pb <- 
  sel_raster_meetnet %>% 
  inner_join(tubes_in_raster, 
             by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters
  left_join(tubes_lgl_eval, 
            by = "loc_code") %>% # aanduiding van pb met een lgl
  group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
  summarise(n_tubes = n(),
            n_tubes_lgl = sum(!is.na(series) & str_ends(series, "1"))) %>% 
  ungroup %>% 
  select(-geom, geom)




### Categorie 1A: Zijn er rastercellen met onvoldoende peilbuizen?

#eerste groep rastercellen: rastercellen zonder peilbuis
# sel_cat1A_raster  <- sel_raster_meetnet %>% 
#   anti_join(tubes_in_raster, group_by = c("rasterid", "groupnr")) %>% 
#   # distinct(rasterid, groupnr) %>% 
#   arrange(rasterid, groupnr)

#eerste groep rastercellen: rastercellen zonder peilbuis of met een onvoldoend aantal peilbuizen
sel_cat1A_raster <- sel_raster_meetnet %>% 
  left_join(sel_raster_pb %>% 
              st_drop_geometry() %>% 
              select(-gew_aantal_meetptn),
            by = c("rasterid", "groupnr")) %>% 
  filter(is.na(n_tubes) | gew_aantal_meetptn > n_tubes) %>% 
  arrange(rasterid, groupnr)

sel_cat1A_table <- sel_cat1A_raster %>% 
  st_drop_geometry() %>% 
  rename(gewenst_aantal_meetpunten = gew_aantal_meetptn) %>% 
  mutate(totaal_peilbuizen =  replace_na(n_tubes,0),
         aantal_cat1A = gewenst_aantal_meetpunten - totaal_peilbuizen) %>% 
  select(-n_tubes) %>% 
  arrange(rasterid)


sel_cat1A_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_cat1A_raster) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.1A: cel met een onvoldoend aantal peilbuizen" )

sel_cat1A_tm

### Categorie 1B: Rastercellen met peilbuizen, maar onvoldoende **geschikte** {#cat1b}


sel_cat1B_raster <- sel_raster_pb %>%
  left_join(sel_cat1A_table %>%
              select(-gewenst_aantal_meetpunten, -totaal_peilbuizen,
                     -n_tubes_lgl, -opp_gw_cel),
            by = c("rasterid", "groupnr")) %>%
  mutate(aantal_cat1A = replace_na(aantal_cat1A,0),
         rest = gew_aantal_meetptn - aantal_cat1A) %>%
  filter(rest - n_tubes_lgl > 0 ) %>%
  mutate(aantal_cat1B = rest - n_tubes_lgl) %>%
  select(-geom, -rest, geom)


# deze rastercellen vormen de tweede groep (groep met pb, maar alle zonder een lgl of er zijn er te weinig)
# voor deze groep kan onderzocht worden of door het modelmatig verbeteren van de tijdreeksen er geen lgl kan berekend worden

sel_cat1B_table <- sel_cat1B_raster %>% 
  st_drop_geometry() %>% 
  arrange(rasterid)



sel_cat1B_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_cat1B_raster) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.1B: cel met een onvoldoend aantal peilbuizen met goede meetreeks" )

sel_cat1B_tm


#### Opzoeken van peilbuizen binnen de rastercellen van cat 1B

 tubes_cat1B <- 
   sel_cat1B_raster %>% 
   inner_join(tubes_in_raster, 
              by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters
   # inner_join(tubes_lg3_avail, 
   #            by = "loc_code") %>% # aanduiding van pb lg3, geen lgl
   st_drop_geometry() %>% 
   select(rasterid, groupnr, gew_aantal_meetptn, loc_code, everything(), 
          -starts_with("loc_v"), -starts_with("loc_t")) %>% 
   arrange(rasterid, groupnr)
 

 #write_csv(tubes_cat1B, file.path(".", "data","local", "tubes_cat1B.csv"))
 output_vc <- write_vc(tubes_cat1B, file.path(".","data","tubes_cat1B"), 
                       sorting = c("loc_code"), strict =  FALSE, root = ".")
 
 rm(output_vc)
 
 
 #oplijsten per rastercel welke peilbuizen een kwalitatief onvoldoende tijdreeks hebben
 
 
 # tubes_in_raster %>% filter(rasterid == 46)

 
 ### Het opleggen van bijkomende kwaliteitscriteria
 

   #### Het toepassen van drie extra kwaliteitscriteria


sel_qual_basis <- 
 sel_raster_pb %>% 
 select(rasterid, groupnr, gew_aantal_meetptn) %>% 
 st_drop_geometry() %>% 
 left_join(sel_cat1A_table %>% 
             select(rasterid, groupnr, aantal_cat1A), 
           by = c("rasterid", "groupnr")) %>%
 left_join(sel_cat1B_table %>% 
             select(rasterid, groupnr, aantal_cat1B), 
           by = c("rasterid", "groupnr")) %>%
 inner_join(tubes_in_raster, 
            by =  c("rasterid", "groupnr")) %>% 
 distinct(rasterid, groupnr, gew_aantal_meetptn, loc_code, aantal_cat1A, aantal_cat1B) %>% 
 inner_join(tubes_lgl_eval, 
            by = "loc_code") %>% 
 mutate(aantal_cat1A = replace_na(aantal_cat1A,0),
        aantal_cat1B = replace_na(aantal_cat1B,0)) %>% 
 group_by(rasterid, groupnr, gew_aantal_meetptn, aantal_cat1A, aantal_cat1B) %>% 
 count(ser_lastyear, ser_nryears)  %>% 
 ungroup() %>% 
 mutate(rest_aantal_meetptn = gew_aantal_meetptn - aantal_cat1A - aantal_cat1B)

sel_qual_basis <- 
 sel_qual_basis %>% 
 group_by(rasterid, groupnr) %>% 
 arrange(rasterid, groupnr, rest_aantal_meetptn, desc(ser_lastyear)) %>% 
 mutate(rankclus_lastyear = 
          floor((cummax(ser_lastyear) - ser_lastyear)/toelaatbare_spreiding_jaren) + 1) %>% 
 arrange(rasterid, groupnr, rest_aantal_meetptn, desc(ser_nryears)) %>% 
 mutate(rankclus_nryears = 
          floor((cummax(ser_nryears) - ser_nryears)/toelaatbaar_verschil_lengte_tijdreeks) + 1) %>% 
 ungroup()


 sel_qual <- 
   sel_qual_basis %>% 
   group_by(rasterid, groupnr, rest_aantal_meetptn, rankclus_lastyear, rankclus_nryears) %>% 
   mutate(rankclus_temp = as.integer(paste0(rankclus_lastyear,rankclus_nryears))) %>% 
   arrange(rasterid, groupnr,rankclus_temp) %>% 
   ungroup() %>% 
   group_by(rasterid, groupnr) %>% 
   mutate(rankclus = dense_rank(rankclus_temp)) %>% 
   group_by(rasterid, groupnr, rest_aantal_meetptn, rankclus, rankclus_lastyear, rankclus_nryears) %>% 
   summarise(beschikbaar_aantal_cluster = sum(n)) %>% 
   ungroup 
 
 # functie om de rang te bepalen die nodig is om tot het gewenst aantal meetpunten te komen
 
 max_rank <-  function(x) {
   # x <- sel_qual_test %>% filter (rasterid == 134, groupnr == 4)
   clusters <- unique(x$rankclus) 
   gewenst_aantal <- x[1,"rest_aantal_meetptn"] %>%
     as.integer()
   beschikbaar_aantal <- 0
   einde <- 0
   
   
   for (i in clusters) {
     # i <- 1
     rank <- as.integer(i)
     beschikbaar_aantal_rank <-  x[1,"beschikbaar_aantal_cluster"] %>%
       as.integer()
     if (gewenst_aantal <= (beschikbaar_aantal_rank + beschikbaar_aantal) & einde == 0) {
       maxrank <- rank
       einde <- 1
     } else {
       beschikbaar_aantal <- beschikbaar_aantal_rank + beschikbaar_aantal
     }  
   }
   return(maxrank)
 }
 
 sel_qual_maxrank <- plyr::ddply(sel_qual, ~rasterid+groupnr, max_rank) %>%
   rename(maxrank = V1)
 
 sel_qual <- sel_qual %>%
   inner_join(sel_qual_maxrank, 
              by = c("rasterid", "groupnr"))
 

 
 ### Categorie 2: Rastercellen met juist voldoende of te weinig geschikte peilbuizen {#cat2}
 

 # rastercellen met een juist voldoende of een tekort aantal evenwaardige meetpunten dat gewenst is voor het meetnet
 sel_cat2_table <- 
   sel_qual %>% 
   filter(rankclus <= maxrank ) %>% 
   group_by(rasterid, groupnr, rest_aantal_meetptn) %>% 
   summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
   ungroup %>% 
   inner_join(sel_qual,
              by = c("rasterid", "groupnr", "rest_aantal_meetptn")) %>% 
   filter(beschikbaar_aantal == rest_aantal_meetptn, rankclus <= maxrank) %>% 
   select(-beschikbaar_aantal, -maxrank)
 

 sel_cat2_raster <- sel_raster_pb %>% 
   semi_join(sel_cat2_table,
             by = c("rasterid", "groupnr")) %>% 
   arrange(rasterid, groupnr)

 
 sel_cat2_tm <- raster_meetnet_poly_tm + 
   tm_shape(sel_cat2_raster) + 
   tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.2: rastercellen met een juist voldoende aantal peilbuizen" )
 
 sel_cat2_tm
 
 
 #### Opzoeken van peilbuizen voor de rastercellen van cat. 2 {#opzoeken-pb-cat2}

 
 # de bijhorende geselecteerde Watina-meetpunten zijn dan
 tubes_cat2 <- 
   tubes_in_raster %>% 
   inner_join(tubes_lgl_eval %>% 
                select(loc_code, ser_firstyear, ser_lastyear, 
                       ser_nryears, ser_length), 
              by = "loc_code") %>% 
   inner_join(sel_cat2_table  %>% 
                inner_join(sel_qual_basis %>% 
                             select(-rest_aantal_meetptn), 
                           by =  c("rasterid","groupnr","rankclus_lastyear", "rankclus_nryears" )), 
              by = c("rasterid", "groupnr", "ser_lastyear", "ser_nryears"))
 
 

 output_vc <- write_vc(tubes_cat2, file.path(".","data","tubes_cat2"), 
                       sorting = c("loc_code"), strict =  FALSE, root = ".")
 
 rm(output_vc)

 
 ### Categorie 3: Rastercellen met een overschot aan geschikte peilbuizen {#cat3}

 # rastercellen met een overschot aan evenwaardige meetpunten in vergelijking met het aantal dat gewenst is voor het meetnet
 sel_cat3_raster <- 
   sel_raster_meetnet %>% 
   anti_join(sel_cat1A_raster %>% 
               st_drop_geometry(), 
             by = c("rasterid", "groupnr"))  %>% 
   anti_join(sel_cat1B_raster %>% 
               st_drop_geometry(), 
             by = c("rasterid", "groupnr"))  %>% 
   anti_join(sel_cat2_raster %>% 
               st_drop_geometry(), 
             by = c("rasterid", "groupnr")) %>% 
   arrange(rasterid, groupnr)
 

 sel_cat3_table <- 
   sel_qual %>% 
   filter(rankclus <= maxrank ) %>% 
   group_by(rasterid, groupnr, rest_aantal_meetptn) %>% 
   summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
   ungroup %>% 
   inner_join(sel_qual, by = c("rasterid", "groupnr", "rest_aantal_meetptn") ) %>% 
   filter(beschikbaar_aantal > rest_aantal_meetptn, rankclus <= maxrank) %>% 
   select(-beschikbaar_aantal, -maxrank)  
 
 

 
 sel_cat3_tm <- raster_meetnet_poly_tm + 
   tm_shape(sel_cat3_raster) + 
   tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.3: rastercellen met een overschot aan geschikte peilbuizen" )
 
 sel_cat3_tm

 
 #### Opzoeken van peilbuizen voor de rastercellen van cat. 3

 #bijhorende Watina-meetpunten ervan opzoeken
 tubes_cat3 <- 
   tubes_in_raster %>% 
   distinct(loc_code, x, y, rasterid, groupnr) %>% 
   inner_join(tubes_lgl_eval %>% 
                select(loc_code, ser_length, ser_firstyear, 
                       ser_lastyear, ser_nryears), 
              by = "loc_code") %>% 
   inner_join(sel_cat3_table  %>% 
                inner_join(sel_qual_basis %>% 
                             select(-rest_aantal_meetptn), 
                           by = c("rasterid","groupnr","rankclus_lastyear","rankclus_nryears" )), 
              by = c("rasterid","groupnr", "ser_lastyear","ser_nryears"))
 
 tubes_cat3 <- tubes_cat3 %>% 
   rownames_to_column("unieknr") %>% 
   mutate(unieknr = as.integer(unieknr))
 # sel_qual_lastyear_vb %>% 
 #   count(rasterid, groupnr )
 
 

 
 output_vc <- write_vc(tubes_cat3, file.path(".","data","tubes_cat3"), 
                       sorting = c("loc_code"), strict =  FALSE, root = ".")
 
 rm(output_vc)


 
 ##Tijdreeksanalyse {#tijdreeksanalyse_uitvoering}

 tubes_menyanthes <- read_csv(file.path(getwd(),"data", "tblTubes_Menyanthes_report.csv"))
 
 #veldnamen aanpassen
 tubes_menyanthes <- janitor::clean_names(tubes_menyanthes, case = "snake")
 
 #syntheseveld maken
 tubes_menyanthes <- tubes_menyanthes %>% 
   mutate(
     uitspraak = case_when(
       modelbaar == 1 ~ "weerhouden, expertoordeel",
       !is.na(evaporatiefactor) ~ "niet weerhouden, expertoordeel",
       evp < 66 ~ "niet weerhouden, te lage modelfit",
       is.na(trend_verschil) | abs(trend_verschil) - 2*trend_sd <= trend_jaren ~ "weerhouden",
       abs(trend_verschil) - 2*trend_sd > trend_jaren ~ "niet weerhouden, trend"
     ),
     selectie = if_else(str_detect(uitspraak, pattern = "niet weerhouden"),0,1)
   )
 
 #wegfilteren van meetreeksen die niet in een raster vallen (is mogelijk wanneer een peilbuis van een oude selectie in de resultaattabel van Menyanthes is verzeild)
 tubes_menyanthes <- tubes_menyanthes %>% 
   semi_join(tubes_in_raster, by = c("watinacode" = "loc_code"))
 
 tubes_menyanthes_synthese <- tubes_menyanthes %>% 
   count(cat, uitspraak) %>% 
   rename(aantal = n)
 

   ## De rastercellen hercategoriseren na de tijdreeksanalysen 
   

 #peilbuizen miv evaluatie Menyanthes
 tubes_eval_namenyanthes <- tubes_in_raster %>% 
   left_join(tubes_menyanthes %>% 
               select(watinacode, selectie), by = c("loc_code" = "watinacode")) %>% 
   left_join(tubes_lgl_eval, 
             by = "loc_code") %>% 
   mutate(selectie = case_when(
     selectie == 0 ~ -1, #afgekeurde meetreeks
     selectie == 1 ~ 1, #goedgekeurde meetreeks
     TRUE ~ 0 #niet geanalyseerde meetreeks
   )
   ) 
 
 #rastercellen met een pb
 sel_raster_pb_bis <- 
   sel_raster_meetnet %>% 
   inner_join(tubes_eval_namenyanthes, 
              by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters 
   group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
   summarise(n_tubes = n(),
             n_tubes_lgl = 
               sum(!is.na(series) & str_ends(series, "1") & selectie >= 0) + 
               sum(selectie > 0)) %>% 
   ungroup %>% 
   select(-geom, geom)
 

 
 
 ### Categorie 1A: Zijn er rastercellen met onvoldoende peilbuizen?
 

 ### Categorie 1B: Rastercellen met een onvoldoend aantal **geschikte** peilbuizen
 

 
 sel_cat1B_raster_bis <- sel_raster_pb_bis %>%
   left_join(sel_cat1A_table %>%
               select(-gewenst_aantal_meetpunten, -totaal_peilbuizen,
                      -n_tubes_lgl, -opp_gw_cel),
             by = c("rasterid", "groupnr")) %>%
   mutate(aantal_cat1A = replace_na(aantal_cat1A,0),
          rest = gew_aantal_meetptn - aantal_cat1A) %>%
   filter(rest - n_tubes_lgl > 0 ) %>%
   mutate(aantal_cat1B = rest - n_tubes_lgl) %>%
   select(-geom, -rest, geom)
 

 sel_cat1B_table_bis <- sel_cat1B_raster_bis %>% 
   st_drop_geometry() %>% 
   arrange(rasterid)
 

 
 sel_cat1B_tm_bis <- raster_meetnet_poly_tm + 
   tm_shape(sel_cat1B_raster_bis) + 
   tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.1B: cel met een onvoldoend aantal peilbuizen met goede meetreeks" )
 
 sel_cat1B_tm_bis

 tubes_cat1B_bis <- 
   sel_cat1B_raster_bis %>% 
   inner_join(tubes_eval_namenyanthes %>% 
                filter(selectie <= 0), by = c("rasterid", "groupnr")) %>% 
   st_drop_geometry() %>% 
   select(rasterid, groupnr, gew_aantal_meetptn, loc_code, everything(),
          -starts_with("loc_v"), -starts_with("loc_t"), -starts_with("ser"), -starts_with("xg3")) %>% 
   arrange(rasterid, groupnr)
 
 
 tubes_cat1Ba <- tubes_cat1B_bis %>% 
   inner_join(tubes_cat1B_bis %>% 
                group_by(rasterid, groupnr, gew_aantal_meetptn, aantal_cat1A) %>% 
                summarise(n_tubes_zondermodel = n())
   ) %>% 
   mutate(n_tubes_zondermodel = n_tubes_zondermodel + aantal_cat1A) %>% 
   filter(gew_aantal_meetptn == n_tubes_zondermodel)
 

 output_vc <- write_vc(tubes_cat1Ba, file.path(".","data","tubes_cat1Ba"), 
                       sorting = c("loc_code"), strict =  FALSE, root = ".")
 
 rm(output_vc)
 

 
 tubes_cat1Bb <- tubes_cat1B_bis %>% 
   anti_join(tubes_cat1Ba, by = "loc_code") %>% 
   inner_join(tubes_cat1B_bis %>% 
                group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
                summarise(n_tubes_zondermodel = n())
   ) 
 

 
 output_vc <- write_vc(tubes_cat1Bb, file.path(".","data","tubes_cat1Bb"), 
                       sorting = c("loc_code"), strict =  FALSE, root = ".")
 
 rm(output_vc)
 test <- tubes_cat1Bb %>% 
   select(-n_tubes_lgl, -aantal_cat1A, -aantal_cat1B,
          -typegroup_name, -selectie) %>% 
   select(1:3,12,7,4,8:9) %>% 
   rename(GTgroep = groupnr,
          rasternr = rasterid,
          watinacode = loc_code,
          'gewenst aantal meetptn' = gew_aantal_meetptn,
          #'# peilbuizen' =  n_tubes,
          gebied = area_name,
          #gebiedcode = area_code,
          '# beschikbare ptn' = n_tubes_zondermodel
   ) %>% 
   arrange(watinacode)

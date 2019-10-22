options(stringsAsFactors = FALSE)
# library(plyr)

library(stringr)
library(knitr)
library(sf)
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

gw_types <- read_scheme_types(lan = "nl") %>%
  filter(scheme == "GW_05.1_terr") %>%
  arrange(typegroup) %>%
  mutate(groupnr = as.integer(str_sub(typegroup, -1))) %>% 
  dplyr::select(type, groupnr, typegroup_name)

gw_types_groupen <- gw_types %>%
  distinct(groupnr, typegroup_name) %>% 
  rename("GT-groep: nummer" = groupnr,
         "GT-groep: naam" = typegroup_name)

habfile <- "20_processed/habitatmap_terr/habitatmap_terr.gpkg"

habmap_stdized <- read_habitatmap_stdized(file = habfile)

habmap_polygons <- habmap_stdized$habitatmap_polygons

habmap_patches <- habmap_stdized$habitatmap_patches

types <- read_types(lang = "nl")

habmap_patches <- habmap_patches %>%
  mutate( polygon_id = as.factor(.data$polygon_id),
          patch_id = as.numeric(.data$patch_id),
          certain = .data$certain == 1,
          type = factor(.data$type,
                        levels = levels(types$type)
          )
  )

tubes_hab_aggr %>% 
  select(loc_code, area_name, x,y, filterdepth, description, type) %>% 
  rename(watinacode = loc_code,
         gebied = area_name,
         "diepte filter" = filterdepth,
         beschrijving = description,
         habitattype = type)

habmap_patches_gw <- habmap_patches %>% 
  inner_join(gw_types, by = c("type" = "type"))
habmap_polygons_gw <- 
  habmap_polygons %>% 
  inner_join(habmap_patches_gw %>% 
               dplyr::select(-code_orig), 
             by = "polygon_id")

tot_n_tub <- 100
aantal_strat <- 5
minaantal_tub_group <- as.integer(tot_n_tub/aantal_strat)

raster_meetnet_poly <- read_GRTSmh_diffres(level = 8, polygon = TRUE)

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

habmap_gw_raster_overlay <- habmap_polygons_gw %>% 
  st_intersection(raster_meetnet_poly)

habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>% 
  mutate(opp = as.integer(st_area(habmap_gw_raster_overlay))) 

habmap_gw_raster_overlay <- lwgeom::st_make_valid(habmap_gw_raster_overlay)
#plot(habmap_gw_raster_overlay, main = "Voorkomen van ")

habmap_gw_raster_overlay_tm <- raster_meetnet_poly_tm + 
  tm_shape(habmap_gw_raster_overlay) + 
  tm_fill(col = "groupnr", style = "cat", palette = "BuGn", title = "Grondwatertype") + 
  tm_layout(title = "Voorkomen GT-groepen" )

# interactieve modus, maar vraagt veel computertijd en genereert ook een mega html-bestand
tmap_mode("view")
# statische modus, dus niet inzoombaar
tmap_mode("plot")

#habmap_gw_raster_overlay_tm

raster_gw_opp <- habmap_gw_raster_overlay %>% 
  st_drop_geometry() %>% 
  group_by(rasterid,groupnr) %>% 
  summarise(opp_gw_cel = sum(opp*phab/100)) %>% 
  ungroup()
# view(raster_gw_opp)

#totale opp van een gw-groep
gw_opp <- raster_gw_opp %>% 
  group_by(groupnr) %>% 
  summarise(opp_gw = sum(opp_gw_cel)) %>% 
  ungroup()

min_aantal_tub = data.frame("groupnr" = 1:aantal_strat, minaantal = seq(minaantal_tub_group,minaantal_tub_group,length.out = aantal_strat))

for (group in seq(1,aantal_strat)) {
  #group <- 1
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
                 ifelse(bewerkt == TRUE, round(opp_gw/minaantal_tub_group * corrafronding,0), minopp))
    } else {
      gw_opp <- gw_opp %>% 
        mutate(minopp = 
                 if_else(bewerkt == TRUE, round(opp_gw/minaantal_tub_group * corrafronding,0), minopp, missing = minopp))
    }    
    
    gw_opp
    
    #berekening van het aantal meetpunten per cel
    
    aantal_meetpunten_cel_group <- 
      raster_gw_opp %>% 
      inner_join(gw_opp, 
                 by = "groupnr") %>% 
      mutate(gew_aantal_meetptn = opp_gw_cel/minopp) %>% 
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
               ifelse(bewerkt == TRUE, round(minopp*minaantal_tub_group/opp_gw, 2), correctiefactor))
  } else {
    gw_opp <- gw_opp %>% 
      mutate(correctiefactor = 
               if_else(bewerkt == TRUE, round(minopp*minaantal_tub_group/opp_gw, 2), correctiefactor, missing = correctiefactor))
  }
  
  gw_opp <- gw_opp %>% 
    select(-bewerkt)   
  
  if (group == 1){
    aantal_meetpunten_cel <- aantal_meetpunten_cel_group
  } else {
    aantal_meetpunten_cel <- bind_rows(aantal_meetpunten_cel, aantal_meetpunten_cel_group)
  }
}
# df <- aantal_meetpunten_cel %>% group_by(groupnr)  %>% 
#    summarise(aantal  = sum(gew_aantal_meetptn_afgerond)) %>% 
#    ungroup()
# df
aantal_meetpunten_cel <- aantal_meetpunten_cel %>% 
  select (-bewerkt, -correctiefactor)

aantal_meetpunten_cel_overzicht <- aantal_meetpunten_cel %>% 
  group_by(groupnr) %>% 
  summarise ("totaal aantal meetptn" = sum(gew_aantal_meetptn_afgerond))

sel_raster_meetnet <- 
  raster_meetnet_poly %>%
  inner_join(raster_gw_opp, by = "rasterid") %>%
  inner_join(aantal_meetpunten_cel %>% 
               select(rasterid, groupnr, gew_aantal_meetptn_afgerond), 
             by =  c("rasterid", "groupnr")) %>% 
  rename(gew_aantal_meetptn = gew_aantal_meetptn_afgerond)

sel_raster_meetnet <- lwgeom::st_make_valid(sel_raster_meetnet)
#st_is_valid(raster_meetnet_poly)

sel_raster_meetnet_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_raster_meetnet %>% filter(groupnr == 1)) + 
  tm_polygons(col = "red") + tm_layout(title = "GT-groep 1" )

sel_raster_meetnet_tm

sel_raster_meetnet_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_raster_meetnet) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE) + tm_layout(title = "alle groepen" )

sel_raster_meetnet_tm


#watina <- connect_watina()
watina <- DBI::dbConnect(odbc::odbc(),
                         driver = "SQL Server",
                         server = "inbo-sql08-prd.inbo.be",
                         port = 1433,
                         database = "W0002_00_Watina",
                         trusted_connection = "YES",
                         encoding = vc <- "UTF-8")
# ?watina
# ?selectlocs_xg3
# ?get_locs
# debugonce(get_locs)
tubes_hab <- get_locs(watina, mask = habmap_gw_raster_overlay, join_mask = TRUE,
                      buffer = 10, loc_type = "P", loc_validity = c("VLD", "ENT"), collect = TRUE)
tubes_hab_3 <- get_locs(watina, mask = habmap_gw_raster_overlay, join_mask = TRUE,
                        buffer = 3, loc_type = "P", loc_validity = c("VLD", "ENT"), collect = TRUE)
#tubes_hab <- mutate_if(tubes_hab, is.character, iconv, to = "UTF-8")

#beperken van peilbuizen tot de rastercel waar ze effectief in liggen. Door het gebruik van een buffer is het immers mogelijk dat een peilbuis in twee of meer cellen komt te liggen.
tubes_hab_sf <- as_points(tubes_hab)
tubes_hab_sf3 <- as_points(tubes_hab_3)

tubes_hab_gw_raster_overlay <- tubes_hab_sf %>% 
  distinct(loc_code, x, y) %>% 
  st_intersection(raster_meetnet_poly)
tubes_hab_gw_raster_overlay3 <- tubes_hab_sf3 %>% 
  distinct(loc_code, x, y) %>% 
  st_intersection(raster_meetnet_poly)

tubes_hab <- tubes_hab %>% 
  semi_join(tubes_hab_gw_raster_overlay, by = c("loc_code","rasterid"))
tubes_hab_3 <- tubes_hab_3 %>% 
  semi_join(tubes_hab_gw_raster_overlay3, by = c("loc_code","rasterid"))

#save tubes_hab as a git2rdata-object
tubes_hab <- tubes_hab %>% 
  arrange(loc_code, polygon_id, rasterid, patch_id, type)
tubes_hab_3 <- tubes_hab_3 %>% 
  arrange(loc_code, polygon_id, rasterid, patch_id, type)

write_vc(tubes_hab, file.path(".","data","tubes_hab"), sorting = c("loc_code", "polygon_id", "rasterid", "patch_id", "type" ), strict =  FALSE)
write_vc(tubes_hab_3, file.path(".","data","tubes_hab_3"), sorting = c("loc_code", "polygon_id", "rasterid", "patch_id", "type" ), strict =  FALSE)

str(tubes_hab)

# een peilbuis kan meerdere keren voorkomen, namelijk wanneer de pb in een habitat-complex ligt. We kunnen ze groeperen als de verschillende eenheden tot eenzelfde gw-groep behoren.
# Alleen de gw-groep met een opp-aandeel van minstens 50% wordt weerhouden. Dit om te vermijden dat indien een rastercel gekozen werd voor een bep. gw-groep, een pb geselecteerd wordt waarvan de kans klein is dat ze die gw-groep representeert.

tubes_hab_groep <- tubes_hab %>%
  group_by(loc_code, polygon_id, rasterid, groupnr) %>%
  summarise(phab_gw = sum(phab),
            aantal =  n()) %>%
  ungroup() %>% 
  filter(phab_gw >= 50)

tubes_hab_groep_3 <- tubes_hab_3 %>%
  group_by(loc_code, polygon_id, rasterid, groupnr) %>%
  summarise(phab_gw = sum(phab),
            aantal =  n()) %>%
  ungroup() %>% 
  filter(phab_gw >= 50)

tubes_hab_multipolyg <- tubes_hab_groep %>% 
  semi_join(tubes_hab_groep %>% 
            distinct(loc_code, groupnr) %>% 
            count(loc_code) %>% 
            filter(n == 1),
          by = "loc_code")
tubes_hab_multipolyg_3 <- tubes_hab_groep_3 %>% 
  semi_join(tubes_hab_groep_3 %>% 
              distinct(loc_code, groupnr) %>% 
              count(loc_code) %>% 
              filter(n == 1),
            by = "loc_code")

test <- tubes_hab_groep %>% 
  anti_join(tubes_hab_multipolyg)

test  <- tubes_hab_groep %>% 
  distinct(loc_code, groupnr) %>% 
  count(loc_code) %>% 
  filter(n == 1)

tubes_hab_aggr <- tubes_hab %>%
  select(-patch_id,-phab, -certain, -type, -source.y) %>% 
  distinct %>% 
  semi_join(tubes_hab_multipolyg_3, by = c("loc_code", "polygon_id", "rasterid", "groupnr" ))

DBI::dbDisconnect(watina)



tubes_in_raster <- tubes_hab_aggr %>% 
  select(-10:-15, -opp) %>%  
  inner_join(sel_raster_meetnet %>% 
               select(rasterid, groupnr) %>% 
               st_drop_geometry(), by = c("rasterid", "groupnr")) %>% 
  distinct()

tubes_in_raster

sel_no_tube <- sel_raster_meetnet %>% 
  anti_join(tubes_in_raster, group_by = c("rasterid", "groupnr")) %>% 
  # distinct(rasterid, groupnr) %>% 
  arrange(rasterid, groupnr)
#eerste groep rastercellen: rastercellen zonder peilbuis
sel_cat1A_raster <- sel_no_tube

sel_cat1_table <- sel_cat1A_raster %>% 
  st_drop_geometry() %>% 
  group_by(rasterid, groupnr) %>% 
  summarise(gewenst_aantal_meetpunten = sum(gew_aantal_meetptn)) %>% 
  arrange(rasterid)

sel_cat1_table %>%
  group_by(groupnr ) %>% 
  summarise('aantal cellen zonder peilbuis' = n(),
            'aantal locaties zonder pb' =  sum(gewenst_aantal_meetpunten ))

sel_cat1_table %>%
  group_by(groupnr ) %>% 
  summarise('aantal cellen zonder peilbuis' = n())

sel_cat1_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_cat1A_raster) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE) + tm_layout(title = "cat.1: cel zonder peilbuis" )

sel_cat1_tm

sum(sel_cat1_table $gewenst_aantal_meetpunten)

watina <- DBI::dbConnect(odbc::odbc(),
                         driver = "SQL Server",
                         server = "inbo-sql08-prd.inbo.be",
                         port = 1433,
                         database = "W0002_00_Watina",
                         trusted_connection = "YES",
                         encoding = "UTF-8")
# minlength <- 5 #jaar
# maxgap <- 2 #jaar
# minnryears <- 5 #jaar

# alle bestaande peilbuizen : tubes_in_raster
# pb met een xg3 (hoeft geen lg3 te zijn) binnen tijdsruimte: tubes_xg3_avail
# pb met min. 1 lg3 binnen tijdsruimte: tubes_lg3_avail
# pb waarvoor een lgl kan berekend worden: tubes_lgl_eval

#voor elke pb de xg3 waarden ophalen (meerdere rec per pb)
tubes_xg3 <- tubes_in_raster %>% 
  get_xg3(watina, startyear = year(now()) - 18, endyear = 2016, vert_crs = "local",
          truncated =  TRUE, collect = TRUE)

#oplossen van UTF-probleem: strings worden in inbo-SQL-databanken opgeslagen in UTF-16, terwijl hier gewerkt wordt met UTF-8. Dit geeft een probleem bij de kable-functie

tubes_xg3 <- mutate_if(tubes_xg3, is.character, iconv, to = "UTF-8")

tubes_xg3 <- tubes_xg3 %>% 
  arrange(loc_code, hydroyear)

output_vc <- write_vc(tubes_xg3, file.path(".","data","tubes_xg3"), sorting = c("loc_code", "hydroyear"),
                      strict =  FALSE, root = ".")
rm(output_vc)

tubes_xg3_avail <- tubes_xg3 %>% 
  eval_xg3_avail( xg3_type = "L")

#beperken tot pb met een lg3
tubes_lg3_avail <- tubes_xg3_avail %>% 
  filter(nryears > 0)

#pb die voldoen aan minimale voorwaarden voor een lgl (nodige voorwaarden, maar niet noodzakelijk voldoende)
# debugonce(eval_xg3_series)

minlength <- 5 #jaar
maxgap <- 2 #jaar
minnryears <- 5 #jaar

tubes_lg3_eval <-   tubes_xg3 %>%
  eval_xg3_series(xg3_type = c("L"),
                  max_gap = maxgap,
                  min_dur = minlength)

#pb die voldoen aan alle voorwaarden voor een lgl
tubes_lgl_eval <- tubes_lg3_eval %>% 
  filter(ser_nryears >= minnryears)

output_vc <- write_vc(tubes_lgl_eval, file.path(".","data","local","tubes_lgl_eval"), sorting = c("loc_code"),
         strict =  FALSE, root = ".")
rm(output_vc)

test <- read_vc ("tubes_lgl_eval", file.path(".","data","local"))
# sessioninfo::session_info()

DBI::dbDisconnect(watina)

sel_no_tube <- sel_raster_meetnet %>% 
  anti_join(tubes_in_raster, group_by = c("rasterid", "groupnr")) %>% 
  # distinct(rasterid, groupnr) %>% 
  arrange(rasterid, groupnr)
#eerste groep rastercellen: rastercellen zonder peilbuis
sel_cat1A_raster <- sel_no_tube


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

sel_raster_pb %>% st_drop_geometry()

test <- sel_raster_pb %>% 
  filter(gew_aantal_meetptn == n_tubes_lgl)

test <- sel_raster_pb %>% 
  filter (gew_aantal_meetptn > n_tubes_lgl)

sel_raster_toolittle_lgl <- sel_raster_pb %>% 
  filter(n_tubes_lgl < gew_aantal_meetptn)

sel_cat1B <- sel_raster_pb %>% 
  filter(n_tubes_lgl < gew_aantal_meetptn)
  

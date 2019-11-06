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
library(units)
library(RSQLite) 
library (googledrive)

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
# 
# habmap_terr <- read_habitatmap_terr()
# 
# habmap_polygons <- habmap_terr$habitatmap_terr_polygons
# 
# habmap_patches <- habmap_terr$habitatmap_terr_patches

types <- read_types(lang = "nl")

habmap_patches <- habmap_patches %>%
  mutate( polygon_id = as.factor(.data$polygon_id),
          patch_id = as.numeric(.data$patch_id),
          certain = .data$certain == 1,
          type = factor(.data$type,
                        levels = levels(types$type)
          )
  )

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
str(raster_gw_opp)

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
                 ifelse(bewerkt == TRUE, round(opp_gw/minaantal_tub_group * corrafronding,0), minopp) %>% set_units("ha")
               ) 
    } else {
      gw_opp <- gw_opp %>% 
        mutate(minopp = 
                 if_else(bewerkt == TRUE, round(opp_gw/minaantal_tub_group * corrafronding,0), minopp, 
                         missing = minopp) %>% set_units("ha")
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
               ifelse(bewerkt == TRUE, round(minopp*minaantal_tub_group/opp_gw, 2), correctiefactor))
  } else {
    gw_opp <- gw_opp %>% 
      mutate(correctiefactor = 
               if_else(bewerkt == TRUE, round(minopp*minaantal_tub_group/opp_gw, 2), correctiefactor, missing = correctiefactor))
  }
  
  gw_opp <- gw_opp %>% 
    select(-bewerkt)   

  if (group == 1) {
    aantal_meetpunten_cel <- aantal_meetpunten_cel_group 
  } else {
    #aantal_meetpunten_cel <- bind_rows(aantal_meetpunten_cel, aantal_meetpunten_cel_group)
    aantal_meetpunten_cel <- aantal_meetpunten_cel %>% 
      full_join(aantal_meetpunten_cel_group %>% 
                  select(-correctiefactor))
  }
}
# df <- aantal_meetpunten_cel %>% group_by(groupnr)  %>% 
#    summarise(aantal  = sum(gew_aantal_meetptn_afgerond)) %>% 
#    ungroup()
# df

aantal_meetpunten_cel

aantal_meetpunten_cel <- aantal_meetpunten_cel %>% 
  select(-bewerkt)

aantal_meetpunten_cel_overzicht <- aantal_meetpunten_cel %>% 
  group_by(groupnr) %>% 
  summarise("totaal aantal meetptn" = sum(gew_aantal_meetptn_afgerond))

sel_raster_meetnet <- 
  raster_meetnet_poly %>%
  inner_join(raster_gw_opp, by = "rasterid") %>%
  inner_join(aantal_meetpunten_cel %>% 
               select(rasterid, groupnr, gew_aantal_meetptn_afgerond), 
             by =  c("rasterid", "groupnr")) %>% 
  rename(gew_aantal_meetptn = gew_aantal_meetptn_afgerond)


sel_raster_meetnetB <- 
  sel_raster_meetnet  %>% 
  group_by(rasterid, groupnr, opp_gw_cel, gew_aantal_meetptn) %>% 
  summarise (temp = n()) %>% 
  ungroup %>% 
  select (-temp)
  
sel_raster_meetnetB <- lwgeom::st_make_valid(sel_raster_meetnetB)

sel_raster_pb <- 
  sel_raster_meetnetB %>% 
  inner_join(tubes_in_raster, 
             by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters
  left_join(tubes_lgl_eval, 
            by = "loc_code") %>% # aanduiding van pb met een lgl
  group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
  summarise(n_tubes = n(),
            n_tubes_lgl = sum(!is.na(series) & str_ends(series, "1"))) %>% 
  ungroup %>% 
  select(-geom, geom)


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


watina <- connect_watina()
# watina <- DBI::dbConnect(odbc::odbc(),
#                          driver = "SQL Server",
#                          server = "inbo-sql08-prd.inbo.be",
#                          port = 1433,
#                          database = "W0002_00_Watina",
#                          trusted_connection = "YES",
#                          encoding = vc <- "UTF-8")
# ?watina
# ?selectlocs_xg3
# ?get_locs
# debugonce(get_locs)
tubes_hab <- get_locs(watina, mask = habmap_gw_raster_overlay, join_mask = TRUE,
                      buffer = 3, loc_type = "P", loc_validity = c("VLD", "ENT"), collect = TRUE)
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

sel_cat1A_table <- sel_cat1A_raster %>% 
  st_drop_geometry() %>% 
  group_by(rasterid, groupnr) %>% 
  summarise(gewenst_aantal_meetpunten = sum(gew_aantal_meetptn)) %>% 
  arrange(rasterid)

sel_cat1A_table %>%
  group_by(groupnr ) %>% 
  summarise('aantal cellen zonder peilbuis' = n(),
            'aantal locaties zonder pb' =  sum(gewenst_aantal_meetpunten ))

sel_cat1A_table %>%
  group_by(groupnr ) %>% 
  summarise('aantal cellen zonder peilbuis' = n())

sel_cat1A_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_cat1A_raster) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE) + tm_layout(title = "cat.1: cel zonder peilbuis" )

sel_cat1A_tm

sum(sel_cat1A_table $gewenst_aantal_meetpunten)

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

tubes_lgl_eval <- read_vc ("tubes_lgl_eval", file.path(".","data","local"))
# sessioninfo::session_info()

DBI::dbDisconnect(watina)

sel_no_tube <- sel_raster_meetnet %>% 
  anti_join(tubes_in_raster, group_by = c("rasterid", "groupnr")) %>% 
  # distinct(rasterid, groupnr) %>% 
  arrange(rasterid, groupnr)
#eerste groep rastercellen: rastercellen zonder peilbuis
sel_cat1A_raster <- sel_no_tube


sel_raster_pb <- 
  sel_raster_meetnetB %>% 
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
  

sel_cat1B_rasterA <- sel_raster_pb %>% 
  filter(n_tubes_lgl < gew_aantal_meetptn)

sel_cat1B_rasterB <- sel_raster_meetnet %>% 
  semi_join(sel_raster_toolittle_lgl %>% 
              st_drop_geometry()) %>% 
  arrange(rasterid, groupnr)

test <- sel_cat1B_rasterB %>% st_drop_geometry() %>% anti_join(sel_cat1B_rasterA %>% st_drop_geometry(), by = c("rasterid", "groupnr"))

sel_cat1B_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_cat1B_rasterA) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE) + tm_layout(title = "cat.1B: cel zonder peilbuis" )


test <- sel_raster_meetnet %>% st_drop_geometry() %>% anti_join(sel_raster_pb%>% st_drop_geometry(), by = c("rasterid", "groupnr"))

test2 <- test %>% anti_join(sel_no_tube%>% st_drop_geometry(), by = c("rasterid", "groupnr"))

sel_cat1B_tm

sel_cat1B_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_cat1B_rasterB) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE) + tm_layout(title = "cat.1B: cel zonder peilbuis" )

sel_cat1B_tm

sel_cat1A_rasterB <- sel_raster_meetnet %>% 
  left_join(sel_raster_pb %>% 
              st_drop_geometry()%>% 
              select(-gew_aantal_meetptn),
            by = c("rasterid", "groupnr")) %>% 
  filter(is.na(n_tubes) | gew_aantal_meetptn > n_tubes)

sel_cat1A_rasterC <- sel_raster_meetnetB %>% 
  left_join(sel_raster_pb %>% 
              st_drop_geometry() %>% 
              select(-gew_aantal_meetptn),
            by = c("rasterid", "groupnr")) %>% 
  filter(is.na(n_tubes) | gew_aantal_meetptn > n_tubes)%>% 
  arrange(rasterid, groupnr)

sel_cat1A_raster <- sel_raster_meetnetB %>% 
  left_join(sel_raster_pb %>% 
              st_drop_geometry() %>% 
              select(-gew_aantal_meetptn),
            by = c("rasterid", "groupnr")) %>% 
  filter(is.na(n_tubes) | gew_aantal_meetptn > n_tubes)%>% 
  arrange(rasterid, groupnr)

sel_cat1A_table <- sel_cat1A_raster %>% 
  st_drop_geometry() %>% 
  rename(gewenst_aantal_meetpunten = gew_aantal_meetptn) %>% 
  mutate(totaal_peilbuizen =  replace_na(n_tubes,0),
         aantal_cat1A = gewenst_aantal_meetpunten - totaal_peilbuizen) %>% 
  select (- n_tubes) %>% 
  arrange(rasterid)

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



sel_cat1B_table <- sel_cat1B_raster %>% 
  st_drop_geometry() %>% 
  arrange(rasterid)

tubes_cat1B <- 
  sel_cat1B_raster %>% 
  inner_join(tubes_in_raster, 
             by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters
  # inner_join(tubes_lg3_avail, 
  #            by = "loc_code") %>% # aanduiding van pb lg3, geen lgl
  st_drop_geometry() %>% 
  group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
  select(rasterid, groupnr, gew_aantal_meetptn, loc_code, everything(),
         -starts_with("loc_v"), -starts_with("loc_t")) %>% 
  arrange(rasterid, groupnr)

tubes_cat1B %>% distinct(rasterid, groupnr)

head(tubes_cat1B, 10)


toelaatbare_spreiding_jaren <- 5
toelaatbaar_verschil_lengte_tijdreeks <- 5
#clusteren meetpunten binnen een rastercel obv lengte tijdreeks en obv laatste meetjaar
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

# check <- sel_qual_lastyear_vb %>% 
#   filter(rasterid == 134, groupnr == 4)

#test
# sel_qual_lastyear <- sel_qual_lastyear %>%
#   mutate(gew_aantal_meetptn = case_when(
#     rasterid == 198 & groupnr == 4 ~ 6,
#     TRUE ~ gew_aantal_meetptn
#   ))

sel_qual_test <- sel_qual %>%
  mutate(gew_aantal_meetptn = case_when(
    rasterid == 134 & groupnr == 4  ~ 5,
    TRUE ~ gew_aantal_meetptn
  ))

sel_qual_maxrank <- plyr::ddply(sel_qual, ~rasterid+groupnr, max_rank) %>%
  rename(maxrank = V1)

sel_qual <- sel_qual %>%
  inner_join(sel_qual_maxrank, 
             by = c("rasterid", "groupnr"))

# rastercellen met een juist voldoende aantal evenwaardige meetpunten dat gewenst is voor het meetnet
sel_sufficient_lgl <- 
  sel_qual %>% 
  filter(rankclus <= maxrank ) %>% 
  group_by(rasterid, groupnr, rest_aantal_meetptn) %>% 
  summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
  ungroup %>% 
  inner_join(sel_qual) %>% 
  filter(beschikbaar_aantal == rest_aantal_meetptn, rankclus <= maxrank) %>% 
  select(-beschikbaar_aantal, -maxrank)

sel_raster_cat2 <- sel_raster_pb %>% 
  semi_join(sel_sufficient_lgl) %>% 
  arrange(rasterid, groupnr)

# de bijhorende geselecteerde Watina-meetpunten zijn dan
tubes_group2 <- 
  tubes_in_raster %>% 
  inner_join(tubes_lg3_eval %>% 
               select(loc_code, ser_lastyear, ser_nryears), 
             by = "loc_code") %>% 
  inner_join(sel_sufficient_lgl  %>% 
               inner_join(sel_qual_basis %>% 
                            select(-rest_aantal_meetptn), 
                          by =  c("rasterid","groupnr","rankclus_lastyear", "rankclus_nryears" )), 
             by = c("rasterid", "groupnr", "ser_lastyear", "ser_nryears"))


write_vc(tubes_group2, file.path(".","data","tubes_group2"), sorting = c("loc_code"),
         strict =  FALSE, root = ".")

sel_cat2_table <- 
  sel_qual %>% 
  filter(rankclus <= maxrank ) %>% 
  group_by(rasterid, groupnr, rest_aantal_meetptn) %>% 
  summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
  ungroup %>% 
  inner_join(sel_qual) %>% 
  filter(beschikbaar_aantal == rest_aantal_meetptn, rankclus <= maxrank) %>% 
  select(-beschikbaar_aantal, -maxrank)

sel_cat2_raster <- sel_raster_pb %>% 
  semi_join(sel_cat2_table) %>% 
  arrange(rasterid, groupnr)

tubes_cat2 <- 
  tubes_in_raster %>% 
  inner_join(tubes_lgl_eval %>% 
               select(loc_code, ser_firstyear, ser_lastyear, ser_nryears, ser_length), 
             by = "loc_code") %>% 
  inner_join(sel_cat2_table  %>% 
               inner_join(sel_qual_basis %>% 
                            select(-rest_aantal_meetptn), 
                          by =  c("rasterid","groupnr","rankclus_lastyear", "rankclus_nryears" )), 
             by = c("rasterid", "groupnr", "ser_lastyear", "ser_nryears"))

sel_cat3_raster <- 
  sel_raster_meetnetB %>% 
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

sel_excess_lgl <- 
  sel_qual %>% 
  filter(rankclus <= maxrank ) %>% 
  group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
  summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
  ungroup %>% 
  inner_join(sel_qual ) %>% 
  filter(beschikbaar_aantal > gew_aantal_meetptn, rankclus <= maxrank) %>% 
  select(-beschikbaar_aantal, -maxrank) 

sel_cat3_table <- 
  sel_qual %>% 
  filter(rankclus <= maxrank ) %>% 
  group_by(rasterid, groupnr, rest_aantal_meetptn) %>% 
  summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
  ungroup %>% 
  inner_join(sel_qual, by = c("rasterid", "groupnr", "rest_aantal_meetptn") ) %>% 
  filter(beschikbaar_aantal > rest_aantal_meetptn, rankclus <= maxrank) %>% 
  select(-beschikbaar_aantal, -maxrank)

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

test2<- 
  sel_cat1B_raster %>% 
  inner_join(tubes_in_raster, 
             by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters
  inner_join(tubes_lg3_avail, 
             by = "loc_code") %>% # aanduiding van pb lg3, geen lgl
  st_drop_geometry() %>% 
  select(rasterid, groupnr, gew_aantal_meetptn, loc_code, everything(), -xg3_variable,
         -starts_with("loc_v"), -starts_with("loc_t")) %>% 
  arrange(rasterid, groupnr, desc(nryears))


tubes_menyanthes <- read_csv(file.path(getwd(),"data", "tblTubes_Menyanthes_report.csv"))
tubes_menyanthes <- janitor::clean_names(tubes_menyanthes, case = "snake")

#summary(tubes_menyanthes)
tubes_menyanthes <- tubes_menyanthes %>% 
  mutate(
    uitspraak = case_when(
      modelbaar == 1 ~ "weerhouden, expertoordeel",
      !is.na(evaporatiefactor) ~ "niet weerhouden, expertoordeel",
      evp < 66 ~ "niet weerhouden, te lage modelfit",
      is.na(trend_verschil) | abs(trend_verschil) - 2*trend_sd <= trend_jaren ~ "weerhouden",
      abs(trend_verschil) - 2*trend_sd > trend_jaren ~ "niet weerhouden, trend"
    ),
    selectie = if_else(str_detect(uitspraak, pattern="niet weerhouden"),0,1)
  )

tubes_menyanthes %>% 
  group_by(uitspraak) %>% 
  summarise(test = n()) 

tubes_menyanthes %>% 
  count(uitspraak) %>% 
  rename(aantal = n)

tubes <- bind_rows(tubes_cat1B %>% ungroup() %>% select(loc_code),tubes_cat2  %>% select(loc_code), tubes_cat3 %>% select(loc_code)) %>% arrange(loc_code)

tubes_menyanthes %>% anti_join(tubes, by = c("Watinacode" = "loc_code" ))
tubes %>% anti_join(tubes_menyanthes, by = c("loc_code" = "Watinacode" ))

tubes_lengthtimeserie <- tubes_xg3 %>% 
  count(loc_code)
head(tubes_menyanthes)

tubes_menyanthes <- tubes_menyanthes %>% 
  semi_join(tubes_in_raster, by = c("watinacode" = "loc_code"))

tubes_menyanthes_synthese <- tubes_menyanthes %>% 
  count(uitspraak) %>% 
  rename(aantal = n)

# sel_raster_pb_bis <- 
#   sel_raster_meetnetB %>% 
#   inner_join(tubes_in_raster %>% 
#                left_join(tubes_menyanthes %>% 
#                            select(watinacode, selectie), by = c("loc_code" = "watinacode")) , 
#              by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters
#   left_join(tubes_lgl_eval, 
#             by = "loc_code") %>% 
#   mutate(selectie = case_when(
#             selectie == 0 ~ -1, #afgekeurde meetreeks
#             selectie == 1 ~ 1, #goedgekeurde meetreeks
#             TRUE ~ 0 #niet geanalyseerde meetreeks
#             )
#           ) %>% # aanduiding van pb met een lgl
#   group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
#   summarise(n_tubes = n(),
#             n_tubes_lgl = sum(!is.na(series) & str_ends(series, "1")) + sum(selectie)) %>% 
#   mutate(n_tubes_lgl = if_else(n_tubes_lgl<0,0,n_tubes_lgl)) %>% 
#   ungroup %>% 
#   select(-geom, geom)

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

#test correctie voor DYLP109
tubes_eval_namenyanthes[tubes_eval_namenyanthes$loc_code == "DYLP109",colnames(tubes_eval_namenyanthes) == "selectie"] <- -1
sel_raster_pb_bis <- 
  sel_raster_meetnetB %>% 
  inner_join(tubes_eval_namenyanthes, 
             by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters 
  group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
  summarise(n_tubes = n(),
            n_tubes_lgl = 
              sum(!is.na(series) & str_ends(series, "1") & selectie >= 0) + sum(selectie > 0)) %>% 
  ungroup %>% 
  select(-geom, geom)



test  <- sel_raster_pb_bis %>% st_drop_geometry() %>% 
  summarise(sum(n_tubes_lgl))

test2 <- sel_raster_pb %>% st_drop_geometry() %>% 
  summarise(sum(n_tubes_lgl))



all.equal(sel_raster_pb_bis %>% st_drop_geometry() %>% select(-geom),sel_raster_pb %>% st_drop_geometry() %>% select(-geom))

test <- unique(tubes_menyanthes$watinacode)


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

sel_cat1B_table_bis %>% 
  group_by(groupnr ) %>% 
  summarise('aantal cellen met een onvoldoend aantal geschikte peilbuizen' = n(),
            'aantal gewenste meetlocaties met een, momenteel ongeschikte, pb' =  sum(aantal_cat1B)) %>% 
  rename(GTgroep = groupnr
  )

sel_cat1B_raster_bis %>% st_drop_geometry() %>% 
  anti_join( sel_cat1B_raster %>% st_drop_geometry(), by = c("rasterid", "groupnr", "n_tubes"))



tubes_cat1B_bis <- 
  sel_cat1B_raster_bis %>% 
  inner_join(tubes_in_raster, 
             by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters
  inner_join(tubes_menyanthes %>% 
               select(watinacode, selectie) %>% 
               filter (selectie ==0), by = c("loc_code" = "watinacode")) %>% 
  st_drop_geometry() %>% 
  select(rasterid, groupnr, gew_aantal_meetptn, loc_code, everything(),
         -starts_with("loc_v"), -starts_with("loc_t")) %>% 
  arrange(rasterid, groupnr)

tubes_cat1B_bis <- 
  sel_cat1B_raster_bis %>% 
  inner_join(tubes_eval_namenyanthes %>% 
               filter(selectie <= 0), by = c("rasterid", "groupnr")) %>% 
  st_drop_geometry() %>% 
  select(rasterid, groupnr, gew_aantal_meetptn, loc_code, everything(),
         -starts_with("loc_v"), -starts_with("loc_t"), -starts_with("ser"), -starts_with("xg3")) %>% 
  arrange(rasterid, groupnr)

tubes_cat1B_tres %>% 
  anti_join(tubes_cat1B_bis, by = "loc_code")

tubes_cat1Ba <- tubes_cat1B_bis %>% 
  inner_join(tubes_cat1B_bis %>% 
               group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
               summarise(n_tubes_zondermodel = n())
             ) %>% 
  filter(gew_aantal_meetptn == n_tubes_zondermodel)
tubes_cat1Bb <- tubes_cat1B_bis %>% 
  anti_join(tubes_cat1Ba, by = "loc_code")


tubes_qual_basis_bis <- 
  sel_raster_pb_bis %>% 
  select(rasterid, groupnr, gew_aantal_meetptn) %>% 
  st_drop_geometry() %>% 
  left_join(sel_cat1A_table %>% 
              select(rasterid, groupnr, aantal_cat1A), 
            by = c("rasterid", "groupnr")) %>%
  left_join(sel_cat1B_table_bis %>% 
              select(rasterid, groupnr, aantal_cat1B), 
            by = c("rasterid", "groupnr")) %>%
  inner_join(tubes_eval_namenyanthes, 
             by =  c("rasterid", "groupnr")) %>% 
  distinct(rasterid, groupnr, gew_aantal_meetptn, loc_code, 
           aantal_cat1A, aantal_cat1B, ser_lastyear, ser_nryears, 
           ser_length, ser_firstyear, selectie) %>%
  filter(selectie == 1 | (ser_nryears > 0 & selectie >= 0)) %>% 
  mutate(aantal_cat1A = replace_na(aantal_cat1A,0),
         aantal_cat1B = replace_na(aantal_cat1B,0),
         ser_lastyear = replace_na(ser_lastyear,2019),
         ser_nryears = replace_na(ser_nryears,100),
  )

sel_qual_basis_bis <- 
  tubes_qual_basis_bis %>% 
  group_by(rasterid, groupnr, gew_aantal_meetptn, aantal_cat1A, aantal_cat1B) %>% 
  count(ser_lastyear, ser_nryears)  %>% 
  ungroup() %>% 
  mutate(rest_aantal_meetptn = gew_aantal_meetptn - aantal_cat1A - aantal_cat1B) %>% 
  filter(rest_aantal_meetptn > 0)

sel_qual_basis_bis <- 
  sel_qual_basis_bis %>% 
  group_by(rasterid, groupnr) %>% 
  arrange(rasterid, groupnr, rest_aantal_meetptn, desc(ser_lastyear)) %>% 
  mutate(rankclus_lastyear = 
           floor((cummax(ser_lastyear) - ser_lastyear)/toelaatbare_spreiding_jaren) + 1) %>% 
  arrange(rasterid, groupnr, rest_aantal_meetptn, desc(ser_nryears)) %>% 
  mutate(rankclus_nryears = 
           floor((cummax(ser_nryears) - ser_nryears)/toelaatbaar_verschil_lengte_tijdreeks) + 1) %>% 
  ungroup()

test <- tub_qual_basis_bis %>% 
  anti_join(sel_qual_basis_bis)

sel_qual_bis <- 
  sel_qual_basis_bis %>% 
  group_by(rasterid, groupnr, rest_aantal_meetptn, rankclus_lastyear, rankclus_nryears) %>% 
  mutate(rankclus_temp = as.integer(paste0(rankclus_lastyear,rankclus_nryears))) %>% 
  arrange(rasterid, groupnr,rankclus_temp) %>% 
  ungroup() %>% 
  group_by(rasterid, groupnr) %>% 
  mutate(rankclus = dense_rank(rankclus_temp)) %>% 
  group_by(rasterid, groupnr, rest_aantal_meetptn, rankclus, rankclus_lastyear, rankclus_nryears) %>% 
  summarise(beschikbaar_aantal_cluster = sum(n)) %>% 
  ungroup 

sel_qual_maxrank_bis <- plyr::ddply(sel_qual_bis, ~rasterid+groupnr, max_rank) %>%
  rename(maxrank = V1)

sel_qual_bis <- sel_qual_bis %>%
  inner_join(sel_qual_maxrank_bis, 
             by = c("rasterid", "groupnr"))

sel_cat2_table_bis <- 
  sel_qual_bis %>% 
  filter(rankclus <= maxrank ) %>% 
  group_by(rasterid, groupnr, rest_aantal_meetptn) %>% 
  summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
  ungroup %>% 
  inner_join(sel_qual_bis,
             by = c("rasterid", "groupnr", "rest_aantal_meetptn")) %>% 
  filter(beschikbaar_aantal == rest_aantal_meetptn, rankclus <= maxrank) %>% 
  select(-beschikbaar_aantal, -maxrank)

sel_cat2_table %>% 
  anti_join(sel_cat2_table_bis)

tubes_cat2_bis <- 
  tubes_eval_namenyanthes %>% 
  filter(selectie == 1 | (ser_nryears > 0 & selectie >= 0 )) %>% 
    inner_join(sel_cat2_table_bis  , 
             by = c("rasterid", "groupnr"))

tubes_cat2_bis <- 
  tubes_qual_basis_bis %>% 
    filter(selectie >= 0) %>% 
    inner_join(sel_qual_basis_bis) %>% #toevoeging is nodig om onderscheid te maken/behouden tussen de clusters
    inner_join(sel_cat2_table_bis)

tubes_cat2_bis %>% 
  anti_join(tubes_cat2_tris, by = "loc_code")

tubes_cat2_bis
sel_cat2_table_bis %>% 
  anti_join(tubes_cat2_bis)


sel_cat2_raster_bis <- sel_raster_pb_bis %>% 
  semi_join(sel_cat2_table_bis,
            by = c("rasterid", "groupnr")) %>% 
  arrange(rasterid, groupnr)

sel_cat2_tm_bis <- raster_meetnet_poly_tm + 
  tm_shape(sel_cat2_raster_bis) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.2: rastercellen met een juist voldoende aantal peilbuizen" )

sel_cat2_tm_bis

sel_cat3_raster_bis <- 
  sel_raster_meetnetB %>% 
  anti_join(sel_cat1A_raster %>% 
              st_drop_geometry(), 
            by = c("rasterid", "groupnr"))  %>% 
  anti_join(sel_cat1B_raster_bis %>% 
              st_drop_geometry(), 
            by = c("rasterid", "groupnr"))  %>% 
  anti_join(sel_cat2_raster_bis %>% 
              st_drop_geometry(), 
            by = c("rasterid", "groupnr")) %>% 
  arrange(rasterid, groupnr)


sel_cat3_table_bis <- 
  sel_qual_bis %>% 
  filter(rankclus <= maxrank ) %>% 
  group_by(rasterid, groupnr, rest_aantal_meetptn) %>% 
  summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
  ungroup %>% 
  inner_join(sel_qual_bis, by = c("rasterid", "groupnr", "rest_aantal_meetptn") ) %>% 
  filter(beschikbaar_aantal > rest_aantal_meetptn, rankclus <= maxrank) %>% 
  select(-beschikbaar_aantal, -maxrank)  

test <- sel_cat3_raster_bis %>% 
  anti_join(sel_cat3_table_bis, by = c("rasterid", "groupnr"))

tubes_cat3_bis <- 
  tubes_in_raster %>% 
  distinct(loc_code, x, y, rasterid, groupnr) %>% 
  inner_join(tubes_lgl_eval %>% 
               select(loc_code, ser_length, ser_firstyear, 
                      ser_lastyear, ser_nryears), 
             by = "loc_code") %>% 
  inner_join(sel_cat3_table_bis  %>% 
               inner_join(sel_qual_basis_bis %>% 
                            select(-rest_aantal_meetptn), 
                          by = c("rasterid","groupnr","rankclus_lastyear","rankclus_nryears" )), 
             by = c("rasterid","groupnr", "ser_lastyear","ser_nryears"))

tubes_cat3_bis <- 
  tub_qual_basis_bis %>% 
  filter(selectie >= 0) %>% 
  inner_join(sel_qual_basis_bis) %>% 
  inner_join(sel_cat3_table_bis)

tubes_cat1Bb <- tubes_cat1B_bis %>% 
  anti_join(tubes_cat1Ba, by = "loc_code")

tubes_cat1Bb <- tubes_cat1B_bis %>% 
  anti_join(tubes_cat1Ba, by = "loc_code") %>% 
  inner_join(tubes_cat1B_bis %>% 
               group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
               summarise(n_tubes_zondermodel = n())
  ) 

tubes_cat1Ba %>% 
  select(-n_tubes_lgl, -aantal_cat1A, -aantal_cat1B,
         -typegroup_name, -selectie) %>% 
  select(1:3,10,7,4,8:9) %>% 
  rename(GTgroep = groupnr,
         rasternr = rasterid,
         watinacode = loc_code,
         'gewenst aantal meetptn' = gew_aantal_meetptn,
         #'# peilbuizen' =  n_tubes,
         gebied = area_name,
         #gebiedcode = area_code,
         '# op te waarderen ptn' = n_tubes_zondermodel) %>% 
  arrange(watinacode)         


sel_cat2_table_bis <- 
  sel_qual_bis %>% 
  filter(rankclus <= maxrank ) %>% 
  group_by(rasterid, groupnr, rest_aantal_meetptn) %>% 
  summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
  ungroup %>% 
  inner_join(sel_qual_bis,
             by = c("rasterid", "groupnr", "rest_aantal_meetptn")) %>% 
  filter(beschikbaar_aantal == rest_aantal_meetptn, rankclus <= maxrank) %>% 
  select(-beschikbaar_aantal, -maxrank)

tubes_cat3_bis <- 
  tubes_qual_basis_bis %>% 
  filter(selectie >= 0) %>% 
  inner_join(sel_qual_basis_bis) %>% 
  inner_join(sel_cat3_table_bis)

tubes_cat3_bis <- tubes_cat3_bis %>% 
  rownames_to_column("unieknr") %>% 
  mutate(unieknr = as.integer(unieknr))

tubes_cat3_bis %>% 
  mutate(ser_nryears = ifelse(is.na(ser_length), NA, ser_nryears),
         ser_firstyear = ifelse(is.na(ser_length), NA, ser_firstyear),
         ser_lastyear = ifelse(is.na(ser_length), NA, ser_lastyear)
  ) %>% 
  select(loc_code, ser_length, ser_nryears, ser_firstyear,
         ser_lastyear) %>% 
  rename(watinacode = loc_code,
         'lengte tijdreeks' = ser_length,
         'aantal lg3' = ser_nryears,
         beginjaar = ser_firstyear,
         eindjaar = ser_lastyear
  )

#zijn er nog meetreeksen die met Menyanthes moeten geanalyseerd worden?
if (nrow(tubes_cat3_bis %>% filter(selectie == 0)) > 0){
  message("Er zijn nog enkele meetreeksen die best met Menyanthes onderzocht worden.")
  nognietklaar <- TRUE
} else{
  message("Er hoeven geen bijkomende meetreeksen met Menyanthes onderzocht te worden.")
  nognietklaar <- FALSE  
}


#gislaag maken van de peilbuizen waaruit kan gekozen worden, inclusief de gw-groep tot dewelke ze gerekend kunnen worden (een pb kan tot meerdere gw-groepen behoren)

str(sel_cat3_raster_bis)
str(tubes_cat3_bis)
tubes_excess <- tubes_in_raster %>% 
            inner_join(tubes_cat3_bis %>% 
              mutate(cat = 3) %>%  
              select(loc_code, cat, gew_aantal_meetptn) %>% 
              bind_rows(tubes_cat1Bb %>% 
                          mutate(cat = 1) %>%  
                          select(loc_code, cat, gew_aantal_meetptn)
              )
            , by = "loc_code") 

#toevoegen van een uniek rijnummer
tubes_excess <- rownames_to_column(tubes_excess, "unieknr") %>% 
  mutate(unieknr = as.integer(unieknr))

tubes_excess_sf <- as_points(tubes_excess)


tubes_excess_tm <- raster_meetnet_poly_tm + 
  tm_shape(tubes_excess_sf %>% mutate (cat = as.factor(cat))) + 
  tm_symbols(size = 0.25, shapes.labels = "loc_code", col = "cat", clustering = FALSE) + tm_layout(title = "beschikbare peilbuizen voor selectie" )

tubes_excess_tm
# tmap_tip()
# data(metro)
# tm_shape(metro[metro$pop2020 > 5e6, ]) +
#   tm_symbols(size = "pop2020", shape = tmap_icons(system.file("img/city.png", package = "tmap")))
# plot(tubes_excess_gw_raster_overlay %>% select(loc_code))

#rastercellen met een overschot aan goede pb
# sel_excess <- 
#   tubes_excess_gw_raster_overlay %>%
#   st_drop_geometry() %>% 
#   distinct(rasterid) %>% 
#   arrange(rasterid)


#inlezen grts-raster level 1 (hoogste resolutie = kleinste gridcelgrootte)
grts_level1 <- read_GRTSmh(brick = TRUE) %>% 
  raster::subset(1)

#inlezen grts-raster level 9 (resolutie = raster_meetnet_poly), het heeft een gridgrootte van 8192 m, let wel de rastercelgrootte is ook hier 32 bij 32m, dus het aantal rastercellen = grts-raster level 1. 
grts_level9 <- read_GRTSmh(brick = TRUE) %>% 
  raster::subset(9)

#raster maken ter grootte van grid-grootte (5182m) waarmee de grote rasterbestanden kunnen verkleind (geknipt) worden

#Alle geselecteerde gridcellen wordt overlopen in een dubbele loop (loop 1: alle gridcellen, loop2: alle gw-groepen binnen een gridcel)

# tubes_excess <- tubes_excess %>% 
#   mutate(selecteerbaar = 1)
# # hierna kunnen indien nodig peilbuizen van de selectiegroep uitgesloten worden, omdat de praktijk heeft uitgewezen dat deze niet geschikt zijn om opgenomen te worden. De variabele selecteerbaar wordt dan op -1 gezet.
# 
# uitgesloten_tubes <- c("MOSP001", "HALP005", "BUIP027")

# reserve <- tubes_excess

sel_excess <- tubes_cat1Bb %>% 
  select(rasterid, groupnr, gew_aantal_meetptn) %>% 
  distinct() %>% 
  bind_rows(sel_cat3_table_bis %>% 
              select(rasterid, groupnr, rest_aantal_meetptn) %>%
              rename(gew_aantal_meetptn = rest_aantal_meetptn)) %>% 
  arrange(rasterid, groupnr, gew_aantal_meetptn) %>% 
  distinct()

sel_excess_rasterid <- sel_excess %>% 
  distinct(rasterid)

tubes_excess$geselecteerd <- 0
tubes_excess$reserve <- 0
rasterid_grid <- 177

for (i in seq(1:nrow(sel_excess_rasterid))) {
  rasterid_grid <- sel_excess_rasterid[i,] %>% as.integer()
  clip9 <- grts_level9[grts_level9 == rasterid_grid, drop =  FALSE]
  clip1 <- grts_level1[clip9, drop =  FALSE]
  
  tubes_excess_1grid <- tubes_excess %>% 
    filter(rasterid == rasterid_grid) %>% 
    count(groupnr, gew_aantal_meetptn) %>% 
    rename(aantalpb = n)
  
  # plot(tubes_excess_level1)
  for (j in seq(1:nrow(tubes_excess_1grid))) {
    #j <- 2
    # sel_excess_one <- tubes_excess_1grid %>% 
    #   filter(rasterid == rasterid_grid) %>%
    #   # arrange(beschikbaar_aantal_cluster) %>% 
    #   slice(j) 
    # ophalen van bijhorend aantal gewenste meetpunten en gw-groep, want een rastercel kan meerdere gw-groepen hebben waar er een overtal is (en het gewenste aantal meetpunten is specifiek per gw-groep)
    gewenst_aantal_pb <- tubes_excess_1grid %>% 
      slice(j) %>% 
      pull(gew_aantal_meetptn) %>% 
      as.integer()
    gwgroup <- tubes_excess_1grid %>% 
      slice(j) %>%
      pull(groupnr) %>% 
      as.integer()
    aantalpb <- tubes_excess_1grid %>% 
      slice(j) %>%
      pull(aantalpb) %>% 
      as.integer()
    
    #binnen een deelraster (clip1), alleen de rastercellen van level1 selecteren waarbinnen een pb valt. 
    #De andere rastercellen worden NA
    tubes_excess_level1 <- 
      raster::rasterize(tubes_excess %>% 
                          filter(groupnr == gwgroup) %>% 
                          select(x, y) %>% 
                          as.matrix(), 
                        y = clip1, #raster-object
                        mask = TRUE)
    
    #rangorde bepalen van de grts-nrs van de geselecteerde rastercellen 
    rank_cells_level1 <- tubes_excess_level1 %>% 
      raster::getValues() %>% 
      as.data.frame()
    names(rank_cells_level1) <- "celwaarde" 
    rank_cells_level1 <- rank_cells_level1 %>% 
      filter(!is.na(celwaarde)) %>%  
      distinct() %>% 
      mutate(minrank = min_rank(celwaarde)) %>% 
      arrange(minrank) 
    
    # rank_cells_level1
    
    # raster met grts-nrs herindexeren naar 0, 1 en 2 waarden. Het aantal 2-waarden stemt overeen met het gewenst aantal peilbuizen (voor dat grid), 1 zijn de reservepunt(en).
    # Hiervoor moet er eerst een n*2 matrix (rcl) gemaakt worden met de oude en nieuwe celwaarde.
    rcl <- data.frame("grtsnr" = rank_cells_level1 %>% pull(celwaarde), 
                      "selectie" = 
                        c(rep(1,gewenst_aantal_pb), seq(from = gewenst_aantal_pb + 1, to = nrow(rank_cells_level1)))) %>% 
      as.matrix()
    
    #herindexeren
    tubes_excess_level1_rcl <- raster::reclassify(tubes_excess_level1, rcl)
    
    #raster maken met de unieke nummers van de pb, maar dat enkel voor het gewenste aantal pb
    tubes_excess_level1_unieknr <- raster::rasterize(tubes_excess %>% 
                                                       filter(groupnr == gwgroup) %>% 
                                                       select(x, y) %>% 
                                                       as.matrix(),
                                                     tubes_excess_level1_rcl[tubes_excess_level1_rcl == 1, 
                                                                             drop = FALSE],
                                                     field = tubes_excess %>% 
                                                       filter(groupnr == gwgroup) %>%
                                                       select(unieknr), 
                                                     mask = FALSE)
    
    #ophalen van de unieke nummers
    tubes_excess_selected_unieknr <- tubes_excess_level1_unieknr %>% 
      raster::getValues() %>% 
      as.data.frame()
    names(tubes_excess_selected_unieknr) <- "unieknr" 
    tubes_excess_selected_unieknr <- tubes_excess_selected_unieknr %>% 
      filter(!is.na(unieknr)) %>%  
      distinct() %>% 
      arrange(unieknr) %>% 
      pull(unieknr)
    
    #peilbuis als geselecteerd markeren
    tubes_excess[tubes_excess$unieknr %in% tubes_excess_selected_unieknr, "geselecteerd"] <- 1

    #idem maar nu voor de reservepunten
    #raster maken met de unieke nummers van de pb, maar dat enkel voor de reservepunten
    for (k in seq(from = (gewenst_aantal_pb + 1), to = nrow(rank_cells_level1))) {
 
      # #herindexeren
      # tubes_excess_level1_rcl <- raster::reclassify(tubes_excess_level1, rcl)
      
      tubes_excess_level1_unieknr <- raster::rasterize(tubes_excess %>% 
                                                         filter(groupnr == gwgroup) %>% 
                                                         select(x, y) %>% 
                                                         as.matrix(),
                                                       tubes_excess_level1_rcl[tubes_excess_level1_rcl == k, 
                                                                               drop = FALSE],
                                                       field = tubes_excess %>% 
                                                         filter(groupnr == gwgroup) %>%
                                                         select(unieknr), 
                                                       mask = FALSE)
      
      #ophalen van de unieke nummers
      tubes_excess_selected_unieknr <- tubes_excess_level1_unieknr %>% 
        raster::getValues() %>% 
        as.data.frame()
      names(tubes_excess_selected_unieknr) <- "unieknr" 
      tubes_excess_selected_unieknr <- tubes_excess_selected_unieknr %>% 
        filter(!is.na(unieknr)) %>%  
        distinct() %>% 
        arrange(unieknr) %>% 
        pull(unieknr)
      
      #peilbuis als geselecteerd markeren
      tubes_excess[tubes_excess$unieknr %in% tubes_excess_selected_unieknr, "reserve"] <- k
    }
    # #opzoeken van pb met die unieke nr(s)
    # tubes_excess_selected_part <- 
    #   tubes_excess %>% 
    #   inner_join(tubes_excess_selected_unieknr, 
    #              by = "unieknr") %>% 
    #   rename(geselecteerd = selecteerbaar) %>% 
    #   select(unieknr, geselecteerd)
    # #markeren en ook zo vermijden dat een pb twee keer wordt geselecteerd
    # tubes_excess <- 
    #   tubes_excess %>% 
    #   left_join(tubes_excess_selected_part, 
    #             by = "unieknr") %>% 
    #   mutate(selecteerbaar = ifelse(is.na(geselecteerd), selecteerbaar, 0)) %>% 
    #   select(-geselecteerd)
    
  } #loop gw-groepen
} # loop gridcellen

#aanduiden welke pb geselecteerd zijn
tubes_cat3_gtrs <- tubes_excess %>% 
  filter(geselecteerd == 1 & cat == 3) %>% 
  arrange(rasterid, groupnr, loc_code)

tubes_cat1Bb_gtrs <- tubes_excess %>% 
  filter(geselecteerd == 1 & cat == 1) %>% 
  arrange(rasterid, groupnr, loc_code)

check_aantal <- tubes_cat3_bis %>% 
  distinct(rasterid, groupnr, gew_aantal_meetptn) %>% 
  summarise(som_aantal_meetptn = sum(gew_aantal_meetptn))

check_aantal <- tubes_cat1Bb %>% 
  distinct(rasterid, groupnr, gew_aantal_meetptn) %>% 
  summarise(som_aantal_meetptn = sum(gew_aantal_meetptn))

#wegschrijven van het resultaat, omdat de berekening hiervan toch wel enkele minuten tijd vraagt.
write_vc(tubes_cat3_gtrs, file.path(".","data","tubes_cat3_gtrs"), sorting = c("rasterid","groupnr", "loc_code"), strict =  FALSE)
write_vc(tubes_cat1Bb_gtrs, file.path(".","data","tubes_cat1Bb_gtrs"), sorting = c("rasterid","groupnr", "loc_code"), strict =  FALSE)

# test <- read_vc(file.path(".","data","tubes_group4"))


test <- sel_cat1B_raster_bis %>% 
  semi_join(tubes_cat1Ba %>% 
              select(rasterid, groupnr) %>% 
              distinct()
            ) %>% 
  select(1:3) %>% 
  bind_rows(tubes_cat3 %>% select(rasterid, groupnr) %>% distinct() )

test %>% count(rasterid, groupnr) %>% arrange (desc(n))

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

tubes_cat1Bb <- tubes_cat1B_bis %>% 
  anti_join(tubes_cat1Ba, by = "loc_code") %>% 
  inner_join(tubes_cat1B_bis %>% 
               group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
               summarise(n_tubes_zondermodel = n())
  ) 
write_csv(tubes_cat1B_bis %>% 
            anti_join(tubes_cat1B, by = "loc_code"), file.path(".","data", "local","tubes_1B_nolgl3.csv"))

test <- 
  sel_cat1B_raster %>% 
  inner_join(tubes_in_raster, 
             by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters
  # inner_join(tubes_lg3_avail, 
  #            by = "loc_code") %>% # aanduiding van pb lg3, geen lgl
  st_drop_geometry() %>% 
  select(rasterid, groupnr, gew_aantal_meetptn, loc_code, everything(), 
         -starts_with("loc_v"), -starts_with("loc_t")) %>% 
  arrange(rasterid, groupnr)

test %>% 
  select(-n_tubes_lgl, -aantal_cat1A, -typegroup_name) %>% 
  rename(GTgroep = groupnr,
         rasternr = rasterid,
         watinacode = loc_code,
         'gewenst aantal meetptn' = gew_aantal_meetptn,
         '# peilbuizen' =  n_tubes,
         gebied = area_name,
         gebiedcode = area_code,
         '# op te waarderen ptn' = aantal_cat1B
  ) %>% 
  arrange(watinacode)

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

dbWriteTable(con, "habitatmap_terr_patches_gw", habmap_patches_gw)

dbDisconnect(con)
test <- drive_get(id = "1nxnpfE3Eh4eCiM2VinGYMJE55qD4Az1c")
test2 <- drive_download(drive_get(id  = "1nxnpfE3Eh4eCiM2VinGYMJE55qD4Az1c"), path = file.path(".","data","local", "habmap_terr_gw.gpkg"), overwrite = TRUE)

drive_download(drive_get(id = "1o4-92mysyQL-kusSYxNKlhyHgyeXkj22"), path = file.path(".","data","local", "droogtemeetnet_gis.gpkg"), overwrite = TRUE)

drive_download(test)

habmap_polygons_gw <- read_sf(file.path(".","data","local", "habmap_terr_gw.gpkg"),
                "habitatmap_terr_polygons_gw")

habmap_polygons_gw <- habmap_polygons_gw %>%
  mutate(polygon_id = factor(.data$polygon_id),
         type = factor(.data$type),
         typegroup_name = factor(.data$typegroup_name)
         )

habmap_patches_gw <- suppressWarnings(read_sf(file.path(".","data","local", "habmap_terr_gw.gpkg"),
                "habitatmap_terr_patches_gw"))

datapath <- "G:/Mijn Drive/PRJ_Meetnet_Droogte/2_Uitvoering/data"
dir.create(file.path(datapath, "GIS/VoorR/"), recursive = TRUE)

st_write(habmap_polygons_gw,
         file.path(datapath, 
                   "GIS/VoorR/droogtemeetnet_gis.gpkg"), 
         layer = "habitatmap_terr_polygons_gw", 
         driver = "GPKG",
         delete_dsn = TRUE)

con = dbConnect(SQLite(),
                dbname = file.path(
                  datapath, 
                  "GIS/VoorR/habmap_terr_gw.gpkg")
)

dbWriteTable(con, "habitatmap_terr_patches_gw", habmap_patches_gw)



#GRTS-data
raster_meetnet_poly <- read_GRTSmh_diffres(level = 8, polygon = TRUE)

dbWriteTable(con, "raster_meetnet_poly", raster_meetnet_poly)

dbDisconnect(con)

drive_download(drive_get(id = "1nxnpfE3Eh4eCiM2VinGYMJE55qD4Az1c"), path = file.path(".","data","local", "habmap_terr_gw.gpkg"), overwrite = TRUE)


all.equal(test,raster_meetnet_poly)

st_write(raster_meetnet_poly,
         file.path(datapath, 
                   "GIS/VoorR/raster_meetnet_poly.gpkg"), 
         layer = "raster_meetnet_poly", 
         driver = "GPKG",
         delete_dsn = TRUE)

drive_download(drive_get(id = "1oHdlUEEZmCDvXDCSXELgtgKNDgLn4E_0"), path = file.path(".","data","local", "raster_meetnet_poly.gpkg"), overwrite = TRUE)

test <- suppressWarnings(read_sf(file.path(".","data","local", "raster_meetnet_poly.gpkg"),
                                 "raster_meetnet_poly"))

#inlezen grts-raster level 1 (hoogste resolutie = kleinste gridcelgrootte)
grts_level1 <- read_GRTSmh(brick = TRUE) %>% 
  raster::subset(1)

#inlezen grts-raster level 9 (resolutie = raster_meetnet_poly), het heeft een gridgrootte van 8192 m, let wel de rastercelgrootte is ook hier 32 bij 32m, dus het aantal rastercellen = grts-raster level 1. 
grts_level9 <- read_GRTSmh(brick = TRUE) %>% 
  raster::subset(9)

writeRaster(filename = "../../n2khab_data/20_processed/GRTSmh_brick/GRTSmh_brick.tif",
            format = "GTiff", 
            datatype = "INT4S",
            overwrite = TRUE)

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



drive_download(drive_get(id = "1oNxe-MITpIVF2BFczLGLXcr0jT-LIWVB"), path = file.path(".","data","local", "grts_level1.tif"), overwrite = TRUE)
drive_download(drive_get(id = "1oJpmNqlYoN3z8ICOlZZSlV0JXoUlcU5n"), path = file.path(".","data","local", "grts_level9.tif"), overwrite = TRUE)

test <- raster(file.path(".","data","local", "grts_level9.tif"))
test2 <- raster(file.path(".","data","local", "grts_level1.tif"))

tubes_hab <- read_vc(file.path(".","data","tubes_hab"))

tubes_xg3_basis <- read_vc(file.path(".","data","tubes_xg3"))

watina <- connect_watina()
tubes_xg3 <- tubes_hab %>% 
  get_xg3(watina, startyear = year(now()) - 18, endyear = 2016, vert_crs = "local",
          truncated =  TRUE, collect = TRUE)

test <- 
  
habmap_gw_raster_overlay <- habmap_polygons_gw %>% 
  st_intersection(raster_meetnet_poly)

habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>% 
  mutate(opp = as.integer(st_area(habmap_gw_raster_overlay))) 

st_write(habmap_gw_raster_overlay,
         file.path(datapath, 
                   "GIS/VoorR/habmap_gw_raster_overlay.gpkg"), 
         layer = "habmap_gw_raster_overlay", 
         driver = "GPKG",
         delete_dsn = TRUE)

dbWriteTable(con, "habmap_gw_raster_overlay", habmap_gw_raster_overlay)
dbDisconnect(con)

drive_download(drive_get(id = "1oY7fXj7Kd59w1LFHhu88E9cLLkC5cPJS"), path = file.path(".","data","local", "habmap_gw_raster_overlay.gpkg"), overwrite = TRUE)
test <- suppressWarnings(read_sf(file.path(".","data","local", "habmap_gw_raster_overlay.gpkg"), "habmap_gw_raster_overlay"))

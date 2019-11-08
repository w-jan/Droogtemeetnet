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

#keuzen voor het aanmaken van een document
#welke data verversen
# 2: alles; 1: enkel de huidige google-drive-bestanden downloaden ; 0: enkel met lokale data
refresh_data <- 1
if (refresh_data == 2) {
  datapath <- "G:/Mijn Drive/PRJ_Meetnet_Droogte/2_Uitvoering/data"
  dir.create(file.path(datapath, "GIS/VoorR/"), recursive = TRUE)
}

#figuren opnieuw aanmaken of inlezen
# 2: figuren terug aanmaken (en wegschrijven); 1: enkel de huidige google-drive-bestanden downloaden, 0: figuren inlezen
refresh_figures <- 2
if (refresh_figures == 2) {
  figpath <- "G:/Mijn Drive/PRJ_Meetnet_Droogte/2_Uitvoering/figuren"
}
# interactieve modus, maar vraagt veel computertijd en genereert ook een mega html-bestand
# tmap_mode("view")
# statische modus, dus niet inzoombaar
tmap_mode("plot")

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
if (refresh_data == 2) {
  gw_types <- read_scheme_types(lang = "nl") %>%
    filter(scheme == "GW_05.1_terr") %>%
    arrange(typegroup) %>%
    mutate(groupnr = as.integer(str_sub(typegroup, -1))) %>% 
    dplyr::select(type, groupnr, typegroup_name)
  output_vc <- write_vc(gw_types, file.path(".","data","gw_types"), sorting = c("type"), strict =  FALSE)
  
  types <- read_types(lang = "nl")
  output_vc <- write_vc(types, file.path(".","data","types"), sorting = c("type"), strict =  FALSE)
  
  
  rm(output_vc)
}

if (refresh_data == 2) {
  habmap_terr <- read_habitatmap_terr()
  
  habmap_polygons <- habmap_terr$habitatmap_terr_polygons
  
  habmap_types <- habmap_terr$habitatmap_terr_types
  
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
  dbDisconnect(con)
  
  
  #GRTS-data
  
  raster_meetnet_poly <- read_GRTSmh_diffres(level = 8, polygon = TRUE)
  raster_meetnet_poly <- raster_meetnet_poly %>% 
    rename(rasterid =  value)
  
  #wegschrijven van dit grid
  st_write(raster_meetnet_poly,
           file.path(datapath, 
                     "GIS/VoorR/raster_meetnet_poly.gpkg"), 
           layer = "raster_meetnet_poly", 
           driver = "GPKG",
           delete_dsn = TRUE)
  
  

  
  #overlay maken hab-kaart en GRTS-rooster
  habmap_gw_raster_overlay <- habmap_polygons_gw %>% 
    st_intersection(raster_meetnet_poly)
  
  habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>% 
    mutate(opp = as.integer(st_area(habmap_gw_raster_overlay))) 
  
  #wegschrijven van deze intersectie
  st_write(habmap_gw_raster_overlay,
           file.path(datapath, 
                     "GIS/VoorR/habmap_gw_raster_overlay.gpkg"), 
           layer = "habmap_gw_raster_overlay", 
           driver = "GPKG",
           delete_dsn = TRUE)
  
  
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
}

if (refresh_data == 2) {
  watina <- connect_watina()
  tubes_hab <- get_locs(watina, mask = habmap_gw_raster_overlay, join_mask = TRUE,
                        buffer = bufferpb, loc_type = "P", loc_validity = c("VLD", "ENT"), 
                        collect = TRUE)
  
  
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
    arrange(loc_code, polygon_id, rasterid, type)
  
  output_vc <- write_vc(tubes_hab, file.path(".","data","tubes_hab"), 
                        sorting = c("loc_code", "polygon_id", "rasterid", "type" ),
                        strict =  FALSE)
  rm(output_vc)
  
  
  tubes_xg3 <- tubes_hab %>% 
    get_xg3(watina, startyear = year(now()) - 18, endyear = 2016, vert_crs = "local",
            truncated =  TRUE, collect = TRUE)
  
  #oplossen van UTF-probleem: strings worden in inbo-SQL-databanken opgeslagen in UTF-16, terwijl hier gewerkt wordt met UTF-8. Dit geeft een probleem bij de kable-functie
  
  #tubes_xg3 <- mutate_if(tubes_xg3, is.character, iconv, to = "UTF-8")
  
  tubes_xg3 <- tubes_xg3 %>% 
    arrange(loc_code, hydroyear)
  
  output_vc <- write_vc(tubes_xg3, file.path(".","data","tubes_xg3"), 
                        sorting = c("loc_code", "hydroyear"),
                        strict =  FALSE, root = ".")
  
  
  DBI::dbDisconnect(watina)
  
  #volgende watina-functie werkt ook zonder databankconnectie, maar omdat deze op sommige pc's niet stabiel werkt, worden de data hier alvast lokaal weggeschreven.
debugonce("eval_xg3_series_lok")
  
filter_xg3_lok <- function(data,
                           xg3_type) {
  
  assertthat::assert_that(all(c("loc_code", "hydroyear") %in% colnames(data)) &
                any(grepl("g3", colnames(data))),
              msg = "data does not have the necessary 'loc_code', 'hydroyear' and XG3 columns.")
  
  assertthat::assert_that(("L" %in% xg3_type & any(grepl("lg3", colnames(data)))) |
                ("H" %in% xg3_type & any(grepl("hg3", colnames(data)))) |
                ("V" %in% xg3_type & any(grepl("vg3", colnames(data)))),
              msg = paste("xg3_type is set to",
                          dput(xg3_type),
                          "but at least one XG3 type is missing in data.")
  )
  
  xg3_filtered <-
    data %>%
    select(.data$loc_code,
           .data$hydroyear,
           if ("L" %in% xg3_type) contains("lg"),
           if ("H" %in% xg3_type) contains("hg"),
           if ("V" %in% xg3_type) contains("vg")
    ) %>%
    arrange(.data$loc_code,
            .data$hydroyear)
  
  return(xg3_filtered)
}

eval_xg3_series_lok <- function(data,
                                xg3_type = c("L", "H", "V"),
                                max_gap,
                                min_dur){
  if (missing(xg3_type)) {
    xg3_type <- match.arg(xg3_type)} else {
      assertthat::assert_that(all(xg3_type %in%
                        c("L", "H", "V")),
                  msg = "You specified at least one unknown xg3_type.")
    }
  series_memberyrs <-
    data %>%
    extract_xg3_series(xg3_type = xg3_type,
                       max_gap = max_gap,
                       min_dur = min_dur)
  
  # summarize series properties:
  tmpf <- tempfile()
  file.create(tmpf)
  capture.output({ # to suppress the many disc_ks_test() messages
    xg3_series_props <-
      series_memberyrs %>%
      group_by(.data$loc_code,
               .data$xg3_variable,
               .data$series) %>%
      summarise(
        ser_length = first(.data$series_length),
        ser_nryears = n(),
        ser_rel_nryears = .data$ser_nryears / .data$ser_length,
        ser_firstyear = min(.data$hydroyear),
        ser_lastyear = max(.data$hydroyear)  
      #   ser_pval_uniform = disc_ks_test(.data$hydroyear,
      #                                   ecdf(seq(.data$ser_firstyear,
      #                                            .data$ser_lastyear)),
      #                                   exact = TRUE) %>%
      #     .$p.value
      ) %>%
      ungroup
  },
  file = tmpf)
  
  # calculation of standard errors
  
  xg3_filtered <-
    data %>%
    filter_xg3_lok(xg3_type = xg3_type)
  
  if (inherits(xg3_filtered, "tbl_lazy")) {
    xg3_filtered <- collect(xg3_filtered)
  }
  
  xg3_series_values <-
    xg3_filtered %>%
    gather(key = "xg3_variable",
           value = "value",
           -.data$loc_code,
           -.data$hydroyear) %>%
    inner_join(
      series_memberyrs,
      by = c("loc_code", "hydroyear", "xg3_variable")
    ) %>%
    group_by(.data$loc_code,
             .data$xg3_variable,
             .data$series)
  
  xg3_series_se <-
    xg3_series_values %>%
    summarise(
      ser_mean = mean(.data$value),
      ser_sd = sd(.data$value),
      ser_se_6y = (var(.data$value) / 6 *
                     (1 - n() / first(.data$series_length))) %>%
        sqrt,
      ser_rel_sd_lcl = ifelse(str_detect(first(.data$xg3_variable), "_lcl"),
                              .data$ser_sd / abs(.data$ser_mean),
                              NA),
      ser_rel_se_6y_lcl = ifelse(str_detect(
        first(.data$xg3_variable), "_lcl"),
        .data$ser_se_6y / abs(.data$ser_mean),
        NA)
    ) %>%
    ungroup
  
  # put it all together...
  xg3_series_props <-
    xg3_series_props %>%
    left_join(xg3_series_se,
              by = c("loc_code",
                     "xg3_variable",
                     "series")
    )  
  return(xg3_series_props)
}  
  
  tubes_lg3_eval <- tubes_xg3 %>%
    eval_xg3_series_lok(xg3_type = c("L"),
                    max_gap = maxgap,
                    min_dur = minlength)
  
  tubes_lgl_eval <- tubes_lg3_eval %>%
    filter(ser_nryears >= minnryears)
  
  output_vc <- write_vc(tubes_lgl_eval, file.path(".","data","tubes_lgl_eval"), 
                        sorting = c("loc_code", "ser_firstyear"), 
                        strict =  FALSE, root = ".")
  
  rm(output_vc)
}
#einde inladen

gw_types <- read_vc("gw_types", file.path(".","data"))

gw_types_groupen <- gw_types %>%
  distinct(groupnr, typegroup_name) %>% 
  rename("GT-groep: nummer" = groupnr,
         "GT-groep: naam" = typegroup_name)


if (file.exists(file.path(".","data","local", "habmap_terr_gw.gpkg")) == FALSE | refresh_data >= 1) {
  drive_download(drive_get(id = "1nxnpfE3Eh4eCiM2VinGYMJE55qD4Az1c"), 
                 path = file.path(".","data","local", "habmap_terr_gw.gpkg"), overwrite = TRUE)
}

habmap_polygons_gw <- read_sf(file.path(".","data","local", "habmap_terr_gw.gpkg"),
                              "habitatmap_terr_polygons_gw")

habmap_polygons_gw <- habmap_polygons_gw %>%
  mutate(polygon_id = factor(.data$polygon_id),
         type = factor(.data$type),
         typegroup_name = factor(.data$typegroup_name)
  )
habmap_types_gw <- suppressWarnings(read_sf(file.path(".","data","local", "habmap_terr_gw.gpkg"),
                                              "habitatmap_terr_types_gw"))


#raster_meetnet_poly <- read_GRTSmh_diffres(level = 8, polygon = TRUE)


if (file.exists(file.path(".","data","local", "raster_meetnet_poly.gpkg")) == FALSE | refresh_data >= 1 ){
  drive_download(drive_get(id = "1oHdlUEEZmCDvXDCSXELgtgKNDgLn4E_0"), 
                 path = file.path(".","data","local", 
                                  "raster_meetnet_poly.gpkg"), overwrite = 
                   TRUE)
}

raster_meetnet_poly <- suppressWarnings(read_sf(file.path(".","data","local", "raster_meetnet_poly.gpkg"), "raster_meetnet_poly"))

#oppervlakte van elk hok berekenen
raster_meetnet_poly_opp <- raster_meetnet_poly %>% 
  mutate(opp = as.integer(st_area(raster_meetnet_poly))) %>% 
  st_drop_geometry() %>% 
  dplyr::select(rasterid, opp) %>% 
  group_by(rasterid) %>% 
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
check <- raster_meetnet_poly %>% st_drop_geometry() %>% count(rasterid) %>% filter(n > 1)
#van bepaalde cellen zijn er dus meerdere polygonen, dit zijn rasters die door de gewestgrens verdeeld werden (bijv. streek van Baarle-Nassau) 


# overlay maken van de habitatkaart (enkel van GT-groepen) en het GTRS-raster (level8)
# habmap_gw_raster_overlay <- habmap_polygons_gw %>% 
#   st_intersection(raster_meetnet_poly)
# 
# habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>% 
#     mutate(opp = as.integer(st_area(habmap_gw_raster_overlay))) 

if (file.exists(file.path(".","data","local", 
                          "habmap_gw_raster_overlay.gpkg")) == FALSE | refresh_data >= 1) {
  drive_download(drive_get(id = "1oY7fXj7Kd59w1LFHhu88E9cLLkC5cPJS"), 
                 path = file.path(".","data","local", 
                                  "habmap_gw_raster_overlay.gpkg"), 
                 overwrite = TRUE)
}

habmap_gw_raster_overlay <- suppressWarnings(read_sf(file.path(".","data","local", "habmap_gw_raster_overlay.gpkg"), "habmap_gw_raster_overlay"))

habmap_gw_raster_overlay <- lwgeom::st_make_valid(habmap_gw_raster_overlay)


if (refresh_figures == 2) {
  habmap_gw_raster_overlay_tm <- raster_meetnet_poly_tm + 
    tm_shape(habmap_gw_raster_overlay) + 
    tm_fill(col = "groupnr", style = "cat", palette = "BuGn", title = "Grondwatertype") + 
    tm_layout(title = "Verspreiding van de GT-groepen" )
  
  tmap_save(habmap_gw_raster_overlay_tm, 
            filename = file.path(figpath, "habmap_gw_raster_overlay.png"),
            dpi = 250
            )
  include_graphics(path = file.path(figpath, "habmap_gw_raster_overlay.png")) 

}
if (file.exists(file.path(".","figures","local", 
                          "habmap_gw_raster_overlay.png")) == FALSE | refresh_figures >= 1) {
  drive_download(drive_get(id = "1hLSIyTT-yk1wY0qtqhD0Qh6Ps_KnI211"), 
                 path = file.path(".","data","local", 
                                  "habmap_gw_raster_overlay.png"), 
                 overwrite = TRUE)
}

if (refresh_figures < 2) {
  include_graphics(path = file.path(".","data","local", 
                                    "habmap_gw_raster_overlay.png"))  
}

#habmap_gw_raster_overlay_tm

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
  select(-phab, -certain, -type, -source.y, -polygon_id, -starts_with("description"), -opp, -source.x) %>% 
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
tubes_lgl_eval <- read_vc("tubes_lgl_eval", file.path(getwd(),"data"))
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
 
 # kable(tubes_qual_basis_bis) %>% 
 #     kable_styling (bootstrap_options = c("striped", "hover", "condensed", "responsive"),
 #                 full_width = T,
 #                 position = "left",
 #                 font_size = 8,
 #                 fixed_thead = T
 #                 ) %>%
 #    # column_spec(1:3, bold = F, border_right = F, width = "35em") %>%
 #    # column_spec(c(3,5:7, 9:10), width = "1cm") %>%
 #    # row_spec(0, angle = -90, align = "c")  %>% #fixeer veldnamen
 #     scroll_box(height = "250px")
 
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
 
 # kable(sel_qual_basis_bis %>% 
 #         select(-gew_aantal_meetptn, -aantal_cat1A, -aantal_cat1B) %>% 
 #         rename(GTgroep = groupnr,
 #                rasternr = rasterid,
 #                'recentste jaar' = ser_lastyear,
 #                'lengte tijdreeks (jaar)' = ser_nryears,
 #                '# peilbuizen' =  n,
 #                '# gezochte meetptn' = rest_aantal_meetptn,
 #                'rang cluster spreiding' = rankclus_lastyear,
 #                'rang cluster lengte tijdreeks' = rankclus_nryears
 #                ) %>% 
 #         arrange(rasternr, GTgroep),
 #       caption =  "Indeling van de peilbuizen in clusters o.b.v. twee criteria"
 #       ) %>% 
 #     kable_styling (bootstrap_options = c("striped", "hover", "condensed", "responsive"),
 #                 full_width = T, 
 #                 position = "left",
 #                 font_size = 8, 
 #                 fixed_thead = T 
 #                 ) %>% 
 #    # column_spec(1:3, bold = F, border_right = F, width = "35em") %>%
 #    # column_spec(c(3,5:7, 9:10), width = "1cm") %>% 
 #    # row_spec(0, angle = -90, align = "c")  %>% #fixeer veldnamen
 #     scroll_box(height = "250px") 
 
 
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
 
 # kable(sel_qual_bis %>%
 #         select(1:3,7,5:6,4,8) %>%
 #         rename(GTgroep = groupnr,
 #                rasternr = rasterid,
 #                'nog gewenst # meetptn' = rest_aantal_meetptn,
 #                '# peilbuizen' =  beschikbaar_aantal_cluster,
 #                'rang combi' = rankclus,
 #                'rang cluster spreiding' = rankclus_lastyear,
 #                'rang cluster lengte tijdreeks' = rankclus_nryears,
 #                'max rang' = maxrank
 #                ) %>%
 #         arrange(rasternr, GTgroep),
 #       caption =  "Overzicht clustering o.b.v. de drie kwaliteitscriteria"
 #       ) %>%
 #     kable_styling (bootstrap_options = c("striped", "hover", "condensed", "responsive"),
 #                 full_width = T,
 #                 position = "left",
 #                 font_size = 8,
 #                 fixed_thead = T
 #                 ) %>%
 #    # column_spec(1:3, bold = F, border_right = F, width = "35em") %>%
 #    # column_spec(c(3,5:7, 9:10), width = "1cm") %>%
 #    # row_spec(0, angle = -90, align = "c")  %>% #fixeer veldnamen
 #     scroll_box(height = "250px")
 

   ### Categorie 2: Rastercellen met een juist voldoend aantal geschikte peilbuizen {#cat2}

 # rastercellen met een juist voldoende aantal evenwaardige meetpunten dat gewenst is voor het meetnet
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
 
 #kable(sel_cat2_table_bis ) %>% 
 #   select(1:3,7,5:6,4) %>%
 #   rename(GTgroep = groupnr,
 #          rasternr = rasterid,
 #          'nog gewenst # meetptn' = rest_aantal_meetptn,
 #          '# peilbuizen' =  beschikbaar_aantal_cluster,
 #          'rang combi' = rankclus,
 #          'rang cluster spreiding' = rankclus_lastyear,
 #          'rang cluster lengte tijdreeks' = rankclus_nryears
 #          ) %>%
 #   arrange(rasternr, GTgroep),
 # caption =  "Categorie 2: rastercellen met een juist voldoende aantal peilbuizen"
 # ) %>%
 # kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
 #             full_width = T,
 #             position = "left",
 #             font_size = 8,
 #             fixed_thead = T
 #             ) %>%
 # column_spec(1:3, bold = F, border_right = F, width = "35em") %>%
 # column_spec(c(3,5:7, 9:10), width = "1cm") %>%
 # row_spec(0, angle = -90, align = "c")  %>% #fixeer veldnamen
 #    scroll_box(height = "250px")

 sel_cat2_raster_bis <- sel_raster_pb_bis %>% 
   semi_join(sel_cat2_table_bis,
             by = c("rasterid", "groupnr")) %>% 
   arrange(rasterid, groupnr)
 

 sel_cat2_tm_bis <- raster_meetnet_poly_tm + 
   tm_shape(sel_cat2_raster_bis) + 
   tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.2: rastercellen met een juist voldoende aantal peilbuizen" )
 
 sel_cat2_tm_bis

 
 # de bijhorende geselecteerde Watina-meetpunten zijn dan
 tubes_cat2_bis <- 
   tubes_qual_basis_bis %>% 
   filter(selectie >= 0) %>% 
   inner_join(sel_qual_basis_bis) %>% #toevoeging is nodig om onderscheid te maken/behouden tussen de clusters
   inner_join(sel_cat2_table_bis)
 

 
 output_vc <- write_vc(tubes_cat2_bis, file.path(".","data","tubes_cat2_bis"), 
                       sorting = c("loc_code"), strict =  FALSE, root = ".")
 
 rm(output_vc)

 
 ### Categorie 3: Rastercellen met een overschot aan geschikte peilbuizen {#cat3}

 
 # rastercellen met een overschot aan evenwaardige meetpunten in vergelijking met het aantal dat gewenst is voor het meetnet
 sel_cat3_raster_bis <- 
   sel_raster_meetnet %>% 
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
 
 
 # kable(sel_cat3_table_bis %>% 
 #         select(1:3,7,5:6,4) %>% 
 #         rename(GTgroep = groupnr,
 #                rasternr = rasterid,
 #                'nog gewenst # meetptn' = rest_aantal_meetptn,
 #                '# peilbuizen' =  beschikbaar_aantal_cluster,
 #                'rang combi' = rankclus,
 #                'rang cluster spreiding' = rankclus_lastyear,
 #                'rang cluster lengte tijdreeks' = rankclus_nryears
 #                ) %>% 
 #         arrange(rasternr, GTgroep),
 #       caption =  "Categorie 3: rastercellen met een overaanbod van peilbuizen"
 #       ) %>% 
 #     kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
 #                 full_width = T, 
 #                 position = "left",
 #                 font_size = 8, 
 #                 fixed_thead = T 
 #                 ) %>% 
 #    # column_spec(1:3, bold = F, border_right = F, width = "35em") %>%
 #    # column_spec(c(3,5:7, 9:10), width = "1cm") %>% 
 #    # row_spec(0, angle = -90, align = "c")  %>% #fixeer veldnamen
 #     scroll_box(height = "250px") 

 
 sel_cat3_tm_bis <- raster_meetnet_poly_tm + 
   tm_shape(sel_cat3_raster_bis) + 
   tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.3: rastercellen met een overschot aan geschikte peilbuizen" )
 
 sel_cat3_tm_bis

 #### Opzoeken van peilbuizen voor de rastercellen van cat. 3
 

 #bijhorende Watina-meetpunten ervan opzoeken
 tubes_cat3_bis <- 
   tubes_qual_basis_bis %>% 
   filter(selectie >= 0) %>% 
   inner_join(sel_qual_basis_bis) %>% 
   inner_join(sel_cat3_table_bis)
 
 tubes_cat3_bis <- tubes_cat3_bis %>% 
   rownames_to_column("unieknr") %>% 
   mutate(unieknr = as.integer(unieknr))
 # sel_qual_lastyear_vb %>% 
 #   count(rasterid, groupnr )
 
 

 output_vc <- write_vc(tubes_cat3_bis, file.path(".","data","tubes_cat3_bis"), 
                       sorting = c("loc_code"), strict =  FALSE, root = ".")
 
 rm(output_vc)
 

 ## Moeten nog bijkomende meetreeksen geanalyseerd worden?
 

 if (nrow(tubes_cat3_bis %>% filter(selectie == 0)) > 0){
   message("Er zijn nog enkele meetreeksen die best met Menyanthes onderzocht worden.")
   nognietklaar <- TRUE
 } else{
   message("Er hoeven geen bijkomende meetreeksen met Menyanthes onderzocht te worden.")
   nognietklaar <- FALSE  
 }
 
 if (nognietklaar == TRUE) {
   kable(tubes_cat3_bis %>% 
           filter (selectie == 0) %>% 
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
           ) ,
         caption =  "Watina-meetpunten die behoren tot rastercellen van cat. 3"
   ) %>% 
     kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                   full_width = T, 
                   position = "left",
                   font_size = 8, 
                   fixed_thead = T) %>%
     # column_spec(1:3, bold = F, border_right = F, width = "35em") %>%
     # column_spec(2, width = "30em", background = "yellow")
     #¬row_spec(0, angle = -90)  %>% #fixeer veldnamen
     scroll_box(height = "250px", box_css = "border: 1px solid #ddd; padding: 5px; margin: 5px;") 
 }
 

 ## Aanduiden van de meetlocaties

 
 ### Het toewijzen van een kandidaat peilbuis aan een meetlocatie
 

 #inlezen grts-raster level 1 (hoogste resolutie = kleinste gridcelgrootte)
 # grts_level1 <- read_GRTSmh(brick = TRUE) %>% 
 #   raster::subset(1)
 
 if (file.exists(file.path(".","data","local", "grts_level1.tif")) == FALSE | refresh_data >= 1) {
   drive_download(drive_get(id = "1oNxe-MITpIVF2BFczLGLXcr0jT-LIWVB"), 
                  path = file.path(".","data","local", "grts_level1.tif"), 
                  overwrite = TRUE)
 }
 grts_level1 <- raster(file.path(".","data","local", "grts_level1.tif"))
 
 
 #inlezen grts-raster level 9 (resolutie = raster_meetnet_poly), het heeft een gridgrootte van 8192 m, let wel de rastercelgrootte is ook hier 32 bij 32m, dus het aantal rastercellen = grts-raster level 1. 
 # grts_level9 <- read_GRTSmh(brick = TRUE) %>% 
 #   raster::subset(9)
 
 if (file.exists(file.path(".","data","local", "grts_level9.tif")) == FALSE | refresh_data >= 1) {
   drive_download(drive_get(id = "1oJpmNqlYoN3z8ICOlZZSlV0JXoUlcU5n"), 
                  path = file.path(".","data","local", "grts_level9.tif"), 
                  overwrite = TRUE)
 }
 
 grts_level9 <- raster(file.path(".","data","local", "grts_level9.tif"))
 

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

 sel_excess <- tubes_cat1Bb %>% 
   select(rasterid, groupnr) %>% 
   distinct() %>% 
   bind_rows(sel_cat3_table_bis %>% 
               select(rasterid, groupnr)) %>%
   arrange(rasterid, groupnr) %>% 
   distinct()

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
     
     # raster met grts-nrs herindexeren. Een 1-waarde stemt overeen met een gewenste peilbuis (voor dat grid), de overige nummer(s) zijn de reservepunt(en).
     # Hiervoor moet er eerst een n*2 matrix (rcl) gemaakt worden met de oude en nieuwe celwaarde.
     rcl <- data.frame("grtsnr" = rank_cells_level1 %>% pull(celwaarde), 
                       "selectie" = 
                         c(rep(1,gewenst_aantal_pb), seq(from = gewenst_aantal_pb + 1, to = nrow(rank_cells_level1)))) %>% 
       as.matrix()
     
     #herindexeren
     tubes_excess_level1_rcl <- raster::reclassify(tubes_excess_level1, rcl)
     
     #raster maken met de unieke nummers van de pb, maar dat enkel voor het gewenste aantal pb
     tubes_excess_level1_unieknr <- 
       raster::rasterize(tubes_excess %>% 
                           filter(groupnr == gwgroup) %>% 
                           select(x, y) %>% 
                           as.matrix(),
                         tubes_excess_level1_rcl[tubes_excess_level1_rcl == 1, drop = FALSE],
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
     
     #idem maar nu voor de reservepunten. Dit kan ik niet in één keer, gelukkig duurt het niet lang.
     #raster maken met de unieke nummers van de pb, maar dat enkel voor de reservepunten
     for (k in seq(from = (gewenst_aantal_pb + 1), to = nrow(rank_cells_level1))) {
       tubes_excess_level1_unieknr <- 
         raster::rasterize(tubes_excess %>% 
                             filter(groupnr == gwgroup) %>% 
                             select(x, y) %>% 
                             as.matrix(),
                           tubes_excess_level1_rcl[tubes_excess_level1_rcl == k, drop = FALSE],
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
     } #loop reservepunten
   } #loop gw-groepen
 } # loop gridcellen
 
 #aanduiden welke pb geselecteerd zijn
 tubes_cat3_gtrs <- tubes_excess %>% 
   filter(geselecteerd == 1 & cat == 3) %>% 
   arrange(rasterid, groupnr, loc_code)
 
 tubes_cat1Bb_gtrs <- tubes_excess %>% 
   filter(geselecteerd == 1 & cat == 1) %>% 
   arrange(rasterid, groupnr, loc_code)
 
 tubes_cat3_gtrs_reserve <- tubes_excess %>% 
   filter(reserve > 0 & cat == 3) %>% 
   arrange(rasterid, groupnr, reserve)
 
 tubes_cat1Bb_gtrs_reserve <- tubes_excess %>% 
   filter(reserve > 0 & cat == 1) %>% 
   arrange(rasterid, groupnr, reserve)

 #wegschrijven van het resultaat, omdat de berekening hiervan toch wel enkele minuten tijd vraagt.
 output_vc <- write_vc(tubes_cat3_gtrs, file.path(".","data","tubes_cat3_gtrs"), sorting = c("rasterid","groupnr", "loc_code"), strict =  FALSE)
 output_vc <- write_vc(tubes_cat1Bb_gtrs, file.path(".","data","tubes_cat1Bb_gtrs"), sorting = c("rasterid","groupnr", "loc_code"), strict =  FALSE)
 output_vc <- write_vc(tubes_cat3_gtrs_reserve, file.path(".","data","tubes_cat3_gtrs_reserve"), sorting = c("rasterid","groupnr", "reserve"), strict =  FALSE)
 output_vc <- write_vc(tubes_cat1Bb_gtrs_reserve, file.path(".","data","tubes_cat1Bb_gtrs_reserve"), sorting = c("rasterid","groupnr", "reserve"), strict =  FALSE)
 rm(output_vc)
 

 ###Samenvatting selectie

 
 
 ###Selecteren van potentiële geschikte habitatvlekken voor gridcellen waarvoor nu geen pb'en bestaan.

 
 #verrasteren van de habitatkaart met de grondwatergroep als waarde
 grts_level6 <- read_GRTSmh(datapath, brick = TRUE) %>% 
   raster::subset(6)
 
 reserve <- habmap_polygons_gw
 
 # habmap_polygons_gw <- habmap_polygons_gw %>% 
 #   rownames_to_column(var = "unieknr") %>% 
 #   mutate(unieknr = as.integer(unieknr),
 #          selecteerbaar = 1) 
 habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>%
   rownames_to_column(var = "unieknr") %>%
   mutate(unieknr = as.integer(unieknr))
 
 habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>% 
   mutate(selecteerbaar = 1, 
          selecteerbaar_reserve = 1) 
 
 sel_raster_group1_gw_df <- sel_raster_group1 %>% 
   st_drop_geometry() %>% 
   select(-opp_gw_cel) %>% 
   arrange(rasterid, groupnr) %>% 
   distinct()
 
 sel_raster_group1_df <- sel_raster_group1_gw_df %>% 
   select(-groupnr) %>% 
   distinct
 
 for (i in seq(1:nrow(sel_raster_group1_df))) {
   
   rasterid_grid <- sel_raster_group1_df[i,1] %>% as.integer()
   
   clip9 <- grts_level9[grts_level9 == rasterid_grid, drop =  FALSE]
   clip1 <- grts_level1[clip9, drop =  FALSE]
   clip6 <- grts_level6[clip9, drop =  FALSE]
   
   for (j in seq(1:nrow(sel_raster_group1_df %>% filter(rasterid == rasterid_grid)))) {
     
     sel_raster_gw_grid <- sel_raster_group1_gw_df %>% 
       filter(rasterid == rasterid_grid) %>% 
       slice(j) 
     
     gewenst_aantal_pb <- sel_raster_gw_grid %>% 
       pull(gew_aantal_meetptn) %>% 
       as.integer()
     
     gwgroup <- sel_raster_gw_grid %>% 
       pull(groupnr) %>% 
       as.integer()
     
     # gewenst_aantal_pb <- 2
     
     
     # raster met binnen het grid (level8) de rangnummers (GRTS) van de habitatpolygonen die behoren tot een bep.
     # grondwatergroep
     habmap_raster_rangnr <- raster::rasterize(habmap_gw_raster_overlay %>% 
                                                 filter(groupnr == gwgroup & selecteerbaar == 1),
                                               clip1, 
                                               mask = TRUE)
     # plot(habmap_raster_gwgroup)
     # plot(habmap_raster_rangnr)
     
     # habmap_raster_rangnr[habmap_raster_rangnr == 170982, drop = FALSE]
     
     #opzoeken van de laagste rangnummer(s) grid level 1(hoogste resolutie)
     grid_rangnr <- habmap_raster_rangnr %>% 
       raster::getValues() %>% 
       as.data.frame()
     names(grid_rangnr) <- "celwaarde" 
     grid_rangnr <- grid_rangnr %>% 
       filter(!is.na(celwaarde)) %>%  
       distinct() %>% 
       arrange(celwaarde) 
     # grid_rangnr_min <- grid_rangnr %>% slice(1:gewenst_aantal_pb)  
     
     
     # raster met grts-nrs herindexeren naar 0 en 1 waarden. Het aantal 1 waarden stemt overeen met het gewenst aantal peilbuizen (voor dat grid).
     # Hiervoor moet er eerst een n*2 matrix (rcl) gemaakt worden met de oude en nieuwe celwaarde.
     rcl <- data.frame("grtsnr" = grid_rangnr %>% pull(celwaarde), 
                       "selectie" = 
                         c(rep(1,gewenst_aantal_pb), rep(0,nrow(grid_rangnr) - gewenst_aantal_pb))) %>% 
       as.matrix()
     
     #herindexeren
     habmap_raster_rcl <- raster::reclassify(habmap_raster_rangnr, rcl)
     # habmap_raster_rcl[habmap_raster_rangnr[habmap_raster_rangnr == 46054, drop = FALSE], drop = FALSE]
     # plot(habmap_raster_rcl)
     
     # raster met binnen het grid (level8) alle unieke nrs van de habitatpolygonen die behoren tot een bep. grondwatergroep
     habmap_raster_unieknr <- raster::rasterize(habmap_gw_raster_overlay %>%
                                                  filter(groupnr == gwgroup & selecteerbaar == 1),
                                                clip1,
                                                field = "unieknr",
                                                mask = FALSE)
     # habmap_raster_unieknr[habmap_raster_rangnr[habmap_raster_rangnr == 46054, drop = FALSE], drop= FALSE]
     
     #raster maken met de unieke nummers van de pb, maar dat enkel voor het gewenste aantal
     #volgende verrastering geeft een verkeerd resultaat, van het bestaande raster-object (y) worden m.i. enkel de extent, de resolutie en de crs overgenomen. Het al dan niet NA zijn van een cel wordt niet meegenomen
     #werkt niet
     # habmap_raster_unieknr_select1 <- raster::rasterize(habmap_gw_raster_overlay %>%
     #                                                    filter(groupnr == gwgroup & selecteerbaar == 1),
     #                                           habmap_raster_rcl[habmap_raster_rcl == 0, drop = FALSE],
     #                                           field = "unieknr",
     #                                           mask = FALSE)
     
     habmap_raster_unieknr_select <- habmap_raster_unieknr * habmap_raster_rcl[habmap_raster_rcl == 1, drop = FALSE]
     
     # plot(habmap_raster_unieknr_select)
     # plot(habmap_raster_unieknr_select1)
     #     plot(habmap_raster_rcl, add = TRUE)
     # all.equal(habmap_raster_unieknr_select,habmap_raster_unieknr_select1)
     # 
     # plot(habmap_gw_raster_overlay %>%  filter (unieknr %in% c(40705, 40915)) %>% select(unieknr))
     
     habmap_raster_unieknr_select_df <- habmap_raster_unieknr_select %>% 
       raster::getValues() %>% 
       as.data.frame()
     names(habmap_raster_unieknr_select_df) <- "unieknr"    
     habmap_raster_unieknr_select_df <- habmap_raster_unieknr_select_df %>% 
       filter(!is.na(unieknr)) %>%  
       distinct() %>% 
       arrange(unieknr) %>% 
       mutate(geselecteerd = 1)
     
     #vind de habmap-polygoon voor die rastercel
     # habmap_polygons_gw_part <- 
     #   habmap_polygons_gw %>% 
     #     st_drop_geometry() %>% 
     #     filter(unieknr == habmap_raster_unieknr[clip1_min]) %>% 
     #     mutate(geselecteerd = 1) %>% 
     #     select(unieknr, geselecteerd)
     #markeren en ook zo vermijden dat een polygoon twee keer wordt geselecteerd
     
     # habmap_polygons_gw <- 
     #   habmap_polygons_gw %>% 
     #     left_join(habmap_raster_unieknr_select_df, 
     #               by = "unieknr") %>% 
     #     mutate(selecteerbaar = ifelse(is.na(geselecteerd), selecteerbaar, 0)) %>% 
     #     select(-geselecteerd)
     
     habmap_gw_raster_overlay <- 
       habmap_gw_raster_overlay %>% 
       left_join(habmap_raster_unieknr_select_df, 
                 by = "unieknr") %>% 
       mutate(selecteerbaar = ifelse(is.na(geselecteerd), selecteerbaar, 0)) %>% 
       select(-geselecteerd)
     
     
     #opzoeken reservepunten
     #ophalen van bijhorende rangnr's van level5
     clip6_select <- clip6 * habmap_raster_rcl[habmap_raster_rcl == 1, drop = FALSE]
     # clip6_select
     # plot(clip6_select)
     # test <- raster::ratify(clip6_select)
     # rat <- levels(test)[[1]]
     
     #selectie van de geselecteerde rangnr(s)
     clip6_select_df <- clip6_select %>% 
       raster::getValues() %>% 
       as.data.frame()
     names(clip6_select_df) <- "celwaarde"  
     clip6_select_df <- 
       clip6_select_df %>% 
       filter(!is.na(celwaarde)) %>% 
       distinct %>% 
       arrange(celwaarde) %>% 
       mutate(selectie =  1)
     
     #opstellen van een clip-raster op basis van de geselecteerde rangnr(s)
     #dit door een reclassering te doen alle cellen van grid6 worden 0, behalve die met de geselecteerde celwaarde
     #alle celwaarden opzoeken binnen grid9
     clip6_range <- clip6 %>% 
       raster::getValues() %>% 
       as.data.frame()
     names(clip6_range) <- "celwaarde"  
     clip6_range <- clip6_range %>% distinct %>% arrange(celwaarde)
     
     #opbouw van reclas
     clip6_rcl <- clip6_range %>% 
       left_join(clip6_select_df) %>% 
       arrange(desc(selectie), celwaarde)
     
     rcl <- data.frame("grtsnr" = clip6_rcl %>% pull(celwaarde), 
                       "selectie" = 
                         c(rep(1,gewenst_aantal_pb), rep(0,nrow(clip6_rcl) - gewenst_aantal_pb))) %>% 
       as.matrix()
     
     habmap_raster_reserve_rcl <- raster::reclassify(clip6, rcl)    
     # plot(habmap_raster_reserve_rcl)
     habmap_raster_unieknr_reserve <- habmap_raster_unieknr * habmap_raster_reserve_rcl
     # plot(habmap_raster_unieknr_reserve)    
     
     #ophalen van unieke nummers
     #selectie van de geselecteerde rangnr(s)
     habmap_raster_unieknr_reserve_df <- habmap_raster_unieknr_reserve %>% 
       raster::getValues()%>% 
       as.data.frame()
     names(habmap_raster_unieknr_reserve_df) <- "unieknr"  
     habmap_raster_unieknr_reserve_df <- 
       habmap_raster_unieknr_reserve_df %>% 
       filter(!is.na(unieknr) & unieknr > 0) %>% 
       distinct %>% 
       arrange(unieknr) %>% 
       mutate(geselecteerd =  1)
     
     
     #markeren en ook zo vermijden dat een polygoon twee keer wordt geselecteerd
     habmap_gw_raster_overlay <- 
       habmap_gw_raster_overlay %>% 
       left_join(habmap_raster_unieknr_reserve_df, 
                 by = "unieknr") %>% 
       mutate(selecteerbaar_reserve = ifelse(is.na(geselecteerd), selecteerbaar_reserve, 0)) %>% 
       select(-geselecteerd)
   } #loop grondwatergroup
 } #loop grid
 # reserve1 <- reserve %>%
 #   inner_join(habmap_gw_raster_overlay %>% 
 #                st_drop_geometry() %>% 
 #                select(polygon_id, groupnr, rasterid), by = c("polygon_id", "groupnr"))
 tubes_pot_group1 <-
   habmap_gw_raster_overlay %>% 
   rename(geselecteerd_basis = selecteerbaar,
          geselecteerd_reserve = selecteerbaar_reserve) %>% 
   mutate(geselecteerd_basis = ifelse(geselecteerd_basis == 0,1,0),
          geselecteerd_reserve = ifelse(geselecteerd_reserve == 0,1,0)
   ) %>% 
   filter(geselecteerd_basis == 1 | geselecteerd_reserve == 1) %>% 
   arrange(rasterid, type, polygon_id) 
 
 habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>% 
   select(-starts_with("selecteerbaar"))
 
 #wegschrijven van het resultaat, omdat de berekening hiervan toch wel enkele minuten tijd vraagt.
 write_vc(tubes_pot_group1 %>% st_drop_geometry(), file.path(".","data","tubes_pot_group1"), sorting = c("rasterid","type", "polygon_id"), strict =  FALSE)
 # test <- read_vc(file.path(".","data","tubes_group4"))
 
 # raster::plot(clip6)
 # raster::plot(clip6_1cel, add= FALSE)
 # 
 # check <- tubes_excess_gw_raster_overlay %>% 
 #   select(loc_code, rasterid, rasterid.1)
 # check <- read_GRTSmh(datapath, brick = TRUE)
 # test <- raster::subset(check, 1)
 # test
 # 
 # test2
 # plot(test2)
 # elev <- spData::elev
 # clip <-  raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
 #               res = 0.3, vals = rep(1, 9))
 # t <- elev[clip, drop =  FALSE]
 # 
 # clipj <- test2[test2==1014, drop =  FALSE]
 # tubes_excess_raster0 <- rasterize(tubes_excess %>% select(x, y) %>% as.matrix(), test, mask = TRUE)
 # tubes_excess_unieknr <- rasterize(tubes_excess %>% select(x, y) %>% as.matrix(), test, field = tubes_excess %>% select(unieknr), mask = FALSE)
 # 
 # plot(tubes_excess_unieknr)
 # 
 # test <- tubes_excess_raster0 %>% 
 #   interse
 # 
 # tubes_excess_pointsO <- rasterToPoints(tubes_excess_raster0)
 # 
 # e1 <- extent(-10, 10, -20, 20)
 # e2 <- extent(0, 20, -40, 5)
 # intersect(e1, e2)
 # 
 # t <- tubes_excess %>% select(rasterid) %>% as.character()
 # plot(test_clip)
 # r <- raster(ncols=36, nrows=18)
 # n <- 1000
 # set.seed(123)
 # x <- runif(n) * 360 - 180
 # y <- runif(n) * 180 - 90
 # xy <- cbind(x, y)
 # # get the (last) indices
 # r0 <- rasterize(xy, r)
 # plot(r2)
 # # presence/absensce (NA) (is there a point or not?)
 # r1 <- rasterize(xy, r, field=1)
 # # how many points?
 # r2 <- rasterize(xy, r, field = xy[,2])
 # vals <- runif(n)
 # # sum of the values associated with the points
 # r3 <- rasterize(xy, r, vals, fun=sum)
 # 
 # 
 # 
 # 
 # clipjb <- test2[rasterize(tubes_excess %>% select(x, y) %>% as.matrix(), test2, field = tubes_excess %>% pull(loc_code))]
 # test3 <- test[clipj, drop = FALSE]
 # test3b <- intersect(test, clipj)
 # compareRaster(test3, test3b)
 #  
 # 
 # test4 <- rasterToPolygons(test3)
 # 
 # tubes_excess_sf <- tubes_excess %>% 
 #   st_as_sf(coords = c("x", "y"), crs = 31370)
 # 
 # test5 <- tubes_excess_sf %>% 
 #   st_intersection(st_as_sf(test4, crs = 31370))
 # 
 # plot(test3)
 # plot(clipj)
 # # plot(test4)
 # test3 <- st_as_sf(test2) %>% 
 #   st_join(st_as_sf(test))
 # 
 # integer <- 1017
 #   m <- c(0, integer - 1, NA, integer,  integer , 1, integer +1 , 2000, NA)
 #   rclmat <- matrix(m, ncol = 3, byrow = TRUE)
 #   rc <- reclassify(test2, rclmat) 
 #   plot(rc)
 ```
 
 

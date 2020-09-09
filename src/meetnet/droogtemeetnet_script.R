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
# 2: alle data worden herberekend, er worden hierbij bestanden gemaakt die in een Google Drive-map worden bewaard ; 1: een herberekening met de data op Google-Drive ; 0: een herberekening met lokale data (dit veronderstelt een eerdere run obv deze parameter met waarde 1 of 2)
params <- data.frame(refresh_data = 0, refresh_figures = 0)
params$refresh_data <- 0
if (params$refresh_data == 2) {
  datapath <- "G:/Mijn Drive/PRJ_Meetnet_Droogte/2_Uitvoering/data"
  #dir.create(file.path(datapath, "GIS/VoorR/"), recursive = TRUE)
}

#figuren opnieuw aanmaken of inlezen
# 2: figuren terug aanmaken (en wegschrijven); 1:  de figuren worden gedownload van Google-Drive ; 0: gebruik van figuren uit een lokale map (dit veronderstelt een eerdere run obv deze parameter met waarde 1 of 2)
params$refresh_figures <- 0
if (params$refresh_figures == 2) {
  figpath <- "G:/Mijn Drive/PRJ_Meetnet_Droogte/2_Uitvoering/figuren"
}
# interactieve modus, maar vraagt veel computertijd en genereert ook een mega html-bestand
# tmap_mode("view")
# statische modus, dus niet inzoombaar
tmap_mode("plot")
outermargins_tm <- c(0,0,0,0)

#gewenste grootte van het meetnet
tot_n_tub <- 100

#aantal stratificatielagen
aantal_strat <- 5

#verdeling meetpunten over de stratificatielagen
minaantal_tub_group <- as.integer(tot_n_tub/aantal_strat)
minaantal_tub_group <- c(0,34,33,22,11)
minaantal_tub_group <- c(11,22,33,34,0)

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

#peilbuizen die zeker niet kunnen opgenomen worden, bijv. geen toelating, omdat het te dicht bij een ander geselecteerd punt ligt
#uitgesloten_tubes <- c("MOSP001", "HALP005", "BUIP027")
uitgesloten_tubes <- c("HAGP018", "WALP101") #pb van een peilbuiskoppel

# upgrade_data(path = ".")

#inladen gegevens (niet verplicht, enkel nodig bij updates van de brondata)
if (params$refresh_data >= 1) {
  gw_types <- read_scheme_types(lang = "nl") %>%
    filter(scheme == "GW_05.1_terr") %>%
    arrange(typegroup) %>%
    mutate(groupnr = as.integer(str_sub(typegroup, -1))) %>% 
    dplyr::select(type, groupnr, typegroup_name)
  output_vc <- write_vc(gw_types, file.path(".","data","processed", "meetnet","gw_types"), sorting = c("type"), strict =  FALSE)
  
  types <- read_types(lang = "nl")
  output_vc <- write_vc(types, file.path(".","data","processed", "meetnet","types"), sorting = c("type"), strict =  FALSE)
  
  
  rm(output_vc)
}

#habitatmap_terr_inclbos.gpkg importeren en opslaan in een (lokale) subdirectory van n2khab. Indien het bestand daar niet gevonden wordt, wordt het gedownload van GD. Bij refresh_data-optie=2 wordt het sowieso gedownload (bijv. bij een nieuwere versie)
if (file.exists(normalizePath(file.path(n2khab::fileman_up("n2khab_data"), "20_processed", "habitatmap_terr", "habitatmap_terr_inclbos.gpkg"))) == FALSE | params$refresh_data ==3 ) {
  drive_download(drive_get(id = "13mon4WXdWVIAOjGLBts_S2Jl4lqzrCG5"), 
                 path = normalizePath(file.path(n2khab::fileman_up("n2khab_data"), "20_processed", "habitatmap_terr", "habitatmap_terr_inclbos.gpkg")), overwrite = 
                   TRUE)
}

if (params$refresh_data >= 1) {
  habmap_terr <- read_habitatmap_terr(file = "20_processed/habitatmap_terr/habitatmap_terr_inclbos.gpkg")
  
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
                 dplyr::select(-code_orig, -source), 
               by = "polygon_id")
  
  habmap_polygons_gw <- sf::st_make_valid(habmap_polygons_gw)
  #wegschrijven als geopackage
  
  
  st_write(habmap_polygons_gw,
           dsn = file.path(".","data","local", "habmap_terr_gw.gpkg"),
           layer = "habitatmap_terr_polygons_gw", 
           driver = "GPKG",
           delete_dsn = TRUE)
  
  con <- dbConnect(SQLite(),
                  dbname = file.path(".","data","local", "habmap_terr_gw.gpkg")
  )
  
  dbWriteTable(con, "habitatmap_terr_types_gw", habmap_types_gw)
  dbDisconnect(con)
  
  
  #GRTS-data
  
  raster_meetnet_poly <- read_GRTSmh_diffres(level = 8, polygon = TRUE)
  raster_meetnet_poly <- raster_meetnet_poly %>% 
    rename(rasterid =  value)
  raster_meetnet_poly <- sf::st_make_valid(raster_meetnet_poly) 
  
  #wegschrijven van dit grid
  st_write(raster_meetnet_poly,
           dsn = file.path(".","data","local", "raster_meetnet_poly.gpkg"),
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
           dsn = file.path(".","data","local", "habmap_gw_raster_overlay.gpkg"),
           layer = "habmap_gw_raster_overlay", 
           driver = "GPKG",
           delete_dsn = TRUE)
  
  
  #inlezen grts-raster level 0 (hoogste resolutie = kleinste gridcelgrootte)
  grts_level0 <- read_GRTSmh(brick = TRUE) %>% 
    raster::subset(1)
  
  #inlezen grts-raster level 8 (resolutie = raster_meetnet_poly), het heeft een gridgrootte van 8192 m, let wel de rastercelgrootte is ook hier 32 bij 32m, dus het aantal rastercellen = grts-raster level 0. 
  grts_level8 <- read_GRTSmh(brick = TRUE) %>% 
    raster::subset(9)
  
  #±inlezen grts-raster level 5 : voor het selecteren van habitatvlekken
  grts_level5 <- read_GRTSmh(brick = TRUE) %>% 
    raster::subset(6)
  
  writeRaster(grts_level8,
              filename = file.path(".","data","local", "grts_level8.tif"),
             # filename = file.path(datapath, "GIS/VoorR/grts_level8.tif"),
              format = "GTiff", 
              datatype = "INT4S",
              overwrite = TRUE)
  
  writeRaster(grts_level0,
              filename = file.path(".","data","local", "grts_level0.tif"),
              format = "GTiff", 
              datatype = "INT4S",
              overwrite = TRUE)
  
  writeRaster(grts_level5,
              filename = file.path(".","data","local", "grts_level5.tif"),
              format = "GTiff", 
              datatype = "INT4S",
              overwrite = TRUE)  
}

if (params$refresh_data >= 1) {
  watina <- connect_watina()
  #debugonce("get_locs")
  tubes_hab <- get_locs(watina, mask = habmap_gw_raster_overlay, join_mask = TRUE,
                        buffer = bufferpb, loc_type = "P", loc_validity = c("VLD", "ENT"), 
                        collect = TRUE) %>% as_tibble()
              
  tubes_hab_basis <-   tubes_hab
#toevoegen van pb gebruikt voor grondwaterstandstandsindicator die gelegen zijn in een gw-afhankelijk type
  vmm_pb <- read_csv(file.path(getwd(), "data","interim", "meetnet", "vmm_indc.csv")) %>% 
    rename(loc_code = FID_stand,
            polygon_id = plygn_d,
            typegroup_name = typgrp_) %>% 
    mutate(area_code = "VMM",
            area_name = "Grondwaterstandindicator") %>%
    inner_join(habmap_gw_raster_overlay %>% st_drop_geometry() %>%  
                dplyr::select(polygon_id, type, groupnr, rasterid, opp), by = c("polygon_id", "groupnr", "type")) %>% as_tibble()
  
  tubes_hab <- tubes_hab %>% 
    full_join(vmm_pb %>% dplyr::select(loc_code, area_code, area_name, x, y, phab, type, groupnr, typegroup_name, rasterid, opp, polygon_id))
  
  tubes_hab <- tubes_hab %>% 
    mutate(polygon_id = as.character(polygon_id),
           loc_validitycode = factor(loc_validitycode),
           loc_validity = factor(loc_validity),
           loc_typecode = factor(loc_typecode),
           loc_typename = factor(loc_typename),
           obswell_statecode = factor(obswell_statecode),
           obswell_state = factor(obswell_state),
           typegroup_name = factor(typegroup_name)
           )
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
  
  output_vc <- write_vc(tubes_hab, file.path(".","data","processed", "meetnet","tubes_hab"), 
                        sorting = c("loc_code", "polygon_id", "rasterid", "type" ),
                        strict =  FALSE)
  rm(output_vc)
  
  library(assertthat)
  #remotes::install_github("inbo/inbodb", build_vignettes = TRUE)
  #library(inbodb)
  #workaround voor slecht functionerende odbc-driver (= alle meetpunten importeren)
  #opgelost in watina versie 0.3.0
  # get_xg3_temp <- function(locs,
  #                          con,
  #                          startyear,
  #                          endyear = year(now()) - 1,
  #                          vert_crs = c("local",
  #                                       "ostend",
  #                                       "both"),
  #                          truncated = TRUE,
  #                          with_estimated = TRUE,
  #                          collect = FALSE) {
  #   
  #   vert_crs <- match.arg(vert_crs)
  #   assert_that(is.number(startyear))
  #   assert_that(is.number(endyear))
  #   assert_that(endyear >= startyear,
  #               msg = "startyear must not be larger than endyear.")
  #   assert_that("loc_code" %in% colnames(locs),
  #               msg = "locs does not have a column name 'loc_code'.")
  #   assert_that(is.flag(truncated), noNA(truncated))
  #   assert_that(is.flag(collect), noNA(collect))
  #   
  #   if (inherits(locs, "data.frame")) {
  #     locs <-
  #       locs %>%
  #       distinct(.data$loc_code)
  #     
  #     try(db_drop_table(con, "#locs"),
  #         silent = TRUE)
  #     
  #     
  #     # locs_oud <-
  #     #   copy_to(con,
  #     #           locs) %>%
  #     #   inner_join(tbl(con, "vwDimMeetpunt") %>%
  #     #                dplyr::select(loc_wid = .data$MeetpuntWID,
  #     #                       loc_code = .data$MeetpuntCode),
  #     #              .,
  #     #              by = "loc_code")
  #     locs <-
  #       tbl(con, "vwDimMeetpunt") %>%
  #       dplyr::select(loc_wid = .data$MeetpuntWID,
  #              loc_code = .data$MeetpuntCode)
  #     
  #   }
  #   
  #   xg3 <-
  #     tbl(con, "ssrs_Precalc") %>%
  #     # left_join(tbl(con, "DimMetingType"),
  #     #           by = "MetingTypeWID") %>%
  #     dplyr::select(loc_wid = .data$MeetpuntWID,
  #            hydroyear = .data$HydroJaar,
  #            # method_code = .data$MetingTypeCode,
  #            # method_name = .data$MetingTypeNaam,
  #            lg3_lcl = .data$GLG_2,
  #            hg3_lcl = .data$GHG_2,
  #            vg3_lcl = .data$GVG_2,
  #            lg3_ost = .data$GLG_1,
  #            hg3_ost = .data$GHG_1,
  #            vg3_ost = .data$GVG_1
  #     ) %>%
  #     filter(.data$hydroyear >= startyear,
  #            .data$hydroyear <= endyear) %>%
  #     inner_join(locs %>%
  #                  dplyr::select(.data$loc_wid,
  #                         .data$loc_code) %>%
  #                  distinct,
  #                .,
  #                by = "loc_wid") %>%
  #     dplyr::select(-.data$loc_wid)
  #   
  #   xg3 <-
  #     switch(vert_crs,
  #            local = xg3 %>% dplyr::select(-contains("ost")),
  #            ostend = xg3 %>% dplyr::select(-contains("lcl")),
  #            both = xg3
  #     ) %>%
  #     arrange(.data$loc_code,
  #             .data$hydroyear)
  #   
  #   if (collect) {
  #     xg3 <-
  #       xg3 %>%
  #       collect
  #   }
  #   
  #   return(xg3)
  #   
  # }    
  # debugonce("get_xg3_temp")  
  library(assertthat)
  #watina2 <- inbodb::connect_inbo_dbase("W0002_00_Watina") 
  
#   tubes_xg3_alles <- tubes_hab %>% 
#     get_xg3(watina2, startyear = year(now()) - 18, endyear = 2016, vert_crs = "local",
#             truncated =  TRUE, collect = TRUE)
#   tubes_xg3 <- tubes_xg3_alles  %>% 
#     semi_join(tubes_hab, by = "loc_code")
# all.equal(tubes_xg3, tubes_xg32)
  #debugonce(copy_to)
  tubes_xg3 <- tubes_hab %>% 
        get_xg3(watina, startyear = year(now()) - 20, endyear = year(now()), vert_crs = "local",
                truncated =  TRUE, collect = TRUE)
#toevoegen van pb gebruikt voor grondwaterstandstandsindicator die gelegen zijn in een gw-afhankelijk type
  tubes_xg3_vmm <- data.frame(loc_code = rep(tubes_hab %>% 
                                       filter(area_code == "VMM") %>% 
                                       distinct(loc_code) %>% 
                                       dplyr::pull(loc_code),times = 31), 
                              hydroyear = seq(from = 2016 - 30, to = 2016),
                              lg3_lcl = rep(-1, 31)) %>% as_tibble()
  
  tubes_xg3 <- bind_rows(tubes_xg3, tubes_xg3_vmm)
 # test <- data.frame(a= "a", b= "b")
 #  locs <-
 #    copy_to(watina2,
 #            test, overwrite = TRUE)
  #oplossen van UTF-probleem: strings worden in inbo-SQL-databanken opgeslagen in UTF-16, terwijl hier gewerkt wordt met UTF-8. Dit geeft een probleem bij de kable-functie
  
  #tubes_xg3 <- mutate_if(tubes_xg3, is.character, iconv, to = "UTF-8")
  
  tubes_xg3 <- tubes_xg3 %>% 
    arrange(loc_code, hydroyear)
  
  output_vc <- write_vc(tubes_xg3, file.path(".","data","processed", "meetnet","tubes_xg3"), 
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
      dplyr::select(.data$loc_code,
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
  # debugonce("extract_xg3_series")
  # test <- tubes_xg3_basis %>%
  #   eval_xg3_series_lok(xg3_type = c("L"),
  #                       max_gap = maxgap,
  #                       min_dur = minlength)
  
  tubes_lg3_eval <- tubes_xg3 %>%
    eval_xg3_series_lok(xg3_type = c("L"),
                        max_gap = maxgap,
                        min_dur = minlength) %>% as_tibble()
  
  tubes_lgl_eval <- tubes_lg3_eval %>%
    filter(ser_nryears >= minnryears)
  
  output_vc <- write_vc(tubes_lgl_eval, file.path(".","data","processed", "meetnet","tubes_lgl_eval"), 
                        sorting = c("loc_code", "ser_firstyear"), 
                        strict =  FALSE, root = ".")
  
  rm(output_vc)
}
#einde inladen

#inlezen van lokale data (alleen van toepassing bij params$refresh_data-optie = 0)
if (params$refresh_data < 1) {
gw_types <- read_vc("gw_types", file.path(".","data"))

habmap_polygons_gw <- read_sf(file.path(".","data","local", "habmap_terr_gw.gpkg"),
                              "habitatmap_terr_polygons_gw")

habmap_types_gw <- suppressWarnings(read_sf(file.path(".","data","local", "habmap_terr_gw.gpkg"),
                                            "habitatmap_terr_types_gw"))

raster_meetnet_poly <- suppressWarnings(read_sf(file.path(".","data","local", "raster_meetnet_poly.gpkg"), "raster_meetnet_poly"))

habmap_gw_raster_overlay <- suppressWarnings(read_sf(file.path(".","data","local", "habmap_gw_raster_overlay.gpkg"), "habmap_gw_raster_overlay"))
}

gw_types_groups <- gw_types %>%
  distinct(groupnr, typegroup_name) %>% 
  inner_join(data.frame(groupnr = seq(1:5), gewenst_aantal_locaties = minaantal_tub_group), by = "groupnr") %>% 
  rename("GT-groep: nummer" = groupnr,
         "GT-groep: naam" = typegroup_name,
         "Gewenst aantal meetlocaties" = gewenst_aantal_locaties)


# write_csv(gw_types, "lijst_grondwaterafhankelijke_habitattypen_rbbs.csv")
# write_csv(gw_types_groups, 'grondwatertypen.csv')
# if (file.exists(file.path(".","data","local", "habmap_terr_gw.gpkg")) == FALSE | params$refresh_data == 2) {
#   drive_download(drive_get(id = "1nxnpfE3Eh4eCiM2VinGYMJE55qD4Az1c"), 
#                  path = file.path(".","data","local", "habmap_terr_gw.gpkg"), overwrite = TRUE)
# }




# write_sf(habmap_polygons_gw, "habmap_gw.shp")

# st_crs(habmap_polygons_gw)

habmap_polygons_gw <- habmap_polygons_gw %>%
  mutate(polygon_id = factor(.data$polygon_id),
         type = factor(.data$type),
         typegroup_name = factor(.data$typegroup_name)
  )



#raster_meetnet_poly <- read_GRTSmh_diffres(level = 8, polygon = TRUE)

# 
# if (file.exists(file.path(".","data","local", "raster_meetnet_poly.gpkg")) == FALSE | params$refresh_data >= 1 ){
#   drive_download(drive_get(id = "1oHdlUEEZmCDvXDCSXELgtgKNDgLn4E_0"), 
#                  path = file.path(".","data","local", 
#                                   "raster_meetnet_poly.gpkg"), overwrite = 
#                    TRUE)
# }


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
raster_meetnet_poly <- sf::st_make_valid(raster_meetnet_poly)
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

# if (file.exists(file.path(".","data","local", 
#                           "habmap_gw_raster_overlay.gpkg")) == FALSE | params$refresh_data >= 1) {
#   drive_download(drive_get(id = "1oY7fXj7Kd59w1LFHhu88E9cLLkC5cPJS"), 
#                  path = file.path(".","data","local", 
#                                   "habmap_gw_raster_overlay.gpkg"), 
#                  overwrite = TRUE)
# }



habmap_gw_raster_overlay <- st_make_valid(habmap_gw_raster_overlay)

habmap_gw_raster_overlay
if (params$refresh_figures >= 1) {
  habmap_gw_raster_overlay_tm <- raster_meetnet_poly_tm + 
    tm_shape(habmap_gw_raster_overlay) + 
    tm_fill(col = "groupnr", style = "cat", palette = "BuGn", title = "Grondwatertype") + 
    tm_layout(title = "Verspreiding van de GT-groepen" )
  
  tmap_save(habmap_gw_raster_overlay_tm, 
            filename = file.path(".","figures","local", 
                                 "habmap_gw_raster_overlay.png"),
            dpi = 250
  )
  include_graphics(path = file.path(".","figures","local", 
                                    "habmap_gw_raster_overlay.png")) 
  
}
# if (file.exists(file.path(".","figures","local", 
#                           "habmap_gw_raster_overlay.png")) == FALSE | params$refresh_figures >= 1) {
#   drive_download(drive_get(id = "1hLSIyTT-yk1wY0qtqhD0Qh6Ps_KnI211"), 
#                  path = file.path(".","figures","local", 
#                                   "habmap_gw_raster_overlay.png"), 
#                  overwrite = TRUE)
# }

if (params$refresh_figures < 1) {
  include_graphics(path = file.path(".","figures","local", 
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
#min_aantal_tub = data.frame("groupnr" = 1:aantal_strat, minaantal = seq(minaantal_tub_group,minaantal_tub_group,length.out = aantal_strat))

#minaantal_tub_group <- c(11,22,33,34,0)
min_aantal_tub = data.frame("groupnr" = 1:aantal_strat, minaantal = minaantal_tub_group)

for (group in seq(1,aantal_strat)) {
  #group <- 4
  minaantal_tub_group_1 <- min_aantal_tub[min_aantal_tub$groupnr == group,"minaantal"]
  aantal_tub_group <- 0
  corrafronding <- 1
  gw_opp <- gw_opp %>% 
    left_join(gw_opp %>% 
                filter(groupnr == group) %>% 
                mutate(bewerkt = TRUE) %>% 
                dplyr::select(groupnr, bewerkt)
              , by = "groupnr")
  
  while (aantal_tub_group < minaantal_tub_group_1) {
    if (group == 1){
      gw_opp <- gw_opp %>% 
        mutate(minopp = 
                 ifelse(bewerkt == TRUE, round(opp_gw/minaantal_tub_group_1 * corrafronding,0), minopp) %>% set_units("ha")
        )
    } else {
      gw_opp <- gw_opp %>% 
        mutate(minopp = 
                 if_else(bewerkt == TRUE, round(opp_gw/minaantal_tub_group_1 * corrafronding,0), minopp, missing = minopp) %>% set_units("ha")
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
                      as.numeric(round(minopp*minaantal_tub_group_1/opp_gw, 2)), 
                      correctiefactor))
  } else {
    gw_opp <- gw_opp %>% 
      mutate(correctiefactor = 
               if_else(bewerkt == TRUE, 
                       as.numeric(round(minopp*minaantal_tub_group_1/opp_gw, 2)), 
                       correctiefactor, missing = correctiefactor))
  }
  
  gw_opp <- gw_opp %>% 
    dplyr::select(-bewerkt)   
  
  if (group == 1){
    aantal_meetpunten_cel <- aantal_meetpunten_cel_group
  } else {
    aantal_meetpunten_cel <- aantal_meetpunten_cel %>% 
      full_join(aantal_meetpunten_cel_group %>% 
                  dplyr::select(-correctiefactor))
  }
}
# df <- aantal_meetpunten_cel %>% group_by(groupnr)  %>% 
#    summarise(aantal  = sum(gew_aantal_meetptn_afgerond)) %>% 
#    ungroup()
# df


aantal_meetpunten_cel <- aantal_meetpunten_cel %>% 
  dplyr::select(-bewerkt)
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
               dplyr::select(rasterid, groupnr, gew_aantal_meetptn_afgerond), 
             by =  c("rasterid", "groupnr")) %>% 
  rename(gew_aantal_meetptn = gew_aantal_meetptn_afgerond)

sel_raster_meetnet <- st_make_valid(sel_raster_meetnet)

#groeperen van gefragmenteerde rastercellen
sel_raster_meetnet <- 
  sel_raster_meetnet  %>% 
  group_by(rasterid, groupnr, opp_gw_cel, gew_aantal_meetptn) %>% 
  summarise (temp = n()) %>% 
  ungroup %>% 
  dplyr::select(-temp)

#uit voorzorg nog eens de geometrie checken
sel_raster_meetnet <- st_make_valid(sel_raster_meetnet)
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

#tubes_hab_2019 <- read_vc(file.path(".","data","tubes_hab_2019"))
tubes_hab <- read_vc(file.path(".","data","tubes_hab"))  %>% as_tibble()

#peilbuizen uit de lijst verwijderen die niet (meer) mogen meegenomen worden

if (length((find("uitgesloten_tubes"))) > 0){
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

# tubes_hab_aggr <- tubes_hab %>%
#   dplyr::select(-phab, -certain, -type, -source.y, -polygon_id, -starts_with("description"), -opp, -source.x) %>% 
#   distinct %>% 
#   semi_join(tubes_hab_multipolyg, by = c("loc_code", "rasterid", "groupnr" ))
tubes_hab_aggr <- tubes_hab %>%
  dplyr::select(-phab, -certain, -type, -source, -polygon_id, -starts_with("description"), -opp) %>% 
  distinct %>% 
  semi_join(tubes_hab_multipolyg, by = c("loc_code", "rasterid", "groupnr" ))


### Opzoeken van peilbuizen in de geselecteerde rastercellen

tubes_in_raster <- tubes_hab_aggr %>% 
  # dplyr::select(-opp, -description_orig, -source.x, -starts_with("loc_"), loc_code) %>%  
  inner_join(sel_raster_meetnet %>% 
               dplyr::select(rasterid, groupnr) %>% 
               st_drop_geometry(), by = c("rasterid", "groupnr")) %>% 
  distinct()


#wegschrijven van deze dataset
output_vc <- write_vc(tubes_in_raster, file.path(".","data","processed", "meetnet","tubes_in_raster"), 
                      sorting = c("loc_code"), 
                      strict =  FALSE, root = ".")

rm(output_vc)

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

tubes_xg3 <- read_vc(file.path(".","data","tubes_xg3")) %>% as_tibble()
tubes_xg3_basis <- tubes_xg3
tubes_xg3 <- tubes_xg3 %>% 
  inner_join (tubes_in_raster %>% 
                dplyr::select(loc_code), by = "loc_code")

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


#all.equal(tubes_xg3_avail,test)
 
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
tubes_lgl_eval <- read_vc("tubes_lgl_eval", file.path(getwd(),"data")) %>% as_tibble()
tubes_lgl_eval_bu <- tubes_lgl_eval
tubes_lgl_eval <- tubes_lgl_eval %>% 
  inner_join(tubes_in_raster %>% 
               dplyr::select(loc_code), by = "loc_code")


## De rastercellen categoriseren o.b.v. de beschikbaarheid van peilbuizen 

### Koppeling van peilbuizen, met de kwaliteit van hun tijdreeksen, aan de geselecteerde rastercellen


#rastercellen met een pb
sel_raster_pb <- 
  sel_raster_meetnet %>% 
  inner_join(tubes_in_raster, 
             by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters
  left_join(tubes_lg3_avail, 
            by = "loc_code") %>% # aanduiding van pb met een lg3
  group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
  summarise(n_tubes = n(),
            n_tubes_lg3 = sum(lastyear > 0)) %>% 
  ungroup %>% 
  dplyr::select(-geom, geom)




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
              dplyr::select(-gew_aantal_meetptn),
            by = c("rasterid", "groupnr")) %>% 
  filter(is.na(n_tubes) | gew_aantal_meetptn > n_tubes) %>% 
  arrange(rasterid, groupnr)

sel_cat1A_table <- sel_cat1A_raster %>% 
  st_drop_geometry() %>% 
  rename(gewenst_aantal_meetpunten = gew_aantal_meetptn) %>% 
  mutate(totaal_peilbuizen =  replace_na(n_tubes,0),
         aantal_cat1A = gewenst_aantal_meetpunten - totaal_peilbuizen) %>% 
  dplyr::select(-n_tubes) %>% 
  arrange(rasterid)


sel_cat1A_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_cat1A_raster) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.1A: cel met een onvoldoend aantal peilbuizen" )

sel_cat1A_tm

### Categorie 1B: Rastercellen met peilbuizen, maar onvoldoende **geschikte** {#cat1b}


# sel_cat1B_raster <- sel_raster_pb %>%
#   left_join(sel_cat1A_table %>%
#               dplyr::select(-gewenst_aantal_meetpunten, -totaal_peilbuizen,
#                      -n_tubes_lgl, -opp_gw_cel),
#             by = c("rasterid", "groupnr")) %>%
#   mutate(aantal_cat1A = replace_na(aantal_cat1A,0),
#          rest = gew_aantal_meetptn - aantal_cat1A) %>%
#   filter(rest - n_tubes_lgl > 0 ) %>%
#   mutate(aantal_cat1B = rest - n_tubes_lgl) %>%
#   dplyr::select(-geom, -rest, geom)


# # deze rastercellen vormen de tweede groep (groep met pb, maar alle zonder een lgl of er zijn er te weinig)
# # voor deze groep kan onderzocht worden of door het modelmatig verbeteren van de tijdreeksen er geen lgl kan berekend worden
# 
# sel_cat1B_table <- sel_cat1B_raster %>% 
#   st_drop_geometry() %>% 
#   arrange(rasterid)
# 
# 
# 
# sel_cat1B_tm <- raster_meetnet_poly_tm + 
#   tm_shape(sel_cat1B_raster) + 
#   tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.1B: cel met een onvoldoend aantal peilbuizen met goede meetreeks" )
# 
# sel_cat1B_tm

# 
# #### Opzoeken van peilbuizen binnen de rastercellen van cat 1B
# 
# tubes_cat1B <- 
#   sel_cat1B_raster %>% 
#   inner_join(tubes_in_raster, 
#              by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters
#   # inner_join(tubes_lg3_avail, 
#   #            by = "loc_code") %>% # aanduiding van pb lg3, geen lgl
#   st_drop_geometry() %>% 
#   dplyr::select(rasterid, groupnr, gew_aantal_meetptn, loc_code, everything(), 
#          -starts_with("loc_v"), -starts_with("loc_t")) %>% 
#   arrange(rasterid, groupnr)
# 
# 
# #write_csv(tubes_cat1B, file.path(".", "data","local", "tubes_cat1B.csv"))
# output_vc <- write_vc(tubes_cat1B, file.path(".","data","tubes_cat1B"), 
#                       sorting = c("loc_code"), strict =  FALSE, root = ".")
# 
# rm(output_vc)


#oplijsten per rastercel welke peilbuizen een kwalitatief onvoldoende tijdreeks hebben



### Het opleggen van bijkomende kwaliteitscriteria


#### Het toepassen van drie extra kwaliteitscriteria


# sel_qual_basis <- 
#   sel_raster_pb %>% 
#   dplyr::select(rasterid, groupnr, gew_aantal_meetptn) %>% 
#   st_drop_geometry() %>% 
#   left_join(sel_cat1A_table %>% 
#               dplyr::select(rasterid, groupnr, aantal_cat1A), 
#             by = c("rasterid", "groupnr")) %>%
#   # left_join(sel_cat1B_table %>% 
#   #             dplyr::select(rasterid, groupnr, aantal_cat1B), 
#   #           by = c("rasterid", "groupnr")) %>%
#   inner_join(tubes_in_raster, 
#              by =  c("rasterid", "groupnr")) %>% 
#   distinct(rasterid, groupnr, gew_aantal_meetptn, loc_code, aantal_cat1A) %>% 
#   left_join(tubes_lg3_avail, 
#              by = "loc_code") %>% 
#   mutate(aantal_cat1A = replace_na(aantal_cat1A,0),
#          lastyear = replace_na(lastyear,0),
#          nryears = replace_na(nryears,0)) %>% 
#   group_by(rasterid, groupnr, gew_aantal_meetptn, aantal_cat1A) %>% 
#   count(lastyear, nryears)  %>% 
#   ungroup() %>% 
#   mutate(rest_aantal_meetptn = gew_aantal_meetptn - aantal_cat1A)

sel_qual_basis <- 
  sel_raster_pb %>% 
  dplyr::select(rasterid, groupnr, gew_aantal_meetptn) %>% 
  st_drop_geometry() %>% 
  anti_join(sel_cat1A_table %>% 
              dplyr::select(rasterid, groupnr), 
            by = c("rasterid", "groupnr")) %>%
  inner_join(tubes_in_raster, 
             by =  c("rasterid", "groupnr")) %>% 
  distinct(rasterid, groupnr, gew_aantal_meetptn, loc_code) %>% 
  left_join(tubes_lg3_avail, 
            by = "loc_code") %>% 
  mutate(lastyear = replace_na(lastyear,0),
         nryears = replace_na(nryears,0)) %>% 
  group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
  count(lastyear, nryears)  %>% 
  ungroup()

sel_qual_basis <- 
  sel_qual_basis %>% 
  group_by(rasterid, groupnr) %>% 
  arrange(rasterid, groupnr, gew_aantal_meetptn, desc(lastyear)) %>% 
  mutate(rankclus_lastyear = 
           floor((cummax(lastyear) - lastyear)/toelaatbare_spreiding_jaren) + 1) %>% 
  arrange(rasterid, groupnr, gew_aantal_meetptn, desc(nryears)) %>% 
  mutate(rankclus_nryears = 
           floor((cummax(nryears) - nryears)/toelaatbaar_verschil_lengte_tijdreeks) + 1) %>% 
  ungroup()


sel_qual <- 
  sel_qual_basis %>% 
  group_by(rasterid, groupnr, gew_aantal_meetptn, rankclus_lastyear, rankclus_nryears) %>% 
  mutate(rankclus_temp = as.integer(paste0(rankclus_lastyear,rankclus_nryears))) %>% 
  arrange(rasterid, groupnr,rankclus_temp) %>% 
  ungroup() %>% 
  group_by(rasterid, groupnr) %>% 
  mutate(rankclus = dense_rank(rankclus_temp)) %>% 
  group_by(rasterid, groupnr, gew_aantal_meetptn, rankclus, rankclus_lastyear, rankclus_nryears) %>% 
  summarise(beschikbaar_aantal_cluster = sum(n)) %>% 
  ungroup 

# functie om de rang te bepalen die nodig is om tot het gewenst aantal meetpunten te komen

max_rank <-  function(x) {
  # x <- sel_qual_test %>% filter (rasterid == 134, groupnr == 4)
  clusters <- unique(x$rankclus) 
  gewenst_aantal <- x[1,"gew_aantal_meetptn"] %>%
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
  rename(maxrank = V1) %>% as_tibble()

sel_qual <- sel_qual %>%
  inner_join(sel_qual_maxrank, 
             by = c("rasterid", "groupnr"))



### Categorie 2: Rastercellen met juist voldoende of te weinig geschikte peilbuizen {#cat2}


# rastercellen met een juist voldoende evenwaardige meetpunten dat gewenst is voor het meetnet
sel_cat2_table <- 
  sel_qual %>% 
#  filter(rankclus <= maxrank ) %>% 
  group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
  summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
  ungroup %>% 
  inner_join(sel_qual,
             by = c("rasterid", "groupnr", "gew_aantal_meetptn")) %>% 
  filter(beschikbaar_aantal == gew_aantal_meetptn) %>% 
  dplyr::select(-beschikbaar_aantal, -maxrank)


sel_cat2_raster <- sel_raster_pb %>% 
  semi_join(sel_cat2_table,
            by = c("rasterid", "groupnr")) %>% 
  arrange(rasterid, groupnr)


sel_cat2_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_cat2_raster) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.2: rastercellen met een juist voldoende aantal peilbuizen" )

sel_cat2_tm





### Categorie 3: Rastercellen met een overschot aan geschikte peilbuizen {#cat3}

# rastercellen met een overschot aan evenwaardige meetpunten in vergelijking met het aantal dat gewenst is voor het meetnet
sel_cat3_raster <- 
  sel_raster_meetnet %>% 
  anti_join(sel_cat1A_raster %>% 
              st_drop_geometry(), 
            by = c("rasterid", "groupnr"))  %>% 
  # anti_join(sel_cat1B_raster %>% 
  #             st_drop_geometry(), 
  #           by = c("rasterid", "groupnr"))  %>% 
  anti_join(sel_cat2_raster %>% 
              st_drop_geometry(), 
            by = c("rasterid", "groupnr")) %>% 
  arrange(rasterid, groupnr)

test <- 
  sel_raster_meetnet %>% 
  anti_join(sel_cat1A_raster %>% 
              st_drop_geometry(), 
            by = c("rasterid", "groupnr"))  %>% 
  anti_join(sel_cat2_raster %>% 
              st_drop_geometry(), 
            by = c("rasterid", "groupnr")) %>% 
  arrange(rasterid, groupnr)

all.equal(sel_cat3_raster %>% st_drop_geometry(), test %>% st_drop_geometry())

sel_cat3_table <- 
  sel_qual %>% 
#  filter(rankclus <= maxrank ) %>% 
  group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
  summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
  ungroup %>% 
  inner_join(sel_qual, by = c("rasterid", "groupnr", "gew_aantal_meetptn") ) %>% 
  filter(beschikbaar_aantal > gew_aantal_meetptn, rankclus <= maxrank) %>% 
  dplyr::select(-beschikbaar_aantal, -maxrank)  




sel_cat3_tm <- raster_meetnet_poly_tm + 
  tm_shape(sel_cat3_raster) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.3: rastercellen met een overschot aan geschikte peilbuizen" )

sel_cat3_tm


#### Opzoeken van peilbuizen voor de rastercellen van cat. 3

#bijhorende Watina-meetpunten ervan opzoeken
tubes_cat1 <- 
  tubes_in_raster %>% 
  left_join(tubes_lg3_avail %>% 
              dplyr::select(loc_code, firstyear, lastyear, 
                     nryears), 
            by = "loc_code") %>% 
  mutate(lastyear = replace_na(lastyear,0),
         nryears = replace_na(nryears,0),
         cat = 1) %>% 
  inner_join(sel_cat1A_table, 
             by = c("rasterid", "groupnr")) %>% 
  mutate(lastyear = ifelse(lastyear == 0, NA, lastyear)) %>% 
  dplyr::select(-c(opp_gw_cel:aantal_cat1A))
  

#### Opzoeken van peilbuizen voor de rastercellen van cat. 2 {#opzoeken-pb-cat2}


# de bijhorende geselecteerde Watina-meetpunten zijn dan
tubes_cat2 <- 
  tubes_in_raster %>% 
  left_join(tubes_lg3_avail %>% 
              dplyr::select(loc_code, firstyear, lastyear, 
                     nryears), 
            by = "loc_code") %>% 
  mutate(lastyear = replace_na(lastyear,0),
         nryears = replace_na(nryears,0),
         cat = 2) %>% 
  inner_join(sel_cat2_table  %>% 
               inner_join(sel_qual_basis %>% 
                            dplyr::select(-gew_aantal_meetptn), 
                          by =  c("rasterid","groupnr","rankclus_lastyear", "rankclus_nryears" )), 
             by = c("rasterid", "groupnr", "lastyear", "nryears")) %>% 
  mutate(lastyear = ifelse(lastyear == 0, NA, lastyear)) %>% 
  dplyr::select(-c(gew_aantal_meetptn:n))

output_vc <- write_vc(tubes_cat2, file.path(".","data","processed", "meetnet","tubes_cat2_run1"), 
                      sorting = c("loc_code"), strict =  FALSE, root = ".")

rm(output_vc)
tubes_cat3 <- 
  tubes_in_raster %>% 
  # distinct(loc_code, x, y, rasterid, groupnr) %>% 
  left_join(tubes_lg3_avail %>% 
               dplyr::select(loc_code, firstyear, 
                      lastyear, nryears), 
             by = "loc_code") %>% 
  mutate(lastyear = replace_na(lastyear,0),
         nryears = replace_na(nryears,0),
         cat = 3) %>%   
  inner_join(sel_cat3_table  %>% 
               inner_join(sel_qual_basis %>% 
                            dplyr::select(-gew_aantal_meetptn), 
                          by = c("rasterid","groupnr","rankclus_lastyear","rankclus_nryears" )), 
             by = c("rasterid","groupnr", "lastyear","nryears")) %>% 
  mutate(lastyear = ifelse(lastyear == 0, NA, lastyear)) %>% 
  dplyr::select(-c(gew_aantal_meetptn:n))


tubes_cat123 <- bind_rows(tubes_cat1, tubes_cat2, tubes_cat3) %>% arrange(loc_code)

tubes_cat3 <- tubes_cat3 %>% 
  rownames_to_column("unieknr") %>% 
  mutate(unieknr = as.integer(unieknr))
# sel_qual_lastyear_vb %>% 
#   count(rasterid, groupnr )


output_vc <- write_vc(tubes_cat3, file.path(".","data","processed", "meetnet","tubes_cat3_run1"), 
                      sorting = c("loc_code"), strict =  FALSE, root = ".")

rm(output_vc)



##Tijdreeksanalyse {#tijdreeksanalyse_uitvoering}

tubes_menyanthes <- read_csv(file.path(getwd(),"data","interim", "meetnet", "tblTubes_Menyanthes_report.csv"))

#veldnamen aanpassen
tubes_menyanthes <- janitor::clean_names(tubes_menyanthes, case = "snake")

#syntheseveld maken
tubes_menyanthes <- tubes_menyanthes %>% 
  mutate(
    uitspraak = case_when(
      modelbaar == 1 ~ "weerhouden, expertoordeel",
      evaporatiefactor == -1 ~ "niet weerhouden, te korte tijdreeks",
      !is.na(evaporatiefactor) ~ "niet weerhouden, expertoordeel",
      evp < 66 ~ "niet weerhouden, te lage modelfit",
      is.na(trend_verschil) | abs(trend_verschil) - 1.96*trend_sd <= trend_jaren ~ "weerhouden",
      abs(trend_verschil) - 1.96*trend_sd > trend_jaren ~ "niet weerhouden, trend"
    ),
    selectie = if_else(str_detect(uitspraak, pattern = "niet weerhouden"),0,1)
  )

#wegfilteren van meetreeksen die niet in een raster vallen (is mogelijk wanneer een peilbuis van een oude selectie in de resultaattabel van Menyanthes is verzeild)
tubes_menyanthes <- tubes_menyanthes %>% 
  semi_join(tubes_in_raster, by = c("watinacode" = "loc_code"))

# tubes_menyanthes_test <- tubes_menyanthes %>% 
#   inner_join(tubes_in_raster, by = c("watinacode" = "loc_code"))
# 
# check_meny <- tubes_menyanthes_test %>% 
#   group_by(rasterid, groupnr, selectie) %>% 
#   count(selectie)

tubes_menyanthes_synthese <- tubes_menyanthes %>% 
  count(uitspraak) %>% 
  rename(aantal = n)


## De rastercellen hercategoriseren na de tijdreeksanalysen 


#peilbuizen miv evaluatie Menyanthes
tubes_eval_namenyanthes <- tubes_in_raster %>% 
  left_join(tubes_menyanthes %>% 
              dplyr::select(watinacode, selectie), by = c("loc_code" = "watinacode")) %>% 
  left_join(tubes_lg3_avail,
            by = "loc_code") %>%
  mutate(selectie = case_when(
    selectie == 0 ~ -1, #afgekeurde meetreeks
    selectie == 1 ~ 1, #goedgekeurde meetreeks
    TRUE ~ 0 #niet geanalyseerde meetreeks
  )
  ) 

#rastercellen met een pb
# sel_raster_pb_bis <- 
#   sel_raster_meetnet %>% 
#   inner_join(tubes_in_raster, 
#              by = c("rasterid", "groupnr")) %>% # koppeling van pb aan rasters
#   left_join(tubes_lg3_avail, 
#             by = "loc_code") %>% # aanduiding van pb met een lg3
#   group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
#   summarise(n_tubes = n(),
#             n_tubes_lg3 = sum(lastyear > 0)) %>% 
#   ungroup %>% 
#   dplyr::select(-geom, geom)



### Categorie 1A: Zijn er rastercellen met onvoldoende peilbuizen?


### Categorie 1B: Rastercellen met een onvoldoend aantal **geschikte** peilbuizen

#' 
#' 
#' sel_cat1B_raster_bis <- sel_raster_pb_bis %>%
#'   left_join(sel_cat1A_table %>%
#'               dplyr::select(-gewenst_aantal_meetpunten, -totaal_peilbuizen,
#'                      -n_tubes_lgl, -opp_gw_cel),
#'             by = c("rasterid", "groupnr")) %>%
#'   mutate(aantal_cat1A = replace_na(aantal_cat1A,0),
#'          rest = gew_aantal_meetptn - aantal_cat1A) %>%
#'   filter(rest - n_tubes_lgl > 0 ) %>%
#'   mutate(aantal_cat1B = rest - n_tubes_lgl) %>%
#'   dplyr::select(-geom, -rest, geom)
#' 
#' 
#' sel_cat1B_table_bis <- sel_cat1B_raster_bis %>% 
#'   st_drop_geometry() %>% 
#'   arrange(rasterid)
#' 
#' 
#' 
#' sel_cat1B_tm_bis <- raster_meetnet_poly_tm + 
#'   tm_shape(sel_cat1B_raster_bis) + 
#'   tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.1B: cel met een onvoldoend aantal peilbuizen met goede meetreeks" )
#' 
#' sel_cat1B_tm_bis
#' 
#' tubes_cat1B_bis <- 
#'   sel_cat1B_raster_bis %>% 
#'   inner_join(tubes_eval_namenyanthes %>% 
#'                filter(selectie <= 0), by = c("rasterid", "groupnr")) %>% 
#'   st_drop_geometry() %>% 
#'   dplyr::select(rasterid, groupnr, gew_aantal_meetptn, loc_code, everything(),
#'          -starts_with("loc_v"), -starts_with("loc_t"), -starts_with("ser"), -starts_with("xg3")) %>% 
#'   arrange(rasterid, groupnr)
#' 
#' 
#' tubes_cat1Ba <- tubes_cat1B_bis %>% 
#'   inner_join(tubes_cat1B_bis %>% 
#'                group_by(rasterid, groupnr, gew_aantal_meetptn, aantal_cat1A) %>% 
#'                summarise(n_tubes_zondermodel = n())
#'   ) %>% 
#'   mutate(n_tubes_zondermodel = n_tubes_zondermodel + aantal_cat1A) %>% 
#'   filter(gew_aantal_meetptn == n_tubes_zondermodel)
#' 
#' 
#' output_vc <- write_vc(tubes_cat1Ba, file.path(".","data","tubes_cat1Ba"), 
#'                       sorting = c("loc_code"), strict =  FALSE, root = ".")
#' 
#' rm(output_vc)
#' 
#' 
#' 
#' tubes_cat1Bb <- tubes_cat1B_bis %>% 
#'   anti_join(tubes_cat1Ba, by = "loc_code") %>% 
#'   inner_join(tubes_cat1B_bis %>% 
#'                group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
#'                summarise(n_tubes_zondermodel = n())
#'   ) 
#' 
#' 
#' 
#' output_vc <- write_vc(tubes_cat1Bb, file.path(".","data","tubes_cat1Bb"), 
#'                       sorting = c("loc_code"), strict =  FALSE, root = ".")
#' 
#' rm(output_vc)
#' test <- tubes_cat1Bb %>% 
#'   dplyr::select(-n_tubes_lgl, -aantal_cat1A, -aantal_cat1B,
#'          -typegroup_name, -selectie) %>% 
#'   dplyr::select(1:3,12,7,4,8:9) %>% 
#'   rename(GTgroep = groupnr,
#'          rasternr = rasterid,
#'          watinacode = loc_code,
#'          'gewenst aantal meetptn' = gew_aantal_meetptn,
#'          #'# peilbuizen' =  n_tubes,
#'          gebied = area_name,
#'          #gebiedcode = area_code,
#'          '# beschikbare ptn' = n_tubes_zondermodel
#'   ) %>% 
#'   arrange(watinacode)

tubes_qual_basis_bis <- 
  sel_raster_pb %>% 
  dplyr::select(rasterid, groupnr, gew_aantal_meetptn) %>% 
  st_drop_geometry() %>% 
  anti_join(sel_cat1A_table %>% 
              dplyr::select(rasterid, groupnr), 
            by = c("rasterid", "groupnr")) %>%
  inner_join(tubes_eval_namenyanthes, 
             by =  c("rasterid", "groupnr")) %>% 
  distinct(rasterid, groupnr, gew_aantal_meetptn, loc_code, 
           lastyear, nryears, 
           firstyear, selectie) %>%
  mutate(lastyear = ifelse(selectie == 1, 2019, ifelse(selectie == -1, 0,replace_na(lastyear,0))),
         nryears = ifelse(selectie == 1, 100, ifelse(selectie == -1, 0,replace_na(nryears,0)))
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
  group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
  count(lastyear, nryears)  %>% 
  ungroup()

sel_qual_basis_bis <- 
  sel_qual_basis_bis %>% 
  group_by(rasterid, groupnr) %>% 
  arrange(rasterid, groupnr, gew_aantal_meetptn, desc(lastyear)) %>% 
  mutate(rankclus_lastyear = 
           floor((cummax(lastyear) - lastyear)/toelaatbare_spreiding_jaren) + 1) %>% 
  arrange(rasterid, groupnr, gew_aantal_meetptn, desc(nryears)) %>% 
  mutate(rankclus_nryears = 
           floor((cummax(nryears) - nryears)/toelaatbaar_verschil_lengte_tijdreeks) + 1) %>% 
  ungroup()

# kable(sel_qual_basis_bis %>% 
#         dplyr::select(-gew_aantal_meetptn, -aantal_cat1A, -aantal_cat1B) %>% 
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
  group_by(rasterid, groupnr, gew_aantal_meetptn, rankclus_lastyear, rankclus_nryears) %>% 
  mutate(rankclus_temp = as.integer(paste0(rankclus_lastyear,rankclus_nryears))) %>% 
  arrange(rasterid, groupnr,rankclus_temp) %>% 
  ungroup() %>% 
  group_by(rasterid, groupnr) %>% 
  mutate(rankclus = dense_rank(rankclus_temp)) %>% 
  group_by(rasterid, groupnr, gew_aantal_meetptn, rankclus, rankclus_lastyear, rankclus_nryears) %>% 
  summarise(beschikbaar_aantal_cluster = sum(n)) %>% 
  ungroup 

sel_qual_maxrank_bis <- plyr::ddply(sel_qual_bis, ~rasterid+groupnr, max_rank) %>%
  rename(maxrank = V1) %>% as_tibble()

sel_qual_bis <- sel_qual_bis %>%
  inner_join(sel_qual_maxrank_bis, 
             by = c("rasterid", "groupnr"))

# kable(sel_qual_bis %>%
#         dplyr::select(1:3,7,5:6,4,8) %>%
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
#  filter(rankclus <= maxrank ) %>% 
  group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
  summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
  ungroup %>% 
  inner_join(sel_qual_bis,
             by = c("rasterid", "groupnr", "gew_aantal_meetptn")) %>% 
  filter(beschikbaar_aantal == gew_aantal_meetptn) %>% 
  dplyr::select(-beschikbaar_aantal, -maxrank)

#kable(sel_cat2_table_bis ) %>% 
#   dplyr::select(1:3,7,5:6,4) %>%
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

sel_cat2_raster_bis <- sel_raster_pb %>% 
  semi_join(sel_cat2_table_bis,
            by = c("rasterid", "groupnr")) %>% 
  arrange(rasterid, groupnr)


sel_cat2_tm_bis <- raster_meetnet_poly_tm + 
  tm_shape(sel_cat2_raster) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.2: rastercellen met een juist voldoende aantal peilbuizen" )

sel_cat2_tm_bis


# de bijhorende geselecteerde Watina-meetpunten zijn dan
tubes_cat2_bis <- 
  tubes_qual_basis_bis %>% 
  # filter(selectie >= 0) %>% 
  inner_join(sel_qual_basis_bis) %>% #toevoeging is nodig om onderscheid te maken/behouden tussen de clusters
  inner_join(sel_cat2_table_bis)

tubes_cat2_bis <- tubes_cat2_bis %>% inner_join(tubes_in_raster) 

output_vc <- write_vc(tubes_cat2_bis, file.path(".","data","processed", "meetnet","tubes_cat2_bis"), 
                      sorting = c("loc_code"), strict =  FALSE, root = ".")

rm(output_vc)


### Categorie 3: Rastercellen met een overschot aan geschikte peilbuizen {#cat3}


# rastercellen met een overschot aan evenwaardige meetpunten in vergelijking met het aantal dat gewenst is voor het meetnet
sel_cat3_raster_bis <- 
  sel_raster_meetnet %>% 
  anti_join(sel_cat1A_raster %>% 
              st_drop_geometry(), 
            by = c("rasterid", "groupnr"))  %>% 
  anti_join(sel_cat2_raster_bis %>% 
              st_drop_geometry(), 
            by = c("rasterid", "groupnr")) %>% 
  arrange(rasterid, groupnr)


sel_cat3_table_bis <- 
  sel_qual_bis %>% 
  #filter(rankclus <= maxrank ) %>% 
  group_by(rasterid, groupnr, gew_aantal_meetptn) %>% 
  summarise(beschikbaar_aantal = sum(beschikbaar_aantal_cluster)) %>% 
  ungroup %>% 
  inner_join(sel_qual_bis, by = c("rasterid", "groupnr", "gew_aantal_meetptn") ) %>% 
  filter(beschikbaar_aantal > gew_aantal_meetptn, rankclus <= maxrank) %>% 
  dplyr::select(-beschikbaar_aantal, -maxrank)  


# kable(sel_cat3_table_bis %>% 
#         dplyr::select(1:3,7,5:6,4) %>% 
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
  tm_shape(sel_cat3_raster) + 
  tm_polygons(c("groupnr"), title = "GT-groepnr", style = "cat", legend.is.portrait = FALSE, palette = viridisLite::plasma(aantal_strat)) + tm_layout(title = "cat.3: rastercellen met een overschot aan geschikte peilbuizen" )

sel_cat3_tm_bis

#### Opzoeken van peilbuizen voor de rastercellen van cat. 3


#bijhorende Watina-meetpunten ervan opzoeken
tubes_cat3_bis <- 
  tubes_qual_basis_bis %>% 
  # filter(selectie >= 0) %>% 
  inner_join(sel_qual_basis_bis) %>% 
  inner_join(sel_cat3_table_bis)

tubes_cat3_bis <- tubes_cat3_bis %>% 
  rownames_to_column("unieknr") %>% 
  mutate(unieknr = as.integer(unieknr))
# sel_qual_lastyear_vb %>% 
#   count(rasterid, groupnr )



output_vc <- write_vc(tubes_cat3_bis, file.path(".","data","processed", "meetnet","tubes_cat3_bis"), 
                      sorting = c("loc_code"), strict =  FALSE, root = ".")

rm(output_vc)


## Moeten nog bijkomende meetreeksen geanalyseerd worden?


if (nrow(tubes_cat3_bis %>% filter(selectie == 0)) > 0 | nrow(tubes_cat2_bis %>% filter(selectie == 0)) > 0){
  message("Er zijn nog enkele meetreeksen die best met Menyanthes onderzocht worden.")
  nognietklaar <- TRUE
} else{
  message("Er hoeven geen bijkomende meetreeksen met Menyanthes onderzocht te worden.")
  nognietklaar <- FALSE  
}

if (nognietklaar == TRUE) {
  # kable(tubes_cat3_bis %>% 
  #         filter (selectie == 0) %>% 
  #         mutate(ser_nryears = ifelse(is.na(ser_length), NA, ser_nryears),
  #                ser_firstyear = ifelse(is.na(ser_length), NA, ser_firstyear),
  #                ser_lastyear = ifelse(is.na(ser_length), NA, ser_lastyear)
  #         ) %>% 
  #         dplyr::select(loc_code, ser_length, ser_nryears, ser_firstyear,
  #                ser_lastyear) %>% 
  #         rename(watinacode = loc_code,
  #                'lengte tijdreeks' = ser_length,
  #                'aantal lg3' = ser_nryears,
  #                beginjaar = ser_firstyear,
  #                eindjaar = ser_lastyear
  #         ) ,
  #       caption =  "Watina-meetpunten die behoren tot rastercellen van cat. 3"
  # ) %>% 
  #   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
  #                 full_width = T, 
  #                 position = "left",
  #                 font_size = 8, 
  #                 fixed_thead = T) %>%
  #   # column_spec(1:3, bold = F, border_right = F, width = "35em") %>%
  #   # column_spec(2, width = "30em", background = "yellow")
  #   #¬row_spec(0, angle = -90)  %>% #fixeer veldnamen
  #   scroll_box(height = "250px", box_css = "border: 1px solid #ddd; padding: 5px; margin: 5px;") 
}


## Aanduiden van de meetlocaties


### Het toewijzen van een kandidaat peilbuis aan een meetlocatie

#inlezen grts-raster level 1 (hoogste resolutie = kleinste gridcelgrootte)
# grts_level0 <- read_GRTSmh(brick = TRUE) %>% 
#   raster::subset(1)

# if (file.exists(file.path(".","data","local", "grts_level0.tif")) == FALSE | params$refresh_data >= 1) {
#   drive_download(drive_get(id = "1oNxe-MITpIVF2BFczLGLXcr0jT-LIWVB"), 
#                  path = file.path(".","data","local", "grts_level0.tif"), 
#                  overwrite = TRUE)
# }
if (params$refresh_data < 1) {
  grts_level0 <- raster(file.path(".","data","local", "grts_level0.tif"))
  grts_level5 <- raster(file.path(".","data","local", "grts_level5.tif"))
  grts_level8 <- raster(file.path(".","data","local", "grts_level8.tif"))
}

#inlezen grts-raster level 9 (resolutie = raster_meetnet_poly), het heeft een gridgrootte van 8192 m, let wel de rastercelgrootte is ook hier 32 bij 32m, dus het aantal rastercellen = grts-raster level 1. 
# grts_level8 <- read_GRTSmh(brick = TRUE) %>% 
#   raster::subset(9)

# if (file.exists(file.path(".","data","local", "grts_level8.tif")) == FALSE | params$refresh_data >= 1) {
#   drive_download(drive_get(id = "1oJpmNqlYoN3z8ICOlZZSlV0JXoUlcU5n"), 
#                  path = file.path(".","data","local", "grts_level8.tif"), 
#                  overwrite = TRUE)
# }
# 
# grts_level8 <- raster(file.path(".","data","local", "grts_level8.tif"))
# 
# 
# if (file.exists(file.path(".","data","local", "grts_level5.tif")) == FALSE | params$refresh_data >= 1) {
#   drive_download(drive_get(id = "1pBI9hTIaRpV_qfExYHLdxiFEl81OQPP8"), 
#                  path = file.path(".","data","local", "grts_level5.tif"), 
#                  overwrite = TRUE)
# }
# 
# grts_level5 <- raster(file.path(".","data","local", "grts_level5.tif"))


tubes_excess <- tubes_in_raster %>% 
  inner_join(tubes_cat3_bis %>% 
               # mutate(cat = if_else(is.na(ser_length),1, 3)) %>%  
               dplyr::select(loc_code, selectie, gew_aantal_meetptn)
             , by = "loc_code") 
# 
# tubes_excess <- tubes_in_raster %>% 
#   inner_join(tubes_cat3_bis %>% 
#                mutate(cat = 3) %>%  
#                dplyr::select(loc_code, cat, gew_aantal_meetptn) %>% 
#                bind_rows(tubes_cat1Bb %>% 
#                            mutate(cat = 1) %>%  
#                            dplyr::select(loc_code, cat, gew_aantal_meetptn)
#                )
#              , by = "loc_code") 

#toevoegen van een uniek rijnummer
tubes_excess <- rownames_to_column(tubes_excess, "unieknr") %>% 
  mutate(unieknr = as.integer(unieknr))

tubes_excess_sf <- as_points(tubes_excess)


tubes_excess_tm <- raster_meetnet_poly_tm + 
  tm_shape(tubes_excess_sf %>% 
             mutate(cat = factor(if_else(selectie == -1, 1, 3)))) + 
  tm_symbols(size = 0.25, shapes.labels = "loc_code", col = "cat", clustering = FALSE) + tm_layout(title = "beschikbare peilbuizen voor selectie" )

tubes_excess_tm

# sel_excess <- tubes_cat1Bb %>% 
#   dplyr::select(rasterid, groupnr) %>% 
#   distinct() %>% 
#   bind_rows(sel_cat3_table_bis %>% 
#               dplyr::select(rasterid, groupnr)) %>%
#   arrange(rasterid, groupnr) %>% 
#   distinct()
# 
# sel_excess <- tubes_cat1Bb %>% 
#   dplyr::select(rasterid, groupnr, gew_aantal_meetptn) %>% 
#   distinct() %>% 
#   bind_rows(sel_cat3_table_bis %>% 
#               dplyr::select(rasterid, groupnr, rest_aantal_meetptn) %>%
#               rename(gew_aantal_meetptn = rest_aantal_meetptn)) %>% 
#   arrange(rasterid, groupnr, gew_aantal_meetptn) %>% 
#   distinct()

sel_excess <- sel_cat3_table_bis %>% 
  dplyr::select(rasterid, groupnr, gew_aantal_meetptn) %>%
  arrange(rasterid, groupnr, gew_aantal_meetptn) %>% 
  distinct()

sel_excess_rasterid <- sel_excess %>% 
  distinct(rasterid)

tubes_excess$geselecteerd <- 0
tubes_excess$reserve <- 0

for (i in seq(1:nrow(sel_excess_rasterid))) {
  #i <- 17
  rasterid_grid <- sel_excess_rasterid[i,] %>% as.integer()
  clip8 <- grts_level8[grts_level8 == rasterid_grid, drop =  FALSE]
  clip0 <- grts_level0[clip8, drop =  FALSE]
  
  tubes_excess_1grid <- tubes_excess %>% 
    filter(rasterid == rasterid_grid) %>% 
    count(groupnr, gew_aantal_meetptn) %>% 
    rename(aantalpb = n)
  
  print(rasterid_grid)
  
  # plot(tubes_excess_level0)
  for (j in seq(1:nrow(tubes_excess_1grid))) {
    # ophalen van bijhorend aantal gewenste meetpunten en gw-groep, want een rastercel kan meerdere gw-groepen hebben waar er een overtal is (en het gewenste aantal meetpunten is specifiek per gw-groep)
    #j <-2
    gewenst_aantal_pb <- tubes_excess_1grid %>% 
      slice(j) %>% 
      dplyr::pull(gew_aantal_meetptn) %>% 
      as.integer()
    gwgroup <- tubes_excess_1grid %>% 
      slice(j) %>%
      dplyr::pull(groupnr) %>% 
      as.integer()
    aantalpb <- tubes_excess_1grid %>% 
      slice(j) %>%
      dplyr::pull(aantalpb) %>% 
      as.integer()
    
    # check <- tubes_excess %>% 
    #   group_by(rasterid, groupnr) %>% 
    #   filter(selectie == 1) %>% 
    #   summarise( goeieptn = n())
    # 
    # check_alleptn <- tubes_excess %>% 
    #   group_by(rasterid, groupnr) %>% 
    #   summarise( alleptn = n())   
    # 
    # check_alleptn <- check_alleptn %>% 
    #   left_join(check)
    #groep pb indien mogelijk beperken tot alleen de geschikte. 
    aantalgoedepb <- tubes_excess %>% 
      filter(rasterid == rasterid_grid & selectie == 1 & groupnr == gwgroup) %>% 
      count(gew_aantal_meetptn) %>% 
      dplyr::pull(n) %>% 
      as.integer()
    geen_grts_nodig <- FALSE
    if (purrr::is_empty(aantalgoedepb)) {
      tubes_toselect <- tubes_excess 
    } else if (aantalgoedepb >= gewenst_aantal_pb) {
      if (aantalgoedepb == gewenst_aantal_pb) {
        geen_grts_nodig <- TRUE
        tubes_toselect <- tubes_excess %>% 
          filter(selectie == 1)
      } else {
        tubes_toselect <- tubes_excess %>% 
          filter(selectie == 1)
      }
    } else {
      tubes_toselect <- tubes_excess 
    }
    tubes_toselect <- tubes_toselect %>% 
      filter(rasterid == rasterid_grid & groupnr == gwgroup)
    
    if (geen_grts_nodig == FALSE) {
      
      #binnen een deelraster (clip0), alleen de rastercellen van level0 selecteren waarbinnen een pb valt. 
      #De andere rastercellen worden NA
      tubes_excess_level0 <- 
        raster::rasterize(tubes_toselect %>% 
                            dplyr::select(x, y) %>% 
                            as.matrix(), 
                          y = clip0, #raster-object
                          mask = TRUE)
      
      #rangorde bepalen van de grts-nrs van de geselecteerde rastercellen 
      rank_cells_level0 <- tubes_excess_level0 %>% 
        raster::getValues() %>% 
        as.data.frame()
      names(rank_cells_level0) <- "celwaarde" 
      rank_cells_level0 <- rank_cells_level0 %>% 
        filter(!is.na(celwaarde)) %>%  
        distinct() %>% 
        mutate(minrank = min_rank(celwaarde)) %>% 
        arrange(minrank) 
      
      # rank_cells_level0
      
      # raster met grts-nrs herindexeren. Een 1-waarde stemt overeen met een gewenste peilbuis (voor dat grid), de overige nummer(s) zijn de reservepunt(en).
      # Hiervoor moet er eerst een n*2 matrix (rcl) gemaakt worden met de oude en nieuwe celwaarde.
      rcl <- data.frame("grtsnr" = rank_cells_level0 %>% dplyr::pull(celwaarde), 
                        "selectie" = 
                          c(rep(1,gewenst_aantal_pb), seq(from = gewenst_aantal_pb + 1, to = nrow(rank_cells_level0)))) %>% 
        as.matrix()
      
      #herindexeren
      tubes_excess_level0_rcl <- raster::reclassify(tubes_excess_level0, rcl)
      
      #raster maken met de unieke nummers van de pb, maar dat enkel voor het gewenste aantal pb
      tubes_excess_level0_unieknr <- 
        raster::rasterize(tubes_excess %>% 
                            filter(groupnr == gwgroup) %>% 
                            dplyr::select(x, y) %>% 
                            as.matrix(),
                          tubes_excess_level0_rcl[tubes_excess_level0_rcl == 1, drop = FALSE],
                          field = tubes_excess %>% 
                            filter(groupnr == gwgroup) %>%
                            dplyr::select(unieknr), 
                          mask = FALSE)
      
      #ophalen van de unieke nummers
      tubes_excess_selected_unieknr <- tubes_excess_level0_unieknr %>% 
        raster::getValues() %>% 
        as.data.frame()
      names(tubes_excess_selected_unieknr) <- "unieknr" 
      tubes_excess_selected_unieknr <- tubes_excess_selected_unieknr %>% 
        filter(!is.na(unieknr)) %>%  
        distinct() %>% 
        arrange(unieknr) %>% 
        dplyr::pull(unieknr)
      
      #peilbuis als geselecteerd markeren
      tubes_excess[tubes_excess$unieknr %in% tubes_excess_selected_unieknr, "geselecteerd"] <- 1
    } else {
      tubes_excess[tubes_excess$unieknr %in% tubes_toselect$unieknr, "geselecteerd"] <- 1      
    }
    #idem maar nu voor de reservepunten. Dit kan ik niet in één keer, gelukkig duurt het niet lang.
    #raster maken met de unieke nummers van de pb, maar dat enkel voor de reservepunten
    #enkel indien het aantal pb (voldoende in rang) > aantal gewenst
    if (aantalpb > gewenst_aantal_pb) {
      for (k in seq(from = (gewenst_aantal_pb + 1), to = nrow(rank_cells_level0))) {
        tubes_excess_level0_unieknr <- 
          raster::rasterize(tubes_excess %>% 
                              filter(groupnr == gwgroup) %>% 
                              dplyr::select(x, y) %>% 
                              as.matrix(),
                            tubes_excess_level0_rcl[tubes_excess_level0_rcl == k, drop = FALSE],
                            field = tubes_excess %>% 
                              filter(groupnr == gwgroup) %>%
                              dplyr::select(unieknr), 
                            mask = FALSE)
        
        #ophalen van de unieke nummers
        tubes_excess_selected_unieknr <- tubes_excess_level0_unieknr %>% 
          raster::getValues() %>% 
          as.data.frame()
        names(tubes_excess_selected_unieknr) <- "unieknr" 
        tubes_excess_selected_unieknr <- tubes_excess_selected_unieknr %>% 
          filter(!is.na(unieknr)) %>%  
          distinct() %>% 
          arrange(unieknr) %>% 
          dplyr::pull(unieknr)
        
        #peilbuis als geselecteerd markeren
        tubes_excess[tubes_excess$unieknr %in% tubes_excess_selected_unieknr, "reserve"] <- k
      }#loop reservepunten
    } #controle aantal reservepunten
  } #loop gw-groepen
} # loop gridcellen
#aanduiden welke pb geselecteerd zijn
tubes_cat3_grts <- tubes_excess %>% 
  filter(geselecteerd == 1) %>% 
  arrange(rasterid, groupnr, loc_code)

tubes_cat3_grts_reserve <- tubes_excess %>% 
  filter(reserve > 0 ) %>% 
  arrange(rasterid, groupnr, reserve)

# tubes_cat1Bb_gtrs <- tubes_excess %>% 
#   filter(geselecteerd == 1 & cat == 1) %>% 
#   arrange(rasterid, groupnr, loc_code)


# tubes_cat1Bb_gtrs_reserve <- tubes_excess %>% 
#   filter(reserve > 0 & cat == 1) %>% 
#   arrange(rasterid, groupnr, reserve)

#wegschrijven van het resultaat, omdat de berekening hiervan toch wel enkele minuten tijd vraagt.
output_vc <- write_vc(tubes_cat3_grts, file.path(".","data","processed", "meetnet","tubes_cat3_grts"), sorting = c("rasterid","groupnr", "loc_code"), strict =  FALSE)
# output_vc <- write_vc(tubes_cat1Bb_gtrs, file.path(".","data","tubes_cat1Bb_gtrs"), sorting = c("rasterid","groupnr", "loc_code"), strict =  FALSE)
output_vc <- write_vc(tubes_cat3_grts_reserve, file.path(".","data","processed", "meetnet","tubes_cat3_grts_reserve"), sorting = c("rasterid","groupnr", "reserve"), strict =  FALSE)
# output_vc <- write_vc(tubes_cat1Bb_gtrs_reserve, file.path(".","data","tubes_cat1Bb_gtrs_reserve"), sorting = c("rasterid","groupnr", "reserve"), strict =  FALSE)
rm(output_vc)

#tubes_cat3_grts <- read_vc(file.path(".","data","tubes_cat3_grts")) %>% as_tibble()
#tubes_cat3_grts_reserve <- read_vc(file.path(".","data","tubes_cat3_grts_reserve"))


###Samenvatting selectie


tubes_selected <- bind_rows(tubes_cat1 %>% mutate(cat = "1"), tubes_cat2_bis %>% mutate(cat = "2"), tubes_cat3_grts %>% mutate(cat = "3"))
tubes_selected <- tubes_selected %>% 
  dplyr::select(-selectie) %>% 
  left_join(tubes_eval_namenyanthes %>% 
              dplyr::select(loc_code, selectie), by = "loc_code")


tubes_reserve <- bind_rows(tubes_cat3_grts_reserve %>% mutate(cat = "3")
)
tubes_reserve <- tubes_reserve %>% 
  dplyr::select(-selectie) %>% 
  left_join(tubes_eval_namenyanthes %>% 
              dplyr::select(loc_code, selectie), by = "loc_code")

output_vc <- write_vc(tubes_selected, file.path(".","data","result", "meetnet","tubes_selected"), sorting = c("loc_code"), strict =  FALSE)
output_vc <- write_vc(tubes_reserve, file.path(".","data","result", "meetnet","tubes_reserve"), sorting = c("loc_code"), strict =  FALSE)
rm(output_vc)

tubes_selected_sf <- as_points(tubes_selected)

tubes_selected_tm <- raster_meetnet_poly_tm + 
  tm_shape(tubes_selected_sf %>% mutate(Inzetbaarheid = factor(if_else(selectie == -1, "potentieel", "actueel")))) + 
  tm_symbols(size = 0.25, shapes.labels = "loc_code", col = "Inzetbaarheid", clustering = FALSE) + tm_layout(title = "geselecteerde peilbuizen" )

tubes_selected_tm
write_sf(tubes_selected_sf %>%  dplyr::select(-obswell_statecode, -loc_validitycode) %>% rename (watinac = "loc_code"), driver = "ESRI Shapefile", file.path("data", "result", "meetnet", "GIS","tubes_selected_2020_05.shp"))
write_sf(tubes_reserve %>% as_points() %>%  dplyr::select(-obswell_statecode, -loc_validitycode) %>% rename (watinac = "loc_code"), file.path("data", "result", "meetnet", "GIS","tubes_reserve_2020_05.shp" ))


#overzicht tot welke habitattypen of habitattype-groepen de geselecteerde punten behoren
types <- 
  read_types(lang = "nl") %>% 
  select(type, type_name, type_shortname, typeclass_name)

 tubes_selected <- git2rdata::read_vc(file.path(".","data","result","meetnet","tubes_selected"))
# all.equal(tubes_selected, tubes_selected_old)
# 
# check <- tubes_selected %>% 
#   select(loc_code, nieuwesel = selectie) %>% 
#   full_join(tubes_selected_old %>% 
#               select(loc_code, oudesel = selectie), by = "loc_code" ) %>% 
#   mutate (vergelijking= ifelse(nieuwesel == oudesel, "ok", "verschil"))

tubes_hab <- read_vc(file.path(".","data","tubes_hab"))

tubes_selected_types <- 
  tubes_selected %>% 
  as_tibble %>% 
  # filter(selectie == 1) %>% 
  select(loc_code, x, y, selectie, 
         target_groupnr = groupnr) %>% 
  left_join(tubes_hab %>% 
              select(loc_code, 
                     description,
                     type, 
                     certain, 
                     phab, 
                     groupnr,
                     group_name = typegroup_name), 
            by = "loc_code") %>% 
  left_join(types, by = "type") %>% 
  distinct()

write_vc(tubes_selected_types, file.path(".","data","processed", "meetnet","tubes_selected_types"), sorting = c("loc_code", "type", "phab", "description"), strict =  FALSE)

tubes_selected_types_stats_present <- tubes_selected_types %>% 
  filter(selectie == 1) %>% 
  inner_join(tubes_selected_types %>% 
               distinct(loc_code, description) %>% 
               group_by(loc_code) %>% 
               summarise(tubes_herh = n()), by = "loc_code") %>% 
  inner_join(tubes_selected_types %>% 
               inner_join(tubes_selected_types %>% 
                            distinct(loc_code, description) %>% 
                            group_by(loc_code) %>% 
                            summarise(tubes_herh = n()), by = "loc_code") %>% 
               group_by(target_groupnr) %>% 
               summarise(somaandeel_groep = sum(phab/tubes_herh)) %>% ungroup(), by = "target_groupnr") %>%   
  group_by(target_groupnr, groupnr, type, type_shortname) %>% 
  summarise(aandeel = round(sum(phab/somaandeel_groep/tubes_herh)*100)) %>% 
  ungroup()

write_vc(tubes_selected_types_stats_present, file.path(".","data","processed", "meetnet","tubes_selected_types_stats_present"), sorting = c("target_groupnr","type"), strict =  FALSE)

tubes_selected_types_stats_future <- tubes_selected_types %>% 
  inner_join(tubes_selected_types %>% 
               distinct(loc_code, description) %>% 
               group_by(loc_code) %>% 
               summarise(tubes_herh = n()), by = "loc_code") %>% 
  inner_join(tubes_selected_types %>% 
               inner_join(tubes_selected_types %>% 
                            distinct(loc_code, description) %>% 
                            group_by(loc_code) %>% 
                            summarise(tubes_herh = n()), by = "loc_code") %>% 
               group_by(target_groupnr) %>% 
               summarise(somaandeel_groep = sum(phab/tubes_herh)) %>% ungroup(), by = "target_groupnr") %>%   
  group_by(target_groupnr, groupnr, type, type_shortname) %>% 
  summarise(aandeel = round(sum(phab/somaandeel_groep/tubes_herh)*100)) %>% 
  ungroup()

write_vc(tubes_selected_types_stats_future, file.path(".","data","processed", "meetnet","tubes_selected_types_stats_future"), sorting = c("target_groupnr","type"), strict =  FALSE)

tubes_selected_typeclasses_stats_present <- tubes_selected_types %>% 
  filter(selectie == 1) %>% 
  inner_join(tubes_selected_types %>% 
               distinct(loc_code, description) %>% 
               group_by(loc_code) %>% 
               summarise(tubes_herh = n()), by = "loc_code") %>% 
  inner_join(tubes_selected_types %>% 
               inner_join(tubes_selected_types %>% 
                            distinct(loc_code, description) %>% 
                            group_by(loc_code) %>% 
                            summarise(tubes_herh = n()), by = "loc_code") %>% 
               group_by(target_groupnr) %>% 
               summarise(somaandeel_groep = sum(phab/tubes_herh)) %>% ungroup(), by = "target_groupnr") %>%   
  group_by(target_groupnr, habitatgroep = typeclass_name) %>% 
  summarise(aandeel = round(sum(phab/somaandeel_groep/tubes_herh)*100)) %>% 
  ungroup() %>% 
  arrange(target_groupnr, desc(aandeel))


write_vc(tubes_selected_typeclasses_stats_present, file.path(".","data","processed", "meetnet","tubes_selected_typeclasses_stats_present"), sorting = c("target_groupnr","habitatgroep"), strict =  FALSE)

tubes_selected_typeclasses_stats_future <- tubes_selected_types %>% 
  inner_join(tubes_selected_types %>% 
               distinct(loc_code, description) %>% 
               group_by(loc_code) %>% 
               summarise(tubes_herh = n()), by = "loc_code") %>% 
  inner_join(tubes_selected_types %>% 
               inner_join(tubes_selected_types %>% 
                            distinct(loc_code, description) %>% 
                            group_by(loc_code) %>% 
                            summarise(tubes_herh = n()), by = "loc_code") %>% 
               group_by(target_groupnr) %>% 
               summarise(somaandeel_groep = sum(phab/tubes_herh)) %>% ungroup(), by = "target_groupnr") %>%   
  group_by(target_groupnr, habitatgroep = typeclass_name) %>% 
  summarise(aandeel = round(sum(phab/somaandeel_groep/tubes_herh)*100)) %>% 
  ungroup() %>% 
  arrange(target_groupnr, desc(aandeel))


write_vc(tubes_selected_typeclasses_stats_future, file.path(".","data","processed", "meetnet","tubes_selected_typeclasses_stats_future"), sorting = c("target_groupnr","habitatgroep"), strict =  FALSE)

###Selecteren van potentiële geschikte habitatvlekken voor gridcellen waarvoor nu geen pb'en bestaan.


#verrasteren van de habitatkaart met de grondwatergroep als waarde
# grts_level5 <- read_GRTSmh(brick = TRUE) %>% 
#   raster::subset(6)

reserve <- habmap_polygons_gw

# habmap_polygons_gw <- habmap_polygons_gw %>% 
#   rownames_to_column(var = "unieknr") %>% 
#   mutate(unieknr = as.integer(unieknr),
#          selecteerbaar = 1) 

#wat voorbereidende databewerkingen aan de overlay habitatkaart en het raster (grid8)
# habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>% dplyr::select(-unieknr)
habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>%
  rownames_to_column(var = "unieknr") %>%
  mutate(unieknr = as.integer(unieknr))

habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>% 
  mutate(selecteerbaar = 1, 
         selecteerbaar_reserve = 1) 

#overzicht van rasterid-GT-groep waarbinnen moet gezocht worden
sel_cat1A_raster_gw_df <- sel_cat1A_raster %>% 
  st_drop_geometry() %>% 
  dplyr::select(-opp_gw_cel) %>% 
  arrange(rasterid, groupnr) %>% 
  distinct()

#sel_cat1A_raster_gw_df is praktisch hetzelfde als sel_cat1A_table

#overzicht van alleen de rastercellen
sel_cat1A_raster_df <- sel_cat1A_raster_gw_df %>% 
  dplyr::select(-groupnr) %>% 
  distinct

# loop per rastercel
for (i in seq(1:nrow(sel_cat1A_raster_df))) {
  
  rasterid_grid <- sel_cat1A_raster_df[i, "rasterid"] %>% 
    as.integer()
  
  sel_cat1A_raster_1grid <- sel_cat1A_raster_gw_df %>% 
    filter(rasterid == rasterid_grid) %>% 
    mutate(n_tubes = replace_na(n_tubes,0),
           gewenst_aantal_pb = gew_aantal_meetptn - n_tubes)
  
  
  clip8 <- grts_level8[grts_level8 == rasterid_grid, drop =  FALSE] #uitsnede van het 8km-raster
  clip0 <- grts_level0[clip8, drop =  FALSE] #uitsnede van het 32m raster
  clip5 <- grts_level5[clip8, drop =  FALSE] #uitsnede van het 1024m-raster
  
  #loop per gGT-groep
  for (j in seq(1:nrow(sel_cat1A_raster_1grid))) {
    
    gewenst_aantal_pb <- sel_cat1A_raster_1grid [j,] %>% 
      dplyr::pull(gewenst_aantal_pb) %>% 
      as.integer()
    
    gwgroup <- sel_cat1A_raster_1grid [j,] %>% 
      dplyr::pull(groupnr) %>% 
      as.integer()
    
    # raster met binnen het grid (level8) de rangnummers (GRTS) van de habitatpolygonen die behoren tot een bep. grondwatergroep
    habmap_raster_rangnr <- raster::rasterize(habmap_gw_raster_overlay %>% 
                                                filter(groupnr == gwgroup & selecteerbaar == 1),
                                              clip0, 
                                              mask = TRUE)
    
    #opzoeken van de laagste rangnummer(s) grid level 0(hoogste resolutie)
    grid_rangnr <- habmap_raster_rangnr %>% 
      raster::getValues() %>% 
      as.data.frame()
    names(grid_rangnr) <- "celwaarde" 
    grid_rangnr <- grid_rangnr %>% 
      filter(!is.na(celwaarde)) %>%  
      distinct() %>% 
      arrange(celwaarde) 
    
    # raster met grts-nrs herindexeren naar 0 en 1 waarden. Het aantal 1 waarden stemt overeen met het gewenst aantal peilbuizen (voor dat grid).
    # Hiervoor moet er eerst een n*2 matrix (rcl) gemaakt worden met de oude en nieuwe celwaarde.
    rcl <- data.frame("grtsnr" = grid_rangnr %>% dplyr::pull(celwaarde), 
                      "selectie" = 
                        c(rep(1,gewenst_aantal_pb), rep(0,nrow(grid_rangnr) - gewenst_aantal_pb))) %>% 
      as.matrix()
    
    #herindexeren
    habmap_raster_rcl <- raster::reclassify(habmap_raster_rangnr, rcl)
    
    # raster met binnen het grid (level8) alle unieke nrs van de habitatpolygonen die behoren tot een bep. grondwatergroep
    habmap_raster_unieknr <- raster::rasterize(habmap_gw_raster_overlay %>%
                                                 filter(groupnr == gwgroup & selecteerbaar == 1),
                                               clip0,
                                               field = "unieknr",
                                               mask = FALSE)
    # habmap_raster_unieknr[habmap_raster_rangnr[habmap_raster_rangnr == 46054, drop = FALSE], drop= FALSE]
    
    #raster maken met de unieke nummers van de pb, maar dat enkel voor het gewenste aantal
    
    habmap_raster_unieknr_select <- habmap_raster_unieknr * habmap_raster_rcl[habmap_raster_rcl == 1, drop = FALSE]
    
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
    habmap_gw_raster_overlay <- 
      habmap_gw_raster_overlay %>% 
      left_join(habmap_raster_unieknr_select_df, 
                by = "unieknr") %>% 
      mutate(selecteerbaar = ifelse(is.na(geselecteerd), selecteerbaar, 0)) %>% 
      dplyr::select(-geselecteerd)
    
    
    #opzoeken reservepunten
    #ophalen van bijhorende rangnr's van level5 (+/- 1km-hok)
    clip5_select <- clip5 * habmap_raster_rcl[habmap_raster_rcl == 1, 
                                              drop = FALSE]
    
    
    #selectie van de geselecteerde rangnr(s)
    clip5_select_df <- clip5_select %>% 
      raster::getValues() %>% 
      as.data.frame()
    names(clip5_select_df) <- "celwaarde"  
    clip5_select_df <- 
      clip5_select_df %>% 
      filter(!is.na(celwaarde)) %>% 
      distinct %>% 
      arrange(celwaarde) %>% 
      mutate(selectie =  1)
    
    #opstellen van een clip-raster op basis van de geselecteerde rangnr(s)
    #dit door een reclassering te doen: alle cellen van grid5 worden 0, behalve die met de geselecteerde celwaarde
    #alle celwaarden opzoeken binnen grid8
    clip5_range <- clip5 %>% 
      raster::getValues() %>% 
      as.data.frame()
    names(clip5_range) <- "celwaarde"  
    clip5_range <- clip5_range %>% distinct %>% arrange(celwaarde)
    
    #opbouw van reclas
    clip5_rcl <- clip5_range %>% 
      left_join(clip5_select_df) %>% 
      arrange(desc(selectie), celwaarde)
    
    rcl <- data.frame("grtsnr" = clip5_rcl %>% dplyr::pull(celwaarde), 
                      "selectie" = 
                        c(rep(1,gewenst_aantal_pb), 
                          rep(0,nrow(clip5_rcl) - gewenst_aantal_pb))) %>% 
      as.matrix()
    
    habmap_raster_reserve_rcl <- raster::reclassify(clip5, rcl)
    habmap_raster_unieknr_reserve <- 
      habmap_raster_unieknr * habmap_raster_reserve_rcl
    
    
    #ophalen van unieke nummers
    #selectie van de geselecteerde rangnr(s)
    habmap_raster_unieknr_reserve_df <- habmap_raster_unieknr_reserve %>% 
      raster::getValues() %>% 
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
      dplyr::select(-geselecteerd)
  } #loop grondwatergroup
} #loop grid

tubes_cat1_polyg <-
  habmap_gw_raster_overlay %>% 
  rename(geselecteerd_basis = selecteerbaar,
         geselecteerd_reserve = selecteerbaar_reserve) %>% 
  mutate(geselecteerd_basis = ifelse(geselecteerd_basis == 0,1,0),
         geselecteerd_reserve = ifelse(geselecteerd_reserve == 0,1,0)
  ) %>% 
  filter(geselecteerd_basis == 1 | geselecteerd_reserve == 1) %>% 
  arrange(rasterid, type, polygon_id) 

# tubes_cat1_polyg <- tubes_cat1_polyg %>% 
#   dplyr::select(-source.y) %>% 
#   rename(source = source.x)

habmap_gw_raster_overlay <- habmap_gw_raster_overlay %>% 
  dplyr::select(-starts_with("selecteerbaar"))

#wegschrijven van het resultaat, omdat de berekening hiervan toch wel enkele minuten tijd vraagt.
output_vc <- write_vc(tubes_cat1_polyg %>% st_drop_geometry(), file.path(".","data","processed", "meetnet","tubes_cat1_polyg"), sorting = c("rasterid","type", "polygon_id"), strict =  FALSE)
rm(output_vc)
write_sf(tubes_cat1_polyg, file.path("data", "result", "meetnet", "GIS", "bijkomende_hokken.shp"))

# raster::plot(clip5)
# raster::plot(clip5_1cel, add= FALSE)
# 
# check <- tubes_excess_gw_raster_overlay %>% 
#   dplyr::select(loc_code, rasterid, rasterid.1)
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
# tubes_excess_raster0 <- rasterize(tubes_excess %>% dplyr::select(x, y) %>% as.matrix(), test, mask = TRUE)
# tubes_excess_unieknr <- rasterize(tubes_excess %>% dplyr::select(x, y) %>% as.matrix(), test, field = tubes_excess %>% dplyr::select(unieknr), mask = FALSE)
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
# t <- tubes_excess %>% dplyr::select(rasterid) %>% as.character()
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
# clipjb <- test2[rasterize(tubes_excess %>% dplyr::select(x, y) %>% as.matrix(), test2, field = tubes_excess %>% dplyr::pull(loc_code))]
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


tubes_cat1_polyg_tm <- raster_meetnet_poly_tm + 
  tm_shape(tubes_cat1_polyg %>% filter (geselecteerd_basis ==1)) + 
  tm_fill(col = "groupnr", palette = "BuGn", title = "Grondwatertype") + 
  tm_layout(title = "Meetpunten van categorie 1 waar nog een peilbuis nodig is" )

tubes_cat1_polyg_tm <- raster_meetnet_poly_tm + 
  tm_shape(tubes_cat1_polyg %>% filter(geselecteerd_basis == 1)) + 
  tm_bubbles(size =  0.5, col = "groupnr", style = "cat", palette = "BuGn", title.col = "Grondwatertype" ) +
  tm_layout(title = "Meetpunten van categorie 1 waar nog een peilbuis nodig is", outer.margins = outermargins_tm )

tubes_cat1_polyg_tm
str(tubes_selected)
tubes_current <- tubes_selected %>% 
  as_tibble() %>% 
  select(-n) %>% #n uitsluiten, want de count-functie loopt hierdoor mis
  filter(selectie == 1) %>% 
  count(groupnr) 

tubes_current
tubes_current_GT <- gw_types_groups %>% 
  rename(gew_aantal = 'Gewenst aantal meetlocaties',
         groupnr = 'GT-groep: nummer') %>% 
  left_join(tubes_current, by = c("groupnr")) %>% 
  mutate (act_inzetbaar = ifelse(gew_aantal > 0, round(n/gew_aantal*100), NA)) %>% 
  mutate(group2 = ifelse(groupnr ==1,2,groupnr)) %>% 
  group_by(group2) %>% 
  mutate(gew_aantal2 = sum(gew_aantal),
         n2 = sum(n)) %>% 
  ungroup %>% 
  mutate (act_inzetbaar2 = ifelse(gew_aantal2 > 0, round(n2/gew_aantal2*100), NA))



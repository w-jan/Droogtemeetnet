library(tidyverse)
library(odbc)
library(DBI)
library (git2rdata)
library (lubridate)
library(MASS)
library(car)
library(lme4)
library(INLA)

#test
con <- dbConnect(odbc::odbc(), .connection_string = "Driver=SQL Server;Server=inbo-sql07-prd.inbo.be,1433;Database=D0136_00_Flaven;Trusted_Connection=Yes;")
ruwedata19 <- dbReadTable(con, "droogte_tijdreeks19")
ruwedata20 <- dbReadTable(con, "droogte_tijdreeks20")
ruwedata01 <- dbReadTable(con, "droogte_tijdreeks1")
write_vc(ruwedata23, file.path("data", "local", "ruwedata23"), sorting = c("dag","meetpunt_import"), strict= FALSE)

test <- read_vc(file.path("data", "local", "ruwdata01"))
start_time <- Sys.time()
test2 <- read_vc(file.path("data", "local", "ruwedata"))
end_time01 <- Sys.time()
test2b <- dbReadTable(con, "droogte_tijdreeks19")
end_time02 <- Sys.time()
tijd_VC <- end_time01 - start_time
tijd_SQL <- end_time02 - end_time01
#het gaat 3xsneller om lokaal een vc-tabel in te lezen, dan de brontabel op sql te gaan halen. Het vergt ook geen VPN-verbinding

#inlezen data van de sql-server  
ruwetabellen_lijst <- data.frame (a = "droogte_tijdreeks", b = "ruwedata", c = seq(from = 1, to = 19))

ruwetabellen_lijst <- ruwetabellen_lijst %>% mutate (naambrontabel = paste0(a,c), 
                                                     naamdoeltabel = sprintf(paste0(b,"%02d"), c)) 

ruwetabellen_lijst <- setNames(ruwetabellen_lijst %>% dplyr::pull(naambrontabel), make.names(ruwetabellen_lijst %>% dplyr::pull(naamdoeltabel)))


list2env(
  lapply(ruwetabellen_lijst, 
         dbReadTable, conn = con,
         guess_max = 300000), 
  envir = .GlobalEnv)

DBI::dbDisconnect(con)


#wegschrijven ruwedata naar tsv-tabellen   
for (i in seq(from = 1, to = 19)){
  write_vc(get(sprintf("ruwedata%02d", i)), file.path("data", "local", sprintf("ruwedata%02d", i)), sorting = c("dag","meetpunt_import"), strict= FALSE)
}

#inlezen ruwedata van tsv-tabellen   
ruwetabellen_lijst <- data.frame(bron = "ruwedata", doel = "ruwedata", c = seq(from = 1, to = 23))

ruwetabellen_lijst <- ruwetabellen_lijst %>% mutate (naambrontabel = file.path("data", "local", paste0(bron,sprintf("%02d", c))), 
                                                     naamdoeltabel = sprintf(paste0(doel,"%02d"), c)) 

ruwetabellen_lijst <- setNames(ruwetabellen_lijst %>% dplyr::pull(naambrontabel), make.names(ruwetabellen_lijst %>% dplyr::pull(naamdoeltabel)))

list2env(
  lapply(ruwetabellen_lijst, 
         read_vc), 
  envir = .GlobalEnv)


#data samenvoegen
for (i in seq (from = 1, to = 23)){
  if (i == 1) {
    ruwedata <- get(sprintf("ruwedata%02d",i))
  } else {
    ruwedata <- bind_rows(ruwedata, get(sprintf("ruwedata%02d",i)))
  }
}

# check <- ruwedata %>% filter(meetpunt == "LAVP080")
tubes_selected <- read_vc(file.path(".","data","tubes_selected"))

tubes_indicator <- tubes_selected %>% 
  filter(selectie == 1) %>% 
  dplyr::select(loc_code, groupnr)

#linken van de metingen aan een gw-groep
tubes_in_raster <- read_vc(file.path(".","data","tubes_in_raster"))
tubes_gw <- tubes_in_raster %>% 
  dplyr::select(loc_code, groupnr)
ruwedata <- ruwedata %>% 
  left_join(tubes_gw, by = c("meetpunt" = "loc_code")) #left_join om te testen

check <- ruwedata %>% 
  filter(is.na(groupnr))
summary(ruwedata)
checkpb <- ruwedata_test %>% 
  distinct(meetpunt)
checkpb <- checkpb %>% 
  mutate(in_indicator = 1)

check_cross <- tubes_selected %>% 
  full_join(checkpb, by= c("loc_code" = "meetpunt"))

check_cross_nieuwepb <- check_cross %>% 
  filter(selectie == 1 & is.na(in_indicator))

#verwijderen van oude geselecteerde pb uit de ruwe data
ruwedata <- ruwedata %>% 
  semi_join(tubes_selected %>% filter(selectie == 1), by = c("meetpunt" = "loc_code"))

#verwijderen van de 20 deelbestanden (beter nog niet doen als je wil testen, het is handig om te testen met één van de bestanden, bijv. ruwedata01 (rel. groot) of ruwedata19 (rel klein)

rm(list = sprintf("ruwedata%02d", seq(from = 1, to = 21)))


ruwedate <-  ruwedata %>% 
  mutate(jaar = year(dag),
         meetpunt = factor(meetpunt)) %>% 
  filter(meetpunt_import != meetreeks, jaar >= 1985) %>%  #uitsluiten van niet gesimuleerde meetreeksen (veldmetingen zitten namelijk in de simuleerde reeksen)
  dplyr::select(-meetpunt_import, -meetreeks, -is_veldmeting)


MyStd <- function(x) { (x - mean(x)) / sd(x)}
unscale <- function(x, m, s) {
    x * s + m
  }

# ruwedata <- ruwedata
absperc <- ruwedata %>% 
  filter(between(jaar,1985,2015)) %>% 
  group_by(meetpunt, jaar) %>% 
  summarise(p01 = quantile(meting_TAW, 1/100),
            p05 = quantile(meting_TAW, 5/100),
            p10 = quantile(meting_TAW, 10/100),
            p30 = quantile(meting_TAW, 30/100),
            p50 = quantile(meting_TAW, 30/100),
            p70 = quantile(meting_TAW, 30/100),
            p90 = quantile(meting_TAW, 30/100),
            p95 = quantile(meting_TAW, 30/100),
            p99 = quantile(meting_TAW, 30/100)
            ) %>% 
  group_by(meetpunt) %>% 
  summarise(p01 = mean(p01),
            p05 = mean(p05),
            p10 = mean(p10),
            p30 = mean(p30),
            p50 = mean(p50),
            p70 = mean(p70),
            p90 = mean(p90),
            p95 = mean(p95),
            p99 = mean(p99)
            ) %>%   
  ungroup()

#herkijking grondwatertype-groepen, want er blijkt nog nauwelijks een verband te bestaan tussen de amplitude van een pb en de gw-typegroep waartoe het wordt gerekend
absperc_gw <- absperc %>% 
  inner_join(tubes_indicator %>% 
               mutate(groep3 = ifelse(groupnr == 1, 2, groupnr)) %>% 
               dplyr::select(-groupnr), by = c("meetpunt" = "loc_code")) %>% 
  mutate(ampl = p99 - p01,
         groep3n = case_when(
           ampl < 0.25 ~2,
           ampl < 0.4 ~3,
           TRUE ~4),
         meetpunt = factor(meetpunt)
         )

write_vc(absperc_gw, file.path("data", "result", "percentielen_1985_2015"), sorting = c("meetpunt"), strict = FALSE)

schrikkeljaar <- ruwedata %>% 
  filter(meetpunt == 
           ruwedata %>% 
           dplyr::select(meetpunt) %>% 
           head(1) %>% 
           dplyr::pull(meetpunt)
         ) %>% 
  filter(day(dag) == 29, month(dag) == 2) %>% 
  mutate(jaar = year(dag)) %>% 
  distinct(jaar) %>% 
  arrange(jaar) %>% 
  dplyr::pull(jaar)


#vrijmaken geheugenruimte !
  gc()

    # cumulatieve som
# indic_cum <-  ruwedata %>% 
#   inner_join(absperc, by = "meetpunt") %>% 
#   mutate(jaar = year(dag)) %>% 
#   filter(meetpunt_import != meetpunt) %>% 
#   mutate( lengte_onder_p01 = if_else(meting_TAW <= p01,p01-meting_TAW,0),
#           lengte_onder_p05 = if_else(meting_TAW <= p05,p05-meting_TAW,0),
#           lengte_onder_p10 = if_else(meting_TAW <= p10,p10-meting_TAW,0),
#           lengte_onder_p30 = if_else(meting_TAW <= p30,p30-meting_TAW,0),
#           lengte_onder_p01_log = log(lengte_onder_p01+1),
#           lengte_onder_p05_log = log(lengte_onder_p05+1),
#           lengte_onder_p10_log = log(lengte_onder_p10+1),
#           lengte_onder_p30_log = log(lengte_onder_p30+1)
#   ) %>% 
#     group_by(meetpunt, simulatienr,jaar) %>% 
#     summarise_at(vars(lengte_onder_p01: lengte_onder_p30_log), sum) %>% 
#     mutate(jaar_scale = scale(jaar)) %>% 
#     group_by(meetpunt, jaar) %>% 
#     summarise(gemlengte_onder_p01 = mean(lengte_onder_p01),
#               gemlengte_onder_p05 = mean(lengte_onder_p05),
#               gemlengte_onder_p10 = mean(lengte_onder_p10),
#               gemlengte_onder_p30 = mean(lengte_onder_p30),
#               # ogdag_onder_p01_exact = qchisq(0.025, 2*gemdag_onder_p01)/2,
#               # bgdag_onder_p01_exact = qchisq(0.975, 2*(gemdag_onder_p01+1))/2,
#               # ogdag_onder_p01_normaal = gemdag_onder_p01 -1.96 * sqrt(gemdag_onder_p01/20),
#               # bgdag_onder_p01_normaal = gemdag_onder_p01 +1.96 * sqrt(gemdag_onder_p01/20),
#               og_onder_p01 = gemlengte_onder_p01 - 1.96 * (sd(lengte_onder_p01)/19),
#               bg_onder_p01 = gemlengte_onder_p01 + 1.96 * (sd(lengte_onder_p01)/19),
#               og_onder_p05 = gemlengte_onder_p05 - 1.96 * (sd(lengte_onder_p05)/19),
#               bg_onder_p05 = gemlengte_onder_p05 + 1.96 * (sd(lengte_onder_p05)/19),
#               og_onder_p10 = gemlengte_onder_p10 - 1.96 * (sd(lengte_onder_p10)/19),
#               bg_onder_p10 = gemlengte_onder_p10 + 1.96 * (sd(lengte_onder_p10)/19),
#               og_onder_p30 = gemlengte_onder_p30 - 1.96 * (sd(lengte_onder_p30)/19),
#               bg_onder_p30 = gemlengte_onder_p30 + 1.96 * (sd(lengte_onder_p30)/19)
#     ) %>% 
#     group_by(jaar) %>% 
#     summarise(glob_gemlengte_p01 = mean(gemlengte_onder_p01),
#               glob_gemlengte_p05 = mean(gemlengte_onder_p05),
#               glob_gemlengte_p10 = mean(gemlengte_onder_p10),
#               glob_gemlengte_p30 = mean(gemlengte_onder_p30),
#               glob_gemlengte_p01_log = log(glob_gemlengte_p01+1),
#               glob_gemlengte_p05_log = log(glob_gemlengte_p05+1),
#               glob_gemlengte_p10_log = log(glob_gemlengte_p10+1),
#               glob_gemlengte_p30_log = log(glob_gemlengte_p30+1),
#               glob_og_onder_p01 = glob_gemlengte_p01 - 1.96 * (sd(gemlengte_onder_p01)/19),
#               glob_bg_onder_p01 = glob_gemlengte_p01 + 1.96 * (sd(gemlengte_onder_p01)/19),
#               glob_og_onder_p05 = glob_gemlengte_p05 - 1.96 * (sd(gemlengte_onder_p05)/19),
#               glob_bg_onder_p05 = glob_gemlengte_p05 + 1.96 * (sd(gemlengte_onder_p05)/19),
#               glob_og_onder_p10 = glob_gemlengte_p10 - 1.96 * (sd(gemlengte_onder_p10)/19),
#               glob_bg_onder_p10 = glob_gemlengte_p10 + 1.96 * (sd(gemlengte_onder_p10)/19),
#               glob_og_onder_p30 = glob_gemlengte_p30 - 1.96 * (sd(gemlengte_onder_p30)/19),
#               glob_bg_onder_p30 = glob_gemlengte_p30 + 1.96 * (sd(gemlengte_onder_p30)/19)              
#     )

indic_cum_basis <-  ruwedata %>% 
  inner_join(absperc_gw, by = "meetpunt") %>% 
  mutate( lengte_onder_p01 = if_else(meting_TAW <= p01,p01-meting_TAW,0),
          lengte_onder_p05 = if_else(meting_TAW <= p05,p05-meting_TAW,0),
          lengte_onder_p10 = if_else(meting_TAW <= p10,p10-meting_TAW,0),
          lengte_onder_p30 = if_else(meting_TAW <= p30,p30-meting_TAW,0)
  ) %>% 
  group_by(meetpunt, simulatienr,jaar, groep3, groep3n) %>% 
  summarise_at(vars(lengte_onder_p01: lengte_onder_p30), sum) %>% 
  ungroup() %>% 
  mutate(jaar_factor = factor(jaar)
         )
indic_cum_basis <- indic_cum_basis %>% filter (simulatienr > 0) %>% 
  mutate(lengte_onder_p01 = if_else(lengte_onder_p01 == 0, 1e-16, lengte_onder_p01),
         lengte_onder_p05 = if_else(lengte_onder_p05 == 0, 1e-16, lengte_onder_p05),
         lengte_onder_p10 = if_else(lengte_onder_p10 == 0, 1e-16, lengte_onder_p10),
         lengte_onder_p30 = if_else(lengte_onder_p30 == 0, 1e-16, lengte_onder_p30)
         ) %>% 
  mutate(lengte_onder_p01_std = MyStd(lengte_onder_p01),
         lengte_onder_p05_std = MyStd(lengte_onder_p05),
         lengte_onder_p10_std = MyStd(lengte_onder_p10),
         lengte_onder_p30_std = MyStd(lengte_onder_p30)
         )
  
indic_cum_basis <- indic_cum_basis %>% 
  filter(simulatienr <= 20, jaar < 2019)

# check <- indic_cum_basis %>% 
#   group_by(meetpunt, jaar, simulatienr) %>% 
#   count(lengte_onder_p01)
# 
# check <- indic_cum_basis %>% distinct(jaar)
# checkdistributie <- indic_cum_basis[indic_cum_basis$jaar == 2003 & indic_cum_basis$meetpunt == "ALMP003",]$lengte_onder_p30
checkdistributie <- indic_cum_basis %>% 
  filter(jaar == 2018, meetpunt == "DYLP029") %>% 
  dplyr::select(!!varcum)
# nbinom <- fitdistr(checkdistributie, "Negative Binomial")
# qqp(checkdistributie, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
# poisson <- fitdistr(checkdistributie, "Poisson")
# qqp(checkdistributie, "pois", lambda = poisson$estimate)
 #binom <- fitdistrplus::fitdist(checkdistributie %>% dplyr::pull(1), "binom", fix.arg = list(size = 365), start = list(prob = 0.1))
#summary(binom)
#qqp(checkdistributie %>% dplyr::pull(1), "binom", size = 365, prob = binom$estimate)
qqp(checkdistributie %>% dplyr::pull(1), "norm")
cgamma <- fitdistr(checkdistributie %>% dplyr::pull(1), "Gamma")
qqp(checkdistributie %>% dplyr::pull(1), "gamma", shape = cgamma$estimate[[1]], rate = cgamma$estimate[[2]])
qqp(checkdistributie %>% dplyr::pull(1), "lnorm")
exponent <- fitdistr(checkdistributie %>% dplyr::pull(1), "exponential")
qqp(checkdistributie %>% dplyr::pull(1), "exp", exponent$estimate[[1]])

#voor p01: gamma of normaal, gamma iets beter
#voor p05: gamma of normaal, gamma iets beter
#voor p10: gamma of normaal, gamma iets beter
#voor p30: gamma of normaal

#gamma distributie en normaal-distributie zijn voor p30 gelijkwaardig, met gamma lichtjes beter, voor p05 is gamma duidelijk beter   
#voor p01 kan er vaak geen gamma-distributie berekend worden
#conclusie: best gamma, indien mogelijk, zoniet normaal
#  qchisq(0.025, 2*x)/2, qchisq(0.975, 2*(x+1))/2   

# PQL <- glmmPQL(lengte_onder_p05 ~ jaar_factor, ~1|meetpunt, family = Gamma(link = "inverse"), data = indic_cum_basis[indic_cum_basis$simulatienr == 1,], verbose = FALSE)
# summary(PQL)

# Bij het toepassen van de gamma-verdeling voor p01 crasht inla.exe
# i <- 1
# mdata <- indic_cum_basis[indic_cum_basis$simulatienr == i,]
# respons <- "lengte_onder_p05"
# prec.prior <- list(prec = list(param = c(0.001, 0.001)))
# I1b <- inla(lengte_onder_p05 ~ f(jaar, model = "rw1", scale.model = TRUE, hyper = prec.prior) + f(meetpunt, model = "iid"),
#            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
#            family = "gamma",
#            control.family = list(
#              link = "log",
#              hyper = list(prec = list(
#                prior = "loggamma",
#                param = c(1, 0.5)))),
#            data = mdata,
#            verbose = FALSE
# )
# summary(I1b)
# 
# f1 <- lengte_onder_p05 ~ f(jaar, model = "rw1", scale.model = TRUE, hyper = prec.prior) + f(meetpunt, model = "iid", hyper = prec.prior)
# I1 <- inla(f1, 
#            control.predictor = list(compute = TRUE),
#            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
#            family = "gamma",
#            data = indic_cum_basis[indic_cum_basis$simulatienr == 1,]
# )
# summary(I1)
# 
#   #vergelijken modellen
# 
#   sum(log(I1$cpo$cpo))
#   sum(log(I1b$cpo$cpo))
#   # sum(log(I2c$cpo$cpo))
#   # sum(log(I2_untr_kleineu$cpo$cpo))
#   # sum(log(I2_binom$cpo$cpo))
# 
#   sum(I1$dic$dic)
#   sum(I1b$dic$dic)
#   # sum(log(I2c$dic$dic))
#   # sum(log(I2_untr_kleineu$dic$dic))
# 
#   sum(I1$waic$waic)
#   sum(I1b$waic$waic)
#   # sum(log(I2c$waic$waic))
#   # sum(log(I2_untr_kleineu$waic$waic))
# 
#   sum(I1$mlik)
#   sum(I1b$mlik)
#   # sum(I2c$mlik)
#   # sum(I2_binom$mlik)
# 
#   #bekijk de gefitte waarden van het model
#   # Plot the fitted values
#   Fit1     <- I1$summary.fitted.values[,"mean"]
#   Fit1.025 <- I1$summary.fitted.values$"0.025quant"
#   Fit1.975 <- I1$summary.fitted.values$"0.975quant"
#   
#   check <- I1$summary.random$meetpunt
#   
#   result_fitted_i 
#   mdata
#   mdata$Fitted1  <- Fit1
#   mdata$Fit1.025 <- Fit1.025
#   mdata$Fit1.975 <- Fit1.975
#   #gdata <- mdata %>% dplyr::select(!!respons, jaar, contains("Fit"))
#   p <- ggplot(data = mdata, aes_string(y = respons, x = "jaar"))
#   p <- p + xlab("Jaar") + ylab(respons)
#   p <- p + theme(text = element_text(size=15)) 
#   p <- p + geom_point(shape = 16, size = 2, col = "black")
#   p <- p + geom_line(aes(x = jaar, y = Fitted1))
#   p <- p + geom_ribbon(data = mdata, aes(x = jaar, 
#                                          ymax = Fit1.975, 
#                                          ymin = Fit1.025),
#                        fill = grey(0.5),
#                        alpha = 0.4)
#   p <- p + theme(strip.text = element_text(size = 15))
#   p
#   
#   # Get Pearson residuals voor gamma
#   mu1  <- I1$summary.fitted.values[,"mean"] 
#   r    <- I1$summary.hyperpar[1,"mean"]
#   VarY <- mu1^2 / r 
#   E1   <- (mdata %>% dplyr::pull(!!respons) - mu1) / sqrt(VarY)
#   # Pearson residuals
#   par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
#   plot(x = Pi,
#        y = E1,
#        xlab = "Fitted values",
#        ylab = "Pearson residuals")
#   abline(h = 0, lty = 2)
#   
#   
#   summary(I2)
#   result_stat_i <-  I1$summary.random$jaar %>% 
#     mutate(simulatienr = i)
#   result_stat_i <- result_stat_i %>%  rename (p0.025 = '0.025quant', 
#                                               p975.5 = '0.975quant',
#                                               jaar =  ID)
#   gplot <- ggplot(data = result_stat_i, aes(x = jaar, y = mean)) + 
#     geom_line(aes(x = jaar, y = p0.025), linetype = "longdash") +
#     geom_ribbon(aes(x = jaar, ymax = p975.5, ymin = p0.025)) +  
#     geom_line(aes(x = jaar, y = p975.5), linetype = "longdash") +
#     geom_line(color = "lightblue") +
#     #geom_point(data = mdata, aes_string(x = "jaar", y = respons)) +
#     geom_hline(aes(yintercept = 0), linetype = "dotted") +
#     labs(x = "Jaar", y = "trend")
#   gplot
# 
# 
# 
# 
# i <- 1
#   indic_cum_p05_jaar_stat_i <- I1$summary.random$jaar %>%
#     mutate(simulatienr = i)
#   indic_cum_p05_jaar_stat_i <- indic_cum_p05_jaar_stat_i %>%
#     mutate(og_berekend = mean - 1.96 * sd,
#             bg_berekend = mean + 1.96 * sd)
# 
#   indic_cum_p05_fitted_i <-  I1$summary.fitted.values %>%
#     rename(p05_mean_fitted = mean,
#            p05_sd_fitted = sd,
#            p05_p02.5_fitted = '0.025quant',
#            p05_p97.5_fitted = '0.975quant') %>%
#     mutate( p05_mean_fitted_untr = unscale(p05_mean_fitted,m,s),
#             p05_p02.5_fitted_untr = unscale(p05_p02.5_fitted,m,s),
#             p05_p97.5_fitted_untr = unscale(p05_p97.5_fitted,m,s)
#           )
#   if (i == 1) {
#    # indic_cum_p05_jaar_stat <- indic_cum_p05_jaar_stat_i
#     #indic_cum_p05_fitted <- indic_cum_p05_fitted_i
#   } else {
#     indic_cum_p05_jaar_stat <- bind_rows(indic_cum_p05_jaar_stat,
#                                          indic_cum_p05_jaar_stat_i)
#     indic_cum_p05_fitted <- bind_rows(indic_cum_p05_fitted, indic_cum_p05_fitted_i)
#   }
# 
# 
#   indic_cum_p05_jaar_stat_i <- indic_cum_p05_jaar_stat_i %>%
#   rename( jaar = ID,
#           p02.5 = '0.025quant',
#           p50 = '0.5quant',
#           p97.5 = '0.975quant')


  # indic_cum_p05_jaar_stat_i <- indic_cum_p05_jaar_stat_i %>%
  # mutate(mean_untr = unscale(mean, m, s),
  #        og_berekend_untr = unscale(og_berekend, m, s),
  #        bg_berekend_untr = unscale(bg_berekend, m, s),
  #        p02.5_untr = unscale(p02.5, m, s),
  #        p97.5_untr = unscale(p97.5, m, s),
  #        se_berekend_untr = ((p97.5_untr  - mean_untr )/1.96 - (p02.5_untr  - mean_untr )/1.96)/2,
  #        )
  #plot cum indicator p05-waarden
  # gplot <- ggplot(data = indic_cum_p05_jaar_stat_i[indic_cum_p05_jaar_stat_i$simulatienr == 1,], aes(x = jaar, y = mean)) + 
  #   geom_line(aes(x = jaar, y = p02.5), linetype = "longdash") +
  #   geom_ribbon(aes(x = jaar, ymax = p97.5, ymin = p02.5)) +  
  #   geom_line(aes(x = jaar, y = p97.5), linetype = "longdash") +
  #   geom_line(color = "red") +
  #   geom_point(data = indic_cum_basis[indic_cum_basis$simulatienr == 1,], aes(x = jaar, y = lengte_onder_p05)) +
  #   geom_hline(aes(yintercept = 0), linetype = "dotted") +
  #   labs(x = "Jaar", y = "trend en geobserveerde waarden")
  # gplot
  
  #vreemd het gamma-model pikt geen signaal op, scale.model = TRUE helpt een beetje, maar ook hier klopt er nog iets mee
# m <- mean(indic_cum_basis$lengte_onder_p05)
# s <- sd(indic_cum_basis$lengte_onder_p05)
# 
# 
# for (i in seq(from = 1, to = 20)) {
#   #i <- 1
#   I2 <- inla(lengte_onder_p05_std ~ 1 + f(jaar, model = "rw1")+ f(meetpunt, model = "iid"), 
#              control.compute = list(dic = TRUE),
#              family = "gaussian",
#              data = indic_cum_basis[indic_cum_basis$simulatienr == i,]
#   )
#   #summary(I2)
#   indic_cum_p05_jaar_stat_i <- I2$summary.random$jaar %>% 
#     mutate(simulatienr = i)
#   indic_cum_p05_jaar_stat_i <- indic_cum_p05_jaar_stat_i %>% 
#     mutate(og_berekend = mean - 1.96 * sd,
#             bg_berekend = mean + 1.96 * sd)
# 
#   indic_cum_p05_fitted_i <-  I2$summary.fitted.values %>% 
#     rename(p05_mean_fitted = mean,
#            p05_sd_fitted = sd,
#            p05_p02.5_fitted = '0.025quant',
#            p05_p97.5_fitted = '0.975quant') %>% 
#     mutate( p05_mean_fitted_untr = unscale(p05_mean_fitted,m,s),
#             p05_p02.5_fitted_untr = unscale(p05_p02.5_fitted,m,s),
#             p05_p97.5_fitted_untr = unscale(p05_p97.5_fitted,m,s)    
#           )
#   if (i == 1) {
#     indic_cum_p05_jaar_stat <- indic_cum_p05_jaar_stat_i
#     indic_cum_p05_fitted <- indic_cum_p05_fitted_i
#   } else {
#     indic_cum_p05_jaar_stat <- bind_rows(indic_cum_p05_jaar_stat, 
#                                          indic_cum_p05_jaar_stat_i)
#     indic_cum_p05_fitted <- bind_rows(indic_cum_p05_fitted, indic_cum_p05_fitted_i)
#   }
# }
# 
# indic_cum_p05_jaar_stat <- indic_cum_p05_jaar_stat %>% 
#   rename( jaar = ID,
#           p02.5 = '0.025quant',
#           p50 = '0.5quant',
#           p97.5 = '0.975quant')
# 
# 
# indic_cum_p05_jaar_stat <- indic_cum_p05_jaar_stat %>% 
#   mutate(mean_untr = unscale(mean, m, s),
#          og_berekend_untr = unscale(og_berekend, m, s),
#          bg_berekend_untr = unscale(bg_berekend, m, s),
#          p02.5_untr = unscale(p02.5, m, s),
#          p97.5_untr = unscale(p97.5, m, s),
#          se_berekend_untr = ((p97.5_untr  - mean_untr )/1.96 - (p02.5_untr  - mean_untr )/1.96)/2,
#          )
# 
# indic_cum_basis <- bind_cols(indic_cum_basis,indic_cum_p05_fitted %>% 
#                                     dplyr::select(contains("fitted")))


  
#plot van de modelfitting
plotfitting <- function(indic_basis, respons, gemid, og, bg) {
  og <- enquo(og)
  bg <- enquo(bg)
  PI_data <- indic_basis %>% group_by(jaar) %>% 
    summarise(ymax_data = max(!! bg, na.rm = TRUE), ymin_data = min(!! og, na.rm = TRUE))
  p <- ggplot(data = indic_basis, aes_string(y = respons, x = "jaar"))
  p <- p + xlab("Jaar") + ylab(respons)
  p <- p + theme(text = element_text(size = 15))
  p <- p + geom_point(shape = 16, size = 2, col = "black")
  p <- p + geom_line(aes_string(x = "jaar", y = gemid), size = 1, color = "red")
  p <- p + geom_ribbon(data = PI_data,
                       aes(x = jaar, ymax = ymax_data, ymin = ymin_data), inherit.aes = F,
                       fill = grey(0.5),
                       alpha = 0.4)
  p <- p + scale_x_continuous(breaks = 1985:2020, 
                              labels = insert_minor(seq(1985, 2020, by = 5), 4))
  p <- p + theme(strip.text = element_text(size = 15))
  return(p)
}

#plot van de trend (zowel modelschatting als berekende)
plottrend <- function(indic_finaal, trend_berekend, intercept) {
  p <- ggplot(data = indic_finaal) + 
    geom_line(aes(x = jaar, y = og_jaar), linetype = "longdash") +
    geom_ribbon(aes(x = jaar, ymax = bg_jaar, ymin = og_jaar),
                fill = grey(0.5),
                alpha = 0.4) +  
    geom_line(aes(x = jaar, y = bg_jaar), linetype = "longdash") +
    geom_line(aes(x = jaar, y = gem_jaar), color = "dark blue") +
    geom_line(aes_string(x = "jaar", y = trend_berekend), color = "red") +
    # geom_point(data = indic_cum_basis, aes(x = jaar, y = lengte_onder_p01)) +
    geom_hline(aes(yintercept = intercept), linetype = "dotted") +
    scale_x_continuous(breaks = 1985:2020, labels = insert_minor(seq(1985, 2020, by = 5), 4)) +
    labs(x = "Jaar", y = "trend")
  return(p)
}

insert_minor <- function(major_labs, n_minor) {
  labs <- c( sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
  labs[1:(length(labs) - n_minor)]}
  
indic_cum_basis <- indic_cum_basis %>% 
  dplyr::select(-contains("fitted") )


modeldata <- indic_cum_basis
modelkeuze <- data.frame(percentiel = c("01", "05", "10", "30" ), model = c("gamma", "gamma", "gamma", "gamma"))
modelkeuze$model <- as.character(modelkeuze$model)

indic_cum_function <- function(modeldata, respons, percentile, indicatorname, standardised) {
  

  # indicator <- modeldata %>% 
  #   dplyr::pull(!!indicatorname)
  # 
  # m <- mean(indicator)
  # s <- sd(indicator)
  prec.prior <- list(prec = list(param = c(0.001, 0.001)))
  model <- as.formula(paste(respons, "~", "1 + f(jaar, model =", "'rw1', scale.model = FALSE,
                            hyper = prec.prior)+ f(meetpunt, model = 'iid', hyper = prec.prior)", sep = " "))   
  
  resultname_stat <- paste0("indic_cum_p", percentile,"_jaar_stat", if (standardised == TRUE) ("_std"))
  resultname_fitted <- paste0("indic_cum_p", percentile,"_fitted", if (standardised == TRUE) ("_std"))
  if (percentile == "01") {
    reeks <- c(1, 3, 7:8, 11:12, 17:18) #12 modellen wilde maar niet convergeren
  } else {
    reeks <- c(1:20)    
  }
  teller <- 0
  for (i in reeks) {
    #i <- 18
   # i <- 20
    teller <- teller + 1
    print(i)
    mdata <- modeldata[modeldata$simulatienr == i,]    
    #mdata1 <- modeldata[modeldata$simulatienr == 1,]  
    #summary(mdata1)
    I1 <- inla(model, 
               control.compute = list(dic = TRUE),
               family = modelkeuze %>% filter(percentiel == percentile) %>% 
                 dplyr::pull(model), 
               data = mdata,
               #control.inla = list(strategy = "gaussian", int.strategy = "eb"),
               verbose = FALSE
    )
    #summary(I1)
    # sum(log(I1$cpo$cpo)) 
    # sum(log(I1b$cpo$cpo))
    # sum(log(I1c$cpo$cpo))
    # sum(log(I1d$cpo$cpo))
    # sum(log(I1_binom$cpo$cpo)) 
    # 
    # sum(log(I1$dic$dic))    
    # sum(log(I1b$dic$dic))  
    # sum(log(I1c$dic$dic))  
    # sum(log(I1d$dic$dic))  
    # 
    # sum(log(I1b$waic$waic))    
    # sum(log(I1$waic$waic))    
    # sum(log(I1c$waic$waic))
    # sum(log(I1d$waic$waic))
    # 
    # sum(I1$mlik)   
    # sum(I1b$mlik)    
    # sum(I1c$mlik)
    # sum(I1d$mlik)
    
    # names(inla.models()$likelihood)
    
    # # Assess overdispersion (variantie / aantal vrijheidsgraden ~ 1)
    # #voor binomiaal
    # Pi   <- I1_binom$summary.fitted.values[,"mean"]
    # ExpY <- Pi * mdata$aantaldagen_jaar
    # VarY <- Pi * mdata$aantaldagen_jaar * (1 - Pi)
    # E1   <- (mdata %>%
    #               dplyr::pull(!!indicatorname) - ExpY) / sqrt(VarY)
    # N    <- nrow(mdata)
    # p <- nrow(I1_binom$summary.fixed)
    # Dispersion <- sum(E1^2) / (N - p)
    # Dispersion
    
    # #voor poisson
    # Pi   <- I1_binom$summary.fitted.values[,"mean"]
    # ExpY <- Pi #* mdata$aantaldagen_jaar
    # VarY <- Pi #* mdata$aantaldagen_jaar * (1 - Pi)
    # E1   <- (mdata %>%
    #               dplyr::pull(!!indicatorname) - ExpY) / sqrt(VarY)
    # N    <- nrow(mdata)
    # p <- nrow(I1_binom$summary.fixed)
    # Dispersion <- sum(E1^2) / (N - p)
    # Dispersion
    
    # #voor negatief binomiaal
    # Pi  <- I1_untr$summary.fitted.values[,"mean"]
    # theta <- I1_untr$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>% 
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)
    # 
    # Pi  <- I1_untr_kleineu$summary.fitted.values[,"mean"]
    # theta <- I1_untr_kleineu$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>% 
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)    
    # 
    # 
    # 
    # Pi  <- I1_prior$summary.fitted.values[,"mean"]
    # theta <- I1_prior$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>% 
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)    
    # 
    # 
    # Pi  <- I1$summary.fitted.values[,"mean"]
    # theta <- I1$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>%
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)
    # 
    # # Pearson residuals
    # par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
    # plot(x = Pi,
    #      y = E1,
    #      xlab = "Fitted values",
    #      ylab = "Pearson residuals")
    # abline(h = 0, lty = 2)
    #
    # #names(inla.models()$likelihood)
    #
    #bekijk de gefitte waarden van het model
    # Plot the fitted values
    # Fit1     <- I1c$summary.fitted.values[,"mean"]
    # Fit1.025 <- I1c$summary.fitted.values$"0.025quant"
    # Fit1.975 <- I1c$summary.fitted.values$"0.975quant"
    # 
    # Fit1     <- I1$summary.fitted.values[,"mean"]
    # Fit1.025 <- I1$summary.fitted.values$"0.025quant"
    # Fit1.975 <- I1$summary.fitted.values$"0.975quant"
    # 
    # # 
    # # check <- I1$summary.random$meetpunt
    # # 
    # # result_fitted_i 
    # mdata
    # mdata$Fitted1  <- Fit1
    # mdata$Fit1.025 <- Fit1.025
    # mdata$Fit1.975 <- Fit1.975
    # #gdata <- mdata %>% dplyr::select(!!respons, jaar, contains("Fit"))
    # p <- ggplot(data = mdata, aes_string(y = respons, x = "jaar"))
    # p <- p + xlab("Jaar") + ylab(respons)
    # p <- p + theme(text = element_text(size=15))
    # p <- p + geom_point(shape = 16, size = 2, col = "black")
    # p <- p + geom_line(aes(x = jaar, y = Fitted1))
    # p <- p + geom_ribbon(data = mdata, aes(x = jaar,
    #                          ymax = Fit1.975,
    #                          ymin = Fit1.025),
    #                      fill = grey(0.5),
    #                      alpha = 0.4)
    # p <- p + theme(strip.text = element_text(size = 15))
    # p
    # summary(I1)
    # result_stat_i <-  I1d$summary.random$jaar %>%
    #   mutate(simulatienr = i)
    #  result_stat_i <- result_stat_i %>%  rename (p0.025 = '0.025quant',
    #                              p975.5 = '0.975quant',
    #                              jaar =  ID)
    #  gplot <- ggplot(data = result_stat_i, aes(x = jaar, y = mean)) + 
    #    geom_line(aes(x = jaar, y = p0.025), linetype = "longdash") +
    #    geom_ribbon(aes(x = jaar, ymax = p975.5, ymin = p0.025)) +  
    #    geom_line(aes(x = jaar, y = p975.5), linetype = "longdash") +
    #    geom_line(color = "lightblue") +
    #    #geom_point(data = mdata, aes_string(x = "jaar", y = respons)) +
    #    geom_hline(aes(yintercept = 0), linetype = "dotted") +
    #    labs(x = "Jaar", y = "trend")
    #  gplot
    # 
    # #conclusie negative binomiaal (bij 30 wel onderdispersed), gestandardiseerd met een gewijzigde non-informatieve prior
    # 
    # result_stat_i_meetpunt <-  I1$summary.random$meetpunt %>% 
    #   mutate(simulatienr = i)
    
    # result_stat_i <- result_stat_i %>% 
    #   mutate(og_berekend = mean - 1.96*sd,
    #          bg_berekend = mean + 1.96*sd)    
    
    
    #gamma-verdeling gebruikt de logit-link
    #om de niet-getransformeerde waarden te krijgen moet men zowel de intercept als de coëfficiënten exponentiëren. Exp(standaardfout) heeft geen zin, maar het was moeilijker om deze uit te sluiten dan ze (verkeerdelijk) mee te nemen
    result_stat_i <-  I1$summary.random$jaar %>% 
      mutate_at(names(I1$summary.random$jaar)[2:6], exp) %>%
      mutate_at(names(I1$summary.random$jaar)[2:6], function(x){x*exp(I1$summary.fixed$mean)}) %>%       mutate(simulatienr = i) %>% 
      dplyr::select(-sd, -mode, -kld)
    
    # result_stat_i <- result_stat_i %>% 
    #   mutate(og_berekend = mean - 1.96*sd,
    #           bg_berekend = mean + 1.96*sd)
    
    varname_mean <- paste0("p", percentile, "_mean", if (standardised == TRUE) ("_std"), "_fitted")
    varname_sd <- paste0("p", percentile, "_sd", if (standardised == TRUE) ("_std"), "_fitted")
    varname_p025 <- paste0("p", percentile, "_p02.5", if (standardised == TRUE) ("_std"), "_fitted")
    varname_p975 <- paste0("p", percentile, "_p97.5", if (standardised == TRUE) ("_std"), "_fitted")
    result_fitted_i <-  I1$summary.fitted.values %>% 
      rename(!!varname_mean := mean,
           !!varname_sd := sd,
           !!varname_p025 := '0.025quant',
           !!varname_p975 := '0.975quant')
    mdata <- bind_cols(mdata, result_fitted_i)   
    # if (standardised == TRUE) {
    #   varname_mean_backtransformed <- paste0(varname_mean, "_untr")
    #   varname_p025_backtransformed <- paste0(varname_p025, "_untr")
    #   varname_p975_backtransformed <- paste0(varname_p975, "_untr")
    #   result_fitted_i <- result_fitted_i %>% 
    #   mutate( !!varname_mean_backtransformed := unscale(result_fitted_i %>% 
    #                                                       dplyr::pull(!!varname_mean),m,s),
    #           !!varname_p025_backtransformed := unscale(result_fitted_i %>% 
    #                                                       dplyr::pull(!!varname_p025),m,s),
    #           !!varname_p975_backtransformed := unscale(result_fitted_i %>% 
    #                                                       dplyr::pull(!!varname_p975),m,s)    
    #     )
    # }

 
    if (i == 1) {
      result_stat <- result_stat_i
      result_fitted <- mdata
      
    } else {
      result_stat <- bind_rows(result_stat, 
                                           result_stat_i)
      result_fitted <- bind_rows(result_fitted, mdata)    
    }
  
  
    # if (standardised == TRUE) {
    #   result_stat <- result_stat %>% 
    #     mutate(mean_untr = unscale(mean, m, s),
    #            og_berekend_untr = unscale(og_berekend, m, s),
    #            bg_berekend_untr = unscale(bg_berekend, m, s),
    #            p02.5_untr = unscale(p02.5, m, s),
    #            p97.5_untr = unscale(p97.5, m, s),
    #            se_berekend_untr = ((p97.5_untr  - mean_untr )/1.96 - 
    #                                  (p02.5_untr  - mean_untr )/1.96)/2,
    #     )
    # }
  
    if (teller == length(reeks)) {
      result_stat <- result_stat %>% 
        rename( jaar = ID,
                p02.5 = '0.025quant',
                p50 = '0.5quant',
                p97.5 = '0.975quant')
      
      # if (standardised == TRUE) {
      #   result_stat <- result_stat %>% 
      #     mutate(mean_untr = unscale(mean, m, s),
      #            og_berekend_untr = unscale(og_berekend, m, s),
      #            bg_berekend_untr = unscale(bg_berekend, m, s),
      #            p02.5_untr = unscale(p02.5, m, s),
      #            p97.5_untr = unscale(p97.5, m, s),
      #            se_berekend_untr = ((p97.5_untr  - mean_untr )/1.96 - 
      #                                  (p02.5_untr  - mean_untr )/1.96)/2,
      #     )
      # }
      resultlijst <- list( result_stat, 
                           result_fitted)
      names(resultlijst) <- c(resultname_stat, resultname_fitted)
      list2env(resultlijst, envir = .GlobalEnv)
      
      if (varname_mean %in% colnames(indic_cum_basis)) {
        indic_cum_basis <- indic_cum_basis %>% 
          dplyr::select(-!!varname_mean, -!!varname_sd, -!!varname_p025, -!!varname_p975) 
      }
      
      indic_cum_basis <- indic_cum_basis %>% 
        inner_join(result_fitted %>% 
                     dplyr::select(contains("fitted"), 1:3), 
                   by = c("meetpunt","simulatienr", "jaar")) 
    }
  }
  return(indic_cum_basis)
}

#debugonce(indic_cum_function)
#prec.prior <- list(prec = list(param = c(0.001, 0.001))) #, hyper = prec.prior
#hyper = list(prec = list(param = c(10, 100)))
#model <- as.formula(paste(respons, "~", "1 + f(jaar, model =", "'rw1', scale.model = TRUE, hyper = list(prec = list(param = c(10, 100))))+ f(meetpunt, model = 'iid')", sep = " "))    

#indic_cum_basis_bu <- indic_cum_basis

percentile <- "01"
indicatorname <- paste0("lengte_onder_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))
respons <- indicatorname
#debugonce(indic_cum_function)
#vreemd voor '01' werkt de functie enkel individueel: de for-loop van simulaties loopt altijd ergens vast.
indic_cum_basis_01 <- indic_cum_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

percentile <- "05"
indicatorname <- paste0("lengte_onder_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))
respons <- indicatorname
indic_cum_basis <- indic_cum_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

percentile <- "10"
indicatorname <- paste0("lengte_onder_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))
respons <- indicatorname
#debugonce(indic_cum_function)
indic_cum_basis <- indic_cum_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

percentile <- "30"
indicatorname <- paste0("lengte_onder_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))
respons <- indicatorname
indic_cum_basis <- indic_cum_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

indic_cum_basis<- indic_cum_basis %>% 
  left_join(indic_cum_basis_01 %>% dplyr::select(1:3, starts_with("p01")), by = c("meetpunt", "simulatienr", "jaar"))

#bewaren resultaat
write_vc(indic_cum_basis, file.path("data", "result", "indic_cum_basis"), sorting = c("jaar", "meetpunt", "simulatienr"), strict = FALSE)

# Toepassen van formule Rubin 1987 voor de multiple imputaties door het Menyanthes-model

# Alg. gemiddelde  = gemiddelde van gemiddelde van elke simulatierun
# Alg. standaardfout = gemiddelde van standaardfout van elke simulatierun + L +1 / L * (Alg. gemiddelde - gemiddelde simulatierun)^2 / (L-1) met L het aantal simulaties

indic_cum_gem <- indic_cum_basis %>% 
  group_by(jaar) %>% 
  summarise_at(vars(lengte_onder_p01:lengte_onder_p30), list(~mean(.), ~median(.))) %>% 
  ungroup

indic_cum_p01_finaal <- indic_cum_p01_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (12 + 1)/12 * sum((gem_og - p02.5)^2) / (12 - 1), #7 simulatieruns convergeerden niet
            bg_jaar = gem_bg + 1.96 * (12 + 1)/12 * sum((gem_bg - p97.5)^2) / (12 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_cum_gem, by = "jaar")

#plot van de trend
#debugonce(plottrend)
gplot <- plottrend(indic_cum_p01_finaal,"lengte_onder_p01_mean", 0)
png(paste0(file.path("figures", "indicator", "lengte_onder_p01"),"_trend",".png"))
gplot
dev.off()

# debugonce("plotfitting")
fig <- plotfitting(indic_basis = indic_cum_basis, respons = "lengte_onder_p01", gemid = "p01_mean_fitted", og = p01_p02.5_fitted, bg = p01_p97.5_fitted)
png(paste0(file.path("figures", "indicator","lengte_onder_p01"),"_tijdreeks",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_cum_p01_finaal, file.path("data", "result", "indic_cum_p01_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_cum_p01_jaar_stat, file.path("data", "result", "indic_cum_p01_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)

indic_cum_p05_finaal <- indic_cum_p05_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_cum_gem, by = "jaar")

#plot van de trend
gplot <- plottrend(indic_cum_p05_finaal,"lengte_onder_p05_mean",0)
png(paste0(file.path("figures", "indicator","lengte_onder_p05"),"_trend",".png"))
gplot
dev.off()


fig <- plotfitting(indic_basis = indic_cum_basis, respons = "lengte_onder_p05", gemid = "p05_mean_fitted", og = p05_p02.5_fitted, bg = p05_p97.5_fitted)
png(paste0(file.path("figures", "indicator","lengte_onder_p05"),"_tijdreeks",".png"))
fig
dev.off()


#bewaren resultaten
write_vc(indic_cum_p05_finaal, file.path("data", "result", "indic_cum_p05_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_cum_p05_jaar_stat, file.path("data", "result", "indic_cum_p05_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)


indic_cum_p10_finaal <- indic_cum_p10_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_cum_gem, by = "jaar") 

#plot van de trend
gplot <- plottrend(indic_cum_p10_finaal,"lengte_onder_p10_mean",0)
png(paste0(file.path("figures", "indicator","lengte_onder_p10"),"_trend",".png"))
gplot
dev.off()


fig <- plotfitting(indic_basis = indic_cum_basis, respons = "lengte_onder_p10", gemid = "p10_mean_fitted", og = p10_p02.5_fitted, bg = p10_p97.5_fitted)
png(paste0(file.path("figures", "indicator","lengte_onder_p10"),"_tijdreeks",".png"))
fig
dev.off()


#bewaren resultaten
write_vc(indic_cum_p10_finaal, file.path("data", "result", "indic_cum_p10_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_cum_p10_jaar_stat, file.path("data", "result", "indic_cum_p10_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)


indic_cum_p30_finaal <- indic_cum_p30_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_cum_gem, by = "jaar")

#plot van de trend
gplot <- plottrend(indic_cum_p30_finaal,"lengte_onder_p30_mean",0)
png(paste0(file.path("figures", "indicator","lengte_onder_p30"),"_trend",".png"))
gplot
dev.off()


fig <- plotfitting(indic_basis = indic_cum_basis, respons = "lengte_onder_p30", gemid = "p30_mean_fitted", og = p30_p02.5_fitted, bg = p30_p97.5_fitted)
png(paste0(file.path("figures", "indicator","lengte_onder_p30"),"_tijdreeks",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_cum_p30_finaal, file.path("data", "result", "indic_cum_p30_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_cum_p30_jaar_stat, file.path("data", "result", "indic_cum_p30_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)



# 
# df <- indic_cum_basis %>% 
#   filter(simulatienr == 1, meetpunt == "DORP024")
# gplot <- ggplot(data = df , aes(x = jaar, y = p05_mean_fitted)) + geom_line() + 
#   geom_line(aes(x = jaar, y = p05_p02.5_fitted), linetype = "twodash") +
#   geom_line(aes(x = jaar, y = p05_p97.5_fitted), linetype = "twodash") +
#   geom_point(aes(x = jaar, y = lengte_onder_p05_std)) +
#   geom_hline(aes(yintercept = 0), linetype = "dotted") +
#   labs(x = "Jaar", y = "Geobserveerde en gefitte waarden")
# gplot
# 
# ggplot_inla_residuals(I1, df$lengte_onder_p05_std, CI = FALSE, binwidth = NULL)

# kijk naaar 0-lijn: indien altijd binnen cred-interval dan is het niet belangrijk en kan het gedropt worden


# PQL <- glmmPQL(lengte_onder_p05 ~ jaar_factor, ~1|meetpunt, family = gaussian(link = "identity"), data = indic_cum_basis[indic_cum_basis$simulatienr == 1,], verbose = FALSE)
# summary(PQL)
# 
# check <- indic_cum_basis[indic_cum_basis$simulatienr == 1,]
# GHQ <- glmer(lengte_onder_p05 ~ jaar_factor + (1|meetpunt), data = indic_cum_basis[indic_cum_basis$simulatienr == 1,], family = poisson(link = "log"), nAGQ = 25) # Set nAGQ to # of desired iterations
# summary(GHQ)



#2. Een absolute:  aantal dagen per jaar dat het grondwaterpeil beneden een kritische drempelwaarde is
# Per meetpunt, per jaar en per simulatie
# Tel het aantal dagen boven en beneden de kritische drempelwaarde
# Per simulatie
# mixed model met formule cbind(aantal beneden, aantal boven) ~ jaar + (1|meetpunt) met binomiale verdeling
# jaar als factor of first order random walk
# we modellen de kans dat een dag onder de drempel is (kans * 365 dagen = aantal dagen), lost ook het probleem van schrikkeljaren op
# haal coëfficiënt van elk jaar en zijn standard error uit het model
# Pas de formule van Rubin tot op alle jaarcoëfficiënten en hun standard error

bladwijzer_abs <- function(x){x}
indic_abs_basis <- ruwedata %>% 
  inner_join(absperc_gw, by = "meetpunt") %>% 
  mutate(jaar = year(dag),
         meetpunt = factor(meetpunt),
         aantaldagen_jaar = if_else(jaar %in% schrikkeljaar, 366, 365 )
         ) %>% 
  mutate( 
          dag_onder_p01 = if_else(meting_TAW <= p01,1,0),
          dag_onder_p05 = if_else(meting_TAW <= p05,1,0),
          dag_onder_p10 = if_else(meting_TAW <= p10,1,0),
          dag_onder_p30 = if_else(meting_TAW <= p30,1,0),
          dag_boven_p01 = if_else(dag_onder_p01 == 0,1,0),
          dag_boven_p05 = if_else(dag_onder_p05 == 0,1,0),
          dag_boven_p10 = if_else(dag_onder_p10 == 0,1,0),
          dag_boven_p30 = if_else(dag_onder_p30 == 0,1,0)          
  ) %>% 
  group_by(meetpunt, simulatienr,jaar, aantaldagen_jaar, groep3, groep3n) %>% 
  summarise_at(vars(dag_onder_p01:dag_boven_p30), sum) %>% 
  ungroup() %>% 
  mutate(jaar_factor = factor(jaar)
  )

#met opgave gw-groep
indic_abs_basis_gw <- ruwedata %>% 
  inner_join(absperc_gw, by = "meetpunt") %>% 
  mutate(jaar = year(dag),
         meetpunt = factor(meetpunt),
         aantaldagen_jaar = if_else(jaar %in% schrikkeljaar, 366, 365 )
  ) %>% 
  mutate( 
    dag_onder_p01 = if_else(meting_TAW <= p01,1,0),
    dag_onder_p05 = if_else(meting_TAW <= p05,1,0),
    dag_onder_p10 = if_else(meting_TAW <= p10,1,0),
    dag_onder_p30 = if_else(meting_TAW <= p30,1,0),
    dag_boven_p01 = if_else(dag_onder_p01 == 0,1,0),
    dag_boven_p05 = if_else(dag_onder_p05 == 0,1,0),
    dag_boven_p10 = if_else(dag_onder_p10 == 0,1,0),
    dag_boven_p30 = if_else(dag_onder_p30 == 0,1,0)          
  ) %>% 
  group_by(groep3n, meetpunt, simulatienr,jaar, aantaldagen_jaar) %>% 
  summarise_at(vars(dag_onder_p01:dag_boven_p30), sum) %>% 
  ungroup() %>% 
  mutate(jaar_factor = factor(jaar)
  )

# indic_abs_basis <- indic_abs_basis %>% filter (simulatienr > 0) %>% 
#   mutate(lengte_onder_p01 = if_else(lengte_onder_p01 == 0, 1e-16, lengte_onder_p01),
#          lengte_onder_p05 = if_else(lengte_onder_p05 == 0, 1e-16, lengte_onder_p05),
#          lengte_onder_p10 = if_else(lengte_onder_p10 == 0, 1e-16, lengte_onder_p10),
#          lengte_onder_p30 = if_else(lengte_onder_p30 == 0, 1e-16, lengte_onder_p30)
#          dag_onder_p01 = if_else(dag_onder_p01 == 0,1,0),
#          dag_onder_p05 = if_else(dag_onder_p01 == p05,1,0),
#          dag_onder_p10 = if_else(dag_onder_p01 == p10,1,0),
#          dag_onder_p30 = if_else(dag_onder_p01 == p30,1,0),
#          dag_boven_p01 = if_else(dag_boven_p01 == 0,1,0),
#          dag_boven_p05 = if_else(dag_boven_p01 == 0,1,0),
#          dag_boven_p10 = if_else(dag_boven_p01 == 0,1,0),
#          dag_boven_p30 = if_else(dag_boven_p01 == 0,1,0)             
#   ) %>% 
#   mutate(lengte_onder_p01_std = MyStd(lengte_onder_p01),
#          lengte_onder_p05_std = MyStd(lengte_onder_p05),
#          lengte_onder_p10_std = MyStd(lengte_onder_p10),
#          lengte_onder_p30_std = MyStd(lengte_onder_p30)
#   )

indic_abs_basis <- indic_abs_basis %>% 
  filter( between(simulatienr, 1,20), jaar < 2019)
indic_abs_basis_gw <- indic_abs_basis_gw %>% 
  filter( between(simulatienr, 1,20), jaar < 2019)

checkdistributie <- indic_abs_basis %>% 
                                 filter(jaar == 1990, meetpunt == "ASBP003") %>% 
        dplyr::pull(dag_onder_p05)
nbinom <- fitdistr(checkdistributie, "Negative Binomial")
qqp(checkdistributie, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
# poisson <- fitdistr(checkdistributie, "Poisson")
# qqp(checkdistributie, "pois", lambda = poisson$estimate)
binom <- fitdistrplus::fitdist(checkdistributie, "binom", fix.arg = list(size = 365), start = list(prob = 0.1))
summary(binom)
qqp(checkdistributie, "binom", size = 365, prob = binom$estimate)

# test op binomiale verdeling is meestal gelukt, voor p01 waren soms alle waarden = 0 en dan kon het niet getest worden

boxplot(checkdistributie/365)

ggplot(data = checkdistributie, aes(dag_onder_p01)) + geom_histogram() 
ggplot(data = checkdistributie, aes(dag_onder_p05)) + geom_histogram() 
ggplot(data = checkdistributie, aes(dag_onder_p10)) + geom_histogram()  
ggplot(data = checkdistributie, aes(dag_onder_p30)) + geom_histogram()


# percentile <- "01"
# indicatorname <- paste0("dag_onder_p",percentile)
# indicatorname_inv <- paste0("dag_boven_p",percentile)
# standardised <- FALSE
# respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))
# #respons2 <- paste0("cbind(", indicatorname, if (standardised == TRUE) ("_std"), ", ", indicatorname_inv, if (standardised == TRUE) ("_std"), ")")
indic_abs_basis_bu <- indic_abs_basis 
indic_abs_basis <- indic_abs_basis %>% 
  dplyr::select(-contains("fitted") )
modeldata <- indic_abs_basis

modelkeuze <- data.frame(percentiel = c("01", "05", "10", "30" ), model = c("zeroinflatednbinomial2", "zeroinflatednbinomial0", "zeroinflatednbinomial0", "nbinomial"))
modelkeuze$model <- as.character(modelkeuze$model)

indic_abs_function <- function(modeldata, suffix= "", respons, percentile, indicatorname, standardised) {

  prec.prior <- list(prec = list(param = c(0.001, 0.001)))
  model <- as.formula(paste(respons, "~", "f(jaar, model =", "'rw1', scale.model = TRUE,
                          hyper = prec.prior)+ f(meetpunt, model = 'iid', hyper = prec.prior)", sep = " "))   
  # model <- as.formula(paste(respons, "~", "1 + jaar_factor + f(meetpunt, model = 'iid', hyper = prec.prior)", sep = " "))     
  suffix <- ifelse(suffix == "", "", paste0("_", suffix))
  resultname_stat <- paste0("indic_abs_p", percentile, suffix, "_jaar_stat", if (standardised == TRUE) ("_std"))
  resultname_fitted <- paste0("indic_abs_p", percentile, suffix, "_fitted", if (standardised == TRUE) ("_std"))
  
  teller <- 0
  for (i in seq(from = 1, to = 20)) {
    #i = 16
    mdata <- modeldata[modeldata$simulatienr == i,]

    # mdata[mdata[,respons] == 0,respons] <- 1e16
    print(i)    
    teller <- teller + 1
    I2 <- inla(model, 
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                     family = modelkeuze %>% filter(percentiel == percentile) %>% 
                              dplyr::pull(model), 
                     #Ntrials = aantaldagen_jaar,
                     data = mdata
    )

    # I2b <- inla(model,
    #            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    #            family = "zeroinflatednbinomial2",
    #            #Ntrials = aantaldagen_jaar,
    #            data = mdata
    # )
    # I2c <- inla(model,
    #             control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    #             family = "zeroinflatednbinomial1",
    #             #Ntrials = aantaldagen_jaar,
    #             data = mdata,
    #             verbose = FALSE
    # )
    # I2d <- inla(model,
    #             control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    #             family = "zeroinflatednbinomial0",
    #             #Ntrials = aantaldagen_jaar,
    #             data = mdata
    # )
    # de negatieve binomiale modellen gebruiken de logit-link
    #om de niet-getransformeerde waarden te krijgen moet men zowel de intercept als de coëfficiënten exponentiëren. Exp(standaardfout) heeft geen zin, maar het was moeilijker om deze uit te sluiten dan ze (verkeerdelijk) mee te nemen
    #summary(I2)
    result_stat_i <-  I2$summary.random$jaar %>% 
      mutate_at(names(I2$summary.random$jaar)[2:6], exp) %>%
      mutate_at(names(I2$summary.random$jaar)[2:6], function(x){x*exp(I2$summary.fixed$mean)}) %>%       mutate(simulatienr = i) %>% 
      dplyr::select(-sd, -mode, -kld)
    #summary(I2b)  
    #modelkeuze %>% filter(percentiel == percentile) %>% dplyr::pull(model)

    # sum(log(I2$cpo$cpo))
    # sum(log(I2b$cpo$cpo))
    # sum(log(I2c$cpo$cpo))
    # sum(log(I2d$cpo$cpo))
    # # sum(log(I2_binom$cpo$cpo))
    # 
    # sum(log(I2$dic$dic))
    # sum(log(I2b$dic$dic))
    # sum(log(I2c$dic$dic))
    # sum(log(I2d$dic$dic))
    # 
    # sum(log(I2$waic$waic))
    # sum(log(I2b$waic$waic))
    # sum(log(I2c$waic$waic))
    # sum(log(I2d$waic$waic))
    # 
    # sum(I2$mlik)
    # sum(I2b$mlik)
    # sum(I2c$mlik)
    # sum(I2d$mlik)

    # names(inla.models()$likelihood)
    
    # # Assess overdispersion (variantie / aantal vrijheidsgraden ~ 1)
     # #voor binomiaal
     # Pi   <- I2b$summary.fitted.values[,"mean"]
     # ExpY <- Pi * mdata$aantaldagen_jaar
     # VarY <- Pi * mdata$aantaldagen_jaar * (1 - Pi)
     # E1   <- (mdata %>%
     #               dplyr::pull(!!indicatorname) - ExpY) / sqrt(VarY)
     # N    <- nrow(mdata)
     # p <- nrow(I2b$summary.fixed)
     # Dispersion <- sum(E1^2) / (N - p)
     # Dispersion
    
    # #voor poisson
    # Pi   <- I2c$summary.fitted.values[,"mean"]
    # ExpY <- Pi #* mdata$aantaldagen_jaar
    # VarY <- Pi #* mdata$aantaldagen_jaar * (1 - Pi)
    # E1   <- (mdata %>%
    #               dplyr::pull(!!indicatorname) - Pi) / sqrt(VarY)
    # N    <- nrow(mdata)
    # p <- nrow(I2c$summary.fixed)
    # Dispersion <- sum(E1^2) / (N - p)
    # Dispersion
    
    # #voor negatief binomiaal
    # Pi  <- I2$summary.fitted.values[,"mean"]
    # theta <- I2$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>%
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)
    # 
    # Pi  <- I2b$summary.fitted.values[,"mean"]
    # theta <- I2b$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>%
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)
    # 
    # 
    # 
    # Pi  <- I2c$summary.fitted.values[,"mean"]
    # theta <- I2c$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>%
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)
    # 
    # 
    # Pi  <- I2d$summary.fitted.values[,"mean"]
    # theta <- I2d$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>%
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)
    # 
    # # Pearson residuals
    # par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
    # plot(x = Pi,
    #      y = E1,
    #      xlab = "Fitted values",
    #      ylab = "Pearson residuals")
    # abline(h = 0, lty = 2)
    # 
    # #names(inla.models()$likelihood)
    # 
    # # bekijk de gefitte waarden van het model
    # # Plot the fitted values
    # Fit1     <- I2$summary.fitted.values[,"mean"]
    # Fit1.025 <- I2$summary.fitted.values$"0.025quant"
    # Fit1.975 <- I2$summary.fitted.values$"0.975quant"
    # 
    # Fit1     <- I2d$summary.fitted.values[,"mean"]
    # Fit1.025 <- I2d$summary.fitted.values$"0.025quant"
    # Fit1.975 <- I2d$summary.fitted.values$"0.975quant"
    # 
    # #
    # # check <- I2$summary.random$meetpunt
    # #
    # # result_fitted_i
    # mdata
    # mdata$Fitted1  <- Fit1
    # mdata$Fit1.025 <- Fit1.025
    # mdata$Fit1.975 <- Fit1.975
    # #gdata <- mdata %>% dplyr::select(!!respons, jaar, contains("Fit"))
    # p <- ggplot(data = mdata, aes_string(y = respons, x = "jaar"))
    # p <- p + xlab("Jaar") + ylab(respons)
    # p <- p + theme(text = element_text(size=15))
    # p <- p + geom_point(shape = 16, size = 2, col = "black")
    # p <- p + geom_line(aes(x = jaar, y = Fitted1))
    # p <- p + geom_ribbon(data = mdata %>% group_by(jaar) %>% summarise(ymax_data = max(Fit1.975), ymin_data = min(Fit1.025)), aes(x = jaar,
    #                          ymax = ymax_data,
    #                          ymin = ymin_data), inherit.aes = F,
    #                      fill = grey(0.5),
    #                      alpha = 0.4)
    # p <- p + theme(strip.text = element_text(size = 15))
    # p
     # summary(I2)
    # result_stat_i <-  I2d$summary.random$jaar %>%
    #   mutate(simulatienr = i)
    #  result_stat_i <- result_stat_i %>%  rename (p0.025 = '0.025quant',
    #                              p975.5 = '0.975quant',
    #                              jaar =  ID)
    #  gplot <- ggplot(data = result_stat_i, aes(x = jaar, y = mean)) + 
    #    geom_line(aes(x = jaar, y = p0.025), linetype = "longdash") +
    #    geom_ribbon(aes(x = jaar, ymax = p975.5, ymin = p0.025)) +  
    #    geom_line(aes(x = jaar, y = p975.5), linetype = "longdash") +
    #    geom_line(color = "lightblue") +
    #    #geom_point(data = mdata, aes_string(x = "jaar", y = respons)) +
    #    geom_hline(aes(yintercept = 0), linetype = "dotted") +
    #    labs(x = "Jaar", y = "trend")
    #  gplot
    # 
    # #conclusie negative binomiaal (bij 30 wel onderdispersed), gestandardiseerd met een gewijzigde non-informatieve prior
    # 
    # result_stat_i_meetpunt <-  I2$summary.random$meetpunt %>% 
    #   mutate(simulatienr = i)
    
    # result_stat_i <- result_stat_i %>% 
    #   mutate(og_berekend = mean - 1.96*sd,
    #          bg_berekend = mean + 1.96*sd)
    
    varname_mean <- paste0("p", percentile, "_mean", if (standardised == TRUE) ("_std"), "_fitted")
    varname_sd <- paste0("p", percentile, "_sd", if (standardised == TRUE) ("_std"), "_fitted")
    varname_p025 <- paste0("p", percentile, "_p02.5", if (standardised == TRUE) ("_std"), "_fitted")
    varname_p975 <- paste0("p", percentile, "_p97.5", if (standardised == TRUE) ("_std"), "_fitted")
    result_fitted_i <-  I2$summary.fitted.values %>% 
      rename(!!varname_mean := mean,
             !!varname_sd := sd,
             !!varname_p025 := '0.025quant',
             !!varname_p975 := '0.975quant')
    mdata <- bind_cols(mdata, result_fitted_i)
    if (standardised == TRUE) {
      varname_mean_backtransformed <- paste0(varname_mean, "_untr")
      varname_p025_backtransformed <- paste0(varname_p025, "_untr")
      varname_p975_backtransformed <- paste0(varname_p975, "_untr")
      result_fitted_i <- result_fitted_i %>% 
        mutate( !!varname_mean_backtransformed := unscale(result_fitted_i %>% 
                                                            dplyr::pull(!!varname_mean),m,s),
                !!varname_p025_backtransformed := unscale(result_fitted_i %>% 
                                                            dplyr::pull(!!varname_p025),m,s),
                !!varname_p975_backtransformed := unscale(result_fitted_i %>% 
                                                            dplyr::pull(!!varname_p975),m,s)    
        )
    }
    if (i == 1) {
      result_stat <- result_stat_i
      result_fitted <- mdata
      
    } else {
      result_stat <- bind_rows(result_stat, 
                               result_stat_i)
      result_fitted <- bind_rows(result_fitted, mdata)    
    }
    
    if (i == 20) {  
      result_stat <- result_stat %>% 
        rename( jaar = ID,
                p02.5 = '0.025quant',
                p50 = '0.5quant',
                p97.5 = '0.975quant')
      
        if (standardised == TRUE) {
          result_stat <- result_stat %>% 
            mutate(mean_untr = unscale(mean, m, s),
                   og_berekend_untr = unscale(og_berekend, m, s),
                   bg_berekend_untr = unscale(bg_berekend, m, s),
                   p02.5_untr = unscale(p02.5, m, s),
                   p97.5_untr = unscale(p97.5, m, s),
                   se_berekend_untr = ((p97.5_untr  - mean_untr )/1.96 - 
                                         (p02.5_untr  - mean_untr )/1.96)/2,
            )
        }
      resultlijst <- list( result_stat, 
                           result_fitted)
      names(resultlijst) <- c(resultname_stat, resultname_fitted)
      list2env(resultlijst, envir = .GlobalEnv)
      
      if (varname_mean %in% colnames(indic_abs_basis)) {
        indic_abs_basis <- indic_abs_basis %>% 
          dplyr::select(-!!varname_mean, -!!varname_sd, -!!varname_p025, -!!varname_p975) 
      }
      
      indic_abs_basis <- indic_abs_basis %>% 
         inner_join(result_fitted %>% 
                      dplyr::select(contains("fitted"), 1:3), 
                    by = c("meetpunt","simulatienr", "jaar")) 
    }
  }
  return(indic_abs_basis)
}



# MyFunBinom <- function(x) { exp(x) / (1 + exp(x)) } 
# check <- data.frame(unlist( 
#   lapply(
#     I1$marginals.random$jaar,
#     function(x) inla.emarginal(MyFunBinom, x))))
# check <- result_stat_i %>% 
#   mutate(checkmean = MyFunBinom(mean))
#debugonce(indic_abs_function)

# indic_abs_basis <- bind_cols(indic_abs_basis, indic_abs_p30_fitted %>%
                               # dplyr::select(contains("fitted")))

percentile <- "01"
indicatorname <- paste0("dag_onder_p",percentile)
indicatorname_inv <- paste0("dag_boven_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))
#debugonce(indic_abs_function)
indic_abs_basis <- indic_abs_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

percentile <- "05"
indicatorname <- paste0("dag_onder_p",percentile)
indicatorname_inv <- paste0("dag_boven_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))

 indic_abs_basis <- indic_abs_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)
# debugonce("indic_abs_function")
# check <- indic_abs_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

percentile <- "10"
indicatorname <- paste0("dag_onder_p",percentile)
indicatorname_inv <- paste0("dag_boven_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))

indic_abs_basis <- indic_abs_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

percentile <- "30"
indicatorname <- paste0("dag_onder_p",percentile)
indicatorname_inv <- paste0("dag_boven_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))

indic_abs_basis <- indic_abs_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

#bewaren resultaat
write_vc(indic_abs_basis, file.path("data", "result", "indic_abs_basis"), sorting = c("jaar", "meetpunt", "simulatienr"), strict = FALSE)

#berekenen van indicator
indic_abs_gem <- indic_abs_basis %>% 
  group_by(jaar) %>% 
  summarise_at(vars(dag_onder_p01:dag_onder_p30), list(~mean(.), ~median(.))) %>% 
  ungroup


# rekening houden met multiple imputaties

indic_abs_p01_finaal <- indic_abs_p01_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_abs_gem, by = "jaar")

#plot van de trend
# gplot <- ggplot(data = indic_abs_p01_finaal, aes(x = jaar, y = gem_jaar)) + 
#   geom_line(aes(x = jaar, y = og_jaar), linetype = "longdash") +
#   geom_ribbon(aes(x = jaar, ymax = bg_jaar, ymin = og_jaar),
#                                     fill = grey(0.5),
#                                    alpha = 0.4) +  
#   geom_line(aes(x = jaar, y = bg_jaar), linetype = "longdash") +
#   geom_line(color = "dark blue") +
#    # geom_point(data = indic_abs_basis, aes(x = jaar, y = dag_onder_p01)) +
#   geom_hline(aes(yintercept = 4), linetype = "dotted") +
#   scale_x_continuous(breaks = 1985:2020, labels = insert_minor(seq(1985, 2020, by = 5), 4)) +  
#   labs(x = "Jaar", y = "trend")

gplot <- plottrend(indic_abs_p01_finaal,"dag_onder_p01_mean",4)
png(paste0(file.path("figures", "indicator","dag_onder_p01"),"_trend",".png"))
gplot
dev.off()

fig <- plotfitting(indic_basis = indic_abs_basis, respons = "dag_onder_p01", gemid = "p01_mean_fitted", og = p01_p02.5_fitted, bg = p01_p97.5_fitted)
png(paste0(file.path("figures", "indicator","dag_onder_p01"),"_tijdreeks",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_abs_p01_finaal, file.path("data", "result", "indic_abs_p01_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_abs_p01_jaar_stat, file.path("data", "result", "indic_abs_p01_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)

# check <- indic_abs_basis %>% 
#   filter(jaar == 1988)
# summary (I2)
# check2 <- I2$summary.random$meetpunt
# model

indic_abs_p05_finaal <- indic_abs_p05_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_abs_gem, by = "jaar")


#plot van de trend
gplot <- plottrend(indic_abs_p05_finaal,"dag_onder_p05_mean", 18)
png(paste0(file.path("figures", "indicator","dag_onder_p05"),"_trend",".png"))
gplot
dev.off()

fig <- plotfitting(indic_basis = indic_abs_basis, respons = "dag_onder_p05", gemid = "p05_mean_fitted", og = p05_p02.5_fitted, bg = p05_p97.5_fitted)
png(paste0(file.path("figures", "indicator","dag_onder_p05"),"_tijdreeks",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_abs_p05_finaal, file.path("data", "result", "indic_abs_p05_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_abs_p05_jaar_stat, file.path("data", "result", "indic_abs_p05_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)

indic_abs_p10_finaal <- indic_abs_p10_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_abs_gem, by = "jaar")

#plot van de trend
gplot <- plottrend(indic_abs_p10_finaal,"dag_onder_p10_mean", 36)
png(paste0(file.path("figures", "indicator","dag_onder_p10"),"_trend",".png"))
gplot
dev.off()

fig <- plotfitting(indic_basis = indic_abs_basis, respons = "dag_onder_p10", gemid = "p10_mean_fitted", og = p10_p02.5_fitted, bg = p10_p97.5_fitted)
png(paste0(file.path("figures", "indicator","dag_onder_p10"),"_tijdreeks",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_abs_p10_finaal, file.path("data", "result", "indic_abs_p10_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_abs_p10_jaar_stat, file.path("data", "result", "indic_abs_p10_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)

indic_abs_p30_finaal <- indic_abs_p30_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_abs_gem, by = "jaar")


#plot van de trend
gplot <- plottrend(indic_abs_p30_finaal,"dag_onder_p30_mean",110)
png(paste0(file.path("figures", "indicator","dag_onder_p30"),"_trend",".png"))
gplot
dev.off()

fig <- plotfitting(indic_basis = indic_abs_basis, respons = "dag_onder_p30", gemid = "p30_mean_fitted", og = p30_p02.5_fitted, bg = p30_p97.5_fitted)
png(paste0(file.path("figures", "indicator","dag_onder_p30"),"_tijdreeks",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_abs_p30_finaal, file.path("data", "result", "indic_abs_p30_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_abs_p30_jaar_stat, file.path("data", "result", "indic_abs_p30_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)

bladwijzer_rel <- function(x){x}

indic_rel_basis <-  ruwedata %>% 
  mutate(daginjaar = yday(dag),
         daginjaar_corr = if_else(jaar %in% schrikkeljaar, 
                                   if_else (daginjaar >= 61, daginjaar - 1, daginjaar), daginjaar)          ) 

indic_rel_basis <- indic_rel_basis %>% 
  filter(simulatienr <= 20)  

indic_rel_percentiel <- indic_rel_basis %>% 
  filter(between(jaar, 1985, 2015)) %>% 
  group_by(meetpunt, simulatienr, daginjaar_corr) %>% 
  summarise(p01 = quantile(meting_TAW, probs = 0.01, names = FALSE),
            p05 = quantile(meting_TAW, probs = 0.05, names = FALSE),
            p10 = quantile(meting_TAW, probs = 0.10, names = FALSE),
            p30 = quantile(meting_TAW, probs = 0.30, names = FALSE),
            aantal = n()
            ) %>% 
  ungroup

write_vc(indic_rel_percentiel, file.path("data", "result", "indic_rel_percentiel"), sorting = c("meetpunt","simulatienr", "daginjaar_corr"), strict = FALSE)

indic_rel <- indic_rel_basis %>% 
  inner_join(indic_rel_percentiel %>% dplyr::select(-aantal), by = c("meetpunt", "simulatienr", "daginjaar_corr")) %>% 
  mutate( 
    rdag_onder_p01 = if_else(meting_TAW <= p01,1,0),
    rdag_onder_p05 = if_else(meting_TAW <= p05,1,0),
    rdag_onder_p10 = if_else(meting_TAW <= p10,1,0),
    rdag_onder_p30 = if_else(meting_TAW <= p30,1,0),
    rdag_boven_p01 = if_else(rdag_onder_p01 == 0,1,0),
    rdag_boven_p05 = if_else(rdag_onder_p05 == 0,1,0),
    rdag_boven_p10 = if_else(rdag_onder_p10 == 0,1,0),
    rdag_boven_p30 = if_else(rdag_onder_p30 == 0,1,0)          
  ) %>% 
  group_by(meetpunt, simulatienr,jaar) %>% 
  summarise_at(vars(rdag_onder_p01: rdag_boven_p30), sum) %>% 
  ungroup() %>% 
  mutate(jaar_factor = factor(jaar)
  )

indic_rel <- indic_rel %>% filter (jaar < 2019)

write_vc(indic_rel, file.path("data", "result", "indic_rel"), sorting = c("jaar", "meetpunt","simulatienr"), strict = FALSE)

checkdistributie <- indic_rel %>% 
  filter(jaar <= 2000, meetpunt == "ASBP003") %>% 
  dplyr::pull(rdag_onder_p30)

nbinom <- fitdistr(checkdistributie, "Negative Binomial")
qqp(checkdistributie, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

#theoretisch is het een poisson-verdeling: het geeft de kans weer dat een bepaald proces binnen een zeker tijdsinterval optreedt
poisson <- fitdistr(checkdistributie, "Poisson")
qqp(checkdistributie, "pois", lambda = poisson$estimate)

#eigenlijk kan het theoretisch gezien geen binomiale verdeling zijn, want het zijn geen 365 trekkingen van een zelfde kans
binom <- fitdistrplus::fitdist(checkdistributie, "binom", fix.arg = list(size = 365), start = list(prob = 0.1))
summary(binom)
qqp(checkdistributie, "binom", size = 365, prob = binom$estimate)

qqp(checkdistributie, "norm")

cgamma <- fitdistr(checkdistributie , "Gamma")
qqp(checkdistributie, "gamma", shape = cgamma$estimate[[1]], rate = cgamma$estimate[[2]])
#p01: bij droge jaren kunnen meestal alle aantal-verdelingen kunnen en zijn gelijkwaardig (met toch een lichte voorkeur voor Poisson), in natte jaren is het vaak negatief binomiaal verdeeld
#p05: idem 
#p10: idem 
#p30: idem 

indic_rel <- indic_rel %>% 
  dplyr::select(-contains("fitted") )
modeldata <- indic_rel
# percentile <- "30"
# indicatorname <- paste0("rdag_onder_p",percentile)
# indicatorname_inv <- paste0("rdag_boven_p",percentile)
# standardised <- FALSE
# respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))

modelkeuze <- data.frame(percentiel = c("01", "05", "10", "30" ), model = c("zeroinflatednbinomial0", "zeroinflatednbinomial1", "nbinomial", "nbinomial"))
modelkeuze$model <- as.character(modelkeuze$model)

indic_rel_function <- function(modeldata, respons, percentile, indicatorname, standardised) {
  
  prec.prior <- list(prec = list(param = c(0.001, 0.001)))
  model <- as.formula(paste(respons, "~", "1 + f(jaar, model =", "'rw1', scale.model = TRUE,
                            hyper = prec.prior)+ f(meetpunt, model = 'iid', hyper = prec.prior)", sep = " "))   
  
  resultname_stat <- paste0("indic_rel_p", percentile,"_jaar_stat", if (standardised == TRUE) ("_std"))
  resultname_fitted <- paste0("indic_rel_p", percentile,"_fitted", if (standardised == TRUE) ("_std"))
  
  for (i in seq(from = 1, to = 20)) {
    #i = 2
    mdata <- modeldata[modeldata$simulatienr == i,]
    print(i)
    
    I3 <- inla(model, 
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
               family = modelkeuze %>% filter(percentiel == percentile) %>% 
                 dplyr::pull(model), 
               #Ntrials = aantaldagen_jaar,
               data = mdata
    )
    # I3b <- inla(model, 
    #            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    #            family = "nbinomial", 
    #            #Ntrials = aantaldagen_jaar,zeroinflatednbinomial2
    #            data = mdata
    # )    
    # 
    # I3c <- inla(model, 
    #           control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    #           family = "zeroinflatednbinomial0", 
    #           #Ntrials = aantaldagen_jaar,zeroinflatednbinomial2
    #           data = mdata
    # )    
    # 
    # 
    # I3d <- inla(model, 
    #           control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    #           family = "gaussian", 
    #           #Ntrials = aantaldagen_jaar,zeroinflatednbinomial2
    #           data = mdata
    # )        
        
    # de negatieve binomiale modellen gebruiken de logit-link
    #om de niet-getransformeerde waarden te krijgen moet men zowel de intercept als de coëfficiënten exponentiëren. Exp(standaardfout) heeft geen zin, maar het was moeilijker om deze uit te sluiten dan ze (verkeerdelijk) mee te nemen
    result_stat_i <-  I3$summary.random$jaar %>% 
      mutate_at(names(I3$summary.random$jaar)[2:6], exp) %>%
      mutate_at(names(I3$summary.random$jaar)[2:6], function(x){x*exp(I3$summary.fixed$mean)}) %>%       mutate(simulatienr = i) %>% 
      dplyr::select(-sd, -mode, -kld)   

    # #modelvergelijking
    # sum(log(I3$cpo$cpo))
    # sum(log(I3b$cpo$cpo))
    # sum(log(I3c$cpo$cpo))
    # sum(log(I3d$cpo$cpo))
    # # sum(log(I3_binom$cpo$cpo)) 
    # # 
    # sum(log(I3$dic$dic))
    # sum(log(I3b$dic$dic))
    # sum(log(I3c$dic$dic))  
    # sum(log(I3d$dic$dic))  
    # # 
    # sum(log(I3$waic$waic))
    # sum(log(I3b$waic$waic))
    # sum(log(I3c$waic$waic))
    # sum(log(I3d$waic$waic))
    # # 
    # sum(I3$mlik)
    # sum(I3b$mlik)
    # sum(I3c$mlik)
    # sum(I3d$mlik)
    
    # names(inla.models()$likelihood)
    
    # # Assess overdispersion (variantie / aantal vrijheidsgraden ~ 1)
    # #voor binomiaal
    # Pi   <- I3_binom$summary.fitted.values[,"mean"]
    # ExpY <- Pi * mdata$aantaldagen_jaar
    # VarY <- Pi * mdata$aantaldagen_jaar * (1 - Pi)
    # E1   <- (mdata %>%
    #               dplyr::pull(!!indicatorname) - ExpY) / sqrt(VarY)
    # N    <- nrow(mdata)
    # p <- nrow(I3_binom$summary.fixed)
    # Dispersion <- sum(E1^2) / (N - p)
    # Dispersion

    # # Get Pearson residuals voor gamma
    # Pi  <- I3$summary.fitted.values[,"mean"] 
    # r    <- I3$summary.hyperpar[1,"mean"]
    # VarY <- Pi^2 / r 
    # E1   <- (mdata %>% dplyr::pull(!!indicatorname) - Pi) / sqrt(VarY)
    # 
    #     
    # #voor poisson
    # Pi   <- I3$summary.fitted.values[,"mean"]
    # ExpY <- Pi #* mdata$aantaldagen_jaar
    # VarY <- Pi #* mdata$aantaldagen_jaar * (1 - Pi)
    # E1   <- (mdata %>%
    #               dplyr::pull(!!indicatorname) - Pi) / sqrt(VarY)
    # N    <- nrow(mdata)
    # p <- nrow(I3$summary.fixed)
    # Dispersion <- sum(E1^2) / (N - p)
    # Dispersion
    # 
    # #voor negatief binomiaal
    # Pi  <- I3b$summary.fitted.values[,"mean"]
    # theta <- I3b$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>%
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)
    # 
    # Pi  <- I3c$summary.fitted.values[,"mean"]
    # theta <- I3c$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>%
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)    
    # 
    # Pi  <- I3d$summary.fitted.values[,"mean"]
    # theta <- I3d$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>%
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)       
    # 
    # 
    # 
    # Pi  <- I3_prior$summary.fitted.values[,"mean"]
    # theta <- I3_prior$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>% 
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)    
    # 
    # 
    # Pi  <- I3$summary.fitted.values[,"mean"]
    # theta <- I3$summary.hyperpar[1,"mean"]
    # E1 <- (mdata %>%
    #          dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
    # sum(E1^2) / (nrow(mdata) - 1)
    # 
    # #voor normaal verdeling
    # Pi  <- I3d$summary.fitted.values[,"mean"]    
    # #sigma2 <- 1 / sqrt(I3d$summary.hyperpar[1,"mean"])
    # tau    <- I3d$marginals.hyperpar$`Precision for the Gaussian observations`
    # #tau2 <- inla.emarginal(function(x) {x }, tau)
    # sigma  <- inla.emarginal(function(x) {1 / sqrt(x) }, tau)
    # E1 <- (mdata %>%
    #          dplyr::pull(!!indicatorname)  - Pi) / sigma
    # sum(E1^2) / (nrow(mdata) - 1)    
    # 
    # 
    # # Pearson residuals
    # par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
    # plot(x = Pi,
    #      y = E1,
    #      xlab = "Fitted values",
    #      ylab = "Pearson residuals")
    # abline(h = 0, lty = 2)
    # 
    # #names(inla.models()$likelihood)
    # 
    # # bekijk de gefitte waarden van het model
    # Plot the fitted values
    # Fit1     <- I3d$summary.fitted.values[,"mean"]
    # Fit1.025 <- I3d$summary.fitted.values$"0.025quant"
    # Fit1.975 <- I3d$summary.fitted.values$"0.975quant"
    # 
    # Fit1     <- I3$summary.fitted.values[,"mean"]
    # Fit1.025 <- I3$summary.fitted.values$"0.025quant"
    # Fit1.975 <- I3$summary.fitted.values$"0.975quant"

    #
    # check <- I3$summary.random$meetpunt
    #
    # result_fitted_i
    # mdata
    # mdata$Fitted1  <- Fit1
    # mdata$Fit1.025 <- Fit1.025
    # mdata$Fit1.975 <- Fit1.975
    # #gdata <- mdata %>% dplyr::select(!!respons, jaar, contains("Fit"))
    # p <- ggplot(data = mdata, aes_string(y = respons, x = "jaar"))
    # p <- p + xlab("Jaar") + ylab(respons)
    # p <- p + theme(text = element_text(size=15))
    # p <- p + geom_point(shape = 16, size = 2, col = "black")
    # p <- p + geom_line(aes(x = jaar, y = Fitted1))
    # p <- p + geom_ribbon(data = mdata %>% group_by(jaar) %>% summarise(ymax_var =  max(Fit1.975),
    #                                                                   ymin.var = min(Fit1.025) ), aes(x = jaar,
    #                          ymax = ymax_var,
    #                          ymin = ymin.var), inherit.aes = F,
    #                      fill = grey(0.5),
    #                      alpha = 0.4)
    # p <- p + theme(strip.text = element_text(size = 15))
    # p
    # # summary(I3)
    # result_stat_i <-  I3d$summary.random$jaar %>%
    #   mutate(simulatienr = i)
    # result_stat_i <- result_stat_i %>%  rename (p0.025 = '0.025quant',
    #                            p975.5 = '0.975quant',
    #                            jaar =  ID)
    # gplot <- ggplot(data = result_stat_i, aes(x = jaar, y = mean)) +
    #  geom_line(aes(x = jaar, y = p0.025), linetype = "longdash") +
    #  geom_ribbon(aes(x = jaar, ymax = p975.5, ymin = p0.025)) +
    #  geom_line(aes(x = jaar, y = p975.5), linetype = "longdash") +
    #  geom_line(color = "lightblue") +
    #  #geom_point(data = mdata, aes_string(x = "jaar", y = respons)) +
    #  geom_hline(aes(yintercept = 0), linetype = "dotted") +
    #  labs(x = "Jaar", y = "trend")
    # gplot

    # 
    # 
    # result_stat_i_meetpunt <-  I3$summary.random$meetpunt %>% 
    #   mutate(simulatienr = i)
    
    # result_stat_i <- result_stat_i %>% 
    #   mutate(og_berekend = mean - 1.96*sd,
    #          bg_berekend = mean + 1.96*sd)
    
    varname_mean <- paste0("p", percentile, "_mean", if (standardised == TRUE) ("_std"), "_fitted")
    varname_sd <- paste0("p", percentile, "_sd", if (standardised == TRUE) ("_std"), "_fitted")
    varname_p025 <- paste0("p", percentile, "_p02.5", if (standardised == TRUE) ("_std"), "_fitted")
    varname_p975 <- paste0("p", percentile, "_p97.5", if (standardised == TRUE) ("_std"), "_fitted")
    result_fitted_i <-  I3$summary.fitted.values %>% 
      rename(!!varname_mean := mean,
             !!varname_sd := sd,
             !!varname_p025 := '0.025quant',
             !!varname_p975 := '0.975quant')
    mdata <- bind_cols(mdata, result_fitted_i)
    if (standardised == TRUE) {
      varname_mean_backtransformed <- paste0(varname_mean, "_untr")
      varname_p025_backtransformed <- paste0(varname_p025, "_untr")
      varname_p975_backtransformed <- paste0(varname_p975, "_untr")
      result_fitted_i <- result_fitted_i %>% 
        mutate( !!varname_mean_backtransformed := unscale(result_fitted_i %>% 
                                                            dplyr::pull(!!varname_mean),m,s),
                !!varname_p025_backtransformed := unscale(result_fitted_i %>% 
                                                            dplyr::pull(!!varname_p025),m,s),
                !!varname_p975_backtransformed := unscale(result_fitted_i %>% 
                                                            dplyr::pull(!!varname_p975),m,s)    
        )
    }
    if (i == 1) {
      result_stat <- result_stat_i
      result_fitted <- mdata
      
    } else {
      result_stat <- bind_rows(result_stat, 
                               result_stat_i)
      result_fitted <- bind_rows(result_fitted, mdata)    
    }
    
    if (i == 20) {  
      result_stat <- result_stat %>% 
        rename( jaar = ID,
                p02.5 = '0.025quant',
                p50 = '0.5quant',
                p97.5 = '0.975quant')
      
      if (standardised == TRUE) {
        result_stat <- result_stat %>% 
          mutate(mean_untr = unscale(mean, m, s),
                 og_berekend_untr = unscale(og_berekend, m, s),
                 bg_berekend_untr = unscale(bg_berekend, m, s),
                 p02.5_untr = unscale(p02.5, m, s),
                 p97.5_untr = unscale(p97.5, m, s),
                 se_berekend_untr = ((p97.5_untr  - mean_untr )/1.96 - 
                                       (p02.5_untr  - mean_untr )/1.96)/2,
          )
      }
      resultlijst <- list( result_stat, 
                           result_fitted)
      names(resultlijst) <- c(resultname_stat, resultname_fitted)
      list2env(resultlijst, envir = .GlobalEnv)
      
      if (varname_mean %in% colnames(indic_rel)) {
        indic_rel <- indic_rel %>% 
          dplyr::select(-!!varname_mean, -!!varname_sd, -!!varname_p025, -!!varname_p975) 
      }
      
      indic_rel <- indic_rel %>% 
        inner_join(result_fitted %>% 
                     dplyr::select(contains("fitted"), 1:3), 
                   by = c("meetpunt","simulatienr", "jaar")) 
    }
  }
  return(indic_rel)
}

#indic_rel_bu <- indic_rel


percentile <- "01"
indicatorname <- paste0("rdag_onder_p",percentile)
indicatorname_inv <- paste0("rdag_boven_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))
#debugonce(indic_rel_function)
indic_rel <- indic_rel_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

percentile <- "05"
indicatorname <- paste0("rdag_onder_p",percentile)
indicatorname_inv <- paste0("rdag_boven_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))
indic_rel <- indic_rel_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

percentile <- "10"
indicatorname <- paste0("rdag_onder_p",percentile)
indicatorname_inv <- paste0("rdag_boven_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))
indic_rel <- indic_rel_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

percentile <- "30"
indicatorname <- paste0("rdag_onder_p",percentile)
indicatorname_inv <- paste0("rdag_boven_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))
indic_rel <- indic_rel_function(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

#wegschrijven resultaat
write_vc(indic_rel, file.path("data", "result", "indic_rel"), sorting = c("jaar", "meetpunt","simulatienr"), strict = FALSE)

#berekenen van indicator
indic_rel_gem <- indic_rel %>% 
  group_by(jaar) %>% 
  summarise_at(vars(rdag_onder_p01:rdag_onder_p30), list(~mean(.), ~median(.))) %>% 
  ungroup

# rekening houden met multiple imputaties

indic_rel_p01_finaal <- indic_rel_p01_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_rel_gem, by = "jaar")

#plot van de trend
# gplot <- ggplot(data = indic_rel_p01_finaal, aes(x = jaar, y = gem_jaar)) + 
#   geom_line(aes(x = jaar, y = og_jaar), linetype = "longdash") +
#   geom_ribbon(aes(x = jaar, ymax = bg_jaar, ymin = og_jaar),
#               fill = grey(0.5),
#               alpha = 0.4) +  
#   geom_line(aes(x = jaar, y = bg_jaar), linetype = "longdash") +
#   geom_line(color = "dark blue") +
#   # geom_point(data = indic_rel, aes(x = jaar, y = rdag_onder_p01)) +
#   geom_hline(aes(yintercept = 4), linetype = "dotted") +
#   scale_x_continuous(breaks = 1985:2020, labels = insert_minor(seq(1985, 2020, by = 5), 4)) +    
#   labs(x = "Jaar", y = "trend")

gplot <- plottrend(indic_rel_p01_finaal,"rdag_onder_p01_mean",4)
png(paste0(file.path("figures", "indicator","rdag_onder_p01"),"_trend",".png"))
gplot
dev.off()


fig <- plotfitting(indic_basis = indic_rel, respons = "rdag_onder_p01", gemid = "p01_mean_fitted", og = p01_p02.5_fitted, bg = p01_p97.5_fitted)
png(paste0(file.path("figures", "indicator","rdag_onder_p01"),"_tijdreeks",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_rel_p01_finaal, file.path("data", "result", "indic_rel_p01_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_rel_p01_jaar_stat, file.path("data", "result", "indic_rel_p01_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)


indic_rel_p05_finaal <- indic_rel_p05_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_rel_gem, by = "jaar")

#plot van de trend
gplot <- plottrend(indic_rel_p05_finaal,"rdag_onder_p05_mean",18)
png(paste0(file.path("figures", "indicator","rdag_onder_p05"),"_trend",".png"))
gplot
dev.off()

fig <- plotfitting(indic_basis = indic_rel, respons = "rdag_onder_p05", gemid = "p05_mean_fitted", og = p05_p02.5_fitted, bg = p05_p97.5_fitted)
png(paste0(file.path("figures", "indicator","rdag_onder_p05"),"_tijdreeks",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_rel_p05_finaal, file.path("data", "result", "indic_rel_p05_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_rel_p05_jaar_stat, file.path("data", "result", "indic_rel_p05_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)


indic_rel_p10_finaal <- indic_rel_p10_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_rel_gem, by = "jaar")


#plot van de trend
gplot <- plottrend(indic_rel_p10_finaal,"rdag_onder_p10_mean",36)
png(paste0(file.path("figures", "indicator","rdag_onder_p10"),"_trend",".png"))
gplot
dev.off()

fig <- plotfitting(indic_basis = indic_rel, respons = "rdag_onder_p10", gemid = "p10_mean_fitted", og = p10_p02.5_fitted, bg = p10_p97.5_fitted)
png(paste0(file.path("figures", "indicator","rdag_onder_p10"),"_tijdreeks",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_rel_p10_finaal, file.path("data", "result", "indic_rel_p10_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_rel_p10_jaar_stat, file.path("data", "result", "indic_rel_p10_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)


indic_rel_p30_finaal <- indic_rel_p30_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_rel_gem, by = "jaar")

#plot van de trend
gplot <- plottrend(indic_rel_p30_finaal,"rdag_onder_p30_mean", 110)
png(paste0(file.path("figures", "indicator","rdag_onder_p30"),"_trend",".png"))
gplot
dev.off()

fig <- plotfitting(indic_basis = indic_rel, respons = "rdag_onder_p30", gemid = "p30_mean_fitted", og = p30_p02.5_fitted, bg = p30_p97.5_fitted)
png(paste0(file.path("figures", "indicator","rdag_onder_p30"),"_tijdreeks",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_rel_p30_finaal, file.path("data", "result", "indic_rel_p30_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_rel_p30_jaar_stat, file.path("data", "result", "indic_rel_p30_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)


#opsplitsen per gw-groep
#de zeer natte (nr1)en natte (nr2) worden geagregeerd om tot een vergelijkbare groepsgrootte te komen


indic_abs_basis_bu <- indic_abs_basis 
indic_abs_basis_gw <- indic_abs_basis %>% 
  dplyr::select(-contains("fitted") )
indic_abs_basis_gw <- indic_abs_basis_gw %>% 
  inner_join(tubes_indicator %>% 
               mutate(groep3 = ifelse(groupnr == 1, 2, groupnr)) %>% 
               dplyr::select(-groupnr), by = c("meetpunt" = "loc_code"))
indic_abs_basis_gw <- indic_abs_basis_gw %>% 
  mutate(meetpunt = factor(meetpunt),
         groep3 = factor(groep3))

indic_abs_basis_gw <- indic_abs_basis_gw %>% 
  dplyr::select(-ends_with("fitted"))
modeldata <- indic_abs_basis_gw

modelkeuze <- data.frame(percentiel = c("01", "05", "10", "30" ), model = c("zeroinflatednbinomial2", "zeroinflatednbinomial0", "zeroinflatednbinomial0", "nbinomial"))
modelkeuze$model <- as.character(modelkeuze$model)

indic_abs_function_gw <- function(modeldata, respons, percentile, indicatorname, standardised) {
  
  prec.prior <- list(prec = list(param = c(0.001, 0.001)))
  model <- as.formula(paste(respons, "~", "f(jaar, model =", "'rw1', scale.model = TRUE,
                            hyper = prec.prior)+ f(meetpunt, model = 'iid', hyper = prec.prior)", sep = " "))   
  # model <- as.formula(paste(respons, "~", "1 + jaar_factor + f(meetpunt, model = 'iid', hyper = prec.prior)", sep = " "))     
  resultname_stat <- paste0("indic_abs_p", percentile,"_jaar_stat_gw", if (standardised == TRUE) ("_std"))
  resultname_fitted <- paste0("indic_abs_p", percentile,"_fitted_gw", if (standardised == TRUE) ("_std"))
  
  teller <- 0
  for (groep in seq(from = 2, to = 4)) {
    for (i in seq(from = 1, to = 20)) {
      #i <- 1
      #groep <- 3
      mdata <- modeldata[modeldata$simulatienr == i & modeldata$groep3n == groep,]

      # mdata[mdata[,respons] == 0,respons] <- 1e16
      print(i)    
      teller <- teller + 1
      print(teller)
      I2 <- inla(model, 
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 family = modelkeuze %>% filter(percentiel == percentile) %>% 
                   dplyr::pull(model), 
                 #Ntrials = aantaldagen_jaar,
                 data = mdata,
                 verbose = FALSE
      )
      
      result_stat_i <-  I2$summary.random$jaar %>% 
        mutate_at(names(I2$summary.random$jaar)[2:6], exp) %>%
        mutate_at(names(I2$summary.random$jaar)[2:6], function(x){x*exp(I2$summary.fixed$mean)}) %>% 
        mutate(simulatienr = i,
               groep3n = groep) %>% 
        dplyr::select(-sd, -mode, -kld)
  
      
      varname_mean <- paste0("p", percentile, "_mean", if (standardised == TRUE) ("_std"), "_fitted")
      varname_sd <- paste0("p", percentile, "_sd", if (standardised == TRUE) ("_std"), "_fitted")
      varname_p025 <- paste0("p", percentile, "_p02.5", if (standardised == TRUE) ("_std"), "_fitted")
      varname_p975 <- paste0("p", percentile, "_p97.5", if (standardised == TRUE) ("_std"), "_fitted")
      result_fitted_i <-  I2$summary.fitted.values %>% 
        rename(!!varname_mean := mean,
               !!varname_sd := sd,
               !!varname_p025 := '0.025quant',
               !!varname_p975 := '0.975quant')
      mdata <- bind_cols(mdata, result_fitted_i)
      if (standardised == TRUE) {
        varname_mean_backtransformed <- paste0(varname_mean, "_untr")
        varname_p025_backtransformed <- paste0(varname_p025, "_untr")
        varname_p975_backtransformed <- paste0(varname_p975, "_untr")
        result_fitted_i <- result_fitted_i %>% 
          mutate( !!varname_mean_backtransformed := unscale(result_fitted_i %>% 
                                                              dplyr::pull(!!varname_mean),m,s),
                  !!varname_p025_backtransformed := unscale(result_fitted_i %>% 
                                                              dplyr::pull(!!varname_p025),m,s),
                  !!varname_p975_backtransformed := unscale(result_fitted_i %>% 
                                                              dplyr::pull(!!varname_p975),m,s)    
          )
      }
      if (teller == 1) {
        result_stat <- result_stat_i
        result_fitted <- mdata
        
      } else {
        result_stat <- bind_rows(result_stat, 
                                 result_stat_i)
        result_fitted <- bind_rows(result_fitted, mdata)    
      }
      
      if (teller == 20 * 3) {  
        result_stat <- result_stat %>% 
          rename( jaar = ID,
                  p02.5 = '0.025quant',
                  p50 = '0.5quant',
                  p97.5 = '0.975quant')
        
        if (standardised == TRUE) {
          result_stat <- result_stat %>% 
            mutate(mean_untr = unscale(mean, m, s),
                   og_berekend_untr = unscale(og_berekend, m, s),
                   bg_berekend_untr = unscale(bg_berekend, m, s),
                   p02.5_untr = unscale(p02.5, m, s),
                   p97.5_untr = unscale(p97.5, m, s),
                   se_berekend_untr = ((p97.5_untr  - mean_untr )/1.96 - 
                                         (p02.5_untr  - mean_untr )/1.96)/2,
            )
        }
        resultlijst <- list( result_stat, 
                             result_fitted)
        names(resultlijst) <- c(resultname_stat, resultname_fitted)
        list2env(resultlijst, envir = .GlobalEnv)
        
        if (varname_mean %in% colnames(indic_abs_basis_gw)) {
          indic_abs_basis_gw <- indic_abs_basis_gw %>% 
            dplyr::select(-!!varname_mean, -!!varname_sd, -!!varname_p025, -!!varname_p975) 
        }
        
        indic_abs_basis_gw <- indic_abs_basis_gw %>% 
          inner_join(result_fitted %>% 
                       dplyr::select(contains("fitted"), 2:4), 
                     by = c("meetpunt","simulatienr", "jaar")) 
      } #if teller = 20*3
    } #for i
  } #for groep 
  return(indic_abs_basis_gw)
}

percentile <- "05"
indicatorname <- paste0("dag_onder_p",percentile)
indicatorname_inv <- paste0("dag_boven_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))

# debugonce("indic_abs_function_gw")

indic_abs_basis_gw <- indic_abs_function_gw(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

percentile <- "30"
indicatorname <- paste0("dag_onder_p",percentile)
indicatorname_inv <- paste0("dag_boven_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))

indic_abs_basis_gw <- indic_abs_function_gw(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)
# adde <- data.frame(groep3 = c(rep(2,680), rep(3,680), rep(4,680)))
# indic_abs_p05_jaar_stat_gw <- bind_cols(indic_abs_p05_jaar_stat_gw, adde)

#berekenen van indicator
indic_abs_gem_gw <- indic_abs_basis_gw %>% 
  group_by(jaar, groep3n) %>% 
  summarise_at(vars(dag_onder_p01:dag_onder_p30), list(~mean(.), ~median(.))) %>% 
  mutate(groep3n = factor(groep3n)) %>% 
  ungroup


indic_abs_p05_finaal_gw <- indic_abs_p05_jaar_stat_gw %>% 
  mutate(groep3n = factor(groep3n)) %>% 
  group_by(jaar, groep3n) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_abs_gem_gw, by = c("jaar", "groep3n"))

plotdata <- indic_abs_p05_finaal_gw %>% inner_join(gw_types_groups  %>% rename (gw = 'GT-groep: nummer') %>% mutate(gw = factor(gw)), by = c("groep3n" = "gw")) %>% rename(Grondwatertype = 'GT-groep: naam')

gplot <- ggplot(data = plotdata, aes(x = jaar, y = dag_onder_p05_mean, color = Grondwatertype)) + 
  # geom_line(aes(x = jaar, y = og_jaar), linetype = "longdash") +
  # geom_ribbon(aes(x = jaar, ymax = bg_jaar, ymin = og_jaar, fill = factor(groep3)), 
  #             #fill = grey(0.5),
  #             alpha = 0.4) +  
  # geom_line(aes(x = jaar, y = bg_jaar), linetype = "longdash") +
  geom_line() +
  # geom_point(data = indic_abs_basis, aes(x = jaar, y = dag_onder_p05)) +
  geom_hline(aes(yintercept = 18), linetype = "dotted") +
  scale_x_continuous(breaks = 1985:2020, labels = insert_minor(seq(1985, 2020, by = 5), 4)) +  
  labs(x = "Jaar", y = "trend")
png(paste0(file.path("figures", "indicator","dag_onder_p05"),"_trend_groep",".png"), width = 800)
gplot
dev.off()

fig <- plotfitting(indic_basis = indic_abs_basis_gw, respons = "dag_onder_p05", gemid = "p05_mean_fitted", og = p05_p02.5_fitted, bg = p05_p97.5_fitted)
png(paste0(file.path("figures", "indicator","dag_onder_p05"),"_tijdreeks_groep",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_abs_p05_finaal_gw, file.path("data", "result", "indic_abs_p05_finaal_groep"), sorting = c("jaar", "groep3n"), strict = FALSE)
write_vc(indic_abs_p05_jaar_stat_gw, file.path("data", "result", "indic_abs_p05_jaar_stat_groep"), sorting = c("jaar", "simulatienr", "groep3n"), strict = FALSE)

indic_abs_p30_finaal_gw <- indic_abs_p30_jaar_stat_gw %>% 
  mutate(groep3n = factor(groep3n)) %>% 
  group_by(jaar, groep3n) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_abs_gem_gw, by = c("jaar", "groep3n"))

plotdata <- indic_abs_p30_finaal_gw %>% inner_join(gw_types_groups  %>% rename (gw = 'GT-groep: nummer') %>% mutate(gw = factor(gw)), by = c("groep3n" = "gw")) %>% rename(Grondwatertype = 'GT-groep: naam')

gplot <- ggplot(data = plotdata, aes(x = jaar, y = dag_onder_p30_mean, color = Grondwatertype)) + 
  # geom_line(aes(x = jaar, y = og_jaar), linetype = "longdash") +
  # geom_ribbon(aes(x = jaar, ymax = bg_jaar, ymin = og_jaar, fill = factor(groep3)), 
  #             #fill = grey(0.5),
  #             alpha = 0.4) +  
  # geom_line(aes(x = jaar, y = bg_jaar), linetype = "longdash") +
  geom_line() +
  # geom_point(data = indic_abs_basis, aes(x = jaar, y = dag_onder_p05)) +
  geom_hline(aes(yintercept = 110), linetype = "dotted") +
  scale_x_continuous(breaks = 1985:2020, labels = insert_minor(seq(1985, 2020, by = 5), 4)) +  
  labs(x = "Jaar", y = "trend")
png(paste0(file.path("figures", "indicator","dag_onder_p30"),"_trend_groep",".png"), width = 800)
gplot
dev.off()

fig <- plotfitting(indic_basis = indic_abs_basis_gw, respons = "dag_onder_p30", gemid = "p30_mean_fitted", og = p30_p02.5_fitted, bg = p30_p97.5_fitted)
png(paste0(file.path("figures", "indicator","dag_onder_p30"),"_tijdreeks_groep",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_abs_p30_finaal_gw, file.path("data", "result", "indic_abs_p30_finaal_groep"), sorting = c("jaar", "groep3n"), strict = FALSE)
write_vc(indic_abs_p30_jaar_stat_gw, file.path("data", "result", "indic_abs_p30_jaar_stat_groep"), sorting = c("jaar", "simulatienr", "groep3n"), strict = FALSE)

#enkel voor zeer droge jaren
modeldata <- indic_abs_basis %>%
  dplyr::select(-ends_with("fitted")) %>% 
  filter (jaar %in% drogejaren_extreem)

percentile <- "05"
indicatorname <- paste0("dag_onder_p",percentile)
indicatorname_inv <- paste0("dag_boven_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))

indic_abs_basis_droog <- indic_abs_function(modeldata = modeldata, suffix = "droog", respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)
# debugonce("indic_abs_function")
# check <- indic_abs_function(model

indic_abs_p05_droog_finaal <- indic_abs_p05_droog_jaar_stat %>% 
  group_by(jaar) %>% 
  summarise(gem_jaar = mean(mean),
            # sd_jaar_biased = mean(sd),            
            gem_og = mean(p02.5),
            gem_bg = mean(p97.5),
            # sd_jaar = sd_jaar_biased + (20 + 1)/20 * sum((gem_jaar - mean)^2)/(20 - 1),
            # og_jaar = gem_jaar - 1.96 * sd_jaar,
            # bg_jaar = gem_jaar + 1.96 * sd_jaar
            og_jaar = gem_og - 1.96 * (20 + 1)/20 * sum((gem_og - p02.5)^2) / (20 - 1),
            bg_jaar = gem_bg + 1.96 * (20 + 1)/20 * sum((gem_bg - p97.5)^2) / (20 - 1)            
  ) %>% 
  ungroup() %>% 
  inner_join(indic_abs_gem, by = "jaar")


#plot van de trend
gplot <- plottrend(indic_abs_p05_droog_finaal,"dag_onder_p05_mean", 18)
png(paste0(file.path("figures", "indicator","dag_onder_p05_droog"),"_trend",".png"))
gplot
dev.off()

fig <- plotfitting(indic_basis = indic_abs_basis_droog, respons = "dag_onder_p05", gemid = "p05_mean_fitted", og = p05_p02.5_fitted, bg = p05_p97.5_fitted)
png(paste0(file.path("figures", "indicator","dag_onder_p05_droog"),"_tijdreeks",".png"))
fig
dev.off()

#bewaren resultaten
write_vc(indic_abs_p05_finaal, file.path("data", "result", "indic_abs_p05_finaal"), sorting = c("jaar"), strict = FALSE)
write_vc(indic_abs_p05_jaar_stat, file.path("data", "result", "indic_abs_p05_jaar_stat"), sorting = c("jaar", "simulatienr"), strict = FALSE)

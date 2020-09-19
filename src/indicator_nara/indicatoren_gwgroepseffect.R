indic_abs_basis_gw <- indic_abs_basis_gw %>% 
  dplyr::select(-contains("fitted") )
modeldata <- indic_abs_basis_gw %>% 
  mutate(groep3n = factor(groep3n))

modelkeuze <- data.frame(percentiel = c("01", "05", "10", "30" ), model = c("zeroinflatednbinomial2", "zeroinflatednbinomial0", "zeroinflatednbinomial0", "nbinomial"))
modelkeuze$model <- as.character(modelkeuze$model)

indic_abs_function_gw_model <- function(modeldata, respons, percentile, indicatorname, standardised) {
  
  prec.prior <- list(prec = list(param = c(0.001, 0.001)))
  model <- as.formula(paste(respons, "~", "1 + groep3n + f(meetpunt, model = 'iid', hyper = prec.prior)", sep = " "))
  modelfull <- as.formula(paste(respons, "~", "1 + groep3n + f(jaar, model =", "'rw1', scale.model = TRUE,
                 hyper = prec.prior) + f(meetpunt, model = 'iid', hyper = prec.prior)", sep = " "))
  # model <- as.formula(paste(respons, "~", "1 + jaar_factor + f(meetpunt, model = 'iid', hyper = prec.prior)", sep = " "))     
  model_gn_meetpunt <-  as.formula(paste(respons, "~", "1 + groep3n + f(jaar, model =", "'rw1', scale.model = TRUE,
                 hyper = prec.prior)", sep = " "))

  resultname_stat <- paste0("indic_abs_p", percentile,"_jaar_stat_gw", if (standardised == TRUE) ("_std"))
  resultname_fitted <- paste0("indic_abs_p", percentile,"_fitted_gw", if (standardised == TRUE) ("_std"))
  
  teller <- 0
    for (i in seq(from = 1, to = 20)) {
      #i <- 1
      #groep <- 3
      mdata <- modeldata[modeldata$simulatienr == i,]
      
      # mdata[mdata[,respons] == 0,respons] <- 1e16
      print(i)    
      teller <- teller + 1
      print(teller)
      I2b <- inla(model, 
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 family = modelkeuze %>% filter(percentiel == percentile) %>% 
                   dplyr::pull(model), 
                 #Ntrials = aantaldagen_jaar,
                 data = mdata,
                 verbose = FALSE
      )
      I2 <- inla(modelfull, 
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 family = modelkeuze %>% filter(percentiel == percentile) %>% 
                   dplyr::pull(model), 
                 #Ntrials = aantaldagen_jaar,
                 data = mdata,
                 verbose = FALSE
      )
      I2c <- inla(model_gn_meetpunt, 
                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                 family = modelkeuze %>% filter(percentiel == percentile) %>% 
                   dplyr::pull(model), 
                 #Ntrials = aantaldagen_jaar,
                 data = mdata,
                 verbose = FALSE
      )
      
      summary(I2)
      summary(I2b)
      summary(I2c)      
      modelkeuze %>% filter(percentiel == percentile) %>% dplyr::pull(model)

      sum(log(I2$cpo$cpo))
      sum(log(I2b$cpo$cpo))
      sum(log(I2c$cpo$cpo))
      sum(log(I2d$cpo$cpo))
      # sum(log(I2_binom$cpo$cpo))

      sum(log(I2$dic$dic))
      sum(log(I2b$dic$dic))
      sum(log(I2c$dic$dic))
      sum(log(I2d$dic$dic))

      sum(log(I2$waic$waic))
      sum(log(I2b$waic$waic))
      sum(log(I2c$waic$waic))
      sum(log(I2d$waic$waic))

      sum(I2$mlik)
      sum(I2b$mlik)
      sum(I2c$mlik)
      sum(I2d$mlik)

      #het volledige model (modelfull) scoort op vlak van dic en waic beter (maar niet cpo) dan het model zonder rekening te houden met de jaarlijkse fluctuaties
      
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

      #voor negatief binomiaal
      Pi  <- I2$summary.fitted.values[,"mean"]
      theta <- I2$summary.hyperpar[1,"mean"]
      E1 <- (mdata %>%
               dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
      sum(E1^2) / (nrow(mdata) - 1)

      Pi  <- I2b$summary.fitted.values[,"mean"]
      theta <- I2b$summary.hyperpar[1,"mean"]
      E1 <- (mdata %>%
               dplyr::pull(!!indicatorname)  - Pi) / sqrt(Pi + Pi^2 / theta)
      sum(E1^2) / (nrow(mdata) - 1)

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
      # Pearson residuals
      par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
      plot(x = Pi,
           y = E1,
           xlab = "Fitted values",
           ylab = "Pearson residuals")
      abline(h = 0, lty = 2)
      
      #names(inla.models()$likelihood)

      # bekijk de gefitte waarden van het model
      # Plot the fitted values
      Fit1     <- I2$summary.fitted.values[,"mean"]
      Fit1.025 <- I2$summary.fitted.values$"0.025quant"
      Fit1.975 <- I2$summary.fitted.values$"0.975quant"

      Fit1     <- I2b$summary.fitted.values[,"mean"]
      Fit1.025 <- I2b$summary.fitted.values$"0.025quant"
      Fit1.975 <- I2b$summary.fitted.values$"0.975quant"

      #
      # check <- I2$summary.random$meetpunt
      #
      # result_fitted_i
      mdata
      mdata$Fitted1  <- Fit1
      mdata$Fit1.025 <- Fit1.025
      mdata$Fit1.975 <- Fit1.975
      #gdata <- mdata %>% dplyr::select(!!respons, jaar, contains("Fit"))
      p <- ggplot(data = mdata, aes_string(y = respons, x = "groep3"))
      p <- p + xlab("Groep3") + ylab(respons)
      p <- p + theme(text = element_text(size=15))
      #p <- p + geom_point(shape = 16, size = 2, col = "black")
      p <- p + geom_line(aes(x = groep3, y = Fitted1))
      p <- p + geom_ribbon(data = mdata %>% group_by(groep3) %>% summarise(ymax_data = max(Fit1.975), ymin_data = min(Fit1.025)), aes(x = groep3,
                               ymax = ymax_data,
                               ymin = ymin_data), inherit.aes = F,
                           fill = grey(0.5),
                           alpha = 0.4)
      p <- p + theme(strip.text = element_text(size = 15))
      p
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

      
            
      result_stat_i <-  I2$summary.random$jaar %>% 
        mutate_at(names(I2$summary.random$jaar)[2:6], exp) %>%
        mutate_at(names(I2$summary.random$jaar)[2:6], function(x){x*exp(I2$summary.fixed$mean)}) %>% 
        mutate(simulatienr = i,
               groep3 = groep) %>% 
        dplyr::select(-sd, -mode, -kld)
      
        check_meetpunt <-  I2$summary.random$meetpunt %>% 
        mutate_at(names(I2$summary.random$meetpunt)[2:6], exp) %>%
        mutate_at(names(I2$summary.random$meetpunt)[2:6], function(x){x*exp(I2$summary.fixed$mean)}) %>% 
        mutate(simulatienr = i,
               groep3 = groep) %>% 
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
                       dplyr::select(contains("fitted"), 1:3), 
                     by = c("meetpunt","simulatienr", "jaar")) 
      } #if teller = 20*3
    } #for i
  #} #for groep 
  return(indic_abs_basis_gw)
}


percentile <- "05"
indicatorname <- paste0("dag_onder_p",percentile)
indicatorname_inv <- paste0("dag_boven_p",percentile)
standardised <- FALSE
respons <- paste0(indicatorname, if (standardised == TRUE) ("_std"))

# debugonce("indic_abs_function_gw")

indic_abs_basis_gw_zonderjaar <- indic_abs_function_gw_model(modeldata = modeldata, respons = respons, percentile = percentile, indicatorname = indicatorname, standardised = standardised)

check_meetpunt2 <- indic_abs_basis %>% 
  dplyr::select(1:3,6,13,18:21) %>% 
  arrange (jaar, simulatienr)

plotdata <- check_meetpunt2 %>% 
  filter(check_meetpunt2$meetpunt  %in% levels(check_meetpunt2$meetpunt)[11:16]) %>% 
  filter(simulatienr == 1)
checkmpt_plot <- ggplot(data = plotdata, aes(x = jaar, y = dag_onder_p05, color = meetpunt))+
  geom_line()
checkmpt_plot
dev.off()

check_meetpunt2_stat <- check_meetpunt2 %>% 
  group_by(jaar) %>% 
  summarise(gemwaarde = mean(dag_onder_p05))
check_gem <- indic_abs_p05_finaal %>% 
  inner_join(check_meetpunt2_stat, by = "jaar")

referentie <- 21
drogejaren <- indic_abs_p05_finaal %>% 
  filter (og_jaar > referentie) %>% 
  dplyr::pull(jaar)

drogejaren_extreem <- indic_abs_p05_finaal %>% 
  filter (og_jaar > referentie*2) %>% 
  dplyr::pull(jaar)

modeldata <- indic_abs_basis_gw %>% 
  filter (jaar %in% drogejaren)

modeldata <- indic_abs_basis_gw %>% 
  filter (jaar %in% drogejaren_extreem)

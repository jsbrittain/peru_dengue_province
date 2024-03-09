#TRAINING-ONLY DATA.TABLE ----
training_ptl_province_df <- subset(ptl_province_df,
                                   YEAR < 2018)



#Setting up candidate baseline formulae for Bayesian modelling -----

#Yearly Effect at province-level
baseline_formula_province <-  CASES ~ 1 + 
  f(MONTH, replicate = PROV_IND,
    model = "rw1", cyclic = TRUE, constr = TRUE,
    scale.model = TRUE,  hyper = prior.prec)+
  f(YEAR, replicate = PROV_IND, model = "iid")+
  f(PROV_IND, model = "bym2",
    hyper = prior.prec, scale.model = TRUE,
    graph = file.path(peru.province.inla.data.in.dir, "nbr_piura_tumbes_lambayeque.graph")
  )+
  SEASON+SQ_RSI_DIR_LAG

#Yearly Effect at region-level
baseline_formula_province_yrly_regional <-  CASES ~ 1 + 
  f(MONTH, replicate = PROV_IND,
    model = "rw1", cyclic = TRUE, constr = TRUE,
    scale.model = TRUE,  hyper = prior.prec)+
  f(YEAR, replicate = RGN_IND, model = "iid")+
  f(PROV_IND, model = "bym2",
    hyper = prior.prec, scale.model = TRUE,
    graph = file.path(peru.province.inla.data.in.dir, "nbr_piura_tumbes_lambayeque.graph")
  )+
  SEASON+SQ_RSI_DIR_LAG

#Monthly Effect at region-level
baseline_formula_province_monthly_regional <-  CASES ~ 1 + 
  f(MONTH, replicate = RGN_IND,
    model = "rw1", cyclic = TRUE, constr = TRUE,
    scale.model = TRUE,  hyper = prior.prec)+
  f(YEAR, replicate = PROV_IND, model = "iid")+
  f(PROV_IND, model = "bym2",
    hyper = prior.prec, scale.model = TRUE,
    graph = file.path(peru.province.inla.data.in.dir, "nbr_piura_tumbes_lambayeque.graph")
  )+
  SEASON+SQ_RSI_DIR_LAG


#Nearest Neighbour Effect
baseline_formulae_province_nn_effect <-  CASES ~ 1 + 
  f(MONTH, replicate = PROV_IND,
    model = "rw1", cyclic = TRUE, constr = TRUE,
    scale.model = TRUE,  hyper = prior.prec)+
  f(YEAR, replicate = PROV_IND, model = "iid")+
  f(PROV_IND, model = "bym2",
    hyper = prior.prec, scale.model = TRUE,
    graph = file.path(peru.province.inla.data.in.dir, "nbr_piura_tumbes_lambayeque.graph")
  )+
  SEASON+SQ_RSI_DIR_LAG + NN_DIR_LAG


#Yearly Effect at province-level with urban
baseline_formula_province_urban <-  CASES ~ 1 + 
  f(MONTH, replicate = PROV_IND,
    model = "rw1", cyclic = TRUE, constr = TRUE,
    scale.model = TRUE,  hyper = prior.prec)+
  f(YEAR, replicate = PROV_IND, model = "iid")+
  f(PROV_IND, model = "bym2",
    hyper = prior.prec, scale.model = TRUE,
    graph = file.path(peru.province.inla.data.in.dir, "nbr_piura_tumbes_lambayeque.graph")
  )+
  SEASON+SQ_RSI_DIR_LAG + PROPN_URBAN_2017

#Difference with lagged historical DIR
baseline_formulae_province_urban_fixed_historical <-  CASES ~ 1 + f(MONTH, replicate = PROV_IND, model = "rw1", cyclic = TRUE, 
                                                              constr = TRUE, scale.model = TRUE, hyper = prior.prec) + 
  f(YEAR, replicate = PROV_IND, model = "iid") + f(PROV_IND, 
                                                   model = "bym2", hyper = prior.prec, scale.model = TRUE, graph = file.path(peru.province.inla.data.in.dir, 
                                                                                                                             "nbr_piura_tumbes_lambayeque.graph")) + SQ_RSI_DIR_LAG + 
  SEASON + PROPN_URBAN_2017 + MODIFIED_DIFF_WITH_HISTORICAL_DIR_LAG



ptl_zi_province_formulae <- c(baseline_formula_province,
                              baseline_formula_province_yrly_regional,
                              baseline_formula_province_monthly_regional,
                              baseline_formulae_province_nn_effect,
                              baseline_formula_province_urban,
                              baseline_formulae_province_urban_fixed_historical)



run_province_inla_model <- function(formulae, family, data)
{
  setkeyv(data, c("TIME", "PROVINCE"))
  inla_base_mod_summary <- data.table(MOD = paste0("Mod_", seq(1, length(formulae))),
                                      DIC = rep(0, length(formulae)),
                                      WAIC = rep(0, length(formulae)),
                                      p_eff = rep(0, length(formulae)))
  verb <- c(FALSE, FALSE, TRUE)
  for(i in 1:length(formulae)){
    print(paste0("We're now on Model ", i))
    formula_in_q <- formulae[[i]]
    if(family == "nbinomial")
    {inla_mod_fit <- inla(formula = formula_in_q, 
                          data = data, family = family, offset = log(POP_OFFSET),
                          verbose = FALSE,
                          control.inla = list(strategy = 'adaptive'), 
                          control.compute = list(waic = TRUE, dic = TRUE, 
                                                 cpo = TRUE, config = TRUE,
                                                 return.marginals = TRUE),
                          control.fixed = list(correlation.matrix = TRUE, 
                                               prec.intercept = 1, prec = 1),
                          control.predictor = list(link = 1, compute = TRUE), 
    )
    }
    else if(family == "zeroinflatednbinomial0")
    {inla_mod_fit <- inla(formula = formula_in_q, 
                          data = data, family = family, offset = log(POP_OFFSET),
                          verbose = FALSE,
                          control.inla = list(strategy = 'adaptive'), 
                          control.compute = list(waic = TRUE, dic = TRUE, 
                                                 cpo = TRUE, config = TRUE,
                                                 return.marginals = TRUE),
                          control.fixed = list(correlation.matrix = TRUE, 
                                               prec.intercept = 1, prec = 1),
                          control.predictor = list(link = 1, compute = TRUE), 
    )
    }
    else if(family == "zeroinflatedpoisson0")
    {inla_mod_fit <- inla(formula = formula_in_q, 
                          data = data, family = family, offset = log(POP_OFFSET),
                          verbose = FALSE,
                          control.inla = list(strategy = 'adaptive',
                                              cmin = 0), 
                          control.family = list(link = "log"),
                          control.compute = list(waic = TRUE, dic = TRUE, 
                                                 cpo = TRUE, config = TRUE,
                                                 return.marginals = TRUE),
                          control.fixed = list(correlation.matrix = TRUE, 
                                               prec.intercept = 1, prec = 1),
                          control.predictor = list(link = 1, compute = TRUE) ) 
    
    }
    inla_mod_fit <- inla.rerun(inla_mod_fit)

    inla_base_mod_summary[which(MOD == paste0("Mod_", i)),
                          DIC:= inla_mod_fit$dic$dic]
    inla_base_mod_summary[which(MOD == paste0("Mod_", i)),
                          WAIC:= inla_mod_fit$waic$waic]
    inla_base_mod_summary[which(MOD == paste0("Mod_", i)),
                          p_eff:= inla_mod_fit$waic$p.eff]
    inla_base_mod_summary[which(MOD == paste0("Mod_", i)),
                          MAE:= 
                            caret::MAE(inla_mod_fit$summary.fitted.values$`0.5quant`,
                                       data$DIR)]
  }
  return(inla_base_mod_summary)
}


#Choosing baseline formula for Bayesian model 
paper_ptl_zi_province_baseline_mods_summary <- run_province_inla_model(formulae = ptl_zi_province_formulae,
                                                                      family = "zeroinflatedpoisson0",
                                                                      data = training_ptl_province_df)
paper_ptl_zi_province_baseline_mods_summary <- paper_ptl_zi_province_baseline_mods_summary[order(WAIC, decreasing = FALSE),]
paper_ptl_zi_province_baseline_mods_summary[order(DIC, decreasing = FALSE),]
paper_ptl_zi_province_baseline_mods_summary <- paper_ptl_zi_province_baseline_mods_summary[which(MOD!= "Mod_1")]
paper_ptl_zi_province_baseline_mods_summary[order(LOG_SCORE, decreasing = FALSE),]

baseline_formula_province <- CASES ~ 1 +  f(MONTH, replicate = PROV_IND, model = "rw1", cyclic = TRUE, 
                                            constr = TRUE, scale.model = TRUE, hyper = prior.prec) + 
  f(YEAR, replicate = PROV_IND, model = "iid") + f(PROV_IND, 
                                                   model = "bym2", hyper = prior.prec, scale.model = TRUE, graph = file.path(peru.province.inla.data.in.dir, 
                                                                                                                             "nbr_piura_tumbes_lambayeque.graph")) + SQ_RSI_DIR_LAG + 
  SEASON + PROPN_URBAN_2017 + ns(MODIFIED_DIFF_WITH_HISTORICAL_DIR_LAG, 
                                 df = 4)







#Cross basis matrices----
#Via the cross basis (a bi-dimensional functional space) we are specifying simultaneously the 
# relationships in the dimensions of the predictor and lags, respectively.

lagknot <- c(1, 2) #Knots in lag dimension

#Specify how many lags want for each climatic variable's DLNM (using exploratory analysis)
lag_tmax <- lag_tmax[, 1:4]
lag_prec <- lag_prec[, 1:4]
lag_tmin <- lag_tmin[, 1:4]
lag_spi <- lag_spi[, 1:3]
lag_icen <- lag_icen[, 1:5]
lag_oni <- lag_oni[, 1:5]
lag_tmax_roll_2 <- lag_tmax_roll_2[, 1:4]
lag_tmin_roll_2 <- lag_tmin_roll_2[, 1:4]
lag_prec_roll_2 <- lag_prec_roll_2[, 1:3]

tmax_basis <- crossbasis(lag_tmax, 
                         argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmax, 2)),
                         arglag=list(fun="bs", knots =lagknot))
tmin_basis <- crossbasis(lag_tmin, 
                         argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmin, 2)),
                         arglag=list(fun="bs", knots =lagknot))

prec_basis <- crossbasis(lag_prec, 
                         argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$prec, 2)),
                         arglag=list(fun="bs", knots = lagknot))

spi_basis <- crossbasis(lag_spi, 
                        argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$SPI_6, 2)),
                        arglag=list(fun="bs", knots = 1))

oni_basis <- crossbasis(lag_oni,  
                        argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$ANOM, 2)),
                        arglag=list(fun="bs", knots = 2))
icen_basis <- crossbasis(lag_icen,  
                         argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$E_INDEX, 2)),
                         arglag=list(fun="bs", knots = 2))
tmax_roll_2_basis <- crossbasis(lag_tmax_roll_2, 
                                argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmax_roll_2, 2)),
                                arglag=list(fun="bs", knots =lagknot))
tmin_roll_2_basis <- crossbasis(lag_tmin_roll_2, 
                                argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmin_roll_2, 2)),
                                arglag=list(fun="bs", knots =lagknot))
prec_roll_2_basis <- crossbasis(lag_prec_roll_2, 
                                argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$prec_roll_2, 2)),
                                arglag=list(fun="bs", knots =lagknot))
colnames(tmax_basis) = paste0("tmax_basis.", colnames(tmax_basis))
colnames(spi_basis) = paste0("spi_basis.", colnames(spi_basis))
colnames(tmin_basis) = paste0("tmin_basis.", colnames(tmin_basis))
colnames(prec_basis) = paste0("prec_basis.", colnames(prec_basis))
colnames(oni_basis) = paste0("oni_basis.", colnames(oni_basis))
colnames(icen_basis) = paste0("icen_basis.", colnames(icen_basis))
colnames(tmax_roll_2_basis) = paste0("tmax_roll_2_basis.", colnames(tmax_roll_2_basis))
colnames(tmin_roll_2_basis) = paste0("tmin_roll_2_basis.", colnames(tmin_roll_2_basis))
colnames(prec_roll_2_basis) = paste0("prec_roll_2_basis.", colnames(prec_roll_2_basis))





#Climate formulae ----
climate_ptl_province_formula_tmax <- update.formula(baseline_formula_province, ~. + tmax_basis)
climate_ptl_province_formula_tmin <- update.formula(baseline_formula_province, ~. + tmin_basis)
climate_ptl_province_formula_prec <- update.formula(baseline_formula_province, ~. + prec_basis)
climate_ptl_province_formula_spi <- update.formula(baseline_formula_province, ~. + spi_basis)


climate_ptl_province_formula_tmax_roll_2 <- update.formula(baseline_formula_province, ~. + tmax_roll_2_basis)
climate_ptl_province_formula_tmin_roll_2 <- update.formula(baseline_formula_province, ~. + tmin_roll_2_basis)
climate_ptl_province_formula_prec_roll_2 <- update.formula(baseline_formula_province, ~. + prec_roll_2_basis)

climate_ptl_province_formula_oni <- update.formula(baseline_formula_province, ~. + oni_basis)
climate_ptl_province_formula_icen <- update.formula(baseline_formula_province, ~. + icen_basis)

climate_ptl_province_formula_tmax_prec <- update.formula(baseline_formula_province, ~. + tmax_basis + prec_basis)
climate_ptl_province_formula_tmin_prec <- update.formula(baseline_formula_province, ~. + tmin_basis + prec_basis)
climate_ptl_province_formula_icen_prec <- update.formula(baseline_formula_province, ~. + icen_basis+ prec_basis)

climate_ptl_province_formula_tmax_spi <- update.formula(baseline_formula_province, ~. + tmax_basis + spi_basis)
climate_ptl_province_formula_tmin_spi <- update.formula(baseline_formula_province, ~. + tmin_basis + spi_basis)
climate_ptl_province_formula_icen_spi <- update.formula(baseline_formula_province, ~. + icen_basis+ spi_basis)


climate_ptl_province_formula_tmax_icen <- update.formula(baseline_formula_province, ~. + tmax_basis + icen_basis)
climate_ptl_province_formula_tmin_icen <- update.formula(baseline_formula_province, ~. + tmin_basis + icen_basis)

climate_ptl_province_formula_tmax_oni <- update.formula(baseline_formula_province, ~. + tmax_basis + oni_basis)
climate_ptl_province_formula_tmin_oni <- update.formula(baseline_formula_province, ~. + tmin_basis + oni_basis)
climate_ptl_province_formula_prec_oni <- update.formula(baseline_formula_province, ~. + tmin_basis + oni_basis)

climate_ptl_province_formula_tmin_oni_prec <- update.formula(baseline_formula_province, ~. + tmin_basis + oni_basis + prec_basis)
climate_ptl_province_formula_tmax_oni_prec <- update.formula(baseline_formula_province, ~. + tmax_basis + oni_basis + prec_basis)

climate_ptl_province_formula_tmin_spi_icen <- update.formula(baseline_formula_province, ~. + tmin_basis + spi_basis + icen_basis)
climate_ptl_province_formula_tmax_spi_icen <- update.formula(baseline_formula_province, ~. + tmax_basis + spi_basis + icen_basis)


climate_ptl_province_formula_tmin_prec_icen <- update.formula(baseline_formula_province, ~. + tmin_basis + prec_basis+icen_basis)
climate_ptl_province_formula_tmax_prec_icen <- update.formula(baseline_formula_province, ~. + tmax_basis + prec_basis+icen_basis)


climate_ptl_province_formula_tmin_roll_2_prec_roll_2_icen <- update.formula(baseline_formula_province, ~. + tmin_roll_2_basis + prec_roll_2_basis+icen_basis)
climate_ptl_province_formula_tmax_roll_2_prec_roll_2_icen <- update.formula(baseline_formula_province, ~. + tmax_roll_2_basis + prec_roll_2_basis+icen_basis)

climate_ptl_province_formula_tmin_prec_icen_spi <- update.formula(baseline_formula_province, ~. + tmin_basis + prec_basis+icen_basis+
                                                                    spi_basis)
climate_ptl_province_formula_tmax_prec_icen_spi <- update.formula(baseline_formula_province, ~. + tmax_basis + prec_basis+icen_basis+
                                                                    spi_basis)

climate_ptl_province_formula_tmin_roll_2_prec_roll_2_icen_spi <- update.formula(baseline_formula_province, ~. + tmin_roll_2_basis + prec_roll_2_basis+icen_basis+
                                                                                  spi_basis)

climate_ptl_province_formula_tmax_roll_2_prec_roll_2_icen_spi <- update.formula(baseline_formula_province, ~. + tmax_roll_2_basis + prec_roll_2_basis+icen_basis+
                                                                                  spi_basis)

climate_ptl_province_formula_tmin_prec_oni_spi <- update.formula(baseline_formula_province, ~. + tmin_basis + prec_basis+oni_basis+
                                                                   spi_basis)
climate_ptl_province_formula_tmax_prec_oni_spi <- update.formula(baseline_formula_province, ~. + tmax_basis + prec_basis+oni_basis+
                                                                   spi_basis)

climate_ptl_province_formula_tmin_roll_2_prec_roll_2_oni_spi <- update.formula(baseline_formula_province, ~. + tmin_roll_2_basis + prec_roll_2_basis+oni_basis+
                                                                                 spi_basis)
climate_ptl_province_formula_tmax_roll_2_prec_roll_2_oni_spi <- update.formula(baseline_formula_province, ~. + tmax_roll_2_basis + prec_roll_2_basis+oni_basis+
                                                                                 spi_basis)

climate_ptl_province_formulae <- c(climate_ptl_province_formula_tmax,climate_ptl_province_formula_tmin,
                                   climate_ptl_province_formula_prec,climate_ptl_province_formula_spi,
                                   climate_ptl_province_formula_tmax_roll_2,
                                   climate_ptl_province_formula_tmin_roll_2,
                                   climate_ptl_province_formula_prec_roll_2,
                                   climate_ptl_province_formula_oni, climate_ptl_province_formula_icen,
                                   climate_ptl_province_formula_tmax_prec, climate_ptl_province_formula_tmin_prec, 
                                   climate_ptl_province_formula_icen_prec, 
                                   climate_ptl_province_formula_tmax_spi,
                                   climate_ptl_province_formula_tmin_spi, 
                                   climate_ptl_province_formula_icen_spi,
                                   climate_ptl_province_formula_tmax_icen,
                                   climate_ptl_province_formula_tmin_icen,climate_ptl_province_formula_tmax_oni,climate_ptl_province_formula_tmin_oni,
                                   climate_ptl_province_formula_prec_oni,climate_ptl_province_formula_tmin_oni_prec,
                                   climate_ptl_province_formula_tmax_oni_prec,
                                   climate_ptl_province_formula_tmin_spi_icen,
                                   climate_ptl_province_formula_tmax_spi_icen, 
                                   climate_ptl_province_formula_tmin_prec_icen,
                                   climate_ptl_province_formula_tmax_prec_icen,
                                   climate_ptl_province_formula_tmin_roll_2_prec_roll_2_icen,
                                   climate_ptl_province_formula_tmax_roll_2_prec_roll_2_icen,
                                   climate_ptl_province_formula_tmin_prec_icen_spi,  
                                   climate_ptl_province_formula_tmin_prec_oni_spi,
                                   climate_ptl_province_formula_tmax_prec_oni_spi,
                                   climate_ptl_province_formula_tmin_roll_2_prec_roll_2_icen_spi,
                                   climate_ptl_province_formula_tmax_roll_2_prec_roll_2_icen_spi,
                                   climate_ptl_province_formula_tmax_prec_icen_spi)


climate_ptl_province_summary <- run_province_inla_model(formulae = climate_ptl_province_formulae,
                                                                   family = "zeroinflatedpoisson0",
                                                                   data = ptl_province_inla_df)

head(climate_ptl_province_summary[order(DIC, decreasing = FALSE)], 15)
head(climate_ptl_province_summary[order(MAE, decreasing = FALSE)], 10)
head(climate_ptl_province_summary[order(LOG_SCORE, decreasing = FALSE)], 15) 
head(climate_ptl_province_summary[order(WAIC, decreasing = FALSE)], 10) 
head(climate_ptl_province_summary[order(p_eff, decreasing = FALSE)], 10)








#Plotting for chosen climate-based model ----
#Set up forecasting model during training window: 2010-2017 inclusive
province_first_time_2018 <- head(ptl_province_inla_df[which(YEAR == 2018), ]$TIME, 1)-1
provinces <- unique(ptl_province_inla_df$PROVINCE)
num_provinces <- length(provinces)


#Run model for 2010-2017 inclusive----
#Leave-future-out forecasting
for(i in 1:(province_first_time_2018 - 1)){
  print(i)
  idx.pred <- seq(i*num_provinces+1, i*num_provinces+num_provinces)
  s <- 5000 #Number of posterior samples
  historical_rt_forecast_dt <- data.table(ptl_province_inla_df)
  setkeyv(historical_rt_forecast_dt, c("TIME", "PROVINCE"))
  historical_rt_forecast_dt[, IND:= seq(1, nrow(historical_rt_forecast_dt))]
  true_cases <- c(historical_rt_forecast_dt[which(TIME == i + 1), ]$CASES)
  true_dirs <- c(historical_rt_forecast_dt[which(TIME == i + 1), ]$DIR)
  pop_offsets <- c(historical_rt_forecast_dt[which(TIME == i + 1), ]$POP_OFFSET)
  
  
  max_ind <- max(historical_rt_forecast_dt[which(TIME == i + 1),]$IND)
  historical_rt_forecast_dt <- historical_rt_forecast_dt[c(1:max_ind),]
  historical_rt_forecast_dt[which(TIME == i + 1), CASES:= NA]
  historical_rt_forecast_dt[which(TIME == i + 1), DIR:= NA]
  setkeyv(historical_rt_forecast_dt, c("TIME", "PROVINCE"))
  
  
  
  # Precipitation (prec)
  setkeyv(ptl_climate_dt_province, c("YEAR", "MONTH","PROVINCE"))
  
  historical_rt_forecast_lag_tmax <- tsModel::Lag(ptl_climate_dt_province$tmax, 
                                       group = ptl_climate_dt_province$PROVINCE, 
                                       k = 1:maximum_climate_lag)
  historical_rt_forecast_lag_tmax <- historical_rt_forecast_lag_tmax[57:nrow(historical_rt_forecast_lag_tmax),]
  historical_rt_forecast_lag_tmax <- historical_rt_forecast_lag_tmax[1:nrow(historical_rt_forecast_dt), 1:3]
  
  # tmin
  historical_rt_forecast_lag_tmin <- tsModel::Lag(ptl_climate_dt_province$tmin, 
                                       group = ptl_climate_dt_province$PROVINCE, 
                                       k = 1:maximum_climate_lag)
  historical_rt_forecast_lag_tmin <- historical_rt_forecast_lag_tmin[57:nrow(historical_rt_forecast_lag_tmin),]
  historical_rt_forecast_lag_tmin <- historical_rt_forecast_lag_tmin[1:nrow(historical_rt_forecast_dt), 1:3]
  
  # Precipitation (prec)
  historical_rt_forecast_lag_prec <- tsModel::Lag(ptl_climate_dt_province$prec, 
                                       group = ptl_climate_dt_province$PROVINCE, 
                                       k = 1:maximum_climate_lag)
  historical_rt_forecast_lag_prec <- historical_rt_forecast_lag_prec[57:nrow(historical_rt_forecast_lag_prec),]
  historical_rt_forecast_lag_prec <- historical_rt_forecast_lag_prec[1:nrow(historical_rt_forecast_dt), 1:3]
  
  historical_rt_forecast_lag_tmax_roll_2 <- tsModel::Lag(ptl_climate_dt_province$tmax_roll_2, 
                                              group = ptl_climate_dt_province$PROVINCE, 
                                              k = 1:maximum_climate_lag)
  historical_rt_forecast_lag_tmax_roll_2 <- historical_rt_forecast_lag_tmax_roll_2[57:nrow(historical_rt_forecast_lag_tmax_roll_2),]
  historical_rt_forecast_lag_tmax_roll_2 <- historical_rt_forecast_lag_tmax_roll_2[1:nrow(historical_rt_forecast_dt), 1:3]
  
  # tmin_roll_2
  historical_rt_forecast_lag_tmin_roll_2 <- tsModel::Lag(ptl_climate_dt_province$tmin_roll_2, 
                                              group = ptl_climate_dt_province$PROVINCE, 
                                              k = 1:maximum_climate_lag)
  historical_rt_forecast_lag_tmin_roll_2 <- historical_rt_forecast_lag_tmin_roll_2[57:nrow(historical_rt_forecast_lag_tmin_roll_2),]
  historical_rt_forecast_lag_tmin_roll_2 <- historical_rt_forecast_lag_tmin_roll_2[1:nrow(historical_rt_forecast_dt), 1:3]
  
  # prec_roll_2ipitation (prec_roll_2)
  historical_rt_forecast_lag_prec_roll_2 <- tsModel::Lag(ptl_climate_dt_province$prec_roll_2, 
                                              group = ptl_climate_dt_province$PROVINCE, 
                                              k = 1:maximum_climate_lag)
  historical_rt_forecast_lag_prec_roll_2 <- historical_rt_forecast_lag_prec_roll_2[57:nrow(historical_rt_forecast_lag_prec_roll_2),]
  historical_rt_forecast_lag_prec_roll_2 <- historical_rt_forecast_lag_prec_roll_2[1:nrow(historical_rt_forecast_dt), 1:3]
  # SPI-6
  historical_rt_forecast_lag_spi <- tsModel::Lag(ptl_climate_dt_province$SPI_6, 
                                      group = ptl_climate_dt_province$PROVINCE, 
                                      k = 1:maximum_climate_lag)
  historical_rt_forecast_lag_spi <- historical_rt_forecast_lag_spi[57:nrow(historical_rt_forecast_lag_spi),]
  historical_rt_forecast_lag_spi <- historical_rt_forecast_lag_spi[1:nrow(historical_rt_forecast_dt), 1:2]
  
  
  # ONI
  historical_rt_forecast_lag_oni <- tsModel::Lag(ptl_climate_dt_province$ANOM, 
                                      k = 1:maximum_climate_lag)
  historical_rt_forecast_lag_oni <- historical_rt_forecast_lag_oni[57:nrow(historical_rt_forecast_lag_oni),]
  historical_rt_forecast_lag_oni <- historical_rt_forecast_lag_oni[1:nrow(historical_rt_forecast_dt), 1:4]
  
  #ICEN
  historical_rt_forecast_lag_icen <- tsModel::Lag(ptl_climate_dt_province$E_INDEX, 
                                       k = 1:maximum_climate_lag)
  historical_rt_forecast_lag_icen <- historical_rt_forecast_lag_icen[57:nrow(historical_rt_forecast_lag_icen),]
  historical_rt_forecast_lag_icen <- historical_rt_forecast_lag_icen[1:nrow(historical_rt_forecast_dt), 1:4]
  
  lagknot <- c(1, 2)
  tmax_basis <- crossbasis(historical_rt_forecast_lag_tmax, 
                           argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmax, 2)),
                           arglag=list(fun="bs", knots =lagknot))
  tmin_basis <- crossbasis(historical_rt_forecast_lag_tmin, 
                           argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmin, 2)),
                           arglag=list(fun="bs", knots =lagknot))
  
  prec_basis <- crossbasis(historical_rt_forecast_lag_prec, 
                           argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$prec, 2)),
                           arglag=list(fun="bs", knots = lagknot))
  
  tmax_roll_2_basis <- crossbasis(historical_rt_forecast_lag_tmax_roll_2, 
                                  argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmax_roll_2, 2)),
                                  arglag=list(fun="bs", knots =lagknot))
  tmin_roll_2_basis <- crossbasis(historical_rt_forecast_lag_tmin_roll_2, 
                                  argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmin_roll_2, 2)),
                                  arglag=list(fun="bs", knots =lagknot))
  
  prec_roll_2_basis <- crossbasis(historical_rt_forecast_lag_prec_roll_2, 
                                  argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$prec_roll_2, 2)),
                                  arglag=list(fun="bs", knots = lagknot))
  
  spi_basis <- crossbasis(historical_rt_forecast_lag_spi, 
                          argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$SPI_6, 2)),
                          arglag=list(fun="bs", knots = 1))
  
  oni_basis <- crossbasis(historical_rt_forecast_lag_oni,  
                          argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$ANOM, 2)),
                          arglag=list(fun="bs", knots = 2))
  icen_basis <- crossbasis(historical_rt_forecast_lag_icen,  
                           argvar=list(fun = "bs", knots = equalknots(ptl_climate_dt_province$E_INDEX, 2)),
                           arglag=list(fun="bs", knots = 2))
  
  colnames(tmax_basis) = paste0("tmax_basis.", colnames(tmax_basis))
  colnames(tmin_basis) = paste0("tmin_basis.", colnames(tmin_basis))
  colnames(prec_basis) = paste0("prec_basis.", colnames(prec_basis))
  colnames(spi_basis) = paste0("spi_basis.", colnames(spi_basis))
  colnames(oni_basis) = paste0("oni_basis.", colnames(oni_basis))
  colnames(icen_basis) = paste0("icen_basis.", colnames(icen_basis))
  colnames(tmax_roll_2_basis) = paste0("tmax_roll_2_basis.", colnames(tmax_roll_2_basis))
  colnames(tmin_roll_2_basis) = paste0("tmin_roll_2_basis.", colnames(tmin_roll_2_basis))
  colnames(prec_roll_2_basis) = paste0("prec_roll_2_basis.", colnames(prec_roll_2_basis))
  

  forecast_formula <- CASES ~ 1 + f(MONTH, replicate = PROV_IND, model = "rw1", cyclic = TRUE, 
                                    constr = TRUE, scale.model = TRUE, hyper = prior.prec) + 
    f(YEAR, replicate = PROV_IND, model = "iid") + f(PROV_IND, 
                                                     model = "bym2", hyper = prior.prec, scale.model = TRUE, graph = file.path(peru.province.inla.data.in.dir, 
                                                                                                                               "nbr_piura_tumbes_lambayeque.graph")) + 
    SQ_RSI_DIR_LAG + 
    SEASON + ns(MODIFIED_DIFF_WITH_HISTORICAL_DIR_LAG, df = 4) + 
    tmin_roll_2_basis + prec_roll_2_basis + icen_basis + spi_basis
  
  tmp_climate_cv_fit <- run_province_model_func(data = historical_rt_forecast_dt, formula = forecast_formula)
  
  xx <- inla.posterior.sample(s, tmp_climate_cv_fit)

  xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor[idx.pred]), xx)
  y.pred <- matrix(NA, num_provinces, s)
  dir.pred <- matrix(NA, num_provinces, s)
  for(s.idx in 1:s) {
    xx.sample <- xx.s[, s.idx]
    
    y.pred[, s.idx] <- rzipois(num_provinces, lambda = exp(xx.sample[-1]), pstr0 = xx.sample[1])
    dir.pred[, s.idx] <- y.pred[, s.idx]/pop_offsets
  }
  saveRDS(dir.pred, file = file.path(peru.province.inla.data.out.dir, paste0("zi_pois_season_sq_rsi_ns_mod_historical_tmin_roll_2_prec_roll_2_spi_icen_historical_dir.pred", i, ".RDS")))
}


#Save Results ----
historical_all_dir.pred <- NULL
for(i in 1:(province_first_time_2018 - 1)){
  historical_province_rt_forecast_preds_dt <- 
    readRDS(file = file.path(peru.province.inla.data.out.dir, 
                             paste0("zi_pois_season_sq_rsi_ns_mod_historical_tmin_roll_2_prec_roll_2_spi_icen_historical_rt_forecast", i, ".RDS")))
  tmp_dir.pred <- readRDS(file = file.path(peru.province.inla.data.out.dir, 
                                           paste0("zi_pois_season_sq_rsi_ns_mod_historical_tmin_roll_2_prec_roll_2_spi_icen_historical_dir.pred", i, ".RDS")))
  historical_all_dir.pred <- rbind(historical_all_dir.pred, tmp_dir.pred)
  
  historical_ovr_province_rt_forecast_preds_dt <- rbind(historical_ovr_province_rt_forecast_preds_dt, historical_province_rt_forecast_preds_dt)
}

saveRDS(historical_ovr_province_rt_forecast_preds_dt,file = file.path(peru.province.inla.data.out.dir, 
        paste0("historical_ovr_province_rt_forecast_preds_dt.RDS")))
saveRDS(historical_all_dir.pred, file = file.path(peru.province.inla.data.out.dir, 
                                                                  paste0("historical_all_dir.pred.RDS")))

historical_ovr_province_rt_forecast_preds_dt <- readRDS(file = file.path(peru.province.inla.data.out.dir, 
                                                                      paste0("historical_ovr_province_rt_forecast_preds_dt.RDS")))
historical_all_dir.pred <- readRDS(file = file.path(peru.province.inla.data.out.dir, 
                                                  paste0("historical_all_dir.pred.RDS")))

climate_dir.pred.dt_2010_2018 <- data.table(historical_all_dir.pred)
climate_dir.pred.dt_2010_2018[, PROVINCE:= ptl_province_inla_df[which(TIME > 1 & TIME <= province_first_time_2018) ]$PROVINCE]
climate_dir.pred.dt_2010_2018[, TIME:= ptl_province_inla_df[which(TIME > 1 & TIME <= province_first_time_2018) ]$TIME]
climate_dir.pred.dt_2010_2018 <- melt(climate_dir.pred.dt_2010_2018, id.vars = c("PROVINCE","TIME"), 
     value.name = "prediction")
climate_dir.pred.dt_2010_2018[, variable:= NULL]
climate_dir.pred.dt_2010_2018 <- merge(climate_dir.pred.dt_2010_2018, subset(ptl_province_inla_df,
                   select = c("PROVINCE", "TIME", "MONTH", "YEAR", "DIR",
                              "LAT_PROV_IND", "LONG_PROV_IND")),
      by = c("PROVINCE","TIME"))
climate_dir.pred.dt_2010_2018[, sample:= rep(seq(1, 5000, by = 1), nrow(climate_dir.pred.dt_2010_2018)/5000)]
setnames(climate_dir.pred.dt_2010_2018, "DIR", "true_value")
climate_dir.pred.dt_2010_2018[, model:= "climate"]
saveRDS(climate_dir.pred.dt_2010_2018, file = file.path(peru.province.inla.data.out.dir, 
                                                        paste0("climate_dir.pred.dt_2010_2018.RDS")))



climate_dir_pred.dt_2010_2018 <- readRDS(file = file.path(peru.province.inla.data.out.dir, "climate_dir.pred.dt_2010_2018.RDS"))
tmp <- subset(ptl_province_inla_df, select = c("TIME", "PROVINCE", "POP_OFFSET", "LOG_CASES"))

climate_log_cases.dt_2010_2018 <- merge(climate_dir_pred.dt_2010_2018, tmp, 
                                        by = c("TIME", "PROVINCE"))
climate_log_cases.dt_2010_2018[, prediction:= log1p(prediction*POP_OFFSET)]
climate_log_cases.dt_2010_2018[, true_value:= log1p(true_value*POP_OFFSET)]
saveRDS(climate_log_cases.dt_2010_2018,
        file = file.path(peru.province.inla.data.out.dir, "climate_log_cases.dt_2010_2018.RDS"))

climate_2010_2018_log_cases_quantile_dt <- 
  sample_to_quantile(climate_log_cases.dt_2010_2018, 
                     quantiles = c(0.01, 0.025, 
                                   seq(0.05, 0.95, 0.05), 
                                   0.975, 0.99))
saveRDS(climate_2010_2018_log_cases_quantile_dt,
        file = file.path(peru.province.inla.data.out.dir, "climate_2010_2018_log_cases_quantile_dt.RDS"))
climate_2010_2018_log_cases_quantile_dt %>%
  score() %>%
  summarise_scores(by = c("model"))
climate_2010_2018_log_cases_quantile_dt

climate_2010_2018_log_cases_quantile_dt[quantile %in% seq(0.1, 0.9, 0.1), ] %>%
  pit(by = c("model")) %>%
  plot_pit() +
  facet_wrap(model ~.)
climate_2010_2018_log_cases_quantile_dt %>%
  score() %>%
  summarise_scores(by = c("model", "range")) %>%
  plot_interval_coverage()


climate_2010_2018_forecast_quantile_dt <- 
  sample_to_quantile(climate_dir.pred.dt_2010_2018, 
                     quantiles = c(0.01, 0.025, 
                                   seq(0.05, 0.95, 0.05), 
                                   0.975, 0.99))
saveRDS(climate_2010_2018_forecast_quantile_dt, 
        file = file.path(peru.province.inla.data.out.dir, 
                         paste0("climate_2010_2018_forecast_quantile_dt.RDS")))

climate_2010_2018_forecast_quantile_dt[quantile %in% seq(0.1, 0.9, 0.1), ] %>%
  pit(by = c("model")) %>%
  plot_pit() +
  facet_wrap(model ~.)
climate_2010_2018_forecast_quantile_dt %>%
  score() %>%
  summarise_scores(by = c("model", "range")) %>%
  plot_interval_coverage()

climate_2010_2018_forecast_quantile_dt %>%
  score() %>%
  summarise_scores(by = c("model", "quantile")) %>%
  plot_quantile_coverage()

score(historical_all_dir.pred.dt)
head(historical_all_dir.pred.dt)
head(example_continuous)
head(ptl_province_inla_df)
historical_all_true_dirs <- ptl_province_inla_df[which(TIME > 1 & TIME <= province_first_time_2018),]$DIR
historical_ovr_province_rt_forecast_preds_dt[which(TIME == 83),]
head(historical_ovr_province_rt_forecast_preds_dt)


#Proper Scoring ----
historical_all_pit <- scoringutils::pit_sample(historical_all_true_dirs,
                                    historical_all_dir.pred)
hist(historical_all_pit)

historical_all_log_scores <- scoringutils::logs_sample(historical_all_true_dirs,
                                            historical_all_dir.pred)
hist(historical_all_log_scores)

#CRPS
historical_all_crps <- crps_sample(historical_all_true_dirs, historical_all_dir.pred)
summary(historical_all_crps)
saveRDS(historical_all_crps, file = file.path(peru.province.inla.data.out.dir, 
                                                  paste0("historical_all_crps.RDS")))

province_historical_all_log_scores_dt <- data.table(LOG_SCORE = historical_all_log_scores,
                                                    PIT_VAL = historical_all_pit,
                                         PROVINCE = ptl_province_inla_df[which(TIME > 1 & TIME <= province_first_time_2018),]$PROVINCE,
                                         TIME = ptl_province_inla_df[which(TIME > 1 & TIME <= province_first_time_2018),]$TIME,
                                         DIR = ptl_province_inla_df[which(TIME > 1 & TIME <= province_first_time_2018),]$DIR,
                                         MONTH = ptl_province_inla_df[which(TIME > 1 & TIME <= province_first_time_2018),]$MONTH)
ggplot(province_historical_all_log_scores_dt) + geom_histogram(aes(x = PIT_VAL))+
  facet_wrap(PROVINCE ~., scales = "free_y") + theme_bw()

ggplot(province_historical_all_log_scores_dt) + geom_histogram(aes(x = LOG_SCORE))+
  facet_wrap(PROVINCE ~., scales = "free_y") + theme_bw()

summary(province_historical_all_log_scores_dt[which(!is.infinite(LOG_SCORE)), cor(LOG_SCORE, DIR), by = "PROVINCE"]$V1)
#High correlation between DIR and Log Score (NOW NO LONGER!)
# i.e. Higher DIR, less reliable forecast
median_province_log_score_climate_forecasting <- 
  province_historical_all_log_scores_dt[, list(MEDIAN_LOG_SCORE = median(LOG_SCORE, na.rm = TRUE),
                                    MEDIAN_DIR = median(DIR, na.rm = TRUE)), by = "PROVINCE"]
 #But on average, yes
median_province_log_score_climate_forecasting[, cor(MEDIAN_LOG_SCORE, MEDIAN_DIR)]

median_time_log_score_climate_forecasting <- 
  province_historical_all_log_scores_dt[, list(MEDIAN_LOG_SCORE = median(LOG_SCORE, na.rm = TRUE),
                                    MEDIAN_DIR = median(DIR, na.rm = TRUE)), by = "TIME"]

median_month_log_score_climate_forecasting <- 
  province_historical_all_log_scores_dt[, list(MEDIAN_LOG_SCORE = median(LOG_SCORE, na.rm = TRUE),
                                    MEDIAN_DIR = median(DIR, na.rm = TRUE)), by = "MONTH"]
median_month_log_score_climate_forecasting[, cor(MEDIAN_LOG_SCORE, MEDIAN_DIR)]

median_time_log_score_climate_forecasting[which(!is.infinite(MEDIAN_LOG_SCORE)), cor(MEDIAN_LOG_SCORE, MEDIAN_DIR)]

dim(historical_all_dir.pred)
head(historical_all_dir.pred[1:5, 1:5])



#Posterior Predictions
historical_dir_rt_forecast_ptl_pred_dt <- plot_dir_cv_posterior_preds_province(historical_ovr_province_rt_forecast_preds_dt, ptl_province_inla_df[which(TIME > 1 & TIME <= province_first_time_2018)])[[1]]
historical_dir_rt_forecast_ptl_pred_dt[, length(which((DIR) >= q2.5 & (DIR) <= q97.5))/length(DIR)]
historical_dir_rt_forecast_ptl_pred_dt[which(TIME == 84),]
historical_dir_rt_forecast_ptl_pred_dt[, length(which((DIR) > q97.5))/length(DIR)]
historical_dir_rt_forecast_ptl_pred_dt[, length(which((DIR) >= MEDIAN))/length(DIR)]
historical_dir_rt_forecast_ptl_pred_dt[, list(ABS_ERROR_MEDIAN = median(ABS_ERROR_MEDIAN),
                                   DIR = median(DIR)), by = "PROVINCE"]
historical_dir_rt_forecast_ptl_pred_dt[, list(ABS_ERROR_MEDIAN = median(ABS_ERROR_MEDIAN),
                                              DIR = median(DIR))]

head(historical_dir_rt_forecast_ptl_pred_dt[order(DIR, decreasing = TRUE)], 10)
cor(historical_dir_rt_forecast_ptl_pred_dt$MEDIAN,
    historical_dir_rt_forecast_ptl_pred_dt$DIR)
caret::R2(historical_dir_rt_forecast_ptl_pred_dt$MEDIAN,
          historical_dir_rt_forecast_ptl_pred_dt$DIR)
caret::MAE(historical_dir_rt_forecast_ptl_pred_dt$MEDIAN,
          historical_dir_rt_forecast_ptl_pred_dt$DIR)

historical_dir_rt_forecast_ptl_pred_dt[, cor(MEDIAN,DIR), by = "PROVINCE"]

with(historical_dir_rt_forecast_ptl_pred_dt, median(abs(DIR - MEDIAN)))


saveRDS(historical_dir_rt_forecast_ptl_pred_dt,
        file = file.path(peru.province.inla.data.out.dir,"historical_dir_rt_forecast_ptl_pred_dt.RDS"))
#Merge with SARIMA

historical_climate_2010_2021_dt <- copy(historical_dir_rt_forecast_ptl_pred_dt)
setnames(historical_climate_2010_2021_dt, 
         c("MEDIAN", "q97.5", "q2.5",
           "ABS_ERROR_MEDIAN", "ABS_ERROR_q97.5", "ABS_ERROR_q2.5",
           "OUTBREAK_PROB_50", "OUTBREAK_PROB", "OUTBREAK_PROB_150"),
         c("CLIMATE_DIR_MEDIAN", "CLIMATE_DIR_CI_U", "CLIMATE_DIR_CI_L",
           "CLIMATE_ABS_ERROR_MEDIAN", "CLIMATE_ABS_ERROR_CI_U", "CLIMATE_ABS_ERROR_CI_L",
           "CLIMATE_OUTBREAK_50", "CLIMATE_OUTBREAK_100", "CLIMATE_OUTBREAK_150"))

historical_climate_2010_2021_dt


tmp <- copy(historical_sarima_forecast_dt)
setnames(tmp, 
         c("UNDIFF_DIR_MEDIAN", "UNDIFF_DIR_CI_U", "UNDIFF_DIR_CI_L",
           "UNDIFF_ABS_ERROR_MEDIAN", "UNDIFF_ABS_ERROR_CI_U", "UNDIFF_ABS_ERROR_CI_L",
           "UNDIFF_OUTBREAK_50", "UNDIFF_OUTBREAK_100", "UNDIFF_OUTBREAK_150"),
         c("SARIMA_DIR_MEDIAN", "SARIMA_DIR_CI_U", "SARIMA_DIR_CI_L",
           "SARIMA_ABS_ERROR_MEDIAN", "SARIMA_ABS_ERROR_CI_U", "SARIMA_ABS_ERROR_CI_L",
           "SARIMA_OUTBREAK_50", "SARIMA_OUTBREAK_100", "SARIMA_OUTBREAK_150"))

historical_climate_sarima_2010_2021_dt <- merge(historical_climate_2010_2021_dt,
      subset(tmp, 
             select = c("SARIMA_DIR_MEDIAN", "SARIMA_DIR_CI_U", "SARIMA_DIR_CI_L",
                             "SARIMA_ABS_ERROR_MEDIAN", "SARIMA_ABS_ERROR_CI_U", 
                        "SARIMA_ABS_ERROR_CI_L","SARIMA_OUTBREAK_50", 
                        "SARIMA_OUTBREAK_100", "SARIMA_OUTBREAK_150",
                             "PROVINCE", "TIME")),
      by = c("PROVINCE", "TIME"))




historical_climate_sarima_2010_2021_dt


setkeyv(historical_climate_sarima_2010_2021_dt, c("TIME", "PROVINCE"))

ggplot(historical_climate_sarima_2010_2021_dt) +
  geom_line(aes(x = TIME, y = CLIMATE_DIR_MEDIAN, color = "CLIMATE"))+
  geom_point(aes(x = TIME, y= DIR, color = "DIR"))+
  geom_line(aes(x = TIME, y = SARIMA_DIR_MEDIAN, color = "SARIMA"))+
  # geom_ribbon(aes(x = TIME, ymin = UNDIFF_DIR_CI_L, ymax = UNDIFF_DIR_CI_U),
  #             fill = "light blue", alpha = 0.4)+
  facet_wrap(PROVINCE ~., scales = "free_y")+
  theme_bw() + theme(legend.position = "bottom")

median(historical_climate_sarima_2010_2021_dt[, cor(CLIMATE_DIR_MEDIAN, SARIMA_DIR_MEDIAN), by = "PROVINCE"]$V1)

historical_climate_sarima_2010_2021_dt[, cor(CLIMATE_DIR_MEDIAN, SARIMA_DIR_MEDIAN)]  

historical_climate_sarima_2010_2021_dt[, MAE(CLIMATE_DIR_MEDIAN, DIR)]  
historical_climate_sarima_2010_2021_dt[, MAE(SARIMA_DIR_MEDIAN, DIR)]  
historical_climate_sarima_2010_2021_dt[, EQUAL_WEIGHT_CLIMATE_SARIMA_DIR_MEDIAN:=
                                         (CLIMATE_DIR_MEDIAN + SARIMA_DIR_MEDIAN)/2]
historical_climate_sarima_2010_2021_dt[, caret::R2(EQUAL_WEIGHT_CLIMATE_SARIMA_DIR_MEDIAN, DIR)]  

historical_climate_sarima_2010_2021_dt[, RMSE(CLIMATE_DIR_MEDIAN, DIR)]  
historical_climate_sarima_2010_2021_dt[, RMSE(SARIMA_DIR_MEDIAN, DIR)]  

historical_climate_sarima_2010_2021_dt[which(TIME >= 80 & TIME <= 86 & PROVINCE == "Piura"), ] 
ggplot(historical_climate_sarima_2010_2021_dt, aes(x = DIR, y = CLIMATE_DIR_MEDIAN))+
  geom_point() + theme_bw()



dim(historical_all_dir.pred)

id_2018_start

#Log Score Processing ----
historical_ptl_province_inla_df <- ptl_province_inla_df[which(YEAR < 2018 & TIME > 1), ]
historical_ptl_province_inla_df
historical_climate_dir_pred_2010_2018 <- historical_all_dir.pred[1:(id_2018_start-1), ]
historical_true_dirs <- ptl_province_inla_df[which(YEAR < 2018 & TIME > 1), ]$DIR
historical_log_scores <- scoringutils::logs_sample(historical_true_dirs,
                                                   historical_climate_dir_pred_2010_2018)
province_historical_log_scores_dt <- data.table(LOG_SCORE = historical_log_scores,
                                         PROVINCE =historical_ptl_province_inla_df$PROVINCE,
                                         TIME = historical_ptl_province_inla_df$TIME,
                                         DIR = historical_ptl_province_inla_df$DIR,
                                         MONTH = historical_ptl_province_inla_df$MONTH)
median(province_historical_log_scores_dt[which(!is.infinite(LOG_SCORE)), 
                                  cor(LOG_SCORE, DIR), by = "PROVINCE"]$V1)
summary_province_historical_log_scores_dt_by_province_and_month <- 
  province_historical_log_scores_dt[, list(MIN = quantile(LOG_SCORE, probs = 0.25),
                                    MAX = quantile(LOG_SCORE, probs = 0.75),
                                    MEDIAN = median(LOG_SCORE, na.rm = TRUE),
                                    MIN_DIR = quantile(DIR, probs = 0.25),
                                    MAX_DIR = quantile(DIR, probs = 0.75),
                                    MEDIAN_DIR = median(DIR, na.rm = TRUE)),
                             by = c("PROVINCE", "MONTH")]
summary_province_historical_log_scores_dt_by_province_and_month
summary_province_historical_log_scores_dt_by_province_and_month[which(!is.infinite(MEDIAN)), 
                                                                cor(MEDIAN, MEDIAN_DIR)]

summary_province_historical_log_scores_dt_by_province_and_month[which(!is.infinite(MEDIAN)), 
                                                                cor(MEDIAN, MONTH)]

historical_log_scores_by_province_and_month_plot <- ggplot(summary_province_historical_log_scores_dt_by_province_and_month)+
  geom_line(aes(x = MONTH, y = MEDIAN))+
  # geom_ribbon(aes(x = MONTH, ymin = MIN, ymax = MAX),
  #             fill = "pink", alpha = 0.2)+
  facet_wrap(PROVINCE ~., scales = "free_y")+
  scale_x_discrete(limit = month.abb)+
  theme_bw()
historical_log_scores_by_province_and_month_plot


historical_dirs_by_province_and_month_plot <- ggplot(summary_province_historical_log_scores_dt_by_province_and_month)+
  geom_line(aes(x = MONTH, y = MEDIAN_DIR))+
  # geom_ribbon(aes(x = MONTH, ymin = MIN_DIR, ymax = MAX_DIR),
  #             fill = "pink", alpha = 0.2)+
  facet_wrap(PROVINCE ~., scales = "free_y")+
  scale_x_discrete(limit = month.abb)+
  theme_bw()
historical_dirs_by_province_and_month_plot
grid.arrange(historical_dirs_by_province_and_month_plot,
             historical_log_scores_by_province_and_month_plot,
             nrow = 2)

summary_province_historical_all_log_scores_dt_by_month <- 
  province_historical_log_scores_dt[, list(MIN = quantile(LOG_SCORE, probs = 0.25),
                                    MAX = quantile(LOG_SCORE, probs = 0.75),
                                    MEDIAN = median(LOG_SCORE, na.rm = TRUE),
                                    MIN_DIR = quantile(DIR, probs = 0.25),
                                    MAX_DIR = quantile(DIR, probs = 0.75),
                                    MEDIAN_DIR = median(DIR, na.rm = TRUE)),
                             by = c("MONTH")]
summary_province_historical_log_scores_dt_by_province_and_month[which(!is.infinite(MEDIAN)), 
                                                                cor(MEDIAN, MONTH)]

summary_province_historical_all_log_scores_dt_by_month[which(!is.infinite(MEDIAN)), 
                                                                cor(MEDIAN, MONTH)]
#Later month, greater skill?
historical_log_scores_by_month_plot <- ggplot(summary_province_historical_all_log_scores_dt_by_month)+
  geom_line(aes(x = MONTH, y = MEDIAN))+
  geom_ribbon(aes(x = MONTH, ymin = MIN, ymax = MAX),
              fill = "pink", alpha = 0.2)+
  scale_x_discrete(limit = month.abb)+
  theme_bw()
historical_log_scores_by_month_plot


historical_dirs_by_month_plot <- ggplot(summary_province_historical_all_log_scores_dt_by_month)+
  geom_line(aes(x = MONTH, y = MEDIAN_DIR))+
  geom_ribbon(aes(x = MONTH, ymin = MIN_DIR, ymax = MAX_DIR),
              fill = "pink", alpha = 0.2)+
  scale_x_discrete(limit = month.abb)+
  theme_bw()
grid.arrange(historical_dirs_by_month_plot,
             historical_log_scores_by_month_plot,
             nrow = 2)
summary_province_historical_all_log_scores_dt_by_month[, cor(MEDIAN_DIR, MEDIAN)]
#Extremely strong correlation between median DIR and median log score by month

province_historical_log_scores_dt



#Log Score Peak Month
province_historical_log_scores_dt <- merge(province_historical_log_scores_dt,
      subset(historical_ptl_province_inla_df,
             select = c("MONTH", "TIME", "YEAR", "PROVINCE",
                        "REGION", "LAT_PROV_IND", "LONG_PROV_IND")),
      by = c("MONTH", "TIME", "PROVINCE"))
mean_historical_dir_dt
  province_historical_log_scores_dt[, list(MEAN_DIR = mean(DIR)), 
                                    by = c("YEAR", "MONTH")]
peak_month_historical_dir_dt <- 
  mean_historical_dir_dt[, list(VAL = max(MEAN_DIR)), by = "YEAR"]

trough_month_historical_dir_dt <- 
  mean_historical_dir_dt[, list(VAL = min(MEAN_DIR)), by = "YEAR"]
trough_month_historical_dir_dt

peak_month_historical_dt <- 
  subset(mean_historical_dir_dt, MEAN_DIR %in% peak_month_historical_dir_dt$VAL)
peak_month_historical_dt[, PEAK_MONTH:= MONTH]

trough_month_historical_dt <- 
  subset(mean_historical_dir_dt, MEAN_DIR %in% trough_month_historical_dir_dt$VAL)
trough_month_historical_dt[, TROUGH_MONTH:= MONTH]
trough_month_historical_dt
province_historical_log_scores_dt <- merge(province_historical_log_scores_dt,
      subset(peak_month_historical_dt,
             select = c("YEAR", "PEAK_MONTH")),
      by = "YEAR")
summary_province_historical_all_log_scores_dt_by_year <- 
  province_historical_log_scores_dt[, list(MIN = quantile(LOG_SCORE, probs = 0.25),
                                           MAX = quantile(LOG_SCORE, probs = 0.75),
                                           MEDIAN = median(LOG_SCORE, na.rm = TRUE),
                                           MIN_DIR = quantile(DIR, probs = 0.25),
                                           MAX_DIR = quantile(DIR, probs = 0.75),
                                           MEDIAN_DIR = median(DIR, na.rm = TRUE),
                                           PEAK_MONTH = PEAK_MONTH),
                                    by = c("YEAR")]
historical_log_scores_by_year_plot <- 
  ggplot(summary_province_historical_all_log_scores_dt_by_year)+
  geom_line(aes(x = YEAR, y = MEDIAN))+
  geom_ribbon(aes(x = YEAR, ymin = MIN, ymax = MAX),
              fill = "pink", alpha = 0.2)+
  theme_bw()

historical_peak_month_by_year_plot <- 
  ggplot(summary_province_historical_all_log_scores_dt_by_year)+
  geom_line(aes(x = YEAR, y = PEAK_MONTH))+
  theme_bw()
gc()
grid.arrange(historical_log_scores_by_year_plot,
             historical_peak_month_by_year_plot,
             nrow = 2)
summary_province_historical_all_log_scores_dt_by_year[, cor(PEAK_MONTH, MEDIAN)]
#Later peak month, greater median log score?


#Merge posterior prediction summary dt with log score dt ----
historical_dir_rt_forecast_ptl_pred_dt




#EQUAL-WEIGHTED POSTERIOR SAMPLES -----
historical_sarima_dir_pred_2010_2018 <- t(historical_sarima_dir_pred[, 1:(id_2018_start-1)])
historical_climate_dir_pred_2010_2018 <- historical_all_dir.pred[1:(id_2018_start-1), ]
# dim(historical_sarima_dir_pred_2010_2018)
# dim(historical_climate_dir_pred_2010_2018)


historical_climate_sarima_dir_pred_2010_2018 <- cbind(historical_climate_dir_pred_2010_2018,
                                                      historical_sarima_dir_pred_2010_2018)

# dim(historical_climate_sarima_dir_pred_2010_2018)

historical_climate_sarima_dir_pred_2010_2018_dt <- data.table(historical_climate_sarima_dir_pred_2010_2018)
historical_forecast_dt <- copy(ptl_province_inla_df[which(TIME > 1 & TIME <= province_first_time_2018),])

setkeyv(historical_forecast_dt, c("TIME", "PROVINCE"))
historical_climate_sarima_dir_pred_2010_2018_dt[, TIME:= historical_forecast_dt$TIME]
historical_climate_sarima_dir_pred_2010_2018_dt[, PROVINCE:= historical_forecast_dt$PROVINCE]
historical_climate_sarima_dir_pred_2010_2018_dt <- data.table(melt(historical_climate_sarima_dir_pred_2010_2018_dt, id.vars = c("TIME", "PROVINCE")))
setnames(historical_climate_sarima_dir_pred_2010_2018_dt, c("variable", "value"), c("ITER", "PRED"))
setkeyv(historical_climate_sarima_dir_pred_2010_2018_dt, c("TIME", "ITER"))
historical_climate_sarima_dir_pred_2010_2018_dt[, ITER:= rep(rep(seq(1, 10000, by = 1), each = length(unique(PROVINCE))),
                                                  length(unique(TIME)))]

historical_climate_sarima_dir_pred_2010_2018_dt <- merge(historical_climate_sarima_dir_pred_2010_2018_dt,
      subset(ptl_province_inla_df, select = c("PROVINCE", "TIME", "DIR", "REGION")),
      by = c("PROVINCE", "TIME"))
summary_historical_climate_sarima_dir_pred_2010_2018_dt <- 
  historical_climate_sarima_dir_pred_2010_2018_dt[, list(MEDIAN = median(PRED),
                                              CI_L = quantile(PRED, probs = 0.025),
                                              CI_U = quantile(PRED, probs = 0.975),
                                              OUTBREAK_PROB_50 = length(which(PRED >= 50))/length(PRED),
                                              OUTBREAK_PROB_150 = length(which(PRED >= 150))/length(PRED)),
                                       by = c("PROVINCE", "TIME")]
ptl_province_inla_df[, OUTBREAK_50_TRUE:= ifelse(DIR >= 50, 1, 0)]
ptl_province_inla_df[, OUTBREAK_150_TRUE:= ifelse(DIR >= 150, 1, 0)]

summary_historical_climate_sarima_dir_pred_2010_2018_dt <- merge(summary_historical_climate_sarima_dir_pred_2010_2018_dt,
      subset(ptl_province_inla_df, select = c("PROVINCE", "TIME", "DIR", "REGION")),
      by = c("PROVINCE", "TIME"))
summary_historical_climate_sarima_dir_pred_2010_2018_dt[,
                                                        length(which(DIR >= CI_L 
                                                                     & DIR <= CI_U))
                                                        /nrow(summary_historical_climate_sarima_dir_pred_2010_2018_dt)]
#98% posterior prediction coverage!
summary_historical_climate_sarima_dir_pred_2010_2018_dt[, caret::R2(DIR, MEDIAN)]

head(summary_historical_climate_sarima_dir_pred_2010_2018_dt)

binned_threshold_50_function <- function(thresholds, data) {
  results_dt <- data.table(cut_off = thresholds, 
                           true_pos = rep(0, length(thresholds)),
                           false_pos = rep(0, length(thresholds)),
                           diff_tpr_fpr = rep(0, length(thresholds)),
                           accuracy = rep(0, length(thresholds)),
                           precision = rep(0, length(thresholds)))
  for(i in 1:length(unique(thresholds))){
    threshold <- unique(thresholds)[i]
    # Calculate the combined score to be minimized/maximized
    data[, OUTBREAK_50_PRED:= ifelse(OUTBREAK_PROB_50 >= threshold, 1, 0)]
    data[, OUTBREAK_50_TRUE:= ifelse(DIR >= 50, 1, 0)]
    
    confusion_mat <- confusionMatrix(data=as.factor(data$OUTBREAK_50_PRED), 
                                     reference = as.factor(data$OUTBREAK_50_TRUE),
                                     positive = "1")
    
    true_pos_50 <- unname(confusion_mat$byClass[1]) #Sensitivity/Hit Rate
    fpr_50 <- 1-unname(confusion_mat$byClass[2]) #1-TNR = 1-Specificity
    acc <- unname(confusion_mat$overall[1])
    prec <- unname(confusion_mat$byClass[3])
    roc_50 <- roc(data$OUTBREAK_50_TRUE, 
                  data$OUTBREAK_50_PRED)
    print(ci.auc(data$OUTBREAK_50_TRUE, data$OUTBREAK_50_PRED))
    # print(roc_50)
    auc_50 <- auc(roc_50)
    
    # Define the combined score (e.g., variable1 - variable2, depending on your goals)
    combined_score <- true_pos_50 -fpr_50
    results_dt[which(cut_off == threshold), true_pos:= true_pos_50]
    results_dt[which(cut_off == threshold), false_pos:= fpr_50]
    results_dt[which(cut_off == threshold), diff_tpr_fpr:= combined_score]
    results_dt[which(cut_off == threshold), accuracy:= acc]
    results_dt[which(cut_off == threshold), precision:= prec]
    results_dt[which(cut_off == threshold), auc:= auc_50]
    # results_dt[which(cut_off == threshold), ci_auc:= auc_ci]
    
  }
  return(results_dt)
}

binned_threshold_50_function_with_ci <- function(thresholds, data) {
  results_dt <- data.table(cut_off = thresholds, 
                           true_pos = rep(0, length(thresholds)),
                           false_pos = rep(0, length(thresholds)),
                           diff_tpr_fpr = rep(0, length(thresholds)),
                           accuracy = rep(0, length(thresholds)),
                           precision = rep(0, length(thresholds)))
  for(i in 1:length(unique(thresholds))){
    threshold <- unique(thresholds)[i]
    # Calculate the combined score to be minimized/maximized
    data[, OUTBREAK_50_PRED:= ifelse(OUTBREAK_PROB_50 >= threshold, 1, 0)]
    data[, OUTBREAK_50_TRUE:= ifelse(DIR >= 50, 1, 0)]
    print(ci.auc(data$OUTBREAK_50_TRUE, data$OUTBREAK_50_PRED))
    confusion_mat <- confusionMatrix(data=as.factor(data$OUTBREAK_50_PRED), 
                                     reference = as.factor(data$OUTBREAK_50_TRUE),
                                     positive = "1")
    new_confusion_mat <- as.matrix(c(confusion_mat$table[2, 2], 
                                     confusion_mat$table[1, 2], 
                                     confusion_mat$table[2, 1], 
                                     confusion_mat$table[1, 1]), byrow = TRUE)
    print(new_confusion_mat)
    #Reverse reference level for epi.tests
    confusion_vals <- epi.tests(new_confusion_mat)
    
  }
  return(confusion_vals)
}

binned_threshold_50_function_returning_data <- function(thresholds, data) {
  results_dt <- data.table(cut_off = thresholds, 
                           true_pos = rep(0, length(thresholds)),
                           false_pos = rep(0, length(thresholds)),
                           diff_tpr_fpr = rep(0, length(thresholds)),
                           accuracy = rep(0, length(thresholds)),
                           precision = rep(0, length(thresholds)))
  for(i in 1:length(unique(thresholds))){
    threshold <- unique(thresholds)[i]
    # Calculate the combined score to be minimized/maximized
    data[, OUTBREAK_50_PRED:= ifelse(OUTBREAK_PROB_50 >= threshold, 1, 0)]
    data[, OUTBREAK_50_TRUE:= ifelse(DIR >= 50, 1, 0)]
    
    confusion_mat <- confusionMatrix(data=as.factor(data$OUTBREAK_50_PRED), 
                                     reference = as.factor(data$OUTBREAK_50_TRUE),
                                     positive = "1")
    
    true_pos_50 <- unname(confusion_mat$byClass[1]) #Sensitivity/Hit Rate
    fpr_50 <- 1-unname(confusion_mat$byClass[2]) #1-TNR = 1-Specificity
    acc <- unname(confusion_mat$overall[1])
    prec <- unname(confusion_mat$byClass[3])
    roc_50 <- roc(data$OUTBREAK_50_TRUE, 
                   data$OUTBREAK_50_PRED)
    print(ci.auc(data$OUTBREAK_50_TRUE, data$OUTBREAK_50_PRED))
    # print(roc_50)
    auc_50 <- auc(roc_50)
    
    # Define the combined score (e.g., variable1 - variable2, depending on your goals)
    combined_score <- true_pos_50 -fpr_50
    results_dt[which(cut_off == threshold), true_pos:= true_pos_50]
    results_dt[which(cut_off == threshold), false_pos:= fpr_50]
    results_dt[which(cut_off == threshold), diff_tpr_fpr:= combined_score]
    results_dt[which(cut_off == threshold), accuracy:= acc]
    results_dt[which(cut_off == threshold), precision:= prec]
    results_dt[which(cut_off == threshold), auc:= auc_50]
    # results_dt[which(cut_off == threshold), ci_auc:= auc_ci]
    
  }
  return(data)
}

head(historical_climate_sarima_2010_2018_results)




historical_climate_sarima_2010_2018_outbreak_50_results <- binned_threshold_50_function(thresholds = seq(0.05, 0.6, by = 0.005),
                                                                  summary_historical_climate_sarima_dir_pred_2010_2018_dt)
historical_climate_sarima_2010_2018_chosen_threshold_50 <- 
  head(historical_climate_sarima_2010_2018_outbreak_50_results[order(diff_tpr_fpr, decreasing = TRUE)]$cut_off, 1)
historical_climate_sarima_2010_2018_chosen_threshold_50

historical_climate_sarima_2010_2018_chosen_threshold_50 <- 
  head(historical_climate_sarima_2010_2018_outbreak_50_results[order(auc, decreasing = TRUE)]$cut_off, 1)
historical_climate_sarima_2010_2018_chosen_threshold_50

# chosen_upper_retrospective_results_thresholds_50 <- binned_threshold_50_function(thresholds = upper_bound_chosen_threshold_50,
#                                                                                  dir_cv_piura_tumbes_posterior_pred_dt)
# 
# chosen_upper_retrospective_results_thresholds_50
# 



climate_sarima_forecasting_2018_2021_outbreak_50_results <- binned_threshold_50_function(thresholds = historical_climate_sarima_2010_2018_chosen_threshold_50,
                             summary_climate_sarima_dir_pred_2018_2021_dt)
climate_sarima_forecasting_2018_2021_outbreak_50_results

climate_sarima_forecasting_2018_2021_outbreak_50_data <- binned_threshold_50_function_returning_data(thresholds = historical_climate_sarima_2010_2018_chosen_threshold_50,
                                                                                         summary_climate_sarima_dir_pred_2018_2021_dt)
climate_sarima_forecasting_2018_2021_outbreak_50_data

climate_sarima_forecasting_2018_2021_outbreak_50_results_ci <- 
  binned_threshold_50_function_with_ci(thresholds = historical_climate_sarima_2010_2018_chosen_threshold_50,
                                       summary_climate_sarima_dir_pred_2018_2021_dt)
climate_sarima_forecasting_2018_2021_outbreak_50_results_ci
paper_province_50_ensemble_equal_forecasting_dt <- copy(climate_sarima_forecasting_2018_2021_outbreak_50_results)
paper_province_50_ensemble_equal_forecasting_dt
paper_province_50_ensemble_equal_forecasting_dt <- subset(paper_province_50_ensemble_equal_forecasting_dt, 
                                                           select = c("true_pos", "false_pos", "accuracy",
                                                                      "precision", "auc"))
paper_province_50_ensemble_equal_forecasting_dt[, true_pos:= round(true_pos, 2)]
paper_province_50_ensemble_equal_forecasting_dt[, false_pos:= round(false_pos, 2)]
paper_province_50_ensemble_equal_forecasting_dt[, accuracy:= round(accuracy, 2)]
paper_province_50_ensemble_equal_forecasting_dt[, precision:= round(precision, 2)]
paper_province_50_ensemble_equal_forecasting_dt[, auc:= round(auc, 2)]
setnames(paper_province_50_ensemble_equal_forecasting_dt,
         c("True Positive", "False Positive", "Accuracy",
           "Precision", "AUC"))
paper_province_50_ensemble_equal_forecasting_dt







binned_threshold_150_function <- function(thresholds, data) {
  results_dt <- data.table(cut_off = thresholds, 
                           true_pos = rep(0, length(thresholds)),
                           false_pos = rep(0, length(thresholds)),
                           diff_tpr_fpr = rep(0, length(thresholds)),
                           accuracy = rep(0, length(thresholds)),
                           precision = rep(0, length(thresholds)))
  for(i in 1:length(unique(thresholds))){
    threshold <- unique(thresholds)[i]
    # Calculate the combined score to be minimized/maximized
    data[, OUTBREAK_150_PRED:= ifelse(OUTBREAK_PROB_150 >= threshold, 1, 0)]
    data[, OUTBREAK_150_TRUE:= ifelse(DIR >= 150, 1, 0)]
    
    confusion_mat <- confusionMatrix(data=as.factor(data$OUTBREAK_150_PRED), 
                                     reference = as.factor(data$OUTBREAK_150_TRUE),
                                     positive = "1")
    
    true_pos_150 <- unname(confusion_mat$byClass[1]) #Sensitivity/Hit Rate
    fpr_150 <- 1-unname(confusion_mat$byClass[2]) #1-TNR = 1-Specificity
    acc <- unname(confusion_mat$overall[1])
    prec <- unname(confusion_mat$byClass[3])
    roc_150 <- roc(data$OUTBREAK_150_TRUE, 
                  data$OUTBREAK_150_PRED)
    print(ci.auc(data$OUTBREAK_150_TRUE, data$OUTBREAK_150_PRED))
    # print(roc_150)
    auc_150 <- auc(roc_150)
    
    # Define the combined score (e.g., variable1 - variable2, depending on your goals)
    combined_score <- true_pos_150 -fpr_150
    results_dt[which(cut_off == threshold), true_pos:= true_pos_150]
    results_dt[which(cut_off == threshold), false_pos:= fpr_150]
    results_dt[which(cut_off == threshold), diff_tpr_fpr:= combined_score]
    results_dt[which(cut_off == threshold), accuracy:= acc]
    results_dt[which(cut_off == threshold), precision:= prec]
    results_dt[which(cut_off == threshold), auc:= auc_150]
    # results_dt[which(cut_off == threshold), ci_auc:= auc_ci]
    
  }
  return(results_dt)
}

#Return predictions
binned_threshold_150_function_returning_data <- function(thresholds, data) {
  results_dt <- data.table(cut_off = thresholds, 
                           true_pos = rep(0, length(thresholds)),
                           false_pos = rep(0, length(thresholds)),
                           diff_tpr_fpr = rep(0, length(thresholds)),
                           accuracy = rep(0, length(thresholds)),
                           precision = rep(0, length(thresholds)))
  for(i in 1:length(unique(thresholds))){
    threshold <- unique(thresholds)[i]
    # Calculate the combined score to be minimized/maximized
    data[, OUTBREAK_150_PRED:= ifelse(OUTBREAK_PROB_150 >= threshold, 1, 0)]
    data[, OUTBREAK_150_TRUE:= ifelse(DIR >= 150, 1, 0)]
    
    confusion_mat <- confusionMatrix(data=as.factor(data$OUTBREAK_150_PRED), 
                                     reference = as.factor(data$OUTBREAK_150_TRUE),
                                     positive = "1")
    
    true_pos_150 <- unname(confusion_mat$byClass[1]) #Sensitivity/Hit Rate
    fpr_150 <- 1-unname(confusion_mat$byClass[2]) #1-TNR = 1-Specificity
    acc <- unname(confusion_mat$overall[1])
    prec <- unname(confusion_mat$byClass[3])
    roc_150 <- roc(data$OUTBREAK_150_TRUE, 
                   data$OUTBREAK_150_PRED)
    print(ci.auc(data$OUTBREAK_150_TRUE, data$OUTBREAK_150_PRED))
    # print(roc_150)
    auc_150 <- auc(roc_150)
    
    # Define the combined score (e.g., variable1 - variable2, depending on your goals)
    combined_score <- true_pos_150 -fpr_150
    results_dt[which(cut_off == threshold), true_pos:= true_pos_150]
    results_dt[which(cut_off == threshold), false_pos:= fpr_150]
    results_dt[which(cut_off == threshold), diff_tpr_fpr:= combined_score]
    results_dt[which(cut_off == threshold), accuracy:= acc]
    results_dt[which(cut_off == threshold), precision:= prec]
    results_dt[which(cut_off == threshold), auc:= auc_150]
    # results_dt[which(cut_off == threshold), ci_auc:= auc_ci]
    
  }
  return(data)
}

binned_threshold_150_function_with_ci <- function(thresholds, data) {
  results_dt <- data.table(cut_off = thresholds, 
                           true_pos = rep(0, length(thresholds)),
                           false_pos = rep(0, length(thresholds)),
                           diff_tpr_fpr = rep(0, length(thresholds)),
                           accuracy = rep(0, length(thresholds)),
                           precision = rep(0, length(thresholds)))
  for(i in 1:length(unique(thresholds))){
    threshold <- unique(thresholds)[i]
    # Calculate the combined score to be minimized/maximized
    data[, OUTBREAK_150_PRED:= ifelse(OUTBREAK_PROB_150 >= threshold, 1, 0)]
    data[, OUTBREAK_150_TRUE:= ifelse(DIR >= 150, 1, 0)]
    print(ci.auc(data$OUTBREAK_150_TRUE, data$OUTBREAK_150_PRED))
    confusion_mat <- confusionMatrix(data=as.factor(data$OUTBREAK_150_PRED), 
                                     reference = as.factor(data$OUTBREAK_150_TRUE),
                                     positive = "1")
    new_confusion_mat <- as.matrix(c(confusion_mat$table[2, 2], 
                                     confusion_mat$table[1, 2], 
                                     confusion_mat$table[2, 1], 
                                     confusion_mat$table[1, 1]), byrow = TRUE)
    print(new_confusion_mat)
    #Reverse reference level for epi.tests
    confusion_vals <- epi.tests(new_confusion_mat)
    
  }
  return(confusion_vals)
}

historical_climate_sarima_2010_2018_outbreak_150_results <- binned_threshold_150_function(thresholds = seq(0.05, 0.6, by = 0.005),
                                                                                        summary_historical_climate_sarima_dir_pred_2010_2018_dt)
historical_climate_sarima_2010_2018_chosen_threshold_150 <- 
  head(historical_climate_sarima_2010_2018_outbreak_150_results[order(diff_tpr_fpr, decreasing = TRUE)]$cut_off, 1)
historical_climate_sarima_2010_2018_chosen_threshold_150

# chosen_upper_retrospective_results_thresholds_150 <- binned_threshold_150_function(thresholds = upper_bound_chosen_threshold_150,
#                                                                                  dir_cv_piura_tumbes_posterior_pred_dt)
# 
# chosen_upper_retrospective_results_thresholds_150
# 

climate_sarima_forecasting_2018_2021_outbreak_150_results <- binned_threshold_150_function(thresholds = historical_climate_sarima_2010_2018_chosen_threshold_150,
                                                                                         summary_climate_sarima_dir_pred_2018_2021_dt)
climate_sarima_forecasting_2018_2021_outbreak_150_results

climate_sarima_forecasting_2018_2021_outbreak_150_data <- binned_threshold_150_function_returning_data(thresholds = historical_climate_sarima_2010_2018_chosen_threshold_150,
                                                                                                     summary_climate_sarima_dir_pred_2018_2021_dt)
climate_sarima_forecasting_2018_2021_outbreak_150_data

climate_sarima_forecasting_2018_2021_outbreak_150_results_ci <- binned_threshold_150_function_with_ci(thresholds = historical_climate_sarima_2010_2018_chosen_threshold_150,
                                                                                           summary_climate_sarima_dir_pred_2018_2021_dt)
climate_sarima_forecasting_2018_2021_outbreak_150_results_ci


paper_province_150_ensemble_equal_forecasting_dt <- copy(climate_sarima_forecasting_2018_2021_outbreak_150_results)
paper_province_150_ensemble_equal_forecasting_dt
paper_province_150_ensemble_equal_forecasting_dt <- subset(paper_province_150_ensemble_equal_forecasting_dt, 
                                            select = c("true_pos", "false_pos", "accuracy",
                                                       "precision", "auc"))
paper_province_150_ensemble_equal_forecasting_dt[, true_pos:= round(true_pos, 2)]
paper_province_150_ensemble_equal_forecasting_dt[, false_pos:= round(false_pos, 2)]
paper_province_150_ensemble_equal_forecasting_dt[, accuracy:= round(accuracy, 2)]
paper_province_150_ensemble_equal_forecasting_dt[, precision:= round(precision, 2)]
paper_province_150_ensemble_equal_forecasting_dt[, auc:= round(auc, 2)]
setnames(paper_province_150_ensemble_equal_forecasting_dt,
         c("True Positive", "False Positive", "Accuracy",
           "Precision", "AUC"))
paper_province_150_ensemble_equal_forecasting_dt

xtable(rbind(paper_province_50_ensemble_equal_forecasting_dt, 
             paper_province_150_ensemble_equal_forecasting_dt))




#Look at first outbreaks in year ----
climate_sarima_first_outbreak_50_dt <- 
  climate_sarima_forecasting_2018_2021_outbreak_50_data[which(first_ptl_province_outbreak_50_months_dt == 1), ]

confusion_mat_climate_sarima_first_outbreak_50 <- confusionMatrix(data=as.factor(climate_sarima_first_outbreak_50_dt$OUTBREAK_50_PRED), 
                                                                  reference = as.factor(climate_sarima_first_outbreak_50_dt$OUTBREAK_50_TRUE),
                                                                  positive = "1")
confusion_mat_climate_sarima_first_outbreak_50$table
confusion_mat_climate_sarima_first_outbreak_50$byClass
accuracy_climate_sarima_first_outbreak_50 <- unname(confusion_mat_climate_sarima_first_outbreak_50$overall[1])
precision_climate_sarima_first_outbreak_50 <- unname(confusion_mat_climate_sarima_first_outbreak_50$byClass[3])

true_pos_climate_sarima_first_outbreak_50 <- unname(confusion_mat_climate_sarima_first_outbreak_50$byClass[1]) #Sensitivity/Hit Rate
fpr_climate_sarima_first_outbreak_50 <- 1-unname(confusion_mat_climate_sarima_first_outbreak_50$byClass[2]) #1-TNR = 1-Specificity
true_neg_climate_sarima_first_outbreak_50 <- unname(confusion_mat_climate_sarima_first_outbreak_50$byClass[2]) #Sensitivity/Hit Rate
fnr_climate_sarima_first_outbreak_50 <- 1 - unname(confusion_mat_climate_sarima_first_outbreak_50$byClass[1]) #Miss Rate
roc_climate_sarima_first_outbreak_50 <- roc(climate_sarima_first_outbreak_50_dt$OUTBREAK_50_TRUE, 
                                            climate_sarima_first_outbreak_50_dt$OUTBREAK_50_PRED)
auc_climate_sarima_first_outbreak_50 <- auc(roc_climate_sarima_first_outbreak_50)
auc_climate_sarima_first_outbreak_50
forecasting_sens_spec_climate_sarima_first_outbreak_50_table <-data.table("Accuracy" = accuracy_climate_sarima_first_outbreak_50,
                                                                          "Precision" = precision_climate_sarima_first_outbreak_50, 
                                                                          "True Positive" = true_pos_climate_sarima_first_outbreak_50,
                                                                          "False Positive" = fpr_climate_sarima_first_outbreak_50,
                                                                          "True Negative Rate" = true_neg_climate_sarima_first_outbreak_50,
                                                                          "False Negative Rate" = fnr_climate_sarima_first_outbreak_50,
                                                                          "AUC" = auc_climate_sarima_first_outbreak_50)
forecasting_sens_spec_climate_sarima_first_outbreak_50_table


climate_sarima_first_outbreak_150_dt <- 
  summary_climate_sarima_dir_pred_2018_2021_dt[which(FIRST_OUTBREAK_150_MONTH == 1), ]

climate_sarima_first_outbreak_150_dt <- 
  climate_sarima_forecasting_2018_2021_outbreak_150_data[which(FIRST_OUTBREAK_150_MONTH == 1), ]

confusion_mat_climate_sarima_first_outbreak_150 <- confusionMatrix(data=as.factor(climate_sarima_first_outbreak_150_dt$OUTBREAK_150_PRED), 
                                                                  reference = as.factor(climate_sarima_first_outbreak_150_dt$OUTBREAK_150_TRUE),
                                                                  positive = "1")
confusion_mat_climate_sarima_first_outbreak_150$table
confusion_mat_climate_sarima_first_outbreak_150$byClass
accuracy_climate_sarima_first_outbreak_150 <- unname(confusion_mat_climate_sarima_first_outbreak_150$overall[1])
precision_climate_sarima_first_outbreak_150 <- unname(confusion_mat_climate_sarima_first_outbreak_150$byClass[3])

true_pos_climate_sarima_first_outbreak_150 <- unname(confusion_mat_climate_sarima_first_outbreak_150$byClass[1]) #Sensitivity/Hit Rate
fpr_climate_sarima_first_outbreak_150 <- 1-unname(confusion_mat_climate_sarima_first_outbreak_150$byClass[2]) #1-TNR = 1-Specificity
true_neg_climate_sarima_first_outbreak_150 <- unname(confusion_mat_climate_sarima_first_outbreak_150$byClass[2]) #Sensitivity/Hit Rate
fnr_climate_sarima_first_outbreak_150 <- 1 - unname(confusion_mat_climate_sarima_first_outbreak_150$byClass[1]) #Miss Rate
roc_climate_sarima_first_outbreak_150 <- roc(climate_sarima_first_outbreak_150_dt$OUTBREAK_150_TRUE, 
                                            climate_sarima_first_outbreak_150_dt$OUTBREAK_150_PRED)
auc_climate_sarima_first_outbreak_150 <- auc(roc_climate_sarima_first_outbreak_150)
auc_climate_sarima_first_outbreak_150
forecasting_sens_spec_climate_sarima_first_outbreak_150_table <-data.table("Accuracy" = accuracy_climate_sarima_first_outbreak_150,
                                                                          "Precision" = precision_climate_sarima_first_outbreak_150, 
                                                                          "True Positive" = true_pos_climate_sarima_first_outbreak_150,
                                                                          "False Positive" = fpr_climate_sarima_first_outbreak_150,
                                                                          "True Negative Rate" = true_neg_climate_sarima_first_outbreak_150,
                                                                          "False Negative Rate" = fnr_climate_sarima_first_outbreak_150,
                                                                          "AUC" = auc_climate_sarima_first_outbreak_150)
forecasting_sens_spec_climate_sarima_first_outbreak_150_table






#Comparing PIT vals of ew ensemble, climate-based, and SARIMA
climate_all_pit <- scoringutils::pit_sample(all_true_dirs,
                                            all_dir.pred)
hist(climate_all_pit)
sarima_all_pit <- scoringutils::pit_sample(all_true_dirs,
                                           sarima_dir_pred_2018_2021)
hist(sarima_all_pit)
ensemble_all_pit <- scoringutils::pit_sample(all_true_dirs,
                                             climate_sarima_dir_pred_2018_2021)
plot(ecdf(ensemble_all_pit))
lines(dunif(0, 1))
data <- runif(1000, min = 0, max = 1)

# Calculate the CDF values
x <- sort(data)
cdf <- punif(x)
plot(ecdf(ensemble_all_pit))
lines(x, cdf, col = "green")
ks.test(climate_all_pit, cdf)

pit_vals_dt <- data.table(ENSEMBLE = ensemble_all_pit, 
                          SARIMA = sarima_all_pit,
                          CLIMATE = climate_all_pit)
all_true_dirs <- ptl_province_inla_df[which(TIME > province_first_time_2018),]$DIR

ggplot(pit_vals_dt) + geom_line(aes)
#Weighting the models -----
new_climate_sarima_binned_weight_50_function <- function(weights, data, thresholds) {
  results_dt <- data.table(mod_weight = rep(weights, each = length(thresholds)),
                           true_pos = rep(rep(0, each = length(weights)), length(thresholds)),
                           false_pos = rep(rep(0, each = length(weights)), length(thresholds)),
                           diff_tpr_fpr = rep(rep(0, each = length(weights)), length(thresholds)),
                           accuracy = rep(rep(0, each = length(weights)), length(thresholds)),
                           precision = rep(rep(0, each = length(weights)), length(thresholds)))
  results_dt[, cut_off:= rep(thresholds, length(weights))]
  for(i in 1:length(unique(weights))){
    weight <- unique(weights)[i]
    for(j in 1:length(unique(thresholds))){# Calculate the combined score to be minimized/maximized
      threshold <- unique(thresholds)[j]
      # for(k in 1:length(unique(thresholds))){
      # data[, OUTBREAK_50_PRED_MOD:= ifelse(OUTBREAK_PROB_50 >= mod_threshold, 1, 0)]
      data[, OUTBREAK_50_PRED_WEIGHTED:= weight * OUTBREAK_PROB_50 + (1-weight)*OUTBREAK_50_PRED_CRUDE]
      data[, OUTBREAK_50_PRED:= ifelse(OUTBREAK_50_PRED_WEIGHTED >= threshold, 1, 0)]
      
      data[, OUTBREAK_50_TRUE:= ifelse(DIR >= 50, 1, 0)]
      
      confusion_mat <- confusionMatrix(data=as.factor(data$OUTBREAK_50_PRED), 
                                       reference = as.factor(data$OUTBREAK_50_TRUE),
                                       positive = "1")
      
      true_pos_50 <- unname(confusion_mat$byClass[1]) #Sensitivity/Hit Rate
      fpr_50 <- 1-unname(confusion_mat$byClass[2]) #1-TNR = 1-Specificity
      acc <- unname(confusion_mat$overall[1])
      prec <- unname(confusion_mat$byClass[3])
      roc_50 <- roc(data$OUTBREAK_50_TRUE, 
                    data$OUTBREAK_50_PRED)
      print(ci.auc(data$OUTBREAK_50_TRUE, data$OUTBREAK_50_PRED))
      # print(roc_50)
      auc_50 <- auc(roc_50)
      
      # Define the combined score (e.g., variable1 - variable2, depending on your goals)
      combined_score <- true_pos_50 -fpr_50
      results_dt[which(cut_off == threshold & mod_weight == weight), true_pos:= true_pos_50]
      results_dt[which(cut_off == threshold & mod_weight == weight), false_pos:= fpr_50]
      results_dt[which(cut_off == threshold & mod_weight == weight), diff_tpr_fpr:= combined_score]
      results_dt[which(cut_off == threshold & mod_weight == weight), accuracy:= acc]
      results_dt[which(cut_off == threshold & mod_weight == weight), precision:= prec]
      results_dt[which(cut_off == threshold & mod_weight == weight), auc:= auc_50]
      # results_dt[which(cut_off == threshold & mod_weight == weight), ci_auc:= auc_ci]
      
    }
  }
  return(results_dt)
}

pre_2018_retrospective_model_crude_outbreak_50_results <- new_model_crude_binned_weight_50_function(weights, pre_2018_retrospective_50, thresholds)
pre_2018_retrospective_model_crude_outbreak_50_results






#Weighting the models' posterior samples -----
new_climate_sarima_binned_weight_50_function <- function(weights, data, thresholds) {
  results_dt <- data.table(mod_weight = rep(weights, each = length(thresholds)),
                           true_pos = rep(rep(0, each = length(weights)), length(thresholds)),
                           false_pos = rep(rep(0, each = length(weights)), length(thresholds)),
                           diff_tpr_fpr = rep(rep(0, each = length(weights)), length(thresholds)),
                           accuracy = rep(rep(0, each = length(weights)), length(thresholds)),
                           precision = rep(rep(0, each = length(weights)), length(thresholds)))
  results_dt[, cut_off:= rep(thresholds, length(weights))]
  for(i in 1:length(unique(weights))){
    weight <- unique(weights)[i]
    for(j in 1:length(unique(thresholds))){# Calculate the combined score to be minimized/maximized
      threshold <- unique(thresholds)[j]
      # for(k in 1:length(unique(thresholds))){
      # data[, OUTBREAK_50_PRED_MOD:= ifelse(OUTBREAK_PROB_50 >= mod_threshold, 1, 0)]
      data[, OUTBREAK_50_PRED_WEIGHTED:= weight * OUTBREAK_PROB_50 + (1-weight)*OUTBREAK_50_PRED_CRUDE]
      data[, OUTBREAK_50_PRED:= ifelse(OUTBREAK_50_PRED_WEIGHTED >= threshold, 1, 0)]
      
      data[, OUTBREAK_50_TRUE:= ifelse(DIR >= 50, 1, 0)]
      
      confusion_mat <- confusionMatrix(data=as.factor(data$OUTBREAK_50_PRED), 
                                       reference = as.factor(data$OUTBREAK_50_TRUE),
                                       positive = "1")
      
      true_pos_50 <- unname(confusion_mat$byClass[1]) #Sensitivity/Hit Rate
      fpr_50 <- 1-unname(confusion_mat$byClass[2]) #1-TNR = 1-Specificity
      acc <- unname(confusion_mat$overall[1])
      prec <- unname(confusion_mat$byClass[3])
      roc_50 <- roc(data$OUTBREAK_50_TRUE, 
                    data$OUTBREAK_50_PRED)
      print(ci.auc(data$OUTBREAK_50_TRUE, data$OUTBREAK_50_PRED))
      # print(roc_50)
      auc_50 <- auc(roc_50)
      
      # Define the combined score (e.g., variable1 - variable2, depending on your goals)
      combined_score <- true_pos_50 -fpr_50
      results_dt[which(cut_off == threshold & mod_weight == weight), true_pos:= true_pos_50]
      results_dt[which(cut_off == threshold & mod_weight == weight), false_pos:= fpr_50]
      results_dt[which(cut_off == threshold & mod_weight == weight), diff_tpr_fpr:= combined_score]
      results_dt[which(cut_off == threshold & mod_weight == weight), accuracy:= acc]
      results_dt[which(cut_off == threshold & mod_weight == weight), precision:= prec]
      results_dt[which(cut_off == threshold & mod_weight == weight), auc:= auc_50]
      # results_dt[which(cut_off == threshold & mod_weight == weight), ci_auc:= auc_ci]
      
    }
  }
  return(results_dt)
}






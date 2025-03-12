# Set up forecasting model during training window: 2010-2017 inclusive
province_first_time_2018 <- head(ptl_province_inla_df[which(YEAR == 2018), ]$TIME, 1) - 1
provinces <- unique(ptl_province_inla_df$PROVINCE)
num_provinces <- length(provinces)

run_province_model_func <- function(data, formula = default_pois_formula){
  setkeyv(data, c("TIME", "PROVINCE"))
  model <- inla(formula = formula, 
                data = data, family = "zeroinflatedpoisson0", offset = log(POP_OFFSET),
                verbose = FALSE,
                control.inla = list(strategy = 'adaptive',
                                    cmin = 0), 
                control.family = list(link = "log"),
                control.compute = list(waic = TRUE, dic = TRUE, 
                                       cpo = TRUE, config = TRUE,
                                       return.marginals = TRUE),
                control.fixed = list(correlation.matrix = TRUE, 
                                     prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE)       
  )
  model <- inla.rerun(model)
  return(model)
}
# Run model for 2010-2017 inclusive----
# Leave-future-out forecasting
for (i in 1:(province_first_time_2018 - 1)) {
  print(i)
  idx.pred <- seq(i * num_provinces + 1, i * num_provinces + num_provinces)
  s <- 5000 # Number of posterior samples
  historical_rt_forecast_dt <- data.table(ptl_province_inla_df)
  setkeyv(historical_rt_forecast_dt, c("TIME", "PROVINCE"))
  historical_rt_forecast_dt[, IND := seq(1, nrow(historical_rt_forecast_dt))]
  true_cases <- c(historical_rt_forecast_dt[which(TIME == i + 1), ]$CASES)
  true_dirs <- c(historical_rt_forecast_dt[which(TIME == i + 1), ]$DIR)
  pop_offsets <- c(historical_rt_forecast_dt[which(TIME == i + 1), ]$POP_OFFSET)


  max_ind <- max(historical_rt_forecast_dt[which(TIME == i + 1), ]$IND)
  historical_rt_forecast_dt <- historical_rt_forecast_dt[c(1:max_ind), ]
  historical_rt_forecast_dt[which(TIME == i + 1), CASES := NA]
  historical_rt_forecast_dt[which(TIME == i + 1), DIR := NA]
  setkeyv(historical_rt_forecast_dt, c("TIME", "PROVINCE"))



  # Precipitation (prec)
  setkeyv(ptl_climate_dt_province, c("YEAR", "MONTH", "PROVINCE"))

  historical_rt_forecast_lag_tmax <- tsModel::Lag(ptl_climate_dt_province$tmax,
    group = ptl_climate_dt_province$PROVINCE,
    k = 1:maximum_climate_lag
  )
  historical_rt_forecast_lag_tmax <- historical_rt_forecast_lag_tmax[57:nrow(historical_rt_forecast_lag_tmax), ]
  historical_rt_forecast_lag_tmax <- historical_rt_forecast_lag_tmax[1:nrow(historical_rt_forecast_dt), 1:3]

  # tmin
  historical_rt_forecast_lag_tmin <- tsModel::Lag(ptl_climate_dt_province$tmin,
    group = ptl_climate_dt_province$PROVINCE,
    k = 1:maximum_climate_lag
  )
  historical_rt_forecast_lag_tmin <- historical_rt_forecast_lag_tmin[57:nrow(historical_rt_forecast_lag_tmin), ]
  historical_rt_forecast_lag_tmin <- historical_rt_forecast_lag_tmin[1:nrow(historical_rt_forecast_dt), 1:3]

  # Precipitation (prec)
  historical_rt_forecast_lag_prec <- tsModel::Lag(ptl_climate_dt_province$prec,
    group = ptl_climate_dt_province$PROVINCE,
    k = 1:maximum_climate_lag
  )
  historical_rt_forecast_lag_prec <- historical_rt_forecast_lag_prec[57:nrow(historical_rt_forecast_lag_prec), ]
  historical_rt_forecast_lag_prec <- historical_rt_forecast_lag_prec[1:nrow(historical_rt_forecast_dt), 1:3]

  historical_rt_forecast_lag_tmax_roll_2 <- tsModel::Lag(ptl_climate_dt_province$tmax_roll_2,
    group = ptl_climate_dt_province$PROVINCE,
    k = 1:maximum_climate_lag
  )
  historical_rt_forecast_lag_tmax_roll_2 <- historical_rt_forecast_lag_tmax_roll_2[57:nrow(historical_rt_forecast_lag_tmax_roll_2), ]
  historical_rt_forecast_lag_tmax_roll_2 <- historical_rt_forecast_lag_tmax_roll_2[1:nrow(historical_rt_forecast_dt), 1:3]

  # tmin_roll_2
  historical_rt_forecast_lag_tmin_roll_2 <- tsModel::Lag(ptl_climate_dt_province$tmin_roll_2,
    group = ptl_climate_dt_province$PROVINCE,
    k = 1:maximum_climate_lag
  )
  historical_rt_forecast_lag_tmin_roll_2 <- historical_rt_forecast_lag_tmin_roll_2[57:nrow(historical_rt_forecast_lag_tmin_roll_2), ]
  historical_rt_forecast_lag_tmin_roll_2 <- historical_rt_forecast_lag_tmin_roll_2[1:nrow(historical_rt_forecast_dt), 1:3]

  # prec_roll_2ipitation (prec_roll_2)
  historical_rt_forecast_lag_prec_roll_2 <- tsModel::Lag(ptl_climate_dt_province$prec_roll_2,
    group = ptl_climate_dt_province$PROVINCE,
    k = 1:maximum_climate_lag
  )
  historical_rt_forecast_lag_prec_roll_2 <- historical_rt_forecast_lag_prec_roll_2[57:nrow(historical_rt_forecast_lag_prec_roll_2), ]
  historical_rt_forecast_lag_prec_roll_2 <- historical_rt_forecast_lag_prec_roll_2[1:nrow(historical_rt_forecast_dt), 1:3]
  # SPI-6
  historical_rt_forecast_lag_spi <- tsModel::Lag(ptl_climate_dt_province$SPI_6,
    group = ptl_climate_dt_province$PROVINCE,
    k = 1:maximum_climate_lag
  )
  historical_rt_forecast_lag_spi <- historical_rt_forecast_lag_spi[57:nrow(historical_rt_forecast_lag_spi), ]
  historical_rt_forecast_lag_spi <- historical_rt_forecast_lag_spi[1:nrow(historical_rt_forecast_dt), 1:2]


  # ONI
  historical_rt_forecast_lag_oni <- tsModel::Lag(ptl_climate_dt_province$ANOM,
    k = 1:maximum_climate_lag
  )
  historical_rt_forecast_lag_oni <- historical_rt_forecast_lag_oni[57:nrow(historical_rt_forecast_lag_oni), ]
  historical_rt_forecast_lag_oni <- historical_rt_forecast_lag_oni[1:nrow(historical_rt_forecast_dt), 1:4]

  # ICEN
  historical_rt_forecast_lag_icen <- tsModel::Lag(ptl_climate_dt_province$E_INDEX,
    k = 1:maximum_climate_lag
  )
  historical_rt_forecast_lag_icen <- historical_rt_forecast_lag_icen[57:nrow(historical_rt_forecast_lag_icen), ]
  historical_rt_forecast_lag_icen <- historical_rt_forecast_lag_icen[1:nrow(historical_rt_forecast_dt), 1:4]

  lagknot <- c(1, 2)
  tmax_basis <- crossbasis(historical_rt_forecast_lag_tmax,
    argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmax, 2)),
    arglag = list(fun = "bs", knots = lagknot)
  )
  tmin_basis <- crossbasis(historical_rt_forecast_lag_tmin,
    argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmin, 2)),
    arglag = list(fun = "bs", knots = lagknot)
  )

  prec_basis <- crossbasis(historical_rt_forecast_lag_prec,
    argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$prec, 2)),
    arglag = list(fun = "bs", knots = lagknot)
  )

  tmax_roll_2_basis <- crossbasis(historical_rt_forecast_lag_tmax_roll_2,
    argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmax_roll_2, 2)),
    arglag = list(fun = "bs", knots = lagknot)
  )
  tmin_roll_2_basis <- crossbasis(historical_rt_forecast_lag_tmin_roll_2,
    argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmin_roll_2, 2)),
    arglag = list(fun = "bs", knots = lagknot)
  )

  prec_roll_2_basis <- crossbasis(historical_rt_forecast_lag_prec_roll_2,
    argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$prec_roll_2, 2)),
    arglag = list(fun = "bs", knots = lagknot)
  )

  spi_basis <- crossbasis(historical_rt_forecast_lag_spi,
    argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$SPI_6, 2)),
    arglag = list(fun = "bs", knots = 1)
  )

  oni_basis <- crossbasis(historical_rt_forecast_lag_oni,
    argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$ANOM, 2)),
    arglag = list(fun = "bs", knots = 2)
  )
  icen_basis <- crossbasis(historical_rt_forecast_lag_icen,
    argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$E_INDEX, 2)),
    arglag = list(fun = "bs", knots = 2)
  )

  colnames(tmax_basis) <- paste0("tmax_basis.", colnames(tmax_basis))
  colnames(tmin_basis) <- paste0("tmin_basis.", colnames(tmin_basis))
  colnames(prec_basis) <- paste0("prec_basis.", colnames(prec_basis))
  colnames(spi_basis) <- paste0("spi_basis.", colnames(spi_basis))
  colnames(oni_basis) <- paste0("oni_basis.", colnames(oni_basis))
  colnames(icen_basis) <- paste0("icen_basis.", colnames(icen_basis))
  colnames(tmax_roll_2_basis) <- paste0("tmax_roll_2_basis.", colnames(tmax_roll_2_basis))
  colnames(tmin_roll_2_basis) <- paste0("tmin_roll_2_basis.", colnames(tmin_roll_2_basis))
  colnames(prec_roll_2_basis) <- paste0("prec_roll_2_basis.", colnames(prec_roll_2_basis))


  forecast_formula <- CASES ~ 1 + f(MONTH,
    replicate = PROV_IND, model = "rw1", cyclic = TRUE,
    constr = TRUE, scale.model = TRUE, hyper = prior.prec
  ) +
    f(YEAR, replicate = PROV_IND, model = "iid") + f(PROV_IND,
      model = "bym2", hyper = prior.prec, scale.model = TRUE, graph = file.path(
        peru.province.inla.data.in.dir,
        "nbr_piura_tumbes_lambayeque.graph"
      )
    ) +
    SQ_RSI_DIR_LAG +
    SEASON + ns(MODIFIED_DIFF_WITH_HISTORICAL_DIR_LAG, df = 4) +
    tmin_roll_2_basis + prec_roll_2_basis + icen_basis + spi_basis

  tmp_climate_cv_fit <- run_province_model_func(data = historical_rt_forecast_dt, formula = forecast_formula)

  xx <- inla.posterior.sample(s, tmp_climate_cv_fit)

  xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor[idx.pred]), xx)
  y.pred <- matrix(NA, num_provinces, s)
  dir.pred <- matrix(NA, num_provinces, s)
  for (s.idx in 1:s) {
    xx.sample <- xx.s[, s.idx]

    y.pred[, s.idx] <- rzipois(num_provinces, lambda = exp(xx.sample[-1]), pstr0 = xx.sample[1])
    dir.pred[, s.idx] <- y.pred[, s.idx] / pop_offsets
  }
  saveRDS(dir.pred, file = file.path(peru.province.inla.data.out.dir, paste0("zi_pois_season_sq_rsi_ns_mod_historical_tmin_roll_2_prec_roll_2_spi_icen_historical_dir.pred", i, ".RDS")))
}


# Save Results ----
historical_all_dir.pred <- NULL
for (i in 1:(province_first_time_2018 - 1)) {
  tmp_dir.pred <- readRDS(file = file.path(
    peru.province.inla.data.out.dir,
    paste0("zi_pois_season_sq_rsi_ns_mod_historical_tmin_roll_2_prec_roll_2_spi_icen_historical_dir.pred", i, ".RDS")
  ))
  historical_all_dir.pred <- rbind(historical_all_dir.pred, tmp_dir.pred)


}

saveRDS(historical_all_dir.pred, file = file.path(
  peru.province.inla.data.out.dir,
  paste0("historical_all_dir.pred.RDS")
))


historical_all_dir.pred <- readRDS(file = file.path(
  peru.province.inla.data.out.dir,
  paste0("historical_all_dir.pred.RDS")
))

climate_dir.pred.dt_2010_2018 <- data.table(historical_all_dir.pred)
climate_dir.pred.dt_2010_2018[, PROVINCE := ptl_province_inla_df[which(TIME > 1 & TIME <= province_first_time_2018)]$PROVINCE]
climate_dir.pred.dt_2010_2018[, TIME := ptl_province_inla_df[which(TIME > 1 & TIME <= province_first_time_2018)]$TIME]
climate_dir.pred.dt_2010_2018 <- melt(climate_dir.pred.dt_2010_2018,
  id.vars = c("PROVINCE", "TIME"),
  value.name = "prediction"
)
climate_dir.pred.dt_2010_2018[, variable := NULL]
climate_dir.pred.dt_2010_2018 <- merge(climate_dir.pred.dt_2010_2018, subset(ptl_province_inla_df,
  select = c(
    "PROVINCE", "TIME", "MONTH", "YEAR", "DIR",
    "LAT_PROV_IND", "LONG_PROV_IND"
  )
),
by = c("PROVINCE", "TIME")
)
climate_dir.pred.dt_2010_2018[, sample := rep(seq(1, 5000, by = 1), nrow(climate_dir.pred.dt_2010_2018) / 5000)]
setnames(climate_dir.pred.dt_2010_2018, "DIR", "true_value")
climate_dir.pred.dt_2010_2018[, model := "climate"]
saveRDS(climate_dir.pred.dt_2010_2018, file = file.path(
  peru.province.inla.data.out.dir,
  paste0("climate_dir.pred.dt_2010_2018.RDS")
))



climate_dir_pred.dt_2010_2018 <- readRDS(file = file.path(peru.province.inla.data.out.dir, "climate_dir.pred.dt_2010_2018.RDS"))
tmp <- subset(ptl_province_inla_df, select = c("TIME", "PROVINCE", "POP_OFFSET", "LOG_CASES"))

climate_log_cases.dt_2010_2018 <- merge(climate_dir_pred.dt_2010_2018, tmp,
  by = c("TIME", "PROVINCE")
)
climate_log_cases.dt_2010_2018[, prediction := log1p(prediction * POP_OFFSET)]
climate_log_cases.dt_2010_2018[, true_value := log1p(true_value * POP_OFFSET)]
saveRDS(climate_log_cases.dt_2010_2018,
  file = file.path(peru.province.inla.data.out.dir, "climate_log_cases.dt_2010_2018.RDS")
)

climate_2010_2018_log_cases_quantile_dt <-
  sample_to_quantile(climate_log_cases.dt_2010_2018,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
saveRDS(climate_2010_2018_log_cases_quantile_dt,
  file = file.path(peru.province.inla.data.out.dir, "climate_2010_2018_log_cases_quantile_dt.RDS")
)


climate_2010_2018_forecast_quantile_dt <-
  sample_to_quantile(climate_dir.pred.dt_2010_2018,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
saveRDS(climate_2010_2018_forecast_quantile_dt,
  file = file.path(
    peru.province.inla.data.out.dir,
    paste0("climate_2010_2018_forecast_quantile_dt.RDS")
  )
)



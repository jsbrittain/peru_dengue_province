library(dlnm)
library(INLA)
library(VGAM)
library(logger)
library(argparse)
library(data.table)

parser <- ArgumentParser()
parser$add_argument("--workspace", "-w", help = "Workspace file")
parser$add_argument("--index", "-i", help = "Index")
parser$add_argument("--dry-run", "-n", help = "Dry run", action = "store_true")
xargs <- parser$parse_args()

peru.province.base.dir <- file.path(getwd(), "data")
peru.province.inla.data.in.dir <- file.path(peru.province.base.dir, "INLA/Input")
peru.province.inla.data.out.dir <- file.path(peru.province.base.dir, "INLA/Output")

# Load workspace file
load(file = xargs$workspace)

# Set up forecasting model during training window: 2010-2017 inclusive
log_info("Setting up forecasting model during training window: 2010-2017 inclusive")
province_first_time_2018 <- head(ptl_province_inla_df[which(ptl_province_inla_df$YEAR == 2018), ]$TIME, 1) - 1
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
log_info("Running model for 2010-2017 inclusive (n=", province_first_time_2018 - 1, ")")
i <- as.numeric(xargs$index)

filename_i <- file.path(peru.province.inla.data.out.dir, paste0("zi_pois_season_sq_rsi_ns_mod_historical_tmin_roll_2_prec_roll_2_spi_icen_historical_dir.pred", i, ".RDS"))
if (file.exists(filename_i)) {
  log_info("File: ", filename_i, ") already exists. Skipping...")
} else {
  
  log_info("Running model for province: ", i)

  if (xargs$dry_run) {
    log_info("Dry run (no data will be read or written)")
    stop()
  }

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
  log_info("Saving file: ", filename_i)
  saveRDS(dir.pred, file = filename_i)
}


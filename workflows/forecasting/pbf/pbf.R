
# REAL-TIME FORECASTING ----
log_info("Running province_bayesian_forecasting.R")

# Packages -----
require(xtable)
require(epiR)
require(pROC)
require(SpiecEasi)
require(spdep)
require(data.table)
require(scoringRules)
require(scoringutils)
require(INLA)

library(argparse)
library(logger)
library(VGAM)

conflicts_prefer(VGAM::rzipois)

parser <- ArgumentParser()
parser$add_argument("--workspace", "-w", help = "Workspace file")
parser$add_argument("--index", "-i", help = "Index")
xargs <- parser$parse_args()

# Load workspace file
load(file = xargs$workspace)

run_province_model_func <- function(data, formula = default_pois_formula) {
  setkeyv(data, c("TIME", "PROVINCE"))
  model <- inla(
    formula = formula,
    data = data, family = "zeroinflatedpoisson0", offset = log(POP_OFFSET),
    verbose = FALSE,
    control.inla = list(
      strategy = "adaptive",
      cmin = 0
    ),
    control.family = list(link = "log"),
    control.compute = list(
      waic = TRUE, dic = TRUE,
      cpo = TRUE, config = TRUE,
      return.marginals = TRUE
    ),
    control.fixed = list(
      correlation.matrix = TRUE,
      prec.intercept = 1, prec = 1
    ),
    control.predictor = list(link = 1, compute = TRUE)
  )
  model <- inla.rerun(model)
  return(model)
}

quantiles <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
setkeyv(ptl_province_inla_df, c("TIME", "PROVINCE"))
province_first_time_2018 <- head(ptl_province_inla_df[which(YEAR == 2018), ]$TIME, 1) - 1
provinces <- unique(ptl_province_inla_df$PROVINCE)
num_provinces <- length(provinces)
maximum_climate_lag <- 4 # Upper bound on lags used

i <- xargs$index
filename_i <- file.path(peru.province.inla.data.out.dir, paste0("zi_pois_season_sq_rsi_dir_lag_tmin_roll_2_prec_roll_2_spi_icen_2018_2021_rt_forecast_dir.pred", i, ".RDS"))

log_info("Processing file: ", filename_i)
idx.pred <- seq(i * num_provinces + 1, i * num_provinces + num_provinces) # This is the key to setting which month we are forecasting
s <- 5000
rt_forecast_dt <- data.table(ptl_province_inla_df)
setkeyv(rt_forecast_dt, c("TIME", "PROVINCE"))
rt_forecast_dt[, IND := seq(1, nrow(rt_forecast_dt))]
true_cases <- c(rt_forecast_dt[which(TIME == i + 1), ]$CASES)
true_dirs <- c(rt_forecast_dt[which(TIME == i + 1), ]$DIR)
pop_offsets <- c(rt_forecast_dt[which(TIME == i + 1), ]$POP_OFFSET)


max_ind <- max(rt_forecast_dt[which(TIME == i + 1), ]$IND)
rt_forecast_dt <- rt_forecast_dt[c(1:max_ind), ]
rt_forecast_dt[which(TIME == i + 1), CASES := NA]
rt_forecast_dt[which(TIME == i + 1), DIR := NA]
setkeyv(rt_forecast_dt, c("TIME", "PROVINCE"))



setkeyv(ptl_climate_dt_province, c("YEAR", "MONTH", "PROVINCE"))


# Setting up DLNMs
rt_forecast_lag_tmax <- tsModel::Lag(ptl_climate_dt_province$tmax,
  group = ptl_climate_dt_province$PROVINCE,
  k = 1:maximum_climate_lag
)
# Note we have excluded zeroth lag as we forecast with one month horizon

# We start in the 5th month of the first year
# ::: JSB ::: The indexing here can't work??? need to adjust the end index...
#rt_forecast_lag_tmax <- rt_forecast_lag_tmax[num_provinces * 4 + 1:nrow(rt_forecast_lag_tmax), ]
rt_forecast_lag_tmax <- rt_forecast_lag_tmax[(num_provinces * 4 + 1):nrow(rt_forecast_lag_tmax), ]

# Only keep first 3
rt_forecast_lag_tmax <- rt_forecast_lag_tmax[1:nrow(rt_forecast_dt), 1:3]


# Repeat for other climatic variables:
# Minimum Temperature (tmin)
rt_forecast_lag_tmin <- tsModel::Lag(ptl_climate_dt_province$tmin,
  group = ptl_climate_dt_province$PROVINCE,
  k = 1:maximum_climate_lag
)
rt_forecast_lag_tmin <- rt_forecast_lag_tmin[(num_provinces * 4 + 1):nrow(rt_forecast_lag_tmin), ]
rt_forecast_lag_tmin <- rt_forecast_lag_tmin[1:nrow(rt_forecast_dt), 1:3]

# Precipitation (prec)
rt_forecast_lag_prec <- tsModel::Lag(ptl_climate_dt_province$prec,
  group = ptl_climate_dt_province$PROVINCE,
  k = 1:maximum_climate_lag
)
rt_forecast_lag_prec <- rt_forecast_lag_prec[(num_provinces * 4 + 1):nrow(rt_forecast_lag_prec), ]
rt_forecast_lag_prec <- rt_forecast_lag_prec[1:nrow(rt_forecast_dt), 1:3]


# tmax (tmax)
rt_forecast_lag_tmax_roll_2 <- tsModel::Lag(ptl_climate_dt_province$tmax_roll_2,
  group = ptl_climate_dt_province$PROVINCE,
  k = 1:maximum_climate_lag
)
rt_forecast_lag_tmax_roll_2 <- rt_forecast_lag_tmax_roll_2[(num_provinces * 4 + 1):nrow(rt_forecast_lag_tmax_roll_2), ]
rt_forecast_lag_tmax_roll_2 <- rt_forecast_lag_tmax_roll_2[1:nrow(rt_forecast_dt), 1:3]

# tmin(tmin_roll_2)
rt_forecast_lag_tmin_roll_2 <- tsModel::Lag(ptl_climate_dt_province$tmin_roll_2,
  group = ptl_climate_dt_province$PROVINCE,
  k = 1:maximum_climate_lag
)
rt_forecast_lag_tmin_roll_2 <- rt_forecast_lag_tmin_roll_2[(num_provinces * 4 + 1):nrow(rt_forecast_lag_tmin_roll_2), ]
rt_forecast_lag_tmin_roll_2 <- rt_forecast_lag_tmin_roll_2[1:nrow(rt_forecast_dt), 1:3]

# precipitation (prec_roll_2)
rt_forecast_lag_prec_roll_2 <- tsModel::Lag(ptl_climate_dt_province$prec_roll_2,
  group = ptl_climate_dt_province$PROVINCE,
  k = 1:maximum_climate_lag
)
rt_forecast_lag_prec_roll_2 <- rt_forecast_lag_prec_roll_2[(num_provinces * 4 + 1):nrow(rt_forecast_lag_prec_roll_2), ]
rt_forecast_lag_prec_roll_2 <- rt_forecast_lag_prec_roll_2[1:nrow(rt_forecast_dt), 1:3]


# SPI-6
rt_forecast_lag_spi <- tsModel::Lag(ptl_climate_dt_province$SPI_6,
  group = ptl_climate_dt_province$PROVINCE,
  k = 1:maximum_climate_lag
)
rt_forecast_lag_spi <- rt_forecast_lag_spi[(num_provinces * 4 + 1):nrow(rt_forecast_lag_spi), ]
rt_forecast_lag_spi <- rt_forecast_lag_spi[1:nrow(rt_forecast_dt), 1:2] # Only keep up to lag 2


# ONI
rt_forecast_lag_oni <- tsModel::Lag(ptl_climate_dt_province$ANOM,
  k = 1:maximum_climate_lag
)
rt_forecast_lag_oni <- rt_forecast_lag_oni[(num_provinces * 4 + 1):nrow(rt_forecast_lag_oni), ]
rt_forecast_lag_oni <- rt_forecast_lag_oni[1:nrow(rt_forecast_dt), 1:4]

# ICEN
rt_forecast_lag_icen <- tsModel::Lag(ptl_climate_dt_province$E_INDEX,
  k = 1:maximum_climate_lag
)
rt_forecast_lag_icen <- rt_forecast_lag_icen[(num_provinces * 4 + 1):nrow(rt_forecast_lag_icen), ]
rt_forecast_lag_icen <- rt_forecast_lag_icen[1:nrow(rt_forecast_dt), 1:4]

lagknot <- c(1, 2) # 1+2 months for lag knots
tmax_basis <- crossbasis(rt_forecast_lag_tmax,
  argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmax, 2)),
  arglag = list(fun = "bs", knots = lagknot)
)
tmin_basis <- crossbasis(rt_forecast_lag_tmin,
  argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmin, 2)),
  arglag = list(fun = "bs", knots = lagknot)
)

prec_basis <- crossbasis(rt_forecast_lag_prec,
  argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$prec, 2)),
  arglag = list(fun = "bs", knots = lagknot)
)

tmax_roll_2_basis <- crossbasis(rt_forecast_lag_tmax_roll_2,
  argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmax_roll_2, 2)),
  arglag = list(fun = "bs", knots = lagknot)
)
tmin_roll_2_basis <- crossbasis(rt_forecast_lag_tmin_roll_2,
  argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$tmin_roll_2, 2)),
  arglag = list(fun = "bs", knots = lagknot)
)

prec_roll_2_basis <- crossbasis(rt_forecast_lag_prec_roll_2,
  argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$prec_roll_2, 2)),
  arglag = list(fun = "bs", knots = lagknot)
)

spi_basis <- crossbasis(rt_forecast_lag_spi,
  argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$SPI_6, 2)),
  arglag = list(fun = "bs", knots = 1)
)

oni_basis <- crossbasis(rt_forecast_lag_oni,
  argvar = list(fun = "bs", knots = equalknots(ptl_climate_dt_province$ANOM, 2)),
  arglag = list(fun = "bs", knots = 2)
)
icen_basis <- crossbasis(rt_forecast_lag_icen,
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

#
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
  SQ_RSI_DIR_LAG + SEASON +
  ns(MODIFIED_DIFF_WITH_HISTORICAL_DIR_LAG, df = 4) +
  tmin_roll_2_basis + prec_roll_2_basis + icen_basis + spi_basis

tmp_climate_cv_fit <- run_province_model_func(data = rt_forecast_dt, formula = forecast_formula)
xx <- inla.posterior.sample(s, tmp_climate_cv_fit)

xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor[idx.pred]), xx)
y.pred <- matrix(NA, num_provinces, s)
error.pred <- matrix(NA, num_provinces, s)
abs_error.pred <- matrix(NA, num_provinces, s)
dir.pred <- matrix(NA, num_provinces, s)
dir_error.pred <- matrix(NA, num_provinces, s)
dir_abs_error.pred <- matrix(NA, num_provinces, s)
for (s.idx in 1:s) { # sample ID
  xx.sample <- xx.s[, s.idx]
  y.pred[, s.idx] <- rzipois(num_provinces, lambda = exp(xx.sample[-1]), pstr0 = xx.sample[1])
  dir.pred[, s.idx] <- y.pred[, s.idx] / pop_offsets
}
if (i %% 5 == 0) {
  gc()
}
log_info("Saving file: ", filename_i)
saveRDS(dir.pred, file = filename_i)

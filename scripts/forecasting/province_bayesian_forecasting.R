
# Snakemake call ---

# Clean up old trigger files
filestem <- "zi_pois_season_sq_rsi_dir_lag_tmin_roll_2_prec_roll_2_spi_icen_2018_2021_rt_forecast_dir.pred"
file_pattern <- paste0("^", filestem, "*.trigger$")
file_names <- list.files(path = peru.province.inla.data.out.dir, pattern = file_pattern, full.names = TRUE)
file.remove(file_names)

# Create workflow trigger files
for (i in province_first_time_2018:(nrow(ptl_province_inla_df) / num_provinces - 1)) {
  filename_i <- file.path(peru.province.inla.data.out.dir, paste0(filestem, i, ".RDS"))
  filename_trigger <- paste0(filename_i, ".trigger")
  file.create(filename_trigger)
}

# Launch Snakemake
log_info("Launch Snakemake to process province_bayesian_forecasting_pre")
current_folder <- getwd()
setwd("../..")
system2("snakemake",
  args = c("--cores", "4", "--snakefile", "workflows/forecasting/pbf/pbf.smk"),
  stdout = TRUE, stderr = TRUE
)
setwd(current_folder)

# End Snakemake call ---




# Merge all posterior samples into single data.table
log_info("Merging all posterior samples into single data.table")
all_dir.pred <- NULL
ovr_province_rt_forecast_preds_dt <- NULL
for (i in (province_first_time_2018):(nrow(ptl_province_inla_df) / num_provinces - 1)) {
  tmp_dir.pred <-
    readRDS(file = file.path(
      peru.province.inla.data.out.dir,
      # JSB: Changed filenames
      # paste0("zi_pois_season_sq_rsi_ns_mod_historical_tmin_roll_2_prec_roll_2_spi_icen_2018_2021_dir.pred", i, ".RDS")
      paste0("zi_pois_season_sq_rsi_dir_lag_tmin_roll_2_prec_roll_2_spi_icen_2018_2021_rt_forecast_dir.pred", i, ".RDS")
    ))
  all_dir.pred <- rbind(all_dir.pred, tmp_dir.pred)
}

saveRDS(all_dir.pred, file = file.path(
  peru.province.inla.data.out.dir,
  paste0("all_dir.pred.RDS")
))

all_true_dirs <- ptl_province_inla_df[which(TIME > province_first_time_2018), ]$DIR

# Quantile-Based Forecasting ----
climate_dir.pred.dt_2018_2021 <- data.table(all_dir.pred)
climate_dir.pred.dt_2018_2021[, PROVINCE := ptl_province_inla_df[which(TIME > province_first_time_2018)]$PROVINCE]
climate_dir.pred.dt_2018_2021[, TIME := ptl_province_inla_df[which(TIME > province_first_time_2018)]$TIME]
climate_dir.pred.dt_2018_2021 <- melt(climate_dir.pred.dt_2018_2021,
  id.vars = c("PROVINCE", "TIME"),
  value.name = "prediction"
)
climate_dir.pred.dt_2018_2021[, variable := NULL]
climate_dir.pred.dt_2018_2021 <- merge(climate_dir.pred.dt_2018_2021, subset(ptl_province_inla_df,
  select = c(
    "PROVINCE", "TIME", "MONTH", "YEAR", "DIR",
    "LAT_PROV_IND", "LONG_PROV_IND"
  )
),
by = c("PROVINCE", "TIME")
)
climate_dir.pred.dt_2018_2021[, sample := rep(seq(1, 5000, by = 1), nrow(climate_dir.pred.dt_2018_2021) / 5000)]
climate_dir.pred.dt_2018_2021[, model := "climate"]
setnames(climate_dir.pred.dt_2018_2021, "DIR", "true_value")

saveRDS(climate_dir.pred.dt_2018_2021, file = file.path(
  peru.province.inla.data.out.dir,
  paste0("climate_dir.pred.dt_2018_2021.RDS")
))

quantiles = c(
  0.01, 0.025,
  seq(0.05, 0.95, 0.05),
  0.975, 0.99
)

climate_2018_2021_forecast_quantile_dt <-
  sample_to_quantile(climate_dir.pred.dt_2018_2021,
    quantiles = quantiles
  )
saveRDS(climate_2018_2021_forecast_quantile_dt,
  file = file.path(
    peru.province.inla.data.out.dir,
    paste0("climate_2018_2021_forecast_quantile_dt.RDS")
  )
)


# Eyeball results
climate_2018_2021_forecast_quantile_dt[quantile %in% seq(0.1, 0.9, 0.1), ] %>%
  pit(by = c("model")) %>%
  plot_pit() +
  facet_wrap(model ~ .)
climate_2018_2021_forecast_quantile_dt %>%
  score() %>%
  summarise_scores(by = c("model", "range")) %>%
  plot_interval_coverage()

climate_2018_2021_forecast_quantile_dt %>%
  score() %>%
  summarise_scores(by = c("model", "quantile")) %>%
  plot_quantile_coverage()

# LOG CASES (Primary Focus) ----
climate_dir_pred.dt_2018_2021 <- readRDS(file = file.path(peru.province.inla.data.out.dir, "climate_dir.pred.dt_2018_2021.RDS"))
tmp <- subset(ptl_province_inla_df, select = c("TIME", "PROVINCE", "POP_OFFSET", "LOG_CASES"))

climate_log_cases.dt_2018_2021 <- merge(climate_dir_pred.dt_2018_2021, tmp,
  by = c("TIME", "PROVINCE")
)
climate_log_cases.dt_2018_2021[, prediction := log1p(prediction * POP_OFFSET)]
climate_log_cases.dt_2018_2021[, true_value := log1p(true_value * POP_OFFSET)]
saveRDS(climate_log_cases.dt_2018_2021,
  file = file.path(peru.province.inla.data.out.dir, "climate_log_cases.dt_2018_2021.RDS")
)

climate_2018_2021_log_cases_quantile_dt <-
  sample_to_quantile(climate_log_cases.dt_2018_2021,
    quantiles = quantiles
  )
saveRDS(climate_2018_2021_log_cases_quantile_dt,
  file = file.path(peru.province.inla.data.out.dir, "climate_2018_2021_log_cases_quantile_dt.RDS")
)
climate_2018_2021_log_cases_quantile_dt %>%
  score() %>%
  summarise_scores(by = c("model"))
climate_2018_2021_log_cases_quantile_dt

climate_2018_2021_log_cases_quantile_dt[quantile %in% seq(0.1, 0.9, 0.1), ] %>%
  pit(by = c("model")) %>%
  plot_pit() +
  facet_wrap(model ~ .)
climate_2018_2021_log_cases_quantile_dt %>%
  score() %>%
  summarise_scores(by = c("model", "range")) %>%
  plot_interval_coverage()

climate_2018_2021_log_cases_quantile_dt %>%
  score() %>%
  summarise_scores(by = c("model", "quantile")) %>%
  plot_quantile_coverage()

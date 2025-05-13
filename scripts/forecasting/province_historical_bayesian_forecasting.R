library(logger)
library(data.table)
library(scoringutils)

peru.province.base.dir <- file.path(getwd(), "data")
peru.province.out.dir <- file.path(peru.province.base.dir, "output")
peru.province.inla.data.out.dir <- file.path(peru.province.base.dir, "INLA/Output")
peru.province.python.data.dir <- file.path(peru.province.base.dir, "python/data")

ptl_province_inla_df <- data.table(read.csv(file.path(peru.province.python.data.dir,
    "ptl_province_inla_df.csv")))

latitude_monthly_dt <- copy(ptl_province_inla_df)
setkeyv(latitude_monthly_dt, c("latitude", "longitude", "TIME"))
tmp <- unique(subset(latitude_monthly_dt, select = c("PROVINCE")))
tmp[, LAT_PROV_IND := seq(1, nrow(tmp), by = 1)]
tmp[, LONG_PROV_IND := seq(1, nrow(tmp), by = 1)]
latitude_monthly_dt <- merge(latitude_monthly_dt, tmp, by = "PROVINCE")
setkeyv(latitude_monthly_dt, c("latitude", "longitude", "TIME"))
latitude_monthly_dt[, SCALED_DIR := scale(DIR), by = "PROVINCE"]
ptl_province_inla_df[, YEAR_DECIMAL := YEAR + (MONTH - 1)/12]
ptl_province_inla_df <- merge(ptl_province_inla_df, unique(subset(latitude_monthly_dt,
    select = c("LAT_PROV_IND", "PROVINCE"))), by = c("PROVINCE"))
ptl_province_inla_df <- merge(ptl_province_inla_df, unique(subset(latitude_monthly_dt,
    select = c("LONG_PROV_IND", "PROVINCE"))), by = c("PROVINCE"))
lines_ptl_province_inla_df <- subset(ptl_province_inla_df, select = c("PROVINCE",
    "REGION", "MONTH", "DIR", "longitude", "latitude"))
ptl_province_inla_df[, SCALED_DIR := scale(DIR), by = "PROVINCE"]
ptl_province_inla_df[, LOG_DIR := log(DIR + 0.01)]






log_info("Cleaning up old workflow trigger files")
filestem <- "zi_pois_season_sq_rsi_ns_mod_historical_tmin_roll_2_prec_roll_2_spi_icen_historical_dir.pred"
file_pattern <- paste0("^", filestem, "*.trigger")
file_names <- list.files(path = peru.province.inla.data.out.dir, pattern = file_pattern,
    full.names = TRUE)
file.remove(file_names)

log_info("Touching triggers for external workflow")
province_first_time_2018 <- head(ptl_province_inla_df[which(ptl_province_inla_df$YEAR ==
    2018), ]$TIME, 1) - 1
file_names <- paste0(peru.province.inla.data.out.dir, "/", filestem, 1:(province_first_time_2018 -
    1), ".RDS.trigger")
file.create(file_names)

# Launch Snakemake to process
# province_historical_bayesian_forecasting_pre.RData
log_info("Launch Snakemake to process province_historical_bayesian_forecasting_pre")
current_folder <- getwd()
system2("snakemake", args = c("--cores", "4", "--snakefile", "workflows/forecasting/phbf/phbf.smk"),
    stdout = TRUE, stderr = TRUE)

log_info("Done with province_historical_bayesian_forecasting_pre")
phbf_filename <- file.path(peru.province.out.dir, "province_historical_bayesian_forecasting.RData")
if (FALSE) { # file.exists(phbf_filename)) {
    log_info("Loading previous workspace (", phbf_filename, ")...")
    load(file = phbf_filename)
} else {

    # Save Results ----
    historical_all_dir.pred <- NULL
    for (i in 1:(province_first_time_2018 - 1)) {

        file = file.path(peru.province.inla.data.out.dir, paste0("zi_pois_season_sq_rsi_ns_mod_historical_tmin_roll_2_prec_roll_2_spi_icen_historical_dir.pred",
            i, ".RDS"))
        log_info("Reading ", file)

        tmp_dir.pred <- readRDS(file = file.path(peru.province.inla.data.out.dir,
            paste0("zi_pois_season_sq_rsi_ns_mod_historical_tmin_roll_2_prec_roll_2_spi_icen_historical_dir.pred",
                i, ".RDS")))
        historical_all_dir.pred <- rbind(historical_all_dir.pred, tmp_dir.pred)
    }

    saveRDS(historical_all_dir.pred, file = file.path(peru.province.inla.data.out.dir,
        paste0("historical_all_dir.pred.RDS")))


    historical_all_dir.pred <- readRDS(file = file.path(peru.province.inla.data.out.dir,
        paste0("historical_all_dir.pred.RDS")))

    climate_dir.pred.dt_2010_2018 <- data.table(historical_all_dir.pred)
    climate_dir.pred.dt_2010_2018[, PROVINCE := ptl_province_inla_df[which(TIME >
        1 & TIME <= province_first_time_2018)]$PROVINCE]
    climate_dir.pred.dt_2010_2018[, TIME := ptl_province_inla_df[which(TIME >
        1 & TIME <= province_first_time_2018)]$TIME]
    climate_dir.pred.dt_2010_2018 <- melt(climate_dir.pred.dt_2010_2018, id.vars = c("PROVINCE",
        "TIME"), value.name = "prediction")
    climate_dir.pred.dt_2010_2018[, variable := NULL]
    climate_dir.pred.dt_2010_2018 <- merge(climate_dir.pred.dt_2010_2018, subset(ptl_province_inla_df,
        select = c("PROVINCE", "TIME", "MONTH", "YEAR", "DIR", "LAT_PROV_IND", "LONG_PROV_IND")),
        by = c("PROVINCE", "TIME"))
    climate_dir.pred.dt_2010_2018[, sample := rep(seq(1, 5000, by = 1), nrow(climate_dir.pred.dt_2010_2018)/5000)]
    setnames(climate_dir.pred.dt_2010_2018, "DIR", "true_value")
    climate_dir.pred.dt_2010_2018[, model := "climate"]
    saveRDS(climate_dir.pred.dt_2010_2018, file = file.path(peru.province.inla.data.out.dir,
        paste0("climate_dir.pred.dt_2010_2018.RDS")))



    climate_dir_pred.dt_2010_2018 <- readRDS(file = file.path(peru.province.inla.data.out.dir,
        "climate_dir.pred.dt_2010_2018.RDS"))
    tmp <- subset(ptl_province_inla_df, select = c("TIME", "PROVINCE", "POP_OFFSET",
        "LOG_CASES"))

    climate_log_cases.dt_2010_2018 <- merge(climate_dir_pred.dt_2010_2018, tmp, by = c("TIME",
        "PROVINCE"))
    climate_log_cases.dt_2010_2018[, prediction := log1p(prediction * POP_OFFSET)]
    climate_log_cases.dt_2010_2018[, true_value := log1p(true_value * POP_OFFSET)]
    saveRDS(climate_log_cases.dt_2010_2018, file = file.path(peru.province.inla.data.out.dir,
        "climate_log_cases.dt_2010_2018.RDS"))

    climate_2010_2018_log_cases_quantile_dt <- sample_to_quantile(climate_log_cases.dt_2010_2018,
        quantiles = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99))
    saveRDS(climate_2010_2018_log_cases_quantile_dt, file = file.path(peru.province.inla.data.out.dir,
        "climate_2010_2018_log_cases_quantile_dt.RDS"))


    climate_2010_2018_forecast_quantile_dt <- sample_to_quantile(climate_dir.pred.dt_2010_2018,
        quantiles = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99))
    saveRDS(climate_2010_2018_forecast_quantile_dt, file = file.path(peru.province.inla.data.out.dir,
        paste0("climate_2010_2018_forecast_quantile_dt.RDS")))

    # log_info("Saving current workspace...")
    # save.image(file = phbf_filename)
    # log_info("Saved current workspace to ", phbf_filename)
}

log_info("Done with province_historical_bayesian_forecasting.R")

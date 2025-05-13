library(logger)
library(data.table)
library(tsModel)
library(lubridate)

peru.province.base.dir <- file.path(getwd(), "data")
peru.province.out.dir <- file.path(peru.province.base.dir, "output")
peru.province.python.data.dir <- file.path(peru.province.base.dir, "python/data")

# Write data.table to csv for Python forecasting as follows: Set up data.table
# with log(CASES + 1) DeepTCN, TimeGPT, and SARIMA models all fit using
# log_cases to ensure non-negative predictions

# We will also analyse all predictions at the log scale as recommended See
# Bosse et al (2023): https://doi.org/10.1371/journal.pcbi.1011393 Look at CRPS
# in windows, and by geography.
log_info("Setting up data for Python forecasting")
ptl_province_inla_df <- readRDS(file = file.path(peru.province.out.dir, "ptl_province_inla_df.RDS"))
setkeyv(ptl_province_inla_df, c("PROVINCE", "TIME"))
ptl_province_inla_df[, LOG_CASES := log1p(CASES)]
ptl_province_inla_df[, LAG_1_LOG_CASES := Lag(LOG_CASES, k = 1), by = "PROVINCE"]
ptl_province_inla_df[, LAG_2_LOG_CASES := Lag(LOG_CASES, k = 2), by = "PROVINCE"]
ptl_province_inla_df[, LAG_1_tmin_roll_2 := Lag(tmin_roll_2, k = 1), by = "PROVINCE"]
ptl_province_inla_df[, LAG_1_prec_roll_2 := Lag(prec_roll_2, k = 1), by = "PROVINCE"]
ptl_province_inla_df[, end_of_month := as.Date(ceiling_date(ymd(paste(YEAR, MONTH,
    1)), "month") - days(1))]


# For timegpt, set up unique_id column - factor variable for PROVINCE
ptl_province_inla_df[, unique_id := as.factor((PROVINCE))]


# Output for python ---
log_info("Writing data for Python forecasting")
fwrite(ptl_province_inla_df, file.path(peru.province.python.data.dir, "ptl_province_inla_df.csv"))

log_info("Done setting up data for Python forecasting.")

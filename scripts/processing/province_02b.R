library(logger)

# # Add lines from province_correlations.R # All times; plot provinces by
# latitude latitude_monthly_dt <- copy(ptl_province_inla_df)
# setkeyv(latitude_monthly_dt, c('latitude', 'TIME')) latitude_monthly_dt[,
# PROV_IND:= NULL] tmp <- unique(subset(latitude_monthly_dt, select =
# c('PROVINCE'))) tmp[, LAT_PROV_IND:= seq(1, nrow(tmp), by = 1)]
# latitude_monthly_dt <- merge(latitude_monthly_dt, tmp, by = 'PROVINCE')
# setkeyv(latitude_monthly_dt, c('latitude', 'TIME')) latitude_monthly_dt[,
# SCALED_DIR:= scale(DIR), by = 'PROVINCE'] ptl_province_inla_df[,
# YEAR_DECIMAL:= YEAR + (MONTH - 1)/12] ptl_province_inla_df <-
# merge(ptl_province_inla_df, unique(subset(latitude_monthly_dt, select =
# c('LAT_PROV_IND', 'PROVINCE'))), by = c('PROVINCE'))
# lines_ptl_province_inla_df <- subset(ptl_province_inla_df, select =
# c('PROVINCE', 'REGION', 'MONTH', 'DIR', 'longitude', 'latitude'))
# ptl_province_inla_df[, SCALED_DIR:= scale(DIR), by = 'PROVINCE']
# ptl_province_inla_df[, LOG_DIR:= log(DIR + 0.01)] # ::: JSB ::: Addition
# ptl_province_inla_df[, LAT_PROV_IND := latitude_monthly_dt$LAT_PROV_IND]
# ptl_province_inla_df[, LONG_PROV_IND := latitude_monthly_dt$LAT_PROV_IND]

log_info("Running province_02b.R")
latitude_monthly_dt <- copy(ptl_province_inla_df)
setkeyv(latitude_monthly_dt, c("latitude", "longitude", "TIME"))
# latitude_monthly_dt[, LAT_PROV_IND:= NULL] latitude_monthly_dt[,
# LONG_PROV_IND:= NULL]
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

log_info("Completed province_02b.R")

library(sf)
library(dplyr)
library(logger)
library(data.table)
library(scoringutils)
library(rgeoboundaries)

# Environment variables
SNAKEMAKE_CORES <- Sys.getenv("SNAKEMAKE_CORES", unset = "1")

province.base.dir <- file.path(getwd(), "data")
province.out.dir <- file.path(province.base.dir, "output")
province.inla.data.out.dir <- file.path(province.base.dir, "INLA/Output")
province.python.data.dir <- file.path(province.base.dir, "python/data")
province.predictions.out.dir <- file.path(getwd(), "predictions")

dir.create(province.out.dir, recursive = TRUE, showWarnings = FALSE)
dir.create(province.inla.data.out.dir, recursive = TRUE, showWarnings = FALSE)
dir.create(province.predictions.out.dir, recursive = TRUE, showWarnings = FALSE)

# --- Minimal required dataset -------------------------------------------------

# Read composite dataframe
df <- data.table(
  read.csv(file.path(province.python.data.dir, "ptl_province_inla_df.csv"))
)

# --- Derive required metrics --------------------------------------------------

# Derive TIME, an index of month-year (1-140)
df <- df %>%
  mutate(date = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
date_lookup <- df %>%
  select(date) %>%
  distinct() %>%
  arrange(date) %>%
  mutate(TIME = row_number())
df <- df %>%
  left_join(date_lookup, by = "date")

# Add additional required columns
df <- df %>%
  mutate(
    POP_OFFSET = POP / 1e5,
    DIR := CASES / POP * 1e5,
    LOG_CASES = log1p(CASES)
  )

# Get ADM2 shapefile and compute centroids
peru_adm2 <- geoboundaries("Peru", "adm2", quiet = TRUE)
centroids <- suppressWarnings(st_centroid(peru_adm2))
# Extract coordinates and keep only required columns
coordinates_df <- as.data.table(
  centroids %>%
    mutate(
      X = st_coordinates(geometry)[, 1],
      Y = st_coordinates(geometry)[, 2]
    ) %>%
    st_drop_geometry() %>%
    select(shapeName, X, Y)
)
setnames(coordinates_df, "shapeName", "PROVINCE")
# Identify unmatched provinces
ptl_region_province <- unique(df[, .(PROVINCE)])
unmatched <- setdiff(ptl_region_province$PROVINCE, coordinates_df$PROVINCE)
# Manual fix for encoding issue
coordinates_df[PROVINCE == "FerreÃ±afe", PROVINCE := "Ferreñafe"]
# Filter centroids to only provinces present in df
filtered_coords <- coordinates_df[PROVINCE %in% ptl_region_province$PROVINCE]
# Merge coordinates into main dataset
df <- merge(
  df,
  filtered_coords[, .(PROVINCE, X, Y)],
  by.x = "PROVINCE",
  by.y = "PROVINCE",
  all.x = TRUE
)
setnames(df, c("X", "Y"), c("longitude", "latitude"))

# Revert to data frame and rename for processing
ptl_province_inla_df <- data.table(df)

# --- Original code ------------------------------------------------------------












latitude_monthly_dt <- copy(ptl_province_inla_df)
setkeyv(latitude_monthly_dt, c("latitude", "longitude", "TIME"))
tmp <- unique(subset(latitude_monthly_dt, select = c("PROVINCE")))
tmp[, LAT_PROV_IND := seq(1, nrow(tmp), by = 1)]
tmp[, LONG_PROV_IND := seq(1, nrow(tmp), by = 1)]
latitude_monthly_dt <- merge(latitude_monthly_dt, tmp, by = "PROVINCE")
setkeyv(latitude_monthly_dt, c("latitude", "longitude", "TIME"))
latitude_monthly_dt[, SCALED_DIR := scale(DIR), by = "PROVINCE"]
ptl_province_inla_df[, YEAR_DECIMAL := YEAR + (MONTH - 1) / 12]
ptl_province_inla_df <- merge(ptl_province_inla_df, unique(subset(latitude_monthly_dt,
  select = c("LAT_PROV_IND", "PROVINCE")
)), by = c("PROVINCE"))
ptl_province_inla_df <- merge(ptl_province_inla_df, unique(subset(latitude_monthly_dt,
  select = c("LONG_PROV_IND", "PROVINCE")
)), by = c("PROVINCE"))
lines_ptl_province_inla_df <- subset(ptl_province_inla_df, select = c(
  "PROVINCE",
  "REGION", "MONTH", "DIR", "longitude", "latitude"
))
ptl_province_inla_df[, SCALED_DIR := scale(DIR), by = "PROVINCE"]
ptl_province_inla_df[, LOG_DIR := log(DIR + 0.01)]






# Snakemake call --- Can combine with forecast_bayes.R as does the same thing
# just on another date set.

log_info("Cleaning up old workflow trigger files")
filestem <- "phbf_pred_"
file_pattern <- paste0("^", filestem, ".*\\.trigger$")
file_names <- list.files(
  path = province.inla.data.out.dir, pattern = file_pattern,
  full.names = TRUE
)
file.remove(file_names)

province_first_time_2018 <- min(
    ptl_province_inla_df[ptl_province_inla_df$YEAR == 2018, ]$TIME
  )

log_info("Touching triggers for external workflow")
# Create workflow trigger files
for (i in 2:(province_first_time_2018-1)) {
  date <- format(
    as.Date(ptl_province_inla_df[ptl_province_inla_df$TIME == i, ]$end_of_month[1]),
    "%Y-%m-%d")
  filename_trigger <- file.path(
    province.inla.data.out.dir,
    paste0(filestem, date, ".RDS.trigger"))
  file.create(filename_trigger)
}

# Launch Snakemake to process
log_info("Launch Snakemake to process province_historical_bayesian_forecasting_pre")
current_folder <- getwd()
system2("snakemake",
  args = c(
    "--cores", SNAKEMAKE_CORES,
    "--configfile", "workflows/forecasting/pbf/config/phbf.yaml",
    "--snakefile", "workflows/forecasting/pbf/workflow/Snakefile"
  ),
  stdout = "", stderr = "", wait = TRUE
)

log_info("Done with province_historical_bayesian_forecasting_pre")

# Save Results ----
historical_all_dir.pred <- NULL
province_list <- unique(ptl_province_inla_df$PROVINCE)
for (i in 2:(province_first_time_2018 - 1)) {
  date <- format(
    as.Date(ptl_province_inla_df[ptl_province_inla_df$TIME == i, ]$end_of_month[1]),
    "%Y-%m-%d")
  filename <- file.path(
    province.inla.data.out.dir,
    paste0(filestem, date, ".RDS"))
  log_info(paste0("Reading ", filename))
  tmp_dir.pred <- readRDS(file = filename)
  tmp_dir.pred <- as.data.table(tmp_dir.pred)
  tmp_dir.pred[, PROVINCE := province_list]
  tmp_dir.pred[, TIME := i]
  historical_all_dir.pred <- rbind(historical_all_dir.pred, tmp_dir.pred)
}

climate_dir.pred.dt_2010_2018 <- data.table(historical_all_dir.pred)
# climate_dir.pred.dt_2010_2018[, PROVINCE := ptl_province_inla_df[which(TIME >
#   1 & TIME <= province_first_time_2018)]$PROVINCE]
# climate_dir.pred.dt_2010_2018[, TIME := ptl_province_inla_df[which(TIME >
#   1 & TIME <= province_first_time_2018)]$TIME]
climate_dir.pred.dt_2010_2018 <- melt(climate_dir.pred.dt_2010_2018, id.vars = c(
  "PROVINCE",
  "TIME"
), value.name = "prediction")
climate_dir.pred.dt_2010_2018[, variable := NULL]
climate_dir.pred.dt_2010_2018 <- merge(climate_dir.pred.dt_2010_2018, subset(ptl_province_inla_df,
  select = c("PROVINCE", "TIME", "MONTH", "YEAR", "DIR", "LAT_PROV_IND", "LONG_PROV_IND")
),
by = c("PROVINCE", "TIME")
)
climate_dir.pred.dt_2010_2018[, sample := rep(seq(1, 5000, by = 1), nrow(climate_dir.pred.dt_2010_2018) / 5000)]
setnames(climate_dir.pred.dt_2010_2018, "DIR", "true_value")
climate_dir.pred.dt_2010_2018[, model := "climate"]

tmp <- subset(ptl_province_inla_df, select = c(
  "TIME", "PROVINCE", "POP_OFFSET",
  "LOG_CASES"
))

climate_log_cases.dt_2010_2018 <- merge(
  climate_dir.pred.dt_2010_2018, tmp,
  by = c(
    "TIME",
    "PROVINCE"
  ))
climate_log_cases.dt_2010_2018[, prediction := log1p(prediction * POP_OFFSET)]
climate_log_cases.dt_2010_2018[, true_value := log1p(true_value * POP_OFFSET)]
# was climate_log_cases.dt_2010_2018.RDS
log_info("Writing log cases samples (historical)")
write.csv(climate_log_cases.dt_2010_2018,
  paste0(province.predictions.out.dir, "/pred_log_cases_samples_historical.csv"),
  row.names = FALSE
)

climate_2010_2018_log_cases_quantile_dt <- sample_to_quantile(
  climate_log_cases.dt_2010_2018,
  quantiles = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
)
# was climate_2010_2018_log_cases_quantile_dt.RDS
log_info("Writing log cases quantiles (historical)")
write.csv(climate_2010_2018_log_cases_quantile_dt,
  paste0(province.predictions.out.dir, "/pred_log_cases_quantiles_historical.csv"),
  row.names = FALSE
)


# DIR
climate_dir_2010_2018_forecast_quantile_dt <- sample_to_quantile(climate_dir.pred.dt_2010_2018,
  quantiles = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
)

log_info("Done with province_historical_bayesian_forecasting.R")

library(sf)
library(dplyr)
library(logger)
library(data.table)
library(scoringutils)
library(rgeoboundaries)

# --- Setup --------------------------------------------------------------------

# Environment variables
SNAKEMAKE_CORES <- Sys.getenv("SNAKEMAKE_CORES", unset = "1")

province.base.dir <- file.path(getwd(), "data")
province.inla.data.out.dir <- file.path(province.base.dir, "INLA/Output")
province.python.data.dir <- file.path(province.base.dir, "python/data")
province.predictions.out.dir <- file.path(getwd(), "predictions")

# Ensure output folders exist
dir.create(province.inla.data.out.dir, recursive = TRUE, showWarnings = FALSE)
dir.create(province.predictions.out.dir, recursive = TRUE, showWarnings = FALSE)

# Analysis options
samples <- 5000
quantiles <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)

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
    select(shapeName, X, Y))
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
  all.x = TRUE)
setnames(df, c("X", "Y"), c("longitude", "latitude"))

# Revert to data frame and rename for processing
ptl_province_inla_df <- data.table(df)

# ------------------------------------------------------------------------------

latitude_monthly_dt <- copy(ptl_province_inla_df)
setkeyv(latitude_monthly_dt, c("latitude", "longitude", "TIME"))
tmp <- unique(subset(latitude_monthly_dt, select = c("PROVINCE")))
tmp[, LAT_PROV_IND := seq(1, nrow(tmp), by = 1)]
tmp[, LONG_PROV_IND := seq(1, nrow(tmp), by = 1)]
latitude_monthly_dt <- merge(latitude_monthly_dt, tmp, by = "PROVINCE")
setkeyv(latitude_monthly_dt, c("latitude", "longitude", "TIME"))
latitude_monthly_dt[, SCALED_DIR := scale(DIR), by = "PROVINCE"]
ptl_province_inla_df[, YEAR_DECIMAL := YEAR + (MONTH - 1) / 12]
ptl_province_inla_df <- merge(
  ptl_province_inla_df,
  unique(subset(latitude_monthly_dt, select = c("LAT_PROV_IND", "PROVINCE"))),
  by = c("PROVINCE"))
ptl_province_inla_df <- merge(
  ptl_province_inla_df,
  unique(subset(latitude_monthly_dt, select = c("LONG_PROV_IND", "PROVINCE"))),
  by = c("PROVINCE"))
ptl_province_inla_df[, SCALED_DIR := scale(DIR), by = "PROVINCE"]
ptl_province_inla_df[, LOG_DIR := log(DIR + 0.01)]

province_first_time_2018 <- min(
    ptl_province_inla_df[ptl_province_inla_df$YEAR == 2018, ]$TIME
  )
provinces <- unique(ptl_province_inla_df$PROVINCE)
num_provinces <- length(provinces)










# Snakemake call ---

# Clean up old trigger files
filestem <- "pbf_pred_"
file_pattern <- paste0("^", filestem, ".*\\.trigger$")
file_names <- list.files(
  path = province.inla.data.out.dir, pattern = file_pattern, full.names = TRUE)
file.remove(file_names)

# Create workflow trigger files
for (i in province_first_time_2018:max(ptl_province_inla_df$TIME)) {
  date <- format(
    as.Date(ptl_province_inla_df[ptl_province_inla_df$TIME == i, ]$end_of_month[1]),
    "%Y-%m-%d")
  filename_trigger <- file.path(
    province.inla.data.out.dir,
    paste0(filestem, date, ".RDS.trigger"))
  print(paste0("Creating trigger file: ", filename_trigger))
  file.create(filename_trigger)
}

# Launch Snakemake
log_info("Launch Snakemake to process province_bayesian_forecasting_pre")
current_folder <- getwd()
system2("snakemake",
  args = c(
    "--cores", SNAKEMAKE_CORES,
    "--configfile", "workflows/forecasting/pbf/config/pbf.yaml",
    "--snakefile", "workflows/forecasting/pbf/workflow/Snakefile"
  ),
  stdout = "", stderr = "", wait = TRUE
)

# End Snakemake call ---




# Merge all posterior samples into single data.table
log_info("Merging all posterior samples into single data.table")
all_dir.pred <- NULL
ovr_province_rt_forecast_preds_dt <- NULL
for (i in province_first_time_2018:max(ptl_province_inla_df$TIME)) {
  date <- format(
    as.Date(
      ptl_province_inla_df[ptl_province_inla_df$TIME == i, ]$end_of_month[1])
    , "%Y-%m-%d")
  filename_i <- file.path(
    province.inla.data.out.dir,
    paste0(filestem, date, ".RDS"))
  tmp_dir.pred <- readRDS(file = filename_i)
  tmp_dir.pred <- as.data.table(tmp_dir.pred)
  tmp_dir.pred[, PROVINCE := provinces]
  tmp_dir.pred[, TIME := i]
  all_dir.pred <- rbind(all_dir.pred, tmp_dir.pred)
}




# Quantile-Based Forecasting ----
climate_dir.pred.dt_2018_2021 <- data.table(all_dir.pred)
# climate_dir.pred.dt_2018_2021[,
#   PROVINCE := ptl_province_inla_df[which(TIME > province_first_time_2018)]$PROVINCE]
# climate_dir.pred.dt_2018_2021[,
#   TIME := ptl_province_inla_df[which(TIME > province_first_time_2018)]$TIME]
climate_dir.pred.dt_2018_2021 <- melt(climate_dir.pred.dt_2018_2021,
  id.vars = c("PROVINCE", "TIME"),
  value.name = "prediction"
)
climate_dir.pred.dt_2018_2021[, variable := NULL]
climate_dir.pred.dt_2018_2021 <- merge(
  climate_dir.pred.dt_2018_2021,
  subset(
    ptl_province_inla_df,
    select = c(
      "PROVINCE", "TIME", "MONTH", "YEAR", "DIR",
      "LAT_PROV_IND", "LONG_PROV_IND")),
  by = c("PROVINCE", "TIME"))
climate_dir.pred.dt_2018_2021[,
  sample := rep(seq(1, samples, by = 1),
                nrow(climate_dir.pred.dt_2018_2021) / samples)]
climate_dir.pred.dt_2018_2021[, model := "climate"]
setnames(climate_dir.pred.dt_2018_2021, "DIR", "true_value")

climate_2018_2021_forecast_quantile_dt <-
  sample_to_quantile(climate_dir.pred.dt_2018_2021,
    quantiles = quantiles
  )
# - save here -

# LOG CASES (Primary Focus) ----
climate_dir_pred.dt_2018_2021 <- climate_dir.pred.dt_2018_2021
tmp <- subset(
  ptl_province_inla_df,
  select = c("TIME", "PROVINCE", "POP_OFFSET", "LOG_CASES"))
climate_log_cases.dt_2018_2021 <- merge(climate_dir_pred.dt_2018_2021, tmp,
  by = c("TIME", "PROVINCE")
)
climate_log_cases.dt_2018_2021[, prediction := log1p(prediction * POP_OFFSET)]
climate_log_cases.dt_2018_2021[, true_value := log1p(true_value * POP_OFFSET)]
write.csv(climate_log_cases.dt_2018_2021,
  paste0(province.predictions.out.dir,
    "/pred_log_cases_samples_forecasting.csv"),
  row.names = FALSE
)

climate_2018_2021_log_cases_quantile_dt <-
  sample_to_quantile(climate_log_cases.dt_2018_2021,
    quantiles = quantiles
  )
write.csv(climate_2018_2021_log_cases_quantile_dt,
  paste0(province.predictions.out.dir,
    "/pred_log_cases_quantiles_forecasting.csv"),
  row.names = FALSE
)

log_info("Done with province_bayesian_forecasting.R")

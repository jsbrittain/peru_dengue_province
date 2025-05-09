library(sf)
library(sp)
library(raster)
library(logger)

# Setup folders
source("scripts/processing/packages_directories.R")
source("scripts/processing/extra_worldclim_funcs.R")  # list_worldclim_variable_tifs*()

district_peru_cases <- readRDS(file = file.path(peru.province.out.dir, "district_peru_cases.RDS"))
region_province <- unique(subset(district_peru_cases, select = c("PROVINCE", "REGION")))

dir.create(file.path("~/.cache/R/gb_cache"), recursive = TRUE, showWarnings = FALSE)
library(rgeoboundaries)
peru_sf <- geoboundaries("Peru", "adm1", quiet = TRUE)

extract_worldclim_variable_provinces_2010_2019 <- function(years_sequence, climate_variable) {
    worldclim_variable_dt <- data.table(YEAR = rep(years_sequence, each = 12 * length(region_province$PROVINCE)),
        PROVINCE = rep(region_province$PROVINCE, length(years_sequence)))
    worldclim_variable_dt[, MONTH := rep(rep(seq(1, 12), each = length(region_province$PROVINCE)),
        length(unique(years_sequence)))]
    worldclim_variable_dt[, TIME := rep(seq(1, 12 * length(unique(YEAR))), each = length(unique(PROVINCE)))]
    worldclim_variable_dt[, VALUE := rep(0, nrow(worldclim_variable_dt))]
    for (i in 1:length(years_sequence)) {
        year <- years_sequence[i]
        log_info("Processing year: ", year)
        climate_variable_monthly_files <- list_worldclim_variable_tifs(years_sequence[i],
            climate_variable)
        for (j in 1:12) {
            rds_file <- paste0(tools::file_path_sans_ext(climate_variable_monthly_files[j]),
                ".RDS")
            if (file.exists(rds_file)) {
                log_info("Reading from RDS file: ", rds_file)
                tmp_climate_provinces <- readRDS(rds_file)
            } else {
                log_info("Processing file: ", rds_file)
                tmp_climate_var_peru <- mask(raster(climate_variable_monthly_files[j]),
                  as_Spatial(peru_sf))
                tmp_climate_provinces <- (raster::extract(tmp_climate_var_peru, peru_district_boundaries2,
                  fun = "mean", weights = TRUE, na.rm = TRUE))
                log_info("Writing RDS file: ", rds_file)
                saveRDS(tmp_climate_provinces, file = rds_file)
            }
            # :::JSB::: added 'rep' worldclim_variable_dt[which(YEAR == year &
            # MONTH == j), VALUE := tmp_climate_provinces]
            worldclim_variable_dt[which(YEAR == year & MONTH == j), VALUE := rep(tmp_climate_provinces,
                length.out = .N)]
        }
    }
    setnames(worldclim_variable_dt, "VALUE", climate_variable)
    return(worldclim_variable_dt)
}
seq_2010_2019_province <- seq(2010, 2019, by = 1)

# Check for existing files, otherwise generate them
filename <- file.path(peru.province.out.dir, "tmax_2010_2019_province.RDS")
if (file.exists(filename)) {
    tmax_2010_2019_province <- readRDS(filename)
} else {
    tmax_2010_2019_province <- extract_worldclim_variable_provinces_2010_2019(seq_2010_2019_province,
        "tmax")
    saveRDS(tmax_2010_2019_province, file = filename)
}
filename <- file.path(peru.province.out.dir, "tmin_2010_2019_province.RDS")
if (file.exists(filename)) {
    tmin_2010_2019_province <- readRDS(filename)
} else {
    tmin_2010_2019_province <- extract_worldclim_variable_provinces_2010_2019(seq_2010_2019_province,
        "tmin")
    saveRDS(tmin_2010_2019_province, file = filename)
}
filename <- file.path(peru.province.out.dir, "prec_2010_2019_province.RDS")
if (file.exists(filename)) {
    prec_2010_2019_province <- readRDS(filename)
} else {
    prec_2010_2019_province <- extract_worldclim_variable_provinces_2010_2019(seq_2010_2019_province,
        "prec")
    saveRDS(prec_2010_2019_province, file = filename)
}

# 2) 2020-2021 data extraction function
log_info("WorldClim Data 2020-2021 data extraction function")
extract_worldclim_variable_provinces_20_21 <- function(years_sequence, climate_variable) {
    worldclim_variable_dt <- data.table(YEAR = rep(years_sequence, each = 12 * length(region_province$PROVINCE)),
        PROVINCE = rep(region_province$PROVINCE, length(years_sequence)))
    worldclim_variable_dt[, MONTH := rep(rep(seq(1, 12), each = length(region_province$PROVINCE)),
        length(unique(years_sequence)))]
    worldclim_variable_dt[, TIME := rep(seq(1, 12 * length(unique(YEAR))), each = length(unique(PROVINCE)))]
    worldclim_variable_dt[, VALUE := rep(0, nrow(worldclim_variable_dt))]
    for (i in 1:length(years_sequence)) {
        year <- years_sequence[i]
        log_info("Processing year: ", year)
        climate_variable_monthly_files <- list_worldclim_variable_tifs_20_21(years_sequence[i],
            climate_variable)
        for (j in 1:12) {
            rds_file <- paste0(tools::file_path_sans_ext(climate_variable_monthly_files[j]),
                ".RDS")
            if (file.exists(rds_file)) {
                log_info("Reading from RDS file: ", rds_file)
                tmp_climate_provinces <- readRDS(rds_file)
            } else {
                log_info("Processing file: ", rds_file)
                tmp_climate_var_peru <- mask(raster(climate_variable_monthly_files[j]),
                  as_Spatial(peru_sf))
                tmp_climate_provinces <- (raster::extract(tmp_climate_var_peru, peru_district_boundaries2,
                  fun = "mean", weights = TRUE, na.rm = TRUE))
                log_info("Writing RDS file: ", rds_file)
                saveRDS(tmp_climate_provinces, file = rds_file)
            }
            # :::JSB::: added 'rep' worldclim_variable_dt[which(YEAR == year &
            # MONTH == j), VALUE := tmp_climate_provinces]
            worldclim_variable_dt[which(YEAR == year & MONTH == j), VALUE := rep(tmp_climate_provinces,
                length.out = .N)]
        }
    }
    setnames(worldclim_variable_dt, "VALUE", climate_variable)
    return(worldclim_variable_dt)
}
seq_2020_2021_province <- seq(2020, 2021, by = 1)

# Check for existing files, otherwise generate them
filename <- file.path(peru.province.out.dir, "tmax_2020_2021_province.RDS")
if (file.exists(filename)) {
    tmax_2020_2021_province <- readRDS(filename)
} else {
    tmax_2020_2021_province <- extract_worldclim_variable_provinces_20_21(seq_2020_2021_province,
        "tmax")
    saveRDS(tmax_2020_2021_province, file = filename)
}
filename <- file.path(peru.province.out.dir, "tmin_2020_2021_province.RDS")
if (file.exists(filename)) {
    tmin_2020_2021_province <- readRDS(filename)
} else {
    tmin_2020_2021_province <- extract_worldclim_variable_provinces_20_21(seq_2020_2021_province,
        "tmin")
    saveRDS(tmin_2020_2021_province, file = filename)
}
filename <- file.path(peru.province.out.dir, "prec_2020_2021_province.RDS")
if (file.exists(filename)) {
    prec_2020_2021_province <- readRDS(filename)
} else {
    prec_2020_2021_province <- extract_worldclim_variable_provinces_20_21(seq_2020_2021_province,
        "prec")
    saveRDS(prec_2020_2021_province, file = filename)
}

library(sf)
library(terra)
library(raster)
library(logger)
library(remotes)
library(argparse)

dir.create(file.path("~/.cache/R/gb_cache"), recursive = TRUE, showWarnings = FALSE)
library(rgeoboundaries)

# Relies on the following data, which is produced by province_01a.R
peru.province.out.dir <- file.path("data", "output")
district_peru_cases <- readRDS(file = file.path(peru.province.out.dir, "district_peru_cases.RDS"))
region_province <- unique(subset(district_peru_cases, select = c("PROVINCE", "REGION")))

peru_sf <- geoboundaries("Peru", "adm1", quiet = TRUE)

peru.province.data.dir <- file.path("data", "shapefiles")
peru_district_boundaries2 <- st_read(file.path(peru.province.data.dir, "per_admbnda_adm2_ign_20200714.shp"))

parser <- ArgumentParser()
parser$add_argument("--input", "-i", help = "Input file")
parser$add_argument("--output", "-o", help = "Output file")
parser$add_argument("--dry-run", "-n", help = "Dry run", action = "store_true")
xargs <- parser$parse_args()

if (xargs$dry_run) {
  log_info("Dry run (no data will be read or written)")
}

log_info("Processing file: ", xargs$output)
if (!xargs$dry_run) {
  tmp_climate_var_peru <- mask(raster(xargs$input), as_Spatial(peru_sf))
  tmp_climate_provinces <- raster::extract(
    tmp_climate_var_peru,
    peru_district_boundaries2,
    fun = "mean",
    weights = TRUE,
    na.rm = TRUE
  )
}
log_info("Writing RDS file: ", xargs$output)
if (!xargs$dry_run) {
  saveRDS(tmp_climate_provinces, file = xargs$output)
}

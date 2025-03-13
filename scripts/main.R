# ======================================================================================
# Parameters
# ======================================================================================

models.baseline <- TRUE  # These get overwritten by intermediate workspace saves
models.bayesian <- FALSE
models.python <- FALSE

ensembles.cases <- FALSE
ensembles.incidence <- FALSE

# ======================================================================================
# Packages
# ======================================================================================

library(sf)
library(sp)
library(raster)

# ======================================================================================
# Data processing
# ======================================================================================

# Setup folders
source("scripts/processing/packages_directories.R")

# Ensure summarize() is taken from dplyr
library(conflicted)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(tsModel::Lag)

# This is geoboundaries cache - creating it avoids a user query (replace with option?)
dir.create(file.path("~/.cache/R/gb_cache"), recursive = TRUE, showWarnings = FALSE)
library(rgeoboundaries)
peru_sf <- geoboundaries("Peru", "adm1", quiet = TRUE)

# :::JSB::: This is an alternative implementation to geoboundaries (unverified)
if (FALSE) {
  install.packages('rnaturalearth')
  library(rnaturalearth)
  library(remotes)
  remotes::install_github("ropensci/rnaturalearthhires")
  peru_sf <- ne_states(country = "Peru", returnclass = "sf")
}

spi_file_name_func <- function(year) {
  return(
    file.path(
      peru.spi6.in.dir,
      paste0("spg06_m_wld_",year,"0101_",year,"1201_m.nc")
    )
  )
}

source("scripts/processing/extra_worldclim_funcs.R")
source("scripts/processing/province_01.R")
source('scripts/processing/province_02.R')

# (must be run to set up LAG_1_LOG_CASES before _any_ forecasting)
source('scripts/forecasting/province_python_setup.R')
source('scripts/processing/province_02b.R')

# ======================================================================================
# Forecasting
# ======================================================================================

# Forecasting
setwd("scripts/forecasting")

# Load the data
# ptl_province_inla_df = read.csv('ptl_province_inla_df.csv')
# setDT(ptl_province_inla_df)  # coerce to datatable

# Run baseline model
models.baseline <- TRUE  # Need to specify here to avoid overwriting
if (models.baseline) {
  source("province_baseline_forecaster.R")
  # Results plot
  baseline_plot <- ggplot(dat2)
    + geom_line(aes(x = target_end_date, y = prediction), color = "orange")
    + geom_line(aes(x = target_end_date, y = true_value), color = "forestgreen")
    + facet_wrap(location ~ ., scales = "free_y")
  ggsave(baseline_plot,file = "baseline_plot.png", h = 16, w = 18)
}

# Run Bayesian model
models.bayesian <- TRUE  # Need to specify here to avoid overwriting
if (models.bayesian) {
  library(VGAM)
  source("province_historical_bayesian_forecasting.R")
  source("province_bayesian_forecasting.R")
}

# Run the Python models
models.python <- FALSE
if (models.python) {
  p02_filename <- file.path(peru.province.out.dir, "province_02.RData")
  load(file = p02_filename)

  # Pre-processing in R
  source("province_python_setup.R")

  # Convert the Jupyter notebook to a Python script and run
  library(reticulate)
  notebook_path <- "scripts/forecasting/python_peru_forecast.ipynb"
  nbconvert_cmd <- sprintf("jupyter nbconvert --to script %s", notebook_path)
  log_info("Running command: %s", nbconvert_cmd)
  system(nbconvert_cmd)

  # Read the result back in
  source("province_python_forecasting.R")
}

# ======================================================================================
# Ensemble models
# ======================================================================================

# Case ensemble
ensembles.cases <- FALSE
if (ensembles.cases) {
  source("province_ensemble_cases.R")
}

# Incidence ensemble
ensembles.incidence <- FALSE
if (ensembles.incidence) {
  source("province_ensemble_incidence.R")
}

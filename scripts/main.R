# ======================================================================================
# Parameters
# ======================================================================================

models.baseline <- FALSE  # These get overwritten by intermediate workspace saves
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

# This is geoboundaries cache - creating it avoids a user query (replace with
# option?)
dir.create(file.path("~/.cache/R/gb_cache"), recursive = TRUE, showWarnings = FALSE)
library(rgeoboundaries)
peru_sf <- geoboundaries("Peru", "adm1", quiet = TRUE)

# :::JSB::: This is an alternative implementation to geoboundaries (unverified)
if (FALSE) {
    install.packages("rnaturalearth")
    library(rnaturalearth)
    library(remotes)
    remotes::install_github("ropensci/rnaturalearthhires")
    peru_sf <- ne_states(country = "Peru", returnclass = "sf")
}

source("scripts/processing/extra_worldclim_funcs.R")
source("scripts/processing/province_01.R")  # <-- can now be sourced in isolation
source("scripts/processing/province_02.R")

# (must be run to set up LAG_1_LOG_CASES before _any_ forecasting)
source("scripts/processing/province_export_csv.R")  # <-- runs in isolation
source("scripts/processing/province_02b.R")  # <-- runs in isolation

# ======================================================================================
# Forecasting
# ======================================================================================

# Forecasting
setwd("scripts/forecasting")

# Load the data ptl_province_inla_df = read.csv('ptl_province_inla_df.csv')
# setDT(ptl_province_inla_df) # coerce to datatable

# Run baseline model
models.baseline <- FALSE  # Need to specify here to avoid overwriting
if (models.baseline) {
    source("province_baseline_forecaster.R")
    # Results plot
    baseline_plot <- ggplot(dat2)
    +geom_line(aes(x = target_end_date, y = prediction), color = "orange")
    +geom_line(aes(x = target_end_date, y = true_value), color = "forestgreen")
    +facet_wrap(location ~ ., scales = "free_y")
    ggsave(baseline_plot, file = "baseline_plot.png", h = 16, w = 18)
}

# Run Bayesian model
models.bayesian <- FALSE  # Need to specify here to avoid overwriting
if (models.bayesian) {
    library(VGAM)
    source("province_historical_bayesian_forecasting.R")
    source("province_bayesian_forecasting.R")
}

# Run the Python models
models.python <- TRUE
if (models.python) {
    py_filename <- file.path(peru.province.out.dir, "python_forecasts.RData")
    if (file.exists(py_filename)) {
        log_info("Loading previous workspace (", py_filename, ")...")
        load(file = py_filename)
    } else {
        p02_filename <- file.path(peru.province.out.dir, "province_02.RData")
        load(file = p02_filename)

        # Pre-processing in R
        source("province_python_setup.R")

        # Convert the Jupyter notebook to a Python script and run
        library(reticulate)  # R interface to Python
        setwd("/app")
        # notebook_path <-
        # '/app/scripts/forecasting/python_peru_forecast.ipynb' nbconvert_cmd
        # <- sprintf('jupyter nbconvert --to script %s
        # --output=/app/data/python/python_peru_forecast', notebook_path)
        # log_info('Running command: ', nbconvert_cmd) system(nbconvert_cmd)
        system("python /app/scripts/forecasting/python_peru_forecast.py")

        # Read the result back in
        source("scripts/processing/packages_directories.R")
        source("scripts/forecasting/forecasting_funcs.R")
        source("scripts/forecasting/province_python_forecasting.R")
        log_info("Finishing running Python scripts.")

        save.image(file = py_filename)
    }
}

# ======================================================================================
# Ensemble models
# ======================================================================================

# Case ensemble
ensembles.cases <- FALSE
if (ensembles.cases) {
    setwd("/app")
    source("scripts/forecasting/province_log_cases.R")  # <-- runs in isolation
}

# # Incidence ensemble
# # --- should be identical to LOG_CASES (with substitutions)
#
# ensembles.incidence <- FALSE
# if (ensembles.incidence) {
#     setwd("/app")
#     source("scripts/forecasting/province_dir_ensemble_scoring.R")
# }

models.baseline = FALSE
models.bayesian = TRUE

package = "https://cran.r-project.org/package=scoringutils&version=1.2.2"
install.packages(pkgs = package, repos = NULL)

library(sf)
library(sp)
library(raster)
source('processing/packages_directories.R')

# Forecasting
setwd('forecasting')

# Load the data
ptl_province_inla_df = read.csv('ptl_province_inla_df.csv')
setDT(ptl_province_inla_df)  # coerce to datatable

# Run baseline model
if (models.baseline) {
  source('province_baseline_forecaster.R')
}

# Run Bayesian model
if (models.bayesian) {
  source('province_historical_bayesian_forecasting.R')
  source('province_bayesian_forecasting.R')
}

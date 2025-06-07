#!/usr/bin/env Rscript

library(styler)
style_dir('scripts')

library(lintr)
lintr::lint('scripts/forecasting/forecast_baseline.R')

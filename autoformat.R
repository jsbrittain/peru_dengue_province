#!/usr/bin/env Rscript

library(styler)
style_dir('scripts')
style_dir('workflows')

library(lintr)
lintr::lint('scripts/forecasting/forecast_baseline.R')

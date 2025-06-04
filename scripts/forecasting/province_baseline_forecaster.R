library(dplyr)
library(logger)
library(lubridate)
library(data.table)
library(scoringutils)

# --- Setup ----------------------------------------------------------------------------

force_rerun <- TRUE

peru.province.base.dir <- file.path(getwd(), "data")
peru.province.out.dir <- file.path(peru.province.base.dir, "output")
peru.province.python.data.dir <- file.path(peru.province.base.dir, "python/data")
peru.province.temp.out.dir <- file.path(peru.province.base.dir, "temp")

peru.province.predictions.out.dir <- file.path(getwd(), "predictions")

# Ensure output folders exist
dir.create(peru.province.temp.out.dir, recursive=TRUE, showWarnings=FALSE)
dir.create(province.predictions.out.dir, recursive=TRUE, showWarnings=FALSE)

# --- Minimal required dataset ---------------------------------------------------------

# Load data
log_info("Load data")
df <- data.table(
    read.csv(file.path(peru.province.python.data.dir, "ptl_province_inla_df.csv"))
)

# This is the subset of columns (out of the original 58) that are actually required for
# this analysis.

df <- df[, .(
    # identifiers
    PROVINCE,  
    YEAR,
    MONTH,
    # measures
    CASES,
    POP
)]

# --- Derive required metrics ----------------------------------------------------------

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

# Derive end_of_month
df <- df %>%
  mutate(end_of_month = ceiling_date(make_date(YEAR, MONTH, 1), "month") - days(1))

# Derive LAG_1_LOG_CASES
df <- df %>%
  arrange(PROVINCE, TIME) %>%
  group_by(PROVINCE) %>%
  mutate(
    LAG_1_LOG_CASES = lag(log1p(CASES), 1)
  ) %>%
  ungroup()

# Add additional required columns
df <- df %>%
  mutate(
    POP_OFFSET = POP/1e5,
    DIR := CASES/POP*1e5,
    PROV_IND = as.integer(factor(PROVINCE)),  # province indexer (temp)
    LAT_PROV_IND = PROV_IND,
    LONG_PROV_IND = PROV_IND,
    YEAR_DECIMAL = YEAR + (MONTH - 1) / 12,
    LAG_1_CASES = expm1(LAG_1_LOG_CASES),
    DIFF_CASES = CASES - LAG_1_CASES
  ) %>%
  select(-PROV_IND)  # remove province indexer

# Revert to data frame and rename for processing
ptl_province_inla_df = data.table(df)

# --- Modelling functions --------------------------------------------------------------

# Create quantile baseline object
new_quantile_baseline <- function(inc_diffs, symmetrize = TRUE) {
  baseline <- if (symmetrize) c(inc_diffs, -inc_diffs) else inc_diffs
  structure(baseline, symmetrize = symmetrize, class = "quantile_baseline")
}

# Predict using quantile baseline object
new_predict.quantile_baseline <- function(quantile_baseline, newdata, horizon, nsim, ...) {
  result <- matrix(NA_real_, nrow = nsim, ncol = horizon)
  last_inc <- newdata

  sampled_inc_diffs <- quantile(quantile_baseline, probs = seq(0, 1, length.out = nsim), na.rm = TRUE)

  for (h in seq_len(horizon)) {
    sampled_inc_diffs <- sample(sampled_inc_diffs, size = nsim, replace = FALSE)
    sampled_inc_raw <- last_inc + sampled_inc_diffs

    if (isTRUE(attr(quantile_baseline, "symmetrize"))) {
      sampled_inc_corrected <- sampled_inc_raw - (median(sampled_inc_raw) - last_inc)
    } else {
      sampled_inc_corrected <- sampled_inc_raw
    }

    result[, h] <- sampled_inc_corrected
  }

  return(result)
}

# Predict and save results per province and time
predict_and_save_baseline <- function(data, province, time, i, j, out_dir, nsim = 5000) {
  tmp_data <- subset(data, PROVINCE == province & TIME <= time)
  newdata <- tmp_data[tmp_data$TIME == time, ]$CASES

  baseline <- new_quantile_baseline(tmp_data$DIFF_CASES)
  preds <- new_predict.quantile_baseline(baseline, newdata, horizon = 1, nsim = nsim)

  result_dt <- data.table(PRED = preds)
  result_dt[, `:=`(
    PROVINCE = province,
    SAMPLE = .I,
    TIME = time + 1
  )]

  filename <- file.path(out_dir, sprintf("baseline_results_%s_%s.RDS", i, j))
  saveRDS(result_dt, file = filename)
  return(filename)
}

# Fit baseline model to all provinces and time points
fit_baseline_function_to_all_data <- function(data, predict_times, out_dir, nsim = 5000) {
  log_info("Fitting baseline model for all provinces and times...")

  provinces <- unique(data$PROVINCE)

  filenames <- unlist(lapply(seq_along(provinces), function(i) {
    prov <- provinces[i]
    lapply(seq_along(predict_times), function(j) {
      time <- predict_times[j]
      log_info(sprintf("Predicting %s [%d], time %d [%d]", prov, i, time, j))
      predict_and_save_baseline(data, prov, time, i, j, out_dir, nsim)
    })
  }))

  log_info("Reading and combining prediction files...")
  results_dt <- rbindlist(lapply(filenames, readRDS), use.names = TRUE, fill = TRUE)
  results_dt[, `:=`(
    PROVINCE = as.factor(PROVINCE),
    TIME = as.integer(TIME),
    SAMPLE = as.integer(SAMPLE)
  )]

  return(results_dt)
}

# --- Run baseline model ---------------------------------------------------------------

# Note: The baseline model predictions 'cases' and derives log_cases, DIR from those
# predictions. In addition, all data is analysed, with historical and forecast values
# simply separated and saved after the fact.

log_info("Run baseline model")
predict_times <- unique(ptl_province_inla_df[which(TIME < max(TIME))]$TIME)
baseline_results_filename <- file.path(peru.province.temp.out.dir, "baseline_results_2010_2021.RDS")
if ((!force_rerun) && file.exists(baseline_results_filename)) {
    baseline_results <- readRDS(baseline_results_filename)
} else {
    baseline_results <- fit_baseline_function_to_all_data(ptl_province_inla_df, predict_times, peru.province.temp.out.dir)
    saveRDS(baseline_results, file = baseline_results_filename)
}

# Tidy-up predictions table
setDT(baseline_results)
setnames(baseline_results, c("PRED.V1", "SAMPLE"), c("prediction", "sample"))
baseline_results <- merge(
    baseline_results,
    subset(ptl_province_inla_df,
        select = c(
            "PROVINCE", "TIME", "MONTH", "YEAR", "CASES", "LAT_PROV_IND",
            "LONG_PROV_IND", "end_of_month", "DIR", "POP_OFFSET"
    )),
    by = c("PROVINCE", "TIME")
)
baseline_results[, model := "baseline"]

# --- Dengue Incidence Rate ------------------------------------------------------------

# These outputs seem incomplete compared with the cases section, below

log_info("Dengue incidence rate...")
baseline_dir.pred_dt_2010_2021 <- copy(baseline_results)
baseline_dir.pred_dt_2010_2021[, true_value := DIR]  # take original DIR as true value
baseline_dir.pred_dt_2010_2021[, prediction := prediction/POP_OFFSET]  # compute DIR prediction from predicted cases

# Separate testing period (>= 2018) and compute quantiles
baseline_dir.pred_dt_2018_2021 <- subset(baseline_dir.pred_dt_2010_2021, YEAR >= 2018)
quantile_baseline_dir.pred_dt_2018_2021 <- sample_to_quantile(  # scoringutils
    baseline_dir.pred_dt_2018_2021,
    quantiles = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99))
saveRDS(quantile_baseline_dir.pred_dt_2018_2021, file = file.path(peru.province.out.dir,
    "quantile_baseline_dir.pred_dt_2018_2021.RDS"))

# --- Log Cases ------------------------------------------------------------------------

log_info("Log cases...")
baseline_log_cases.pred_dt_2010_2021 <- copy(baseline_results)
setnames(baseline_log_cases.pred_dt_2010_2021, "CASES", "true_value")

baseline_log_cases.pred_dt_2010_2021[which(prediction < 0), prediction := 0]
baseline_log_cases.pred_dt_2010_2021[, true_value := log1p(true_value)]
baseline_log_cases.pred_dt_2010_2021[, prediction := log1p(prediction)]

# Historical cases
log_info("Saving historical predictions")
baseline_log_cases.pred_dt_2010_2018 <- subset(baseline_log_cases.pred_dt_2010_2021, YEAR < 2018)
write.csv(baseline_log_cases.pred_dt_2010_2018,
    paste0(peru.province.predictions.out.dir, "/pred_log_cases_samples_historical.csv"), row.names=FALSE)

# Historical quantiles
quantile_baseline_log_cases.pred_dt_2010_2018 <- sample_to_quantile(baseline_log_cases.pred_dt_2010_2018,
    quantiles = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99))
write.csv(quantile_baseline_log_cases.pred_dt_2010_2018,
    paste0(peru.province.predictions.out.dir, "/pred_log_cases_quantiles_historical.csv"), row.names=FALSE)

# Forecast cases
log_info("Saving forecasting predictions")
baseline_log_cases.pred_dt_2018_2021 <- subset(baseline_log_cases.pred_dt_2010_2021, YEAR >= 2018)
write.csv(baseline_log_cases.pred_dt_2018_2021,
    paste0(peru.province.predictions.out.dir, "/pred_log_cases_samples_forecasting.csv"), row.names=FALSE)

# Forecast quantiles
quantile_baseline_log_cases.pred_dt_2018_2021 <- sample_to_quantile(baseline_log_cases.pred_dt_2018_2021,
    quantiles = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99))
write.csv(quantile_baseline_log_cases.pred_dt_2018_2021,
    paste0(peru.province.predictions.out.dir, "/pred_log_cases_quantiles_forecasting.csv"), row.names=FALSE)

# --------------------------------------------------------------------------------------

log_info("Finished province_baseline_forecaster.R")

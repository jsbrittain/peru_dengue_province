library(dplyr)
library(logger)
library(runner)
library(scales)
library(foreach)
library(stringr)
library(tsModel)
library(quantgen)
library(doParallel)
library(data.table)
library(scoringutils)

metric_id = 1
if (metric_id == 1) {
    metric_name = "log_cases"
    metric_field = "LOG_CASES"
} else {
    metric_name = "dir"
    metric_field = "DIR"
}

source('scripts/ensemble/funcs/form_untrained_ensembles.R')
source('scripts/ensemble/funcs/iteratively_train_quantile_ensemble_by_province.R')

peru.province.base.dir <- file.path(getwd(), "data")
peru.province.out.dir <- file.path(peru.province.base.dir, "output")
peru.province.python.out.dir <- file.path(peru.province.base.dir, "python/output")
peru.province.python.data.dir <- file.path(peru.province.base.dir, "python/data")
peru.province.inla.data.out.dir <- file.path(peru.province.base.dir, "INLA/Output")
peru.province.predictions.out.dir <- file.path(getwd(), "predictions")
peru.province.analysis.out.dir <- file.path(getwd(), "analysis")

# Quantiles
quantiles <- c(
  0.01, 0.025,
  seq(0.05, 0.95, 0.05),
  0.975, 0.99
)

find_quantile_violations <- function(df) {
    violations <- df %>%
      arrange(location, target_end_date, quantile, model) %>%
      group_by(location, target_end_date, model) %>%
      mutate(
        prev_prediction = lag(prediction),
        prev_quantile = lag(quantile),
        decreasing = prediction < cummax(prediction)
      ) %>%
      filter(decreasing)

    print(violations, width=Inf)
}

fix_quantile_violations <- function(df, tolerance = 1e-8) {
  df_fixed <- df %>%
    arrange(location, target_end_date, quantile, model) %>%
    group_by(location, target_end_date, model) %>%
    group_split() %>%
    lapply(function(data) {
      for (i in 2:nrow(data)) {
        prev <- data$prediction[i - 1]
        curr <- data$prediction[i]

        # Fix if drop is within tolerance
        if (!is.na(prev) && (prev > curr) && ((prev - curr) <= tolerance)) {
          data$prediction[i] <- prev
        }
      }
      return(data)
    }) %>%
    bind_rows()

  return(df_fixed)
}

# Load in data ----
log_info("Load data")
ptl_province_inla_df <- data.table(read.csv(file.path(
  peru.province.python.data.dir,
  "ptl_province_inla_df.csv"
)))
ptl_province_2018_2021_data <- subset(
  ptl_province_inla_df,
  YEAR >= 2018
)
ptl_province_2018_2021_data[, IND := seq(1, length(DIR)), by = "PROVINCE"]
ptl_province_2010_2018_data <- subset(
    ptl_province_inla_df,
    YEAR < 2018
)
ptl_province_2010_2018_data[, IND := seq(1, length(DIR)), by = "PROVINCE"]
scoring_columns <- c(
  "location", "true_value", "model", "target_end_date",
  "quantile", "prediction"
)

# Load in forecasts ----
# 1) SARIMA
log_info("Load SARIMA")
quantile_sarima_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_sarima_preds_dt.RDS"
))

# 2) TIMEGPT
log_info("Load TimeGPT")
quantile_fine_tuned_timegpt_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_fine_tuned_timegpt_preds_dt.RDS"
))
quantile_fine_tuned_timegpt_preds_dt <- subset(quantile_fine_tuned_timegpt_preds_dt,
  select = scoring_columns
)

# 3) DEEPTCN
log_info("Load DeepTCN")
quantile_tcn_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_tcn_preds_dt.RDS"
))
quantile_tcn_preds_dt <- subset(quantile_tcn_preds_dt,
  select = scoring_columns
)
# 4) TIMEGPT(No Covars)
log_info("TimeGPT (no covars)")
quantile_finetuned_no_covars_timegpt_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_finetuned_no_covars_timegpt_preds_dt.RDS"
))
quantile_finetuned_no_covars_timegpt_preds_dt <- subset(quantile_finetuned_no_covars_timegpt_preds_dt,
  select = scoring_columns
)

# 5) Climate
log_info("Load climate")
climate_2018_2021_log_cases_quantile_dt <- readRDS(file = file.path(peru.province.inla.data.out.dir, paste0("climate_2018_2021_", metric_name, "_quantile_dt.RDS")))
climate_2018_2021_log_cases_quantile_dt <- merge(climate_2018_2021_log_cases_quantile_dt,
  subset(ptl_province_2018_2021_data,
    select = c("PROVINCE", "TIME", "MONTH", "YEAR", "end_of_month")
  ),
  by = c("PROVINCE", "TIME", "MONTH", "YEAR")
)
setnames(
  climate_2018_2021_log_cases_quantile_dt, c("PROVINCE", "end_of_month"),
  c("location", "target_end_date")
)
climate_2018_2021_log_cases_quantile_dt <- subset(climate_2018_2021_log_cases_quantile_dt,
  select = scoring_columns
)

# 6)Baseline
log_info("Baseline")
baseline_log_cases.pred_dt_2010_2018 <- data.table(read.csv(file = file.path(
  peru.province.predictions.out.dir,
  paste0("baseline/pred_", metric_name, "_samples_forecasting.csv")
)))
quantile_baseline_log_cases.pred_dt_2018_2021 <- data.table(read.csv(file = file.path(
  peru.province.predictions.out.dir,
  paste0("baseline/pred_", metric_name, "_quantiles_forecasting.csv")
)))
quantile_baseline_log_cases.pred_dt_2018_2021[which(quantile == 0.5), caret::MAE(prediction, true_value)]
quantile_baseline_log_cases.pred_dt_2018_2021[, target_end_date := as.character(target_end_date)]  # JSB


# Combine all into single data.table
log_info("Combine all into single data.table")
quantile_log_cases_components_dt <- rbind(
  quantile_sarima_preds_dt,
  quantile_tcn_preds_dt,
  quantile_fine_tuned_timegpt_preds_dt,
  quantile_finetuned_no_covars_timegpt_preds_dt,
  climate_2018_2021_log_cases_quantile_dt,
  quantile_baseline_log_cases.pred_dt_2018_2021
)
quantile_log_cases_components_dt[, true_value := round(true_value, 8)]
quantile_log_cases_components_dt[, length(unique(true_value)), by = "model"]
quantile_log_cases_components_dt[, target_end_date := as.Date(target_end_date)]
quantile_log_cases_components_dt[which(prediction < 0), prediction := 0]
quantile_log_cases_components_dt %>% fix_quantile_violations
quantile_log_cases_components_dt %>%
  score() %>%
  summarise_scores(by = c("model"))
quantile_log_cases_components_plus_ensembles_dt <-
  form_untrained_ensembles(quantile_log_cases_components_dt)
quantile_log_cases_components_plus_ensembles_dt[, length(prediction), by = "model"]

# Trained ensembles ----
log_info("Trained ensembles")

# Train ensemble using quantile loss with iterative updating to reflect
# real-world conditions of information becoming iteratively available

iteratively_train_quantile_ensemble <- function(quantiles,
                                                number_testing_dates,
                                                input_quantile_dt,
                                                pred_points_dt,
                                                historical_input_quantile_dt,
                                                true_data,
                                                metric_field) {
  # ovr_num_pred_points = total number points (includes all quantile predictions)
  # input_quantile_dt is predictions for entire testing window (2018-2021)
  # historical_input_quantile_dt is historical predictions

  # prediction_indices <- sort(unique(input_quantile_dt$IND))

  model_names <- sort(unique(input_quantile_dt$model))
  # log_info(paste0(model_names))
  ensemble_model_name <- paste(model_names, collapse = "_")
  # testing_dates <- sort(unique(input_quantile_dt$target_end_date))
  testing_dates <- sort(unique(pred_points_dt$target_end_date))
  num_ensemble_components <- length(unique(input_quantile_dt$model))
  num_quantile_levels <- length(quantiles)

  # data.tables to store results
  ovr_preds_dt <- NULL
  ovr_model_weights_dt <- NULL

  # JSB: This can be re-written and parallelised (snakemake)
  for (t in 1:number_testing_dates) {
    log_info(paste0("iteratively_train_quantile_ensemble (", testing_dates[t],
        ", t = ", t, " of ", number_testing_dates, ")"))
    new_date <- testing_dates[t]
    # number of historical pred points -- JSB: Iteratively adds rows to analysis df
    tmp_true_data <- subset(true_data, end_of_month <= new_date)
    testing_input_quantile_dt <- subset(input_quantile_dt, target_end_date == new_date)
    historical_input_quantile_dt <- rbind(
      historical_input_quantile_dt,
      testing_input_quantile_dt
    )
    num_pred_points <- nrow(historical_input_quantile_dt) / num_quantile_levels
    num_pred_points <- num_pred_points / num_ensemble_components
    # log_info(num_pred_points)
    setkeyv(historical_input_quantile_dt, c(
      "location", "model",
      "target_end_date", "quantile"
    ))
    qarr <- array(NA, dim = c(
      num_pred_points,
      num_ensemble_components,
      num_quantile_levels
    ))
    prediction_indices <- sort(unique(historical_input_quantile_dt$IND))
    for (i in 1:num_pred_points) {
      for (j in 1:num_ensemble_components) {
        qarr[i, j, ] <-
          historical_input_quantile_dt[which(IND == prediction_indices[i] &
            model == unique(model_names[j])), ]$prediction
      }
    }
    quantile_ensemble_weights <- quantile_ensemble(
      qarr,
      tmp_true_data[[metric_field]],
      quantiles
    )
    tmp_trained_quantile_ensemble <- copy(testing_input_quantile_dt)
    model_weights_dt <- data.table(model = model_names, weights = quantile_ensemble_weights$alpha)
    tmp_trained_quantile_ensemble <-
      merge(tmp_trained_quantile_ensemble,
        model_weights_dt,
        by = "model"
      )
    model_weights_dt[, target_end_date := unique(testing_input_quantile_dt$target_end_date)]
    ovr_model_weights_dt <- rbind(
      ovr_model_weights_dt,
      model_weights_dt
    )
  }
  return(ovr_model_weights_dt)
}

iteratively_train_quantile_ensemble_parallel <- function(quantiles,
                                                number_testing_dates,
                                                input_quantile_dt,
                                                pred_points_dt,
                                                historical_input_quantile_dt,
                                                true_data,
                                                metric_field) {
  model_names <- sort(unique(input_quantile_dt$model))
  ensemble_model_name <- paste(model_names, collapse = "_")
  testing_dates <- sort(unique(pred_points_dt$target_end_date))
  num_ensemble_components <- length(model_names)
  num_quantile_levels <- length(quantiles)

  # Setup parallel backend
  num_cores <- parallel::detectCores() - 1
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  # Parallel loop
  results <- foreach(t = 1:number_testing_dates, .combine = rbind, .packages = c("data.table", "logger"), .export = c("quantile_ensemble")) %dopar% {
    log_info(paste0("iteratively_train_quantile_ensemble (", testing_dates[t],
                    ", t = ", t, " of ", number_testing_dates, ")"))

    new_date <- testing_dates[t]

    tmp_true_data <- subset(true_data, end_of_month <= new_date)
    testing_input_quantile_dt <- subset(input_quantile_dt, target_end_date == new_date)

    local_historical <- rbind(copy(historical_input_quantile_dt), testing_input_quantile_dt)

    num_pred_points <- nrow(local_historical) / num_quantile_levels / num_ensemble_components
    setkeyv(local_historical, c("location", "model", "target_end_date", "quantile"))

    qarr <- array(NA, dim = c(num_pred_points, num_ensemble_components, num_quantile_levels))
    prediction_indices <- sort(unique(local_historical$IND))

    for (i in 1:num_pred_points) {
      for (j in 1:num_ensemble_components) {
        qarr[i, j, ] <- local_historical[IND == prediction_indices[i] & model == model_names[j], prediction]
      }
    }

    quantile_ensemble_weights <- quantile_ensemble(
      qarr,
      tmp_true_data[[metric_field]],
      quantiles
    )

    model_weights_dt <- data.table(
      model = model_names,
      weights = quantile_ensemble_weights$alpha,
      target_end_date = new_date
    )

    return(model_weights_dt)
  }

  stopCluster(cl)
  return(results)
}

num_pred_points <- nrow(ptl_province_2018_2021_data)
input_quantile_dt <- copy(quantile_log_cases_components_dt)
setkeyv(input_quantile_dt, c("location", "model", "target_end_date", "quantile"))
num_ensemble_components <- length(unique(input_quantile_dt$model))
num_quantile_levels <- length(quantiles)
pred_points_dt <- unique(subset(input_quantile_dt,
  select = c("location", "model", "target_end_date")
))
setkeyv(pred_points_dt, c("model", "location", "target_end_date"))
pred_points_dt[, IND := seq(1, length(target_end_date)), by = "model"]
input_quantile_dt <- merge(input_quantile_dt, pred_points_dt, by = c("model", "location", "target_end_date"))
setkeyv(input_quantile_dt, c("location", "model", "target_end_date", "quantile"))
number_testing_dates = length(unique(pred_points_dt$target_end_date))
true_log_cases_data <- copy(ptl_province_inla_df)

min_date <- min(input_quantile_dt$target_end_date)

log_info(paste0("iteratively_trained_weights_", metric_name, "_dt"))
iteratively_trained_weights_log_cases_dt <- iteratively_train_quantile_ensemble_parallel(
  quantiles = quantiles,
  number_testing_dates = number_testing_dates,
  input_quantile_dt = input_quantile_dt,
  pred_points_dt,
  historical_input_quantile_dt = NULL,
  ptl_province_2018_2021_data,
  metric_field = metric_field
)
saveRDS(iteratively_trained_weights_log_cases_dt,
  file = file.path(peru.province.out.dir, paste0("iteratively_trained_weights_",metric_name ,"_dt.RDS"))
)

iteratively_trained_weights_log_cases_dt[, model_factor := factor(model)]
iteratively_trained_weights_log_cases_dt[, target_end_date := as.Date(target_end_date)]
input_quantile_dt[, target_end_date := as.Date(target_end_date)]
weighted_quantile_log_cases_dt <- merge(iteratively_trained_weights_log_cases_dt,
  unique(subset(input_quantile_dt,
    select = c(
      "IND", "model", "target_end_date", "location", "true_value",
      "prediction", "quantile"
    )
  )),
  by = c("model", "target_end_date")
)
setkeyv(weighted_quantile_log_cases_dt, c("location", "IND"))
weighted_quantile_log_cases_dt[, true_value := round(true_value, digits = 8)]
weighted_quantile_log_cases_dt <- unique(weighted_quantile_log_cases_dt)
weighted_quantile_log_cases_dt[, TRAINED_WEIGHT := Lag(weights, 1), by = c("location", "model", "quantile")]
weighted_quantile_log_cases_dt[which(target_end_date == min(target_end_date)), TRAINED_WEIGHT := 1 / length(unique(model))]
weighted_quantile_log_cases_dt
weighted_quantile_log_cases_forecasts_2018_2021_dt <-
  weighted_quantile_log_cases_dt[, list(
    prediction = sum(TRAINED_WEIGHT * prediction),
    true_value = unique(true_value)
  ),
  by = c("location", "quantile", "target_end_date")
  ]
weighted_quantile_log_cases_forecasts_2018_2021_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
weighted_quantile_log_cases_forecasts_2018_2021_dt[which(quantile == 0.5), caret::MAE(prediction, true_value)]
weighted_quantile_log_cases_forecasts_2018_2021_dt[, model := "pinball_trained_ensemble"]

# 2) Province-dependent
log_info("Province-dependent")
iteratively_trained_weights_dt_by_province <- iteratively_train_quantile_ensemble_by_province(
  quantiles = quantiles,
  number_testing_dates = number_testing_dates,
  input_quantile_dt = input_quantile_dt,
  pred_points_dt = pred_points_dt,
  historical_input_quantile_dt = NULL,
  true_data = ptl_province_2018_2021_data,
  data_field = metric_field
)
iteratively_trained_weights_dt_by_province

# WIS over time (expanding window) ----
unique(quantile_log_cases_components_dt$target_end_date)

# Use later (move to outbreak_functions script) ----
log_info("WIS expanding window")
wis_expanding_window_over_time <- function(models_dt,
                                           times) {
  # times <- unique(models_dt$target_end_date)
  # print(times)
  score_summary <- NULL
  score_summary_by_location <- NULL

  for (i in seq(1, length(times) - 1, 1)) {
    log_info(paste0("wis_expanding_window_over_time (i = ", i, " of ", length(times) - 1, ")"))
    time_in_q <- times[i]
    expanding_window_dt <- subset(models_dt, target_end_date <= time_in_q)
    # print(expanding_window_dt)
    tmp_score_summary <- expanding_window_dt %>%
      score() %>%
      summarise_scores(by = c("model"))
    tmp_score_summary[, effective_time := times[i + 1]] # effective time is time at which we use weights
    tmp_score_summary[, weight := interval_score / sum(interval_score)]
    score_summary <- rbind(
      score_summary,
      tmp_score_summary
    )
    # print(paste0("Here?"))
    tmp_score_summary_by_location <- expanding_window_dt %>%
      score() %>%
      summarise_scores(by = c("model", "location"))
    tmp_score_summary_by_location[, effective_time := times[i + 1]] # effective time is time at which we use weights
    tmp_score_summary_by_location[, weight := interval_score / sum(interval_score), by = "location"]
    score_summary_by_location <- rbind(
      score_summary_by_location,
      tmp_score_summary_by_location
    )
  }
  return(list(
    score_summary,
    score_summary_by_location
  ))
}
wis_expanding_results <- wis_expanding_window_over_time(quantile_log_cases_components_dt,
  times = unique(quantile_log_cases_components_dt$target_end_date)
)
wis_expanding_results_spatially_homoegenous <- wis_expanding_results[[1]]
wis_expanding_results_space_dependent <- wis_expanding_results[[2]]
wis_expanding_results_space_dependent

# 1) Iterative WIS-based weighting (Spatially homogeneous)
# Add equal weight for first month
tmp <- subset(
  wis_expanding_results_spatially_homoegenous,
  effective_time == min(effective_time)
)
tmp[, weight := 1 / length(unique(model))]
tmp[, effective_time := min(quantile_log_cases_components_dt$target_end_date)]
wis_expanding_results_spatially_homoegenous <- rbind(tmp, wis_expanding_results_spatially_homoegenous)
wis_expanding_results_spatially_homoegenous[, model_factor := factor(model)]
wis_expanding_results_spatially_homoegenous[, target_end_date := as.Date(effective_time)]

# WIS over time (Rolling Window) ----
log_info("WIS rolling window")
wis_rolling_window_over_time <- function(models_dt, times) {
  # times <- unique(models_dt$target_end_date)
  score_summary <- NULL
  score_summary_by_location <- NULL
  runner_windows_list <- runner(times, k = 3)
  runner_windows_list <- runner_windows_list[3:length(runner_windows_list)]
  for (i in seq(1, length(runner_windows_list), 1)) {
    log_info(paste0("wis_rolling_window_over_time (i = ", i, " of ", length(runner_windows_list), ")"))
    times_in_q <- runner_windows_list[[i]]
    min_time <- times_in_q[1]
    centering_time <- times_in_q[2]
    max_time <- times_in_q[3]


    rolling_window_dt <- subset(models_dt, target_end_date >= min_time &
      target_end_date <= max_time)

    rolling_window_dt <- fix_quantile_violations(rolling_window_dt)

    tmp_score_summary <- rolling_window_dt %>%
      score() %>%
      summarise_scores(by = c("model"))
    tmp_score_summary[, centering_date := centering_time]
    score_summary <- rbind(
      score_summary,
      tmp_score_summary
    )
    # print(paste0("Here?"))
    tmp_score_summary_by_location <- rolling_window_dt %>%
      score() %>%
      summarise_scores(by = c("model", "location"))
    tmp_score_summary_by_location[, centering_date := centering_time]
    score_summary_by_location <- rbind(
      score_summary_by_location,
      tmp_score_summary_by_location
    )
  }
  return(list(
    score_summary,
    score_summary_by_location
  ))
}
quantile_log_cases_components_plus_ensembles_dt[, target_end_date := as.Date(target_end_date)]

log_info("Computing WIS (rolling window)")
wis_combined <- wis_rolling_window_over_time(quantile_log_cases_components_plus_ensembles_dt, times=unique(quantile_log_cases_components_plus_ensembles_dt$target_end_date))

wis_summary <- wis_combined[[1]]
wis_summary[, target_end_date := as.Date(centering_date)]

wis_province <- wis_combined[[2]]
wis_province[, target_end_date := as.Date(centering_date)]

log_info("Writing WIS over time (rolling window) - Summary...")
dir.create(file.path(peru.province.analysis.out.dir,
    "wis"), recursive = TRUE, showWarnings = FALSE)
write.csv(wis_summary,
    file.path(peru.province.analysis.out.dir, "wis", "wis_summary.csv"),
    row.names=FALSE)
log_info("Writing WIS over time (rolling window) - Provinces...")
write.csv(wis_province,
    file.path(peru.province.analysis.out.dir, "wis", "wis_province.csv"),
    row.names=FALSE)
log_info("Written WIS over time (rolling window).")

# Historical models ----
quantile_baseline_log_cases.pred_dt_2010_2018 <- data.table(read.csv(file = file.path(
  peru.province.predictions.out.dir,
  paste0("baseline/pred_", metric_name, "_quantiles_historical.csv")
)))
quantile_baseline_log_cases.pred_dt_2010_2018[, target_end_date := as.character(target_end_date)]  # JSB
quantile_historical_tcn_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_historical_tcn_preds_dt.RDS"
))

quantile_historical_sarima_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_historical_sarima_preds_dt.RDS"
))

quantile_historical_no_covars_fine_tuned_timegpt_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_historical_no_covars_fine_tuned_timegpt_preds_dt.RDS"
))
quantile_historical_no_covars_fine_tuned_timegpt_preds_dt <- subset(quantile_historical_no_covars_fine_tuned_timegpt_preds_dt, select = scoring_columns)

quantile_historical_no_covars_fine_tuned_timegpt_preds_dt$model <- "finetuned_no_covars_timegpt"

quantile_historical_fine_tuned_timegpt_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_historical_fine_tuned_timegpt_preds_dt.RDS"
))
quantile_historical_fine_tuned_timegpt_preds_dt <- subset(quantile_historical_fine_tuned_timegpt_preds_dt, select = scoring_columns)

climate_2010_2018_log_cases_quantile_dt <- readRDS(file = file.path(peru.province.inla.data.out.dir, paste0("climate_2010_2018_", metric_name, "_quantile_dt.RDS")))
climate_2010_2018_log_cases_quantile_dt <- merge(climate_2010_2018_log_cases_quantile_dt,
  subset(ptl_province_2010_2018_data,
    select = c("PROVINCE", "TIME", "MONTH", "YEAR", "end_of_month")
  ),
  by = c("PROVINCE", "TIME", "MONTH", "YEAR")
)
setnames(
  climate_2010_2018_log_cases_quantile_dt, c("PROVINCE", "end_of_month"),
  c("location", "target_end_date")
)
climate_2010_2018_log_cases_quantile_dt <- subset(climate_2010_2018_log_cases_quantile_dt,
  select = scoring_columns
)
climate_2010_2018_log_cases_quantile_dt


historical_quantile_log_cases_components_dt <- rbind(
  quantile_historical_fine_tuned_timegpt_preds_dt,
  quantile_historical_no_covars_fine_tuned_timegpt_preds_dt,
  quantile_historical_tcn_preds_dt,
  climate_2010_2018_log_cases_quantile_dt,
  quantile_historical_sarima_preds_dt,
  quantile_baseline_log_cases.pred_dt_2010_2018
)
historical_quantile_log_cases_components_dt[which(prediction < 0), prediction := 0]
historical_quantile_log_cases_components_dt[, target_end_date := as.Date(target_end_date)]

# EW-NoC
# Median-NoC

# EW-NoBCM
# Median-NoBCM

# EW-NoB
# Median-NoB

# historical_quantile_log_cases_components_dt[, length(which(is.na(prediction))), by = "model"]
find_min_date_no_nas_after <- function(models_dt) {
  tmp <- models_dt[which(is.na(prediction)), ]
  tmp[, target_end_date]
  max_date <- max(tmp$target_end_date)
  return(max_date)
}
min_date <- find_min_date_no_nas_after(historical_quantile_log_cases_components_dt)
min_date <- max(historical_quantile_log_cases_components_dt[, list(MIN_DATE_BY_MODEL = min(target_end_date)),
  by = "model"
]$MIN_DATE_BY_MODEL)
historical_quantile_log_cases_components_dt <- historical_quantile_log_cases_components_dt[which(target_end_date > min_date), ]
historical_quantile_log_cases_components_dt[, model := gsub("historical_", "", model)]
# process_quantile_predictions(historical_quantile_log_cases_components_dt)
historical_quantile_log_cases_components_dt %>%
  score() %>%
  summarise_scores(by = "model")

# Form untrained ensembles ----
historical_quantile_log_cases_components_plus_ensembles_dt <-
  form_untrained_ensembles(historical_quantile_log_cases_components_dt)
historical_quantile_log_cases_components_plus_ensembles_dt[, length(prediction), by = "model"]
head(historical_quantile_log_cases_components_plus_ensembles_dt, 10)

# Form trained ensembles -----
log_info("Form trained ensembles")
quantile_log_cases_components_dt[, target_end_date := as.Date(target_end_date)]
historical_quantile_log_cases_components_dt[, target_end_date := as.Date(target_end_date)]

combined_quantile_log_cases_components_dt <- rbind(
  historical_quantile_log_cases_components_dt,
  quantile_log_cases_components_dt
)
input_quantile_dt <- copy(combined_quantile_log_cases_components_dt)
setkeyv(input_quantile_dt, c("location", "model", "target_end_date", "quantile"))
num_ensemble_components <- length(unique(input_quantile_dt$model))
num_quantile_levels <- length(quantiles)

input_quantile_dt[, target_end_date := as.Date(target_end_date)]
input_quantile_dt[, lagged_date := as.Date(Lag(target_end_date, 11)), by = c("location", "model", "quantile")]
input_quantile_dt[, lagged_date := as.Date(lagged_date)]
input_quantile_dt[, in_testing := ifelse(year(target_end_date) >= 2018, 1, 0)]
max_date <- max(input_quantile_dt[which(in_testing == 0)]$target_end_date)
# Include as we use realistic application of weights i.e. from historical performance (minimum 1 month lag)
input_quantile_dt[which(target_end_date == max_date), in_testing := 1]
dates <- (unique(input_quantile_dt[which(in_testing == 1)]$target_end_date))
number_dates <- length(unique(input_quantile_dt[which(in_testing == 1)]$target_end_date))
lagged_dates <- unique(input_quantile_dt[which(in_testing == 1)]$lagged_date)

true_log_cases_data <- copy(ptl_province_inla_df)
true_log_cases_data <- subset(true_log_cases_data, end_of_month >= max_date)
setkeyv(true_log_cases_data, c("PROVINCE", "end_of_month"))


iteratively_train_quantile_ensemble_runner <- function(quantiles,
                                                       dates,
                                                       lagged_dates,
                                                       number_dates,
                                                       input_quantile_dt,
                                                       true_data) {
  # ovr_num_pred_points = total number points (includes all quantile predictions)
  # input_quantile_dt is predictions for entire testing window (2018-2021)
  # historical_input_quantile_dt is historical predictions

  # prediction_indices <- sort(unique(input_quantile_dt$IND))

  model_names <- sort(unique(input_quantile_dt$model))
  ensemble_model_name <- paste(model_names, collapse = "_")
  # dates <- sort(unique(input_quantile_dt$target_end_date))
  num_ensemble_components <- length(unique(input_quantile_dt$model))
  num_quantile_levels <- length(quantiles)

  # data.tables to store results
  ovr_preds_dt <- NULL
  ovr_model_weights_dt <- NULL

  for (t in 1:number_dates) {
    log_info(paste0("iteratively_train_quantile_ensemble_runner: ", t, " / ", number_dates))
    new_date <- dates[t]
    lag_12_date <- lagged_dates[t]
    tmp_true_data <- subset(true_data, end_of_month <= new_date)
    tmp_true_data <- subset(tmp_true_data, end_of_month >= lag_12_date)
    setkeyv(tmp_true_data, c("PROVINCE", "end_of_month"))

    tmp_input_quantile_dt <- subset(input_quantile_dt, target_end_date <= new_date)
    historical_input_quantile_dt <- subset(tmp_input_quantile_dt, target_end_date >= lag_12_date)
    setkeyv(historical_input_quantile_dt, c(
      "location", "model",
      "target_end_date", "quantile"
    ))
    tmp_dates_dt <- unique(subset(historical_input_quantile_dt,
      select = c("target_end_date", "location")
    ))
    setkeyv(tmp_dates_dt, c("location", "target_end_date"))
    tmp_dates_dt[, IND := seq(1, nrow(tmp_dates_dt))]
    num_pred_points <- nrow(tmp_dates_dt)
    historical_input_quantile_dt <- merge(historical_input_quantile_dt,
      tmp_dates_dt,
      by = c("target_end_date", "location")
    )
    setkeyv(historical_input_quantile_dt, c(
      "location", "model",
      "target_end_date", "quantile"
    ))

    setkeyv(historical_input_quantile_dt, c(
      "location", "model",
      "target_end_date", "quantile"
    ))

    # num_pred_points = nrow()
    qarr <- array(NA, dim = c(
      num_pred_points, # 12*14
      num_ensemble_components, # 6
      num_quantile_levels
    )) # 23
    prediction_indices <- sort(unique(historical_input_quantile_dt$IND)) # Should always be 12 here
    for (i in 1:num_pred_points) {
      for (j in 1:num_ensemble_components) {
        qarr[i, j, ] <-
          historical_input_quantile_dt[which(IND == prediction_indices[i] &
            model == unique(model_names[j])), ]$prediction
      }
    }
    quantile_ensemble_weights <- quantile_ensemble(
      qarr,
      tmp_true_data[[metric_field]],
      quantiles
    )

    tmp_trained_quantile_ensemble <- copy(input_quantile_dt)
    model_weights_dt <- data.table(model = model_names, weights = quantile_ensemble_weights$alpha)
    tmp_trained_quantile_ensemble <-
      merge(tmp_trained_quantile_ensemble,
        model_weights_dt,
        by = "model"
      )
    model_weights_dt[, target_end_date := new_date]
    ovr_model_weights_dt <- rbind(
      ovr_model_weights_dt,
      model_weights_dt
    )
  }
  return(ovr_model_weights_dt)
}

# JSB - dont need this output!
# Present results for no lookback window in appendix
runner_trained_ensemble_weights_quantile_log_cases_dt <- iteratively_train_quantile_ensemble_runner(
  quantiles,
  dates,
  lagged_dates,
  number_dates,
  input_quantile_dt,
  true_log_cases_data
)

saveRDS(
    runner_trained_ensemble_weights_quantile_log_cases_dt,
    file = file.path(
        peru.province.out.dir,
        paste0("runner_trained_ensemble_weights_quantile_", metric_name, "_dt.RDS")
    )
)
runner_trained_ensemble_weights_quantile_log_cases_dt <- readRDS(file = file.path(peru.province.out.dir, paste0("runner_trained_ensemble_weights_quantile_", metric_name, "_dt.RDS")))

runner_trained_ensemble_weights_quantile_log_cases_dt[, model_factor := factor(model)]
runner_trained_ensemble_weights_quantile_log_cases_dt[, target_end_date := as.Date(target_end_date)]

tmp <- copy(runner_trained_ensemble_weights_quantile_log_cases_dt)
tmp[, target_end_date := Lag(target_end_date, -1), by = c("model")]
input_quantile_dt <- merge(input_quantile_dt,
  tmp,
  by = c("model", "target_end_date")
)
input_quantile_dt[, true_value := round(true_value, digits = 8)]
input_quantile_dt <- unique(input_quantile_dt)
trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt <-
  input_quantile_dt[, list(
    prediction = sum(weights * prediction),
    true_value = unique(true_value)
  ),
  by = c("location", "quantile", "target_end_date")
  ]
trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt[which(quantile == 0.5), caret::MAE(prediction, true_value)]
trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt[, model := "pinball_trained_ensemble"]

# JSB: Deal with small numerical imprecision...
# trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt$prediction <- log1p(exp(trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt$prediction) - 1)
trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt <- fix_quantile_violations(trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt)

# JSB: 'predictions must be increasing with quantiles'
trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt %>%
  score() %>%
  summarise_scores(by = c("model"))

# By province
log_info("By province")
provinces <- unique(combined_quantile_log_cases_components_dt$location)
runner_trained_ensemble_weights_by_province_quantile_log_cases_dt <- NULL
for (i in 1:length(provinces)) {
  log_info(paste0("By province ", i, " of ", length(provinces)))
  prov_in_q <- provinces[i]
  input_quantile_dt <- copy(combined_quantile_log_cases_components_dt)
  # num_pred_points <- nrow(ptl_province_2018_2021_data)
  setkeyv(input_quantile_dt, c("location", "model", "target_end_date", "quantile"))
  input_quantile_dt <- subset(input_quantile_dt, location == prov_in_q)
  num_ensemble_components <- length(unique(input_quantile_dt$model))
  num_quantile_levels <- length(quantiles)
  input_quantile_dt[, target_end_date := as.Date(target_end_date)]
  input_quantile_dt[, lagged_date := as.Date(Lag(target_end_date, 11)), by = c("location", "model", "quantile")]
  input_quantile_dt[, lagged_date := as.Date(lagged_date)]
  input_quantile_dt[, in_testing := ifelse(year(target_end_date) >= 2018, 1, 0)]
  max_date <- max(input_quantile_dt[which(in_testing == 0)]$target_end_date)
  # Include as we use realistic application of weights i.e. from historical performance (minimum 1 month lag)
  input_quantile_dt[which(target_end_date == max_date), in_testing := 2]
  dates <- (unique(input_quantile_dt[which(in_testing == 1)]$target_end_date))
  number_dates <- length(unique(input_quantile_dt[which(in_testing == 1)]$target_end_date))
  lagged_dates <- unique(input_quantile_dt[which(in_testing == 1)]$lagged_date)
  lagged_dates
  true_log_cases_data <- copy(ptl_province_inla_df)
  true_log_cases_data <- subset(true_log_cases_data, end_of_month >= max_date)
  true_log_cases_data <- subset(true_log_cases_data, PROVINCE == prov_in_q)
  setkeyv(true_log_cases_data, c("PROVINCE", "end_of_month"))


  tmp_runner_trained_ensemble_weights_quantile_log_cases_dt <- iteratively_train_quantile_ensemble_runner(
    quantiles,
    dates,
    lagged_dates,
    number_dates,
    input_quantile_dt,
    true_log_cases_data
  )
  tmp_runner_trained_ensemble_weights_quantile_log_cases_dt[, location := prov_in_q]
  runner_trained_ensemble_weights_by_province_quantile_log_cases_dt <-
    rbind(
      runner_trained_ensemble_weights_by_province_quantile_log_cases_dt,
      tmp_runner_trained_ensemble_weights_quantile_log_cases_dt
    )
}

saveRDS(runner_trained_ensemble_weights_by_province_quantile_log_cases_dt,
  file = file.path(peru.province.out.dir, paste0("runner_trained_ensemble_weights_by_province_quantile_", metric_name, "_dt.RDS"))
)

runner_trained_ensemble_weights_by_province_quantile_log_cases_dt[, model_factor := factor(model)]
runner_trained_ensemble_weights_by_province_quantile_log_cases_dt[, target_end_date := as.Date(target_end_date)]

summary_province_dependent_weights_by_province <-
  runner_trained_ensemble_weights_by_province_quantile_log_cases_dt[, list(
    MEDIAN = median(weights),
    MEAN = mean(weights)
  ),
  by = c("location", "model")
  ]


runner_trained_ensemble_weights_by_province_quantile_log_cases_dt
tmp <- copy(runner_trained_ensemble_weights_by_province_quantile_log_cases_dt)
tmp[, target_end_date := Lag(target_end_date, -1), by = c("model", "location")]
tmp
input_quantile_dt <- copy(combined_quantile_log_cases_components_dt)

input_quantile_dt <- merge(input_quantile_dt,
  tmp,
  by = c(
    "model", "target_end_date",
    "location"
  )
)
input_quantile_dt
input_quantile_dt[, true_value := round(true_value, digits = 8)]
input_quantile_dt
trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt <-
  input_quantile_dt[, list(
    prediction = sum(weights * prediction),
    true_value = unique(true_value)
  ),
  by = c("location", "quantile", "target_end_date")
  ]
trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt[which(quantile == 0.5), caret::MAE(prediction, true_value)]
trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt[, model := "pinball_trained_ensemble_by_province"]

trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt <- fix_quantile_violations(trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt)


# Merge in trained approaches ----
log_info("Merge trained approaches")
quantile_log_cases_components_plus_ensembles_dt <- rbind(
  quantile_log_cases_components_plus_ensembles_dt,
  trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt
)
quantile_log_cases_components_plus_ensembles_dt <- rbind(
  quantile_log_cases_components_plus_ensembles_dt,
  trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt
)

quantile_log_cases_components_plus_ensembles_dt[, length(prediction), by = "model"]





# FIX NAMES ----
original_model_names <- sort(unique(quantile_log_cases_components_plus_ensembles_dt$model))
new_model_names <- c(
  "Baseline", "Bayes-Climate", "EW-Mean *", "EW-Mean-NoBase *", "Ew-Mean-NoBayes *", "EW-Mean-NoCov *", "TimeGPT",
  "TimeGPT-NoCov ", "Median *", "Median-NoBase *", "Median-NoBayes *",
  "Median-NoCov *", "Trained *", "Prov-Trained *", "SARIMA", "TCN"
)
historical_original_model_names <- sort(unique(historical_quantile_log_cases_components_plus_ensembles_dt$model))
historical_new_model_names <- c(
  "Baseline", "Bayes-Climate", "EW-Mean *", "EW-Mean-NoBase *", "EW-Mean-NoBayes *", "EW-Mean-NoCov *", "TimeGPT",
  "TimeGPT-NoCov ", "Median *", "Median-NoBase *", "Median-NoBayes *",
  "Median-NoCov *", "SARIMA", "TCN"
)
quantile_log_cases_components_plus_ensembles_dt[, model := factor(model,
  labels = new_model_names
)]

historical_quantile_log_cases_components_plus_ensembles_dt[, model := factor(model,
  labels = historical_new_model_names
)]
historical_quantile_log_cases_components_plus_ensembles_dt
setkey(quantile_log_cases_components_plus_ensembles_dt, "model")
setkey(historical_quantile_log_cases_components_plus_ensembles_dt, "model")

# Write model predictions (quantiles) out
for (name in historical_new_model_names) {
    model_name <- str_trim(name)
    model_name <- str_replace_all(model_name, "-", "_")
    model_name <- str_replace_all(model_name, " ", "_")
    model_name <- str_replace_all(model_name, fixed("*"), fixed("star"))
    model_name <- paste0('Ensemble_', model_name)
    print(model_name)

    # Forecasting
    tmp <- copy(quantile_log_cases_components_plus_ensembles_dt)
    tmp <- tmp[tmp$model == name]
    dir.create(file.path(peru.province.predictions.out.dir,
        model_name), recursive = TRUE, showWarnings = FALSE)
    write.csv(tmp,
        file.path(peru.province.predictions.out.dir, model_name, paste0("pred_", metric_name, "_quantiles_forecasting.csv")),
        row.names=FALSE)
    
    # Historical
    tmp <- copy(historical_quantile_log_cases_components_plus_ensembles_dt)
    tmp <- tmp[tmp$model == name]
    dir.create(file.path(peru.province.predictions.out.dir,
        model_name), recursive = TRUE, showWarnings = FALSE)
    write.csv(tmp,
        file.path(peru.province.predictions.out.dir, model_name, paste0("pred_", metric_name, "_quantiles_historical.csv")),
        row.names=FALSE)
}

# INSERT LATITUDE AND LONGITUDE INDICATORS ----

latitude_monthly_dt <- copy(ptl_province_inla_df)
setkeyv(latitude_monthly_dt, c("latitude", "longitude", "TIME"))
tmp <- unique(subset(latitude_monthly_dt, select = c("PROVINCE")))
tmp[, LAT_PROV_IND:= seq(1, nrow(tmp), by = 1)]
tmp[, LONG_PROV_IND:= seq(1, nrow(tmp), by = 1)]
latitude_monthly_dt <- merge(latitude_monthly_dt, tmp, by = "PROVINCE")
setkeyv(latitude_monthly_dt, c("latitude", "longitude", "TIME"))
latitude_monthly_dt[, SCALED_DIR:= scale(DIR), by = "PROVINCE"]

ptl_province_inla_df[, YEAR_DECIMAL:= YEAR + (MONTH - 1)/12]

ptl_province_inla_df <- merge(
  ptl_province_inla_df, 
  unique(
    subset(
      latitude_monthly_dt,
      select = c("LAT_PROV_IND", "PROVINCE")
    )
  ),
  by = c("PROVINCE")
)
ptl_province_inla_df <- merge(
  ptl_province_inla_df, 
  unique(
    subset(
      latitude_monthly_dt,
      select = c("LONG_PROV_IND", "PROVINCE")
    )
  ),
  by = c("PROVINCE")
)

tmp <- unique(subset(ptl_province_inla_df,
  select = c("PROVINCE", "LAT_PROV_IND", "LONG_PROV_IND")
))
historical_quantile_log_cases_components_plus_ensembles_dt <-
  merge(historical_quantile_log_cases_components_plus_ensembles_dt,
    tmp,
    by.y = "PROVINCE", by.x = "location"
  )
quantile_log_cases_components_plus_ensembles_dt <-
  merge(quantile_log_cases_components_plus_ensembles_dt,
    tmp,
    by.y = "PROVINCE", by.x = "location"
  )

log_info(paste0("Finished province_log_cases.R for metric: ", metric_name))

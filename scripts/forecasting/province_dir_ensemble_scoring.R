# Script for DIR scoring + outbreak assessment
source("scripts/processing/packages_directories.R")
# library(quantgen)
library(logger)


ptl_province_inla_df <- data.table(read.csv(file.path(
  peru.province.python.data.dir,
  "ptl_province_inla_df.csv"
)))
ptl_province_2018_2021_data <- subset(
  ptl_province_inla_df,
  YEAR >= 2018
)
ptl_province_2018_2021_data[, IND := seq(1, length(DIR)), by = "PROVINCE"]
# colnames(quantile_tcn_preds_dt)  # JSB
scoring_columns <- c(
  "location", "true_value", "model", "target_end_date",
  "quantile", "prediction"
)


# Component models

# Time-GPT
quantile_fine_tuned_timegpt_dir_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_fine_tuned_timegpt_dir_preds_dt.RDS"
))
quantile_fine_tuned_timegpt_dir_preds_dt <- subset(quantile_fine_tuned_timegpt_dir_preds_dt,
  select = scoring_columns
)

quantile_fine_tuned_no_covars_timegpt_dir_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_fine_tuned_no_covars_timegpt_dir_preds_dt.RDS"
))
quantile_fine_tuned_no_covars_timegpt_dir_preds_dt <- subset(quantile_fine_tuned_no_covars_timegpt_dir_preds_dt,
  select = scoring_columns
)
quantile_fine_tuned_no_covars_timegpt_dir_preds_dt
# TCN
quantile_tcn_dir_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_tcn_dir_preds_dt.RDS"
))
quantile_tcn_dir_preds_dt <- subset(quantile_tcn_dir_preds_dt,
  select = scoring_columns
)

# Climate
climate_2018_2021_forecast_quantile_dt <- readRDS(file = file.path(
  peru.province.inla.data.out.dir,
  paste0("climate_2018_2021_forecast_quantile_dt.RDS")
))
climate_2018_2021_forecast_quantile_dt
climate_2018_2021_forecast_quantile_dt <- merge(climate_2018_2021_forecast_quantile_dt,
  subset(ptl_province_2018_2021_data, select = c("TIME", "PROVINCE", "end_of_month")),
  by = c("TIME", "PROVINCE")
)
climate_2018_2021_forecast_quantile_dt
setnames(climate_2018_2021_forecast_quantile_dt, c("PROVINCE", "end_of_month"), c("location", "target_end_date"))
climate_2018_2021_forecast_quantile_dt <- subset(climate_2018_2021_forecast_quantile_dt,
  select = scoring_columns
)

# SARIMA Model Forecasts ----
sarima_2010_2018_forecast_quantile_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  # paste0("sarima_2010_2018_forecast_quantile_dt.RDS")  # JSB
  paste0("quantile_historical_sarima_preds_dt.RDS")
))
sarima_2018_2021_forecast_quantile_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  # paste0("sarima_2018_2021_forecast_quantile_dt.RDS")  # JSB
  paste0("quantile_sarima_preds_dt.RDS")  # JSB
))
sarima_2018_2021_forecast_quantile_dt <- merge(sarima_2018_2021_forecast_quantile_dt,
  subset(ptl_province_2018_2021_data, select = c("TIME", "PROVINCE", "end_of_month")),
  by = c("TIME", "PROVINCE")
)
# print(sarima_2018_2021_forecast_quantile_dt)
setnames(sarima_2018_2021_forecast_quantile_dt, c("PROVINCE", "end_of_month"), c("location", "target_end_date"))
sarima_2018_2021_forecast_quantile_dt <- subset(sarima_2018_2021_forecast_quantile_dt,
  select = scoring_columns
)

quantile_sarima_dir_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_sarima_dir_preds_dt.RDS"
))

# Random Forest Model Forecasts (should use predict quantiles)----
quantiles_random_forest_dir.pred_2018_2021 <-
  readRDS(file = file.path(
    peru.province.xgb.out.dir,
    paste0("quantiles_random_forest_dir.pred_2018_2021.RDS")
  ))

quantiles_random_forest_dir.pred_2018_2021 <- merge(quantiles_random_forest_dir.pred_2018_2021,
  subset(ptl_province_2018_2021_data, select = c("TIME", "PROVINCE", "end_of_month")),
  by = c("TIME", "PROVINCE")
)
quantiles_random_forest_dir.pred_2018_2021
setnames(quantiles_random_forest_dir.pred_2018_2021, c("PROVINCE", "end_of_month"), c("location", "target_end_date"))
quantiles_random_forest_dir.pred_2018_2021 <- subset(quantiles_random_forest_dir.pred_2018_2021,
  select = scoring_columns
)

# Baseline
quantile_baseline_dir.pred_dt_2018_2021 <-
  readRDS(file = file.path(
    peru.province.out.dir,
    paste0("quantile_baseline_dir.pred_dt_2018_2021.RDS")
  ))

quantile_dir_components_dt <- rbind(
  quantile_fine_tuned_timegpt_dir_preds_dt,
  quantile_fine_tuned_no_covars_timegpt_dir_preds_dt,
  quantile_tcn_dir_preds_dt,
  climate_2018_2021_forecast_quantile_dt,
  quantile_sarima_dir_preds_dt,
  quantile_baseline_dir.pred_dt_2018_2021
)
quantile_dir_components_dt[, true_value := round(true_value, 8)]
quantile_dir_components_dt[, length(unique(true_value)), by = "model"]
quantile_dir_components_dt[, target_end_date := as.Date(target_end_date)]
quantile_dir_components_dt[which(prediction < 0), prediction := 0]
quantile_dir_components_dt %>%
  score() %>%
  summarise_scores(by = c("model"))


process_quantile_predictions(quantile_dir_components_dt)


# Untrained ensembles
quantile_dir_components_plus_ensembles_dt <-
  form_untrained_ensembles(quantile_dir_components_dt)
quantile_dir_components_plus_ensembles_dt[, length(prediction), by = "model"]
quantile_dir_components_plus_ensembles_dt_scores <- quantile_dir_components_plus_ensembles_dt %>%
  score() %>%
  summarise_scores(by = c("model"))
quantile_dir_components_plus_ensembles_dt_scores[order(interval_score)]
quantile_log_cases_components_plus_ensembles_dt_scores[order(interval_score)]
tmp <- process_summary_predictions(summary_predictions(quantile_dir_components_plus_ensembles_dt))
tmp[order(COVERAGE)]
quantile_dir_components_plus_ensembles_dt_performance <-
  process_quantile_predictions(quantile_dir_components_plus_ensembles_dt)
quantile_dir_components_plus_ensembles_dt_performance[order(r2, decreasing = TRUE)]
quantile_dir_components_plus_ensembles_dt %>%
  score() %>%
  summarise_scores(by = c("model", "quantile")) %>%
  plot_quantile_coverage() + geom_line(aes(y = quantile_coverage), linewidth = 1.5, alpha = 0.6) +
  facet_wrap(model ~ ., ) +
  theme(
    text = element_text(size = 32),
    axis.text.x = element_text(size = 32),
    axis.text.y = element_text(size = 32),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_blank(),
    axis.title = element_text(size = 32),
    legend.text = element_text(size = 24),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend("Model"))


# Trained Ensembles ----
# All iteratively trained -> Reflective of real-world conditions
# Trained Ensemble 1) (by quantile loss) ----
num_pred_points <- nrow(ptl_province_2018_2021_data)
input_quantile_dt <- copy(quantile_dir_components_dt)
setkeyv(input_quantile_dt, c("location", "model", "target_end_date", "quantile"))
num_ensemble_components <- length(unique(input_quantile_dt$model))
num_quantile_levels <- length(quantiles)
pred_points_dt <- unique(subset(input_quantile_dt,
  select = c("location", "model", "target_end_date")
))
setkeyv(pred_points_dt, c("model", "location", "target_end_date"))
pred_points_dt[, IND := seq(1, length(target_end_date)), by = "model"]
pred_points_dt
input_quantile_dt <- merge(input_quantile_dt, pred_points_dt, by = c("model", "location", "target_end_date"))
setkeyv(input_quantile_dt, c("location", "model", "target_end_date", "quantile"))
# prediction_indices <- sort(unique(pred_points_dt$IND))
# prediction_indices
# model_indices <- sort(unique(pred_points_dt$model))
# model_indices
# number_testing_dates = length(unique(pred_points_dt$target_end_date))


# Train ensemble using quantile loss with iterative updating to reflect
# real-world conditions of information becoming iteratively available
iteratively_train_quantile_ensemble <- function(quantiles,
                                                number_testing_dates,
                                                input_quantile_dt,
                                                pred_points_dt,
                                                historical_input_quantile_dt,
                                                true_data) {
  # ovr_num_pred_points = total number points (includes all quantile predictions)
  # input_quantile_dt is predictions for entire testing window (2018-2021)
  # historical_input_quantile_dt is historical predictions

  # prediction_indices <- sort(unique(input_quantile_dt$IND))

  model_names <- sort(unique(input_quantile_dt$model))
  print(model_names)
  ensemble_model_name <- paste(model_names, collapse = "_")
  testing_dates <- sort(unique(input_quantile_dt$target_end_date))
  num_ensemble_components <- length(unique(input_quantile_dt$model))
  num_quantile_levels <- length(quantiles)

  # data.tables to store results
  ovr_preds_dt <- NULL
  ovr_model_weights_dt <- NULL

  for (t in 1:number_testing_dates) {
    new_date <- testing_dates[t]
    # number of historical pred points
    tmp_true_data <- subset(true_data, end_of_month <= new_date)
    testing_input_quantile_dt <- subset(input_quantile_dt, target_end_date == new_date)
    print(testing_input_quantile_dt)
    historical_input_quantile_dt <- rbind(
      historical_input_quantile_dt,
      testing_input_quantile_dt
    )
    num_pred_points <- nrow(historical_input_quantile_dt) / num_quantile_levels
    num_pred_points <- num_pred_points / num_ensemble_components
    setkeyv(historical_input_quantile_dt, c(
      "location", "model",
      "target_end_date", "quantile"
    ))

    # num_pred_points = nrow()
    qarr <- array(NA, dim = c(
      num_pred_points,
      num_ensemble_components,
      num_quantile_levels
    ))
    print(dim(qarr))
    # print(dim(qarr))
    prediction_indices <- sort(unique(historical_input_quantile_dt$IND))
    for (i in 1:num_pred_points) {
      # print(paste0("i: ", i))
      for (j in 1:num_ensemble_components) {
        # print(paste0("j: ", j))
        # print(prediction_indices[i])
        # print(unique(model_names[j]))
        # print(historical_input_quantile_dt[which(IND == prediction_indices[i] &
        #                                            model== unique(model_names[j])), ])
        qarr[i, j, ] <-
          historical_input_quantile_dt[which(IND == prediction_indices[i] &
            model == unique(model_names[j])), ]$prediction
      }
    }
    quantile_ensemble_weights <- quantile_ensemble(
      qarr,
      tmp_true_data$DIR,
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
    # tmp_trained_quantile_ensemble <- tmp_trained_quantile_ensemble[, list(prediction = sum(weights*prediction)),
    #                                                                                by = c("location", "true_value",
    #                                                                                       "target_end_date", "quantile")]
    #
    # tmp_trained_quantile_ensemble[, model:= paste("trained_quantile", ensemble_model_name)]
    # ovr_preds_dt <- tmp_trained_quantile_ensemble
  }
  return(ovr_model_weights_dt)
}

iteratively_trained_weights_dir_dt <- iteratively_train_quantile_ensemble(
  quantiles = quantiles,
  number_testing_dates = number_testing_dates,
  input_quantile_dt = input_quantile_dt,
  pred_points_dt,
  historical_input_quantile_dt = NULL,
  ptl_province_2018_2021_data
)
saveRDS(iteratively_trained_weights_dir_dt,
  file = file.path(peru.province.out.dir, "iteratively_trained_weights_dir_dt.RDS")
)
iteratively_trained_weights_dir_dt <- readRDS(file = file.path(peru.province.out.dir, "iteratively_trained_weights_dir_dt.RDS"))
iteratively_trained_weights_dir_dt


iteratively_trained_weights_dir_dt[, model_factor := factor(model)]
iteratively_trained_weights_dir_dt[, target_end_date := as.Date(target_end_date)]
iteratively_trained_weights_dir_dt[, mean(weights), by = "model"]
ggplot(iteratively_trained_weights_dir_dt) +
  geom_line(aes(x = target_end_date, y = weights, color = model_factor)) +
  theme_bw() +
  theme(legend.position = "bottom")
input_quantile_dt[, target_end_date := as.Date(target_end_date)]
input_quantile_dt


weighted_quantile_dir_dt <- merge(iteratively_trained_weights_dir_dt,
  unique(subset(input_quantile_dt,
    select = c(
      "IND", "model", "target_end_date", "location", "true_value",
      "prediction", "quantile"
    )
  )),
  by = c("model", "target_end_date")
)
setkeyv(weighted_quantile_dir_dt, c("location", "IND"))
weighted_quantile_dir_dt[, true_value := round(true_value, digits = 8)]
weighted_quantile_dir_dt <- unique(weighted_quantile_dir_dt)
weighted_quantile_dir_dt[, TRAINED_WEIGHT := Lag(weights, 1), by = c("location", "model", "quantile")]
weighted_quantile_dir_dt[which(target_end_date == min(target_end_date)), TRAINED_WEIGHT := 1 / length(unique(model))]
weighted_quantile_dir_dt
weighted_quantile_dir_forecasts_2018_2021_dt <-
  weighted_quantile_dir_dt[, list(
    prediction = sum(TRAINED_WEIGHT * prediction),
    true_value = unique(true_value)
  ),
  by = c("location", "quantile", "target_end_date")
  ]
weighted_quantile_dir_forecasts_2018_2021_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
weighted_quantile_dir_forecasts_2018_2021_dt[which(quantile == 0.5), caret::MAE(prediction, true_value)]
weighted_quantile_dir_forecasts_2018_2021_dt[, model := "pinball_trained_ensemble"]
weighted_quantile_dir_forecasts_2018_2021_dt %>%
  score() %>%
  summarise_scores(by = c("model"))
process_quantile_predictions(weighted_quantile_dir_forecasts_2018_2021_dt)
# Ensembles - iteratively trained, province-level trained, no covars (Mean), no covars (median), all median
# Covariate-dependent



# WIS Assessment over time ----
wis_expanding_dir_results <- wis_expanding_window_over_time(quantile_dir_components_dt,
  times = unique(quantile_dir_components_dt$target_end_date)
)

wis_expanding_dir_results_spatially_homoegenous <- wis_expanding_dir_results[[1]]
wis_expanding_dir_results_space_dependent <- wis_expanding_dir_results[[2]]
wis_expanding_dir_results_space_dependent
# 1) Iterative WIS-based weighting (Spatially homogeneous)
# Add equal weight for first month
tmp <- subset(
  wis_expanding_dir_results_spatially_homoegenous,
  effective_time == min(effective_time)
)
tmp[, weight := 1 / length(unique(model))]
tmp[, effective_time := min(quantile_log_cases_components_dt$target_end_date)]
tmp
wis_expanding_dir_results_spatially_homoegenous <- rbind(tmp, wis_expanding_dir_results_spatially_homoegenous)
wis_expanding_dir_results_spatially_homoegenous[, model_factor := factor(model)]
# wis_expanding_dir_results_spatially_homoegenous[, effective_time:= as.Date(effective_time)]
wis_expanding_dir_results_spatially_homoegenous[, target_end_date := as.Date(effective_time)]
wis_expanding_dir_results_spatially_homoegenous$weight
ggplot(wis_expanding_dir_results_spatially_homoegenous) +
  geom_line(aes(x = target_end_date, y = interval_score, color = model_factor)) +
  theme_bw() +
  theme(legend.position = "bottom")





ggplot(
  wis_expanding_dir_results_space_dependent,
  aes(x = effective_time, y = interval_score, color = model)
) +
  geom_line() +
  theme_bw() +
  facet_wrap(location ~ ., scales = "free_y")




# Historical ----
ptl_province_2010_2018_data <- subset(
  ptl_province_inla_df,
  YEAR < 2018
)
ptl_province_2010_2018_data[, IND := seq(1, length(DIR)), by = "PROVINCE"]

# Need historical predictions

# TimeGPT
quantile_historical_fine_tuned_timegpt_dir_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_historical_fine_tuned_timegpt_dir_preds_dt.RDS"
))
quantile_historical_fine_tuned_timegpt_dir_preds_dt <-
  subset(quantile_historical_fine_tuned_timegpt_dir_preds_dt,
    select = scoring_columns
  )
quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt.RDS"
))
quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt <-
  subset(quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt,
    select = scoring_columns
  )
quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt[, model := "finetuned_no_covars_timegpt"]
# Climate
climate_2010_2018_forecast_quantile_dt <- readRDS(file = file.path(
  peru.province.inla.data.out.dir,
  paste0("climate_2010_2018_forecast_quantile_dt.RDS")
))
climate_2010_2018_forecast_quantile_dt <- merge(climate_2010_2018_forecast_quantile_dt,
  subset(ptl_province_2010_2018_data, select = c("TIME", "PROVINCE", "end_of_month")),
  by = c("TIME", "PROVINCE")
)
setnames(climate_2010_2018_forecast_quantile_dt, c("PROVINCE", "end_of_month"), c("location", "target_end_date"))
climate_2010_2018_forecast_quantile_dt <- subset(climate_2010_2018_forecast_quantile_dt,
  select = scoring_columns
)

# TCN
quantile_historical_tcn_dir_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_historical_tcn_dir_preds_dt.RDS"
))
setnames(quantile_historical_tcn_dir_preds_dt, "end_of_month", "target_end_date")
# Random forest
quantiles_random_forest_dir.pred_2010_2018 <-
  readRDS(file = file.path(
    peru.province.xgb.out.dir,
    paste0("quantiles_random_forest_dir.pred_2010_2018.RDS")
  ))
quantiles_random_forest_dir.pred_2010_2018 <- merge(quantiles_random_forest_dir.pred_2010_2018,
  subset(ptl_province_2010_2018_data, select = c("TIME", "PROVINCE", "end_of_month")),
  by = c("TIME", "PROVINCE")
)
quantiles_random_forest_dir.pred_2010_2018

setnames(quantiles_random_forest_dir.pred_2010_2018, c("PROVINCE", "end_of_month"), c("location", "target_end_date"))
quantiles_random_forest_dir.pred_2010_2018 <- subset(quantiles_random_forest_dir.pred_2010_2018,
  select = scoring_columns
)


# SARIMA
quantile_historical_sarima_dir_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_historical_sarima_dir_preds_dt.RDS"
))
quantile_historical_sarima_dir_preds_dt
setnames(quantile_historical_sarima_dir_preds_dt, "end_of_month", "target_end_date")

# Baseline

quantile_baseline_dir.pred_dt_2010_2018 <- readRDS(file = file.path(
  peru.province.out.dir,
  "quantile_baseline_dir.pred_dt_2010_2018.RDS"
))
quantile_baseline_dir.pred_dt_2010_2018

historical_quantile_dir_components_dt <- rbind(
  quantile_historical_fine_tuned_timegpt_dir_preds_dt,
  quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt,
  quantile_historical_tcn_dir_preds_dt,
  climate_2010_2018_forecast_quantile_dt,
  quantile_historical_sarima_dir_preds_dt,
  quantile_baseline_dir.pred_dt_2010_2018
)
historical_quantile_dir_components_dt[, length(which(is.na(prediction))), by = "model"]
historical_quantile_dir_components_dt[which(prediction < 0), prediction := 0]
find_min_date_no_nas_after <- function(models_dt) {
  tmp <- models_dt[which(is.na(prediction)), ]
  tmp[, target_end_date]
  max_date <- max(tmp$target_end_date)
  return(max_date)
}
min_date <- find_min_date_no_nas_after(historical_quantile_dir_components_dt)
min_date <- max(historical_quantile_dir_components_dt[, list(MIN_DATE_BY_MODEL = min(target_end_date)),
  by = "model"
]$MIN_DATE_BY_MODEL)
min_date
historical_quantile_dir_components_dt <- historical_quantile_dir_components_dt[which(target_end_date > min_date), ]
historical_quantile_dir_components_dt %>%
  score() %>%
  summarise_scores(by = "model")
historical_quantile_dir_components_dt[, model := gsub("historical_", "", model)]

head(historical_quantile_dir_components_dt)

# Untrained ensembles
historical_quantile_dir_components_plus_ensembles_dt <-
  form_untrained_ensembles(historical_quantile_dir_components_dt)
historical_quantile_dir_components_plus_ensembles_dt[, length(prediction), by = "model"]
historical_quantile_dir_components_plus_ensembles_dt_scores <- historical_quantile_dir_components_plus_ensembles_dt %>%
  score() %>%
  summarise_scores(by = c("model"))
historical_quantile_dir_components_plus_ensembles_dt_scores[order(interval_score)]
historical_quantile_dir_components_plus_ensembles_dt_performance <-
  process_quantile_predictions(historical_quantile_dir_components_plus_ensembles_dt)
historical_quantile_dir_components_plus_ensembles_dt_performance[order(r2, decreasing = TRUE)]
# EW-Mean

# Median

# No covars (Mean)

# Climate+SARIMA+


# Trained Approaches ----
runner_trained_ensemble_weights_by_province_quantile_log_cases_dt
tmp <- copy(runner_trained_ensemble_weights_by_province_quantile_log_cases_dt)
tmp[, target_end_date := Lag(target_end_date, -1), by = c("model", "location")]
tmp
quantile_dir_components_dt
input_quantile_dt <- merge(quantile_dir_components_dt,
  tmp,
  by = c("model", "location", "target_end_date")
)
input_quantile_dt
input_quantile_dt[, true_value := round(true_value, digits = 8)]
input_quantile_dt
trained_ensemble_by_province_quantile_dir_forecasts_2018_2021_dt <-
  input_quantile_dt[, list(
    prediction = sum(weights * prediction),
    true_value = unique(true_value)
  ),
  by = c("location", "quantile", "target_end_date")
  ]
trained_ensemble_by_province_quantile_dir_forecasts_2018_2021_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
trained_ensemble_by_province_quantile_dir_forecasts_2018_2021_dt[which(quantile == 0.5), caret::MAE(prediction, true_value)]
trained_ensemble_by_province_quantile_dir_forecasts_2018_2021_dt[, model := "pinball_trained_ensemble_by_province"]



tmp <- copy(runner_trained_ensemble_weights_quantile_log_cases_dt)
tmp[, target_end_date := Lag(target_end_date, -1), by = c("model")]
tmp
quantile_dir_components_dt
input_quantile_dt <- merge(quantile_dir_components_dt,
  tmp,
  by = c("model", "target_end_date")
)
input_quantile_dt
input_quantile_dt[, true_value := round(true_value, digits = 8)]
input_quantile_dt
trained_ensemble_quantile_dir_forecasts_2018_2021_dt <-
  input_quantile_dt[, list(
    prediction = sum(weights * prediction),
    true_value = unique(true_value)
  ),
  by = c("location", "quantile", "target_end_date")
  ]
trained_ensemble_quantile_dir_forecasts_2018_2021_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
trained_ensemble_quantile_dir_forecasts_2018_2021_dt[which(quantile == 0.5), caret::MAE(prediction, true_value)]
trained_ensemble_quantile_dir_forecasts_2018_2021_dt[, model := "pinball_trained_ensemble"]


trained_ensemble_quantile_dir_forecasts_2018_2021_dt[, target_end_date := as.Date(target_end_date)]
trained_ensemble_by_province_quantile_dir_forecasts_2018_2021_dt[, target_end_date := as.Date(target_end_date)]
quantile_dir_components_plus_ensembles_dt[, target_end_date := as.Date(target_end_date)]
historical_quantile_dir_components_plus_ensembles_dt[, target_end_date := as.Date(target_end_date)]


quantile_dir_components_plus_ensembles_dt <- rbind(
  quantile_dir_components_plus_ensembles_dt,
  trained_ensemble_by_province_quantile_dir_forecasts_2018_2021_dt,
  trained_ensemble_quantile_dir_forecasts_2018_2021_dt
)











# Outbreak Detection ----
model_nums_dt <- data.table(
  model = unique(quantile_dir_components_plus_ensembles_dt$model),
  model_num = seq(1, length(unique(quantile_dir_components_plus_ensembles_dt$model)))
)
quantile_dir_components_plus_ensembles_dt[, quantile := round(as.numeric(quantile), 3)]
historical_quantile_dir_components_plus_ensembles_dt[, quantile := round(as.numeric(quantile), 3)]
quantile_dir_components_plus_ensembles_dt[, target_end_date := as.Date(target_end_date)]
historical_quantile_dir_components_plus_ensembles_dt[, target_end_date := as.Date(target_end_date)]

# historical_quantile_dir_components_plus_ensembles_dt.split <- dlply(historical_quantile_dir_components_plus_ensembles_dt, .(model))
# models_interval_2010_2018_thresholds_50 <- rep(NA, length(historical_quantile_dir_components_plus_ensembles_dt.split))
# for(i in 1:length(historical_quantile_dir_components_plus_ensembles_dt.split)){
#   print(i)
#   tmp <- quantile_threshold_50_function(data = data.table(historical_quantile_dir_components_plus_ensembles_dt.split[[i]]))
#   tmp_2010_2018_chosen_threshold_50 <-
#     head(tmp[order(auc, decreasing = TRUE)]$cut_off, 1)  #Chosen threshold from 2010-2017
#   models_interval_2010_2018_thresholds_50[i] <- tmp_2010_2018_chosen_threshold_50
# }
#
#
# historical_quantile_dir_components_plus_ensembles_dt.split <- dlply(historical_quantile_dir_components_plus_ensembles_dt, .(model))
# models_interval_2010_2018_thresholds_150 <- rep(NA, length(historical_quantile_dir_components_plus_ensembles_dt.split))
# for(i in 1:length(historical_quantile_dir_components_plus_ensembles_dt.split)){
#   print(i)
#   tmp <- quantile_threshold_150_function(data = data.table(historical_quantile_dir_components_plus_ensembles_dt.split[[i]]))
#   tmp_2010_2018_chosen_threshold_150 <-
#     head(tmp[order(auc, decreasing = TRUE)]$cut_off, 1)  #Chosen threshold from 2010-2017
#   models_interval_2010_2018_thresholds_150[i] <- tmp_2010_2018_chosen_threshold_150
# }
#
#
#
# #Historical results
# interval_outbreak_50_results <- NULL
# interval_outbreak_150_results <- NULL
# for(i in 1:length(quantile_dir_components_plus_ensembles_dt.split)){
#   tmp <- subset(data.table(quantile_dir_components_plus_ensembles_dt.split[[i]]),
#                 quantile == models_interval_2010_2018_thresholds_150[i])
#
#   tmp <- unique(tmp)
#   tmp
#   print("Here 1:")
#
#   tmp_interval_outbreak_150_results <-
#     quantile_threshold_150_function(tmp)
#   tmp_interval_outbreak_150_results[, model:= model_nums_dt$model[i]]
#   interval_outbreak_150_results <- rbind(interval_outbreak_150_results,
#                                          tmp_interval_outbreak_150_results)
#   tmp <- subset(data.table(quantile_dir_components_plus_ensembles_dt.split[[i]]),
#                 quantile == models_interval_2010_2018_thresholds_50[i])
#   tmp <- unique(tmp)
#   print("Here 2:")
#   tmp_interval_outbreak_50_results <-
#     quantile_threshold_50_function(tmp)
#   tmp_interval_outbreak_50_results[, model:= model_nums_dt$model[i]]
#
#   interval_outbreak_50_results <- rbind(interval_outbreak_50_results,
#                                         tmp_interval_outbreak_50_results)
#
# }
# interval_outbreak_50_results <- unique(interval_outbreak_50_results)
# interval_outbreak_50_results[order(auc, decreasing = TRUE)]
# interval_outbreak_150_results <- unique(interval_outbreak_150_results)
# interval_outbreak_150_results[order(auc, decreasing = TRUE)]
#
# # saveRDS(interval_outbreak_50_results,
# #         file = file.path(peru.province.ensemble.out.dir,
# #                          "interval_outbreak_50_results.RDS"))
# # saveRDS(interval_outbreak_150_results,
# #         file = file.path(peru.province.ensemble.out.dir,
# #                          "interval_outbreak_150_results.RDS"))
# #
# # interval_outbreak_50_results <- readRDS(file = file.path(peru.province.ensemble.out.dir,
# #                                                          "interval_outbreak_50_results.RDS"))
# # interval_outbreak_50_results[order(auc, decreasing = TRUE)]
# # interval_outbreak_150_results <- readRDS(file = file.path(peru.province.ensemble.out.dir,
# #                                                           "interval_outbreak_150_results.RDS"))
# gc()


# For historical, treat trained models' posterior probabilities as akin to equal-weighted
tmp <- subset(
  historical_quantile_dir_components_plus_ensembles_dt,
  model == "EW-Mean *"
)
tmp[, model := "Prov-Trained *"]
tmp2 <- subset(
  historical_quantile_dir_components_plus_ensembles_dt,
  model == "EW-Mean *"
)
tmp2[, model := "Trained *"]
tmp
tmp2
tmp <- rbind(
  historical_quantile_dir_components_plus_ensembles_dt,
  tmp,
  tmp2
)
all_interval_estimates_2010_2021 <- rbind(tmp, quantile_dir_components_plus_ensembles_dt)
all_interval_estimates_2010_2021.split <- dlply(all_interval_estimates_2010_2021, .(model))
all_interval_estimates_2010_2021
all_interval_estimates_2010_2021[, length(true_value), by = "model"]

times_in_2018_2021 <- unique(ptl_province_inla_df[which(YEAR >= 2018), ]$end_of_month)
quantile_dir_components_plus_ensembles_dt.split <- dlply(quantile_dir_components_plus_ensembles_dt, .(model))

all_interval_estimates_2010_2021_with_150_thresholds <- NULL

for (i in 1:length(all_interval_estimates_2010_2021.split)) {
  model_df <- data.table(all_interval_estimates_2010_2021.split[[i]])
  model_df[, quantile := as.numeric(round(quantile, 3))]
  for (j in 1:length(times_in_2018_2021)) {
    time_in_q <- times_in_2018_2021[j]
    training_model_df <- copy(model_df)
    training_model_df <- subset(training_model_df, target_end_date < time_in_q)
    tmp <- quantile_threshold_150_function(data = training_model_df)
    chosen_threshold_150 <- head(tmp[order(auc, decreasing = TRUE)]$cut_off, 1)
    model_df[which(target_end_date == time_in_q), OUTBREAK_150_THRESHOLD := chosen_threshold_150]
  }
  all_interval_estimates_2010_2021_with_150_thresholds <-
    rbind(
      all_interval_estimates_2010_2021_with_150_thresholds,
      model_df
    )
  if (i %% 2 == 0) {
    gc()
  }
}


moving_outbreak_150_quantile_2018_2021 <- NULL
all_interval_estimates_2010_2021_with_150_thresholds[, YEAR := year(target_end_date)]
all_interval_estimates_2018_2021_with_150_thresholds <-
  subset(
    all_interval_estimates_2010_2021_with_150_thresholds,
    YEAR >= 2018
  )

all_interval_estimates_2018_2021_with_150_thresholds.split <-
  dlply(all_interval_estimates_2018_2021_with_150_thresholds, .(model))
for (i in 1:length(quantile_dir_components_plus_ensembles_dt.split)) {
  tmp <- subset(
    data.table(all_interval_estimates_2018_2021_with_150_thresholds.split[[i]]),
    quantile == OUTBREAK_150_THRESHOLD
  )
  tmp <- unique(tmp)
  tmp_interval_outbreak_150_results <-
    quantile_threshold_150_function(tmp)
  tmp_interval_outbreak_150_results[, model := model_nums_dt$model[i]]
  moving_outbreak_150_quantile_2018_2021 <-
    rbind(
      moving_outbreak_150_quantile_2018_2021,
      tmp_interval_outbreak_150_results
    )
}

moving_outbreak_150_quantile_2018_2021 <- unique(moving_outbreak_150_quantile_2018_2021)
moving_outbreak_150_quantile_2018_2021[order(auc, decreasing = TRUE)]




# moving_outbreak_onset_150_quantile_2018_2021 <- NULL
# all_interval_estimates_2010_2021_with_150_thresholds.split <-
#   dlply(all_interval_estimates_2010_2021_with_150_thresholds, .(model))
# for(i in 1:length(quantile_dir_components_plus_ensembles_dt.split)){
#   model_dt <- subset(data.table(all_interval_estimates_2010_2021_with_150_thresholds.split[[i]]),
#                 quantile == OUTBREAK_150_THRESHOLD)
#   model_dt[, YEAR:= year(target_end_date)]
#   tmp <- subset(model_dt, true_value >= 50)
#   tmp
#   first_outbreak_months <-
#     tmp[, list(FIRST_OUTBREAK_50_MONTH = min(month(target_end_date))), by = c("location", "YEAR")]
#   model_dt <- merge(model_dt,
#                first_outbreak_months, by = c("location", "YEAR"),
#                all.x = TRUE)
#   model_dt[which(is.na(FIRST_OUTBREAK_50_MONTH)),
#                                  FIRST_OUTBREAK_50_MONTH:= 13] #If never occurs in year
#   #Subset such that only have portion of year on or before first onset month
#   model_dt <- model_dt[which(month(target_end_date) <= FIRST_OUTBREAK_50_MONTH),]
#   tmp_interval_outbreak_150_results <-
#     quantile_threshold_150_function(model_dt)
#   tmp_interval_outbreak_150_results[, model:= model_nums_dt$model[i]]
#   moving_outbreak_onset_150_quantile_2018_2021 <-
#     rbind(moving_outbreak_onset_150_quantile_2018_2021,
#           tmp_interval_outbreak_150_results)
# }
# moving_outbreak_onset_150_quantile_2018_2021 <- unique(moving_outbreak_onset_150_quantile_2018_2021)
# moving_outbreak_onset_150_quantile_2018_2021[order(auc, decreasing = TRUE)]
#





all_interval_estimates_2010_2021_with_50_thresholds <- NULL
for (i in 1:length(all_interval_estimates_2010_2021.split)) {
  model_df <- data.table(all_interval_estimates_2010_2021.split[[i]])
  model_df[, quantile := as.numeric(round(quantile, 3))]
  for (j in 1:length(times_in_2018_2021)) {
    time_in_q <- times_in_2018_2021[j]
    training_model_df <- copy(model_df)
    training_model_df <- subset(training_model_df, target_end_date < time_in_q)
    tmp <- quantile_threshold_50_function_choose_cut_off(data = training_model_df)
    chosen_threshold_50 <- head(tmp[order(auc, decreasing = TRUE)]$cut_off, 1)
    model_df[which(target_end_date == time_in_q), OUTBREAK_50_THRESHOLD := chosen_threshold_50]
  }
  all_interval_estimates_2010_2021_with_50_thresholds <-
    rbind(
      all_interval_estimates_2010_2021_with_50_thresholds,
      model_df
    )
  if (i %% 2 == 0) {
    gc()
  }
}





moving_outbreak_50_quantile_2018_2021 <- NULL
all_interval_estimates_2010_2021_with_50_thresholds[, YEAR := year(target_end_date)]
all_interval_estimates_2018_2021_with_50_thresholds <-
  subset(
    all_interval_estimates_2010_2021_with_50_thresholds,
    YEAR >= 2018
  )
tmp
all_interval_estimates_2018_2021_with_50_thresholds.split <-
  dlply(all_interval_estimates_2018_2021_with_50_thresholds, .(model))
for (i in 1:length(quantile_dir_components_plus_ensembles_dt.split)) {
  tmp <- subset(
    data.table(all_interval_estimates_2018_2021_with_50_thresholds.split[[i]]),
    quantile == OUTBREAK_50_THRESHOLD
  )
  tmp <- unique(tmp)

  tmp_interval_outbreak_50_results <-
    quantile_threshold_50_function(tmp)
  tmp_interval_outbreak_50_results[, model := model_nums_dt$model[i]]
  moving_outbreak_50_quantile_2018_2021 <-
    rbind(
      moving_outbreak_50_quantile_2018_2021,
      tmp_interval_outbreak_50_results
    )
}
moving_outbreak_50_quantile_2018_2021
moving_outbreak_50_quantile_2018_2021 <- unique(moving_outbreak_50_quantile_2018_2021)
moving_outbreak_50_quantile_2018_2021[order(auc, decreasing = TRUE)]
















# SEASON ONSET ------
model_nums_dt <- data.table(
  model = unique(quantile_dir_components_plus_ensembles_dt$model),
  model_num = seq(1, length(unique(quantile_dir_components_plus_ensembles_dt$model)))
)

tmp <- subset(
  historical_quantile_dir_components_plus_ensembles_dt,
  model == "EW-Mean *"
)
tmp[, model := "Prov-Trained *"]
tmp2 <- subset(
  historical_quantile_dir_components_plus_ensembles_dt,
  model == "EW-Mean *"
)
tmp2[, model := "Trained *"]
historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt <-
  rbind(
    historical_quantile_dir_components_plus_ensembles_dt,
    tmp,
    tmp2
  )
unique(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt$model)

historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt[, YEAR := year(target_end_date)]
tmp <- subset(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt, true_value >= 50)
first_outbreak_months <-
  tmp[, list(FIRST_OUTBREAK_50_MONTH = min(month(target_end_date))), by = c("location", "YEAR")]
historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt <-
  merge(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt,
    first_outbreak_months,
    by = c("location", "YEAR"),
    all.x = TRUE
  )
historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt[
  which(is.na(FIRST_OUTBREAK_50_MONTH)),
  FIRST_OUTBREAK_50_MONTH := 13
] # If never occurs in year
# Subset such that only have portion of year on or before first onset month
historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt <-
  historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt[which(month(target_end_date) <= FIRST_OUTBREAK_50_MONTH), ]

historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt.split <- dlply(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt, .(model))


# models_interval_2010_2018_thresholds_onset_50 <- rep(NA, length(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt.split))
# for(i in 1:length(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt.split)){
#   print(i)
#   tmp <- quantile_threshold_50_function(data = data.table(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt.split[[i]]))
#   tmp_2010_2018_chosen_threshold_50 <-
#     head(tmp[order(auc, decreasing = TRUE)]$cut_off, 1)  #Chosen threshold from 2010-2017
#   models_interval_2010_2018_thresholds_onset_50[i] <- tmp_2010_2018_chosen_threshold_50
# }
# models_interval_2010_2018_thresholds_onset_50







unique(quantile_dir_components_plus_ensembles_dt$model)

tmp <- subset(
  historical_quantile_dir_components_plus_ensembles_dt,
  model == "EW-Mean *"
)
tmp[, model := "Prov-Trained *"]
tmp2 <- subset(
  historical_quantile_dir_components_plus_ensembles_dt,
  model == "EW-Mean *"
)
tmp2[, model := "Trained *"]
historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt <-
  rbind(
    historical_quantile_dir_components_plus_ensembles_dt,
    tmp,
    tmp2
  )
unique(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt$model)

historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt[, YEAR := year(target_end_date)]
tmp <- subset(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt, true_value >= 150)
first_outbreak_months <-
  tmp[, list(FIRST_OUTBREAK_150_MONTH = min(month(target_end_date))), by = c("location", "YEAR")]
historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt <-
  merge(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt,
    first_outbreak_months,
    by = c("location", "YEAR"),
    all.x = TRUE
  )
historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt[
  which(is.na(FIRST_OUTBREAK_150_MONTH)),
  FIRST_OUTBREAK_150_MONTH := 13
] # If never occurs in year
# Subset such that only have portion of year on or before first onset month
historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt <-
  historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt[which(month(target_end_date) <= FIRST_OUTBREAK_150_MONTH), ]

historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt.split <- dlply(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt, .(model))



# models_interval_2010_2018_thresholds_onset_150 <- rep(NA, length(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt.split))
# for(i in 1:length(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt.split)){
#   print(i)
#   tmp <- quantile_threshold_150_function(data = data.table(historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt.split[[i]]))
#   tmp_2010_2018_chosen_threshold_150 <-
#     head(tmp[order(auc, decreasing = TRUE)]$cut_off, 1)  #Chosen threshold from 2010-2017
#   models_interval_2010_2018_thresholds_onset_150[i] <- tmp_2010_2018_chosen_threshold_150
# }
# models_interval_2010_2018_thresholds_onset_150

# 2018-2021 (onset 50)
quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt <-
  copy(quantile_dir_components_plus_ensembles_dt)
quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt[, YEAR := year(target_end_date)]
tmp <- subset(quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt, true_value >= 50)
first_outbreak_months <-
  tmp[, list(FIRST_OUTBREAK_50_MONTH = min(month(target_end_date))), by = c("location", "YEAR")]
quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt <-
  merge(quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt,
    first_outbreak_months,
    by = c("location", "YEAR"),
    all.x = TRUE
  )
quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt[
  which(is.na(FIRST_OUTBREAK_50_MONTH)),
  FIRST_OUTBREAK_50_MONTH := 13
] # If never occurs in year
# Subset such that only have portion of year on or before first onset month
quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt <-
  quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt[which(month(target_end_date) <= FIRST_OUTBREAK_50_MONTH), ]

quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt.split <- dlply(quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt, .(model))



# 2018-2021 (onset 50)
quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt <-
  copy(quantile_dir_components_plus_ensembles_dt)
quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt[, YEAR := year(target_end_date)]
tmp <- subset(quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt, true_value >= 150)
first_outbreak_months <-
  tmp[, list(FIRST_OUTBREAK_150_MONTH = min(month(target_end_date))), by = c("location", "YEAR")]
quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt <-
  merge(quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt,
    first_outbreak_months,
    by = c("location", "YEAR"),
    all.x = TRUE
  )
quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt[
  which(is.na(FIRST_OUTBREAK_150_MONTH)),
  FIRST_OUTBREAK_150_MONTH := 13
] # If never occurs in year
# Subset such that only have portion of year on or before first onset month
quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt <-
  quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt[which(month(target_end_date) <= FIRST_OUTBREAK_150_MONTH), ]

# quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt.split <- dlply(quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt, .(model))
#
# interval_outbreak_onset_50_results <- NULL
# interval_outbreak_onset_150_results <- NULL
# for(i in 1:length(quantile_dir_components_plus_ensembles_dt.split)){
#   tmp <- subset(data.table(quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt.split[[i]]),
#                 quantile == models_interval_2010_2018_thresholds_onset_150[i])
#
#   tmp <- unique(tmp)
#   print("Here 1:")
#
#   tmp_interval_outbreak_150_results <-
#     quantile_threshold_150_function(tmp)
#   tmp_interval_outbreak_150_results[, model:= model_nums_dt$model[i]]
#   interval_outbreak_onset_150_results <- rbind(interval_outbreak_onset_150_results,
#                                          tmp_interval_outbreak_150_results)
#   tmp <- subset(data.table(quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt.split[[i]]),
#                 quantile == models_interval_2010_2018_thresholds_onset_50[i])
#   tmp <- unique(tmp)
#   print("Here 2:")
#   tmp_interval_outbreak_50_results <-
#     quantile_threshold_50_function(tmp)
#   tmp_interval_outbreak_50_results[, model:= model_nums_dt$model[i]]
#
#   interval_outbreak_onset_50_results <- rbind(interval_outbreak_onset_50_results,
#                                         tmp_interval_outbreak_50_results)
#
# }
# interval_outbreak_onset_50_results <- unique(interval_outbreak_onset_50_results)
# interval_outbreak_onset_50_results[order(auc, decreasing = TRUE)]
# interval_outbreak_onset_150_results <- unique(interval_outbreak_onset_150_results)
# interval_outbreak_onset_150_results[order(auc, decreasing = TRUE)]
#
#
#
#


# Moving threshold
all_interval_estimates_2010_2021 <- rbind(
  historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt,
  quantile_dir_components_plus_ensembles_dt_only_season_onset_150_dt
)
all_interval_estimates_2010_2021.split <- dlply(all_interval_estimates_2010_2021, .(model))
all_interval_estimates_2010_2021
all_interval_estimates_2010_2021[, length(true_value), by = "model"]
times_in_2018_2021 <- unique(ptl_province_inla_df[which(YEAR >= 2018), ]$end_of_month)
all_interval_estimates_2010_2021_with_onset_150_thresholds <- NULL
for (i in 1:length(all_interval_estimates_2010_2021.split)) {
  model_df <- data.table(all_interval_estimates_2010_2021.split[[i]])
  model_df[, quantile := as.numeric(round(quantile, 3))]
  for (j in 1:length(times_in_2018_2021)) {
    time_in_q <- times_in_2018_2021[j]
    training_model_df <- copy(model_df)
    training_model_df
    training_model_df <- subset(training_model_df, target_end_date < time_in_q)
    tmp <- quantile_threshold_150_function(data = training_model_df)
    chosen_threshold_150 <- head(tmp[order(auc, decreasing = TRUE)]$cut_off, 1)
    model_df[which(target_end_date == time_in_q), OUTBREAK_150_THRESHOLD := chosen_threshold_150]
  }
  all_interval_estimates_2010_2021_with_onset_150_thresholds <-
    rbind(
      all_interval_estimates_2010_2021_with_onset_150_thresholds,
      model_df
    )
  if (i %% 2 == 0) {
    gc()
  }
}
all_interval_estimates_2010_2021_with_onset_150_thresholds

moving_outbreak_onset_150_quantile_2018_2021 <- NULL



all_interval_estimates_2018_2021_with_onset_150_thresholds <-
  subset(
    all_interval_estimates_2018_2021_with_onset_150_thresholds,
    YEAR >= 2018
  )
all_interval_estimates_2018_2021_with_onset_150_thresholds.split <-
  dlply(all_interval_estimates_2018_2021_with_onset_150_thresholds, .(model))
all_interval_estimates_2018_2021_with_onset_150_thresholds
for (i in 1:length(quantile_dir_components_plus_ensembles_dt.split)) {
  tmp <- subset(
    data.table(all_interval_estimates_2018_2021_with_onset_150_thresholds.split[[i]]),
    quantile == OUTBREAK_150_THRESHOLD
  )
  tmp <- unique(tmp)
  tmp_interval_outbreak_150_results <-
    quantile_threshold_150_function(tmp)
  tmp_interval_outbreak_150_results[, model := model_nums_dt$model[i]]
  moving_outbreak_onset_150_quantile_2018_2021 <-
    rbind(
      moving_outbreak_onset_150_quantile_2018_2021,
      tmp_interval_outbreak_150_results
    )
}
moving_outbreak_onset_150_quantile_2018_2021 <- unique(moving_outbreak_onset_150_quantile_2018_2021)
moving_outbreak_onset_150_quantile_2018_2021[order(auc, decreasing = TRUE)]

moving_outbreak_150_quantile_2018_2021[order(auc, decreasing = TRUE)]









all_interval_estimates_2010_2021 <- rbind(
  historical_quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt,
  quantile_dir_components_plus_ensembles_dt_only_season_onset_50_dt
)
all_interval_estimates_2010_2021.split <- dlply(all_interval_estimates_2010_2021, .(model))

all_interval_estimates_2010_2021[, length(true_value), by = "model"]
times_in_2018_2021 <- unique(ptl_province_inla_df[which(YEAR >= 2018), ]$end_of_month)
all_interval_estimates_2010_2021_with_onset_50_thresholds <- NULL
gc()
for (i in 1:length(all_interval_estimates_2010_2021.split)) {
  model_df <- data.table(all_interval_estimates_2010_2021.split[[i]])
  model_df[, quantile := as.numeric(round(quantile, 3))]
  for (j in 1:length(times_in_2018_2021)) {
    time_in_q <- times_in_2018_2021[j]
    training_model_df <- copy(model_df)
    training_model_df
    training_model_df <- subset(training_model_df, target_end_date < time_in_q)
    tmp <- quantile_threshold_50_function(data = training_model_df)
    chosen_threshold_50 <- head(tmp[order(auc, decreasing = TRUE)]$cut_off, 1)
    model_df[, OUTBREAK_50_THRESHOLD := chosen_threshold_50]
  }
  all_interval_estimates_2010_2021_with_onset_50_thresholds <-
    rbind(
      all_interval_estimates_2010_2021_with_onset_50_thresholds,
      model_df
    )
  if (i %% 2 == 0) {
    gc()
  }
}
all_interval_estimates_2010_2021_with_onset_50_thresholds[, length(unique(OUTBREAK_50_THRESHOLD)), by = "model"]
all_interval_estimates_2010_2021_with_onset_50_thresholds
moving_outbreak_onset_50_quantile_2018_2021 <- NULL
all_interval_estimates_2010_2021_with_onset_50_thresholds.split <-
  dlply(all_interval_estimates_2010_2021_with_50_thresholds, .(model))
all_interval_estimates_2010_2021_with_onset_50_thresholds.split
quans <- list()
for (i in 1:length(quantile_dir_components_plus_ensembles_dt.split)) {
  tmp <- subset(
    data.table(all_interval_estimates_2010_2021_with_onset_50_thresholds.split[[i]]),
    quantile == OUTBREAK_50_THRESHOLD
  )
  tmp <- unique(tmp)
  tmp[, YEAR := year(target_end_date)]
  tmp <- subset(tmp, YEAR >= 2018)
  quans[[i]] <- unique(tmp$quantile)
  tmp_interval_outbreak_50_results <-
    quantile_threshold_50_function(tmp)
  tmp_interval_outbreak_50_results[, model := model_nums_dt$model[i]]
  moving_outbreak_onset_50_quantile_2018_2021 <-
    rbind(
      moving_outbreak_onset_50_quantile_2018_2021,
      tmp_interval_outbreak_50_results
    )
}
quans
tmp_interval_outbreak_50_results
moving_outbreak_onset_50_quantile_2018_2021 <- unique(moving_outbreak_onset_50_quantile_2018_2021)
moving_outbreak_onset_50_quantile_2018_2021[order(auc, decreasing = TRUE)]













# FIX NAMES ----
original_model_names <- sort(unique(quantile_dir_components_plus_ensembles_dt$model))
original_model_names
new_model_names <- c(
  "Baseline", "Bayes-Climate", "EW-Mean *",
  "EW-Mean-NoBase *", "Ew-Mean-NoBayes *", "EW-Mean-NoCov *",
  "TimeGPT", "TimeGPT-NoCov ", "Median *", "Median-NoBase *",
  "Median-NoBayes *", "Median-NoCov *", "Trained *", "Prov-Trained *",
  "SARIMA", "TCN"
)
new_model_names
# extract hex color codes for a plot with three elements in ggplot2
hex <- hue_pal()(length(new_model_names))
historical_original_model_names <- sort(unique(historical_quantile_dir_components_plus_ensembles_dt$model))
historical_original_model_names
historical_new_model_names <- c(
  "Baseline", "Bayes-Climate", "EW-Mean *",
  "EW-Mean-NoBase *", "Ew-Mean-NoBayes *", "EW-Mean-NoCov *",
  "TimeGPT", "TimeGPT-NoCov ", "Median *", "Median-NoBase *",
  "Median-NoBayes *", "Median-NoCov *",
  "SARIMA", "TCN"
)
historical_model_colours <- hex[which(new_model_names %in% historical_new_model_names)]
show_col(historical_model_colours)
setkey(quantile_dir_components_plus_ensembles_dt, "model")
quantile_dir_components_plus_ensembles_dt[, model := factor(model,
  labels = new_model_names
)]
quantile_dir_components_plus_ensembles_dt

historical_quantile_dir_components_plus_ensembles_dt[, model := factor(model,
  labels = historical_new_model_names
)]
historical_quantile_dir_components_plus_ensembles_dt
setkey(quantile_dir_components_plus_ensembles_dt, "model")
setkey(historical_quantile_dir_components_plus_ensembles_dt, "model")

# Repeat for outbreak results
tmp <- copy(moving_outbreak_150_quantile_2018_2021)
moving_outbreak_150_quantile_2018_2021[, model := factor(model,
  labels = new_model_names
)]
moving_outbreak_50_quantile_2018_2021[, model := factor(model,
  labels = new_model_names
)]


# INSERT LATITUDE AND LONGITUDE INDICATORS ----
tmp <- unique(subset(ptl_province_inla_df,
  select = c("PROVINCE", "LAT_PROV_IND", "LONG_PROV_IND")
))
historical_quantile_dir_components_plus_ensembles_dt <-
  merge(historical_quantile_dir_components_plus_ensembles_dt,
    tmp,
    by.y = "PROVINCE", by.x = "location"
  )
quantile_dir_components_plus_ensembles_dt <-
  merge(quantile_dir_components_plus_ensembles_dt,
    tmp,
    by.y = "PROVINCE", by.x = "location"
  )






# Visualise Outbreak Detection

# Visualise WIS

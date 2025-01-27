library(logger)

# Ensuring the following have been defined
log_info("Ensure the following have been defined")
ptl_province_inla_df[, LAG_1_CASES := expm1(LAG_1_LOG_CASES)]
ptl_province_inla_df[, DIFF_CASES := CASES - LAG_1_CASES]


# Functions to run baseline model
log_info("Functions to run baseline model")
new_quantile_baseline <- function(inc_diffs, symmetrize = TRUE) {
  if (symmetrize) {
    quantile_baseline <- structure(
      c(inc_diffs, -inc_diffs),
      symmetrize = symmetrize,
      class = "quantile_baseline"
    )
  } else {
    quantile_baseline <- structure(
      inc_diffs,
      symmetrize = symmetrize,
      class = "quantile_baseline"
    )
  }

  return(quantile_baseline)
}

new_predict.quantile_baseline <- function(
    quantile_baseline,
    newdata,
    horizon,
    nsim,
    ...) {
  # storage space for result
  result <- matrix(NA_real_,
    nrow = nsim,
    ncol = horizon
  )

  # initialize at most recent observed incidence
  # last_inc <- sampled_inc_raw <- tail(newdata, 1)
  last_inc <- sampled_inc_raw <- newdata

  # quantiles of past differences in incidence
  sampled_inc_diffs <- quantile(
    quantile_baseline,
    probs = seq(from = 0, to = 1.0, length = nsim),
    na.rm = TRUE
  )
  # print(sampled_inc_diffs)
  for (h in seq_len(horizon)) {
    # print(h)
    sampled_inc_diffs <- sample(sampled_inc_diffs, size = nsim, replace = FALSE)
    sampled_inc_raw <- sampled_inc_raw + sampled_inc_diffs

    # if fit was done with symmetrize=TRUE, force median difference = 0
    if (attr(quantile_baseline, "symmetrize")) {
      # print(sampled_inc_raw)
      sampled_inc_corrected <- sampled_inc_raw - (median(sampled_inc_raw) - last_inc)
    } else {
      sampled_inc_corrected <- sampled_inc_raw
    }

    # save results
    result[, h] <- sampled_inc_corrected
  }

  return(result)
}

fit_baseline_function_to_all_data <- function(data, predict_times) {
  log_info("Fit baseline function to all data")
  filenames <- NULL
  results_dt <- NULL
  for (i in 1:length(unique(data$PROVINCE))) {
    prov_in_q <- unique(data$PROVINCE)[i]
    province_level_data <- subset(data, PROVINCE == prov_in_q)
    for (j in 1:length(predict_times)) {

      # Skip if the file already exists
      if (file.exists(file.path(
        peru.province.ensemble.out.dir,
        paste0("baseline_results_", i, "_", j, ".RDS")
      ))) {
        log_info(paste0("File exists -- skipping province ", i, " time ", j))
        next
      }

      log_info(paste0("Predicting province ", prov_in_q, " [", i, "]", " time ", predict_times[j], " [", j, "]"))
      forecast_time_in_q <- predict_times[j] # Times at which we make the predictions
      tmp_province_level_data <- subset(
        province_level_data,
        TIME <= forecast_time_in_q
      )
      tmp_quantile_baseline_object <- new_quantile_baseline(tmp_province_level_data$DIFF_CASES)
      tmp_baseline_result <- new_predict.quantile_baseline(tmp_quantile_baseline_object,
        newdata = tmp_province_level_data[which(TIME == forecast_time_in_q), ]$CASES,
        horizon = 1,
        nsim = 5000
      )
      tmp_result <- data.table(PRED = tmp_baseline_result)
      tmp_result[, PROVINCE := rep(prov_in_q, nrow(tmp_result))]
      tmp_result[, SAMPLE := seq(1, nrow(tmp_result), by = 1)]

      # Important: This prediction is for the next time point
      tmp_result[, TIME := rep(forecast_time_in_q + 1, nrow(tmp_result))]

      filename <- file.path(
        peru.province.ensemble.out.dir,
        paste0("baseline_results_", i, "_", j, ".RDS")
      )
      saveRDS(tmp_result, file = filename)
      filenames.append(filename)
      tmp_result <- NULL   # Clear the memory
      tmp_province_level_data <- NULL
      tmp_quantile_baseline_object <- NULL
      tmp_baseline_result <- NULL

      #results_dt <- rbind(results_dt, tmp_result)
    }
  }
  
  for (i in 1:length(unique(data$PROVINCE))) {
    for (j in 1:length(predict_times)) {
        filenames <- c(filenames, file.path(
          peru.province.ensemble.out.dir,
          paste0("baseline_results_", i, "_", j, ".RDS")
        ))
    }
  }

  # Read and combine all files
  log_info("Concatenating baseline results")
  results_dt <- rbindlist(
    lapply(
      filenames,
      function(f) {
        d <- readRDS(f)
        d[, PROVINCE := as.factor(PROVINCE)]
        d[, TIME := as.integer(TIME)]
        d[, SAMPLE := as.integer(SAMPLE)]
        return(d)
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )

  return(results_dt)
}

# Times at which we make our forecasts
log_info("Times at which we make our forecasts")
predict_times <- unique(ptl_province_inla_df[which(TIME < max(TIME))]$TIME)
baseline_results_filename <- file.path(
  peru.province.ensemble.out.dir,
  "baseline_results_2010_2021.RDS"
)
if (file.exists(baseline_results_filename)) {
  baseline_results <- readRDS(baseline_results_filename)
} else {
  baseline_results <- fit_baseline_function_to_all_data(
    ptl_province_inla_df,
    predict_times
  )
  saveRDS(baseline_results, file = baseline_results_filename)
}

# Set up data.tables for scoring ----
# DIR ----
log_info("Set up data.tables for scoring")
baseline_dir.pred_dt_2010_2021 <- data.table(baseline_results)
baseline_dir.pred_dt_2010_2021
setnames(
  baseline_dir.pred_dt_2010_2021,
  c("PRED.V1", "SAMPLE"),
  c("prediction", "sample")
)
baseline_dir.pred_dt_2010_2021 <- merge(baseline_dir.pred_dt_2010_2021,
  subset(ptl_province_inla_df,
    select = c(
      "PROVINCE", "TIME", "MONTH", "YEAR", "DIR",
      "POP_OFFSET",
      "LAT_PROV_IND", "LONG_PROV_IND", "end_of_month"
    )
  ),
  by = c("PROVINCE", "TIME")
)
baseline_dir.pred_dt_2010_2021[, true_value := DIR]
baseline_dir.pred_dt_2010_2021
baseline_dir.pred_dt_2010_2021[, prediction := prediction / POP_OFFSET]
# baseline_dir.pred_dt_2010_2021[which(prediction <0), prediction:= 0]
baseline_dir.pred_dt_2010_2018 <- subset(
  baseline_dir.pred_dt_2010_2021,
  YEAR < 2018
)

baseline_dir.pred_dt_2010_2018_for_scoring <- copy(baseline_dir.pred_dt_2010_2018)
baseline_dir.pred_dt_2010_2018_for_scoring[, model := "baseline"]
baseline_dir.pred_dt_2010_2018_for_scoring[which(prediction < 0), prediction := 0]
setnames(
  baseline_dir.pred_dt_2010_2018_for_scoring,
  c("PROVINCE", "end_of_month"), c("location", "target_end_date")
)
baseline_dir.pred_dt_2010_2018_for_scoring <- subset(baseline_dir.pred_dt_2010_2018_for_scoring,
  select = c(
    "location", "sample",
    "true_value", "prediction",
    "model", "target_end_date"
  )
)

log_info("Check forecasts")
check_forecasts(baseline_dir.pred_dt_2010_2018_for_scoring)

quantile_baseline_dir.pred_dt_2010_2018 <-
  sample_to_quantile(baseline_dir.pred_dt_2010_2018_for_scoring,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
quantile_baseline_dir.pred_dt_2010_2018 %>%
  score() %>%
  summarise_scores(by = c("model"))
quantile_baseline_dir.pred_dt_2010_2018
quantile_baseline_dir.pred_dt_2010_2018[which(quantile == 0.5 & !is.na(prediction)), caret::R2(true_value, prediction)]
quantile_baseline_dir.pred_dt_2010_2018[which(quantile == 0.5), caret::MAE(true_value, prediction)]
saveRDS(quantile_baseline_dir.pred_dt_2010_2018,
  file = file.path(
    peru.province.out.dir,
    "quantile_baseline_dir.pred_dt_2010_2018.RDS"
  )
)


# Testing Period (2018 - 2021)
baseline_dir.pred_dt_2018_2021 <- subset(
  baseline_dir.pred_dt_2010_2021,
  YEAR >= 2018
)

baseline_dir.pred_dt_2018_2021_for_scoring <- copy(baseline_dir.pred_dt_2018_2021)
baseline_dir.pred_dt_2018_2021_for_scoring[, model := "baseline"]
setnames(
  baseline_dir.pred_dt_2018_2021_for_scoring,
  c("PROVINCE", "end_of_month"), c("location", "target_end_date")
)
baseline_dir.pred_dt_2018_2021_for_scoring <- subset(baseline_dir.pred_dt_2018_2021_for_scoring,
  select = c(
    "location", "sample",
    "true_value", "prediction",
    "model", "target_end_date"
  )
)

check_forecasts(baseline_dir.pred_dt_2018_2021_for_scoring)


quantile_baseline_dir.pred_dt_2018_2021 <-
  sample_to_quantile(baseline_dir.pred_dt_2018_2021_for_scoring,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
quantile_baseline_dir.pred_dt_2018_2021 %>%
  score() %>%
  summarise_scores(by = c("model"))
quantile_baseline_dir.pred_dt_2018_2021
quantile_baseline_dir.pred_dt_2018_2021[which(quantile == 0.5), caret::R2(true_value, prediction)]
quantile_baseline_dir.pred_dt_2018_2021[which(quantile == 0.5), caret::MAE(true_value, prediction)]
saveRDS(quantile_baseline_dir.pred_dt_2018_2021,
  file = file.path(
    peru.province.out.dir,
    "quantile_baseline_dir.pred_dt_2018_2021.RDS"
  )
)

quantile_baseline_dir.pred_dt_2018_2021 <- readRDS(file = file.path(
  peru.province.out.dir,
  "quantile_baseline_dir.pred_dt_2018_2021.RDS"
))


# 2) Log Cases ----
log_info("Log cases...")
baseline_log_cases.pred_dt_2010_2021 <- data.table(baseline_results)
setnames(
  baseline_log_cases.pred_dt_2010_2021,
  c("PRED.V1", "SAMPLE"),
  c("prediction", "sample")
)
baseline_log_cases.pred_dt_2010_2021 <- merge(baseline_log_cases.pred_dt_2010_2021,
  subset(ptl_province_inla_df,
    select = c(
      "PROVINCE", "TIME", "MONTH", "YEAR", "CASES",
      "LAT_PROV_IND", "LONG_PROV_IND", "end_of_month"
    )
  ),
  by = c("PROVINCE", "TIME")
)
baseline_log_cases.pred_dt_2010_2021[, model := "baseline"]
setnames(baseline_log_cases.pred_dt_2010_2021, "CASES", "true_value")

baseline_log_cases.pred_dt_2010_2021[which(prediction < 0), prediction := 0]
baseline_log_cases.pred_dt_2010_2021[, true_value := log1p(true_value)]
baseline_log_cases.pred_dt_2010_2021[, prediction := log1p(prediction)]

baseline_log_cases.pred_dt_2010_2018 <- subset(
  baseline_log_cases.pred_dt_2010_2021,
  YEAR < 2018
)

baseline_log_cases.pred_dt_2010_2018_for_scoring <- copy(baseline_log_cases.pred_dt_2010_2018)
baseline_log_cases.pred_dt_2010_2018_for_scoring
setnames(
  baseline_log_cases.pred_dt_2010_2018_for_scoring,
  c("PROVINCE", "end_of_month"), c("location", "target_end_date")
)
baseline_log_cases.pred_dt_2010_2018_for_scoring <- subset(baseline_log_cases.pred_dt_2010_2018_for_scoring,
  select = c(
    "location", "sample",
    "true_value", "prediction",
    "model", "target_end_date"
  )
)

log_info("Check forecasts")
check_forecasts(baseline_log_cases.pred_dt_2010_2018_for_scoring)


quantile_baseline_log_cases.pred_dt_2010_2018 <-
  sample_to_quantile(baseline_log_cases.pred_dt_2010_2018_for_scoring,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
quantile_baseline_log_cases.pred_dt_2010_2018 %>%
  score() %>%
  summarise_scores(by = c("model"))
quantile_baseline_log_cases.pred_dt_2010_2018[which(quantile == 0.5 & !is.na(prediction)), caret::R2(true_value, prediction)]
quantile_baseline_log_cases.pred_dt_2010_2018[which(quantile == 0.5 & !is.na(prediction)), caret::MAE(true_value, prediction)]

saveRDS(baseline_log_cases.pred_dt_2010_2018, file = file.path(
  peru.province.inla.data.out.dir,
  paste0("baseline_log_cases.pred_dt_2010_2018.RDS")
))
saveRDS(quantile_baseline_log_cases.pred_dt_2010_2018, file = file.path(
  peru.province.out.dir,
  paste0("quantile_baseline_log_cases.pred_dt_2010_2018.RDS")
))



baseline_log_cases.pred_dt_2018_2021 <- subset(
  baseline_log_cases.pred_dt_2010_2021,
  YEAR >= 2018
)

baseline_log_cases.pred_dt_2018_2021_for_scoring <- copy(baseline_log_cases.pred_dt_2018_2021)
setnames(
  baseline_log_cases.pred_dt_2018_2021_for_scoring,
  c("PROVINCE", "end_of_month"), c("location", "target_end_date")
)
baseline_log_cases.pred_dt_2018_2021_for_scoring <- subset(baseline_log_cases.pred_dt_2018_2021_for_scoring,
  select = c(
    "location", "sample",
    "true_value", "prediction",
    "model", "target_end_date"
  )
)

check_forecasts(baseline_log_cases.pred_dt_2018_2021_for_scoring)


quantile_baseline_log_cases.pred_dt_2018_2021 <-
  sample_to_quantile(baseline_log_cases.pred_dt_2018_2021_for_scoring,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
quantile_baseline_log_cases.pred_dt_2018_2021 %>%
  score() %>%
  summarise_scores(by = c("model"))
quantile_baseline_log_cases.pred_dt_2018_2021[which(quantile == 0.5), caret::R2(true_value, prediction)]
quantile_baseline_log_cases.pred_dt_2018_2021[which(quantile == 0.5), caret::MAE(true_value, prediction)]

saveRDS(baseline_log_cases.pred_dt_2010_2018, file = file.path(
  peru.province.inla.data.out.dir,
  paste0("baseline_log_cases.pred_dt_2010_2018.RDS")
))
saveRDS(quantile_baseline_log_cases.pred_dt_2018_2021, file = file.path(
  peru.province.out.dir,
  paste0("quantile_baseline_log_cases.pred_dt_2018_2021.RDS")
))

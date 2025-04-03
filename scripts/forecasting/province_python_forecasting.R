library(logger)

quantiles = c(
  0.01, 0.025,
  seq(0.05, 0.95, 0.05),
  0.975, 0.99
)

# data
log_info("load data")
ptl_province_inla_df <- data.table(read.csv(file.path(
  peru.province.python.data.dir,
  "ptl_province_inla_df.csv"
)))
ptl_province_2018_2021_data <- subset(
  ptl_province_inla_df,
  YEAR >= 2018
)
ptl_province_2018_2021_data[, IND := seq(1, length(DIR)), by = "PROVINCE"]

province_names <- unique(ptl_province_inla_df$PROVINCE)
province_names
# Functions ----
process_python_deep_preds <- function(model_name) {
  ovr_preds_dt <- NULL
  for (i in 0:13) {
    prov_in_q <- province_names[i + 1]
    tmp <-
      data.table(read.csv(file.path(
        peru.province.python.out.dir,
        paste0(
          model_name,
          "_dataframe_", i,
          ".csv"
        )
      )))
    tmp[, PROVINCE := prov_in_q]
    ovr_preds_dt <- rbind(ovr_preds_dt, tmp)
  }
  return(ovr_preds_dt)
}

# HISTORICAL SARIMA ----
log_info("historical sarima")
raw_historical_sarima_preds_dt <- process_python_deep_preds("historical_sarima")
ptl_province_2010_2018_data <- subset(
  ptl_province_inla_df,
  YEAR < 2018
)
ptl_province_2010_2018_data[, IND := seq(1, length(DIR)), by = "PROVINCE"]
# If re-using this code, please note the hard-coded 82 below, based on
# the fact that we only have 42 data points in historical data.table
ptl_province_2010_2018_data <- ptl_province_2010_2018_data[which(IND > (max(IND)) - 82)]
ptl_province_2010_2018_data[, IND := seq(1, length(DIR)), by = "PROVINCE"] # New Index to match forecast data.table
raw_historical_sarima_preds_dt[, IND := rep(
  seq(
    1,
    length(unique(ptl_province_2010_2018_data$TIME))
  ),
  each = 1
),
by = "PROVINCE"
]
head(raw_historical_sarima_preds_dt)

raw_historical_sarima_preds_dt$X <- NULL
historical_sarima_preds_dt <- melt(raw_historical_sarima_preds_dt,
  id.vars = c("IND", "PROVINCE"),
  variable.name = "SAMPLE", value.name = "prediction"
)
historical_sarima_preds_dt[, SAMPLE := as.numeric(gsub("X", "", SAMPLE))]
historical_sarima_preds_dt[, SAMPLE := SAMPLE + 1]

historical_sarima_preds_dt <-
  merge(historical_sarima_preds_dt,
    ptl_province_2010_2018_data,
    by = c("PROVINCE", "IND")
  )
historical_sarima_preds_dt[, model := "historical_sarima"]
historical_sarima_preds_dt[, expm1(LOG_CASES) / POP_OFFSET]
historical_sarima_preds_dt[, DIR_prediction := expm1(prediction) / POP_OFFSET]
setnames(historical_sarima_preds_dt, "LOG_CASES", "true_value")
setnames(historical_sarima_preds_dt, "SAMPLE", "sample")
historical_sarima_preds_dt[, location := PROVINCE]
historical_sarima_preds_dt_for_scoring <- subset(historical_sarima_preds_dt,
  select = c(
    "location", "sample",
    "true_value", "prediction",
    "model", "end_of_month"
  )
)
setnames(historical_sarima_preds_dt_for_scoring, "end_of_month", "target_end_date")
check_forecasts(historical_sarima_preds_dt_for_scoring)
process_quantile_predictions
historical_sarima_scores_log_cases_2010_2018_dt <- historical_sarima_preds_dt_for_scoring %>%
  score() %>%
  summarise_scores(by = c("model"))

quantile_historical_sarima_preds_dt <-
  sample_to_quantile(historical_sarima_preds_dt_for_scoring,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
process_quantile_predictions(quantile_historical_sarima_preds_dt)

quantile_historical_sarima_preds_dt %>%
  score() %>%
  summarise_scores(by = c("model", "range")) %>%
  plot_interval_coverage()
quantile_historical_sarima_preds_dt[which(quantile == 0.5), caret::R2(prediction, true_value), by = "location"]

saveRDS(quantile_historical_sarima_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_historical_sarima_preds_dt.RDS"
  )
)


historical_sarima_dir_preds_dt_for_scoring <-
  copy(historical_sarima_preds_dt)
historical_sarima_dir_preds_dt_for_scoring[, prediction := expm1(prediction) / POP_OFFSET]
historical_sarima_dir_preds_dt_for_scoring[, true_value := expm1(true_value) / POP_OFFSET]
# setnames(historical_sarima_dir_preds_dt_for_scoring, "SAMPLE", "sample")  # JSB: Changed _dt name
historical_sarima_dir_preds_dt_for_scoring[, location := PROVINCE]
historical_sarima_dir_preds_dt_for_scoring <- subset(historical_sarima_dir_preds_dt_for_scoring,
  select = c(
    "location", "sample",
    "true_value", "prediction",
    "model", "end_of_month"
  )
)
check_forecasts(historical_sarima_dir_preds_dt_for_scoring)

historical_sarima_scores_dir_2010_2018_dt <- historical_sarima_dir_preds_dt_for_scoring %>%
  score() %>%
  summarise_scores(by = c("model"))

quantile_historical_sarima_dir_preds_dt <-
  sample_to_quantile(historical_sarima_dir_preds_dt_for_scoring,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
process_quantile_predictions(quantile_historical_sarima_dir_preds_dt)

saveRDS(quantile_historical_sarima_dir_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_historical_sarima_dir_preds_dt.RDS"
  )
)




# Historical FINETUNED TIMEGPT----
# 95, 90, 85, 80, 75, 70,  65, 60,

log_info("finetuned timegpt")
historical_fine_tuned_timegpt_preds_dt <- data.table(process_python_deep_preds("historical_timegpt"))
length(colnames(historical_fine_tuned_timegpt_preds_dt)[4:26])
#historical_fine_tuned_timegpt_preds_dt[, TimeGPT.lo.0 := NULL] #  JSB
tmp_quantiles <- rev.default(quantiles)
tmp_quantiles <- tmp_quantiles[which(tmp_quantiles != 0.5)]
tmp_quantiles[1:11] <- rev.default(tmp_quantiles[1:11])

setnames(
  historical_fine_tuned_timegpt_preds_dt,
  colnames(historical_fine_tuned_timegpt_preds_dt)[4:26],
  c("MEDIAN", paste0("q_", tmp_quantiles))
)
# historical_fine_tuned_timegpt_preds_dt[, IND:= seq(1, length(TimeGPT)), by = "PROVINCE"]
historical_fine_tuned_timegpt_preds_dt
ptl_province_2010_2018_data <- subset(
  ptl_province_inla_df,
  YEAR < 2018
)
ptl_province_2010_2018_data <- subset(
  ptl_province_2010_2018_data,
  end_of_month >= min(historical_fine_tuned_timegpt_preds_dt$end_of_month)
)
# ptl_province_2010_2018_data[, IND:= seq(1, length(DIR)), by = "PROVINCE"]
historical_fine_tuned_timegpt_preds_dt
historical_fine_tuned_timegpt_preds_dt[, X := NULL] # Redundant index column
historical_fine_tuned_timegpt_preds_dt[, length(end_of_month), by = "PROVINCE"]
historical_fine_tuned_timegpt_preds_dt
historical_fine_tuned_timegpt_preds_dt <-
  merge(historical_fine_tuned_timegpt_preds_dt,
    ptl_province_2010_2018_data,
    by = c("PROVINCE", "end_of_month")
  )

historical_fine_tuned_timegpt_preds_dt
historical_fine_tuned_timegpt_preds_dt[, caret::R2(TimeGPT, LOG_CASES), by = "PROVINCE"]
historical_fine_tuned_timegpt_preds_dt[, caret::MAE(TimeGPT, LOG_CASES), by = "PROVINCE"]
historical_fine_tuned_timegpt_preds_dt[, caret::MAE(expm1(TimeGPT) / POP_OFFSET, DIR)]
historical_fine_tuned_timegpt_preds_dt[, caret::R2(expm1(TimeGPT) / POP_OFFSET, DIR)]

historical_fine_tuned_timegpt_preds_dt[, caret::R2(TimeGPT, LOG_CASES)]
historical_fine_tuned_timegpt_preds_dt[, caret::MAE(TimeGPT, LOG_CASES)]

historical_fine_tuned_timegpt_preds_dt[, length(which(q_0.025 <= LOG_CASES & q_0.975 >= LOG_CASES)) /
  length(TimeGPT)]

tmp <- copy(historical_fine_tuned_timegpt_preds_dt)
colnames(tmp)[1:26]
tmp
tmp <- subset(tmp, select = c(colnames(tmp)[1:26], "LOG_CASES", "POP_OFFSET"))
tmp[, MEDIAN := NULL]
quantile_historical_fine_tuned_timegpt_preds_dt <- melt(tmp,
  id.vars = c("PROVINCE", "LOG_CASES", "end_of_month", "POP_OFFSET"),
  variable.name = "quantile", value.name = "prediction"
)
quantile_historical_fine_tuned_timegpt_preds_dt[, quantile := gsub("q_", "", quantile)]
quantile_historical_fine_tuned_timegpt_preds_dt[, quantile := gsub("TimeGPT", 0.5, quantile)]
quantile_historical_fine_tuned_timegpt_preds_dt[, quantile := as.numeric(quantile)]
quantile_historical_fine_tuned_timegpt_preds_dt <- quantile_historical_fine_tuned_timegpt_preds_dt[which(quantile != "TimeGPT")]
setnames(
  quantile_historical_fine_tuned_timegpt_preds_dt, c("PROVINCE", "LOG_CASES", "end_of_month"),
  c("location", "true_value", "target_end_date")
)

setkeyv(quantile_historical_fine_tuned_timegpt_preds_dt, "quantile")
quantile_historical_fine_tuned_timegpt_preds_dt


quantile_historical_fine_tuned_timegpt_preds_dt[, model := "historical_fine_tuned_timegpt"]
# quantile_historical_fine_tuned_timegpt_preds_dt[, prediction:= expm1(prediction)/POP_OFFSET]
# quantile_historical_fine_tuned_timegpt_preds_dt[, true_value:= expm1(true_value)/POP_OFFSET]

## JSB: Fails here
quantile_historical_fine_tuned_timegpt_preds_dt %>%
  score() %>%
  summarise_scores(by = c("model"))
quantile_historical_fine_tuned_timegpt_preds_dt %>%
  score() %>%
  summarise_scores(by = c("model", "range")) %>%
  plot_interval_coverage()

saveRDS(quantile_historical_fine_tuned_timegpt_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_historical_fine_tuned_timegpt_preds_dt.RDS"
  )
)
quantile_historical_fine_tuned_timegpt_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_historical_fine_tuned_timegpt_preds_dt.RDS"
))
quantile_historical_fine_tuned_timegpt_dir_preds_dt <- copy(quantile_historical_fine_tuned_timegpt_preds_dt)
quantile_historical_fine_tuned_timegpt_dir_preds_dt[, prediction := expm1(prediction) / POP_OFFSET]
quantile_historical_fine_tuned_timegpt_dir_preds_dt[, true_value := expm1(true_value) / POP_OFFSET]
quantile_historical_fine_tuned_timegpt_dir_preds_dt[which(prediction < 0), prediction := 0]
quantile_historical_fine_tuned_timegpt_dir_preds_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
quantile_historical_fine_tuned_timegpt_dir_preds_dt
saveRDS(quantile_historical_fine_tuned_timegpt_dir_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_historical_fine_tuned_timegpt_dir_preds_dt.RDS"
  )
)
ggplot(historical_fine_tuned_timegpt_preds_dt) +
  geom_point(aes(x = TimeGPT, y = LOG_CASES, col = "Observed")) +
  facet_wrap(PROVINCE ~ ., )

historical_fine_tuned_timegpt_facet_plots_2018_2021 <-
  ggplot(historical_fine_tuned_timegpt_preds_dt) +
  geom_point(aes(x = YEAR_DECIMAL, y = LOG_CASES, col = "Observed")) +
  geom_line(aes(x = YEAR_DECIMAL, y = TimeGPT, col = "Estimated")) +
  geom_point(aes(x = YEAR_DECIMAL, y = TimeGPT, col = "Estimated")) +
  geom_ribbon(aes(x = YEAR_DECIMAL, ymin = TimeGPT.q.2, ymax = TimeGPT.q.97), alpha = 0.18) +
  theme_bw() +
  xlab("Time") +
  facet_wrap(fct_rev(PROVINCE) ~ ., scales = "free_y", nrow = 5) +
  # facet_wrap(fct_rev(LAT_PROV_IND_FACTOR) ~ ., scales = "free_y", nrow = 5)+
  theme(legend.position = "bottom") +
  theme(
    text = element_text(size = 25),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 25) + geom_text(size = 25),
    legend.position = "bottom",
    legend.title = element_blank()
  )
historical_fine_tuned_timegpt_facet_plots_2018_2021



# Historical No covars FINETUNED TIMEGPT (USE)----
# 95, 90, 85, 80, 75, 70,  65, 60,
log_info("historical no covars finetuned timegpt")
historical_no_covars_fine_tuned_timegpt_preds_dt <- data.table(process_python_deep_preds("historical_no_covars_timegpt"))
# historical_no_covars_fine_tuned_timegpt_preds_dt[, TimeGPT.lo.0 := NULL]  # JSB
tmp_quantiles <- rev.default(quantiles)
tmp_quantiles <- tmp_quantiles[which(tmp_quantiles != 0.5)]
tmp_quantiles[1:11] <- rev.default(tmp_quantiles[1:11])
setnames(
  historical_no_covars_fine_tuned_timegpt_preds_dt,
  colnames(historical_no_covars_fine_tuned_timegpt_preds_dt)[4:26],
  c("MEDIAN", paste0("q_", tmp_quantiles))
)
ptl_province_2010_2018_data <- subset(
  ptl_province_inla_df,
  YEAR < 2018
)
ptl_province_2010_2018_data <- subset(
  ptl_province_2010_2018_data,
  end_of_month >= min(historical_no_covars_fine_tuned_timegpt_preds_dt$end_of_month)
)
historical_no_covars_fine_tuned_timegpt_preds_dt[, X := NULL] # Redundant index column
historical_no_covars_fine_tuned_timegpt_preds_dt[, length(end_of_month), by = "PROVINCE"]
historical_no_covars_fine_tuned_timegpt_preds_dt <-
  merge(historical_no_covars_fine_tuned_timegpt_preds_dt,
    ptl_province_2010_2018_data,
    by = c("PROVINCE", "end_of_month")
  )

historical_no_covars_fine_tuned_timegpt_preds_dt[, caret::R2(TimeGPT, LOG_CASES), by = "PROVINCE"]
historical_no_covars_fine_tuned_timegpt_preds_dt[, caret::MAE(TimeGPT, LOG_CASES), by = "PROVINCE"]
historical_no_covars_fine_tuned_timegpt_preds_dt[, caret::MAE(expm1(TimeGPT) / POP_OFFSET, DIR)]
historical_no_covars_fine_tuned_timegpt_preds_dt[, caret::R2(expm1(TimeGPT) / POP_OFFSET, DIR)]

historical_no_covars_fine_tuned_timegpt_preds_dt[, caret::R2(TimeGPT, LOG_CASES)]
historical_no_covars_fine_tuned_timegpt_preds_dt[, caret::MAE(TimeGPT, LOG_CASES)]

historical_no_covars_fine_tuned_timegpt_preds_dt[, length(which(q_0.025 <= LOG_CASES & q_0.975 >= LOG_CASES)) /
  length(TimeGPT)]

tmp <- copy(historical_no_covars_fine_tuned_timegpt_preds_dt)
colnames(tmp)[1:26]
tmp <- subset(tmp, select = c(colnames(tmp)[1:26], "LOG_CASES", "POP_OFFSET"))
tmp[, MEDIAN := NULL]
quantile_historical_no_covars_fine_tuned_timegpt_preds_dt <- melt(tmp,
  id.vars = c("PROVINCE", "LOG_CASES", "end_of_month", "POP_OFFSET"),
  variable.name = "quantile", value.name = "prediction"
)
quantile_historical_no_covars_fine_tuned_timegpt_preds_dt[, quantile := gsub("q_", "", quantile)]
quantile_historical_no_covars_fine_tuned_timegpt_preds_dt[, quantile := gsub("TimeGPT", 0.5, quantile)]
quantile_historical_no_covars_fine_tuned_timegpt_preds_dt[, quantile := as.numeric(quantile)]
quantile_historical_no_covars_fine_tuned_timegpt_preds_dt <- quantile_historical_no_covars_fine_tuned_timegpt_preds_dt[which(quantile != "TimeGPT")]
setnames(
  quantile_historical_no_covars_fine_tuned_timegpt_preds_dt, c("PROVINCE", "LOG_CASES", "end_of_month"),
  c("location", "true_value", "target_end_date")
)
setkeyv(quantile_historical_no_covars_fine_tuned_timegpt_preds_dt, "quantile")

quantile_historical_no_covars_fine_tuned_timegpt_preds_dt[, model := "historical_no_covars_fine_tuned_timegpt"]
quantile_historical_no_covars_fine_tuned_timegpt_preds_dt %>%
  score() %>%
  summarise_scores(by = c("model"))
quantile_historical_no_covars_fine_tuned_timegpt_preds_dt %>%
  score() %>%
  summarise_scores(by = c("model", "range")) %>%
  plot_interval_coverage()

saveRDS(quantile_historical_no_covars_fine_tuned_timegpt_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_historical_no_covars_fine_tuned_timegpt_preds_dt.RDS"
  )
)
quantile_historical_no_covars_fine_tuned_timegpt_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_historical_no_covars_fine_tuned_timegpt_preds_dt.RDS"
))
quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt <- copy(quantile_historical_no_covars_fine_tuned_timegpt_preds_dt)
quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt[, prediction := expm1(prediction) / POP_OFFSET]
quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt[, true_value := expm1(true_value) / POP_OFFSET]
quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt[which(prediction < 0), prediction := 0]
quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]

saveRDS(quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_historical_no_covars_fine_tuned_timegpt_dir_preds_dt.RDS"
  )
)
ggplot(historical_no_covars_fine_tuned_timegpt_preds_dt) +
  geom_point(aes(x = TimeGPT, y = LOG_CASES, col = "Observed")) +
  facet_wrap(PROVINCE ~ ., )

historical_no_covars_fine_tuned_timegpt_facet_plots_2018_2021 <-
  ggplot(historical_no_covars_fine_tuned_timegpt_preds_dt) +
  geom_point(aes(x = YEAR_DECIMAL, y = LOG_CASES, col = "Observed")) +
  geom_line(aes(x = YEAR_DECIMAL, y = TimeGPT, col = "Estimated")) +
  geom_point(aes(x = YEAR_DECIMAL, y = TimeGPT, col = "Estimated")) +
  geom_ribbon(aes(x = YEAR_DECIMAL, ymin = TimeGPT.q.2, ymax = TimeGPT.q.97), alpha = 0.18) +
  theme_bw() +
  xlab("Time") +
  facet_wrap(fct_rev(PROVINCE) ~ ., scales = "free_y", nrow = 5) +
  # facet_wrap(fct_rev(LAT_PROV_IND_FACTOR) ~ ., scales = "free_y", nrow = 5)+
  theme(legend.position = "bottom") +
  theme(
    text = element_text(size = 25),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 25) + geom_text(size = 25),
    legend.position = "bottom",
    legend.title = element_blank()
  )
historical_no_covars_fine_tuned_timegpt_facet_plots_2018_2021



# HISTORICAL TCN ----
log_info("historical_tcn")
raw_historical_tcn_preds_dt <- process_python_deep_preds("new_historical_tcn")
ptl_province_2010_2018_data <- subset(
  ptl_province_inla_df,
  YEAR < 2018
)
ptl_province_2010_2018_data[, IND := seq(1, length(DIR)), by = "PROVINCE"]
# Only have 42 data points in historical data.table
ptl_province_2010_2018_data <- ptl_province_2010_2018_data[which(IND > (max(IND)) - 43)]
ptl_province_2010_2018_data[, IND := seq(1, length(DIR)), by = "PROVINCE"] # New Index to match forecast data.table
raw_historical_tcn_preds_dt[, IND := rep(
  seq(
    1,
    length(unique(ptl_province_2010_2018_data$TIME))
  ),
  each = 1
),
by = "PROVINCE"
]
head(raw_historical_tcn_preds_dt)

raw_historical_tcn_preds_dt$X <- NULL
# historical_tcn_preds_dt <- melt(raw_historical_tcn_preds_dt,
#                                 id.vars = )
# raw_historical_tcn_preds_dt$prediction <- NULL
# raw_historical_tcn_preds_dt
historical_tcn_preds_dt <- melt(raw_historical_tcn_preds_dt,
  id.vars = c("IND", "PROVINCE"),
  variable.name = "SAMPLE", value.name = "prediction"
)
historical_tcn_preds_dt[, SAMPLE := as.numeric(gsub("X", "", SAMPLE))]
historical_tcn_preds_dt[, SAMPLE := SAMPLE + 1]

historical_tcn_preds_dt <-
  merge(historical_tcn_preds_dt,
    ptl_province_2010_2018_data,
    by = c("PROVINCE", "IND")
  )
historical_tcn_preds_dt[, model := "historical_tcn"]
historical_tcn_preds_dt[, expm1(LOG_CASES) / POP_OFFSET]
historical_tcn_preds_dt[, DIR_prediction := expm1(prediction) / POP_OFFSET]
setnames(historical_tcn_preds_dt, "LOG_CASES", "true_value")
setnames(historical_tcn_preds_dt, "SAMPLE", "sample")
historical_tcn_preds_dt[, location := PROVINCE]
historical_tcn_preds_dt_for_scoring <- subset(historical_tcn_preds_dt,
  select = c(
    "location", "sample",
    "true_value", "prediction",
    "model", "end_of_month"
  )
)
setnames(historical_tcn_preds_dt_for_scoring, "end_of_month", "target_end_date")
check_forecasts(historical_tcn_preds_dt_for_scoring)

historical_tcn_scores_log_cases_2010_2018_dt <- historical_tcn_preds_dt_for_scoring %>%
  score() %>%
  summarise_scores(by = c("model"))

quantile_historical_tcn_preds_dt <-
  sample_to_quantile(historical_tcn_preds_dt_for_scoring,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
quantile_historical_tcn_preds_dt %>%
  score() %>%
  summarise_scores(by = c("model", "range")) %>%
  plot_interval_coverage()
quantile_historical_tcn_preds_dt[which(quantile == 0.5), caret::R2(prediction, true_value), by = "location"]
quantile_historical_tcn_preds_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
quantile_historical_tcn_preds_dt %>%
  score() %>%
  summarise_scores(by = "model")

saveRDS(quantile_historical_tcn_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_historical_tcn_preds_dt.RDS"
  )
)
historical_tcn_dir_preds_dt_for_scoring <-
  copy(historical_tcn_preds_dt)
historical_tcn_preds_dt
historical_tcn_dir_preds_dt_for_scoring[, prediction := expm1(prediction) / POP_OFFSET]
historical_tcn_dir_preds_dt_for_scoring[which(prediction < 0), prediction := 0]
historical_tcn_dir_preds_dt_for_scoring[, true_value := expm1(true_value) / POP_OFFSET]
# setnames(historical_tcn_dir_preds_dt_for_scoring, "SAMPLE", "sample")
historical_tcn_dir_preds_dt_for_scoring[, location := PROVINCE]
historical_tcn_dir_preds_dt_for_scoring <- subset(historical_tcn_dir_preds_dt_for_scoring,
  select = c(
    "location", "sample",
    "true_value", "prediction",
    "model", "end_of_month"
  )
)
check_forecasts(historical_tcn_dir_preds_dt_for_scoring)

historical_tcn_scores_dir_2010_2018_dt <- historical_tcn_dir_preds_dt_for_scoring %>%
  score() %>%
  summarise_scores(by = c("model"))
historical_tcn_scores_dir_2010_2018_dt

quantile_historical_tcn_dir_preds_dt <-
  sample_to_quantile(historical_tcn_dir_preds_dt_for_scoring,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
process_quantile_predictions(quantile_historical_tcn_dir_preds_dt)
quantile_historical_tcn_dir_preds_dt %>%
  score() %>%
  summarise_scores(by = "model")
saveRDS(quantile_historical_tcn_dir_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_historical_tcn_dir_preds_dt.RDS"
  )
)









# Testing Period (2018 - 2021) ----
# NO COVARS TIMEGPT (USE) ----
log_ingo("testing period 2018-2021, no covars timegpt")
finetuned_no_covars_timegpt_preds_dt <- data.table(process_python_deep_preds("finetuned_no_covars_timegpt"))
finetuned_no_covars_timegpt_preds_dt
finetuned_no_covars_timegpt_preds_dt[, IND := seq(1, length(TimeGPT)), by = "PROVINCE"]
finetuned_no_covars_timegpt_preds_dt[, X := NULL] # Redundant index column
finetuned_no_covars_timegpt_preds_dt[, length(end_of_month), by = "PROVINCE"]

finetuned_no_covars_timegpt_preds_dt <-
  merge(finetuned_no_covars_timegpt_preds_dt,
    ptl_province_2018_2021_data,
    by = c("PROVINCE", "IND", "end_of_month")
  )
finetuned_no_covars_timegpt_preds_dt
finetuned_no_covars_timegpt_preds_dt[, caret::R2(TimeGPT, LOG_CASES), by = "PROVINCE"]
finetuned_no_covars_timegpt_preds_dt[, caret::MAE(TimeGPT, LOG_CASES), by = "PROVINCE"]
finetuned_no_covars_timegpt_preds_dt[, caret::R2(TimeGPT, LOG_CASES)]
finetuned_no_covars_timegpt_preds_dt[, caret::MAE(TimeGPT, LOG_CASES)]

finetuned_no_covars_timegpt_preds_dt[, length(which(TimeGPT.q.2 <= LOG_CASES & TimeGPT.q.97 >= LOG_CASES)) /
  length(TimeGPT)]

finetuned_no_covars_timegpt_preds_dt
tmp <- copy(finetuned_no_covars_timegpt_preds_dt)
colnames(tmp)[1:27]
tmp <- subset(tmp, select = c(colnames(tmp)[1:30], "LOG_CASES", "POP_OFFSET"))
quantile_finetuned_no_covars_timegpt_preds_dt <- melt(tmp,
  id.vars = c(
    "PROVINCE", "IND", "TIME",
    "MONTH", "YEAR", "LOG_CASES", "end_of_month",
    "POP_OFFSET"
  ),
  variable.name = "quantile", value.name = "prediction"
)
quantile_finetuned_no_covars_timegpt_preds_dt <- quantile_finetuned_no_covars_timegpt_preds_dt[which(quantile != "TimeGPT")]
setnames(
  quantile_finetuned_no_covars_timegpt_preds_dt, c("PROVINCE", "LOG_CASES", "end_of_month"),
  c("location", "true_value", "target_end_date")
)

setkeyv(quantile_finetuned_no_covars_timegpt_preds_dt, "quantile")
num_repeats_for_each_quantile <- unique(quantile_finetuned_no_covars_timegpt_preds_dt[, length(IND), by = "quantile"]$V1)
num_repeats_for_each_quantile

quantile_finetuned_no_covars_timegpt_preds_dt[, quantile := rep(c(
  0.01, 0.025,
  seq(0.05, 0.95, 0.05),
  0.975, 0.99
), each = num_repeats_for_each_quantile)]
quantile_finetuned_no_covars_timegpt_preds_dt[, model := "finetuned_no_covars_timegpt"]
quantile_finetuned_no_covars_timegpt_preds_dt

saveRDS(quantile_finetuned_no_covars_timegpt_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_finetuned_no_covars_timegpt_preds_dt.RDS"
  )
)
quantile_fine_tuned_no_covars_timegpt_dir_preds_dt <- copy(quantile_finetuned_no_covars_timegpt_preds_dt)
quantile_fine_tuned_no_covars_timegpt_dir_preds_dt <- merge(quantile_fine_tuned_no_covars_timegpt_dir_preds_dt,
  subset(ptl_province_2018_2021_data, select = c("PROVINCE", "end_of_month", "POP_OFFSET")),
  by.x = c("location", "target_end_date"),
  by.y = c("PROVINCE", "end_of_month")
)

quantile_fine_tuned_no_covars_timegpt_dir_preds_dt[, prediction := expm1(prediction) / POP_OFFSET]
quantile_fine_tuned_no_covars_timegpt_dir_preds_dt[, true_value := expm1(true_value) / POP_OFFSET]
quantile_fine_tuned_no_covars_timegpt_dir_preds_dt[which(prediction < 0), prediction := 0]
quantile_fine_tuned_no_covars_timegpt_dir_preds_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]

saveRDS(quantile_fine_tuned_no_covars_timegpt_dir_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_fine_tuned_no_covars_timegpt_dir_preds_dt.RDS"
  )
)

# quantile_finetuned_no_covars_timegpt_preds_dt[, prediction:= expm1(prediction)/POP_OFFSET]
# quantile_finetuned_no_covars_timegpt_preds_dt[, true_value:= expm1(true_value)/POP_OFFSET]
quantile_finetuned_no_covars_timegpt_preds_dt %>%
  score() %>%
  summarise_scores(by = c("model"))



# FINETUNED TIMEGPT (USE)----
log_info("finetuned timegpt")
fine_tuned_timegpt_preds_dt <- data.table(process_python_deep_preds("finetuned_timegpt"))
fine_tuned_timegpt_preds_dt[, IND := seq(1, length(TimeGPT)), by = "PROVINCE"]
ptl_province_2018_2021_data <- subset(
  ptl_province_inla_df,
  YEAR >= 2018
)
ptl_province_2018_2021_data[, IND := seq(1, length(DIR)), by = "PROVINCE"]
fine_tuned_timegpt_preds_dt[, X := NULL] # Redundant index column
fine_tuned_timegpt_preds_dt[, length(end_of_month), by = "PROVINCE"]

fine_tuned_timegpt_preds_dt <-
  merge(fine_tuned_timegpt_preds_dt,
    ptl_province_2018_2021_data,
    by = c("PROVINCE", "IND", "end_of_month")
  )
fine_tuned_timegpt_preds_dt[, caret::R2(TimeGPT, LOG_CASES), by = "PROVINCE"]
fine_tuned_timegpt_preds_dt[, caret::MAE(TimeGPT, LOG_CASES), by = "PROVINCE"]
fine_tuned_timegpt_preds_dt[, caret::MAE(expm1(TimeGPT) / POP_OFFSET, DIR)]
fine_tuned_timegpt_preds_dt[, caret::R2(expm1(TimeGPT) / POP_OFFSET, DIR)]

fine_tuned_timegpt_preds_dt[, caret::R2(TimeGPT, LOG_CASES)]
fine_tuned_timegpt_preds_dt[, caret::MAE(TimeGPT, LOG_CASES)]

fine_tuned_timegpt_preds_dt[, length(which(TimeGPT.q.2 <= LOG_CASES & TimeGPT.q.97 >= LOG_CASES)) /
  length(TimeGPT)]

tmp <- copy(fine_tuned_timegpt_preds_dt)
colnames(tmp)[1:27]
tmp <- subset(tmp, select = c(colnames(tmp)[1:30], "LOG_CASES", "POP_OFFSET"))
quantile_fine_tuned_timegpt_preds_dt <- melt(tmp,
  id.vars = c(
    "PROVINCE", "IND", "TIME",
    "MONTH", "YEAR", "LOG_CASES", "end_of_month", "POP_OFFSET"
  ),
  variable.name = "quantile", value.name = "prediction"
)
quantile_fine_tuned_timegpt_preds_dt <- quantile_fine_tuned_timegpt_preds_dt[which(quantile != "TimeGPT")]
setnames(
  quantile_fine_tuned_timegpt_preds_dt, c("PROVINCE", "LOG_CASES", "end_of_month"),
  c("location", "true_value", "target_end_date")
)

setkeyv(quantile_fine_tuned_timegpt_preds_dt, "quantile")
num_repeats_for_each_quantile <- unique(quantile_fine_tuned_timegpt_preds_dt[, length(IND), by = "quantile"]$V1)
num_repeats_for_each_quantile

quantile_fine_tuned_timegpt_preds_dt[, quantile := rep(c(
  0.01, 0.025,
  seq(0.05, 0.95, 0.05),
  0.975, 0.99
), each = num_repeats_for_each_quantile)]
quantile_fine_tuned_timegpt_preds_dt[, model := "fine_tuned_timegpt"]
quantile_fine_tuned_timegpt_preds_dt %>%
  score() %>%
  summarise_scores(by = c("model"))
quantile_fine_tuned_timegpt_preds_dt %>%
  score() %>%
  summarise_scores(by = c("model", "range")) %>%
  plot_interval_coverage()

saveRDS(quantile_fine_tuned_timegpt_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_fine_tuned_timegpt_preds_dt.RDS"
  )
)
quantile_fine_tuned_timegpt_preds_dt <- readRDS(file = file.path(
  peru.province.python.out.dir,
  "quantile_fine_tuned_timegpt_preds_dt.RDS"
))
quantile_fine_tuned_timegpt_dir_preds_dt <- copy(quantile_fine_tuned_timegpt_preds_dt)
quantile_fine_tuned_timegpt_dir_preds_dt[, prediction := expm1(prediction) / POP_OFFSET]
quantile_fine_tuned_timegpt_dir_preds_dt[, true_value := expm1(true_value) / POP_OFFSET]
quantile_fine_tuned_timegpt_dir_preds_dt[which(prediction < 0), prediction := 0]
quantile_fine_tuned_timegpt_dir_preds_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]

saveRDS(quantile_fine_tuned_timegpt_dir_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_fine_tuned_timegpt_dir_preds_dt.RDS"
  )
)
ggplot(fine_tuned_timegpt_preds_dt) +
  geom_point(aes(x = TimeGPT, y = LOG_CASES, col = "Observed")) +
  facet_wrap(PROVINCE ~ ., )

fine_tuned_timegpt_facet_plots_2018_2021 <-
  ggplot(fine_tuned_timegpt_preds_dt) +
  geom_point(aes(x = YEAR_DECIMAL, y = LOG_CASES, col = "Observed")) +
  geom_line(aes(x = YEAR_DECIMAL, y = TimeGPT, col = "Estimated")) +
  geom_point(aes(x = YEAR_DECIMAL, y = TimeGPT, col = "Estimated")) +
  geom_ribbon(aes(x = YEAR_DECIMAL, ymin = TimeGPT.q.2, ymax = TimeGPT.q.97), alpha = 0.18) +
  theme_bw() +
  xlab("Time") +
  facet_wrap(fct_rev(PROVINCE) ~ ., scales = "free_y", nrow = 5) +
  # facet_wrap(fct_rev(LAT_PROV_IND_FACTOR) ~ ., scales = "free_y", nrow = 5)+
  theme(legend.position = "bottom") +
  theme(
    text = element_text(size = 25),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 25) + geom_text(size = 25),
    legend.position = "bottom",
    legend.title = element_blank()
  )
fine_tuned_timegpt_facet_plots_2018_2021



# DEEPTCN (USE) ----
log_info("deeptcn")
raw_tcn_preds_dt <- process_python_deep_preds("tcn")
ptl_province_2018_2021_data <- subset(
  ptl_province_inla_df,
  YEAR >= 2018
)
ptl_province_2018_2021_data[, IND := seq(1, length(DIR)), by = "PROVINCE"]
raw_tcn_preds_dt
raw_tcn_preds_dt[, X := X + 1] # Sample index column
setnames(raw_tcn_preds_dt, "X", "SAMPLE")
setnames(raw_tcn_preds_dt, "X0", "prediction")

raw_tcn_preds_dt[, length(SAMPLE), by = "PROVINCE"]
# raw_tcn_preds_dt <- raw_tcn_preds_dt[which(PROVINCE != "Chiclayo")]
raw_tcn_preds_dt[, IND := rep(
  seq(
    1,
    length(unique(ptl_province_2018_2021_data$TIME))
  ),
  each = 2500
),
by = "PROVINCE"
]
raw_tcn_preds_dt
raw_tcn_preds_dt <-
  merge(raw_tcn_preds_dt,
    ptl_province_2018_2021_data,
    by = c("PROVINCE", "IND")
  )
raw_tcn_preds_dt[, model := "tcn"]
raw_tcn_preds_dt[, expm1(LOG_CASES) / POP_OFFSET]
tmp <- expm1(raw_tcn_preds_dt$prediction)
raw_tcn_preds_dt[, DIR_prediction := expm1(prediction) / POP_OFFSET]
raw_tcn_preds_dt[which(DIR_prediction < 0)]
setnames(raw_tcn_preds_dt, "LOG_CASES", "true_value")
setnames(raw_tcn_preds_dt, "SAMPLE", "sample")
raw_tcn_preds_dt[, location := PROVINCE]
raw_tcn_preds_dt_for_scoring <- subset(raw_tcn_preds_dt,
  select = c(
    "location", "sample",
    "true_value", "prediction",
    "model", "end_of_month"
  )
)
setnames(raw_tcn_preds_dt_for_scoring, "end_of_month", "target_end_date")
check_forecasts(raw_tcn_preds_dt_for_scoring)

tcn_scores_log_cases_2018_2021_dt <- raw_tcn_preds_dt_for_scoring %>%
  score() %>%
  summarise_scores(by = c("model"))

tcn_scores_log_cases_2018_2021_dt
quantile_tcn_preds_dt <-
  sample_to_quantile(raw_tcn_preds_dt_for_scoring,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
quantile_tcn_preds_dt %>%
  score() %>%
  summarise_scores(by = c("model", "range")) %>%
  plot_interval_coverage()

saveRDS(quantile_tcn_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_tcn_preds_dt.RDS"
  )
)

# Set up data.table with DIR predictions
dir_tcn_preds_dt <- copy(raw_tcn_preds_dt)
dir_tcn_preds_dt_for_scoring <- subset(dir_tcn_preds_dt,
  select = c(
    "location", "sample",
    "DIR", "DIR_prediction",
    "model", "end_of_month"
  )
)
setnames(dir_tcn_preds_dt_for_scoring, "DIR", "true_value")
setnames(dir_tcn_preds_dt_for_scoring, "DIR_prediction", "prediction")
setnames(dir_tcn_preds_dt_for_scoring, "end_of_month", "target_end_date")

saveRDS(dir_tcn_preds_dt_for_scoring,
  file = file.path(
    peru.province.python.out.dir,
    "dir_tcn_preds_dt_for_scoring.RDS"
  )
)
dir_tcn_preds_dt_for_scoring
tcn_scores_dir_2018_2021_dt <- dir_tcn_preds_dt_for_scoring %>%
  score() %>%
  summarise_scores(by = c("model"))
tcn_scores_dir_2018_2021_dt

quantile_tcn_dir_preds_dt <-
  sample_to_quantile(dir_tcn_preds_dt_for_scoring,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
quantile_tcn_dir_preds_dt[which(quantile == 0.5), caret::R2(true_value, prediction)]
saveRDS(quantile_tcn_dir_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_tcn_dir_preds_dt.RDS"
  )
)

# Summarise
log_info("summarise")
summary_tcn_preds_dt <-
  raw_tcn_preds_dt[, list(
    CI_L = quantile(prediction, probs = 0.025, na.rm = TRUE),
    CI_U = quantile(prediction, probs = 0.975, na.rm = TRUE),
    MEDIAN = median(prediction, na.rm = TRUE),
    MEAN = mean(prediction, na.rm = TRUE),
    true_value = unique(true_value),
    true_DIR = unique(DIR),
    DIR_CI_L = quantile(DIR_prediction, probs = 0.025, na.rm = TRUE),
    DIR_CI_U = quantile(DIR_prediction, probs = 0.975, na.rm = TRUE),
    DIR_MEDIAN = quantile(DIR_prediction, 0.5),
    OUTBREAK_PROB_50 = length(which(DIR_prediction >= 50 &
      !is.na(prediction))) / length(which(!is.na(prediction))),
    OUTBREAK_PROB_150 = length(which(DIR_prediction >= 150 &
      !is.na(prediction))) / length(which(!is.na(prediction)))
  ),
  by = c(
    "PROVINCE", "TIME",
    "YEAR", "model", "YEAR_DECIMAL"
  )
  ]

summary_tcn_preds_dt[, length(which(true_DIR <= DIR_CI_U & true_DIR >= DIR_CI_L)) / length(true_DIR)]

summary_tcn_preds_dt[, length(which(true_value <= CI_U & true_value >= CI_L)) / length(true_value)]
summary_tcn_preds_dt[, caret::R2(true_value, MEDIAN), by = "PROVINCE"]
summary_tcn_preds_dt[, caret::R2(true_value, MEDIAN)]

summary_tcn_preds_dt[, caret::MAE(true_value, MEDIAN), by = "PROVINCE"]
summary_tcn_preds_dt[, caret::R2(true_DIR, DIR_MEDIAN), by = "PROVINCE"]
summary_tcn_preds_dt[, caret::MAE(true_DIR, DIR_MEDIAN), by = "PROVINCE"]
summary_tcn_preds_dt[, caret::MAE(true_DIR, DIR_MEDIAN)]
summary_tcn_preds_dt[, caret::R2(true_DIR, DIR_MEDIAN)]

ggplot(summary_tcn_preds_dt) +
  geom_point(aes(x = TIME, y = DIR_MEDIAN), color = "Prediction") +
  geom_point(aes(x = TIME, y = true_DIR), color = "")


tcn_facet_plots_2018_2021 <-
  ggplot(summary_tcn_preds_dt) +
  geom_point(aes(x = YEAR_DECIMAL, y = true_DIR, col = "Observed")) +
  geom_line(aes(x = YEAR_DECIMAL, y = DIR_MEDIAN, col = "Estimated")) +
  geom_point(aes(x = YEAR_DECIMAL, y = DIR_MEDIAN, col = "Estimated")) +
  geom_ribbon(aes(x = YEAR_DECIMAL, ymin = DIR_CI_L, ymax = DIR_CI_U), alpha = 0.18) +
  theme_bw() +
  xlab("Time") +
  facet_wrap(fct_rev(PROVINCE) ~ ., scales = "free_y", nrow = 5) +
  # facet_wrap(fct_rev(LAT_PROV_IND_FACTOR) ~ ., scales = "free_y", nrow = 5)+
  theme(legend.position = "bottom") +
  theme(
    text = element_text(size = 25),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 25) + geom_text(size = 25),
    legend.position = "bottom",
    legend.title = element_blank()
  )
tcn_facet_plots_2018_2021



# SARIMA (USE) ----
log_info("sarima")
raw_sarima_preds_dt <- process_python_deep_preds("arima")
ptl_province_2018_2021_data <- subset(
  ptl_province_inla_df,
  YEAR >= 2018
)
ptl_province_2018_2021_data[, IND := seq(1, length(DIR)), by = "PROVINCE"]
raw_sarima_preds_dt
raw_sarima_preds_dt[, X := X + 1] # Sample index column
setnames(raw_sarima_preds_dt, "X", "SAMPLE")
setnames(raw_sarima_preds_dt, "X0", "prediction")

raw_sarima_preds_dt[, length(SAMPLE), by = "PROVINCE"]
# raw_sarima_preds_dt <- raw_sarima_preds_dt[which(PROVINCE != "Chiclayo")]
raw_sarima_preds_dt[, IND := rep(
  seq(
    1,
    length(unique(ptl_province_2018_2021_data$TIME))
  ),
  each = 2500
),
by = "PROVINCE"
]
raw_sarima_preds_dt
raw_sarima_preds_dt <-
  merge(raw_sarima_preds_dt,
    ptl_province_2018_2021_data,
    by = c("PROVINCE", "IND")
  )
raw_sarima_preds_dt[, model := "sarima"]
raw_sarima_preds_dt[, expm1(LOG_CASES) / POP_OFFSET]
raw_sarima_preds_dt[, DIR_prediction := expm1(prediction) / POP_OFFSET]
setnames(raw_sarima_preds_dt, "LOG_CASES", "true_value")
setnames(raw_sarima_preds_dt, "SAMPLE", "sample")
raw_sarima_preds_dt[, location := PROVINCE]
raw_sarima_preds_dt_for_scoring <- subset(raw_sarima_preds_dt,
  select = c(
    "location", "sample",
    "true_value", "prediction",
    "model", "end_of_month"
  )
)
setnames(raw_sarima_preds_dt_for_scoring, "end_of_month", "target_end_date")
check_forecasts(raw_sarima_preds_dt_for_scoring)

sarima_scores_log_cases_2018_2021_dt <- raw_sarima_preds_dt_for_scoring %>%
  score() %>%
  summarise_scores(by = c("model"))

sarima_scores_log_cases_2018_2021_dt

quantile_sarima_preds_dt <-
  sample_to_quantile(raw_sarima_preds_dt_for_scoring,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
quantile_sarima_preds_dt %>%
  score() %>%
  summarise_scores(by = c("model", "range")) %>%
  plot_interval_coverage()
quantile_sarima_preds_dt %>%
  score() %>%
  summarise_scores(by = c("model"))
quantile_sarima_preds_dt
saveRDS(quantile_sarima_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_sarima_preds_dt.RDS"
  )
)
gc()
# Set up data.table with DIR predictions
dir_sarima_preds_dt <- copy(raw_sarima_preds_dt)
dir_sarima_preds_dt_for_scoring <- subset(dir_sarima_preds_dt,
  select = c(
    "location", "sample",
    "DIR", "DIR_prediction",
    "model", "end_of_month"
  )
)
setnames(dir_sarima_preds_dt_for_scoring, "DIR", "true_value")
setnames(dir_sarima_preds_dt_for_scoring, "DIR_prediction", "prediction")
setnames(dir_sarima_preds_dt_for_scoring, "end_of_month", "target_end_date")
saveRDS(dir_sarima_preds_dt_for_scoring,
  file = file.path(
    peru.province.python.out.dir,
    "dir_sarima_preds_dt_for_scoring.RDS"
  )
)
sarima_scores_dir_2018_2021_dt <- dir_sarima_preds_dt_for_scoring %>%
  score() %>%
  summarise_scores(by = c("model"))
quantile_sarima_dir_preds_dt <-
  sample_to_quantile(dir_sarima_preds_dt_for_scoring,
    quantiles = c(
      0.01, 0.025,
      seq(0.05, 0.95, 0.05),
      0.975, 0.99
    )
  )
quantile_sarima_dir_preds_dt[which(prediction < 0), prediction := 0]
quantile_sarima_dir_preds_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
saveRDS(quantile_sarima_dir_preds_dt,
  file = file.path(
    peru.province.python.out.dir,
    "quantile_sarima_dir_preds_dt.RDS"
  )
)



# Summarise
log_info("summarise")
summary_sarima_preds_dt <-
  raw_sarima_preds_dt[, list(
    CI_L = quantile(prediction, probs = 0.025, na.rm = TRUE),
    CI_U = quantile(prediction, probs = 0.975, na.rm = TRUE),
    MEDIAN = median(prediction, na.rm = TRUE),
    MEAN = mean(prediction, na.rm = TRUE),
    true_value = unique(true_value),
    true_DIR = unique(DIR),
    DIR_CI_L = quantile(DIR_prediction, probs = 0.025, na.rm = TRUE),
    DIR_CI_U = quantile(DIR_prediction, probs = 0.975, na.rm = TRUE),
    DIR_MEDIAN = median(DIR_prediction),
    OUTBREAK_PROB_50 = length(which(DIR_prediction >= 50 &
      !is.na(prediction))) / length(which(!is.na(prediction))),
    OUTBREAK_PROB_150 = length(which(DIR_prediction >= 150 &
      !is.na(prediction))) / length(which(!is.na(prediction)))
  ),
  by = c(
    "PROVINCE", "TIME",
    "YEAR", "model", "YEAR_DECIMAL"
  )
  ]

summary_sarima_preds_dt[, length(which(true_DIR <= DIR_CI_U & true_DIR >= DIR_CI_L)) / length(true_DIR)]

summary_sarima_preds_dt[, length(which(true_value <= CI_U & true_value >= CI_L)) / length(true_value)]
summary_sarima_preds_dt[, caret::R2(true_value, MEDIAN), by = "PROVINCE"]
summary_sarima_preds_dt[, caret::R2(true_value, MEDIAN)]

summary_sarima_preds_dt[, caret::MAE(true_value, MEDIAN), by = "PROVINCE"]
summary_sarima_preds_dt[, caret::R2(true_DIR, DIR_MEDIAN), by = "PROVINCE"]
summary_sarima_preds_dt[, caret::MAE(true_DIR, DIR_MEDIAN), by = "PROVINCE"]
summary_sarima_preds_dt[, caret::MAE(true_DIR, DIR_MEDIAN)]
summary_sarima_preds_dt[, caret::R2(true_DIR, DIR_MEDIAN)]

ggplot(summary_sarima_preds_dt) +
  geom_point(aes(x = TIME, y = DIR_MEDIAN), color = "Prediction") +
  geom_point(aes(x = TIME, y = true_DIR), color = "")


sarima_facet_plots_2018_2021 <-
  ggplot(summary_sarima_preds_dt) +
  geom_point(aes(x = YEAR_DECIMAL, y = true_DIR, col = "Observed")) +
  geom_line(aes(x = YEAR_DECIMAL, y = DIR_MEDIAN, col = "Estimated")) +
  geom_point(aes(x = YEAR_DECIMAL, y = DIR_MEDIAN, col = "Estimated")) +
  geom_ribbon(aes(x = YEAR_DECIMAL, ymin = DIR_CI_L, ymax = DIR_CI_U), alpha = 0.18) +
  theme_bw() +
  xlab("Time") +
  facet_wrap(fct_rev(PROVINCE) ~ ., scales = "free_y", nrow = 5) +
  # facet_wrap(fct_rev(LAT_PROV_IND_FACTOR) ~ ., scales = "free_y", nrow = 5)+
  theme(legend.position = "bottom") +
  theme(
    text = element_text(size = 25),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 25) + geom_text(size = 25),
    legend.position = "bottom",
    legend.title = element_blank()
  )
sarima_facet_plots_2018_2021

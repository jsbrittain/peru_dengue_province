#!/usr/bin/env python
# This script launches jobs in a thread pool

import os
import logging
import numpy as np
import pandas as pd

from pathlib import Path
from darts import TimeSeries
from darts.models import StatsForecastAutoARIMA
from concurrent.futures import ProcessPoolExecutor, wait
from darts.utils.timeseries_generation import datetime_attribute_timeseries

# Defaults
FORCE_RERUN = os.environ.get("FORCE_RERUN", "false").lower() == "true"
HORIZON_MONTHS = int(os.environ.get("HORIZON_MONTHS", "48"))
RETRAIN_MODELS = os.environ.get("RETRAIN_MODELS", "true").lower() == "true"
MAX_WORKERS = int(os.environ.get("MAX_WORKERS", os.cpu_count()))

logging.basicConfig(level=logging.INFO)

data_dir = "/app/data/python/data/"
output_dir = "/app/data/python/output/"

Path(output_dir).mkdir(parents=True, exist_ok=True)

timeseries_df = pd.read_csv("/app/data/python/data/ptl_province_inla_df.csv")
timeseries_df = timeseries_df.fillna(0)

climate_df = pd.read_csv("/app/data/output/climate_dt_province.csv")
climate_df = climate_df.fillna(0)

# Required columns
base_cols = ["PROVINCE", "MONTH", "YEAR", "end_of_month", "CASES"]
case_cols = ["LOG_CASES"]  # must be univariate (only one column)
covariate_cols = ["LAG_1_LOG_CASES", "LAG_1_tmin_roll_2", "LAG_1_prec_roll_2"]

# --- Minimal required dataset ---------------------------------------------------------

timeseries_df = timeseries_df[base_cols]
timeseries_df["LOG_CASES"] = np.log1p(timeseries_df["CASES"])
timeseries_df["LAG_1_LOG_CASES"] = timeseries_df.groupby("PROVINCE")[
    "LOG_CASES"
].shift(1)

# Merge climate data
timeseries_df = timeseries_df.merge(
    climate_df, on=["PROVINCE", "YEAR", "MONTH"], how="left"
)
timeseries_df["LAG_1_tmin_roll_2"] = (
    timeseries_df.groupby("PROVINCE")["tmin_roll_2"].shift(1)
)
timeseries_df["LAG_1_prec_roll_2"] = (
    timeseries_df.groupby("PROVINCE")["prec_roll_2"].shift(1)
)

# --------------------------------------------------------------------------------------

province_names = timeseries_df["PROVINCE"].unique()
province_count = len(province_names)


# Historical SARIMA forecasting
def historical_sarima(i, timeseries_df):
    fname = f"/app/data/python/output/historical_sarima_dataframe_{i}.csv"
    if not FORCE_RERUN and Path(fname).exists():
        logging.info(f"Historical SARIMA {i} file already exists, skipping...")
        return
    logging.info(f"Processing province {i + 1} out of {province_count}")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    start_idx = subset_df.shape[0] - HORIZON_MONTHS
    subset_df = subset_df[:start_idx]

    target_series = [
        TimeSeries.from_dataframe(
            subset_df, time_col="end_of_month", value_cols=case_cols
        )
    ]
    series = target_series[0][:start_idx]

    model = StatsForecastAutoARIMA(season_length=12)
    model.fit(series)

    # Forecast the next 1 month (on scaled covariates)
    sarima_backtest = model.historical_forecasts(
        series=series, forecast_horizon=1, num_samples=2500, verbose=True
    )
    tmp = TimeSeries.all_values(sarima_backtest)
    tmp = tmp.reshape(
        82, 2500
    )  # Remove hard-coded 82 = number of predictions of backtest, 2500 = number of samples
    sarima_df = pd.DataFrame(tmp)
    sarima_df.to_csv(fname)


# Testing Window SARIMA


def window_sarima(i, timeseries_df):
    fname = f"/app/data/python/output/arima_dataframe_{i}.csv"
    if not FORCE_RERUN and Path(fname).exists():
        logging.info(f"Tesing window SARIMA {i} already exists, skipping...")
        return
    logging.info(f"Processing province {i + 1} out of {province_count}")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    target_series = [
        TimeSeries.from_dataframe(
            subset_df, time_col="end_of_month", value_cols=case_cols
        )
    ]
    start_idx = subset_df.shape[0] - HORIZON_MONTHS
    arima_predictions = []
    for j in range(HORIZON_MONTHS):
        logging.info(f"Processing month {j + 1} out of {HORIZON_MONTHS}")
        series = target_series[0][: start_idx + j]
        future_cov = datetime_attribute_timeseries(
            series, "month", cyclic=True, add_length=6
        )
        model = StatsForecastAutoARIMA(season_length=12)
        model.fit(series, future_covariates=future_cov)

        # Forecast the next 1 month (on scaled covariates)
        pred = model.predict(n=1, future_covariates=future_cov, num_samples=2500)

        tmp = TimeSeries.all_values(pred[0])
        tmp = tmp.reshape(2500, 1)
        df = pd.DataFrame(tmp)
        arima_predictions.insert(j, df)
    arima_df = arima_predictions[0]
    for k in range(1, len(arima_predictions), 1):
        df = arima_predictions[k]
        arima_df = pd.concat([arima_df, df])
    arima_df.to_csv(fname)


# Submit all jobs to the thread pool

with ProcessPoolExecutor(max_workers=MAX_WORKERS) as executor:
    logging.info(f"Available workers: {executor._max_workers}")
    logging.info(f"Launching {2*province_count} jobs.")
    futures = []
    for i in range(province_count):
        futures.append(executor.submit(historical_sarima, i, timeseries_df))
        futures.append(executor.submit(window_sarima, i, timeseries_df))
    wait(futures)

logging.info("Finished SARIMA.")

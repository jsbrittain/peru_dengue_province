#!/usr/bin/env python

import os
import logging
import numpy as np
import pandas as pd

from darts import TimeSeries
from pathlib import Path
from darts.models import TCNModel
from concurrent.futures import ProcessPoolExecutor, wait
from darts.utils.likelihood_models import QuantileRegression
from darts.dataprocessing.transformers import Scaler

# Defaults
FORCE_RERUN = os.environ.get("FORCE_RERUN", "false").lower() == "true"
HORIZON_MONTHS = int(os.environ.get("HORIZON_MONTHS", "48"))
RETRAIN_MODELS = os.environ.get("RETRAIN_MODELS", "true").lower() == "true"

logging.basicConfig(level=logging.INFO)

data_dir = "/app/data/python/data/"
output_dir = "/app/data/python/output/"

Path(output_dir).mkdir(parents=True, exist_ok=True)

# --- Load data ------------------------------------------------------------------------

timeseries_df = pd.read_csv("/app/data/python/data/ptl_province_inla_df.csv")
climate_df = pd.read_csv("/app/data/output/climate_dt_province.csv")

# Isolate required columns 
base_cols = ["PROVINCE", "MONTH", "YEAR", "end_of_month", "CASES"]

# These vectors are used for the TCN model
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

timeseries_df['end_of_month'] = pd.to_datetime(timeseries_df['end_of_month'])
timeseries_df.fillna(0, inplace=True)  # Fill any remaining NaNs with 0

# --------------------------------------------------------------------------------------

province_names = timeseries_df["PROVINCE"].unique()
province_count = len(province_names)

# If needed for working with standardised/scaled variables
scaler_covars = Scaler()

# ## 1) TCN MODEL Setup

tcn_model_name = "TCN_Model"
tcn_model = TCNModel(
    input_chunk_length=HORIZON_MONTHS,  # Use 48 months of history to predict the next month
    output_chunk_length=1,  # Forecast 1 step ahead
    kernel_size=3,
    num_filters=4,
    random_state=42,
    model_name=tcn_model_name,
    likelihood=QuantileRegression(),
    save_checkpoints=True,
    force_reset=True,
    n_epochs=150,
    optimizer_kwargs={"lr": 1e-4},
)


# TCN historical forecasting
def historical_tcn(i, timeseries_df):
    fname = f"/app/data/python/output/historical_tcn_dataframe_{i}.csv"
    if not FORCE_RERUN and Path(fname).exists():
        logging.info(f"Historical forecasting TCN '{fname}' already exist, skipping...")
        return
    logging.info(f"Processing province {i + 1} out of {len(province_names)}")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    subset_df = subset_df[subset_df["YEAR"] < 2018]

    # DEEP TCN
    target_series = (
        TimeSeries.from_dataframe(
            subset_df, time_col="end_of_month", value_cols=case_cols
        )
    )
    covariate_series = [
        TimeSeries.from_dataframe(
            subset_df, time_col="end_of_month", value_cols=covariate_cols
        )
    ]
    covariates_scaled = scaler_covars.fit_transform(covariate_series)

    tcn_model.fit(
        series=target_series,
        past_covariates=covariates_scaled,
        epochs=150,
        verbose=False,
    )
    tcn_backtest = tcn_model.historical_forecasts(
        series=target_series,
        past_covariates=covariates_scaled,
        forecast_horizon=1,
        num_samples=2500,
        retrain=RETRAIN_MODELS,
        verbose=True,
    )
    tmp = TimeSeries.all_values(tcn_backtest)
    tmp = tmp.reshape(-1, 2500)  # (num_forecasts x 1<univariate> x 2500 = num_samples)
    tcn_df = pd.DataFrame(tmp)

    tcn_df.to_csv(fname)


# Testing Window TCN


def window_tcn(i, timeseries_df):
    fname = f"/app/data/python/output/tcn_dataframe_{i}.csv"
    if not FORCE_RERUN and Path(fname).exists():
        logging.info(f"Testing window {i} exists, skipping...")
        return
    logging.info(f"Processing province {i + 1} out of {len(province_names)}")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    target_series = [
        TimeSeries.from_dataframe(
            subset_df, time_col="end_of_month", value_cols=case_cols
        )
    ]
    covariate_series = [
        TimeSeries.from_dataframe(
            subset_df, time_col="end_of_month", value_cols=covariate_cols
        )
    ]

    tcn_predictions = []
    start_idx = (
        subset_df.shape[0] - HORIZON_MONTHS
    )  # Starting point for the 48-month horizon
    for j in range(HORIZON_MONTHS):
        logging.info(f"Processing month {j + 1} out of {HORIZON_MONTHS}")
        tmp = TimeSeries.all_values(target_series[0][: start_idx + j + 1])

        covariates_scaled = scaler_covars.fit_transform(
            covariate_series[: start_idx + j + 1]
        )

        tcn_model_name = "TCN_Model"
        tcn_model = TCNModel(
            input_chunk_length=HORIZON_MONTHS,  # Use 48 months of history to predict the next month
            output_chunk_length=1,  # Forecast 1 step ahead
            kernel_size=3,
            num_filters=4,
            random_state=42,
            model_name=tcn_model_name,
            likelihood=QuantileRegression(),
            save_checkpoints=True,
            force_reset=True,
            optimizer_kwargs={"lr": 1e-2},
            n_epochs=150,
        )
        tcn_model.fit(
            series=target_series[0][: start_idx + j],
            past_covariates=covariates_scaled[: start_idx + j],
            epochs=150,
            verbose=False,
        )

        # Forecast the next 1 month (on scaled covariates)
        tcn_forecast_scaled = tcn_model.predict(
            n=1, past_covariates=covariates_scaled, num_samples=2500
        )
        tmp = TimeSeries.all_values(tcn_forecast_scaled[0])
        tmp = tmp.reshape(2500, 1)
        df = pd.DataFrame(tmp)
        tcn_predictions.insert(j, df)

    # Save forecasts to df, then to csv
    tcn_df = tcn_predictions[0]  # Initialise TCN predictions
    for k in range(1, len(tcn_predictions), 1):
        df = tcn_predictions[k]
        tcn_df = pd.concat([tcn_df, df])
    tcn_df.to_csv(fname)


# Do not use threadpools with this function as darts is highly threaded.
for i in range(province_count):
    historical_tcn(i, timeseries_df)
    window_tcn(i, timeseries_df)

logging.info("Finished TCN.")

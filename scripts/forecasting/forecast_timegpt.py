#!/usr/bin/env python

import os
import logging
import pandas as pd

from pathlib import Path
from nixtla import NixtlaClient
from darts.dataprocessing.transformers import Scaler
from concurrent.futures import ProcessPoolExecutor, wait

# Defaults
FORCE_RERUN = os.environ.get('FORCE_RERUN', 'false').lower() == 'true'
HORIZON_MONTHS = int(os.environ.get('HORIZON_MONTHS', '48'))
RETRAIN_MODELS = os.environ.get('RETRAIN_MODELS', 'true').lower() == 'true'
MAX_WORKERS = int(os.environ.get('MAX_WORKERS', os.cpu_count()))

logging.basicConfig(level=logging.INFO)

data_dir = "/app/data/python/data/"
output_dir = "/app/data/python/output/"

Path(output_dir).mkdir(parents=True, exist_ok=True)


quantiles = [
    0.010,
    0.025,
    0.050,
    0.100,
    0.150,
    0.200,
    0.250,
    0.300,
    0.350,
    0.400,
    0.450,
    0.500,
    0.550,
    0.600,
    0.650,
    0.700,
    0.750,
    0.800,
    0.850,
    0.900,
    0.950,
    0.975,
    0.990,
]


timeseries_df = pd.read_csv("/app/data/python/data/ptl_province_inla_df.csv")
timeseries_df = timeseries_df.fillna(0)
province_names = timeseries_df["PROVINCE"].unique()
province_count = len(province_names)

# Set up columns for covariates and
covariate_cols = ["LAG_1_LOG_CASES", "LAG_1_tmin_roll_2", "LAG_1_prec_roll_2"]
case_cols = ["LOG_CASES"]


# If needed for working with standardised/scaled variables
scaler_case, scaler_covars = Scaler(), Scaler()


# TimeGPT-1 MODEL Setup


TIMEGPT_KEY = os.environ.get("TIMEGPT_KEY")
nixtla_client = NixtlaClient(
    api_key=TIMEGPT_KEY,
)
historical_gpt_covar_cols = [
    "LAG_1_LOG_CASES",
    "LAG_1_tmin_roll_2",
    "LAG_1_prec_roll_2",
    "end_of_month",
    "LOG_CASES",
    "MONTH",
]
time_gpt_covar_cols = [
    "LAG_1_LOG_CASES",
    "LAG_1_tmin_roll_2",
    "LAG_1_prec_roll_2",
    "end_of_month",
    "MONTH",
]


# Historical forecasting (with covariates)
def historical_covars_timegpt(i, timeseries_df):
    fname = f"/app/data/python/output/historical_timegpt_dataframe_{i}.csv"
    if not FORCE_RERUN and Path(fname).exists():
        logging.info(f"Historial forecasting GPT {i} already exist, skipping...")
        return
    logging.info(f"Processing province {i + 1} out of {len(province_names)}")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    subset_df = subset_df[subset_df["YEAR"] < 2018]

    # TIME GPT
    subset_df_copy = subset_df.copy()
    subset_df_copy = subset_df_copy[historical_gpt_covar_cols]
    time_gpt_covar_df = subset_df[time_gpt_covar_cols]
    historical_timegpt_fcst_df = nixtla_client.forecast(
        df=subset_df_copy,
        X_df=time_gpt_covar_df.iloc[[-1]],
        finetune_steps=10,
        h=1,
        quantiles=quantiles,
        time_col="end_of_month",
        target_col="LOG_CASES",
        add_history=True,
    )
    historical_timegpt_fcst_df.to_csv(fname)


# Testing Window (with covariates)
def window_covars_timegpt(i, timeseries_df):
    fname = f"/app/data/python/output/finetuned_timegpt_dataframe_{i}.csv"
    if not FORCE_RERUN and Path(fname).exists():
        logging.info(f"Testing window {i} exists, skipping...")
        return
    logging.info(f"Processing province {i + 1} out of {province_count}")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    start_idx = subset_df.shape[0] - HORIZON_MONTHS  # Starting point for the 48-month horizon

    timegpt_predictions = []
    for j in range(HORIZON_MONTHS):
        logging.info(f"Processing month {j + 1} out of {HORIZON_MONTHS}")
        subset_df_copy = subset_df.copy()
        subset_df_copy = subset_df_copy[: start_idx + j]
        subset_df_copy = subset_df_copy.drop("PROVINCE", axis=1)
        subset_df_copy = subset_df_copy[historical_gpt_covar_cols]
        time_gpt_covar_df = subset_df[time_gpt_covar_cols]
        time_gpt_covar_df = time_gpt_covar_df[: start_idx + j + 1]
        timegpt_fcst_df = nixtla_client.forecast(
            df=subset_df_copy,
            X_df=time_gpt_covar_df.iloc[[-1]],
            finetune_steps=10,
            h=1,
            quantiles=quantiles,
            time_col="end_of_month",
            target_col="LOG_CASES",
        )
        timegpt_predictions.insert(j, timegpt_fcst_df)

    # Save forecasts to df, then to csv
    timegpt_df = timegpt_predictions[0]
    for k in range(1, len(timegpt_predictions), 1):
        df = timegpt_predictions[k]
        timegpt_df = pd.concat([timegpt_df, df])
    timegpt_df.to_csv(fname)


# Historical Forecasting (without covariates)
def historical_nocovars_timegpt(i, timeseries_df):
    fname = (
        f"/app/data/python/output/historical_no_covars_timegpt_dataframe_{i}.csv"
    )
    if Path(fname).exists():
        logging.info(f"TimeGPT without covariates {i} already exists, skipping...")
        return

    logging.info(f"Processing province {i + 1} out of {province_count}")
    no_covar_cols = ["end_of_month", "LOG_CASES"]
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    subset_df_copy = subset_df.copy()
    subset_df_copy = subset_df_copy[no_covar_cols]
    historical_timegpt_fcst_df = nixtla_client.forecast(
        df=subset_df_copy,
        finetune_steps=10,
        h=1,
        quantiles=quantiles,
        time_col="end_of_month",
        target_col="LOG_CASES",
        add_history=True,
    )
    historical_timegpt_fcst_df.to_csv(fname)


# Testing Window (without covariates)
def window_nocovars_timegpt(i, timeseries_df):
    fname = (
        f"/app/data/python/output/finetuned_no_covars_timegpt_dataframe_{i}.csv"
    )
    if Path(fname).exists():
        logging.info(f"TimeGPT without covars {i} exists, skipping...")
        return
    logging.info(f"Processing province {i + 1} out of {province_count}")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    start_idx = subset_df.shape[0] - HORIZON_MONTHS  # Starting point for the 48-month horizon

    no_covar_timegpt_predictions = []
    no_covar_cols = ["end_of_month", "LOG_CASES"]
    for j in range(HORIZON_MONTHS):
        logging.info(f"Processing month {j + 1} out of {HORIZON_MONTHS}")
        subset_df_copy = subset_df.copy()
        subset_df_copy = subset_df_copy[: start_idx + j]
        subset_df_copy = subset_df_copy.drop("PROVINCE", axis=1)
        subset_df_copy = subset_df_copy[no_covar_cols]
        timegpt_fcst_df = nixtla_client.forecast(
            df=subset_df_copy,
            finetune_steps=10,
            h=1,
            quantiles=quantiles,
            time_col="end_of_month",
            target_col="LOG_CASES",
        )
        no_covar_timegpt_predictions.insert(j, timegpt_fcst_df)

    timegpt_df = no_covar_timegpt_predictions[0]
    for k in range(1, len(no_covar_timegpt_predictions), 1):
        df = no_covar_timegpt_predictions[k]
        timegpt_df = pd.concat([timegpt_df, df])
    timegpt_df.to_csv(fname)

# Submit all jobs to the thread pool

with ProcessPoolExecutor(max_workers=MAX_WORKERS) as executor:
    logging.info(f"Available workers: {executor._max_workers}")
    logging.info(f"Launching {4*province_count} jobs.")
    futures = []
    for i in range(province_count):
        futures.append(executor.submit(historical_covars_timegpt, i, timeseries_df))
        futures.append(executor.submit(window_covars_timegpt, i, timeseries_df))
        futures.append(executor.submit(historical_nocovars_timegpt, i, timeseries_df))
        futures.append(executor.submit(window_nocovars_timegpt, i, timeseries_df))
    wait(futures)

logging.info("Finished TimeGPT.")

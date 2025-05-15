#!/usr/bin/env python

import os
from pathlib import Path
from darts.models import TCNModel
import pandas as pd
from darts import TimeSeries
from darts.dataprocessing.transformers import Scaler
from darts.utils.timeseries_generation import datetime_attribute_timeseries
from darts.utils.missing_values import fill_missing_values
from darts.utils.likelihood_models import GaussianLikelihood, QuantileRegression
from darts import concatenate
from darts.utils.callbacks import TFMProgressBar
import numpy as np
import torch
from pytorch_lightning.callbacks import EarlyStopping
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from darts.models import NBEATSModel
from nixtla import NixtlaClient
from darts.models import NaiveDrift
from darts.models import StatsForecastAutoARIMA


def generate_torch_kwargs():
    # run torch models on CPU, and disable progress bars for all model stages except training.
    return {
        "pl_trainer_kwargs": {
            "accelerator": "cpu",
            "callbacks": [TFMProgressBar(enable_train_bar_only=True)],
        }
    }



retrain_models = False  # JSB: Set False for faster testing; set True for predictions


data_dir = "/app/data/python/data/"
output_dir = "/app/data/python/output/"


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
# timeseries_df = timeseries_df[2:timeseries_df.shape[0]] #Remove first month where LAG values = 0
timeseries_df = timeseries_df.fillna(0)
province_names = timeseries_df["PROVINCE"].unique()


# Set up columns for covariates and
covariate_cols = ["LAG_1_LOG_CASES", "LAG_1_tmin_roll_2", "LAG_1_prec_roll_2"]
case_cols = ["LOG_CASES"]


# If needed for working with standardised/scaled variables
scaler_case, scaler_covars = Scaler(), Scaler()


# ## 1) TCN MODEL Setup


tcn_model_name = "TCN_Model"
tcn_model = TCNModel(
    input_chunk_length=48,  # Use 48 months of history to predict the next month
    output_chunk_length=1,  # Forecast 1 step ahead
    kernel_size=3,
    num_filters=4,
    random_state=42,
    model_name=tcn_model_name,
    likelihood=QuantileRegression(),
    save_checkpoints=True,
    force_reset=True,
    n_epochs=150,
    optimizer_kwargs={"lr": 1e-2},
)


# ## 2) TimeGPT-1 MODEL Setup


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


# ## TimeGPT + Deep TCN Historical forecasting


# Historical forecasting
for i in range(len(province_names)):
    filename_tcn_i = f"/app/data/python/output/historical_tcn_dataframe_{i}.csv"
    filename_gpt_i = f"/app/data/python/output/historical_timegpt_dataframe_{i}.csv"
    if Path(filename_gpt_i).exists() and Path(filename_tcn_i).exists():
        print(f"Historial forecasting TCN & GPT {i} already exist, skipping...")
        continue
    print(f"Processing province {i + 1} out of {len(province_names)}")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    # start_idx = subset_df.shape[0] - 48  # Starting point for the 48-month horizon
    # subset_df = subset_df[:start_idx + 1]
    subset_df = subset_df[subset_df["YEAR"] < 2018]
    # DEEP TCN
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
    covariates_scaled = scaler_covars.fit_transform(covariate_series)

    tcn_model.fit(
        series=target_series[0],
        past_covariates=covariates_scaled,
        epochs=150,
        verbose=False,
    )
    tcn_backtest = tcn_model.historical_forecasts(
        series=target_series[0],
        past_covariates=covariates_scaled,
        forecast_horizon=1,
        num_samples=2500,
        retrain=retrain_models,
        verbose=True,
    )
    tmp = TimeSeries.all_values(tcn_backtest)
    tmp = tmp.reshape(44, 2500)  # Need to change this 43 to not be hard-coded
    tcn_df = pd.DataFrame(tmp)

    tcn_df.to_csv(filename_tcn_i)

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
    historical_timegpt_fcst_df.to_csv(filename_gpt_i)

    # tcn_model.fit(series=target_series[0],
    #                   past_covariates=covariates_scaled,
    #                  epochs=150,
    #                  verbose = False)
    # tcn_backtest = tcn_model.historical_forecasts(
    #     series=target_series[0],
    #     past_covariates=covariates_scaled,
    #     forecast_horizon=1,
    #     num_samples = 2500,
    #     retrain=False,
    #     verbose=True)


# ## Historical SARIMA


# Historical SARIMA forecasting
for i in range(14):
    filename_i = f"/app/data/python/output/historical_sarima_dataframe_{i}.csv"
    if Path(filename_i).exists():
        print(f"Historical SARIMA {i} file already exists, skipping...")
        continue
    print(f"Processing province {i + 1} out of 14")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    start_idx = subset_df.shape[0] - 48  # Starting point for the 48-month horizon
    subset_df = subset_df[:start_idx]

    target_series = [
        TimeSeries.from_dataframe(
            subset_df, time_col="end_of_month", value_cols=case_cols
        )
    ]
    series = target_series[0][:start_idx]

    future_cov = datetime_attribute_timeseries(
        series, "month", cyclic=True, add_length=6
    )
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
    sarima_df.to_csv(filename_i)


# ## Testing Window SARIMA


counter = 0
for i in range(14):
    filename_i = f"/app/data/python/output/arima_dataframe_{i}.csv"
    if Path(filename_i).exists():
        print(f"Tesing window SARIMA {i} already exists, skipping...")
        continue
    print(f"Processing province {i + 1} out of 14")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    dates = subset_df["end_of_month"].unique()
    target_series = [
        TimeSeries.from_dataframe(
            subset_df, time_col="end_of_month", value_cols=case_cols
        )
    ]
    start_idx = subset_df.shape[0] - 48  # Starting point for the 48-month horizon
    arima_predictions = []
    for j in range(48):
        print(f"Counter: {counter}")

        print(f"Processing month {j + 1} out of 48")
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
        counter = counter + 1
    arima_df = arima_predictions[0]
    for k in range(1, len(arima_predictions), 1):
        df = arima_predictions[k]
        arima_df = pd.concat([arima_df, df])
    arima_df.to_csv(filename_i)


# ## Testing Window TCN + TimeGPT


for i in range(14):
    filename_tcn_i = f"/app/data/python/output/tcn_dataframe_{i}.csv"
    filename_gpt_i = f"/app/data/python/output/finetuned_timegpt_dataframe_{i}.csv"
    if Path(filename_tcn_i).exists() and Path(filename_gpt_i).exists():
        print(f"Testing window {i} exists, skipping...")
        continue
    print(f"Processing province {i + 1} out of 14")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    dates = subset_df["end_of_month"].unique()
    target_series = [
        TimeSeries.from_dataframe(
            subset_df, time_col="end_of_month", value_cols=case_cols
        )
    ]
    start_idx = subset_df.shape[0] - 48  # Starting point for the 48-month horizon

    covariate_series = [
        TimeSeries.from_dataframe(
            subset_df, time_col="end_of_month", value_cols=covariate_cols
        )
    ]

    tcn_predictions = []
    timegpt_predictions = []
    for j in range(48):
        print(f"Counter: {counter}")
        print(f"Processing month {j + 1} out of 48")
        tmp = TimeSeries.all_values(target_series[0][: start_idx + j + 1])
        # true_cases.insert(counter, tmp[-1])  # JSB: Have removed as true_cases is never mentioned again
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

        covariates_scaled = scaler_covars.fit_transform(
            covariate_series[: start_idx + j + 1]
        )

        tcn_model_name = "TCN_Model"
        tcn_model = TCNModel(
            input_chunk_length=48,  # Use 48 months of history to predict the next month
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

        counter = counter + 1  # Made forecasts for that individual time-province point

    # Save forecasts to df, then to csv

    # 1) TCN
    tcn_df = tcn_predictions[0]  # Initialise TCN predictions
    for k in range(1, len(tcn_predictions), 1):
        df = tcn_predictions[k]
        tcn_df = pd.concat([tcn_df, df])
    tcn_df.to_csv(filename_tcn_i)

    # 2) TimeGPT
    timegpt_df = timegpt_predictions[0]
    for k in range(1, len(timegpt_predictions), 1):
        df = timegpt_predictions[k]
        timegpt_df = pd.concat([timegpt_df, df])
    timegpt_df.to_csv(filename_gpt_i)


# ## TimeGPT Without Covariates


# Historical Forecasting
no_covar_cols = ["end_of_month", "LOG_CASES"]
counter = 0
for i in range(14):
    filename_i = (
        f"/app/data/python/output/historical_no_covars_timegpt_dataframe_{i}.csv"
    )
    if Path(filename_i).exists():
        print(f"TimeGPT without covariates {i} already exists, skipping...")
        continue

    print(f"Processing province {i + 1} out of 14")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    dates = subset_df["end_of_month"].unique()
    target_series = [
        TimeSeries.from_dataframe(
            subset_df, time_col="end_of_month", value_cols=case_cols
        )
    ]
    start_idx = subset_df.shape[0] - 48  # Starting point for the 48-month horizon
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
    historical_timegpt_fcst_df.to_csv(filename_i)


# Testing Window - TimeGPT Without covars
no_covar_cols = ["end_of_month", "LOG_CASES"]
counter = 0
for i in range(14):
    filename_i = (
        f"/app/data/python/output/finetuned_no_covars_timegpt_dataframe_{i}.csv"
    )
    if Path(filename_i).exists():
        print(f"TimeGPT without covars {i} exists, skipping...")
        continue
    print(f"Processing province {i + 1} out of 14")
    subset_df = timeseries_df[timeseries_df["PROVINCE"] == province_names[i]]
    dates = subset_df["end_of_month"].unique()
    target_series = [
        TimeSeries.from_dataframe(
            subset_df, time_col="end_of_month", value_cols=case_cols
        )
    ]
    start_idx = subset_df.shape[0] - 48  # Starting point for the 48-month horizon

    no_covar_timegpt_predictions = []
    for j in range(48):
        print(f"Counter: {counter}")

        print(f"Processing month {j + 1} out of 48")
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
        # timegpt_predictions.insert(counter, timegpt_fcst_df)
        no_covar_timegpt_predictions.insert(j, timegpt_fcst_df)

    timegpt_df = no_covar_timegpt_predictions[0]
    for k in range(1, len(no_covar_timegpt_predictions), 1):
        df = no_covar_timegpt_predictions[k]
        timegpt_df = pd.concat([timegpt_df, df])
    timegpt_df.to_csv(filename_i)

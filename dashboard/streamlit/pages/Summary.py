import numpy as np
import pandas as pd
import streamlit as st

from pathlib import Path

FOLDER_PATH = "./predictions"

st.set_page_config(layout="wide")
st.title("🦟 Dengue Prediction Platform")

st.warning("WARNING - Development build — Do not rely on data.")

# --- Sidebar Controls
models = {
    str(dir.name).replace("Ensemble_", "").replace("_", " ").replace("star", "*"): str(
        dir.name
    )
    for dir in sorted(Path(FOLDER_PATH).iterdir())
}
metrics = {
    "Cases (from quantiles)": "cases_quantiles",
    "Log cases (from quantiles)": "log_cases_quantiles",
}
countries = {
    "Peru": "PER",
    "Chile": "CHL",
}
admin_levels = {
    "Admin 2": "ADM2",
}

country_name = st.sidebar.selectbox("Country:", list(countries.keys()))
country = countries[country_name]

metric_name = st.sidebar.selectbox("Metric:", list(metrics.keys()), index=1)
metric = metrics[metric_name]

admin_level_name = st.sidebar.selectbox("Admin Level:", list(admin_levels.keys()))
admin_level = admin_levels[admin_level_name]


# --- Load Real Model Data
def get_filename(model, field="prediction"):
    match metric:
        case "cases_samples":
            filename = (
                Path(FOLDER_PATH) / model / "pred_log_cases_samples_forecasting.csv"
            )
            data_transform = lambda df: np.exp(np.median(df[field])) + 1
            tooltip_text = "Cases: {data}"
        case "log_cases_samples":
            filename = (
                Path(FOLDER_PATH) / model / "pred_log_cases_samples_forecasting.csv"
            )
            data_transform = lambda df: np.median(df[field])
            tooltip_text = "Log cases: {data}"
        case "cases_quantiles":
            filename = (
                Path(FOLDER_PATH) / model / "pred_log_cases_quantiles_forecasting.csv"
            )
            data_transform = (
                lambda df: np.exp(df[df["quantile"] == 0.5][field].values[-1]) + 1
            )  # last value (should only be one)
            tooltip_text = "Cases (log): {data}"
        case "log_cases_quantiles":
            filename = (
                Path(FOLDER_PATH) / model / "pred_log_cases_quantiles_forecasting.csv"
            )
            data_transform = lambda df: df[df["quantile"] == 0.5][field].values[
                -1
            ]  # last value (should only be one)
            tooltip_text = "Log cases: {data}"
        case _:
            raise RuntimeError(f"Unrecognised metric: {metric_name}")
    return filename, data_transform, tooltip_text


df_summary = pd.DataFrame(
    {
        "Model": [],
        "R2": [],
    }
)


def fcn_R2(y_true, y_pred):
    ss_res = np.sum((y_true - y_pred) ** 2)
    ss_tot = np.sum((y_true - np.mean(y_true)) ** 2)
    r2 = 1 - (ss_res / ss_tot)
    return r2


with st.spinner("Analysing datasets..."):
    for model_key in models:
        model = models[model_key]
        print(model)
        filename, data_transform, tooltip_text = get_filename(model)

        df = pd.read_csv(str(filename), sep=",")
        if "location" not in df.columns and "PROVINCE" in df.columns:
            df = df.rename(columns={"PROVINCE": "location"})
        if (
            "target_end_date" not in df.columns
            and "MONTH" in df.columns
            and "YEAR" in df.columns
        ):
            df["target_end_date"] = pd.to_datetime(
                df[["YEAR", "MONTH"]].assign(day=1)
            ) + pd.offsets.MonthEnd(0)
        df["target_end_date"] = pd.to_datetime(df["target_end_date"])

        _, trans_prediction, _ = get_filename(model, "prediction")
        _, trans_target, _ = get_filename(model, "true_value")

        date_vector = df["target_end_date"].unique()
        # date_vector = df['target_end_date'].max()

        prediction = np.array([])
        target = np.array([])
        provinces = df["location"].unique()
        for prov in provinces:
            for date in date_vector:
                df1 = df[df["location"] == prov]
                df1 = df1[df1["target_end_date"] == date]
                prediction = np.append(prediction, trans_prediction(df1))
                target = np.append(target, trans_target(df1))

        df_summary = pd.concat(
            [
                df_summary,
                pd.DataFrame(
                    {
                        "Model": [model_key],
                        "R2": [fcn_R2(np.array(target), np.array(prediction))],
                    },
                ),
            ],
            ignore_index=True,
        )

st.dataframe(df_summary)

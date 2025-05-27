import numpy as np
import pandas as pd
import seaborn as sns
import streamlit as st
import matplotlib.dates as mdates
import matplotlib.pyplot as plt

from pathlib import Path

FOLDER_PATH = "./predictions"

st.set_page_config(layout="wide")
st.title("🦟 Dengue Prediction Platform")

st.warning("WARNING - Development build — Do not rely on data.")

# --- Sidebar Controls
countries = {
    "Peru": "PER",
}
models = {
    str(dir.name).replace("Ensemble_", "").replace("_", " ").replace("star", "*"): str(
        dir.name
    )
    for dir in sorted(Path(FOLDER_PATH).iterdir())
}
metrics = {
    "Cases": "cases_quantiles",
    "Log cases": "log_cases_quantiles",
}

country_name = st.sidebar.selectbox("Country:", list(countries.keys()))
country = countries[country_name]

model_name = st.sidebar.selectbox("Model:", list(models.keys()))
model = models[model_name]

metric_name = st.sidebar.selectbox("Metric:", list(metrics.keys()), index=1)
metric = metrics[metric_name]

# --- Load Real Model Data
with st.spinner("Analysing datasets..."):
    filename = Path(FOLDER_PATH) / model / "pred_log_cases_quantiles_forecasting.csv"

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

    if metric == "cases_quantiles":
        df["prediction"] = np.exp(df["prediction"]) - 1
        df["true_value"] = np.exp(df["true_value"]) - 1

    df_median = df[df["quantile"] == 0.5]
    df_q05 = df[df["quantile"] == 0.05]
    df_q95 = df[df["quantile"] == 0.95]

    def overlay_plot(data, color, **kwargs):
        ax = plt.gca()
        ax.fill_between(
            data[data["quantile"] == 0.5]["target_end_date"],
            data[data["quantile"] == 0.05]["prediction"],
            data[data["quantile"] == 0.95]["prediction"],
            color="green",
            alpha=0.1,
            label="95% Interval",
        )
        sns.lineplot(
            data=data[data["quantile"] == 0.5],
            x="target_end_date",
            y="prediction",
            ax=ax,
            label="Prediction",
            color="green",
        )
        sns.scatterplot(
            data=data[data["quantile"] == 0.5],
            x="target_end_date",
            y="true_value",
            ax=ax,
            label="Observed",
            color="red",
            s=20,
        )

    # Create FacetGrid
    g = sns.FacetGrid(
        df, col="location", col_wrap=2, height=2.5, aspect=2, sharey=False
    )
    g.map_dataframe(overlay_plot)
    g.set_axis_labels("Date", metric_name)
    # Format x-axis labels to be Year only (no repeat labels)
    for ax in g.axes.flat:
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
        ax.xaxis.set_major_locator(mdates.YearLocator(1))
    # Figure legend (on second axis only)
    g.axes.flat[1].legend()

    st.pyplot(g.figure)

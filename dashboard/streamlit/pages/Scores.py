import numpy as np
import pandas as pd
import seaborn as sns
import streamlit as st
import matplotlib
import matplotlib.dates as mdates
import matplotlib.pyplot as plt

print(matplotlib.get_backend())

from pathlib import Path


FOLDER_PATH = "./analysis"


st.set_page_config(layout="wide")
st.title("🦟 Dengue Prediction Platform")

# with st.spinner("Analysing datasets..."):
filename = Path(FOLDER_PATH).resolve() / "wis" / "wis_summary.csv"
df = pd.read_csv(str(filename), sep=",")
df["target_end_date"] = pd.to_datetime(df["target_end_date"])

metric = "interval_score"
baseline_model = "baseline"

dfs = []
for model in df["model"].unique():
    dates = df[df.model == baseline_model]["target_end_date"].to_numpy()
    baseline_score = df[df.model == baseline_model][metric].to_numpy()
    interval_score = df[df.model == model][metric].to_numpy()

    dfs.append(
        pd.DataFrame(
            {
                "Date": dates,
                "Model": [model] * len(interval_score),
                "Score": interval_score / baseline_score,
            }
        )
    )

data = pd.concat(dfs, ignore_index=True)

fig, ax = plt.subplots(figsize=(20, 8))
sns.lineplot(data=data, x="Date", y="Score", hue="Model")
ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
ax.xaxis.set_major_locator(mdates.YearLocator(1))
ax.set_title("Interval Score", fontsize=18)
ax.set_xlabel("Date", fontsize=14)
ax.set_ylabel("Interval Score", fontsize=14)
ax.legend(
    bbox_to_anchor=(0.5, -0.3),
    loc="upper center",
    ncol=6,
    fontsize=14,
)
st.pyplot(fig)

import os
import numpy as np
import pandas as pd
import streamlit as st

from openai import OpenAI
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
df_collate = None


def fcn_R2(y_true, y_pred):
    ss_res = np.sum((y_true - y_pred) ** 2)
    ss_tot = np.sum((y_true - np.mean(y_true)) ** 2)
    r2 = 1 - (ss_res / ss_tot)
    return r2


with st.spinner("Analysing datasets..."):
    for model_key in models:
        model = models[model_key]
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

        prediction = np.array([])
        target = np.array([])
        provinces = df["location"].unique()
        for prov in provinces:
            for date in date_vector:
                df1 = df[df["location"] == prov]
                df1 = df1[df1["target_end_date"] == date]
                prediction = np.append(prediction, trans_prediction(df1))
                target = np.append(target, trans_target(df1))

        df1 = df[df['quantile'] == 0.5]
        model_name = df1['model'].iloc[0]
        df1 = df1.rename(columns={'prediction': f"prediction_{model_name}"})
        df1 = df1.drop(columns=['quantile', 'model'])

        if df_collate is None:
            df_collate = df1
        else:
            df1 = df1.drop(columns=['true_value'])
            df_collate = df_collate.merge(df1, on=['target_end_date', 'location'])

        print(df_collate)

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

# AI Summary

OPENAI_API_KEY=os.environ.get('OPENAI_API_KEY', None)

def ask_llm(instructions, input_text, api_key=None, model="gpt-4o"):
    if not api_key:
        raise Exception("No API key provided for LLM query")

    client = OpenAI(
        api_key=api_key,
    )

    response = client.responses.create(
        model=model,
        instructions=instructions,
        input=input_text,
    )

    print(response)

    return response.output_text


if OPENAI_API_KEY:
    with st.spinner("Generating AI summary..."):
        instructions = """
        You are a public health advisor and an expert in Dengue disease and disease outbreaks.
        Provide three paragraphs interpreting the available data as if reporting to public health or governmental organisations.
        Focus on interpretaion of the available dataset, placing it into context within your wider expert knowledge.
        """
        
        user_input = "Here is a database of Dengue predictions using different statistical models for provinces in northern Peru:"
        df_collate.columns = df_collate.columns.str.replace('*', '', regex=False).str.strip()
        user_input += '\n\n' + df_collate[
                (df_collate['target_end_date'] > pd.Timestamp('2021-01-01'))
            ].to_string()

        print(user_input)
        response = ask_llm(instructions, user_input, api_key=OPENAI_API_KEY)

        st.subheader("AI Summary")
        st.write(response)

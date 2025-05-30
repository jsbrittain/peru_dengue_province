import os
import json
import numpy as np
import pandas as pd
import pydeck as pdk
import colorcet as cc
import streamlit as st

from PIL import Image, ImageDraw
from pathlib import Path

st.set_page_config(layout="wide")
st.title("🦟 Dengue Prediction Platform")

st.warning("WARNING - Development build — Do not rely on data.")

# --- Sidebar Controls
models = {
    str(dir.name).replace("Ensemble_", "").replace("_", " ").replace("star", "*"): str(
        dir.name
    )
    for dir in sorted(Path("./predictions").iterdir())
}
metrics = {
    "Cases (from quantiles)": "cases_quantiles",
    "Log cases (from quantiles)": "log_cases_quantiles",
}
countries = {
    "Peru": "PER",
    "Chile": "CHL",
    "Brazil": "BRA",
    "Mexico": "MEX",
    # "Bolivia": "BOL",
    # "Ecuador": "ECU",
    # "Colombia": "COL",
    # "Paraguay": "PRY",
    # "Uruguay": "URY",
    # "Argentina": "ARG",
    # "Guyana": "GUY",
    # "Suriname": "SUR",
    # "Venezuela": "VEN",
    # "Panama": "PAN",
}
admin_levels = {
    "Admin 2": "ADM2",
}
basemaps = {
    "Light": "light",
    "Dark": "dark",
    "Satellite": "satellite",
}
colormaps = {
    "CET D8": cc.CET_D8,
    "Fire": cc.fire,
    "Rainbow": cc.rainbow,
    "Blue": cc.blues,
    "Cool": cc.kbc,
}

country_name = st.sidebar.selectbox("Country:", list(countries.keys()))
country = countries[country_name]

model_name = st.sidebar.selectbox("Model:", list(models.keys()))
model = models[model_name]

if model in ["baseline", "bayesian"]:
    metrics.update(
        {
            "Cases (from samples)": "cases_samples",
            "Log cases (from samples)": "log_cases_samples",
        }
    )

metric_name = st.sidebar.selectbox("Metric:", list(metrics.keys()))
metric = metrics[metric_name]

admin_level_name = st.sidebar.selectbox("Admin Level:", list(admin_levels.keys()))
admin_level = admin_levels[admin_level_name]

basemap_name = st.sidebar.selectbox("Basemap style:", list(basemaps.keys()))
basemap = basemaps[basemap_name]

selected_cmap = st.sidebar.selectbox("Colormap:", list(colormaps.keys()))
alpha_value = st.sidebar.slider(
    "Fill Opacity (Alpha)", min_value=0, max_value=255, value=140
)
invert_cmap = st.sidebar.checkbox("Invert Colormap:", False)

# --- GeoJSON path
geojson_path = f"maps/geoBoundaries-{country}-{admin_level}_simplified.geojson"

if not os.path.exists(geojson_path):
    st.error(f"GeoJSON file not found: {geojson_path}")
    st.stop()

if st.session_state.get("geojson_path") != geojson_path:
    with open(geojson_path, "r") as f:
        geojson = json.load(f)
    st.session_state.geojson = geojson
    st.session_state.geojson_path = geojson_path

geojson = st.session_state.geojson
geojson_path = st.session_state.geojson_path

fields = {
    "Prediction": "prediction",
    "Ground truth": "true_value",
}
col1, col2, col3 = st.columns([1, 1, 1])
with col1:
    field_name = st.selectbox("Field:", list(fields.keys()))
field = fields[field_name]

# --- Load Real Model Data
match metric:
    case "cases_sampl_es":
        filename = (
            Path("./predictions") / model / "pred_log_cases_samples_forecasting.csv"
        )
        data_transform = lambda df: np.exp(np.median(df[field])) + 1
        tooltip_text = "Cases: {data}"
    case "log_cases_samples":
        filename = (
            Path("./predictions") / model / "pred_log_cases_samples_forecasting.csv"
        )
        data_transform = lambda df: np.median(df[field])
        tooltip_text = "Log cases: {data}"
    case "cases_quantiles":
        filename = (
            Path("./predictions") / model / "pred_log_cases_quantiles_forecasting.csv"
        )
        data_transform = (
            lambda df: np.exp(df[df["quantile"] == 0.5][field].values[-1]) + 1
        )  # last value (should only be one)
        tooltip_text = "Cases (log): {data}"
    case "log_cases_quantiles":
        filename = (
            Path("./predictions") / model / "pred_log_cases_quantiles_forecasting.csv"
        )
        data_transform = lambda df: df[df["quantile"] == 0.5][field].values[
            -1
        ]  # last value (should only be one)
        tooltip_text = "Log cases: {data}"
    case _:
        raise RuntimeError(f"Unrecognised metric: {metric_name}")

if st.session_state.get("filename", None) != filename:
    with st.spinner("Loading data..."):
        st.session_state.filename = filename
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
        st.session_state.df = df

df = st.session_state.df

# --- Date slider
available_dates = sorted(df["target_end_date"].unique())
closest_date = st.select_slider(
    "Select a date:",
    options=available_dates,
    format_func=lambda d: d.strftime("%Y-%m-%d"),
    value=available_dates[-1],
)


model_data = {f["properties"]["shapeName"]: np.nan for f in geojson["features"]}
df_provinces = df["location"].unique()
for prov in model_data.keys():
    if prov in df_provinces:
        df1 = df[df["location"] == prov]
        df1 = df1[df1["target_end_date"] == closest_date]
        model_data[prov] = data_transform(df1)


# --- Assign scores and fill colors
def get_color(score, cmap, alpha=0):
    hex_color = cmap[int(score * (len(cmap) - 1))]
    rgb = tuple(int(hex_color.lstrip("#")[i : i + 2], 16) for i in (0, 2, 4))
    return list(rgb) + [alpha]


def draw_colorbar(cmap_hex_list, width=300, height=20):
    """
    Draw a horizontal colorbar using a list of hex colors.
    """
    n_colors = len(cmap_hex_list)
    bar = Image.new("RGBA", (width, height), (255, 255, 255, 0))
    draw = ImageDraw.Draw(bar)

    for x in range(width):
        # Get corresponding color index
        color_index = int((x / width) * (n_colors - 1))
        color_hex = cmap_hex_list[color_index]

        # Validate and convert hex to RGB
        if (
            isinstance(color_hex, str)
            and color_hex.startswith("#")
            and len(color_hex) == 7
        ):
            try:
                rgb = tuple(int(color_hex[i : i + 2], 16) for i in (1, 3, 5))
            except ValueError:
                rgb = (150, 150, 150)  # fallback grey
        else:
            rgb = (150, 150, 150)

        draw.line([(x, 0), (x, height)], fill=rgb)

    return bar


def get_colormap():
    cmap = colormaps[selected_cmap]
    if invert_cmap:
        # copy and reverse colormap
        cmap = list(reversed(cmap))
    return cmap


min_data = np.nanmin(list(model_data.values()))
max_data = np.nanmax(list(model_data.values()))
if max_data > 0:
    color_white = [255, 255, 255, alpha_value]
    for feature in geojson["features"]:
        name = feature["properties"]["shapeName"]
        data = model_data.get(name, np.nan)
        if np.isnan(data):
            # white fill
            feature["properties"]["data"] = "-"
            feature["properties"]["fill_color"] = color_white
        else:
            # scale color to data
            normalised_data = (data - min_data) / (max_data - min_data)
            feature["properties"]["data"] = f"{data:.1f}"
            feature["properties"]["fill_color"] = get_color(
                normalised_data, get_colormap(), alpha_value
            )

# --- Select basemap
view_by_country = {
    "PER": pdk.ViewState(latitude=-9.2, longitude=-75.1, zoom=4),
    "CHL": pdk.ViewState(latitude=-35.6, longitude=-71.5, zoom=3),  # bearing=90
}
initial_view = (
    view_by_country[country]
    if country in view_by_country
    else pdk.ViewState(latitude=-9.2, longitude=-75.1, zoom=2)
)
map_style = {
    "light": "mapbox://styles/mapbox/light-v11",
    "dark": "mapbox://styles/mapbox/dark-v11",
    "satellite": "mapbox://styles/mapbox/satellite-streets-v12",
}[basemap]

# --- Create layer
layer = pdk.Layer(
    "GeoJsonLayer",
    geojson,
    pickable=True,
    auto_highlight=True,
    get_fill_color="properties.fill_color",
    get_line_color=[30, 30, 30],
    line_width_min_pixels=0.5,
)

# --- Tooltip config
tooltip = {
    "html": f"<b>{{shapeName}}</b><br>{tooltip_text}",
    "style": {"backgroundColor": "white", "color": "black"},
}

# --- Show map
deck = pdk.Deck(
    layers=[layer],
    initial_view_state=initial_view,
    map_style=map_style,
    tooltip=tooltip,
)
st.pydeck_chart(deck)

# --- Colorbar
colorbar_image = draw_colorbar(get_colormap(), width=400, height=25)
col1, col2, col3 = st.columns([1, 1, 1])
with col1:
    st.markdown(
        f"<div style='text-align:right; margin:0; padding:0; font-size:14px;'>{min_data:.1f}</div>",
        unsafe_allow_html=True,
    )
with col2:
    st.image(colorbar_image, use_container_width=True)
with col3:
    st.caption(f"{max_data:.2}")

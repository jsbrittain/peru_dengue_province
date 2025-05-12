import os
import json
import random
import numpy as np
import pydeck as pdk
import colorcet as cc
import streamlit as st
from PIL import Image, ImageDraw

st.set_page_config(layout="wide")
st.title("🦟 Dengue Prediction Platform")

st.warning("Development build — using surrogate or demo data only.")

# --- Sidebar Controls
countries = {
    "Peru": "PER",
    "Chile": "CHL",
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
    "CET D8" : cc.CET_D8,
    "Fire": cc.fire,
    "Rainbow": cc.rainbow,
    "Blue": cc.blues,
    "Cool": cc.kbc,
}

country = st.sidebar.selectbox("Country:", list(countries.keys()))
admin_level = st.sidebar.selectbox("Admin Level:", list(admin_levels.keys()))
selected_basemap = st.sidebar.selectbox("Basemap style:", list(basemaps.keys()))
selected_cmap = st.sidebar.selectbox("Colormap:", list(colormaps.keys()))
alpha_value = st.sidebar.slider("Fill Opacity (Alpha)", min_value=0, max_value=255, value=140)
invert_cmap = st.sidebar.checkbox("Invert Colormap:", False)

# --- GeoJSON path
geojson_path = f"maps/geoBoundaries-{countries[country]}-{admin_levels[admin_level]}_simplified.geojson"

if not os.path.exists(geojson_path):
    st.error(f"GeoJSON file not found: {geojson_path}")
    st.stop()

with open(geojson_path, "r") as f:
    geojson = json.load(f)

# --- Load Real Model Data
model_data = {
    f["properties"]["shapeName"]: random.uniform(0, 1) for f in geojson["features"]
}


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
        if isinstance(color_hex, str) and color_hex.startswith("#") and len(color_hex) == 7:
            try:
                rgb = tuple(int(color_hex[i:i+2], 16) for i in (1, 3, 5))
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

for feature in geojson["features"]:
    name = feature["properties"]["shapeName"]
    risk = model_data.get(name, 0)
    feature["properties"]["risk"] = f"{risk:.2f}"
    feature["properties"]["fill_color"] = get_color(risk, get_colormap(), alpha_value)

# --- Select basemap
view_by_country = {
    "Peru": pdk.ViewState(latitude=-9.2, longitude=-75.1, zoom=4),
    "Chile": pdk.ViewState(latitude=-35.6, longitude=-71.5, zoom=3),  # bearing=90
}
initial_view = view_by_country[country]
map_style = {
    "light": "mapbox://styles/mapbox/light-v11",
    "dark": "mapbox://styles/mapbox/dark-v11",
    "satellite": "mapbox://styles/mapbox/satellite-streets-v12",
}[basemaps[selected_basemap]]

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
    "html": "<b>{shapeName}</b><br>Risk: {risk}",
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
st.subheader("Color Legend")
colorbar_image = draw_colorbar(get_colormap(), width=400, height=25)
st.image(colorbar_image)
st.caption("Low Risk ←→ High Risk")

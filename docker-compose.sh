#/usr/bin/env bash
#
# Bash equivalent script to docker-compose.yml

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# ======================================================================================
# Datasets
# ======================================================================================

# Dataset: ICEN
docker build -t dataset-icen workflows/datasets/icen
docker run \
    -v $(pwd)/data/icen:/app/data/icen \
    dataset-icen

# Dataset: ONI
docker build -t dataset-icen workflows/datasets/oni
docker run \
    -v $(pwd)/data/oni:/app/data/oni \
    dataset-oni

# Dataset: proy
docker build -t dataset-proy workflows/datasets/proy
docker run \
    -v $(pwd)/data/pop:/app/data/pop \
    dataset-proy

# Dataset: shapefiles
docker build -t dataset-shapefiles workflows/datasets/shapefiles
docker run \
    -v $(pwd)/data/shapefiles:/app/data/shapefiles \
    dataset-shapefiles

# Dataset: spi6
docker build -t dataset-spi6 workflows/datasets/spi6
docker run \
    -v $(pwd)/data/spi6:/app/data/spi6 \
    dataset-spi6

# Dataset: worldclim
docker build -t dataset-worldclim workflows/datasets/worldclim
docker run \
    -v $(pwd)/data/climate:/app/data/climate \
    dataset-worldclim

# ======================================================================================
# Processing
# ======================================================================================

# Processing: province_01
docker run \
    --platform linux/amd64 \
    -v "${SCRIPT_DIR}"/scripts:/app/scripts:ro \
    -v "${SCRIPT_DIR}"/workflows:/app/workflows:ro \
    -v "${SCRIPT_DIR}"/data:/app/data \
    -it \
    peru-build \
    R -e 'source("scripts/processing/province_01.R"); source("scripts/processing/province_02.R")'

# Processing: province_export_csv
docker run \
    --platform linux/amd64 \
    -v "${SCRIPT_DIR}"/scripts:/app/scripts:ro \
    -v "${SCRIPT_DIR}"/workflows:/app/workflows:ro \
    -v "${SCRIPT_DIR}"/data:/app/data \
    -it \
    peru-build \
    R -e 'source("scripts/processing/province_export_csv.R")'

# Processing: province_02b
docker run \
    --platform linux/amd64 \
    -v "${SCRIPT_DIR}"/scripts:/app/scripts:ro \
    -v "${SCRIPT_DIR}"/workflows:/app/workflows:ro \
    -v "${SCRIPT_DIR}"/data:/app/data \
    -it \
    peru-build \
    R -e 'source("scripts/processing/province_02b.R")'

# ======================================================================================
# Models
# ======================================================================================

# Baseline forecasting
docker run \
    --platform linux/amd64 \
    -v "${SCRIPT_DIR}"/scripts:/app/scripts:ro \
    -v "${SCRIPT_DIR}"/workflows:/app/workflows:ro \
    -v "${SCRIPT_DIR}"/data:/app/data \
    -it \
    peru-build \
    R -e 'source("scripts/forecasting/province_baseline_forecaster.R")'

# Bayesian forecasting (historical)
docker run \
    --platform linux/amd64 \
    -v "${SCRIPT_DIR}"/scripts:/app/scripts:ro \
    -v "${SCRIPT_DIR}"/workflows:/app/workflows:ro \
    -v "${SCRIPT_DIR}"/data:/app/data \
    -it \
    peru-build \
    R -e 'source("scripts/forecasting/province_historical_bayesian_forecasting.R")'

# Bayesian forecasting
docker run \
    --platform linux/amd64 \
    -v "${SCRIPT_DIR}"/scripts:/app/scripts:ro \
    -v "${SCRIPT_DIR}"/workflows:/app/workflows:ro \
    -v "${SCRIPT_DIR}"/data:/app/data \
    -it \
    peru-build \
    R -e 'source("scripts/forecasting/province_bayesian_forecasting.R")'

# This is a Python runtime, so can be run with a separate minimal container
######################################################################################## <--
docker run \
    --platform linux/amd64 \
    -v "${SCRIPT_DIR}"/scripts:/app/scripts:ro \
    -v "${SCRIPT_DIR}"/workflows:/app/workflows:ro \
    -v "${SCRIPT_DIR}"/data:/app/data \
    -it \
    peru-build \
    python scripts/forecasting/python_peru_forecast.py

# This imports the Python results into R, so needs to run after Python
docker run \
    --platform linux/amd64 \
    -v "${SCRIPT_DIR}"/scripts:/app/scripts:ro \
    -v "${SCRIPT_DIR}"/workflows:/app/workflows:ro \
    -v "${SCRIPT_DIR}"/data:/app/data \
    -it \
    peru-build \
    R -e 'source("scripts/forecasting/province_python_forecasting.R")'

# ======================================================================================
# Ensemble models
# ======================================================================================

# Case ensemble
docker run \
    --platform linux/amd64 \
    -v "${SCRIPT_DIR}"/scripts:/app/scripts:ro \
    -v "${SCRIPT_DIR}"/workflows:/app/workflows:ro \
    -v "${SCRIPT_DIR}"/data:/app/data \
    -it \
    peru-build \
    R -e 'source("scripts/forecasting/province_log_cases.R")'

# Dengue Incidence Rate (DIR)
######################################################################################## <--
docker run \
    --platform linux/amd64 \
    -v "${SCRIPT_DIR}"/scripts:/app/scripts:ro \
    -v "${SCRIPT_DIR}"/workflows:/app/workflows:ro \
    -v "${SCRIPT_DIR}"/data:/app/data \
    -it \
    peru-build \
    R -e 'source("scripts/forecasting/province_dir_ensemble_scoring.R")'

# ======================================================================================
# Visualisation
# ======================================================================================

# Dashboard (streamlit app)
docker build -t streamlit vis/streamlit
docker run \
    -v $(pwd)/vis/streamlit:/app \
    -p 8501:8501 \
    streamlit

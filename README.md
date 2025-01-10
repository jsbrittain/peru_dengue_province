# Interdisciplinary modelling and forecasting of dengue

This repository provides scripts and instructions to perform the analyses presented in the paper *Interdisciplinary modelling and forecasting of dengue* [https://doi.org/10.1101/2024.10.18.24315690].

To perform the ensemble analysis presented in the paper requires three steps:
1. [Data download](#data)
1. [Preprocessing](#preprocessing)
1. [Forecasting](#forecasting)

## Data download

### Collated data

Raw collated case data are available as a pre-downloaded and collated archive provided in [compressed_data.csv.gz](processing/compressed_data.csv.gz), with province-level population data provided in [province_pop.xlsx](processing/province_pop.xlsx). This is a useful starting point to familiarise yourself with the workflow. If you wish to download more recent data then follow the instructions below.

### Manual download

To collate the data manually you will need to access data from the following public repositories.

| Dataset | Author | URL |
| - | - | - |
| Dengue incidence surveillance data | [National Centre for Epidemiology, Disease Prevention and Control (Peru CDC)](https://www.dge.gob.pe/salasituacional) | https://www.dge.gob.pe/sala-situacional-dengue/#grafico01 |
| Mid-year population estimates | [National Institute of Statistics and Information of Peru](https://www.gob.pe/inei/) | https://www.gob.pe/inei/ ([direct link](https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/proy_04.xls)) |
| Monthly historical temperature and precipiation | [WorldClim](https://www.worldclim.org/) | https://www.worldclim.org/data/monthlywth.html<br>2.5min resolution:<table><thead><tr><th>Years</th><th>Min temp</th><th>Max temp</th><th>Precipitation</th></tr></thead><tbody><tr><td>2010-2019</td><td>[tmin_2010-2019](https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.06/2.5m/wc2.1_cruts4.06_2.5m_tmin_2010-2019.zip)</td><td>[tmax_2010-2019](https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.06/2.5m/wc2.1_cruts4.06_2.5m_tmax_2010-2019.zip)</td><td>[prec_2010-2019](https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.06/2.5m/wc2.1_cruts4.06_2.5m_prec_2010-2019.zip)</td></tr><tr><td>2020-2021</td><td>[tmin_2020-2021](https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.06/2.5m/wc2.1_cruts4.06_2.5m_tmin_2020-2021.zip)</td><td>[tmax_2020-2021](https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.06/2.5m/wc2.1_cruts4.06_2.5m_tmax_2020-2021.zip)</td><td>[prec_2020-2021](https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.06/2.5m/wc2.1_cruts4.06_2.5m_prec_2020-2021.zip)</td></tr></tbody></table> |
| Standardized Precipitation Index 6 | [European Drought Observatory](https://jeodpp.jrc.ec.europa.eu/) | https://data.jrc.ec.europa.eu/dataset/1534c8f7-42e8-4212-b2dd-6388f60987eb#dataaccess ([direct link](https://drought.emergency.copernicus.eu/data/Drought_Observatories_datasets/GDO_Standardized_Precipitation_Index_SPI6/ver1-2-0/)) |
| Oceanic Niño Index (ONI) | [National Oceanic and Atmospheric Administration (NOAA)](https://origin.cpc.ncep.noaa.gov/) | https://www.cpc.ncep.noaa.gov/data/ ([direct link](https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt)) |
| Indice Costero El Niño (ICEN) | [Geophysical Institute of Peru](http://met.igp.gob.pe) | http://met.igp.gob.pe/datos/ ([direct link](http://met.igp.gob.pe/datos/icen.txt)) |
| Shape files | [Humanitarian Data Exchange](https://data.humdata.org/) | https://data.humdata.org/dataset/cod-ab-per ([direct link](https://data.humdata.org/dataset/54fc7f4d-f4c0-4892-91f6-2fe7c1ecf363/resource/63cc642a-2957-4f25-8a17-086c99d275e8/download/per_adm_ign_20200714_shp.zip))|

## Preprocessing

### Processed data

Processed data are available as a pre-downloaded and collated archive provided in [ptl_province_inla_df.csv](processing/ptl_province_inla_df.csv). If you wish to process the data yourself then follow the instructions below.

### Manual processing

1. Download the relevant archives (listed above), or decompress the provided archive [compressed_data.csv.gz](processing/compressed_data.csv).
1. Check the `DIRECTORIES` section of the script [processing/packages_directories.R](processing/packages_directories.R) and ensure that the appropriate directories are set for your system.
1. Run the following scripts in order in the same R environment from the `processing` directory:
   1. [packages_directories.R](processing/packages_directories.R)
   1. [province_01.R](processing/province_01.R)
   1. [province_02.R](processing/province_02.R)

The output of the final step will be a dataset of province-level monthly cases across 2010-2011 inclusive, alongside the corresponding demographic and climate information. This is stored in the file `ptl_province_inla_df.csv`. Move this file to the `forecasting` directory to continue with the forecasting analysis.

# Forecasting

Forecasting will run several independent models before combining them into ensembles. The scripts take the processed data `ptl_province_inla_df.csv` from the previous step.

First, load R with a fresh session and set the working directory to the `forecasting` directory. Run the script `forecasting_funcs.R` to load the necessary libraries and utility functions.

Run each of the independent models:
1. Baseline model:
   - `province_baseline_forecaster.R`
1. Bayesian model:
   - `province_historical_bayesian_forecasting.R`
   - `province_bayesian_forecasting.R`
1. TCN, SARIMA and TimeGPT models are run through a Python Jupyter notebook:
   - Preprocess the data from R: `province_python_setup.R`
   - Run the Jupyter notebook: `python_peru_forecast.ipynb`
   - Read the data back into R: `province_python_forecasting.R`

The data can then be merged and the ensemble formed. The following scripts form the ensemble, score the model predictions, and produce visualisations for different variables:
| Variable | Script |
| - | - |
| Log cases | `province_log_cases.R` |
| Dengue incidence rate | `province_dir_ensemble_scoring.R` |

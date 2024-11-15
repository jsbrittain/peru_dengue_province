To perform the pipeline presented in https://doi.org/10.1101/2024.10.18.24315690, follow these steps:

**1) Data**
i) Downloading: Raw case data are provided in the file "compressed_data.csv.gz", and province-level population data are provided in "province_pop.xlsx".
The dengue incidence surveillance data are publicly available from the National Centre for Epidemiology, Disease Prevention and Control (Peru CDC) in Peru’s Ministry of Health. We sourced these from https://www.dge.gob.pe/salasituacional. Mid-year population estimates are made available by the National Institute of Statistics and Information of Peru at https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/proy_04.xls. 

We sourced climate data of i) temperature and precipiation from the WorldClim monthly historical climate dataset, available at https://www.worldclim.org/, ii) SPI-6 data from the European Drought Observatory are available at https://jeodpp.jrc.ec.europa.eu/, and iii) The El Niño indices of the ONI and ICEN from the NOAA (https://origin.cpc.ncep.noaa.gov/) and the Geophysical Institute of Peru (http://met.igp.gob.pe). 

Spatial 

ii) Processing: province_01.R and province_02.R processes the raw data to produce a dataset of province-level monthly cases across 2010-2011 inclusive, alongside the corresponding demographic and climate information. 

If users wish to use this dataset and avoid all the previous processsing steps, this provided in the file "ptl_province_inla_df.csv".

**2) Retrospective analyses**

**Forecasting**

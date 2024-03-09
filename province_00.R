#Load all packages 
require(tidyverse)
require(tidymodels)
require("ranger")
require("pec")
require(randomForestSRC)
# require(drf)
require(quantregForest)
require(xgboost)
require(sarimaTD)
require(ggpubr)
require(rlang)
require(forecast)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(kcde)
require("astsa")
require(sarima)
require(SpecsVerification)
require(xtable)
require(epiR)
require(pROC)
require(SpiecEasi)
require(spdep)
require(data.table)
require(scoringRules)
require(scoringutils)
library(plyr)
library(dplyr)
library(ggplot2)
require(geosphere)
require(gridExtra)
require(mgcv)
require("ggh4x")
# install.packages("rphylopic")
require(rphylopic)
require("ggtree")
library(cowplot)
require(ggnewscale)
require(mgcv)
require(MuMIn)
require(texreg)
require(WaveletComp)
require(plyr)
require(RColorBrewer)
require(tidyverse)
require(viridis)
require(xgbstack)
require(densitystackr)
require(covidcast)
library(quantmod)
require(dlnm)
require(SpatialEpi)
require(grid)
require(tsModel)
require(INLA)
library(RColorBrewer)
library(quantmod)
require(maps)
require(caret)
require(leafem)
require(htmlwidgets)
require(PropCIs)
require(mapview)
require(splines)
require(rstan)
require(readxl)
require(ubigeo)
require(remotes)
require(mapsPERU) #For centroids of provinces
require(ISOweek)

gc()


#DIRECTORIES
peru.province.data.dir <- "C:/Users/mills/Documents/peru_dengue/province/data"
peru.province.spei.dir <- "C:/Users/mills/Documents/peru_dengue/province/data/spei"

peru.province.out.dir <- "C:/Users/mills/Documents/peru_dengue/province/output"
peru.province.inla.data.in.dir <- "C:/Users/mills/Documents/peru_dengue/province/INLA/Input"
peru.province.inla.data.out.dir <- "C:/Users/mills/Documents/peru_dengue/province/INLA/Output"
peru.province.wavelet.out.dir <- "C:/Users/mills/Documents/peru_dengue/province/wavelet/Output"
peru.province.wavelet.annual_power.out.dir <- "C:/Users/mills/Documents/peru_dengue/province/wavelet/Output/annual_power"
peru.province.wavelet.multi_power.out.dir <- "C:/Users/mills/Documents/peru_dengue/province/wavelet/Output/multi_power"


#Boundaries
piura_tumbes_lambayeque <- c("Piura","Tumbes","Lambayeque")
peru_district_boundaries2 <- st_read(file.path(peru.province.data.dir, "per_admbnda_adm2_ign_20200714.shp"))
piura_tumbes_lambayeque_boundaries <- subset(peru_district_boundaries2,
                                             peru_district_boundaries2$ADM1_ES %in% piura_tumbes_lambayeque)
tmp2 <- st_as_sf(peru_district_boundaries2)
tmp2 <- as_Spatial(tmp2)

#Set up province areas
province_areas_dt <- data.table(PROVINCE = tmp2$ADM2_ES, REGION_AREA_KM2 = area(tmp2)/ 1000000)
ptl_province_areas_dt <- subset(province_areas_dt, PROVINCE %in% piura_tumbes_lambayeque_boundaries$ADM2_ES)




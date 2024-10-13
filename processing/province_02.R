#Setting up data for wavelet analysis, climate-based modelling, and 
  # probabilistic forecasting


#Extract provinces in Piura, Tumbes, and Lambayeque that registered cases
  # across 2010-2021.
tmp <- subset(monthly_province_peru_cases , REGION %in% piura_tumbes_lambayeque)
piura_tumbes_lambayeque_provinces_with_cases <- unique(tmp$PROVINCE)
piura_tumbes_lambayeque_boundaries_with_cases <- subset(piura_tumbes_lambayeque_boundaries,
                                                        piura_tumbes_lambayeque_boundaries$ADM2_ES %in% 
                                                          piura_tumbes_lambayeque_provinces_with_cases)
#Making graph for climate-based model
province_neighbour_list <- poly2nb(st_make_valid(piura_tumbes_lambayeque_boundaries))
nb2INLA(file.path(peru.province.inla.data.in.dir, "nbr_piura_tumbes_lambayeque.graph"),
        province_neighbour_list)

#Specifying PC Priors (Simpson, 2017) ----
prior.prec <- list(prec = list(prior = "pc.prec",
                               param = c(0.5, 0.01) )) 

#Setting up lagged variables ----
maximum_climate_lag <- 4
# ptl_climate_dt_province <- subset(climate_dt_province, 
#                                   REGION %in% piura_tumbes_lambayeque)
# climate_dt_province
ptl_climate_dt_province <- subset(climate_dt_province,
                                  REGION %in% piura_tumbes_lambayeque &
                                    PROVINCE %in% piura_tumbes_lambayeque_districts_with_cases)
length(unique(ptl_climate_dt_province$PROVINCE))

setkeyv(ptl_climate_dt_province, c("YEAR", "MONTH","PROVINCE"))
num_provinces <- length(unique(ptl_climate_dt_province$PROVINCE))

# Year-to-year (Historical Diff)
tmp <- copy(monthly_province_peru_cases)
tmp[, FUTURE_DIR:= Lag(DIR, -12), by = "PROVINCE" ]

#For 1 year in training period (i.e. model development period),
  # for 2010 differences, have to use difference with 2011
tmp[which(is.na(DIFF_WITH_HISTORICAL_DIR)), DIFF_WITH_HISTORICAL_DIR:= DIR - FUTURE_DIR]
tmp[, DIFF_WITH_HISTORICAL_DIR_LAG:= Lag(DIFF_WITH_HISTORICAL_DIR, 1), by = "PROVINCE"]
tmp2 <- subset(tmp, select = c("PROVINCE", "TIME", "DIFF_WITH_HISTORICAL_DIR_LAG"))
setnames(tmp2, "DIFF_WITH_HISTORICAL_DIR_LAG", "MODIFIED_DIFF_WITH_HISTORICAL_DIR_LAG")
monthly_province_peru_cases <- merge(monthly_province_peru_cases,tmp2,
                                     by = c("PROVINCE", "TIME"))

# Maximum temperature (Tmax)
lag_tmax <- tsModel::Lag(ptl_climate_dt_province$tmax, 
                         group = ptl_climate_dt_province$PROVINCE, 
                         k = 0:maximum_climate_lag)
#Only keep from Time Point 5 (May 2010) onwards -
# Reason = We use a 3-month period to evaluate momentum (RSI) and the
# RSI predictor is lagged by 1 month -> May RSI predictor is the value from April
lag_tmax <- lag_tmax[(num_provinces*4+1):nrow(lag_tmax),]
# Minimum temperature (Tmin)
lag_tmin <- tsModel::Lag(ptl_climate_dt_province$tmin, 
                         group = ptl_climate_dt_province$PROVINCE, 
                         k = 0:maximum_climate_lag)
lag_tmin <- lag_tmin[(num_provinces*4+1):nrow(lag_tmin),]


#Precipitation
lag_prec <- tsModel::Lag(ptl_climate_dt_province$prec, 
                         group = ptl_climate_dt_province$PROVINCE, 
                         k = 0:maximum_climate_lag)
lag_prec <- lag_prec[(num_provinces*4+1):nrow(lag_prec),]

# SPI-6
lag_spi <- tsModel::Lag(ptl_climate_dt_province$SPI_6, 
                        group = ptl_climate_dt_province$PROVINCE, 
                        k = 0:maximum_climate_lag)
lag_spi <- lag_spi[(num_provinces*4+1):nrow(lag_spi),]

# ONI
lag_oni <- tsModel::Lag(ptl_climate_dt_province$ANOM, 
                        group = ptl_climate_dt_province$PROVINCE,
                        k = 0:maximum_climate_lag)
lag_oni <- lag_oni[(num_provinces*4+1):nrow(lag_oni),]


#ICEN
lag_icen <- tsModel::Lag(ptl_climate_dt_province$E_INDEX, 
                         group = ptl_climate_dt_province$PROVINCE,
                         k = 0:maximum_climate_lag)
lag_icen <- lag_icen[(num_provinces*4+1):nrow(lag_icen),]


# tmax_roll_2
lag_tmax_roll_2 <- tsModel::Lag(ptl_climate_dt_province$tmax_roll_2, 
                                group = ptl_climate_dt_province$PROVINCE, 
                                k = 0:maximum_climate_lag)
lag_tmax_roll_2 <- lag_tmax_roll_2[(num_provinces*4+1):nrow(lag_tmax_roll_2),]

# tmin_roll_2
lag_tmin_roll_2 <- tsModel::Lag(ptl_climate_dt_province$tmin_roll_2, 
                                group = ptl_climate_dt_province$PROVINCE, 
                                k = 0:maximum_climate_lag)
lag_tmin_roll_2 <- lag_tmin_roll_2[(num_provinces*4+1):nrow(lag_tmin_roll_2),]


# prec_roll_2
lag_prec_roll_2 <- tsModel::Lag(ptl_climate_dt_province$prec_roll_2, 
                                group = ptl_climate_dt_province$PROVINCE, 
                                k = 0:maximum_climate_lag)
lag_prec_roll_2 <- lag_prec_roll_2[(num_provinces*4+1):nrow(lag_prec_roll_2),]



#Setting up data.table used in wavelet analysis, climate-based modelling, and forecasting ----
#For model development, we will subset this data.table to only include data between
  # 2010 and 2017 (inclusive)
ptl_province_inla_df <- copy(monthly_province_peru_cases)
ptl_province_inla_df
#Merging in areas dt will subset to the provinces in Piura, Tumbes, and Lambayeque
  # that registered cases across 2010-2021
ptl_province_inla_df <- merge(ptl_province_inla_df, ptl_province_areas_dt, by = "PROVINCE")
ptl_province_inla_df[, POP_DENSITY:= POP/REGION_AREA_KM2]
ptl_province_inla_df <- subset(ptl_province_inla_df, select = c("PROVINCE","REGION", "TIME","MONTH", "YEAR", "ym_cases", 
                                                                "DIR","POP"))
ptl_province_inla_df[, POP_OFFSET:= POP/100000]
setnames(ptl_province_inla_df, "ym_cases", "CASES")
ptl_province_inla_df <- ptl_province_inla_df[which(TIME > 4)]
ptl_province_inla_df[, TIME:= TIME - 4]
setkeyv(ptl_province_inla_df, c("PROVINCE", "TIME"))

#Setting up region indexing
tmp2 <- unique(ptl_province_inla_df$REGION)
tmp <- data.table(REGION = tmp2, RGN_IND = c(1:length(tmp2)))
merge(ptl_province_inla_df, tmp, by = "REGION")
ptl_province_inla_df <- merge(ptl_province_inla_df, tmp, by = "REGION")
setkeyv(ptl_province_inla_df, c("PROVINCE", "TIME"))

#Setting up province indexing ----
tmp2 <- unique(ptl_province_inla_df$PROVINCE)
tmp <- data.table(PROVINCE = tmp2, PROV_IND = c(1:length(tmp2)))
merge(ptl_province_inla_df, tmp, by = "PROVINCE")
ptl_province_inla_df <- merge(ptl_province_inla_df, tmp, by = "PROVINCE")


#Binary seasonality variable ----
summer_months <- c(12, seq(1, 4))
ptl_province_inla_df[, SEASON:= if_else(MONTH %in% summer_months, 1, 0)]




#Momentum Indicator ----
tmp <- subset(monthly_province_peru_cases , PROVINCE %in% piura_tumbes_lambayeque_districts_with_cases)
setkeyv(tmp, c("PROVINCE", "TIME"))
tmp2 <- tmp[, list(RSI_DIR = RSI(DIR, n = 3)), by = c("PROVINCE")]
tmp[, RSI_DIR:= tmp2$RSI_DIR]
tmp <- subset(tmp, select = c("RSI_DIR", "PROVINCE", "TIME"))
tmp[, TIME:= TIME - 4]
ptl_province_inla_df <- merge(ptl_province_inla_df, tmp, by = c("PROVINCE", "TIME"))
tmp[, RSI_DIR_LAG:= Lag(RSI_DIR, 1), by = "PROVINCE"]
tmp[, RSI_DIR_LAG_2:= Lag(RSI_DIR, 2), by = "PROVINCE"]
tmp_lag <- subset(tmp, select = c("RSI_DIR_LAG", "PROVINCE", "TIME"))
tmp_lag2 <- subset(tmp, select = c("RSI_DIR_LAG_2", "PROVINCE", "TIME"))

ptl_province_inla_df <- merge(ptl_province_inla_df, tmp_lag, by = c("PROVINCE", "TIME"))
ptl_province_inla_df <- merge(ptl_province_inla_df, tmp_lag2, by = c("PROVINCE", "TIME"))
ptl_province_inla_df[which(is.na(RSI_DIR_LAG_2)), SQ_RSI_DIR_LAG:= RSI_DIR_LAG^2]
ptl_province_inla_df[which(!is.na(RSI_DIR_LAG_2)), SQ_RSI_DIR_LAG:= (RSI_DIR_LAG_2*RSI_DIR_LAG)]
ptl_province_inla_df[which(is.na(RSI_DIR_LAG)), RSI_DIR_LAG:= 0]
ptl_province_inla_df[which(is.na(SQ_RSI_DIR_LAG)), SQ_RSI_DIR_LAG:= 0]



#Merge in LONGITUDE and LATITUDE (for plotting later) ----
ptl_province_inla_df <- merge(ptl_province_inla_df, 
                              subset(ptl_province_peru_dt, 
                                     select = c("PROVINCIA","coords_x", "coords_y")),
                              by.x = "PROVINCE", by.y = "PROVINCIA")
setnames(ptl_province_inla_df, c("coords_x", "coords_y"), c("longitude", "latitude"))
ptl_province_inla_df


#Important **************
#Set key of data.table ----
setkeyv(ptl_province_inla_df, c("TIME", "PROVINCE"))



#Merge in Climate Data ----
tmp <- copy(ptl_climate_dt_province)
tmp[, TIME:= NULL]
ptl_province_inla_df <- merge(ptl_province_inla_df, tmp, by = c("PROVINCE","REGION", "YEAR", "MONTH"))


#Census Data ----
ptl_province_inla_df <- merge(ptl_province_inla_df,
                              unique(subset(monthly_province_peru_cases, select = c("PROVINCE", "PROPN_URBAN_2017", "PROPN_URBAN_2007"))),
                              by = "PROVINCE")
ptl_province_inla_df
tmp <- unique(subset(ptl_province_inla_df, 
                     select = c("PROVINCE", "PROPN_URBAN_2017")))
tmp <- tmp[order(PROPN_URBAN_2017, decreasing = FALSE), ]
tmp[, URBAN_ORDER:= seq(1, nrow(tmp))] #Province urban indexing (low to high)
ordered_urban_province_names <- unique(tmp$PROVINCE)
ptl_province_inla_df <- merge(ptl_province_inla_df, subset(tmp, select = c("PROVINCE", "URBAN_ORDER")),
                              by = "PROVINCE")
setkeyv(ptl_province_inla_df, c("TIME", "PROVINCE"))

ptl_province_peru_dt <- merge(ptl_province_peru_dt, 
                              unique(subset(ptl_province_inla_df, select = c("PROPN_URBAN_2007","PROPN_URBAN_2017", "PROVINCE"))),
                              by.x = "PROVINCIA", by.y = "PROVINCE")
ptl_province_peru_dt

#Set key of data.table ----
setkeyv(ptl_province_inla_df, c("TIME", "PROVINCE"))

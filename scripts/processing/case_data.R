library(sf)
library(zoo)
library(spdep)
library(dplyr)
library(terra)
library(readxl)
library(raster)
library(logger)
library(stringr)
library(tsModel)
library(ISOweek)
library(quantmod)
library(lubridate)
library(data.table)
library(rgeoboundaries)

# Ensure summarize() is taken from dplyr
library(conflicted)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(tsModel::Lag)

province.base.dir <- file.path(getwd(), "data")
case_data.in.dir <- file.path(province.base.dir, "cases")
province.out.dir <- file.path(province.base.dir, "output")
province.out.dir <- file.path(province.base.dir, "output")
climate.data.dir <- file.path(province.base.dir, "climate")
province.data.dir <- file.path(province.base.dir, "province")
shapefiles.data.dir <- file.path(province.base.dir, "shapefiles")
province.python.data.dir <- file.path(province.base.dir, "python/data")
province.inla.data.in.dir <- file.path(province.base.dir, "INLA/Input")

# Ensure output folder exists
dir.create(province.python.data.dir, recursive = TRUE, showWarnings = FALSE)

# ### Note: ###
# Data are truncated between May-10 and Dec-21 to match the original analysis.

# District boundaries
peru_district_boundaries2 <- st_read(file.path(shapefiles.data.dir,
  "per_admbnda_adm2_ign_20200714.shp"))
piura_tumbes_lambayeque <- c("Piura", "Tumbes", "Lambayeque")
piura_tumbes_lambayeque_boundaries <- subset(peru_district_boundaries2,
  peru_district_boundaries2$ADM1_ES %in% piura_tumbes_lambayeque)

# --- Load data ----------------------------------------------------------------

# Case data
cases_file <- file.path(case_data.in.dir, "2010_2021_cases_full_data.csv")
log_info(paste0("Read in raw cases: ", cases_file))
raw_peru_cases <- data.table(read.csv(cases_file))

# Province population
log_info("Read in province population data")
raw_province_peru_pops <- data.table(read.csv(file.path(
  province.data.dir,
  "province_pop.csv"
), encoding = "Latin-1"))

# Census data (2007)
log_info("Read in 2007 census data")
raw_peru_2007_census_data <- data.table(readxl::read_excel(file.path(
  province.data.dir,
  "2007_census_data.xlsx"
), sheet = 1))

# Census data (2017)
log_info("Read in 2017 census data")
raw_peru_2017_census_data <- data.table(readxl::read_excel(file.path(
  province.data.dir,
  "census_data.xlsx"
), sheet = 1, skip = 2))

# --- Process case data --------------------------------------------------------

# Aggregate case counts from raw_peru_cases
dept_prob_conf_cases <- raw_peru_cases[
  , .(TOTAL_CASES = .N),
  by = c("Provincia", "Departamnento", "Semana", "Ano", "Tipo.de.diagnóstico")
]

# Rename columns
setnames(
  dept_prob_conf_cases,
  old = c("Provincia", "Departamnento", "Semana", "Ano", "Tipo.de.diagnóstico"),
  new = c("PROVINCE", "REGION", "WEEK", "YEAR", "CASE_TYPE")
)

# Title-case province and region names
dept_prob_conf_cases[
  , `:=`(
    PROVINCE = str_to_title(PROVINCE),
    REGION = str_to_title(REGION)
  )
]

# Region name replacements
region_replacements <- c(
  "Madre De Dios" = "Madre de Dios",
  "San Martin" = "San Martín",
  "Junin" = "Junín",
  "Huanuco" = "Huánuco"
)
dept_prob_conf_cases[, REGION := ifelse(
  REGION %in% names(region_replacements),
  region_replacements[REGION],
  REGION
)]

# Remove cases without an assigned province
dept_prob_conf_cases <- dept_prob_conf_cases[PROVINCE != ""]

# Province-year combinations
weeks <- 1:52
case_types <- unique(dept_prob_conf_cases$CASE_TYPE)
province_year <- unique(dept_prob_conf_cases[, .(PROVINCE, YEAR)])
province_year_comb <- unique(dept_prob_conf_cases[, .(PROVINCE, YEAR)])

all_combinations <- province_year[
  , CJ(WEEK = weeks, CASE_TYPE = case_types, sorted = FALSE),
  by = .(PROVINCE, YEAR)
]

# ------------------------------------------------------------------------------

# Merge the original data.table with the full combinations to include
# missing weeks; create an ISO Date column
log_info(paste0("Merge the original data.table with the full combinations to ",
                "include missing weeks"))
filled_dt <- merge(all_combinations, dept_prob_conf_cases,
  by = c("PROVINCE", "YEAR", "WEEK", "CASE_TYPE"), all.x = TRUE, all.y = TRUE
)
filled_dt[, YEAR_WEEK := paste0(YEAR, "-W", WEEK, "-1")]
filled_dt[which(WEEK < 10), YEAR_WEEK := paste0(YEAR, "-W0", WEEK, "-1")]
filled_dt[, REPORTED_DATE := ISOweek2date(filled_dt$YEAR_WEEK)] # class=Date
filled_dt[, MONTH := substr(REPORTED_DATE, 1, 7)] # trim day(date), class=char

# Create Monthly dt
log_info("Create Monthly dt")
monthly_dept_prob_conf_cases <- filled_dt[, list(ym_cases = sum(TOTAL_CASES,
  na.rm = TRUE
)), by = c("PROVINCE", "MONTH", "CASE_TYPE")]
monthly_dept_prob_conf_cases <- as.data.table(monthly_dept_prob_conf_cases)

monthly_dept_prob_conf_cases[, m := substr(MONTH, nchar(MONTH) -
  1, nchar(MONTH))]
monthly_dept_prob_conf_cases[, YEAR := substr(
  MONTH, 1,
  4
)]
monthly_dept_prob_conf_cases[, m := as.numeric(m)] # m is numeric month 1-12
monthly_dept_prob_conf_cases[, MONTH := NULL] # Remove old Month col
setnames(monthly_dept_prob_conf_cases, "m", "MONTH") # rename m to MONTH
monthly_dept_prob_conf_cases[, YEAR := as.numeric(YEAR)]

# Merge in total collapsed (confirmed+reported) cases
tmp <- monthly_dept_prob_conf_cases[, list(TOTAL_CASES = sum(ym_cases)),
  by = c("MONTH", "PROVINCE", "YEAR")
]
# adds a col (TOTAL_CASES) with combined case counts (prob and confirmed)
monthly_dept_prob_conf_cases <- merge(monthly_dept_prob_conf_cases,
  tmp,
  by = c("MONTH", "PROVINCE", "YEAR")
)
# extract confirmed cases
confirmed_monthly_cases <- monthly_dept_prob_conf_cases[
  which(CASE_TYPE =="Confirmados"),
  list(CONFIRMED = ym_cases),
  by = c("MONTH", "PROVINCE", "YEAR")
]
# adds a col (CONFIRMED) with confirmed case counts (prob and confirmed)
monthly_dept_prob_conf_cases <- merge(monthly_dept_prob_conf_cases,
  confirmed_monthly_cases,
  by = c("MONTH", "PROVINCE", "YEAR")
)
# add 'proportion confirmed' col
monthly_dept_prob_conf_cases[which(TOTAL_CASES != 0), PROPN_CONFIRMED :=
  CONFIRMED / TOTAL_CASES]
# prop_confirmed = 0 where total_cases = 0 (were NA)
monthly_dept_prob_conf_cases[which(TOTAL_CASES == 0), PROPN_CONFIRMED :=
  0]

# Collapsing probable + confirmed ----
log_info("Collapsing probable + confirmed")
district_peru_cases <- raw_peru_cases[, list(TOTAL_CASES = length(Sexo)),
  by = c("Distrito", "Provincia", "Departamnento", "Semana", "Ano")
]
setnames(district_peru_cases, colnames(district_peru_cases), c(
  "DISTRICT",
  "PROVINCE", "REGION", "WEEK", "YEAR", "TOTAL_CASES"
))
district_peru_cases[, PROVINCE := str_to_title(PROVINCE)]
district_peru_cases[, DISTRICT := str_to_title(DISTRICT)]
district_peru_cases[, REGION := str_to_title(REGION)]

missing_regions_from_cases <- unique(district_peru_cases$REGION)[which(!(unique(district_peru_cases$REGION) %in% peru_district_boundaries2$ADM1_ES))]
print(missing_regions_from_cases) # displays: "Madre De Dios" ""
district_peru_cases[which(REGION == "Madre De Dios"), REGION := "Madre de Dios"]

# ------------------------------------------------------------------------------

province_peru_cases <- district_peru_cases[,
  list(TOTAL_CASES = sum(TOTAL_CASES)),
  by = c("PROVINCE", "WEEK", "YEAR")
]
setkeyv(province_peru_cases, c("PROVINCE", "YEAR", "WEEK"))
province_peru_cases <- province_peru_cases[which(PROVINCE != "")]
province_peru_cases

province_year_comb <- province_peru_cases[, .(PROVINCE, YEAR), by = c(
  "PROVINCE",
  "YEAR"
)]
# Create a data.table with all weeks from 1 to 52
all_weeks <- data.table(week = 1:52)
# Perform a cross join to get all possible combinations of
# PROVINCE-YEAR-week
log_info("Cross join to get all possible combinations of PROVINCE-YEAR-week")
all_combinations <- unique(CJ(
  PROVINCE = province_year_comb$PROVINCE, YEAR = province_year_comb$YEAR,
  WEEK = all_weeks$week
))

# Merge the original data.table with the full combinations to include
# missing weeks
log_info("Merge the original data to include missing weeks")
filled_dt <- merge(all_combinations, province_peru_cases, by = c(
  "PROVINCE",
  "YEAR", "WEEK"
), all.x = TRUE)
filled_dt <- filled_dt[which(PROVINCE != "")]
filled_dt[, YEAR_WEEK := paste0(YEAR, "-W", WEEK, "-1")]
filled_dt[which(WEEK < 10), YEAR_WEEK := paste0(YEAR, "-W0", WEEK, "-1")]
filled_dt[, REPORTED_DATE := ISOweek2date(filled_dt$YEAR_WEEK)]
filled_dt[, MONTH := substr(REPORTED_DATE, 1, 7)]
# Set up region-province dt (for future repeated usage)
log_info("Set up region-province dt")
region_province <- unique(subset(district_peru_cases, select = c(
  "PROVINCE",
  "REGION"
)))

# Set up Piura-Tumbes-Lambayeque province-region names data.table
log_info("Set up Piura-Tumbes-Lambayeque province-region names data.table")
ptl_region_province <- subset(region_province, REGION %in%
                              piura_tumbes_lambayeque)
ptl_region_province
filled_dt <- subset(filled_dt, PROVINCE %in% ptl_region_province$PROVINCE)
filled_dt <- merge(filled_dt, ptl_region_province, by = "PROVINCE")
setkeyv(filled_dt, c("PROVINCE", "REGION"))

# Set up monthly
log_info("Set up monthly")
monthly_province_peru_cases <- filled_dt %>%
  group_by(PROVINCE, MONTH) %>%
  summarize(ym_cases = sum(TOTAL_CASES, na.rm = TRUE))
monthly_province_peru_cases <- as.data.table(monthly_province_peru_cases)
monthly_province_peru_cases[, YEAR := as.numeric(substr(MONTH, 1, 4))]
monthly_province_peru_cases[, MONTH := as.numeric(substr(MONTH, 6, 8))]
monthly_province_peru_cases

monthly_province_peru_cases[, length(MONTH), by = "PROVINCE"]
unique(monthly_province_peru_cases$PROVINCE)
monthly_province_peru_cases <- monthly_province_peru_cases[which(PROVINCE !=
  0)]
monthly_province_peru_cases

# Population Data ----
log_info("Population Data")
province_peru_pops <- subset(raw_province_peru_pops, Status == "Province")
setnames(province_peru_pops, colnames(province_peru_pops)[3:6], c(
  "11/07/1993",
  "21/10/2007", "22/10/2017", "30/06/2022"
))
province_peru_pops[, `11/07/1993` := NULL]
province_peru_pops[, Status := NULL]

setnames(province_peru_pops, "Name", "PROVINCE")
fill_province_pops_dt <- function(data) {
  min_yr <- min(unique(data$YEAR))
  max_yr <- max(unique(data$YEAR))

  filled_pops <- data.table(
    YEAR = rep(seq(min_yr, max_yr, by = 1),
    each = 12 * length(unique(data$PROVINCE))),
    MONTH = rep(seq(1, 12, by = 1),
      length(seq(2007, 2022, by = 1)) * length(unique(data$PROVINCE))))

  filled_pops  # return
}

province_peru_pops <- melt(province_peru_pops,
  id.vars = "PROVINCE", variable.name = "YEAR",
  value.name = "POP"
)
province_peru_pops <- data.table(province_peru_pops)
province_peru_pops[, MONTH := as.numeric(as.character(substr(
  YEAR, 4,
  5
)))]
province_peru_pops[, YEAR := as.numeric(as.character(substr(YEAR, 7, 11)))]
province_peru_pops_filled <- fill_province_pops_dt(province_peru_pops)
province_peru_pops_filled[, length(MONTH), by = "YEAR"]
setkeyv(province_peru_pops_filled, c("YEAR", "MONTH"))
province_peru_pops_filled[, PROVINCE := rep(
  unique(province_peru_pops$PROVINCE),
  nrow(province_peru_pops_filled) / length(unique(province_peru_pops$PROVINCE))
)]
setkeyv(province_peru_pops_filled, c("PROVINCE", "YEAR", "MONTH"))
province_peru_pops_filled <- merge(
  province_peru_pops_filled,
  province_peru_pops,
  by = c("YEAR", "MONTH", "PROVINCE"),
  all.x = TRUE
)
setkeyv(province_peru_pops_filled, c("PROVINCE", "YEAR", "MONTH"))

num_times <- nrow(province_peru_pops_filled) /
  length(unique(province_peru_pops_filled$PROVINCE))
province_peru_pops_filled[, TIME := rep(seq(1, 192), length(unique(PROVINCE)))]

province_peru_pops_filled <- province_peru_pops_filled[which(TIME >= 7)]
for (i in 1:length(unique(province_peru_pops_filled$PROVINCE))) {
  rgn_in_q <- unique(province_peru_pops_filled$PROVINCE)[i]
  tmp <- subset(province_peru_pops_filled, PROVINCE == rgn_in_q)
  tmp2 <- na.approx(tmp$POP, xout = 1:186)
  province_peru_pops_filled[
    which(PROVINCE == rgn_in_q & TIME <= 183),
    POP := tmp2
  ]
}
province_peru_pops_filled <- subset(province_peru_pops_filled, TIME <= 183)

# Fixing different names in population and case datasets
log_info("Fixing different names in population and case datasets")
tmp <- subset(province_peru_pops_filled, YEAR >= 2010)
tmp[, TIME := NULL]
missing_from_pop_data <- which(
  !(unique(tmp$PROVINCE) %in% monthly_province_peru_cases$PROVINCE))
tmp_names <- unique(subset(tmp, select = c("PROVINCE")))
for (i in 1:nrow(tmp_names)) {
  closest_match <- unique(peru_district_boundaries2$ADM2_ES)[
    which.min(stringdist::stringdist(
      unique(tmp_names$PROVINCE)[i],
      unique(peru_district_boundaries2$ADM2_ES)))]
  tmp_names$PROVINCE_NEW[i] <- closest_match
}

# Set province to be province_new
log_info("Set province to be province_new")
for (i in 1:nrow(tmp)) {
  tmp[which(PROVINCE == tmp_names$PROVINCE[i]),
      PROVINCE_NEW := tmp_names$PROVINCE_NEW[i]]
}
tmp[, PROVINCE := NULL]
setnames(tmp, "PROVINCE_NEW", "PROVINCE")

missing_from_pop_data <- which(
  !(unique(monthly_province_peru_cases$PROVINCE) %in% tmp$PROVINCE))
unique(monthly_province_peru_cases$PROVINCE)[missing_from_pop_data]
tmp[which(PROVINCE == "Datem del Marañon"), PROVINCE := "Datem Del Marañon"]
tmp[which(PROVINCE == "Dos de Mayo"), PROVINCE := "Dos De Mayo"]
tmp[which(PROVINCE == "Paucar del Sara Sara"),
  PROVINCE := "Paucar Del Sara Sara"]
tmp[which(PROVINCE == "Rodriguez de Mendoza"),
  PROVINCE := "Rodriguez De Mendoza"]

# Merge in population data
log_info("Merge in population data")
monthly_province_peru_cases <- merge(monthly_province_peru_cases, tmp, by = c(
  "PROVINCE",
  "YEAR", "MONTH"
))
monthly_province_peru_cases[which(is.na(ym_cases))] # Check no N/A values
monthly_province_peru_cases[, DIR := ym_cases / POP * 1e+05]
monthly_province_peru_cases

# ------------------------------------------------------------------------------

# URBAN 2007 and 2017 Data ----
log_info("URBAN 2007 and 2017 Data")
peru_2007_census_data <- copy(raw_peru_2007_census_data)
peru_2007_census_data <- peru_2007_census_data[, 1:3]
setnames(peru_2007_census_data, c("AREA_TYPE", "PROVINCE", "PROPN"))
peru_2007_census_province_names_dt <- peru_2007_census_data[
  grepl("Prov.", peru_2007_census_data$PROVINCE), ]
peru_2007_census_province_names <- sub(
  ".*Prov. ", "", peru_2007_census_province_names_dt$PROVINCE)
peru_2007_census_province_urban_propns <- as.numeric(
  peru_2007_census_data[
    grepl("Urbano", peru_2007_census_data$AREA_TYPE), ]$PROPN)
peru_2007_census_province_rural_propns <- as.numeric(
  peru_2007_census_data[
    grepl("Rural", peru_2007_census_data$AREA_TYPE), ]$PROPN)

# Fixing missing Rural entry for Constitucional del Callao; should be 0%
log_info("Fixing missing Rural entry for Constitucional del Callao")
peru_2007_census_province_urban_propns + peru_2007_census_province_rural_propns
length(peru_2007_census_province_rural_propns)
length(peru_2007_census_province_urban_propns)
tmp <- peru_2007_census_province_urban_propns +
  peru_2007_census_province_rural_propns
peru_2007_census_province_names[which(tmp == 104.35)]
peru_2007_census_province_rural_propns <- c(
  peru_2007_census_province_rural_propns[1:66],
  0,
  peru_2007_census_province_rural_propns[
    67:length(peru_2007_census_province_rural_propns)]
)
peru_2007_census_province_urban_propns <- peru_2007_census_province_urban_propns[1:(length(peru_2007_census_province_urban_propns) -
  1)]
peru_2007_census_province_rural_propns <- peru_2007_census_province_rural_propns[1:(length(peru_2007_census_province_rural_propns) -
  1)]
length(peru_2007_census_province_names)

peru_2007_census_data <- data.table(
  PROVINCE = peru_2007_census_province_names,
  PROPN_URBAN_2007 = peru_2007_census_province_urban_propns,
  PROPN_RURAL_2007 = peru_2007_census_province_rural_propns
)

# Note that we only use 2017 as a continuous variable in our analysis
log_info("2017 Census Data")
peru_2017_census_data <- copy(raw_peru_2017_census_data)
peru_2017_census_data <- subset(
  peru_2017_census_data,
  select = colnames(peru_2017_census_data)[c(1, 2, 5, 8)])
setnames(peru_2017_census_data,
  c("PROVINCE", "TOTAL", "TOTAL_URBAN", "TOTAL_RURAL"))
province_ids <- (grepl("PROVINCIA", peru_2017_census_data$PROVINCE))
peru_2017_census_data <- peru_2017_census_data[c(province_ids), ]
peru_2017_census_data[, PROVINCE := substr(PROVINCE, 11, length(PROVINCE))]
peru_2017_census_data[, PROVINCE := str_to_title(PROVINCE)]
peru_2017_census_data[, TOTAL_URBAN := as.numeric(TOTAL_URBAN)]
peru_2017_census_data[, TOTAL_RURAL := as.numeric(TOTAL_RURAL)]
peru_2017_census_data[, PROPN_URBAN_2017 := TOTAL_URBAN / TOTAL]

# Aligning 2007 Census Province Names with case data
log_info("Aligning 2007 Census Province Names with case data")
tmp_peru_2007_census_names <- unique(subset(
  peru_2007_census_data, select = c("PROVINCE")))
tmp_peru_2007_census_names

# Merge in census data
log_info("Merge in census data")
tmp_monthly_province_peru_cases_names <- unique(subset(monthly_province_peru_cases,
  select = c("PROVINCE")
))
for (i in 1:nrow(tmp_monthly_province_peru_cases_names)) {
  closest_match <- unique(tmp_peru_2007_census_names$PROVINCE)[which.min(stringdist::stringdist(
    unique(tmp_monthly_province_peru_cases_names$PROVINCE)[i],
    unique(tmp_peru_2007_census_names$PROVINCE)
  ))]
  tmp_monthly_province_peru_cases_names$PROVINCE_NEW[i] <- closest_match
}

# PROVINCE_NEW = Name given in census data
tmp <- copy(peru_2007_census_data)
setnames(tmp, "PROVINCE", "PROVINCE_NEW")
tmp <- merge(tmp, tmp_monthly_province_peru_cases_names, by = "PROVINCE_NEW")
tmp
monthly_province_peru_cases
merge(monthly_province_peru_cases, subset(tmp, select = c("PROVINCE", "PROPN_URBAN_2007")),
  by = "PROVINCE"
)
monthly_province_peru_cases <- merge(monthly_province_peru_cases, subset(tmp,
  select = c("PROVINCE", "PROPN_URBAN_2007")
), by = "PROVINCE")
monthly_province_peru_cases[, PROPN_URBAN_2007 := PROPN_URBAN_2007 / 100]
tmp_peru_2017_census_names <- unique(subset(peru_2017_census_data, select = c("PROVINCE")))

# Merge in census data
tmp_monthly_province_peru_cases_names <- unique(subset(monthly_province_peru_cases,
  select = c("PROVINCE")
))
for (i in 1:nrow(tmp_monthly_province_peru_cases_names)) {
  closest_match <- unique(tmp_peru_2017_census_names$PROVINCE)[which.min(stringdist::stringdist(
    unique(tmp_monthly_province_peru_cases_names$PROVINCE)[i],
    unique(tmp_peru_2017_census_names$PROVINCE)
  ))]
  tmp_monthly_province_peru_cases_names$PROVINCE_NEW[i] <- closest_match
}

# PROVINCE_NEW = Name given in census data
tmp <- copy(peru_2017_census_data)
setnames(tmp, "PROVINCE", "PROVINCE_NEW")
tmp <- merge(tmp, tmp_monthly_province_peru_cases_names, by = "PROVINCE_NEW")

monthly_province_peru_cases <- merge(monthly_province_peru_cases, subset(tmp,
  select = c("PROVINCE", "PROPN_URBAN_2017")
), by = "PROVINCE")

# DIR Historical Difference DATA----
log_info("DIR Historical Difference DATA")
num_months <- nrow(monthly_province_peru_cases) /
  length(unique(monthly_province_peru_cases$PROVINCE))
setkeyv(monthly_province_peru_cases, c("PROVINCE", "YEAR", "MONTH"))
monthly_province_peru_cases[,
  TIME := rep(seq(1, num_months), length(unique(PROVINCE)))]















# Census Data ----
log_info("Merging in census data")
ptl_province_inla_df <- copy(monthly_province_peru_cases)
ptl_province_inla_df
tmp <- unique(subset(
  ptl_province_inla_df,
  select = c("PROVINCE", "PROPN_URBAN_2017")))
tmp <- tmp[order(PROPN_URBAN_2017, decreasing = FALSE), ]
tmp[, URBAN_ORDER := seq(1, nrow(tmp))] # Province urban indexing (low to high)
ordered_urban_province_names <- unique(tmp$PROVINCE)
ptl_province_inla_df <- merge(ptl_province_inla_df, subset(tmp, select = c(
  "PROVINCE",
  "URBAN_ORDER"
)), by = "PROVINCE")

#
setkeyv(ptl_province_inla_df, c("PROVINCE", "TIME"))
ptl_province_inla_df[, end_of_month := as.Date(ceiling_date(ymd(paste(
  YEAR, MONTH,
  1
)), "month") - days(1))]

setnames(ptl_province_inla_df, "ym_cases", "CASES")
ptl_province_inla_df[, PROPN_URBAN_2007 := NULL]
ptl_province_inla_df[, PROPN_URBAN_2017 := NULL]
ptl_province_inla_df[, URBAN_ORDER := NULL]
ptl_province_inla_df[, TIME := NULL]

# Add DISTRICT
ptl_province_inla_df <- merge(ptl_province_inla_df, region_province,
  by = "PROVINCE", all.x = TRUE)

# Remove data before May-2010 and after Dec-2021 to match original analysis
ptl_province_inla_df <- ptl_province_inla_df[
  ptl_province_inla_df$end_of_month >= as.Date("2010-05-01") &
    ptl_province_inla_df$end_of_month <= as.Date("2021-12-31")]

log_info("Writing case data...")
# REGION, PROVINCE, YEAR, MONTH, CASES, POP, [DIR], end_of_month
fwrite(ptl_province_inla_df,
       file.path(province.python.data.dir, "ptl_province_inla_df.csv"))

log_info("Finished case_data.R")

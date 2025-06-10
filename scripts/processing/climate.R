library(sf)
library(zoo)
library(dplyr)
library(terra)
library(readxl)
library(raster)
library(logger)
library(stringr)
library(tsModel)
library(ISOweek)
library(data.table)
library(rgeoboundaries)

source("scripts/processing/funcs/worldclim.R")

# Environment variables
SNAKEMAKE_CORES <- Sys.getenv("SNAKEMAKE_CORES", unset = "1")

spi_file_name_func <- function(year) {
  file.path(peru.spi6.in.dir, paste0(
    "spg06_m_wld_", year, "0101_", year,
    "1201_m.nc"
  ))  # implicit return
}

peru.province.base.dir <- file.path(getwd(), "data")
peru.case_data.in.dir <- file.path(peru.province.base.dir, "cases")
peru.shapefiles.data.dir <- file.path(peru.province.base.dir, "shapefiles")
peru.province.data.dir <- file.path(peru.province.base.dir, "province")
peru.spi6.in.dir <- file.path(peru.province.base.dir, "spi6")
peru.province.out.dir <- file.path(peru.province.base.dir, "output")
peru.climate.data.dir <- file.path(peru.province.base.dir, "climate")

dir.create(peru.province.out.dir, recursive = TRUE, showWarnings = FALSE)

# District boundaries
peru_district_boundaries2 <- st_read(file.path(peru.shapefiles.data.dir,
  "per_admbnda_adm2_ign_20200714.shp"))
piura_tumbes_lambayeque <- c("Piura", "Tumbes", "Lambayeque")
piura_tumbes_lambayeque_boundaries <- subset(peru_district_boundaries2,
  peru_district_boundaries2$ADM1_ES %in% piura_tumbes_lambayeque)

peru_sf <- geoboundaries("Peru", "adm1", quiet = TRUE)

# --- Load data ----------------------------------------------------------------

# Case data
cases_file <- file.path(peru.case_data.in.dir, "2010_2021_cases_full_data.csv")
log_info(paste0("Read in raw cases: ", cases_file))
raw_peru_cases <- data.table(read.csv(cases_file))

# Province population
log_info("Read in province population data")
raw_province_peru_pops <- data.table(read.csv(file.path(
  peru.province.data.dir,
  "province_pop.csv"
), encoding = "Latin-1"))

# Census data (2007)
log_info("Read in 2007 census data")
raw_peru_2007_census_data <- data.table(readxl::read_excel(file.path(
  peru.province.data.dir,
  "2007_census_data.xlsx"
), sheet = 1))

# Census data (2017)
log_info("Read in 2017 census data")
raw_peru_2017_census_data <- data.table(readxl::read_excel(file.path(
  peru.province.data.dir,
  "census_data.xlsx"
), sheet = 1, skip = 2))

# ICEN data
log_info("Read in ICEN data")
icen_data <- data.table(read.table(file.path(
  peru.province.base.dir, "icen",
  "ICEN.txt"
), header = FALSE, comment.char = "%"))
setnames(icen_data, colnames(icen_data), c("YEAR", "MONTH", "E_INDEX"))

# ONI data
log_info("Read in ONI data")
oni_data <- data.table(
  read.table(
    file.path(peru.province.base.dir, "oni", "detrend.nino34.ascii.txt"),
    header = TRUE
  )
)
setnames(oni_data, c("YR", "MON"), c("YEAR", "MONTH"))

# --- Process case data --------------------------------------------------------

# Aggregate case counts from raw_peru_cases
department_probable_confirmed_cases <- raw_peru_cases[
  , .(TOTAL_CASES = .N),
  by = c("Provincia", "Departamnento", "Semana", "Ano", "Tipo.de.diagnóstico")
]

# Rename columns
setnames(
  department_probable_confirmed_cases,
  old = c("Provincia", "Departamnento", "Semana", "Ano", "Tipo.de.diagnóstico"),
  new = c("PROVINCE", "REGION", "WEEK", "YEAR", "CASE_TYPE")
)

# Title-case province and region names
department_probable_confirmed_cases[
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
department_probable_confirmed_cases[, REGION := ifelse(
  REGION %in% names(region_replacements),
  region_replacements[REGION],
  REGION
)]

# Remove cases without an assigned province
department_probable_confirmed_cases <- department_probable_confirmed_cases[PROVINCE != ""]

# Province-year combinations
weeks <- 1:52
case_types <- unique(department_probable_confirmed_cases$CASE_TYPE)
province_year <- unique(department_probable_confirmed_cases[, .(PROVINCE, YEAR)])
province_year_comb <- unique(department_probable_confirmed_cases[, .(PROVINCE, YEAR)])

all_combinations <- province_year[
  , CJ(WEEK = weeks, CASE_TYPE = case_types, sorted = FALSE),
  by = .(PROVINCE, YEAR)
]

# ------------------------------------------------------------------------------

# Merge the original data.table with the full combinations to include
# missing weeks; create an ISO Date column
log_info("Merge the original data.table with the full combinations to include missing weeks")
filled_dt <- merge(all_combinations, department_probable_confirmed_cases,
  by = c("PROVINCE", "YEAR", "WEEK", "CASE_TYPE"), all.x = TRUE, all.y = TRUE
)
filled_dt[, YEAR_WEEK := paste0(YEAR, "-W", WEEK, "-1")]
filled_dt[which(WEEK < 10), YEAR_WEEK := paste0(YEAR, "-W0", WEEK, "-1")]
filled_dt[, REPORTED_DATE := ISOweek2date(filled_dt$YEAR_WEEK)] # class=Date
filled_dt[, MONTH := substr(REPORTED_DATE, 1, 7)] # trim day(date), class=char

# Create Monthly dt
log_info("Create Monthly dt")
monthly_department_probable_confirmed_cases <- filled_dt[, list(ym_cases = sum(TOTAL_CASES,
  na.rm = TRUE
)), by = c("PROVINCE", "MONTH", "CASE_TYPE")]
monthly_department_probable_confirmed_cases <- as.data.table(monthly_department_probable_confirmed_cases)

monthly_department_probable_confirmed_cases[, m := substr(MONTH, nchar(MONTH) -
  1, nchar(MONTH))]
monthly_department_probable_confirmed_cases[, YEAR := substr(
  MONTH, 1,
  4
)]
monthly_department_probable_confirmed_cases[, m := as.numeric(m)] # m is numeric month 1-12
monthly_department_probable_confirmed_cases[, MONTH := NULL] # Remove old Month col
setnames(monthly_department_probable_confirmed_cases, "m", "MONTH") # rename m to MONTH
monthly_department_probable_confirmed_cases[, YEAR := as.numeric(YEAR)]

# Merge in total collapsed (confirmed+reported) cases
tmp <- monthly_department_probable_confirmed_cases[, list(TOTAL_CASES = sum(ym_cases)),
  by = c("MONTH", "PROVINCE", "YEAR")
]
# adds a col (TOTAL_CASES) with combined case counts for all rows (prob and confirmed)
monthly_department_probable_confirmed_cases <- merge(monthly_department_probable_confirmed_cases,
  tmp,
  by = c("MONTH", "PROVINCE", "YEAR")
)
# extract confirmed cases
confirmed_monthly_cases <- monthly_department_probable_confirmed_cases[which(CASE_TYPE ==
  "Confirmados"), list(CONFIRMED = ym_cases), by = c(
  "MONTH", "PROVINCE",
  "YEAR"
)]
# adds a col (CONFIRMED) with confirmed case counts to all rows (prob and confirmed)
monthly_department_probable_confirmed_cases <- merge(monthly_department_probable_confirmed_cases,
  confirmed_monthly_cases,
  by = c("MONTH", "PROVINCE", "YEAR")
)
# add 'proportion confirmed' col
monthly_department_probable_confirmed_cases[which(TOTAL_CASES != 0), PROPN_CONFIRMED :=
  CONFIRMED / TOTAL_CASES]
# prop_confirmed = 0 where total_cases = 0 (were NA)
monthly_department_probable_confirmed_cases[which(TOTAL_CASES == 0), PROPN_CONFIRMED :=
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

missing_regions_from_cases <- unique(district_peru_cases$REGION)[which(!(unique(district_peru_cases$REGION) %in%
  peru_district_boundaries2$ADM1_ES))]
print(missing_regions_from_cases) # displays: "Madre De Dios" ""
district_peru_cases[which(REGION == "Madre De Dios"), REGION := "Madre de Dios"]

# ------------------------------------------------------------------------------

province_peru_cases <- district_peru_cases[, list(TOTAL_CASES = sum(TOTAL_CASES)),
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
log_info("Perform a cross join to get all possible combinations of PROVINCE-YEAR-week")
all_combinations <- unique(CJ(
  PROVINCE = province_year_comb$PROVINCE, YEAR = province_year_comb$YEAR,
  WEEK = all_weeks$week
))

# Merge the original data.table with the full combinations to include
# missing weeks
log_info("Merge the original data.table with the full combinations to include missing weeks")
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
ptl_region_province <- subset(region_province, REGION %in% piura_tumbes_lambayeque)
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
raw_province_peru_pops
raw_province_peru_pops
province_peru_pops <- subset(raw_province_peru_pops, Status == "Province")
setnames(province_peru_pops, colnames(province_peru_pops)[3:6], c(
  "11/07/1993",
  "21/10/2007", "22/10/2017", "30/06/2022"
))
province_peru_pops[, `11/07/1993` := NULL]
province_peru_pops[, Status := NULL]
length(rep(seq(2007, 2022, by = 1), each = 12 * length(unique(province_peru_pops$PROVINCE))))

setnames(province_peru_pops, "Name", "PROVINCE")
fill_province_pops_dt <- function(data) {
  min_yr <- min(unique(data$YEAR))
  max_yr <- max(unique(data$YEAR))

  filled_pops <- data.table(YEAR = rep(seq(min_yr, max_yr, by = 1), each = 12 *
    length(unique(data$PROVINCE))), MONTH = rep(seq(1, 12, by = 1), length(seq(2007,
    2022,
    by = 1
  )) * length(unique(data$PROVINCE))))

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
province_peru_pops_filled <- merge(province_peru_pops_filled, province_peru_pops,
  by = c("YEAR", "MONTH", "PROVINCE"), all.x = TRUE
)
setkeyv(province_peru_pops_filled, c("PROVINCE", "YEAR", "MONTH"))

num_times <- nrow(province_peru_pops_filled) / length(unique(province_peru_pops_filled$PROVINCE))
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
missing_from_pop_data <- which(!(unique(tmp$PROVINCE) %in% monthly_province_peru_cases$PROVINCE))
tmp_names <- unique(subset(tmp, select = c("PROVINCE")))
for (i in 1:nrow(tmp_names)) {
  closest_match <- unique(peru_district_boundaries2$ADM2_ES)[which.min(stringdist::stringdist(
    unique(tmp_names$PROVINCE)[i],
    unique(peru_district_boundaries2$ADM2_ES)
  ))]
  tmp_names$PROVINCE_NEW[i] <- closest_match
}

# Set province to be province_new
log_info("Set province to be province_new")
for (i in 1:nrow(tmp)) {
  tmp[which(PROVINCE == tmp_names$PROVINCE[i]), PROVINCE_NEW := tmp_names$PROVINCE_NEW[i]]
}
tmp[, PROVINCE := NULL]
setnames(tmp, "PROVINCE_NEW", "PROVINCE")

missing_from_pop_data <- which(!(unique(monthly_province_peru_cases$PROVINCE) %in%
  tmp$PROVINCE))
unique(monthly_province_peru_cases$PROVINCE)[missing_from_pop_data]
tmp[which(PROVINCE == "Datem del Marañon"), PROVINCE := "Datem Del Marañon"]
tmp[which(PROVINCE == "Dos de Mayo"), PROVINCE := "Dos De Mayo"]
tmp[which(PROVINCE == "Paucar del Sara Sara"), PROVINCE := "Paucar Del Sara Sara"]
tmp[which(PROVINCE == "Rodriguez de Mendoza"), PROVINCE := "Rodriguez De Mendoza"]

# Merge in population data
log_info("Merge in population data")
monthly_province_peru_cases <- merge(monthly_province_peru_cases, tmp, by = c(
  "PROVINCE",
  "YEAR", "MONTH"
))
monthly_province_peru_cases[which(is.na(ym_cases))] # Check no N/A values
monthly_province_peru_cases[, DIR := ym_cases / POP * 1e+05]
monthly_province_peru_cases

# Launch Snakemake to process the worldclim data
log_info("Launch Snakemake to process the worldclim data")
system2(
  "snakemake",
  args = c(
    "--cores", SNAKEMAKE_CORES,
    "--snakefile", "workflows/processing/climate/workflow/Snakefile"
  ),
  stdout = "", stderr = "",
  wait = TRUE,
)

log_info("WorldClim data extraction")
extract_worldclim_variable_provinces <- function(years_sequence,
                                                 climate_variable) {
  worldclim_variable_dt <- data.table(
    YEAR = rep(
      years_sequence,
      each = 12 * length(region_province$PROVINCE)
    ),
    PROVINCE = rep(
      region_province$PROVINCE,
      length(years_sequence)
    )
  )
  worldclim_variable_dt[, MONTH := rep(
    rep(seq(1, 12), each = length(region_province$PROVINCE)),
    length(unique(years_sequence))
  )]
  worldclim_variable_dt[, TIME := rep(seq(1, 12 * length(unique(YEAR))),
    each = length(unique(PROVINCE))
  )]
  worldclim_variable_dt[, VALUE := rep(0, nrow(worldclim_variable_dt))]
  for (i in 1:length(years_sequence)) {
    year <- years_sequence[i]
    log_info("Processing year: ", year)
    climate_variable_monthly_files <- list_worldclim_variable_tifs(
      years_sequence[i],
      climate_variable
    )
    for (j in 1:12) {
      rds_file <- paste0(
        tools::file_path_sans_ext(climate_variable_monthly_files[j]),
        ".RDS"
      )
      if (file.exists(rds_file)) {
        log_info("Reading from RDS file: ", rds_file)
        tmp_climate_provinces <- readRDS(rds_file)
      } else {
        # If this section is running, then the Snakemake workflow has failed.
        log_error(
          paste0(
            "RDS file does not exist [climate workflow likely failed]: ",
            rds_file))
        stop()
      }
      worldclim_variable_dt[which(YEAR == year & MONTH == j), VALUE :=
        rep(tmp_climate_provinces, length.out = .N)]
    }
  }
  setnames(worldclim_variable_dt, "VALUE", climate_variable)

  worldclim_variable_dt  # return
}

# Extract WorldClim variables for 2010-2019
seq_2010_2019_province <- seq(2010, 2019, by = 1)
tmax_2010_2019_province <- extract_worldclim_variable_provinces(
  seq_2010_2019_province,
  "tmax"
)

tmin_2010_2019_province <- extract_worldclim_variable_provinces(
  seq_2010_2019_province,
  "tmin"
)
prec_2010_2019_province <- extract_worldclim_variable_provinces(
  seq_2010_2019_province,
  "prec"
)

# Extract WorldClim variables for 2020-2021
seq_2020_2021_province <- seq(2020, 2021, by = 1)
tmax_2020_2021_province <- extract_worldclim_variable_provinces(
  seq_2020_2021_province,
  "tmax"
)
tmin_2020_2021_province <- extract_worldclim_variable_provinces(
  seq_2020_2021_province,
  "tmin"
)
prec_2020_2021_province <- extract_worldclim_variable_provinces(
  seq_2020_2021_province,
  "prec"
)

# ------------------------------------------------------------------------------

# SPI-6 Data ---- Standardized Precipitation Index (at 6 month accumulation
# period)
log_info("SPI-6 Data")
spi_data_table_func <- function() {
  spi_dt <- data.table(
    YEAR = rep(
      seq(2001, 2022, by = 1),
      each = length(peru_district_boundaries2$ADM2_ES) * 12
    ),
    MONTH = rep(
      seq(1, 12),
      length(seq(2001, 2022, by = 1)) * length(unique(peru_district_boundaries2$ADM2_ES))
    )
  )
  spi_dt[
    ,
    PROVINCE := rep(
      rep(peru_district_boundaries2$ADM2_ES, each = 12),
      length(seq(2001, 2022, by = 1))
    )
  ]
  for (i in 1:length(unique(spi_dt$YEAR))) {
    year <- unique(spi_dt$YEAR)[i]
    log_info(paste0("Processing year ", year))
    spi_file_name <- spi_file_name_func(year)
    r <- rast(spi_file_name_func(year))
    r <- crop(r, extent(peru_district_boundaries2))
    tmp <- copy(peru_district_boundaries2)
    tmp$spi <- data.table(raster::extract(r, tmp,
      fun = "mean", weights = TRUE,
      na.rm = TRUE
    ))
    tmp2 <- data.table(cbind(tmp$ADM2_ES, tmp$spi))
    tmp2[, ID := NULL]
    setnames(tmp2, colnames(tmp2)[1], c("PROVINCE"))
    for (j in 1:12) {
      spi_val_dt <- subset(tmp2, select = c("PROVINCE", colnames(tmp2)[j +
        1]))
      for (k in 1:length(unique(tmp$ADM2_ES))) {
        tmp_PROVINCE <- unique(tmp$ADM2_ES)[k]
        spi_val_dt2 <- subset(spi_val_dt, PROVINCE == tmp_PROVINCE)
        spi_val <- subset(spi_val_dt2, select = colnames(spi_val_dt2)[2])
        spi_dt[which(PROVINCE == tmp_PROVINCE & MONTH == j & YEAR ==
          year), SPI_6 := spi_val]
      }
    }
  }

  spi_dt  # return
}

spi_province_dt <- spi_data_table_func()
spi_province_dt <- data.table(spi_province_dt)

# ICEN and ONI Data-----
log_info("ICEN and ONI Data")

# --- This is where WorldClim, SPI, ICEN, and ONI data are combined ------------

# 2010-2019 climate data ----

# Set up climate data.table ---- Note need to keep separate as we need to
# include up to 4 months lag for modelling analysis
log_info("Set up climate data.table")
climate_dt_province <- merge(tmax_2010_2019_province, tmin_2010_2019_province,
  by = c("PROVINCE", "YEAR", "TIME", "MONTH")
)
climate_dt_province <- merge(climate_dt_province, prec_2010_2019_province, by = c(
  "PROVINCE",
  "YEAR", "TIME", "MONTH"
))
climate_dt_province <- merge(climate_dt_province, spi_province_dt, by = c(
  "PROVINCE",
  "YEAR", "MONTH"
))
climate_dt_province <- merge(climate_dt_province, oni_data, by = c("YEAR", "MONTH"))
climate_dt_province <- merge(climate_dt_province, icen_data, by = c("YEAR", "MONTH"))

# 2020-2021 climate data ----

climate_20_21_province <- merge(tmax_2020_2021_province, prec_2020_2021_province,
  by = c("PROVINCE", "YEAR", "TIME", "MONTH")
)
climate_20_21_province <- merge(climate_20_21_province, tmin_2020_2021_province,
  by = c("PROVINCE", "YEAR", "TIME", "MONTH")
)
climate_20_21_province <- merge(climate_20_21_province, icen_data, by = c(
  "YEAR",
  "MONTH"
))
climate_20_21_province <- merge(climate_20_21_province, oni_data, by = c(
  "YEAR",
  "MONTH"
))
climate_20_21_province <- merge(climate_20_21_province, spi_province_dt, by = c(
  "PROVINCE",
  "YEAR", "MONTH"
))
climate_dt_province <- rbind(climate_dt_province, climate_20_21_province)
climate_dt_province <- merge(climate_dt_province, region_province, by = "PROVINCE")
climate_dt_province[, TIME := 1:length(tmin), by = "PROVINCE"]

# ------------------------------------------------------------------------------

missing_prov_ids <- which(!(unique(monthly_province_peru_cases$PROVINCE) %in%
  climate_dt_province$PROVINCE))
climate_dt_province[which(PROVINCE == "Datem del Marañon"), PROVINCE := "Datem Del Marañon"]
climate_dt_province[which(PROVINCE == "Dos de Mayo"), PROVINCE := "Dos De Mayo"]
climate_dt_province[which(PROVINCE == "Paucar del Sara Sara"), PROVINCE :=
  "Paucar Del Sara Sara"]
climate_dt_province[which(PROVINCE == "Rodriguez de Mendoza"), PROVINCE :=
  "Rodriguez De Mendoza"]
missing_prov_ids <- which(!(unique(monthly_province_peru_cases$PROVINCE) %in%
  climate_dt_province$PROVINCE))
unique(monthly_province_peru_cases$PROVINCE)[missing_prov_ids]
unique(monthly_province_peru_cases$PROVINCE)

# Setting up right-aligned rolling averages of climatic variables
log_info("Setting up right-aligned rolling averages of climatic variables")
climate_dt_province[, tmax_roll_2 := rollmean(tmax, 2, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[, tmax_roll_3 := rollmean(tmax, 3, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[, tmax_roll_sum_2 := rollsum(tmax, 2, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[, tmax_roll_sum_3 := rollsum(tmax, 3, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[which(is.na(tmax_roll_2)), tmax_roll_2 := tmax]
climate_dt_province[which(is.na(tmax_roll_3)), tmax_roll_3 := tmax]
climate_dt_province[which(is.na(tmax_roll_sum_2)), tmax_roll_sum_2 := tmax]
climate_dt_province[which(is.na(tmax_roll_sum_3)), tmax_roll_sum_3 := tmax]

climate_dt_province[, tmin_roll_2 := rollmean(tmin, 2, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[, tmin_roll_3 := rollmean(tmin, 3, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[, tmin_roll_sum_2 := rollsum(tmin, 2, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[, tmin_roll_sum_3 := rollsum(tmin, 3, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[which(is.na(tmin_roll_2)), tmin_roll_2 := tmin]
climate_dt_province[which(is.na(tmin_roll_3)), tmin_roll_3 := tmin]
climate_dt_province[which(is.na(tmin_roll_sum_2)), tmin_roll_sum_2 := tmin]
climate_dt_province[which(is.na(tmin_roll_sum_3)), tmin_roll_sum_3 := tmin]

climate_dt_province[, prec_roll_2 := rollmean(prec, 2, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[, prec_roll_3 := rollmean(prec, 3, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[which(is.na(prec_roll_2)), prec_roll_2 := prec]
climate_dt_province[which(is.na(prec_roll_3)), prec_roll_3 := prec]
climate_dt_province[, prec_roll_sum_2 := rollsum(prec, 2, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[, prec_roll_sum_3 := rollsum(prec, 3, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[which(is.na(prec_roll_2)), prec_roll_2 := prec]
climate_dt_province[which(is.na(prec_roll_3)), prec_roll_3 := prec]
climate_dt_province[which(is.na(prec_roll_sum_2)), prec_roll_sum_2 := prec]
climate_dt_province[which(is.na(prec_roll_sum_3)), prec_roll_sum_3 := prec]

climate_dt_province[, spi_roll_2 := rollmean(SPI_6, 2, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[, spi_roll_3 := rollmean(SPI_6, 3, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[which(is.na(spi_roll_2)), spi_roll_2 := SPI_6]
climate_dt_province[which(is.na(spi_roll_3)), spi_roll_3 := SPI_6]
climate_dt_province[, spi_roll_sum_2 := rollsum(SPI_6, 2, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[, spi_roll_sum_3 := rollsum(SPI_6, 3, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[which(is.na(spi_roll_2)), spi_roll_2 := SPI_6]
climate_dt_province[which(is.na(spi_roll_3)), spi_roll_3 := SPI_6]
climate_dt_province[which(is.na(spi_roll_sum_2)), spi_roll_sum_2 := SPI_6]
climate_dt_province[which(is.na(spi_roll_sum_3)), spi_roll_sum_3 := SPI_6]

climate_dt_province[, icen_roll_2 := rollmean(E_INDEX, 2, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[, icen_roll_3 := rollmean(E_INDEX, 3, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[which(is.na(icen_roll_2)), icen_roll_2 := E_INDEX]
climate_dt_province[which(is.na(icen_roll_3)), icen_roll_3 := E_INDEX]
climate_dt_province[, icen_roll_sum_2 := rollsum(E_INDEX, 2, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[, icen_roll_sum_3 := rollsum(E_INDEX, 3, fill = NA, align = "right"),
  by = "PROVINCE"
]
climate_dt_province[which(is.na(icen_roll_2)), icen_roll_2 := E_INDEX]
climate_dt_province[which(is.na(icen_roll_3)), icen_roll_3 := E_INDEX]
climate_dt_province[which(is.na(icen_roll_sum_2)), icen_roll_sum_2 := E_INDEX]
climate_dt_province[which(is.na(icen_roll_sum_3)), icen_roll_sum_3 := E_INDEX]

monthly_province_peru_cases <- merge(monthly_province_peru_cases, climate_dt_province,
  by = c("PROVINCE", "YEAR", "MONTH")
)

saveRDS(climate_dt_province, file = file.path(peru.province.out.dir, "climate_dt_province.RDS"))
write.csv(climate_dt_province, file.path(peru.province.out.dir, "climate_dt_province.csv"), row.names=FALSE)

log_info("Finished climate.R")

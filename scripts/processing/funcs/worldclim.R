#' List WorldClim .tif files for a climate variable across a year range
#'
#' @param years_sequence A vector of years, e.g., 2020:2021
#' @param climate_variable One of "tmax", "tmin", or "prec"
#' @return A character vector of file paths for each month/year combination
list_worldclim_variable_tifs <- function(years_sequence, climate_variable) {
  if (!climate_variable %in% c("tmax", "tmin", "prec")) {
    stop("climate_variable must be one of 'tmax', 'tmin', or 'prec'")
  }

  climate_file_list <- character()

  for (file_year in years_sequence) {
    for (month in 1:12) {
      file_month <- sprintf("%02d", month)  # zero-pad to "01", "02", ...
      file_name <- paste0("wc2.1_2.5m_", climate_variable, "_", file_year, "-", file_month, ".tif")
      climate_file <- unname(file.path(peru.climate.data.dir, file_name))
      print(climate_file)
      climate_file_list <- c(climate_file_list, climate_file)
    }
  }

  climate_file_list  # return
}

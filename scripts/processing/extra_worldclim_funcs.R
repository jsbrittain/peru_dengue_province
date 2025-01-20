list_worldclim_variable_tifs <- function(years_sequence, climate_variable) {
  climate_file_list <- 0
  for (i in 1:length(years_sequence)) {
    file_year <- years_sequence[i]
    for (j in seq(1, 12)) {
      if (j < 10) {
        j <- paste0("0", j)
      }
      file_month <- j
      if (climate_variable == "tmax") {
        climate_file <- unname(file.path(
          peru.data.in.dir,
          paste0(
            "wc2.1_2.5m_", climate_variable, "_",
            file_year, "-", file_month, ".tif"
          )
        ))
      }
      if (climate_variable == "tmin") {
        climate_file <- unname(file.path(
          peru.data.in.dir,
          paste0(
            "wc2.1_2.5m_", climate_variable, "_",
            file_year, "-", file_month, ".tif"
          )
        ))
      }
      if (climate_variable == "prec") {
        climate_file <- unname(file.path(
          peru.data.in.dir,
          paste0(
            "wc2.1_2.5m_", climate_variable, "_",
            file_year, "-", file_month, ".tif"
          )
        ))
      }
      print(climate_file)
      climate_file_list <- c(climate_file_list, climate_file)
    }
  }
  return(climate_file_list[2:length(climate_file_list)])
}

# Function to list worldclim 2020-2021 files for climatic variables
list_worldclim_variable_tifs_20_21 <- function(years_sequence, climate_variable) {
  climate_file_list <- 0
  for (i in 1:length(years_sequence)) {
    file_year <- years_sequence[i]
    for (j in seq(1, 12)) {
      if (j < 10) {
        j <- paste0("0", j)
      }
      file_month <- j
      if (climate_variable == "tmax") {
        climate_file <- unname(file.path(
          peru.data.in.dir,
          paste0(
            "wc2.1_2.5m_", climate_variable, "_",
            file_year, "-", file_month, ".tif"
          )
        ))
      }
      if (climate_variable == "tmin") {
        climate_file <- unname(file.path(
          peru.data.in.dir,
          paste0(
            "wc2.1_2.5m_", climate_variable, "_",
            file_year, "-", file_month, ".tif"
          )
        ))
      }
      if (climate_variable == "prec") {
        climate_file <- unname(file.path(
          peru.data.in.dir,
          paste0(
            "wc2.1_2.5m_", climate_variable, "_",
            file_year, "-", file_month, ".tif"
          )
        ))
      }
      print(climate_file)
      climate_file_list <- c(climate_file_list, climate_file)
    }
  }
  return(climate_file_list[2:length(climate_file_list)])
}

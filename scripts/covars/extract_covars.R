library(data.table)
library(dplyr)

climate_dt <- read.csv('data/output/climate_dt_province.csv')

covar_exports <- c('tmin', 'tmax', 'prec')

for (covar in covar_exports) {
  print(covar)
  df <- data.table(copy(climate_dt))
  df <- df %>%
    select(c('TIME', 'PROVINCE', 'MONTH', 'YEAR')) %>%
    mutate(
      value = df[[covar]]
    )
  dirstem <- paste0('predictions/Covar_', covar)
  dir.create(dirstem, recursive = TRUE, showWarnings = FALSE)
  write.csv(df, paste0(dirstem, '/values.csv'))
}

form_untrained_ensembles <- function(models_dt) {
  new_ensembles_dt <- NULL
  # EW
  models_dt[, true_value := round(true_value, digits = 6)]
  models_dt[, quantile := round(quantile, digits = 3)]

  mean_ensemble_dt <- models_dt[, list(prediction = mean(prediction)),
    by = c("location", "target_end_date", "quantile")
  ]
  mean_ensemble_dt[, model := "EW_MEAN_ENSEMBLE"]
  new_ensembles_dt <- rbind(
    new_ensembles_dt,
    mean_ensemble_dt
  )
  # Median
  median_ensemble_dt <- models_dt[, list(prediction = median(prediction)),
    by = c("location", "target_end_date", "quantile")
  ]
  median_ensemble_dt[, model := "MEDIAN_ENSEMBLE"]
  new_ensembles_dt <- rbind(
    new_ensembles_dt,
    median_ensemble_dt
  )

  # No Bayesian climate model
  tmp <- models_dt[which(model != "climate")]
  mean_ensemble_dt <- tmp[, list(prediction = mean(prediction)),
    by = c("location", "target_end_date", "quantile")
  ]
  mean_ensemble_dt[, model := "EW_MEAN_ENSEMBLE_MINUS_CLIMATE"]
  new_ensembles_dt <- rbind(
    new_ensembles_dt,
    mean_ensemble_dt
  )

  # Median
  median_ensemble_dt <- tmp[, list(prediction = median(prediction)),
    by = c("location", "target_end_date", "quantile")
  ]
  median_ensemble_dt[, model := "MEDIAN_ENSEMBLE_MINUS_CLIMATE"]
  new_ensembles_dt <- rbind(
    new_ensembles_dt,
    median_ensemble_dt
  )


  # No Baseline
  tmp <- models_dt[which(model != "baseline")]
  mean_ensemble_dt <- tmp[, list(prediction = mean(prediction)),
    by = c("location", "target_end_date", "quantile")
  ]
  mean_ensemble_dt[, model := "EW_MEAN_ENSEMBLE_MINUS_BASELINE"]
  new_ensembles_dt <- rbind(
    new_ensembles_dt,
    mean_ensemble_dt
  )

  # Median
  median_ensemble_dt <- tmp[, list(prediction = median(prediction)),
    by = c("location", "target_end_date", "quantile")
  ]
  median_ensemble_dt[, model := "MEDIAN_ENSEMBLE_MINUS_BASELINE"]
  new_ensembles_dt <- rbind(
    new_ensembles_dt,
    median_ensemble_dt
  )

  # No climate data
  tmp <- models_dt[which(model != "climate")]
  tmp <- tmp[which(model != "fine_tuned_timegpt")]
  tmp <- tmp[which(model != "tcn")]

  mean_ensemble_dt <- tmp[, list(prediction = mean(prediction)),
    by = c("location", "target_end_date", "quantile")
  ]
  mean_ensemble_dt[, model := "EW_MEAN_ENSEMBLE_NO_COVARS"]
  new_ensembles_dt <- rbind(
    new_ensembles_dt,
    mean_ensemble_dt
  )

  # Median
  median_ensemble_dt <- tmp[, list(prediction = median(prediction)),
    by = c("location", "target_end_date", "quantile")
  ]
  median_ensemble_dt[, model := "MEDIAN_ENSEMBLE_NO_COVARS"]
  new_ensembles_dt <- rbind(
    new_ensembles_dt,
    median_ensemble_dt
  )

  tmp <- unique(subset(models_dt, select = c(
    "location", "target_end_date", "quantile",
    "true_value"
  )))

  new_ensembles_dt <- merge(new_ensembles_dt,
    tmp,
    by = c("location", "target_end_date", "quantile")
  )

  new_ensembles_dt <- rbind(
    new_ensembles_dt,
    models_dt
  ) # Merge in original data.table
  return(new_ensembles_dt)
}

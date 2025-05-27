iteratively_train_quantile_ensemble_by_province <- function(quantiles,
                                                            number_testing_dates,
                                                            input_quantile_dt,
                                                            pred_points_dt,
                                                            historical_input_quantile_dt,
                                                            true_data,
                                                            data_field = "LOG_CASES") {
  # ovr_num_pred_points = total number points (includes all quantile predictions)
  # input_quantile_dt is predictions for entire testing window (2018-2021)
  # historical_input_quantile_dt is historical predictions

  # prediction_indices <- sort(unique(input_quantile_dt$IND))

  model_names <- sort(unique(input_quantile_dt$model))
  # print(model_names)
  ensemble_model_name <- paste(model_names, collapse = "_")
  testing_dates <- sort(unique(input_quantile_dt$target_end_date))
  num_ensemble_components <- length(unique(input_quantile_dt$model))
  num_quantile_levels <- length(quantiles)

  # data.tables to store results
  ovr_preds_dt <- NULL
  ovr_model_weights_dt <- NULL
  for (p in 1:length(unique(input_quantile_dt$location))) {
    log_info(paste0("iteratively_train_quantile_ensemble_by_province (p = ", p, " of ", length(unique(input_quantile_dt$location)), ")"))
    historical_input_quantile_dt <- NULL
    prov_in_q <- unique(input_quantile_dt$location)[p]
    for (t in 1:number_testing_dates) {
      new_date <- testing_dates[t]
      # number of historical pred points
      tmp_true_data <- subset(true_data, end_of_month <= new_date)
      tmp_true_data <- subset(tmp_true_data, location = prov_in_q)

      testing_input_quantile_dt <- subset(
        input_quantile_dt,
        target_end_date == new_date
      )
      testing_input_quantile_dt <- subset(
        testing_input_quantile_dt,
        location == prov_in_q
      )
      setkeyv(testing_input_quantile_dt, c(
        "location", "model",
        "target_end_date", "quantile"
      ))

      historical_input_quantile_dt <- rbind(
        historical_input_quantile_dt,
        testing_input_quantile_dt
      )
      num_pred_points <- nrow(historical_input_quantile_dt) / num_quantile_levels
      num_pred_points <- num_pred_points / num_ensemble_components
      setkeyv(historical_input_quantile_dt, c(
        "location", "model",
        "target_end_date", "quantile"
      ))

      # num_pred_points = nrow()
      qarr <- array(NA, dim = c(
        num_pred_points,
        num_ensemble_components,
        num_quantile_levels
      ))
      # print(dim(qarr))
      # print(dim(qarr))
      prediction_indices <- sort(unique(historical_input_quantile_dt$IND))
      for (i in 1:num_pred_points) {
        # print(paste0("i: ", i))
        for (j in 1:num_ensemble_components) {
          # print(paste0("j: ", j))
          # print(prediction_indices[i])
          # print(unique(model_names[j]))
          # print(historical_input_quantile_dt[which(IND == prediction_indices[i] &
          #                                            model== unique(model_names[j])), ])
          qarr[i, j, ] <-
            historical_input_quantile_dt[which(IND == prediction_indices[i] &
              model == unique(model_names[j])), ]$prediction
        }
      }
      quantile_ensemble_weights <- quantile_ensemble(
        qarr,
        tmp_true_data[[data_field]],
        quantiles
      )
      # print(quantile_ensemble_weights$alpha)
      tmp_trained_quantile_ensemble <- copy(testing_input_quantile_dt)
      model_weights_dt <- data.table(model = sort(unique(model_names)), weights = quantile_ensemble_weights$alpha)
      tmp_trained_quantile_ensemble <-
        merge(tmp_trained_quantile_ensemble,
          model_weights_dt,
          by = "model"
        )
      model_weights_dt[, target_end_date := unique(testing_input_quantile_dt$target_end_date)]
      model_weights_dt[, location := prov_in_q]
      ovr_model_weights_dt <- rbind(
        ovr_model_weights_dt,
        model_weights_dt
      )
      # tmp_trained_quantile_ensemble <- tmp_trained_quantile_ensemble[, list(prediction = sum(weights*prediction)),
      #                                                                                by = c("location", "true_value",
      #                                                                                       "target_end_date", "quantile")]
      #
      # tmp_trained_quantile_ensemble[, model:= paste("trained_quantile", ensemble_model_name)]
      # ovr_preds_dt <- tmp_trained_quantile_ensemble
    }
  }
  return(ovr_model_weights_dt)
}

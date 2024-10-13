#Quantiles
quantiles <- c(0.01, 0.025, 
  seq(0.05, 0.95, 0.05), 
  0.975, 0.99)
#Functions
form_untrained_ensembles <- function(models_dt){
  new_ensembles_dt <- NULL
  #EW
  models_dt[, true_value:= round(true_value, digits = 6)]
  models_dt[, quantile:= round(quantile, digits = 3)]
  
  mean_ensemble_dt <- models_dt[, list(prediction = mean(prediction)),
                                by = c("location", "target_end_date", "quantile")]
  mean_ensemble_dt[, model:= "EW_MEAN_ENSEMBLE"]
  new_ensembles_dt <- rbind(new_ensembles_dt,
                            mean_ensemble_dt)
  #Median
  median_ensemble_dt <- models_dt[, list(prediction = median(prediction)),
                                  by = c("location", "target_end_date", "quantile")]
  median_ensemble_dt[, model:= "MEDIAN_ENSEMBLE"]
  new_ensembles_dt <- rbind(new_ensembles_dt,
                            median_ensemble_dt)
  
  #No Bayesian climate model
  tmp <- models_dt[which(model!= "climate")]
  mean_ensemble_dt <- tmp[, list(prediction = mean(prediction)),
                          by = c("location", "target_end_date", "quantile")]
  mean_ensemble_dt[, model:= "EW_MEAN_ENSEMBLE_MINUS_CLIMATE"]
  new_ensembles_dt <- rbind(new_ensembles_dt,
                            mean_ensemble_dt)
  
  #Median
  median_ensemble_dt <- tmp[, list(prediction = median(prediction)),
                            by = c("location", "target_end_date", "quantile")]
  median_ensemble_dt[, model:= "MEDIAN_ENSEMBLE_MINUS_CLIMATE"]
  new_ensembles_dt <- rbind(new_ensembles_dt,
                            median_ensemble_dt)
  
  
  #No Baseline
  tmp <- models_dt[which(model!= "baseline")]
  mean_ensemble_dt <- tmp[, list(prediction = mean(prediction)),
                          by = c("location", "target_end_date", "quantile")]
  mean_ensemble_dt[, model:= "EW_MEAN_ENSEMBLE_MINUS_BASELINE"]
  new_ensembles_dt <- rbind(new_ensembles_dt,
                            mean_ensemble_dt)
  
  #Median
  median_ensemble_dt <- tmp[, list(prediction = median(prediction)),
                            by = c("location", "target_end_date", "quantile")]
  median_ensemble_dt[, model:= "MEDIAN_ENSEMBLE_MINUS_BASELINE"]
  new_ensembles_dt <- rbind(new_ensembles_dt,
                            median_ensemble_dt)
  
  #No climate data
  tmp <- models_dt[which(model!= "climate")]
  tmp <- tmp[which(model!= "fine_tuned_timegpt")]
  tmp <- tmp[which(model!= "tcn")]
  
  mean_ensemble_dt <- tmp[, list(prediction = mean(prediction)),
                          by = c("location", "target_end_date", "quantile")]
  mean_ensemble_dt[, model:= "EW_MEAN_ENSEMBLE_NO_COVARS"]
  new_ensembles_dt <- rbind(new_ensembles_dt,
                            mean_ensemble_dt)
  
  #Median
  median_ensemble_dt <- tmp[, list(prediction = median(prediction)),
                            by = c("location", "target_end_date", "quantile")]
  median_ensemble_dt[, model:= "MEDIAN_ENSEMBLE_NO_COVARS"]
  new_ensembles_dt <- rbind(new_ensembles_dt,
                            median_ensemble_dt)
  
  tmp <- unique(subset(models_dt, select = c("location", "target_end_date", "quantile",
                                             "true_value")))
  new_ensembles_dt <- merge(new_ensembles_dt,
                            tmp, 
                            by = c("location", "target_end_date", "quantile"))
  
  new_ensembles_dt <- rbind(new_ensembles_dt,
                            models_dt) #Merge in original data.table
  return(new_ensembles_dt)
}


#Load in data ----
ptl_province_inla_df <- data.table(read.csv(file.path(peru.province.python.data.dir,
                                                      "ptl_province_inla_df.csv")))
ptl_province_2018_2021_data <- subset(ptl_province_inla_df,
                                      YEAR >= 2018)
ptl_province_2018_2021_data[, IND:= seq(1, length(DIR)), by = "PROVINCE"]
scoring_columns <- c("location", "true_value", "model", "target_end_date", 
                     "quantile", "prediction")
#Load in forecasts ----
#1) SARIMA
quantile_sarima_preds_dt <- readRDS(file = file.path(peru.province.python.out.dir,
                         "quantile_sarima_preds_dt.RDS"))
quantile_sarima_preds_dt
#2) TIMEGPT
quantile_fine_tuned_timegpt_preds_dt <- readRDS(file = file.path(peru.province.python.out.dir,
                 "quantile_fine_tuned_timegpt_preds_dt.RDS"))
quantile_fine_tuned_timegpt_preds_dt <- subset(quantile_fine_tuned_timegpt_preds_dt,
                                select = scoring_columns)
quantile_fine_tuned_timegpt_preds_dt
#3) DEEPTCN
quantile_tcn_preds_dt <- readRDS(file = file.path(peru.province.python.out.dir,
                         "quantile_tcn_preds_dt.RDS"))
quantile_tcn_preds_dt <- subset(quantile_tcn_preds_dt,
                                select = scoring_columns)
#4) TIMEGPT(No Covars)
quantile_finetuned_no_covars_timegpt_preds_dt <- readRDS(file = file.path(peru.province.python.out.dir,
                                                                 "quantile_finetuned_no_covars_timegpt_preds_dt.RDS"))
quantile_finetuned_no_covars_timegpt_preds_dt <- subset(quantile_finetuned_no_covars_timegpt_preds_dt,
                                                  select = scoring_columns)
quantile_finetuned_no_covars_timegpt_preds_dt

#5) Climate
climate_2018_2021_log_cases_quantile_dt <- readRDS(file = file.path(peru.province.inla.data.out.dir, "climate_2018_2021_log_cases_quantile_dt.RDS"))
climate_2018_2021_log_cases_quantile_dt <- merge(climate_2018_2021_log_cases_quantile_dt, 
                                                subset(ptl_province_2018_2021_data, 
                                                       select = c("PROVINCE","TIME", "MONTH", "YEAR", "end_of_month")),
                                                by = c("PROVINCE","TIME", "MONTH", "YEAR"))
setnames(climate_2018_2021_log_cases_quantile_dt, c("PROVINCE", "end_of_month"),
         c("location", "target_end_date"))
climate_2018_2021_log_cases_quantile_dt <- subset(climate_2018_2021_log_cases_quantile_dt,
                                                 select = scoring_columns)
climate_2018_2021_log_cases_quantile_dt

#6)Baseline
quantile_baseline_log_cases.pred_dt_2018_2021[which(quantile == 0.5), caret::MAE(prediction, true_value)]


#Combine all into single data.table
quantile_log_cases_components_dt <- rbind(quantile_sarima_preds_dt,
                                     quantile_tcn_preds_dt,
                                     quantile_fine_tuned_timegpt_preds_dt,
                                     quantile_finetuned_no_covars_timegpt_preds_dt,
                                     climate_2018_2021_log_cases_quantile_dt,
                                     quantile_baseline_log_cases.pred_dt_2018_2021)
quantile_log_cases_components_dt[, true_value:= round(true_value, 8)]
quantile_log_cases_components_dt[, length(unique(true_value)), by = "model"]
quantile_log_cases_components_dt[, target_end_date:= as.Date(target_end_date)]
quantile_log_cases_components_dt[which(prediction <0 ), prediction:= 0]
quantile_log_cases_components_dt %>% score() %>% summarise_scores(by = c("model"))
quantile_log_cases_components_plus_ensembles_dt <- 
  form_untrained_ensembles(quantile_log_cases_components_dt)
quantile_log_cases_components_plus_ensembles_dt[, length(prediction), by = "model"]

#Trained ensembles ----
#Train ensemble using quantile loss with iterative updating to reflect 
  # real-world conditions of information becoming iteratively available
iteratively_train_quantile_ensemble <- function(quantiles,
                                                number_testing_dates,
                                                input_quantile_dt,
                                                pred_points_dt,
                                                historical_input_quantile_dt,
                                                true_data){
  
  #ovr_num_pred_points = total number points (includes all quantile predictions)
  #input_quantile_dt is predictions for entire testing window (2018-2021)
  #historical_input_quantile_dt is historical predictions
  
  # prediction_indices <- sort(unique(input_quantile_dt$IND))
  
  model_names <- sort(unique(input_quantile_dt$model))
  print(model_names)
  ensemble_model_name <- paste(model_names, collapse = "_")
  testing_dates <- sort(unique(input_quantile_dt$target_end_date))
  num_ensemble_components <- length(unique(input_quantile_dt$model))
  num_quantile_levels <- length(quantiles)
  
  #data.tables to store results
  ovr_preds_dt <- NULL
  ovr_model_weights_dt <- NULL
  
  for(t in 1:number_testing_dates){
    new_date <- testing_dates[t]
    #number of historical pred points
    tmp_true_data <- subset(true_data, end_of_month <= new_date)
    testing_input_quantile_dt <- subset(input_quantile_dt, target_end_date == new_date)
    historical_input_quantile_dt <- rbind(historical_input_quantile_dt,
                                          testing_input_quantile_dt)
    num_pred_points <- nrow(historical_input_quantile_dt)/num_quantile_levels
    num_pred_points <- num_pred_points/num_ensemble_components
    print(num_pred_points)
    setkeyv(historical_input_quantile_dt, c("location", "model", 
                                            "target_end_date", "quantile"))
    
    #num_pred_points = nrow()
    qarr <- array(NA, dim = c(num_pred_points, 
                              num_ensemble_components, 
                              num_quantile_levels))
    print(dim(qarr))
    # print(dim(qarr))
    prediction_indices <- sort(unique(historical_input_quantile_dt$IND))
    for(i in 1:num_pred_points){
      # print(paste0("i: ", i))
      for(j in 1:num_ensemble_components){
        # print(paste0("j: ", j))
        # print(prediction_indices[i])
        # print(unique(model_names[j]))
        # print(historical_input_quantile_dt[which(IND == prediction_indices[i] & 
        #                                            model== unique(model_names[j])), ])
        qarr[i,j,] <- 
          historical_input_quantile_dt[which(IND == prediction_indices[i] & 
                                    model== unique(model_names[j])), ]$prediction         
      }
    }
    quantile_ensemble_weights <- quantile_ensemble(qarr,
                                                   tmp_true_data$LOG_CASES,
                                                   quantiles)

    tmp_trained_quantile_ensemble <- copy(testing_input_quantile_dt)
    model_weights_dt <- data.table(model = model_names, weights = quantile_ensemble_weights$alpha)
    tmp_trained_quantile_ensemble <- 
      merge(tmp_trained_quantile_ensemble, 
            model_weights_dt, by = "model")
    model_weights_dt[, target_end_date:= unique(testing_input_quantile_dt$target_end_date)]
    ovr_model_weights_dt <- rbind(ovr_model_weights_dt,
                                  model_weights_dt)
    # tmp_trained_quantile_ensemble <- tmp_trained_quantile_ensemble[, list(prediction = sum(weights*prediction)),
    #                                                                                by = c("location", "true_value", 
    #                                                                                       "target_end_date", "quantile")]
    # 
    # tmp_trained_quantile_ensemble[, model:= paste("trained_quantile", ensemble_model_name)]
    # ovr_preds_dt <- tmp_trained_quantile_ensemble
  }
  return(ovr_model_weights_dt)
}



min_date <- min(input_quantile_dt$target_end_date)
iteratively_trained_weights_log_cases_dt <- iteratively_train_quantile_ensemble(quantiles = quantiles,
                                    number_testing_dates = number_testing_dates,
                                    input_quantile_dt = input_quantile_dt[which(target_end_date > min_date)],
                                    pred_points_dt,
                                    historical_input_quantile_dt = input_quantile_dt[which(target_end_date== min_date)],
                                    true_log_cases_data)

iteratively_trained_weights_log_cases_dt <- iteratively_train_quantile_ensemble(quantiles = quantiles,
                                                                                number_testing_dates = number_testing_dates,
                                                                                input_quantile_dt = input_quantile_dt,
                                                                                pred_points_dt,
                                                                                historical_input_quantile_dt = NULL,
                                                                                ptl_province_2018_2021_data)
iteratively_trained_weights_log_cases_dt
saveRDS(iteratively_trained_weights_log_cases_dt,
        file = file.path(peru.province.out.dir, "iteratively_trained_weights_log_cases_dt.RDS"))

# iteratively_trained_weights_log_cases_dt <- readRDS(file = file.path(peru.province.out.dir, "iteratively_trained_weights_log_cases_dt.RDS"))

iteratively_trained_weights_log_cases_dt[, model_factor:= factor(model)]
iteratively_trained_weights_log_cases_dt[, target_end_date:= as.Date(target_end_date)]
input_quantile_dt[, target_end_date:= as.Date(target_end_date)]
weighted_quantile_log_cases_dt <- merge(iteratively_trained_weights_log_cases_dt,
      unique(subset(input_quantile_dt, 
                    select = c("IND", "model", "target_end_date", "location", "true_value",
                               "prediction", "quantile"))),
      by = c("model", "target_end_date"))
setkeyv(weighted_quantile_log_cases_dt, c("location", "IND"))
weighted_quantile_log_cases_dt[, true_value:= round(true_value, digits = 8)]
weighted_quantile_log_cases_dt <- unique(weighted_quantile_log_cases_dt)
weighted_quantile_log_cases_dt[, TRAINED_WEIGHT:= Lag(weights, 1), by = c("location", "model", "quantile")]
weighted_quantile_log_cases_dt[which(target_end_date == min(target_end_date)), TRAINED_WEIGHT:= 1/length(unique(model))]
weighted_quantile_log_cases_dt
weighted_quantile_log_cases_forecasts_2018_2021_dt <- 
  weighted_quantile_log_cases_dt[, list(prediction = sum(TRAINED_WEIGHT * prediction),
                              true_value = unique(true_value)),
                       by = c("location", "quantile", "target_end_date")]
weighted_quantile_log_cases_forecasts_2018_2021_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
weighted_quantile_log_cases_forecasts_2018_2021_dt[which(quantile == 0.5), caret::MAE(prediction, true_value)]
weighted_quantile_log_cases_forecasts_2018_2021_dt[, model:= "pinball_trained_ensemble"]

#2) Province-dependent
iteratively_train_quantile_ensemble_by_province <- function(quantiles,
                                                number_testing_dates,
                                                input_quantile_dt,
                                                pred_points_dt,
                                                historical_input_quantile_dt,
                                                true_data){
  
  #ovr_num_pred_points = total number points (includes all quantile predictions)
  #input_quantile_dt is predictions for entire testing window (2018-2021)
  #historical_input_quantile_dt is historical predictions
  
  # prediction_indices <- sort(unique(input_quantile_dt$IND))
  
  model_names <- sort(unique(input_quantile_dt$model))
  print(model_names)
  ensemble_model_name <- paste(model_names, collapse = "_")
  testing_dates <- sort(unique(input_quantile_dt$target_end_date))
  num_ensemble_components <- length(unique(input_quantile_dt$model))
  num_quantile_levels <- length(quantiles)
  
  #data.tables to store results
  ovr_preds_dt <- NULL
  ovr_model_weights_dt <- NULL
  for(p in 1:length(unique(input_quantile_dt$location))){
    historical_input_quantile_dt <- NULL
    prov_in_q <- unique(input_quantile_dt$location)[p]
    for(t in 1:number_testing_dates){
      new_date <- testing_dates[t]
      #number of historical pred points
      tmp_true_data <- subset(true_data, end_of_month <= new_date)
      tmp_true_data <- subset(tmp_true_data, location = prov_in_q)
      
      testing_input_quantile_dt <- subset(input_quantile_dt, 
                                          target_end_date == new_date)
      testing_input_quantile_dt <- subset(testing_input_quantile_dt,
                                          location == prov_in_q)
      setkeyv(testing_input_quantile_dt, c("location", "model", 
                                              "target_end_date", "quantile"))
      
      historical_input_quantile_dt <- rbind(historical_input_quantile_dt,
                                            testing_input_quantile_dt)
      num_pred_points <- nrow(historical_input_quantile_dt)/num_quantile_levels
      num_pred_points <- num_pred_points/num_ensemble_components
      setkeyv(historical_input_quantile_dt, c("location", "model", 
                                              "target_end_date", "quantile"))
      
      #num_pred_points = nrow()
      qarr <- array(NA, dim = c(num_pred_points, 
                                num_ensemble_components, 
                                num_quantile_levels))
      print(dim(qarr))
      # print(dim(qarr))
      prediction_indices <- sort(unique(historical_input_quantile_dt$IND))
      for(i in 1:num_pred_points){
        # print(paste0("i: ", i))
        for(j in 1:num_ensemble_components){
          # print(paste0("j: ", j))
          # print(prediction_indices[i])
          # print(unique(model_names[j]))
          # print(historical_input_quantile_dt[which(IND == prediction_indices[i] & 
          #                                            model== unique(model_names[j])), ])
          qarr[i,j,] <- 
            historical_input_quantile_dt[which(IND == prediction_indices[i] & 
                                                 model== unique(model_names[j])), ]$prediction         
        }
      }
      quantile_ensemble_weights <- quantile_ensemble(qarr,
                                                     tmp_true_data$LOG_CASES,
                                                     quantiles)
      print(quantile_ensemble_weights$alpha)
      tmp_trained_quantile_ensemble <- copy(testing_input_quantile_dt)
      model_weights_dt <- data.table(model = sort(unique(model_names)), weights = quantile_ensemble_weights$alpha)
      tmp_trained_quantile_ensemble <- 
        merge(tmp_trained_quantile_ensemble, 
              model_weights_dt, by = "model")
      model_weights_dt[, target_end_date:= unique(testing_input_quantile_dt$target_end_date)]
      model_weights_dt[, location:= prov_in_q]
      ovr_model_weights_dt <- rbind(ovr_model_weights_dt,
                                    model_weights_dt)
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
iteratively_trained_weights_dt_by_province <- iteratively_train_quantile_ensemble_by_province(quantiles = quantiles,
                                                                      number_testing_dates = number_testing_dates,
                                                                      input_quantile_dt = input_quantile_dt,
                                                                      pred_points_dt,
                                                                      historical_input_quantile_dt = historical_quantile_log_cases_components_dt,
                                                                      ptl_province_2018_2021_data)
iteratively_trained_weights_dt_by_province
#Merge in DIR  + Check outbreak 50 threshold



# WIS over time (expanding window) ----
unique(quantile_log_cases_components_dt$target_end_date)



#Use later (move to outbreak_functions script) ----
wis_expanding_window_over_time <- function(models_dt,
                                           times){
  # times <- unique(models_dt$target_end_date)
  print(times)
  score_summary <- NULL
  score_summary_by_location <- NULL
  
  for(i in seq(1, length(times)-1, 1)){
    print(i)
    time_in_q <- times[i]
    expanding_window_dt <- subset(models_dt, target_end_date <= time_in_q)
    # print(expanding_window_dt)
    tmp_score_summary <- expanding_window_dt %>%
      score() %>%
      summarise_scores(by = c("model"))
    tmp_score_summary[, effective_time:= times[i+1]] #effective time is time at which we use weights
    tmp_score_summary[, weight:= interval_score/sum(interval_score)]
    score_summary <- rbind(score_summary,
                           tmp_score_summary)
    # print(paste0("Here?"))
    tmp_score_summary_by_location <- expanding_window_dt %>%
      score() %>%
      summarise_scores(by = c("model", "location"))
    tmp_score_summary_by_location[, effective_time:= times[i+1]] #effective time is time at which we use weights
    tmp_score_summary_by_location[, weight:= interval_score/sum(interval_score), by = "location"]
    score_summary_by_location <- rbind(score_summary_by_location,
                                       tmp_score_summary_by_location)
  }
  return(list(score_summary,
              score_summary_by_location))
}
wis_expanding_results <- wis_expanding_window_over_time(quantile_log_cases_components_dt,
                                                        times = unique(quantile_log_cases_components_dt$target_end_date))

wis_expanding_results_spatially_homoegenous <- wis_expanding_results[[1]]
wis_expanding_results_space_dependent <- wis_expanding_results[[2]]
wis_expanding_results_space_dependent
#1) Iterative WIS-based weighting (Spatially homogeneous)
#Add equal weight for first month
tmp <- subset(wis_expanding_results_spatially_homoegenous, 
              effective_time == min(effective_time))
tmp[, weight:= 1/length(unique(model))]
tmp[, effective_time:= min(quantile_log_cases_components_dt$target_end_date)]
tmp
wis_expanding_results_spatially_homoegenous <- rbind(tmp, wis_expanding_results_spatially_homoegenous)
wis_expanding_results_spatially_homoegenous[, model_factor:= factor(model)]
# wis_expanding_results_spatially_homoegenous[, effective_time:= as.Date(effective_time)]
wis_expanding_results_spatially_homoegenous[, target_end_date:= as.Date(effective_time)]
wis_expanding_results_spatially_homoegenous$weight
ggplot(wis_expanding_results_spatially_homoegenous)+
  geom_line(aes(x = target_end_date, y = interval_score, color = model_factor))+
  theme_bw()+
  theme(legend.position = "bottom")




#WIS over time (Rolling Window) ----
wis_rolling_window_over_time <- function(models_dt,
                                         times){
  # times <- unique(models_dt$target_end_date)
  # print(times)
  score_summary <- NULL
  score_summary_by_location <- NULL
  runner_windows_list <- runner(times, k = 3)
  runner_windows_list <- runner_windows_list[3:length(runner_windows_list)] 
  for(i in seq(1, length(runner_windows_list), 1)){
    print(i)
    times_in_q <- runner_windows_list[[i]]
    # print(times_in_q)
    min_time <- times_in_q[1]
    centering_time <- times_in_q[2]
    max_time <- times_in_q[3]
    
    
    rolling_window_dt <- subset(models_dt, target_end_date >= min_time
                                & target_end_date <= max_time)
    # print(rolling_window_dt)
    # print(expanding_window_dt)
    tmp_score_summary <- rolling_window_dt %>%
      score() %>%
      summarise_scores(by = c("model"))
    tmp_score_summary[, centering_date:= centering_time]
    score_summary <- rbind(score_summary,
                           tmp_score_summary)
    # print(paste0("Here?"))
    tmp_score_summary_by_location <- rolling_window_dt %>%
      score() %>%
      summarise_scores(by = c("model", "location"))
    tmp_score_summary_by_location[, centering_date:= centering_time]
    score_summary_by_location <- rbind(score_summary_by_location,
                                       tmp_score_summary_by_location)
  }
  return(list(score_summary,
              score_summary_by_location))
}
quantile_log_cases_components_plus_ensembles_dt[, target_end_date:= as.Date(target_end_date)]

mean_rolling_wis_by_location <- 
  rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location[, list(MEAN = mean(interval_score)),
                                                                                  by = c("model_factor", "location")]
ggplot(mean_rolling_wis_by_location)+
  geom_col(aes(x = MEAN, y = model_factor, fill = model_factor))+
  theme_bw()+
  facet_wrap(location~., 
             scales = "free_x")+
  theme(legend.position = "bottom")
tmp <- subset(ptl_province_2018_2021_data,
              PROVINCE == "Ayabaca")
tmp[which(CASES > 1)]
ggplot(tmp, aes(x = TIME, y = CASES))+geom_line()
wis_expanding_results_spatially_homoegenous <- 
  merge(wis_expanding_results_spatially_homoegenous,
        unique(subset(input_quantile_dt, 
                      select = c("model", "target_end_date", "location", "true_value",
                                 "prediction", "quantile"))),
                              by = c("model", "target_end_date"))
setkeyv(weighted_quantile_dt, c("location", "IND"))
weighted_quantile_dt[, true_value:= round(true_value, digits = 8)]
weighted_quantile_dt
weighted_quantile_dt <- unique(weighted_quantile_dt)
weighted_quantile_dt
weighted_quantile_dt[, TRAINED_WEIGHT:= Lag(weights, 1), by = c("location", "model", "quantile")]
weighted_quantile_dt[which(target_end_date == min(target_end_date)), TRAINED_WEIGHT:= 1/length(unique(model))]
weighted_quantile_dt
weighted_quantile_forecasts_2018_2021_dt <- 
  weighted_quantile_dt[, list(prediction = sum(TRAINED_WEIGHT * prediction),
                              true_value = unique(true_value)),
                       by = c("location", "quantile", "target_end_date")]
weighted_quantile_forecasts_2018_2021_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
weighted_quantile_forecasts_2018_2021_dt[which(quantile == 0.5), caret::MAE(prediction, true_value)]
weighted_quantile_forecasts_2018_2021_dt[, model:= "pinball_trained_ensemble"]
weighted_quantile_forecasts_2018_2021_dt %>%
  score() %>% 
  summarise_scores(by = c("model"))


ggplot(wis_expanding_results_space_dependent,
       aes(x = effective_time, y = interval_score, color = model))+
  geom_line()+theme_bw()+
  facet_wrap(location ~., scales = "free_y")



#WIS at individual times
wis_by_time_points <- function(models_dt){
  score_summary <- models_dt %>%
    score() %>%
    summarise_scores(by = c("model", "target_end_date"))
  return(score_summary)
}
tmp <- wis_by_time_points(quantile_log_cases_components_dt)
tmp[, target_end_date:= as.Date(target_end_date)]
tmp
ggplot(tmp)+
  geom_line(aes(x = target_end_date, y = interval_score,
                color = model))+
  theme(legend.position = "bottom")
  


wis_by_location <- function(models_dt){
  score_summary <- models_dt %>%
    score() %>%
    summarise_scores(by = c("model", "location"))
  return(score_summary)
}
tmp <- wis_by_location(input_quantile_dt)
tmp[, model_factor:= as.factor(model)]
tmp
ggplot(tmp)+
  geom_point(aes(x = model_factor, y = interval_score))+
  facet_wrap(location ~. ,)+
  theme(legend.position = "bottom")


#Historical models ----
quantile_baseline_log_cases.pred_dt_2010_2018 <- readRDS(file = file.path(peru.province.out.dir, 
                                                                        paste0("quantile_baseline_log_cases.pred_dt_2010_2018.RDS")))
quantile_baseline_log_cases.pred_dt_2010_2018
quantile_historical_tcn_preds_dt <- readRDS(file = file.path(peru.province.python.out.dir,
                         "quantile_historical_tcn_preds_dt.RDS"))

quantile_historical_sarima_preds_dt <- readRDS(file = file.path(peru.province.python.out.dir,
                                                             "quantile_historical_sarima_preds_dt.RDS"))

quantile_historical_no_covars_fine_tuned_timegpt_preds_dt <- readRDS(file = file.path(peru.province.python.out.dir,
                                                                                      "quantile_historical_no_covars_fine_tuned_timegpt_preds_dt.RDS"))
quantile_historical_no_covars_fine_tuned_timegpt_preds_dt <- subset(quantile_historical_no_covars_fine_tuned_timegpt_preds_dt, select = scoring_columns)

quantile_historical_no_covars_fine_tuned_timegpt_preds_dt$model <- "finetuned_no_covars_timegpt"

quantile_historical_fine_tuned_timegpt_preds_dt <- readRDS(file = file.path(peru.province.python.out.dir,
                                                                                      "quantile_historical_fine_tuned_timegpt_preds_dt.RDS"))
quantile_historical_fine_tuned_timegpt_preds_dt <- subset(quantile_historical_fine_tuned_timegpt_preds_dt, select = scoring_columns)

climate_2010_2018_log_cases_quantile_dt <- readRDS(file = file.path(peru.province.inla.data.out.dir, "climate_2010_2018_log_cases_quantile_dt.RDS"))
climate_2010_2018_log_cases_quantile_dt <- merge(climate_2010_2018_log_cases_quantile_dt, 
                                                 subset(ptl_province_2010_2018_data, 
                                                        select = c("PROVINCE","TIME", "MONTH", "YEAR", "end_of_month")),
                                                 by = c("PROVINCE","TIME", "MONTH", "YEAR"))
setnames(climate_2010_2018_log_cases_quantile_dt, c("PROVINCE", "end_of_month"),
         c("location", "target_end_date"))
climate_2010_2018_log_cases_quantile_dt <- subset(climate_2010_2018_log_cases_quantile_dt,
                                                  select = scoring_columns)
climate_2010_2018_log_cases_quantile_dt


historical_quantile_log_cases_components_dt <- rbind(quantile_historical_fine_tuned_timegpt_preds_dt,
                                                     quantile_historical_no_covars_fine_tuned_timegpt_preds_dt,
                                    quantile_historical_tcn_preds_dt,
                                    climate_2010_2018_log_cases_quantile_dt,
                                    quantile_historical_sarima_preds_dt,
                                    quantile_baseline_log_cases.pred_dt_2010_2018)
historical_quantile_log_cases_components_dt[which(prediction <0), prediction:= 0]
# historical_quantile_log_cases_components_dt[, length(unique(model))]

#EW-NoC
#Median-NoC

#EW-NoBCM
#Median-NoBCM

#EW-NoB
#Median-NoB

# historical_quantile_log_cases_components_dt[, length(which(is.na(prediction))), by = "model"]
find_min_date_no_nas_after <- function(models_dt){
  tmp <- models_dt[which(is.na(prediction)),]
  tmp[, target_end_date]
  max_date <- max(tmp$target_end_date)
  return(max_date)
}
min_date <- find_min_date_no_nas_after(historical_quantile_log_cases_components_dt)
min_date <- max(historical_quantile_log_cases_components_dt[, list(MIN_DATE_BY_MODEL = min(target_end_date)),
                                                      by = "model"]$MIN_DATE_BY_MODEL)
historical_quantile_log_cases_components_dt <- historical_quantile_log_cases_components_dt[which(target_end_date > min_date), ]
historical_quantile_log_cases_components_dt[, model:= gsub("historical_", "",model )]
# process_quantile_predictions(historical_quantile_log_cases_components_dt)
historical_quantile_log_cases_components_dt %>% score() %>% summarise_scores(by = "model")

# quantiles_random_forest_for_scoring <- readRDS(file = file.path(peru.province.xgb.out.dir, 
#                  paste0("quantiles_random_forest_for_scoring.RDS")))
# quantiles_random_forest_for_scoring <- subset(quantiles_random_forest_for_scoring, 
#                                               select = scoring_columns)
# quantiles_random_forest_for_scoring


#Form untrained ensembles ----
historical_quantile_log_cases_components_plus_ensembles_dt <- 
  form_untrained_ensembles(historical_quantile_log_cases_components_dt)
historical_quantile_log_cases_components_plus_ensembles_dt[, length(prediction), by = "model"]
head(historical_quantile_log_cases_components_plus_ensembles_dt, 10)




#Ignore ----
tmp <- form_untrained_ensembles(historical_quantile_log_cases_components_dt)
head(tmp, 10)
#Score
historical_quantile_log_cases_components_plus_ensembles_dt_scores <- historical_quantile_log_cases_components_plus_ensembles_dt %>% score() %>% 
  summarise_scores(by = c("model"))

historical_quantile_log_cases_components_plus_ensembles_dt_scores[order(interval_score)]

#Coverage
tmp <-process_summary_predictions(summary_predictions(historical_quantile_log_cases_components_plus_ensembles_dt))
tmp
process_quantile_predictions(historical_quantile_log_cases_components_plus_ensembles_dt)

#Add in extra details (e.g. LAT_PROV_IND)
historical_quantile_log_cases_components_plus_ensembles_dt_for_plotting <- 
  merge(historical_quantile_log_cases_components_plus_ensembles_dt,
        unique(subset(ptl_province_inla_df, select = c("PROVINCE", "LAT_PROV_IND"))),
        by.x = "location", by.y = "PROVINCE")


historical_quantile_log_cases_components_plus_ensembles_dt_for_plotting %>%
  score() %>%
  summarise_scores(by = c("model", "quantile")) %>%
  plot_quantile_coverage() + geom_line(aes(y = quantile_coverage), linewidth = 1.5, alpha = 0.6)+
  facet_wrap(model~. ,)+
  theme(text = element_text(size = 32),
        axis.text.x = element_text(size=32),
        axis.text.y = element_text(size=32),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        axis.title=element_text(size=32), 
        legend.text=element_text(size=24),
        legend.position = "bottom")+
  guides(color=guide_legend("Model"))





#Form trained ensembles -----
quantile_log_cases_components_dt[, target_end_date:= as.Date(target_end_date)]
historical_quantile_log_cases_components_dt[, target_end_date:= as.Date(target_end_date)]

combined_quantile_log_cases_components_dt <- rbind(historical_quantile_log_cases_components_dt,
                                                   quantile_log_cases_components_dt)
input_quantile_dt <- copy(combined_quantile_log_cases_components_dt)
# num_pred_points <- nrow(ptl_province_2018_2021_data)
setkeyv(input_quantile_dt, c("location", "model","target_end_date", "quantile"))
num_ensemble_components <- length(unique(input_quantile_dt$model))
num_quantile_levels <- length(quantiles)
# pred_points_dt <- unique(subset(quantile_log_cases_components_dt, 
#                                 select = c("location", "model", "target_end_date")))
# setkeyv(pred_points_dt, c("model","location", "target_end_date"))
# pred_points_dt[, IND:= seq(1, length(target_end_date)), by = "model"]

input_quantile_dt[, target_end_date:= as.Date(target_end_date)]
input_quantile_dt[, lagged_date:= as.Date(Lag(target_end_date, 11)), by = c("location", "model", "quantile")]
input_quantile_dt[, lagged_date:= as.Date(lagged_date)]
input_quantile_dt[, in_testing:= ifelse(year(target_end_date) >= 2018, 1, 0)]
max_date <- max(input_quantile_dt[which(in_testing ==0)]$target_end_date) 
#Include as we use realistic application of weights i.e. from historical performance (minimum 1 month lag)
input_quantile_dt[which(target_end_date == max_date), in_testing:= 1]
dates = (unique(input_quantile_dt[which(in_testing ==1)]$target_end_date))
number_dates = length(unique(input_quantile_dt[which(in_testing ==1)]$target_end_date))
lagged_dates <- unique(input_quantile_dt[which(in_testing == 1)]$lagged_date)
lagged_dates
# input_quantile_dt <- merge(input_quantile_dt,pred_points_dt, by = c("model","location", "target_end_date"))
# setkeyv(input_quantile_dt, c("location", "model","target_end_date", "quantile"))
# input_quantile_dt
true_log_cases_data <- copy(ptl_province_inla_df)
true_log_cases_data <- subset(true_log_cases_data, end_of_month >= max_date)
setkeyv(true_log_cases_data, c("PROVINCE", "end_of_month"))


iteratively_train_quantile_ensemble_runner <- function(quantiles,
                                                       dates,
                                                       lagged_dates,
                                                       number_dates,
                                                       input_quantile_dt,
                                                       true_data){
  
  #ovr_num_pred_points = total number points (includes all quantile predictions)
  #input_quantile_dt is predictions for entire testing window (2018-2021)
  #historical_input_quantile_dt is historical predictions
  
  # prediction_indices <- sort(unique(input_quantile_dt$IND))
  
  model_names <- sort(unique(input_quantile_dt$model))
  ensemble_model_name <- paste(model_names, collapse = "_")
  # dates <- sort(unique(input_quantile_dt$target_end_date))
  num_ensemble_components <- length(unique(input_quantile_dt$model))
  num_quantile_levels <- length(quantiles)
  
  #data.tables to store results
  ovr_preds_dt <- NULL
  ovr_model_weights_dt <- NULL
  
  for(t in 1:number_dates){
    new_date <- dates[t]
    lag_12_date <- lagged_dates[t]
    tmp_true_data <- subset(true_data, end_of_month <= new_date)
    tmp_true_data <- subset(tmp_true_data, end_of_month >=  lag_12_date)
    setkeyv(tmp_true_data, c("PROVINCE", "end_of_month"))
    
    tmp_input_quantile_dt <- subset(input_quantile_dt, target_end_date <= new_date)
    historical_input_quantile_dt <- subset(tmp_input_quantile_dt, target_end_date >=  lag_12_date)
    setkeyv(historical_input_quantile_dt, c("location", "model", 
                                            "target_end_date", "quantile"))
    tmp_dates_dt <- unique(subset(historical_input_quantile_dt,
                                  select = c("target_end_date", "location")))
    setkeyv(tmp_dates_dt, c("location", "target_end_date"))
    tmp_dates_dt[, IND:= seq(1, nrow(tmp_dates_dt))]
    num_pred_points <- nrow(tmp_dates_dt)
    historical_input_quantile_dt <- merge(historical_input_quantile_dt, 
          tmp_dates_dt, by = c("target_end_date", "location"))
    setkeyv(historical_input_quantile_dt, c("location", "model", 
                                            "target_end_date", "quantile"))
    
    # historical_input_quantile_dt[, IND:= seq(1, 12), by = c("location", "model", "quantile")]
    setkeyv(historical_input_quantile_dt, c("location", "model", 
                                            "target_end_date", "quantile"))
    
    #num_pred_points = nrow()
    qarr <- array(NA, dim = c(num_pred_points, #12*14
                              num_ensemble_components, #6
                              num_quantile_levels)) #23
    print(dim(qarr))
    # print(dim(qarr))
    prediction_indices <- sort(unique(historical_input_quantile_dt$IND)) # Should always be 12 here
    # print(prediction_indices)
    for(i in 1:num_pred_points){
      # print(paste0("i: ", i))
      for(j in 1:num_ensemble_components){
        # print(paste0("j: ", j))
        # print(prediction_indices[i])
        # print(unique(model_names[j]))
        # print(historical_input_quantile_dt[which(IND == prediction_indices[i] &
        #                                            model== unique(model_names[j])), ])
        qarr[i,j,] <- 
          historical_input_quantile_dt[which(IND == prediction_indices[i] & 
                                               model== unique(model_names[j])), ]$prediction         
      }
    }
    quantile_ensemble_weights <- quantile_ensemble(qarr,
                                                   tmp_true_data$LOG_CASES,
                                                   quantiles)
    
    tmp_trained_quantile_ensemble <- copy(input_quantile_dt)
    model_weights_dt <- data.table(model = model_names, weights = quantile_ensemble_weights$alpha)
    tmp_trained_quantile_ensemble <- 
      merge(tmp_trained_quantile_ensemble, 
            model_weights_dt, by = "model")
    model_weights_dt[, target_end_date:= new_date]
    ovr_model_weights_dt <- rbind(ovr_model_weights_dt,
                                  model_weights_dt)

  }
  return(ovr_model_weights_dt)
}
#Present results for no lookback window in appendix
runner_trained_ensemble_weights_quantile_log_cases_dt <- iteratively_train_quantile_ensemble_runner(quantiles,
                                                       dates,
                                                       lagged_dates,
                                                       number_dates,
                                                       input_quantile_dt,
                                                       true_log_cases_data)

saveRDS(runner_trained_ensemble_weights_quantile_log_cases_dt,
        file = file.path(peru.province.out.dir, "runner_trained_ensemble_weights_quantile_log_cases_dt.RDS"))
runner_trained_ensemble_weights_quantile_log_cases_dt <- readRDS(file = file.path(peru.province.out.dir, "runner_trained_ensemble_weights_quantile_log_cases_dt.RDS"))
runner_trained_ensemble_weights_quantile_log_cases_dt


runner_trained_ensemble_weights_quantile_log_cases_dt[, model_factor:= factor(model)]
runner_trained_ensemble_weights_quantile_log_cases_dt[, target_end_date:= as.Date(target_end_date)]

p1 <- ggplot(runner_trained_ensemble_weights_quantile_log_cases_dt)+
  geom_line(aes(x = target_end_date, y = weights, color = model_factor))+
  theme_bw()+
  theme(legend.position = "bottom")

p2 <- ggplot(ptl_province_2018_2021_data)+
  geom_boxplot(aes(x = end_of_month, y = LOG_CASES))+
  theme_bw()+
  theme(legend.position = "bottom")
p2
tmp <- copy(runner_trained_ensemble_weights_quantile_log_cases_dt)
tmp[, target_end_date:= Lag(target_end_date, -1), by = c("model")]
tmp
input_quantile_dt <- merge(input_quantile_dt,
                           tmp, 
                           by = c("model", "target_end_date"))
input_quantile_dt
input_quantile_dt[, true_value:= round(true_value, digits = 8)]
input_quantile_dt <- unique(input_quantile_dt)
trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt <- 
  input_quantile_dt[, list(prediction = sum(weights * prediction),
                                        true_value = unique(true_value)),
                                 by = c("location", "quantile", "target_end_date")]
trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt[which(quantile == 0.5), caret::MAE(prediction, true_value)]
trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt[, model:= "pinball_trained_ensemble"]
trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt %>%
  score() %>% 
  summarise_scores(by = c("model"))




#By province 
provinces <- unique(combined_quantile_log_cases_components_dt$location)
runner_trained_ensemble_weights_by_province_quantile_log_cases_dt <- NULL
for(i in 1:length(provinces)){
  prov_in_q <- provinces[i]
  input_quantile_dt <- copy(combined_quantile_log_cases_components_dt)
  # num_pred_points <- nrow(ptl_province_2018_2021_data)
  setkeyv(input_quantile_dt, c("location", "model","target_end_date", "quantile"))
  input_quantile_dt <- subset(input_quantile_dt, location == prov_in_q)
  num_ensemble_components <- length(unique(input_quantile_dt$model))
  num_quantile_levels <- length(quantiles)
  input_quantile_dt[, target_end_date:= as.Date(target_end_date)]
  input_quantile_dt[, lagged_date:= as.Date(Lag(target_end_date, 11)), by = c("location", "model", "quantile")]
  input_quantile_dt[, lagged_date:= as.Date(lagged_date)]
  input_quantile_dt[, in_testing:= ifelse(year(target_end_date) >= 2018, 1, 0)]
  max_date <- max(input_quantile_dt[which(in_testing ==0)]$target_end_date) 
  #Include as we use realistic application of weights i.e. from historical performance (minimum 1 month lag)
  input_quantile_dt[which(target_end_date == max_date), in_testing:= 1]
  dates = (unique(input_quantile_dt[which(in_testing ==1)]$target_end_date))
  number_dates = length(unique(input_quantile_dt[which(in_testing ==1)]$target_end_date))
  lagged_dates <- unique(input_quantile_dt[which(in_testing == 1)]$lagged_date)
  lagged_dates
  true_log_cases_data <- copy(ptl_province_inla_df)
  true_log_cases_data <- subset(true_log_cases_data, end_of_month >= max_date)
  true_log_cases_data <- subset(true_log_cases_data, PROVINCE == prov_in_q)
  setkeyv(true_log_cases_data, c("PROVINCE", "end_of_month"))
  
  
  tmp_runner_trained_ensemble_weights_quantile_log_cases_dt <- iteratively_train_quantile_ensemble_runner(quantiles,
                                                                                                      dates,
                                                                                                      lagged_dates,
                                                                                                      number_dates,
                                                                                                      input_quantile_dt,
                                                                                                      true_log_cases_data)
  tmp_runner_trained_ensemble_weights_quantile_log_cases_dt[, location:= prov_in_q]
  runner_trained_ensemble_weights_by_province_quantile_log_cases_dt <- 
    rbind(runner_trained_ensemble_weights_by_province_quantile_log_cases_dt,
          tmp_runner_trained_ensemble_weights_quantile_log_cases_dt)
}


saveRDS(runner_trained_ensemble_weights_by_province_quantile_log_cases_dt,
        file = file.path(peru.province.out.dir, "runner_trained_ensemble_weights_by_province_quantile_log_cases_dt.RDS"))

runner_trained_ensemble_weights_by_province_quantile_log_cases_dt[, model_factor:= factor(model)]
runner_trained_ensemble_weights_by_province_quantile_log_cases_dt[, target_end_date:= as.Date(target_end_date)]

p1 <- ggplot(runner_trained_ensemble_weights_by_province_quantile_log_cases_dt)+
  geom_line(aes(x = target_end_date, y = weights, color = model_factor))+
  theme_bw()+
  facet_wrap(location ~., )+
  theme(legend.position = "bottom")
p1
p2 <- ggplot(ptl_province_2018_2021_data)+
  geom_boxplot(aes(x = end_of_month, y = LOG_CASES))+
  theme_bw()+
  theme(legend.position = "bottom")
p2
ggplot(runner_trained_ensemble_weights_by_province_quantile_log_cases_dt)+
  geom_boxplot(aes(x = model_factor, y = weights, color = model_factor))+
  facet_wrap(location~.,)+
  theme(legend.position = "bottom")

summary_province_dependent_weights_by_province <-  
  runner_trained_ensemble_weights_by_province_quantile_log_cases_dt[,list(MEDIAN = median(weights),
                                                                          MEAN = mean(weights)),
                                                                    by = c("location", "model")]
ggplot(summary_province_dependent_weights_by_province, aes(x = model, y = median ))


runner_trained_ensemble_weights_by_province_quantile_log_cases_dt
tmp <- copy(runner_trained_ensemble_weights_by_province_quantile_log_cases_dt)
tmp[, target_end_date:= Lag(target_end_date, -1), by = c("model", "location")]
tmp
input_quantile_dt <- copy(combined_quantile_log_cases_components_dt)

input_quantile_dt <- merge(input_quantile_dt,
                           tmp, 
                           by = c("model", "target_end_date",
                                  "location"))
input_quantile_dt
input_quantile_dt[, true_value:= round(true_value, digits = 8)]
input_quantile_dt
trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt <- 
  input_quantile_dt[, list(prediction = sum(weights * prediction),
                           true_value = unique(true_value)),
                    by = c("location", "quantile", "target_end_date")]
trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt[which(quantile == 0.5), caret::R2(prediction, true_value)]
trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt[which(quantile == 0.5), caret::MAE(prediction, true_value)]
trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt[, model:= "pinball_trained_ensemble_by_province"]
trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt %>%
  score() %>% 
  summarise_scores(by = c("model"))
tmp <- process_summary_predictions(summary_predictions(trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt))
tmp



#Merge in trained approaches ----
quantile_log_cases_components_plus_ensembles_dt <- rbind(quantile_log_cases_components_plus_ensembles_dt,
      trained_ensemble_quantile_log_cases_forecasts_2018_2021_dt)
quantile_log_cases_components_plus_ensembles_dt <- rbind(quantile_log_cases_components_plus_ensembles_dt,
                                                         trained_ensemble_by_province_quantile_log_cases_forecasts_2018_2021_dt)

quantile_log_cases_components_plus_ensembles_dt[, length(prediction), by = "model"]





#FIX NAMES ----
original_model_names <- sort(unique(quantile_log_cases_components_plus_ensembles_dt$model))
original_model_names
new_model_names <- c("Baseline","Bayes-Climate","EW-Mean *","EW-Mean-NoBase *","Ew-Mean-NoBayes *", "EW-Mean-NoCov *","TimeGPT",
                     "TimeGPT-NoCov ", "Median *", "Median-NoBase *",   "Median-NoBayes *" ,
                     "Median-NoCov *","Trained *","Prov-Trained *","SARIMA","TCN")
new_model_names
#extract hex color codes for a plot with three elements in ggplot2 
hex <- hue_pal()(length(new_model_names))
historical_original_model_names <- sort(unique(historical_quantile_log_cases_components_plus_ensembles_dt$model))
historical_original_model_names
historical_new_model_names <- c("Baseline","Bayes-Climate","EW-Mean *","EW-Mean-NoBase *","Ew-Mean-NoBayes *", "EW-Mean-NoCov *","TimeGPT",
                                "TimeGPT-NoCov ", "Median *", "Median-NoBase *",   "Median-NoBayes *" ,
                                "Median-NoCov *","SARIMA","TCN")
historical_new_model_names
historical_model_colours <- hex[which(new_model_names %in% historical_new_model_names)]
show_col(historical_model_colours)
setkey(quantile_log_cases_components_plus_ensembles_coverage_dt, "model")
quantile_log_cases_components_plus_ensembles_dt[, model:= factor(model,
                                                                 labels = new_model_names)]



historical_quantile_log_cases_components_plus_ensembles_dt[, model:= factor(model,
                                                                 labels = historical_new_model_names)]
historical_quantile_log_cases_components_plus_ensembles_dt
setkey(quantile_log_cases_components_plus_ensembles_dt, "model")

setkey(historical_quantile_log_cases_components_plus_ensembles_dt, "model")
historical_quantile_log_cases_components_plus_ensembles_dt[, model_factor:= NULL]
#INSERT LATITUDE AND LONGITUDE INDICATORS ----
tmp <- unique(subset(ptl_province_inla_df, 
                     select = c("PROVINCE", "LAT_PROV_IND", "LONG_PROV_IND")))
historical_quantile_log_cases_components_plus_ensembles_dt <- 
  merge(historical_quantile_log_cases_components_plus_ensembles_dt,
      tmp, by.y = "PROVINCE", by.x = "location")
quantile_log_cases_components_plus_ensembles_dt <- 
  merge(quantile_log_cases_components_plus_ensembles_dt,
        tmp, by.y = "PROVINCE", by.x = "location")

#All figures and tables ----

#Score
quantile_log_cases_components_plus_ensembles_dt_scores <- quantile_log_cases_components_plus_ensembles_dt %>% score() %>% 
  summarise_scores(by = c("model"))
quantile_log_cases_components_plus_ensembles_dt_scores[order(interval_score)]
quantile_log_cases_components_plus_ensembles_dt_scores[order(coverage_deviation)]
quantile_log_cases_components_plus_ensembles_dt_scores_by_location <- quantile_log_cases_components_plus_ensembles_dt %>% score() %>% 
  summarise_scores(by = c("model", "location"))
quantile_log_cases_components_plus_ensembles_dt
tmp <- quantile_log_cases_components_plus_ensembles_dt %>% score()
tmp[, length(unique(model))]
summarised_wis_quantile_log_cases_by_province <- tmp[, list(mean_interval_score = mean(interval_score),
                                                            median_interval_score = median(interval_score),
                                                            interval_score_ci_l = quantile(interval_score, probs = 0.25),
                                                            interval_score_ci_u = quantile(interval_score, probs = 0.75)), by = c("model", "location")]
summarised_wis_quantile_log_cases_by_province[, model_factor:= as.factor(model)]

ggplot(summarised_wis_quantile_log_cases_by_province)+ 
  geom_errorbar(aes(x = model_factor, ymin = interval_score_ci_l, ymax =  interval_score_ci_u,
                    color = model_factor))+
  geom_point(aes(x = model_factor,  y = median_interval_score))+
  theme_bw()+
  facet_wrap(location ~., scales = "free_y")+
  theme(legend.position = "bottom")


quantile_log_cases_components_plus_ensembles_dt_scores_by_location %>% plot_heatmap(x= "location", metric = "interval_score")
quantile_log_cases_components_plus_ensembles_dt_scores[order(interval_score)]
tmp <-process_summary_predictions(summary_predictions(quantile_log_cases_components_plus_ensembles_dt))
quantile_log_cases_components_plus_ensembles_performance_dt <- 
  process_quantile_predictions(quantile_log_cases_components_plus_ensembles_dt)
quantile_log_cases_components_plus_ensembles_performance_dt[order(r2, decreasing = TRUE)]
quantile_log_cases_components_plus_ensembles_coverage_dt <- 
  process_summary_predictions(summary_predictions(quantile_log_cases_components_plus_ensembles_dt))
quantile_log_cases_components_plus_ensembles_coverage_dt[order(COVERAGE, decreasing = TRUE)]


#Coverage Plots 


#Quantile
quantile_coverage_function(quantile_log_cases_components_plus_ensembles_dt,
                           "quantile_log_cases_components_plus_ensembles",
                           historical = FALSE)
quantile_coverage_function(historical_quantile_log_cases_components_plus_ensembles_dt,
                           "historical_quantile_log_cases_components_plus_ensembles",
                           historical = TRUE)
#Interval
quantile_log_cases_interval_coverage_dt <- 
  interval_coverage_function(quantile_log_cases_components_plus_ensembles_dt,
                           "quantile_log_cases_components_plus_ensembles",
                           historical = FALSE)

historical_quantile_log_cases_interval_coverage_dt <- interval_coverage_function(historical_quantile_log_cases_components_plus_ensembles_dt,
                           "historical_quantile_log_cases_components_plus_ensembles",
                           historical = TRUE)

#Plot WIS ----
rolling_wis_quantile_log_cases_components_plus_ensembles_dt <- 
  wis_rolling_window_over_time(quantile_log_cases_components_plus_ensembles_dt,
                               times = unique(quantile_log_cases_components_plus_ensembles_dt$target_end_date))

rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt <- rolling_wis_quantile_log_cases_components_plus_ensembles_dt[[1]]
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt[, model_factor:= factor(model, levels = new_model_names)]
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt[, target_end_date:= as.Date(centering_date)]
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt

baseline_dt <- subset(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt, model == "Baseline")
baseline_dt <- subset(baseline_dt, select = c("interval_score", "target_end_date"))
setnames(baseline_dt, "interval_score", "baseline_interval_score")
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt <- merge(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt,
                                                                             baseline_dt, by = "target_end_date")
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt[, scaled_relative_skill:= interval_score/baseline_interval_score]
setkeyv(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt, "model")
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt[, width := ifelse(model == "Median *", 0.75, 0.6)]
# setkeyv(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt,
#         c("model"))
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt
rolling_wis_plot_over_time <- ggplot(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt)+
  geom_line(aes(x = target_end_date, y = scaled_relative_skill, colour = model_factor),
            linewidth = 1.1)+theme_bw()+
  gghighlight::gghighlight(model_factor == "Median *", use_direct_label = FALSE,
                           unhighlighted_params = list(colour = NULL, alpha = 0.35))+
  labs(x = "Year", y = "Relative WIS (log scale)")+
  theme(text = element_text(size = 30),
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        axis.title=element_text(size=24), 
        legend.text=element_text(size=24),
        legend.position = "bottom",
        plot.margin=unit(c(1,1,1,1),"cm"))+
  guides(color=guide_legend("Model", override.aes = list(linewidth=1.2, alpha = 0.5)))
rolling_wis_plot_over_time
# geom_line(tmp, aes(x = target_end_date, y = scaled_rel_skill, color = model, linewidth = 1.5))
setkeyv(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt, c("target_end_date","model"))
model_ind <- rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt[, list(MODEL_IND = which.min(interval_score)), by = "target_end_date"]
model_dt <- data.table(model = unique(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt$model))
model_dt[, MODEL_IND:= seq(1, nrow(model_dt))]
model_ind <- merge(model_ind,
                   model_dt, by =  "MODEL_IND")
setkeyv(model_ind, "target_end_date")
model_ind
tmp <- (ptl_province_2018_2021_data[, list(MEAN_LOG_CASES = mean(LOG_CASES)), by = "end_of_month"])
tmp[, end_of_month:= as.Date(end_of_month)]
ggplot(tmp)+ geom_line(aes(x = end_of_month, y = MEAN_LOG_CASES)) + theme_bw()


ggplot(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt)+
  geom_col(aes(x = model_factor, y = interval_score, fill = model_factor))+
  theme_bw()+
  facet_wrap(target_end_date ~., scales = "free_y")+
  theme(legend.position = "bottom")

rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location <- 
  rolling_wis_quantile_log_cases_components_plus_ensembles_dt[[2]]
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location[, centering_date:= as.Date(centering_date)]
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location[, model_factor:= factor(model,
                                                                                                        levels = new_model_names)]
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location
ggplot(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location)+
  geom_line(aes(x = centering_date, y = interval_score, color = model_factor))+
  theme_bw()+
  facet_wrap(location~., 
             scales = "free_y")+
  theme(legend.position = "bottom")

rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location
baseline_dt <- subset(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location, 
                      model == "Baseline")
baseline_dt <- subset(baseline_dt, select = c("interval_score", "centering_date", "location"))
baseline_dt
setnames(baseline_dt, "interval_score", "baseline_interval_score")
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location <- 
  merge(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location,
        baseline_dt, by = c("centering_date", "location"))
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location[, scaled_relative_skill:= interval_score/baseline_interval_score]
rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location$scaled_relative_skill

rolling_wis_plot_over_time_by_location <- ggplot(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location)+
  geom_line(aes(x = centering_date, y = scaled_relative_skill, colour = model_factor),
            linewidth = 0.8)+theme_bw()+
  gghighlight::gghighlight(model_factor == "Median *", use_direct_label = FALSE,
                           unhighlighted_params = list(colour = NULL, alpha = 0.3))+
  facet_wrap(location ~., )+
  labs(x = "Year", y = "Relative WIS (log scale)")+
  theme(text = element_text(size = 30),
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        axis.title=element_text(size=24), 
        legend.text=element_text(size=24),
        legend.position = "bottom",
        plot.margin=unit(c(1,1,1,1),"cm"))+
  guides(color=guide_legend("Model", override.aes = list(linewidth=0.9)))
rolling_wis_plot_over_time_by_location





rolling_wis_violin_plot_by_location <- ggplot(rolling_wis_quantile_log_cases_components_plus_ensembles_summary_dt_by_location)+
  geom_violin(aes(x = interval_score, y = model_factor, fill = model_factor),
              alpha = 0.8)+theme_bw()+
  # gghighlight::gghighlight(model_factor == "Median *", use_direct_label = FALSE,
  #                          unhighlighted_params = list(colour = NULL, alpha = 0.3))+
  facet_wrap(location ~., )+
  labs(x = "WIS (log scale)")+  theme(axis.title.y=element_blank(),
                                      axis.text.y=element_blank(),
                                      axis.ticks.y=element_blank())+
  theme(text = element_text(size = 30),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        axis.title=element_text(size=24), 
        legend.text=element_text(size=24),
        legend.position = "right",
        plot.margin=unit(c(1,1,1,1),"cm"))+
  guides(fill=guide_legend("Model", override.aes = list(alpha=0.8)))
rolling_wis_violin_plot_by_location
ggsave(rolling_wis_violin_plot_by_location,
       file = file.path(peru.province.ensemble.out.dir,
                        "rolling_wis_violin_plot_by_location.pdf"),
       h = 22, w = 22)

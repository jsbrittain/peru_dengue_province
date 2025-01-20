process_predictions <- function(data) {
  summary_accuracy_performance_median <-
    data[, list(
      r2 = caret::R2(MEDIAN, DIR),
      rmse = caret::RMSE(MEDIAN, DIR),
      mae = caret::MAE(MEDIAN, DIR),
      coverage = length(which(DIR >= CI_L & DIR <= CI_U)) / length(DIR)
    ),
    by = "model"
    ]

  return(summary_accuracy_performance_median)
}
process_quantile_predictions <- function(models_dt) {
  results_dt <- models_dt[which(quantile == 0.5),
    list(
      r2 = caret::R2(prediction, true_value),
      mae = caret::MAE(prediction, true_value)
    ),
    by = c("model")
  ]

  return(results_dt)
}



# OUTBREAK DETECTION FUNCTIONS ----


# Process detection capabilities at given threshold
quantile_threshold_150_function <- function(data) {
  results_dt <- data.table(
    true_pos = rep(0, 1),
    false_pos = rep(0, 1),
    diff_tpr_fpr = rep(0, 1),
    accuracy = rep(0, 1),
    precision = rep(0, 1)
  )
  for (i in 1:length(unique(data$quantile))) {
    threshold <- unique(data$quantile)[i]
    print(threshold)
    quantile_data <- copy(data)
    # quantile_data <- subset(quantile_data, quantile == threshold)
    # Calculate the combined score to be minimized/maximized
    quantile_data[, OUTBREAK_150_PRED := ifelse(prediction >= 150, 1, 0)]
    quantile_data[, OUTBREAK_150_TRUE := ifelse(true_value >= 150, 1, 0)]
    print(dim(quantile_data))
    confusion_mat <- confusionMatrix(
      data = as.factor(quantile_data$OUTBREAK_150_PRED),
      reference = as.factor(quantile_data$OUTBREAK_150_TRUE),
      positive = "1"
    )

    true_pos_150 <- unname(confusion_mat$byClass[1]) # Sensitivity/Hit Rate
    fpr_150 <- 1 - unname(confusion_mat$byClass[2]) # 1-TNR = 1-Specificity
    acc <- unname(confusion_mat$overall[1])
    prec <- unname(confusion_mat$byClass[3])

    roc_150 <- roc(
      quantile_data$OUTBREAK_150_TRUE,
      quantile_data$OUTBREAK_150_PRED
    )
    auc_150 <- auc(roc_150)
    print(ci.auc(quantile_data$OUTBREAK_150_TRUE, quantile_data$OUTBREAK_150_PRED))
    # Define the combined score (e.g., variable1 - variable2, depending on your goals)
    combined_score <- true_pos_150 - fpr_150
    results_dt[, true_pos := true_pos_150]
    results_dt[, false_pos := fpr_150]
    results_dt[, diff_tpr_fpr := combined_score]
    results_dt[, accuracy := acc]
    results_dt[, precision := prec]
    results_dt[, auc := auc_150]
  }
  return(results_dt)
}

quantile_threshold_50_function <- function(data) {
  results_dt <- data.table(
    true_pos = rep(0, 1),
    false_pos = rep(0, 1),
    diff_tpr_fpr = rep(0, 1),
    accuracy = rep(0, 1),
    precision = rep(0, 1)
  )
  for (i in 1:length(unique(data$quantile))) {
    threshold <- unique(data$quantile)[i]
    print(threshold)
    quantile_data <- copy(data)
    # quantile_data <- subset(quantile_data, quantile == threshold)
    # Calculate the combined score to be minimized/maximized
    quantile_data[, OUTBREAK_50_PRED := ifelse(prediction >= 50, 1, 0)]
    quantile_data[, OUTBREAK_50_TRUE := ifelse(true_value >= 50, 1, 0)]
    print(dim(quantile_data))
    confusion_mat <- confusionMatrix(
      data = as.factor(quantile_data$OUTBREAK_50_PRED),
      reference = as.factor(quantile_data$OUTBREAK_50_TRUE),
      positive = "1"
    )

    true_pos_50 <- unname(confusion_mat$byClass[1]) # Sensitivity/Hit Rate
    fpr_50 <- 1 - unname(confusion_mat$byClass[2]) # 1-TNR = 1-Specificity
    acc <- unname(confusion_mat$overall[1])
    prec <- unname(confusion_mat$byClass[3])

    roc_50 <- roc(
      quantile_data$OUTBREAK_50_TRUE,
      quantile_data$OUTBREAK_50_PRED
    )
    auc_50 <- auc(roc_50)
    print(ci.auc(quantile_data$OUTBREAK_50_TRUE, quantile_data$OUTBREAK_50_PRED))
    # Define the combined score (e.g., variable1 - variable2, depending on your goals)
    combined_score <- true_pos_50 - fpr_50
    results_dt[, true_pos := true_pos_50]
    results_dt[, false_pos := fpr_50]
    results_dt[, diff_tpr_fpr := combined_score]
    results_dt[, accuracy := acc]
    results_dt[, precision := prec]
    results_dt[, auc := auc_50]
  }
  return(results_dt)
}


# Function for detection capabilities at different thresholds
quantile_threshold_150_function_choose_cut_off <- function(data) {
  results_dt <- data.table(
    cut_off = (unique(data$quantile)),
    true_pos = rep(0, length(data$quantile)),
    false_pos = rep(0, length(data$quantile)),
    diff_tpr_fpr = rep(0, length(data$quantile)),
    accuracy = rep(0, length(data$quantile)),
    precision = rep(0, length(data$quantile))
  )
  for (i in 1:length(unique(data$quantile))) {
    threshold <- unique(data$quantile)[i]
    print(threshold)
    quantile_data <- copy(data)
    quantile_data <- subset(quantile_data, quantile == threshold)
    # Calculate the combined score to be minimized/maximized
    quantile_data[, OUTBREAK_150_PRED := ifelse(prediction >= 150, 1, 0)]
    quantile_data[, OUTBREAK_150_TRUE := ifelse(true_value >= 150, 1, 0)]
    print(dim(quantile_data))
    confusion_mat <- confusionMatrix(
      data = as.factor(quantile_data$OUTBREAK_150_PRED),
      reference = as.factor(quantile_data$OUTBREAK_150_TRUE),
      positive = "1"
    )

    true_pos_150 <- unname(confusion_mat$byClass[1]) # Sensitivity/Hit Rate
    fpr_150 <- 1 - unname(confusion_mat$byClass[2]) # 1-TNR = 1-Specificity
    acc <- unname(confusion_mat$overall[1])
    prec <- unname(confusion_mat$byClass[3])

    roc_150 <- roc(
      quantile_data$OUTBREAK_150_TRUE,
      quantile_data$OUTBREAK_150_PRED
    )
    auc_150 <- auc(roc_150)
    print(ci.auc(quantile_data$OUTBREAK_150_TRUE, quantile_data$OUTBREAK_150_PRED))
    # Define the combined score (e.g., variable1 - variable2, depending on your goals)
    combined_score <- true_pos_150 - fpr_150
    results_dt[which(cut_off == threshold), true_pos := true_pos_150]
    results_dt[which(cut_off == threshold), false_pos := fpr_150]
    results_dt[which(cut_off == threshold), diff_tpr_fpr := combined_score]
    results_dt[which(cut_off == threshold), accuracy := acc]
    results_dt[which(cut_off == threshold), precision := prec]
    results_dt[which(cut_off == threshold), auc := auc_150]
  }
  return(results_dt)
}

quantile_threshold_50_function_choose_cut_off <- function(data) {
  results_dt <- data.table(
    cut_off = (unique(data$quantile)),
    true_pos = rep(0, length(data$quantile)),
    false_pos = rep(0, length(data$quantile)),
    diff_tpr_fpr = rep(0, length(data$quantile)),
    accuracy = rep(0, length(data$quantile)),
    precision = rep(0, length(data$quantile))
  )
  for (i in 1:length(unique(data$quantile))) {
    threshold <- unique(data$quantile)[i]
    print(threshold)
    quantile_data <- copy(data)
    quantile_data <- subset(quantile_data, quantile == threshold)
    # Calculate the combined score to be minimized/maximized
    quantile_data[, OUTBREAK_50_PRED := ifelse(prediction >= 50, 1, 0)]
    quantile_data[, OUTBREAK_50_TRUE := ifelse(true_value >= 50, 1, 0)]
    print(dim(quantile_data))
    confusion_mat <- confusionMatrix(
      data = as.factor(quantile_data$OUTBREAK_50_PRED),
      reference = as.factor(quantile_data$OUTBREAK_50_TRUE),
      positive = "1"
    )

    true_pos_50 <- unname(confusion_mat$byClass[1]) # Sensitivity/Hit Rate
    fpr_50 <- 1 - unname(confusion_mat$byClass[2]) # 1-TNR = 1-Specificity
    acc <- unname(confusion_mat$overall[1])
    prec <- unname(confusion_mat$byClass[3])

    roc_50 <- roc(
      quantile_data$OUTBREAK_50_TRUE,
      quantile_data$OUTBREAK_50_PRED
    )
    auc_50 <- auc(roc_50)
    print(ci.auc(quantile_data$OUTBREAK_50_TRUE, quantile_data$OUTBREAK_50_PRED))
    # Define the combined score (e.g., variable1 - variable2, depending on your goals)
    combined_score <- true_pos_50 - fpr_50
    results_dt[which(cut_off == threshold), true_pos := true_pos_50]
    results_dt[which(cut_off == threshold), false_pos := fpr_50]
    results_dt[which(cut_off == threshold), diff_tpr_fpr := combined_score]
    results_dt[which(cut_off == threshold), accuracy := acc]
    results_dt[which(cut_off == threshold), precision := prec]
    results_dt[which(cut_off == threshold), auc := auc_50]
  }
  return(results_dt)
}


# ROC ----
# Plot all models' ROC curves in single plot
roc_plot_all_models_function <- function(results_dt,
                                         outbreak_threshold,
                                         onset = NULL) {
  # setkeyv(results_dt, "model_factor")
  # outbreak_threshold = 50 or 150
  roc_dt <- NULL
  for (i in 1:length(unique(results_dt$model))) {
    model_in_q <- c(unique(results_dt$model)[i])
    if (outbreak_threshold == 50) {
      tmp_dt <- subset(results_dt, model == model_in_q &
        quantile == OUTBREAK_50_THRESHOLD)
      tmp_dt[, OUTBREAK_PRED_50 := ifelse(prediction >= 50,
        1, 0
      )]
      tmp_dt[, OUTBREAK_TRUE_50 := ifelse(true_value >= 50,
        1, 0
      )]
      roc_50 <- with(tmp_dt, roc(OUTBREAK_TRUE_50, OUTBREAK_PRED_50))
      tmp_roc_dt <- data.table(sens = roc_50$sensitivities, inv_spec = 1 - roc_50$specificities)
      tmp_roc_dt[, model := rep(model_in_q, nrow(tmp_roc_dt))]
      print(tmp_roc_dt)
      print(roc_dt)
      roc_dt <- rbind(roc_dt, tmp_roc_dt)
    } else {
      tmp_dt <- subset(results_dt, model == model_in_q &
        quantile == OUTBREAK_150_THRESHOLD)
      tmp_dt[, OUTBREAK_PRED_150 := ifelse(prediction >= 150,
        1, 0
      )]
      tmp_dt[, OUTBREAK_TRUE_150 := ifelse(true_value >= 150,
        1, 0
      )]
      roc_150 <- with(tmp_dt, roc(OUTBREAK_TRUE_150, OUTBREAK_PRED_150))
      tmp_roc_dt <- data.table(sens = roc_150$sensitivities, inv_spec = 1 - roc_150$specificities)
      tmp_roc_dt[, model := rep(model_in_q, nrow(tmp_roc_dt))]
      print(tmp_roc_dt)
      print(roc_dt)
      roc_dt <- rbind(roc_dt, tmp_roc_dt)
    }
  }
  roc_dt[, model_factor := factor(model, levels = new_model_names)]
  setkeyv(roc_dt, "model_factor")
  roc_plot <- ggplot(roc_dt) +
    geom_line(aes(x = inv_spec, y = sens, color = model_factor),
      alpha = 0.45,
      linewidth = 0.85
    ) +
    theme_bw() +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    geom_abline(
      intercept = 0, slope = 1,
      linewidth = 1.2, color = "grey"
    ) +
    gghighlight(model_factor == "Median *",
      use_direct_label = FALSE,
      unhighlighted_params = list(colour = NULL, alpha = 0.35)
    ) +
    labs(x = "1 - Specificity (%)", y = "Sensitivity (%)") +
    theme(
      text = element_text(size = 30),
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 24),
      legend.text = element_text(size = 24),
      legend.position = "bottom",
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    ) +
    guides(color = guide_legend("Model", override.aes = list(linewidth = 1.25)))

  if (!is.null(onset)) {
    outbreak_threshold <- paste0("onset_", outbreak_threshold)
  }
  ggsave(roc_plot,
    filename = file.path(
      peru.province.ensemble.out.dir,
      paste0(outbreak_threshold, "roc_all_models_plot.pdf")
    ),
    h = 14, w = 18
  )
  return(roc_plot)
}

# Plot models' individual ROC curves in individual plots
roc_plot_function <- function(results_dt,
                              outbreak_threshold,
                              onset = NULL) {
  # outbreak_threshold = 50 or 150

  for (i in 1:length(unique(results_dt$model))) {
    model_in_q <- c(unique(results_dt$model)[i])
    if (outbreak_threshold == 50) {
      # baseline_dt <- subset(results_dt, model == "Baseline" & quantile == OUTBREAK_50_THRESHOLD)
      # baseline_dt[, OUTBREAK_PRED_50:= ifelse(prediction >= 50,
      #                                    1, 0)]
      # baseline_dt[, OUTBREAK_TRUE_50:= ifelse(true_value >= 50,
      #                                    1, 0)]
      # # print(baseline_dt)
      # baseline_confusion_mat <- confusionMatrix(data=as.factor(baseline_dt$OUTBREAK_PRED_50),
      #                                  reference = as.factor(baseline_dt$OUTBREAK_TRUE_50),
      #                                  positive = "1")
      # baseline_roc_50 <- roc(baseline_dt$OUTBREAK_TRUE_50,
      #                        baseline_dt$OUTBREAK_PRED_50)
      # baseline_auc_50 <- auc(baseline_roc_50)
      #
      tmp_dt <- subset(results_dt, model == model_in_q &
        quantile == OUTBREAK_50_THRESHOLD)
      tmp_dt[, OUTBREAK_PRED_50 := ifelse(prediction >= 50,
        1, 0
      )]
      tmp_dt[, OUTBREAK_TRUE_50 := ifelse(true_value >= 50,
        1, 0
      )]

      confusion_mat <- confusionMatrix(
        data = as.factor(tmp_dt$OUTBREAK_PRED_50),
        reference = as.factor(tmp_dt$OUTBREAK_TRUE_50),
        positive = "1"
      )
      roc_50 <- roc(
        tmp_dt$OUTBREAK_TRUE_50,
        tmp_dt$OUTBREAK_PRED_50
      )
      auc_50 <- auc(roc_50)
      print("Here?")
      tmp_roc_dt <- data.table(
        sens = roc_50$sensitivities, inv_spec = 1 - roc_50$specificities,
        model = model_in_q
      )
      roc_50_object <- ggplot(tmp_roc_dt) +
        geom_line(aes(x = inv_spec, y = sens), color = "mediumpurple4", linewidth = 1.5) +
        theme_bw() +
        labs(y = "Sensitivity", x = "1 - Specificity (%)") +
        theme_bw() +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        geom_abline(
          intercept = 0, slope = 1,
          # linetype = "dashed",
          linewidth = 1.2,
          color = "grey"
        ) +
        annotate("text",
          x = 0.65, y = 0.25, label = paste("AUC = ", c(round(roc_50$auc, 2))),
          size = 11
        ) +
        theme(
          text = element_text(size = 28),
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
          plot.subtitle = element_blank(),
          axis.title = element_text(size = 25),
          legend.text = element_text(size = 25),
          legend.position = "bottom", legend.key.size = unit(1.5, "cm"),
          plot.margin = unit(c(1, 1, 1, 1), "cm")
        )

      model_in_q <- toString(model_in_q)
      new_model_in_q <- gsub(" \\*", "", model_in_q)
      if (!is.null(onset)) {
        new_model_in_q <- paste0(new_model_in_q, "_onset")
      }

      ggsave(roc_50_object,
        filename = file.path(
          peru.province.ensemble.out.dir,
          paste0(new_model_in_q, "_outbreak_roc_50_plot.pdf")
        ),
        h = 14, w = 18
      )
      final_object <- roc_50_object
    } else {
      # baseline_dt <- subset(results_dt, model == "Baseline" & quantile == OUTBREAK_150_THRESHOLD)
      # baseline_dt[, OUTBREAK_PRED_150:= ifelse(prediction >= 150,
      #                                         1, 0)]
      # baseline_dt[, OUTBREAK_TRUE_150:= ifelse(true_value >= 150,
      #                                         1, 0)]
      # # print(baseline_dt)
      # baseline_confusion_mat <- confusionMatrix(data=as.factor(baseline_dt$OUTBREAK_PRED_150),
      #                                           reference = as.factor(baseline_dt$OUTBREAK_TRUE_150),
      #                                           positive = "1")
      # baseline_roc_150 <- roc(baseline_dt$OUTBREAK_TRUE_150,
      #                        baseline_dt$OUTBREAK_PRED_150)
      # baseline_auc_150 <- auc(baseline_roc_150)
      #
      tmp_dt <- subset(results_dt, model == model_in_q &
        quantile == OUTBREAK_150_THRESHOLD)
      tmp_dt[, OUTBREAK_PRED_150 := ifelse(prediction >= 150,
        1, 0
      )]
      tmp_dt[, OUTBREAK_TRUE_150 := ifelse(true_value >= 150,
        1, 0
      )]

      confusion_mat <- confusionMatrix(
        data = as.factor(tmp_dt$OUTBREAK_PRED_150),
        reference = as.factor(tmp_dt$OUTBREAK_TRUE_150),
        positive = "1"
      )
      roc_150 <- roc(
        tmp_dt$OUTBREAK_TRUE_150,
        tmp_dt$OUTBREAK_PRED_150
      )
      auc_150 <- auc(roc_150)
      print("Here?")
      tmp_roc_dt <- data.table(
        sens = roc_150$sensitivities, inv_spec = 1 - roc_150$specificities,
        model = model_in_q
      )
      roc_150_object <- ggplot(tmp_roc_dt) +
        geom_line(aes(x = inv_spec, y = sens), color = "mediumpurple4", linewidth = 1.5) +
        theme_bw() +
        labs(y = "Sensitivity", x = "1 - Specificity (%)") +
        theme_bw() +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        geom_abline(
          intercept = 0, slope = 1,
          # linetype = "dashed",
          linewidth = 1.2,
          color = "grey"
        ) +
        annotate("text",
          x = 0.65, y = 0.25, label = paste("AUC = ", c(round(roc_150$auc, 2))),
          size = 11
        ) +
        theme(
          text = element_text(size = 28),
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
          plot.subtitle = element_blank(),
          axis.title = element_text(size = 25),
          legend.text = element_text(size = 25),
          legend.position = "bottom", legend.key.size = unit(1.5, "cm"),
          plot.margin = unit(c(1, 1, 1, 1), "cm")
        )

      model_in_q <- toString(model_in_q)
      new_model_in_q <- gsub(" \\*", "", model_in_q)
      if (!is.null(onset)) {
        new_model_in_q <- paste0(new_model_in_q, "_onset")
      }

      ggsave(roc_150_object,
        filename = file.path(
          peru.province.ensemble.out.dir,
          paste0(new_model_in_q, "_outbreak_roc_150_plot.pdf")
        ),
        h = 14, w = 18
      )
      final_object <- roc_150_object
    }
  }
  return(final_object)
}


outbreak_classification_binary_plot <- function(results_dt,
                                                outbreak_threshold,
                                                onset = NULL) {
  # outbreak_threshold = 50 or 150
  for (i in 1:length(unique(results_dt$model))) {
    model_in_q <- c(unique(results_dt$model)[i])
    if (outbreak_threshold == 50) {
      tmp_dt <- subset(results_dt, model == model_in_q &
        quantile == OUTBREAK_50_THRESHOLD)
      tmp_dt[, OUTBREAK_PRED_50 := ifelse(prediction >= 50,
        1, 0
      )]
      tmp_dt[, OUTBREAK_TRUE_50 := ifelse(true_value >= 50,
        1, 0
      )]
      tmp_dt[, INDICATOR := ifelse(OUTBREAK_PRED_50 == OUTBREAK_TRUE_50,
        1, 0
      )]

      tmp_dt[, INDICATOR := factor(INDICATOR, labels = c("False", "True"))]
      setkeyv(tmp_dt, c("target_end_date"))
      tmp_dt[, IND := seq(1, nrow(tmp_dt))]
      tmp_interval_outbreak_50_results <-
        quantile_threshold_50_function(tmp_dt)
      tmp_interval_outbreak_50_results$auc <- round(tmp_interval_outbreak_50_results$auc, 2)
      tmp_interval_outbreak_50_results[, true_pos := label_percent(accuracy = 0.1)(true_pos)]
      # tmp_interval_outbreak_50_results$false_pos
      tmp_interval_outbreak_50_results[, Specificity := 1 - false_pos]
      tmp_interval_outbreak_50_results[, Specificity := label_percent(accuracy = 0.1)(Specificity)]

      # annotation <- data.frame(
      #   x = c(0.5,0.5, 0.5),
      #   y = c(300, 300),
      #   label = c("AUC = ", "label 2")
      # )
      tmp_plot_binary <-
        ggplot(tmp_dt, aes(x = IND, y = true_value)) +
        geom_col(aes(fill = INDICATOR),
          width = 0.7,
          alpha = 0.7
        ) +
        geom_hline(aes(yintercept = 50),
          linetype = "longdash", linewidth = 2.0,
          colour = "forest green"
        ) +
        # coord_flip()+
        scale_y_continuous(breaks = seq(0, 400, by = 50)) +
        scale_fill_discrete(name = "Classification") +
        theme_bw() +
        coord_cartesian(ylim = c(0, 400)) +
        # scale_fill_discrete(
        #   name = expression(widehat("DIR") >= "50")
        # )+theme_bw()+
        labs(x = "Outbreak Number", y = "DIR") +
        theme_bw() +
        # + annotate('text', x = 0, y = 0,
        #            label = "AUC==0.6 ",parse = TRUE,size=10)+
        annotate("text",
          x = 100, y = 330, label = paste("AUC = ", c(tmp_interval_outbreak_50_results$auc)),
          size = 11
        ) +
        annotate("text",
          x = 100, y = 275, label = paste("Sensitivity = ", c(tmp_interval_outbreak_50_results$true_pos)),
          size = 11
        ) +
        annotate("text",
          x = 100, y = 220, label = paste("Specificity = ", c(tmp_interval_outbreak_50_results$Specificity)),
          size = 11
        ) +
        # geom_label(x=8, y=0.005*10^4, label="First vaccinations",
        #            size=10, fontface = 3, color = "gray27")+
        # geom_label(x=12, y=0.00485*10^4, label="Full vaccination \n becoming \n prevalent",
        #            size=10, fontface = 3, color = "gray27")+
        theme(
          text = element_text(size = 28),
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
          plot.subtitle = element_blank(),
          axis.title = element_text(size = 25),
          legend.text = element_text(size = 25),
          legend.position = "bottom", legend.key.size = unit(1.5, "cm"),
          plot.margin = unit(c(1, 1, 1, 1), "cm")
        )
      model_in_q <- toString(model_in_q)
      new_model_in_q <- gsub(" \\*", "", model_in_q)
      if (!is.null(onset)) {
        new_model_in_q <- paste0(new_model_in_q, "_onset")
      }

      ggsave(tmp_plot_binary,
        filename = file.path(
          peru.province.ensemble.out.dir,
          paste0(new_model_in_q, "_outbreak_50_binary_plot.pdf")
        ),
        h = 14, w = 20
      )
    } else {
      tmp_dt <- subset(results_dt, model == model_in_q &
        quantile == OUTBREAK_150_THRESHOLD)
      tmp_dt[, OUTBREAK_PRED_150 := ifelse(prediction >= 150,
        1, 0
      )]
      tmp_dt[, OUTBREAK_TRUE_150 := ifelse(true_value >= 150,
        1, 0
      )]
      tmp_dt[, INDICATOR := ifelse(OUTBREAK_PRED_150 == OUTBREAK_TRUE_150,
        1, 0
      )]

      tmp_dt[, INDICATOR := factor(INDICATOR, labels = c("False", "True"))]
      setkeyv(tmp_dt, c("target_end_date"))
      tmp_dt[, IND := seq(1, nrow(tmp_dt))]
      tmp_interval_outbreak_150_results <-
        quantile_threshold_150_function(tmp_dt)
      tmp_interval_outbreak_150_results$auc <- round(tmp_interval_outbreak_150_results$auc, 2)
      tmp_interval_outbreak_150_results[, true_pos := label_percent(accuracy = 0.1)(true_pos)]
      # tmp_interval_outbreak_150_results$false_pos
      tmp_interval_outbreak_150_results[, Specificity := 1 - false_pos]
      tmp_interval_outbreak_150_results[, Specificity := label_percent(accuracy = 0.1)(Specificity)]

      # annotation <- data.frame(
      #   x = c(0.5,0.5, 0.5),
      #   y = c(300, 300),
      #   label = c("AUC = ", "label 2")
      # )
      tmp_plot_binary <-
        ggplot(tmp_dt, aes(x = IND, y = true_value)) +
        geom_col(aes(fill = INDICATOR),
          width = 0.7,
          alpha = 0.7
        ) +
        geom_hline(aes(yintercept = 150),
          linetype = "longdash", linewidth = 2.0,
          colour = "forest green"
        ) +
        # coord_flip()+
        scale_y_continuous(breaks = seq(0, 400, by = 50)) +
        scale_fill_discrete(name = "Classification") +
        theme_bw() +
        coord_cartesian(ylim = c(0, 400)) +

        # scale_fill_discrete(
        #   name = expression(widehat("DIR") >= "150")
        # )+theme_bw()+
        labs(x = "Outbreak Number", y = "DIR") +
        theme_bw() +
        # + annotate('text', x = 0, y = 0,
        #            label = "AUC==0.6 ",parse = TRUE,size=10)+
        annotate("text",
          x = 100, y = 330, label = paste("AUC = ", c(tmp_interval_outbreak_150_results$auc)),
          size = 11
        ) +
        annotate("text",
          x = 100, y = 275, label = paste("Sensitivity = ", c(tmp_interval_outbreak_150_results$true_pos)),
          size = 11
        ) +
        annotate("text",
          x = 100, y = 220, label = paste("Specificity = ", c(tmp_interval_outbreak_150_results$Specificity)),
          size = 11
        ) +
        # geom_label(x=8, y=0.005*10^4, label="First vaccinations",
        #            size=10, fontface = 3, color = "gray27")+
        # geom_label(x=12, y=0.00485*10^4, label="Full vaccination \n becoming \n prevalent",
        #            size=10, fontface = 3, color = "gray27")+
        theme(
          text = element_text(size = 28),
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
          plot.subtitle = element_blank(),
          axis.title = element_text(size = 25),
          legend.text = element_text(size = 25),
          legend.position = "bottom", legend.key.size = unit(1.5, "cm"),
          plot.margin = unit(c(1, 1, 1, 1), "cm")
        )
      model_in_q <- toString(model_in_q)
      new_model_in_q <- gsub(" \\*", "", model_in_q)
      if (!is.null(onset)) {
        new_model_in_q <- paste0(new_model_in_q, "_onset")
      }

      ggsave(tmp_plot_binary,
        filename = file.path(
          peru.province.ensemble.out.dir,
          paste0(new_model_in_q, "_outbreak_150_binary_plot.pdf")
        ),
        h = 14, w = 20
      )
    }
  }
  return(tmp_plot_binary)
}



# Binary classification plot function for outbreak detection over time
outbreak_classification_binary_plot <- function(results_dt,
                                                outbreak_threshold,
                                                onset = NULL) {
  # outbreak_threshold = 50 or 150
  for (i in 1:length(unique(results_dt$model))) {
    model_in_q <- c(unique(results_dt$model)[i])
    if (outbreak_threshold == 50) {
      tmp_dt <- subset(results_dt, model == model_in_q &
        quantile == OUTBREAK_50_THRESHOLD)
      tmp_dt[, OUTBREAK_PRED_50 := ifelse(prediction >= 50,
        1, 0
      )]
      tmp_dt[, OUTBREAK_TRUE_50 := ifelse(true_value >= 50,
        1, 0
      )]
      tmp_dt[, INDICATOR := ifelse(OUTBREAK_PRED_50 == OUTBREAK_TRUE_50,
        1, 0
      )]

      tmp_dt[, INDICATOR := factor(INDICATOR, labels = c("False", "True"))]
      setkeyv(tmp_dt, c("target_end_date"))
      tmp_dt[, IND := seq(1, nrow(tmp_dt))]
      tmp_interval_outbreak_50_results <-
        quantile_threshold_50_function(tmp_dt)
      tmp_interval_outbreak_50_results$auc <- round(tmp_interval_outbreak_50_results$auc, 2)
      tmp_interval_outbreak_50_results[, true_pos := label_percent(accuracy = 0.1)(true_pos)]
      # tmp_interval_outbreak_50_results$false_pos
      tmp_interval_outbreak_50_results[, Specificity := 1 - false_pos]
      tmp_interval_outbreak_50_results[, Specificity := label_percent(accuracy = 0.1)(Specificity)]

      # annotation <- data.frame(
      #   x = c(0.5,0.5, 0.5),
      #   y = c(300, 300),
      #   label = c("AUC = ", "label 2")
      # )
      tmp_plot_binary <-
        ggplot(
          tmp_dt,
          aes(x = true_value, y = OUTBREAK_PRED_50)
        ) +
        geom_point(aes(colour = INDICATOR),
          alpha = 0.5, size = 5.5
        ) +
        geom_vline(aes(xintercept = 50),
          linetype = "longdash", linewidth = 2.0,
          colour = "forest green"
        ) +
        # coord_flip()+
        scale_x_continuous(breaks = seq(0, 400, by = 50)) +
        scale_y_continuous(breaks = seq(0, 1, by = 1)) +
        scale_colour_discrete(name = "Classification") +
        theme_bw() +
        coord_cartesian(xlim = c(0, 400)) +
        # scale_colour_discrete(
        #   name = expression(widehat("DIR") >= "50")
        # )+theme_bw()+
        labs(y = "Outbreak ()", x = "DIR") +
        theme_bw() +
        # + annotate('text', x = 0, y = 0,
        #            label = "AUC==0.6 ",parse = TRUE,size=10)+
        annotate("text",
          x = 300, y = 0.75, label = paste("AUC = ", c(tmp_interval_outbreak_50_results$auc)),
          size = 11
        ) +
        annotate("text",
          x = 300, y = 0.5, label = paste("Sensitivity = ", c(tmp_interval_outbreak_50_results$true_pos)),
          size = 11
        ) +
        annotate("text",
          x = 300, y = 0.25, label = paste("Specificity = ", c(tmp_interval_outbreak_50_results$Specificity)),
          size = 11
        ) +
        # geom_label(x=8, y=0.005*10^4, label="First vaccinations",
        #            size=10, fontface = 3, color = "gray27")+
        # geom_label(x=12, y=0.00485*10^4, label="Full vaccination \n becoming \n prevalent",
        #            size=10, fontface = 3, color = "gray27")+
        theme(
          text = element_text(size = 28),
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
          plot.subtitle = element_blank(),
          axis.title = element_text(size = 25),
          legend.text = element_text(size = 25),
          legend.position = "bottom", legend.key.size = unit(1.5, "cm"),
          plot.margin = unit(c(1, 1, 1, 1), "cm")
        ) +
        guides(colour = guide_legend(override.aes = list(size = 6)))
      model_in_q <- toString(model_in_q)
      new_model_in_q <- gsub(" \\*", "", model_in_q)
      if (!is.null(onset)) {
        new_model_in_q <- paste0(new_model_in_q, "_onset")
      }

      ggsave(tmp_plot_binary,
        filename = file.path(
          peru.province.ensemble.out.dir,
          paste0(new_model_in_q, "_outbreak_50_binary_plot.pdf")
        ),
        h = 14, w = 20
      )
    } else {
      tmp_dt <- subset(results_dt, model == model_in_q &
        quantile == OUTBREAK_150_THRESHOLD)
      tmp_dt[, OUTBREAK_PRED_150 := ifelse(prediction >= 150,
        1, 0
      )]
      tmp_dt[, OUTBREAK_TRUE_150 := ifelse(true_value >= 150,
        1, 0
      )]
      tmp_dt[, INDICATOR := ifelse(OUTBREAK_PRED_150 == OUTBREAK_TRUE_150,
        1, 0
      )]

      tmp_dt[, INDICATOR := factor(INDICATOR, labels = c("False", "True"))]
      setkeyv(tmp_dt, c("target_end_date"))
      tmp_dt[, IND := seq(1, nrow(tmp_dt))]
      tmp_interval_outbreak_150_results <-
        quantile_threshold_150_function(tmp_dt)
      tmp_interval_outbreak_150_results$auc <- round(tmp_interval_outbreak_150_results$auc, 2)
      tmp_interval_outbreak_150_results[, true_pos := label_percent(accuracy = 0.1)(true_pos)]
      # tmp_interval_outbreak_150_results$false_pos
      tmp_interval_outbreak_150_results[, Specificity := 1 - false_pos]
      tmp_interval_outbreak_150_results[, Specificity := label_percent(accuracy = 0.1)(Specificity)]

      # annotation <- data.frame(
      #   x = c(0.5,0.5, 0.5),
      #   y = c(300, 300),
      #   label = c("AUC = ", "label 2")
      # )
      tmp_plot_binary <-
        ggplot(tmp_dt, aes(x = IND, y = true_value)) +
        geom_col(aes(fill = INDICATOR),
          width = 0.7,
          alpha = 0.7
        ) +
        geom_hline(aes(yintercept = 150),
          linetype = "longdash", linewidth = 2.0,
          colour = "forest green"
        ) +
        # coord_flip()+
        scale_y_continuous(breaks = seq(0, 400, by = 50)) +
        scale_fill_discrete(name = "Classification") +
        theme_bw() +
        coord_cartesian(ylim = c(0, 400)) +

        # scale_fill_discrete(
        #   name = expression(widehat("DIR") >= "150")
        # )+theme_bw()+
        labs(x = "Outbreak Number", y = "DIR") +
        theme_bw() +
        # + annotate('text', x = 0, y = 0,
        #            label = "AUC==0.6 ",parse = TRUE,size=10)+
        annotate("text",
          x = 100, y = 300, label = paste("AUC = ", c(tmp_interval_outbreak_150_results$auc)),
          size = 11
        ) +
        annotate("text",
          x = 100, y = 300, label = paste("Sensitivity = ", c(tmp_interval_outbreak_150_results$true_pos)),
          size = 11
        ) +
        annotate("text",
          x = 100, y = 300, label = paste("Specificity = ", c(tmp_interval_outbreak_150_results$Specificity)),
          size = 11
        ) +
        # geom_label(x=8, y=0.005*10^4, label="First vaccinations",
        #            size=10, fontface = 3, color = "gray27")+
        # geom_label(x=12, y=0.00485*10^4, label="Full vaccination \n becoming \n prevalent",
        #            size=10, fontface = 3, color = "gray27")+
        theme(
          text = element_text(size = 28),
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
          plot.subtitle = element_blank(),
          axis.title = element_text(size = 25),
          legend.text = element_text(size = 25),
          legend.position = "bottom", legend.key.size = unit(1.5, "cm"),
          plot.margin = unit(c(1, 1, 1, 1), "cm")
        )
      model_in_q <- toString(model_in_q)
      new_model_in_q <- gsub(" \\*", "", model_in_q)
      if (!is.null(onset)) {
        new_model_in_q <- paste0(new_model_in_q, "_onset")
      }

      ggsave(tmp_plot_binary,
        filename = file.path(
          peru.province.ensemble.out.dir,
          paste0(new_model_in_q, "_outbreak_150_binary_plot.pdf")
        ),
        h = 14, w = 20
      )
    }
  }
  return(tmp_plot_binary)
}


# Summarise Predictions ----
summary_predictions <- function(model_results) {
  tmp_lower <- unique(subset(model_results, quantile == 0.025))
  tmp_lower_dt <- data.table(CI_L = tmp_lower$prediction)
  tmp_upper <- unique(subset(model_results, quantile == 0.975))
  tmp_upper_dt <- data.table(CI_U = tmp_upper$prediction)
  tmp_median <- unique(subset(model_results, quantile == 0.5))
  pred_dt <- cbind(tmp_lower_dt, tmp_upper_dt, tmp_median)
  return(pred_dt)
}

process_summary_predictions <- function(summary_dt) {
  coverage_dt <- summary_dt[, list(coverage = length(which(true_value >= CI_L & true_value <= CI_U)) / length(true_value)),
    by = "model"
  ]
}
# toMatch <- c("log_cases", "dir")
# matches <- unique (grep(paste(toMatch,collapse="|"),
#                         "2010_2017_log_cases", value=TRUE))
summary_score_table_function <- function(model_results) {
  model_results_score_summary <- model_results %>%
    score() %>%
    summarise_scores(by = c("model"))
  tmp <- process_quantile_predictions(model_results)

  model_results_score_summary <- merge(model_results_score_summary, tmp,
    by = "model"
  )
  pred_dt <- summary_predictions(model_results)
  tmp <- process_summary_predictions(pred_dt)
  model_results_score_summary <- merge(model_results_score_summary, tmp,
    by = "model"
  )

  return(model_results_score_summary)
}


# Main function for processing forecasting results ----
visualise_and_summarise_quantile_results <- function(model_results,
                                                     outbreak_results = NULL,
                                                     time_period_quantity,
                                                     plot_directory) {
  # Function to visualise and summarise results from quantile forecasts
  # For historical, only show best untrained -> Proceed with these in
  # testing period
  # Trained approaches also
  # time_period_quantity = string for saving plot consisting of
  # i) time period + ii) quantity assessed (log cases or DIR)
  quantity <- str_sub(time_period_quantity, 11)
  pred_dt <- summary_predictions(model_results)
  results_dt <- model_results[which(quantile == 0.5),
    list(
      r2 = caret::R2(prediction, true_value),
      mae = caret::MAE(prediction, true_value)
    ),
    by = c("model", "location")
  ] # R^2 for plot
  setkeyv(results_dt, "location")
  results_dt[, LAT_PROV_IND_FACTOR := factor(location,
    labels = unique(location)
  )]

  scored_results <- model_results %>%
    score() %>%
    summarise_scores(by = c("model"))

  wis_decomposition_plot <- scored_results %>%
    plot_wis() +
    ylab("Model") +
    theme(
      text = element_text(size = 30),
      axis.text.x = element_text(size = 30),
      axis.text.y = element_text(size = 30),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 30),
      legend.text = element_text(size = 30) + geom_text(size = 30),
      legend.position = "bottom"
    )



  pred_dt[, target_end_date := as.Date(target_end_date)]
  setnames(pred_dt, "prediction", "MEDIAN")
  pred_dt[, LAT_PROV_IND_FACTOR := factor(LAT_PROV_IND,
    labels = unique(location)
  )]
  setkeyv(pred_dt, "LAT_PROV_IND_FACTOR")
  tmp <-
    unique(subset(pred_dt,
      select = c("location", "LAT_PROV_IND")
    ))
  setkeyv(tmp, "LAT_PROV_IND")
  levels(pred_dt$LAT_PROV_IND_FACTOR) <-
    tmp$location

  scored_results_by_model_and_date <- model_results %>%
    score() %>%
    summarise_scores(by = c("model", "target_end_date"))
  tmp_scored_results_by_model_and_date <- copy(scored_results_by_model_and_date)
  tmp_scored_results_by_model_and_date[, Month_Yr := format(as.Date(target_end_date), "%Y-%m")]
  tmp_scored_results_by_model_and_date[, target_end_date := Month_Yr]


  wis_decomposition_facet_plot_over_time <-
    tmp_scored_results_by_model_and_date %>% plot_wis(x = "target_end_date") +
    coord_flip() +
    ylab("Date") +
    facet_wrap(model ~ ., )
  theme(
    text = element_text(size = 28),
    panel.spacing.x = unit(6, "lines"),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(
      size = 20,
      angle = 90
    ),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 28, hjust = 0.5, face = "bold"),
    plot.subtitle = element_blank(),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 25),
    legend.position = "bottom", legend.key.size = unit(1.5, "cm"),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )
  ggsave(wis_decomposition_facet_plot_over_time,
    filename = file.path(
      plot_directory,
      paste0("facet_plot_", time_period_quantity, "_wis_decomposition_plot_over_time.pdf")
    ),
    h = 20, w = 32
  )
  list_pred_plots <- list()
  list_wis_decomposition_plot_over_time <- list()
  for (i in 1:length(unique(pred_dt$model))) {
    model_in_q <- c(unique(pred_dt$model)[i])
    tmp_pred_dt <- subset(pred_dt, model == model_in_q)
    tmp_results_dt <- subset(results_dt, model == model_in_q)
    dat_text <- data.table(
      model_factor = tmp_results_dt$model,
      LAT_PROV_IND_FACTOR = tmp_results_dt$LAT_PROV_IND_FACTOR,
      label = round(tmp_results_dt$r2, 2)
    )
    dat_text$r2 <- sprintf("italic(R^2) == %.2f", dat_text$label)


    tmp <- palette("Set2")
    if (quantity == "log_cases") {
      y_text <- "Log(Cases + 1)"
    } else {
      y_text <- "DIR (per 100,000)"
    }
    tmp_pred_plot <- ggplot(tmp_pred_dt) +
      geom_point(aes(x = target_end_date, y = true_value, col = "Observed")) +
      geom_line(aes(x = target_end_date, y = MEDIAN, col = "Estimated")) +
      geom_point(aes(x = target_end_date, y = MEDIAN, col = "Estimated")) +
      geom_ribbon(aes(x = target_end_date, ymin = CI_L, ymax = CI_U),
        alpha = 0.18, fill = tmp[1]
      ) +
      facet_wrap(fct_rev(LAT_PROV_IND_FACTOR) ~ ., scales = "free_y", nrow = 5) +
      # geom_text(data =  dat_text,
      #           aes(x = max(tmp_pred_dt$target_end_date),
      #               y = max(tmp_pred_dt$DIR), label= r2, group=TRUE), size = 9,
      #           hjust   = 1.05,
      #           vjust   = 1.5,
      #           color = "black", parse = T)+

      theme_bw() +
      guides(colour = guide_legend(override.aes = list(size = 6))) +
      xlab("Time") +
      ylab(y_text) +
      labs(title = model_in_q) +
      theme(legend.position = "bottom") +
      theme(
        text = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = "bottom", legend.key.size = unit(1.5, "cm")
      )
    tmp_pred_plot <- set_palette(tmp_pred_plot, "Set2")
    model_in_q <- toString(model_in_q)
    new_model_in_q <- gsub(" \\*", "", model_in_q)
    ggsave(tmp_pred_plot,
      filename = file.path(
        plot_directory,
        paste0(new_model_in_q, "_", time_period_quantity, "_summary_preds.pdf")
      ),
      h = 16, w = 24
    )

    tmp_scored_results_by_model_and_date <-
      subset(scored_results_by_model_and_date, model == model_in_q)
    tmp_scored_results_by_model_and_date[, Month_Yr := format(as.Date(target_end_date), "%Y-%m")]
    tmp_scored_results_by_model_and_date[, target_end_date := Month_Yr]
    wis_decomposition_plot_over_time <-
      tmp_scored_results_by_model_and_date %>% plot_wis(x = "target_end_date") +
      coord_flip() +
      ylab("Date") + labs(title = model_in_q) +
      theme(
        text = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(
          size = 20,
          angle = 90
        ),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 28, hjust = 0.5, face = "bold"),
        plot.subtitle = element_blank(),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.position = "bottom", legend.key.size = unit(1.5, "cm")
      )
    ggsave(wis_decomposition_plot_over_time,
      filename = file.path(
        plot_directory,
        paste0(new_model_in_q, "_", time_period_quantity, "wis_decomposition_plot_over_time.pdf")
      ),
      h = 14, w = 30
    )
    list_pred_plots[[i]] <- tmp_pred_plot
    list_wis_decomposition_plot_over_time[[i]] <- wis_decomposition_plot_over_time
  }



  # Table
  summary_score_table <- summary_score_table_function(model_results)

  if (!is.null(outbreak_results)) {
    outbreak_results <- subset(outbreak_results,
      select = c("true_pos", "false_pos", "auc", "model")
    )
    summary_score_table <- merge(summary_score_table,
      outbreak_results,
      by = "model"
    )
    summary_score_table <-
      subset(summary_score_table,
        select = c(
          "model", "interval_score", "bias", "coverage", "true_pos",
          "false_pos", "auc", "ae_median", "r2"
        )
      )
    summary_score_table <-
      summary_score_table[order(interval_score, decreasing = FALSE), ]
    setnames(
      summary_score_table,
      c(
        "Model", "WIS", "Bias", "PICoverage",
        "Sensitivity", "FP", "AUC", "MAE",
        "R2"
      )
    )
    # summary_score_table[, Sensitivity:= round(Sensitivity, 2)]
    # summary_score_table[, Specificity:= round(1-FP, 2)]
    summary_score_table[, Sensitivity := label_percent(accuracy = 0.1)(Sensitivity)]
    summary_score_table[, Specificity := 1 - FP]
    summary_score_table[, Specificity := label_percent(accuracy = 0.1)(Specificity)]

    summary_score_table[, FP := NULL]
    # summary_score_table[, TN:= round(TN, 2)]
    # summary_score_table[, FN:= round(FN, 2)]
    summary_score_table[, AUC := round(AUC, 2)]
    summary_score_table[order(AUC, decreasing = TRUE)]
    summary_score_table[, PICoverage := label_percent(accuracy = 0.1)(PICoverage)]
    # summary_score_table[, QuantDev:= label_percent(accuracy = 0.1)(QuantDev)]
    # summary_score_table[, `Coverage Deviation`:= NULL]

    setcolorder(
      summary_score_table,
      c(
        "Model", "WIS", "Bias", "PICoverage", "R2",
        "MAE", "Sensitivity", "Specificity", "AUC"
      )
    )
  } else {
    summary_score_table <-
      subset(summary_score_table,
        select = c("model", "interval_score", "bias", "coverage", "r2", "ae_median")
      )
    summary_score_table <-
      summary_score_table[order(interval_score, decreasing = FALSE), ]
    setnames(
      summary_score_table,
      c(
        "Model", "WIS", "Bias", "PICoverage",
        "R2", "MAE"
      )
    )
    summary_score_table[, PICoverage := label_percent(accuracy = 0.1)(PICoverage)]
    # summary_score_table[, QuantDev:= label_percent(accuracy = 0.1)(QuantDev)]

    setcolorder(
      summary_score_table,
      c(
        "Model", "WIS", "Bias", "PICoverage", "R2",
        "MAE"
      )
    )
  }

  return(list(
    list_pred_plots, summary_score_table,
    wis_decomposition_plot, list_wis_decomposition_plot_over_time
  ))
}






# COVERAGE FUNCTIONS ----
# Quantile coverage
quantile_coverage_function <- function(data,
                                       results_string,
                                       historical = NULL) {
  # 1) Single Plot
  quantile_coverage_single_plot <-
    data %>%
    score() %>%
    summarise_scores(by = c("model", "quantile")) %>%
    plot_quantile_coverage() + geom_line(aes(y = quantile_coverage), linewidth = 1.5, alpha = 0.6) +
    theme(
      text = element_text(size = 24),
      axis.text.x = element_text(size = 24),
      axis.text.y = element_text(size = 24),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 24),
      legend.text = element_text(size = 24),
      legend.position = "bottom",
      plot.margin = unit(c(.2, .5, .2, .5), "cm")
    ) +
    guides(color = guide_legend("Model"))

  ggsave(quantile_coverage_single_plot,
    file = file.path(
      peru.province.ensemble.out.dir,
      paste0(results_string, "_quantile_coverage_single_plot.pdf")
    ),
    h = 14, w = 18
  )



  # 2) facet Plot
  quantile_coverage_facet_plot <-
    data %>%
    score() %>%
    summarise_scores(by = c("model", "quantile")) %>%
    plot_quantile_coverage() + geom_line(aes(y = quantile_coverage), linewidth = 1.5, alpha = 0.6) +
    facet_wrap(model ~ ., ) + theme(panel.spacing.x = unit(4, "lines")) +
    theme(
      text = element_text(size = 30),
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 24),
      legend.text = element_text(size = 24),
      legend.position = "bottom",
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    ) +
    guides(color = guide_legend("Model"))

  if (historical == TRUE) {
    quantile_coverage_facet_plot <- quantile_coverage_facet_plot + scale_color_manual(values = historical_model_colours)
  }
  ggsave(quantile_coverage_facet_plot,
    file = file.path(
      peru.province.ensemble.out.dir,
      paste0(results_string, "_quantile_coverage_facet_plot.pdf")
    ),
    h = 20, w = 30
  )
  return(quantile_coverage_facet_plot)
}
# Interval Coverage
interval_coverage_function <- function(data,
                                       results_string,
                                       historical = NULL) {
  # 1) Single Plot
  interval_coverage_single_plot <-
    data %>%
    score() %>%
    summarise_scores(by = c("model", "range")) %>%
    plot_interval_coverage() +
    geom_line(aes(y = coverage * 100),
      linewidth = 1.6,
      alpha = 0.6
    ) +
    labs(x = "Nominal interval coverage (%)") +
    theme(
      text = element_text(size = 24),
      axis.text.x = element_text(size = 24),
      axis.text.y = element_text(size = 24),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 24),
      legend.text = element_text(size = 24),
      legend.position = "bottom",
      plot.margin = unit(c(.2, .5, .2, .5), "cm")
    ) +
    guides(color = guide_legend("Model"))

  ggsave(interval_coverage_single_plot,
    file = file.path(
      peru.province.ensemble.out.dir,
      paste0(results_string, "_interval_coverage_single_plot.pdf")
    ),
    h = 14, w = 18
  )



  # 2) facet Plot
  interval_coverage_facet_plot <-
    data %>%
    score() %>%
    summarise_scores(by = c("model", "range")) %>%
    plot_interval_coverage() +
    geom_line(aes(y = coverage * 100),
      linewidth = 1.6,
      alpha = 0.6
    ) +
    facet_wrap(model ~ ., ) + theme(panel.spacing.x = unit(4, "lines")) +
    labs(x = "Nominal interval coverage (%)") +
    theme(
      text = element_text(size = 30),
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 24),
      legend.text = element_text(size = 24),
      legend.position = "bottom",
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    ) +
    guides(color = guide_legend("Model"))


  if (historical == TRUE) {
    interval_coverage_facet_plot <- interval_coverage_facet_plot + scale_color_manual(values = historical_model_colours)
  }


  interval_coverage_dt <-
    data %>%
    score() %>%
    summarise_scores(by = c("model"))
  interval_coverage_dt
  interval_coverage_dt <-
    subset(interval_coverage_dt,
      select = c("model", "interval_score", "coverage_deviation")
    )
  interval_coverage_dt <-
    interval_coverage_dt[order(interval_score, decreasing = FALSE), ]
  setnames(
    interval_coverage_dt,
    c("Model", "WIS", "Coverage Deviation")
  )
  interval_coverage_dt[, `Coverage Deviation` := label_percent(accuracy = 0.1)(`Coverage Deviation`)]

  ggsave(interval_coverage_facet_plot,
    file = file.path(
      peru.province.ensemble.out.dir,
      paste0(results_string, "_interval_coverage_facet_plot.pdf")
    ),
    h = 20, w = 30
  )
  return(interval_coverage_facet_plot)
}

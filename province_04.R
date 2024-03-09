#Leave-one-time-point-out cross-validation script 
default_pois_formula <- CASES ~ 1 +   f(MONTH, replicate = PROV_IND, model = "rw1", cyclic = TRUE,
                                        constr = TRUE, scale.model = TRUE, hyper = prior.prec) +
  f(YEAR, replicate = PROV_IND, model = "iid") + f(PROV_IND,
                                                   model = "bym2", hyper = prior.prec, scale.model = TRUE, graph = file.path(peru.province.inla.data.in.dir,
                                                                                                                             "nbr_piura_tumbes_lambayeque.graph")) +
  SQ_RSI_DIR_LAG+SEASON+
  ns(MODIFIED_DIFF_WITH_HISTORICAL_DIR_LAG, df = 4) +
  tmin_roll_2_basis + prec_roll_2_basis + icen_basis + spi_basis


#RUN CV (2010-2017)-----
gc()
------------------------------------
for(i in 1:(nrow(training_ptl_province_df)/14 - 1)){
    print(i)
    idx.pred <- seq(i*14+1, i*14+14) #14 provinces per time step
    s <- 5000
    cv_dt <- data.table(training_ptl_province_df)
    setkeyv(cv_dt, c("TIME", "PROVINCE"))
    cv_dt[, IND:= seq(1, nrow(cv_dt))]
    true_cases <- c(cv_dt[which(TIME == i + 1), ]$CASES)
    true_dirs <- c(cv_dt[which(TIME == i + 1), ]$DIR)
    pop_offsets <- c(cv_dt[which(TIME == i + 1), ]$POP_OFFSET)
    
    cv_dt[which(TIME == i + 1), CASES:= NA]
    setkeyv(cv_dt, c("TIME", "PROVINCE"))
    
    tmp_climate_cv_fit <- run_province_model_func(data = cv_dt, default_pois_formula)
    print(paste0("Fit ", i , " successful"))
    
    xx <- inla.posterior.sample(s, tmp_climate_cv_fit)
    
    #xx.s = 14 entries (by province) and 5000 columns (by sample)
    xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor[idx.pred]), xx)
    
    y.pred <- matrix(NA, 14, s)
    error.pred <- matrix(NA, 14, s)
    abs_error.pred <- matrix(NA, 14, s)
    dir.pred <- matrix(NA, 14, s)
    dir_error.pred <- matrix(NA, 14, s)
    dir_abs_error.pred <- matrix(NA, 14, s)

    
    for(s.idx in 1:s) {
      xx.sample <- xx.s[, s.idx]
      y.pred[, s.idx] <- rzipois(14, lambda = exp(xx.sample[-1]), pstr0 = xx.sample[1])
      
      error.pred[, s.idx] <- true_cases - y.pred[, s.idx]
      abs_error.pred[, s.idx] <- abs(true_cases - y.pred[, s.idx])
      
      dir.pred[, s.idx] <- y.pred[, s.idx]/pop_offsets
      dir_error.pred[, s.idx] <- true_dirs - dir.pred[, s.idx]
      dir_abs_error.pred[, s.idx] <- abs(true_dirs - dir.pred[, s.idx])

    }

    outbreak_prob_100 <- rep(0, 14)
    for(j in 1:nrow(dir.pred)){
      outbreak_prob_100[j] <-  length(which(dir.pred[j,] >= 100))/s
    }
    
    outbreak_prob_50 <- rep(0, 14)
    for(j in 1:nrow(dir.pred)){
      outbreak_prob_50[j] <-  length(which(dir.pred[j,] >= 50))/s
    }
    
    outbreak_prob_150 <- rep(0, 14)
    for(j in 1:nrow(dir.pred)){
      outbreak_prob_150[j] <-  length(which(dir.pred[j,] >= 150))/s
    }
    
    outbreak_prob_200 <- rep(0, 14)
    for(j in 1:nrow(dir.pred)){
      outbreak_prob_200[j] <-  length(which(dir.pred[j,] >= 200))/s
    }
    outbreak_prob_250 <- rep(0, 14)
    for(j in 1:nrow(dir.pred)){
      outbreak_prob_250[j] <-  length(which(dir.pred[j,] >= 250))/s
    }

    preds_dt <- data.table(TIME = rep(i + 1, each = 14),
                           CI_L = apply(y.pred, 1, quantile, probs = c(0.025)),
                           CI_U = apply(y.pred, 1, quantile, probs = c(0.975)),
                           MEDIAN = apply(y.pred, 1, quantile ,probs = c(0.5)),
                           MEAN = apply(y.pred, 1, mean),
                           
                           ERROR_CI_L = apply(error.pred, 1, quantile, probs = c(0.025)),
                           ERROR_CI_U = apply(error.pred, 1, quantile, probs = c(0.975)),
                           ERROR_MEDIAN = apply(error.pred, 1, quantile ,probs = c(0.5)),
                           ERROR_MEAN = apply(error.pred, 1, mean),
                           ABS_ERROR_CI_L = apply(abs_error.pred, 1, quantile, probs = c(0.025)),
                           ABS_ERROR_CI_U = apply(abs_error.pred, 1, quantile, probs = c(0.975)),
                           ABS_ERROR_MEDIAN = apply(abs_error.pred, 1, quantile ,probs = c(0.5)),
                           ABS_ERROR_MEAN = apply(abs_error.pred, 1, mean),
                           
                           DIR_CI_L = apply(dir.pred, 1, quantile, probs = c(0.025)),
                           DIR_CI_U = apply(dir.pred, 1, quantile, probs = c(0.975)),
                           DIR_MEDIAN = apply(dir.pred, 1, quantile ,probs = c(0.5)),
                           DIR_MEAN = apply(dir.pred, 1, mean),
                           
                           OUTBREAK_PROB_100 = outbreak_prob_100,
                           OUTBREAK_PROB_50 = outbreak_prob_50,
                           OUTBREAK_PROB_150 = outbreak_prob_150,
                           OUTBREAK_PROB_200 = outbreak_prob_200,
                           OUTBREAK_PROB_250 = outbreak_prob_250,

                           
                           DIR_ERROR_CI_L = apply(dir_error.pred, 1, quantile, probs = c(0.025)),
                           DIR_ERROR_CI_U = apply(dir_error.pred, 1, quantile, probs = c(0.975)),
                           DIR_ERROR_MEDIAN = apply(dir_error.pred, 1, quantile ,probs = c(0.5)),
                           DIR_ERROR_MEAN = apply(dir_error.pred, 1, mean),
                           DIR_ABS_ERROR_CI_L = apply(dir_abs_error.pred, 1, quantile, probs = c(0.025)),
                           DIR_ABS_ERROR_CI_U = apply(dir_abs_error.pred, 1, quantile, probs = c(0.975)),
                           DIR_ABS_ERROR_MEDIAN = apply(dir_abs_error.pred, 1, quantile ,probs = c(0.5)),
                           DIR_ABS_ERROR_MEAN = apply(dir_abs_error.pred, 1, mean))
    if(i %%5 == 0){gc()}
    saveRDS(preds_dt, file = file.path(peru.province.inla.data.out.dir, paste0("training_ptl_loocv_preds", i, ".RDS")))
}


ovr_preds_dt <- NULL
for(i in 1:(nrow(training_ptl_province_df)/14 - 1)){
  preds_dt <- readRDS(file = file.path(peru.province.inla.data.out.dir, paste0("training_ptl_loocv_preds", i, ".RDS")))
  ovr_preds_dt <- rbind(ovr_preds_dt, preds_dt)
}


plot_dir_cv_posterior_preds_province <- function(cv_data, data){
  # Function allows for several plots of posterior predictions and
    # also returns a data.table for analysis of posterior predictions
  #Note: This is at a DIR level
  setkeyv(data, c("TIME", "PROVINCE"))
  cv_post_preds <- data.table(MEDIAN = (cv_data$DIR_MEDIAN),
                              MEAN = (cv_data$DIR_MEAN),
                              q2.5 = (cv_data$DIR_CI_L),
                              q97.5 = (cv_data$DIR_CI_U),
                              OUTBREAK_PROB = cv_data$OUTBREAK_PROB,
                              OUTBREAK_PROB_50 = cv_data$OUTBREAK_PROB_50,
                              OUTBREAK_PROB_150 = cv_data$OUTBREAK_PROB_150,
                              OUTBREAK_PROB_200 = cv_data$OUTBREAK_PROB_200,
                              OUTBREAK_PROB_250 = cv_data$OUTBREAK_PROB_250,
                              UPPER_QUANTILE_THRESHOLD_PROB  = cv_data$UPPER_QUANTILE_THRESHOLD_PROB,
                              ERROR_MEDIAN = (cv_data$DIR_ERROR_MEDIAN),
                              ERROR_MEAN = (cv_data$DIR_ERROR_MEAN),
                              ERROR_q2.5 = (cv_data$DIR_ERROR_CI_L),
                              ERROR_q97.5 = (cv_data$DIR_ERROR_CI_U),
                              ABS_ERROR_MEDIAN = (cv_data$DIR_ABS_ERROR_MEDIAN),
                              ABS_ERROR_MEAN = (cv_data$DIR_ABS_ERROR_MEAN),
                              ABS_ERROR_q2.5 = (cv_data$DIR_ABS_ERROR_CI_L),
                              ABS_ERROR_q97.5 = (cv_data$DIR_ABS_ERROR_CI_U),
                              DIR = data$DIR,
                              CASES = data$CASES,
                              PROVINCE = data$PROVINCE,

                              TIME = cv_data$TIME,
                              MONTH = data$MONTH,
                              YEAR = data$YEAR,
                              POP = data$POP,
                              IND = seq(1, nrow(data))
  )
  posterior_pred_plot <- ggplot(cv_post_preds)+
    geom_line(aes(x = IND, y = DIR, col = "Observed"))+ 
    geom_ribbon(aes(x = IND, ymin = q2.5, ymax = q97.5), alpha = 0.2)+
    geom_line(aes(x = IND, y = MEDIAN, col = "Estimated"))+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylim(c(0,1000))

  posterior_pred_plot2 <- ggplot(cv_post_preds)+
    geom_line(aes(x = MONTH, y = DIR, col = "Observed"))+ 
    geom_ribbon(aes(x = MONTH, ymin = q2.5, ymax = q97.5), alpha = 0.2)+
    geom_line(aes(x = MONTH, y = MEDIAN, col = "Estimated"))+
    theme_bw()+
    facet_wrap(PROVINCE ~ YEAR, scales = "free_y",
               ncol= 12)+
    theme(legend.position = "bottom")+
    coord_cartesian(xlim = c(1, 12))+
    scale_x_continuous(breaks=seq(1,12, by = 2))+
    theme(text = element_text(size = 25),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
          plot.subtitle = element_blank(),
          axis.title=element_text(size=25), 
          legend.text=element_text(size=20)+geom_text(size = 20),
          legend.position = "bottom",
          legend.title = element_blank())
  
  posterior_pred_plot3 <- ggplot(cv_post_preds)+
    geom_line(aes(x = MONTH, y = DIR, col = "Observed"))+ 
    # geom_ribbon(aes(x = MONTH, ymin = q2.5, ymax = q97.5), alpha = 0.2)+
    geom_line(aes(x = MONTH, y = MEDIAN, col = "Estimated"))+
    theme_bw()+
    facet_wrap(PROVINCE ~ YEAR, scales = "free_y", ncol= 12 )+
    theme(legend.position = "bottom")+
    coord_cartesian(xlim = c(1, 12))+
    scale_x_continuous(breaks=seq(1,12))+
    theme(text = element_text(size = 25),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
          plot.subtitle = element_blank(),
          axis.title=element_text(size=25), 
          legend.text=element_text(size=20)+geom_text(size = 20),
          legend.position = "bottom",
          legend.title = element_blank())
  posterior_pred_plot_errorbars <- ggplot(cv_post_preds)+
    geom_point(aes(x = MONTH, y = DIR, col = "Observed"))+ 
    geom_point(aes(x = MONTH, y = MEDIAN, col = "Estimated"))+ 
    geom_errorbar(aes(x = MONTH, ymin = q2.5, ymax = q97.5, col = "Estimated"), alpha = 0.6)+
    theme_bw()+
    facet_wrap(PROVINCE ~ YEAR, scales = "free_y", ncol= 12 )+
    theme(legend.position = "bottom")+
    coord_cartesian(xlim = c(1, 12))+
    scale_x_continuous(breaks=seq(1,12))+
    theme(text = element_text(size = 25),
          axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title=element_text(size=25), 
          legend.text=element_text(size=25)+geom_text(size = 25),
          legend.position = "bottom",
          legend.title = element_blank())
  tmp_xy_plot <- ggplot(cv_post_preds, aes(x = MEDIAN, y = DIR))+ 
    geom_point(alpha = 0.3, size = 3)+
    geom_abline(intercept = 0, slope = 1)+
    theme_bw()+
    labs(x = "Estimated DIR", y = "Observed DIR")+
    theme(text = element_text(size = 25),
          axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title=element_text(size=25), 
          legend.text=element_text(size=25)+geom_text(size = 25),
          legend.position = "bottom",
          legend.title = element_blank())+
    coord_cartesian(xlim=c(0, 1000), ylim=c(0, 1000))
  coloured_xy_plot <- ggplot(cv_post_preds, aes(x = MEDIAN, y = DIR, colour = PROVINCE))+ 
    geom_point(alpha = 0.4, size = 8)+
    geom_abline(intercept = 0, slope = 1)+
    theme_bw()+
    labs(x = "Estimated DIR", y = "Observed DIR")+
    theme(text = element_text(size = 25),
          axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title=element_text(size=25), 
          legend.text=element_text(size=25)+geom_text(size = 25),
          legend.position = "bottom",
          legend.title = element_blank())+
    coord_cartesian(xlim=c(0, 1000), ylim=c(0, 1000))
  
  
  table_errors_by_PROVINCE <- cv_post_preds[, median(ERROR_MEDIAN), by = "PROVINCE"]
  
  
  posterior_pred_plot_of_errors <- ggplot(cv_post_preds)+
    # geom_ribbon(aes(x = MONTH, ymin = q2.5, ymax = q97.5), alpha = 0.2)+
    geom_line(aes(x = TIME, y = ABS_ERROR_MEDIAN))+
    theme_bw()+
    facet_wrap(PROVINCE ~ . , scales = "free_y", nrow = 3 )+
    theme(legend.position = "bottom")+
    xlab("Month")+ylab("Median Absolute Error")+
    scale_x_continuous(breaks = seq(80, 140, by = 10))+
    theme(text = element_text(size = 25),
          axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title=element_text(size=25), 
          legend.text=element_text(size=25)+geom_text(size = 25),
          legend.position = "bottom",
          legend.title = element_blank())
  
  
  two_facet_posterior_pred_plot_errorbars <- ggplot(cv_post_preds)+
    geom_point(aes(x = TIME, y = DIR, col = "Observed"))+ 
    geom_point(aes(x = TIME, y = MEDIAN, col = "Estimated"))+ 
    geom_errorbar(aes(x = TIME, ymin = q2.5, ymax = q97.5, col = "Estimated"), alpha = 0.6)+
    theme_bw()+
    facet_wrap(PROVINCE ~ ., scales = "free_y", ncol= 1)+
    theme(legend.position = "bottom")+
    xlab("Month")+
    scale_x_continuous(breaks = seq(0, 140, by = 20))+
    theme(text = element_text(size = 25),
          axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title=element_text(size=25), 
          legend.text=element_text(size=25)+geom_text(size = 25),
          legend.position = "bottom",
          legend.title = element_blank())
  
  two_facet_posterior_pred_plot_cis <- ggplot(cv_post_preds)+
    geom_point(aes(x = TIME, y = DIR, col = "Observed"))+ 
    geom_line(aes(x = TIME, y = MEDIAN, col = "Estimated"))+ 
    geom_point(aes(x = TIME, y = MEDIAN, col = "Estimated"))+ 
    geom_ribbon(aes(x = TIME, ymin = q2.5, ymax = q97.5), alpha = 0.18)+
    # geom_ribbon(aes(x = TIME, ymin = q2.5, ymax = q97.5, fill = "Estimated"), alpha = 0.3)+
    # scale_fill_manual(values = hue_pal()(2)[2], name = NULL)+
    theme_bw()+
    xlab("Month")+
    scale_x_continuous(breaks = seq(0, 140, by = 30))+
    facet_wrap(PROVINCE ~ ., scales = "free_y", nrow = 5)+
    theme(legend.position = "bottom")+
    theme(text = element_text(size = 25),
          axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title=element_text(size=25), 
          legend.text=element_text(size=25)+geom_text(size = 25),
          legend.position = "bottom",
          legend.title = element_blank())
  
  two_facet_posterior_pred_plot_no_cis <- ggplot(cv_post_preds)+
    geom_line(aes(x = TIME, y = DIR, col = "Observed"))+ 
    geom_point(aes(x = TIME, y = MEDIAN, col = "Estimated"))+ 
    theme_bw()+
    facet_wrap(PROVINCE ~ ., scales = "free_y", ncol= 1)+
    theme(legend.position = "bottom")
  
  
  
  tmp_xy_plot_limited <- ggplot(cv_post_preds, aes(x = MEDIAN, y = DIR))+ 
    geom_point(alpha = 0.3, size = 3)+
    geom_abline(intercept = 0, slope = 1)+
    theme_bw()+
    labs(x = "Estimated DIR", y = "Observed DIR")+
    theme(text = element_text(size = 25),
          axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title=element_text(size=25), 
          legend.text=element_text(size=25)+geom_text(size = 25),
          legend.position = "bottom",
          legend.title = element_blank())+
    coord_cartesian(xlim=c(0, 300), ylim=c(0, 300))
  coloured_xy_plot_limited <- ggplot(cv_post_preds, aes(x = MEDIAN, y = DIR, colour = PROVINCE))+ 
    geom_point(alpha = 0.3, size = 3)+
    geom_abline(intercept = 0, slope = 1)+
    theme_bw()+
    labs(x = "Estimated DIR", y = "Observed DIR")+
    theme(text = element_text(size = 25),
          axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=25),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.title=element_text(size=25), 
          legend.text=element_text(size=25)+geom_text(size = 25),
          legend.position = "bottom",
          legend.title = element_blank())+
    coord_cartesian(xlim=c(0, 300), ylim=c(0, 300))
  
  return(list(cv_post_preds, posterior_pred_plot, posterior_pred_plot2, posterior_pred_plot3,
              tmp_xy_plot, coloured_xy_plot, posterior_pred_plot_errorbars, table_errors_by_PROVINCE,
              posterior_pred_plot_of_errors, two_facet_posterior_pred_plot_errorbars, two_facet_posterior_pred_plot_cis,
              two_facet_posterior_pred_plot_no_cis, tmp_xy_plot_limited, coloured_xy_plot_limited))
}


dir_loocv_ptl_province_pred_dt <- plot_dir_cv_posterior_preds_province(ovr_preds_dt, training_ptl_province_df[which(TIME > 1)])[[1]]
#95% Leave-one-time-point out PP Check
dir_loocv_ptl_province_pred_dt[, length(which((DIR) >= q2.5 & (DIR) <= q97.5))/length(DIR)] 

#Merge in region information also
dir_loocv_ptl_province_pred_dt <- merge(dir_loocv_ptl_province_pred_dt, region_province, by = "PROVINCE")
setkeyv(dir_loocv_ptl_province_pred_dt, c("TIME", "PROVINCE"))

#Eyeball some summary statistics
cor(dir_loocv_ptl_province_pred_dt$MEDIAN,
    dir_loocv_ptl_province_pred_dt$DIR)
caret::R2(dir_loocv_ptl_province_pred_dt$MEDIAN,
          dir_loocv_ptl_province_pred_dt$DIR)
caret::MAE(dir_loocv_ptl_province_pred_dt$MEDIAN,
           dir_loocv_ptl_province_pred_dt$DIR)





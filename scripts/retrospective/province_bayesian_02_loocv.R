# Leave-one-time-point-out cross-validation script
default_pois_formula <- CASES ~ 1 + f(MONTH, replicate = PROV_IND, model = "rw1",
    cyclic = TRUE, constr = TRUE, scale.model = TRUE, hyper = prior.prec) + f(YEAR,
    replicate = PROV_IND, model = "iid") + f(PROV_IND, model = "bym2", hyper = prior.prec,
    scale.model = TRUE, graph = file.path(peru.province.inla.data.in.dir, "nbr_piura_tumbes_lambayeque.graph")) +
    SQ_RSI_DIR_LAG + SEASON + ns(MODIFIED_DIFF_WITH_HISTORICAL_DIR_LAG, df = 4) +
    tmin_roll_2_basis + prec_roll_2_basis + icen_basis + spi_basis

num_provinces <- length(unique(training_ptl_province_inla_df$PROVINCE))
# RUN CV (2010-2017)-----
------------------------------------for (i in 1:(nrow(training_ptl_province_inla_df)/num_provinces -
    1)) {
    print(i)
    idx.pred <- seq(i * num_provinces + 1, i * num_provinces + num_provinces)  # num_provinces provinces per time step
    s <- 5000
    cv_dt <- data.table(training_ptl_province_inla_df)
    setkeyv(cv_dt, c("TIME", "PROVINCE"))
    cv_dt[, IND := seq(1, nrow(cv_dt))]
    true_cases <- c(cv_dt[which(TIME == i + 1), ]$CASES)
    true_dirs <- c(cv_dt[which(TIME == i + 1), ]$DIR)
    pop_offsets <- c(cv_dt[which(TIME == i + 1), ]$POP_OFFSET)

    cv_dt[which(TIME == i + 1), CASES := NA]
    setkeyv(cv_dt, c("TIME", "PROVINCE"))

    tmp_climate_cv_fit <- run_province_model_func(data = cv_dt, default_pois_formula)
    print(paste0("Fit ", i, " successful"))

    xx <- inla.posterior.sample(s, tmp_climate_cv_fit)

    # xx.s = num_provinces entries (by province) and 5000 columns (by sample)
    xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor[idx.pred]),
        xx)

    y.pred <- matrix(NA, num_provinces, s)
    error.pred <- matrix(NA, num_provinces, s)
    abs_error.pred <- matrix(NA, num_provinces, s)
    dir.pred <- matrix(NA, num_provinces, s)
    dir_error.pred <- matrix(NA, num_provinces, s)
    dir_abs_error.pred <- matrix(NA, num_provinces, s)


    for (s.idx in 1:s) {
        xx.sample <- xx.s[, s.idx]
        y.pred[, s.idx] <- rzipois(num_provinces, lambda = exp(xx.sample[-1]), pstr0 = xx.sample[1])
        dir.pred[, s.idx] <- y.pred[, s.idx]/pop_offsets
    }

    preds_dt <- data.table(TIME = rep(i + 1, each = num_provinces), CI_L = apply(y.pred,
        1, quantile, probs = c(0.025)), CI_U = apply(y.pred, 1, quantile, probs = c(0.975)),
        MEDIAN = apply(y.pred, 1, quantile, probs = c(0.5)), MEAN = apply(y.pred,
            1, mean), DIR_CI_L = apply(dir.pred, 1, quantile, probs = c(0.025)),
        DIR_CI_U = apply(dir.pred, 1, quantile, probs = c(0.975)), DIR_MEDIAN = apply(dir.pred,
            1, quantile, probs = c(0.5)), DIR_MEAN = apply(dir.pred, 1, mean))
    if (i%%5 == 0) {
        gc()
    }
    saveRDS(preds_dt, file = file.path(peru.province.inla.data.out.dir, paste0("training_ptl_loocv_preds",
        i, ".RDS")))
}


ovr_preds_dt <- NULL
for (i in 1:(nrow(training_ptl_province_inla_df)/num_provinces - 1)) {
    preds_dt <- readRDS(file = file.path(peru.province.inla.data.out.dir, paste0("training_ptl_loocv_preds",
        i, ".RDS")))
    ovr_preds_dt <- rbind(ovr_preds_dt, preds_dt)
}

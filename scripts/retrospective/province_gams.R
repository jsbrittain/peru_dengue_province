# install.packages("texreg")
require(mgcv)
require(MuMIn)

# Support functions ----
get_partial_resids <- function(gamFit, terms, seWithMean) {
  predType <- ifelse(seWithMean, "iterms", "terms") # Doesn't have much meaning here, but included for consistency with get_partial_preds

  linearTerm <- predict(gamFit, type = predType, terms = terms) %>%
    rowSums()

  partialResids <- residuals(gamFit) + linearTerm # TODO: unclear if using deviance residuals is the best idea (differs from plot.gam)

  partialResids
}

get_partial_preds <- function(gamFit, newdata, terms, seWithMean) {
  predType <- ifelse(seWithMean, "iterms", "terms")
  tmp_preds <- predict(gamFit,
    newdata = newdata, se.fit = T,
    type = predType, terms = terms
  ) %>%
    lapply(rowSums) %>%
    as.data.frame()
  tmp_preds <- as.data.table(tmp_preds)
  setnames(tmp_preds, c("fit", "se.fit"), c("y", "se"))
  tmp_preds[, ylower := y - 1.96 * se]
  tmp_preds[, yupper := y + 1.96 * se]
  print(tmp_preds$ylower)

  return(tmp_preds)
}

get_partial_effects_interaction <- function(gamFit, var1, var2, seWithMean = TRUE, fixedEffect = FALSE) {
  ## Term names: wrap in s():
  if (!is.null(var2)) {
    if (fixedEffect) stop("Non-smooth interactions not implemented")

    termnames <- c(paste0("s(", var1, ",", var2, ")"))
  } else {
    if (!fixedEffect) {
      termnames <- paste0("s(", var1, ")")
    } else {
      termnames <- var1
    }
  }


  ## Variables not part of the effect / interaction are kept constant:
  modelData <- gamFit$model
  responseIndex <- attr(modelData, "terms") %>% attr("response")
  responseName <- colnames(modelData)[responseIndex]

  otherData <- modelData %>%
    select(-one_of(responseName, var1, var2))

  numericData <- otherData %>%
    summarise_if(is.numeric, ~ median(.))

  factorData <- otherData %>%
    summarise_if(is.factor, ~ names(which.max(table(.))))

  stopifnot(all(colnames(otherData) %in% c(colnames(numericData), colnames(factorData)))) # Would indicate unhandled column types


  ## Calculate partial residuals
  partialDat <- modelData %>%
    data.frame() %>%
    select(one_of(var1, var2))

  if (length(numericData) > 0) partialDat <- cbind(partialDat, numericData)
  if (length(factorData) > 0) partialDat <- cbind(partialDat, factorData)


  partialResids <- get_partial_resids(gamFit, termnames, seWithMean)
  partialResids <- cbind(partialDat,
    Residual = partialResids
  )


  ## Predictions
  # - Make a prediction for each level of the interaction var (so for all interactions that occur)
  newData <- modelData %>%
    data.frame() %>%
    select(one_of(var1, var2)) %>%
    unique()

  # - All other data get set to their median (or the most common factor level)
  if (length(numericData) > 0) newData <- cbind(newData, numericData)
  if (length(factorData) > 0) newData <- cbind(newData, factorData)


  # - Make predictions
  newPredictions <- get_partial_preds(gamFit, newData, termnames, seWithMean) %>%
    mutate(IsSignificant = if_else(ylower <= 0 & yupper >= 0, "No", "Yes")) %>% # Check if CI crosses zero
    cbind(newData)

  # Add significance to residuals (for plotting):
  partialResids <- newPredictions %>%
    select(one_of(var1, var2), IsSignificant) %>%
    right_join(partialResids)

  # Return:
  list(effects = newPredictions, partialResiduals = partialResids)
}

get_partial_effects <- function(fit, var, seWithMean = TRUE) {
  get_partial_effects_interaction(fit, var, NULL, seWithMean)
}
get_partial_effects_binary_single <- function(fit, var, seWithMean = TRUE, fixedEffect = TRUE, removeNegatives = TRUE) {
  plotData <- get_partial_effects_interaction(fit, var1 = var, NULL, seWithMean, fixedEffect)

  # Remove negatives
  if (removeNegatives) {
    plotData$effects <- plotData$effects[plotData$effects[[var]] == 1, ]
    plotData$partialResiduals <- plotData$partialResiduals[plotData$partialResiduals[[var]] == 1, ]
  }

  # Add a column containing var as a label
  plotData$effects$variable <- var
  plotData$partialResiduals$variable <- var

  # Return
  plotData
}

get_partial_effects_binary <- function(fit, vars, seWithMean = TRUE, fixedEffect = TRUE, removeNegatives = TRUE) {
  allData <- lapply(vars, get_partial_effects_binary_single,
    fit = fit,
    seWithMean = seWithMean,
    fixedEffect = fixedEffect,
    removeNegatives = removeNegatives
  )

  extract_by_name <- function(x, name) x[[name]]
  effects <- lapply(allData, extract_by_name, "effects")
  partialResiduals <- lapply(allData, extract_by_name, "partialResiduals")

  effects <- do.call(rbind, effects)
  partialResiduals <- do.call(rbind, partialResiduals)

  list(effects = effects, partialResiduals = partialResiduals)
}

get_partial_effects_continuous <- function(gamFit, var, resolution = 1, seWithMean = TRUE) {
  ## Term names: wrap in s():
  termnames <- paste0("s(", var, ")")


  ## Data not part of effect kept constant:
  modelData <- gamFit$model
  responseIndex <- attr(modelData, "terms") %>% attr("response")
  responseName <- colnames(modelData)[responseIndex]

  otherData <- modelData %>%
    select(-one_of(responseName, var))

  numericData <- otherData %>%
    summarise_if(is.numeric, ~ median(.))

  factorData <- otherData %>%
    summarise_if(is.factor, ~ names(which.max(table(.))))

  stopifnot(all(colnames(otherData) %in% c(colnames(numericData), colnames(factorData)))) # Would indicate unhandled column types


  ## Calculate partial residuals
  partialDat <- modelData %>%
    data.frame() %>%
    select(one_of(var))

  if (length(numericData) > 0) partialDat <- cbind(partialDat, numericData)
  if (length(factorData) > 0) partialDat <- cbind(partialDat, factorData)


  partialResids <- get_partial_resids(gamFit, termnames, seWithMean)
  partialResids <- cbind(partialDat,
    Residual = partialResids
  )


  ## Predictions
  # - Predictions over a smooth range of values spanning the range of var:
  newData <- seq(min(modelData[, var]), max(modelData[, var]), by = resolution) %>%
    data.frame()

  colnames(newData) <- var

  # - All other data get set to their median (or the most common factor level)
  if (length(numericData) > 0) newData <- cbind(newData, numericData)
  if (length(factorData) > 0) newData <- cbind(newData, factorData)

  # - Make predictions
  newPredictions <- get_partial_preds(gamFit, newData, termnames, seWithMean) %>%
    mutate(
      NotSignificant = ylower <= 0 & yupper >= 0,
      IsSignificant = if_else(all(NotSignificant), "No", "Yes")
    ) %>% # Check if CI crosses 0 over entire range
    cbind(newData)

  partialResids$IsSignificant <- unique(newPredictions$IsSignificant)

  # Return:
  list(effects = newPredictions, partialResiduals = partialResids)
}

plot.partial <- function(df, var, response_var) {
  df1 <- df$effects
  df2 <- df$partialResiduals
  # head(df2)

  # head(df1)
  names(df1)[names(df1) == var] <- "var"
  names(df2)[names(df2) == var] <- "var"

  fillz <- c("No" = "gray70", "Yes" = "skyblue3")


  # p2 <- ggplot(data=df2, aes(var,  Residual)) +
  #     geom_boxplot(aes(var~Residual))

  p1 <- ggplot(data = df1, aes(var, y)) +
    geom_crossbar(aes(ymin = ylower, ymax = yupper, fill = IsSignificant),
      alpha = .4, show.legend = F
    ) +
    # geom_point(aes(x=var, y=y, color=var), size=5) +
    # geom_jitter(data=df2, aes(x=var, y=Residual), width=.1, alpha=.2, size=.3)+
    scale_fill_manual(values = fillz) +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    theme_bw() +
    theme(
      panel.grid = element_blank(), axis.title.x = element_blank(),
      axis.text.x = element_text(size = 8, angle = 90),
      plot.margin = unit(c(.1, .1, .5, .1), "cm")
    ) +
    ylab(paste0("partial effect of ", var, " on ", response_var))

  # print(p1)

  return(p1)
}

plot.partial.cont <- function(df, log, var, response_var, alt_var) {
  df1 <- df$effects
  df2 <- df$partialResiduals
  # head(df2)

  # head(df1)
  names(df1)[names(df1) == var] <- "var"
  names(df2)[names(df2) == var] <- "var"

  fillz <- c("No" = "gray70", "Yes" = "skyblue3")


  # p2 <- ggplot(data=df2, aes(var,  Residual)) +
  #     geom_boxplot(aes(var~Residual))

  if (log == F) {
    p1 <- ggplot(data = df1, aes(var, y)) +
      geom_line(aes(color = IsSignificant), size = 3) +
      geom_ribbon(aes(ymin = ylower, ymax = yupper, fill = IsSignificant),
        alpha = .4, show.legend = F
      ) +
      # geom_point(aes(x=var, y=y, color=var), size=5) +
      # geom_jitter(data=df2, aes(x=var, y=Residual), width=.1, alpha=.2, size=.3)+
      scale_fill_manual(values = fillz) +
      scale_color_manual(values = fillz) +
      scale_x_continuous(labels = scales::comma) +
      geom_hline(aes(yintercept = 0), linetype = 2) +
      theme_bw() +
      theme(
        panel.grid = element_blank(), # axis.title.x = element_blank(),
        # axis.text.x = element_text(size=8, angle = 45),
        plot.margin = unit(c(.1, .1, .5, .1), "cm"),
        legend.position = c(.85, .85)
      ) +
      ylab(paste0("partial effect of ", alt_var, " on ", response_var)) +
      xlab(alt_var)
  } else {
    df1$var <- 10^(df1$var)

    p1 <- ggplot(data = df1, aes(var, y)) +
      geom_line(aes(color = IsSignificant), size = 3) +
      geom_ribbon(aes(ymin = ylower, ymax = yupper, fill = IsSignificant),
        alpha = .4, show.legend = F
      ) +
      # geom_point(aes(x=var, y=y, color=var), size=5) +
      # geom_jitter(data=df2, aes(x=var, y=Residual), width=.1, alpha=.2, size=.3)+
      scale_fill_manual(values = fillz) +
      scale_color_manual(values = fillz) +
      scale_x_continuous(labels = scales::comma) +
      geom_hline(aes(yintercept = 0), linetype = 2) +
      theme_bw() +
      theme(
        panel.grid = element_blank(), # axis.title.x = element_blank(),
        # axis.text.x = element_text(size=8, angle = 45),
        plot.margin = unit(c(.1, .1, .5, .1), "cm"),
        legend.position = c(.85, .85)
      ) +
      ylab(paste0("partial effect of ", alt_var, " on ", response_var)) +
      xlab(alt_var)
  }

  print(p1)

  return(p1)
}



# Set up correlation data.table
# Adding some variables to correlation data.table for GAMs
tmp <- subset(ptl_province_inla_df, select = c("YEAR", "TIME", "PROVINCE", "POP"))
setnames(tmp, c("PROVINCE", "POP"), c("comp_prov", "POP2"))
tmp <- tmp[, list(POP2 = mean(POP2)), by = c("comp_prov", "YEAR")]
annual.case.pearsons.df2 <- merge(annual.case.pearsons.df2, tmp, by = c("comp_prov", "YEAR"))

annual.case.pearsons.df2[, POP_1_2 := log(POP) + log(POP2)] # Gravity approx, recall log(x) + log(y) = log(xy)

# Correlation Gravity GAMS ----
# Annual Cases
correlation_gravity_gams <- function(data) {
  # Formulae
  tmax_pop_prec_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP_1_2, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_pop_spi_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP_1_2, bs = "tp") + s(spi, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_pop_prec_icen_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP_1_2, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )
  tmax_prec_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_prec_icen_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )

  tmin_pop_prec_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmin, bs = "tp") + s(POP_1_2, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmin_pop_spi_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmin, bs = "tp") + s(POP_1_2, bs = "tp") + s(spi, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmin_pop_prec_icen_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmin, bs = "tp") + s(POP_1_2, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )
  tmin_prec_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmin, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmin_prec_icen_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmin, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )


  tmax_pop_prec_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP_1_2, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_pop_spi_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP_1_2, bs = "tp") + s(spi, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_pop_prec_icen_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP_1_2, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )
  tmax_prec_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_prec_icen_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )

  tmin_pop_prec_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmin, bs = "tp") + s(POP_1_2, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmin_pop_spi_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmin, bs = "tp") + s(POP_1_2, bs = "tp") + s(spi, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmin_pop_prec_icen_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmin, bs = "tp") + s(POP_1_2, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )
  tmin_prec_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmin, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmin_prec_icen_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmin, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )
  model_fits <- list(
    tmax_pop_prec_year_fit, tmax_pop_spi_year_fit, tmax_pop_prec_icen_year_fit,
    tmax_prec_year_fit, tmax_prec_icen_year_fit, tmin_pop_prec_year_fit, tmin_pop_spi_year_fit, tmin_pop_prec_icen_year_fit, tmin_prec_year_fit,
    tmin_prec_icen_year_fit, tmax_pop_prec_fit, tmax_pop_spi_fit, tmax_pop_prec_icen_fit,
    tmax_prec_fit, tmax_prec_icen_fit, tmin_pop_prec_fit, tmin_pop_spi_fit, tmin_pop_prec_icen_fit,
    tmin_prec_fit, tmin_prec_icen_fit
  )

  texreg_model_fits <- lapply(model_fits, function(x) texreg::extract(x))

  model_names <- c(
    "tmax_pop_prec_year_fit", "tmax_pop_spi_year_fit", "tmax_pop_prec_icen_year_fit",
    "tmax_prec_year_fit", "tmax_prec_icen_year_fit", "tmin_pop_prec_year_fit", "tmin_pop_spi_year_fit",
    "tmin_pop_prec_icen_year_fit", "tmin_prec_year_fit",
    "tmin_prec_icen_year_fit", "tmax_pop_prec_fit", "tmax_pop_spi_fit", "tmax_pop_prec_icen_fit",
    "tmax_prec_fit", "tmax_prec_icen_fit", "tmin_pop_prec_fit", "tmin_pop_spi_fit", "tmin_pop_prec_icen_fit",
    "tmin_prec_fit", "tmin_prec_icen_fit"
  )
  aic_vals <- c(
    tmax_pop_prec_year_fit$aic, tmax_pop_spi_year_fit$aic, tmax_pop_prec_icen_year_fit$aic,
    tmax_prec_year_fit$aic, tmax_prec_icen_year_fit$aic, tmin_pop_prec_year_fit$aic, tmin_pop_spi_year_fit$aic, tmin_pop_prec_icen_year_fit$aic, tmin_prec_year_fit$aic,
    tmin_prec_icen_year_fit$aic, tmax_pop_prec_fit$aic, tmax_pop_spi_fit$aic, tmax_pop_prec_icen_fit$aic,
    tmax_prec_fit$aic, tmax_prec_icen_fit$aic, tmin_pop_prec_fit$aic, tmin_pop_spi_fit$aic, tmin_pop_prec_icen_fit$aic,
    tmin_prec_fit$aic, tmin_prec_icen_fit$aic
  )

  aic_vals <- c(
    tmax_pop_prec_year_fit$aic, tmax_pop_spi_year_fit$aic, tmax_pop_prec_icen_year_fit$aic,
    tmax_prec_year_fit$aic, tmax_prec_icen_year_fit$aic, tmin_pop_prec_year_fit$aic, tmin_pop_spi_year_fit$aic, tmin_pop_prec_icen_year_fit$aic, tmin_prec_year_fit$aic,
    tmin_prec_icen_year_fit$aic, tmax_pop_prec_fit$aic, tmax_pop_spi_fit$aic, tmax_pop_prec_icen_fit$aic,
    tmax_prec_fit$aic, tmax_prec_icen_fit$aic, tmin_pop_prec_fit$aic, tmin_pop_spi_fit$aic, tmin_pop_prec_icen_fit$aic,
    tmin_prec_fit$aic, tmin_prec_icen_fit$aic
  )

  bic_vals <- c(
    BIC(tmax_pop_prec_year_fit), BIC(tmax_pop_spi_year_fit), BIC(tmax_pop_prec_icen_year_fit),
    BIC(tmax_prec_year_fit), BIC(tmax_prec_icen_year_fit),
    BIC(tmin_pop_prec_year_fit), BIC(tmin_pop_spi_year_fit),
    BIC(tmin_pop_prec_icen_year_fit), BIC(tmin_prec_year_fit),
    BIC(tmin_prec_icen_year_fit), BIC(tmax_pop_prec_fit), BIC(tmax_pop_spi_fit), BIC(tmax_pop_prec_icen_fit),
    BIC(tmax_prec_fit), BIC(tmax_prec_icen_fit), BIC(tmin_pop_prec_fit), BIC(tmin_pop_spi_fit), BIC(tmin_pop_prec_icen_fit),
    BIC(tmin_prec_fit), BIC(tmin_prec_icen_fit)
  )

  aicc_vals <- c(
    AICc(tmax_pop_prec_year_fit), AICc(tmax_pop_spi_year_fit), AICc(tmax_pop_prec_icen_year_fit),
    AICc(tmax_prec_year_fit), AICc(tmax_prec_icen_year_fit),
    AICc(tmin_pop_prec_year_fit), AICc(tmin_pop_spi_year_fit),
    AICc(tmin_pop_prec_icen_year_fit), AICc(tmin_prec_year_fit),
    AICc(tmin_prec_icen_year_fit), AICc(tmax_pop_prec_fit), AICc(tmax_pop_spi_fit), AICc(tmax_pop_prec_icen_fit),
    AICc(tmax_prec_fit), AICc(tmax_prec_icen_fit), AICc(tmin_pop_prec_fit), AICc(tmin_pop_spi_fit), AICc(tmin_pop_prec_icen_fit),
    AICc(tmin_prec_fit), AICc(tmin_prec_icen_fit)
  )

  gcv_vals <- c(
    tmax_pop_prec_year_fit$gcv.ubre, tmax_pop_spi_year_fit$gcv.ubre, tmax_pop_prec_icen_year_fit$gcv.ubre,
    tmax_prec_year_fit$gcv.ubre, tmax_prec_icen_year_fit$gcv.ubre, tmin_pop_prec_year_fit$gcv.ubre, tmin_pop_spi_year_fit$gcv.ubre, tmin_pop_prec_icen_year_fit$gcv.ubre, tmin_prec_year_fit$gcv.ubre,
    tmin_prec_icen_year_fit$gcv.ubre, tmax_pop_prec_fit$gcv.ubre, tmax_pop_spi_fit$gcv.ubre, tmax_pop_prec_icen_fit$gcv.ubre,
    tmax_prec_fit$gcv.ubre, tmax_prec_icen_fit$gcv.ubre, tmin_pop_prec_fit$gcv.ubre, tmin_pop_spi_fit$gcv.ubre, tmin_pop_prec_icen_fit$gcv.ubre,
    tmin_prec_fit$gcv.ubre, tmin_prec_icen_fit$gcv.ubre
  )
  model_summary <- data.table(
    MODEL = model_names, AIC = aic_vals, BIC = bic_vals, gcv = gcv_vals,
    aicc = aicc_vals,
    MODEL_NUMBER = seq(1, length(model_names), by = 1)
  )
  return(list(model_fits, model_summary))
}

correlation_gravity_gams_output <- correlation_gravity_gams(annual.case.pearsons.df2)
correlation_gravity_gams_output[[2]][order(gcv, decreasing = FALSE), ]
correlation_gravity_gams_output[[2]][order(AIC, decreasing = FALSE), ]

correlation_gravity_gams_models <- correlation_gravity_gams_output[[1]]
correlation_gravity_gams_summary <- correlation_gravity_gams_output[[2]]
correlation_gravity_gams_summary[, MOD_TYPE_NUM := paste0("Gravity_", MODEL_NUMBER)]
correlation_gravity_gams_summary[order(gcv, decreasing = FALSE), ]
correlation_gravity_gams_summary[order(AIC, decreasing = FALSE), ]
correlation_gravity_gams_summary[order(BIC, decreasing = FALSE), ]
correlation_gravity_gams_summary[order(aicc, decreasing = FALSE), ]


# AIC, AICc and gcv correlation best fit gravity model ----
corr_tmin_pop_prec_year_gravity_gam_fit <- correlation_gravity_gams_models[[6]]
corr_tmin_pop_prec_year_gravity_gam_fit.year.df <- get_partial_effects(fit = corr_tmin_pop_prec_year_gravity_gam_fit, var = "YEAR_FACTOR")
corr_tmin_pop_prec_year_gravity_gam_fit.year.df <- corr_tmin_pop_prec_year_gravity_gam_fit.year.df[[1]]
corr_tmin_pop_prec_year_gravity_gam_fit.prov.df <- get_partial_effects(fit = corr_tmin_pop_prec_year_gravity_gam_fit, var = "PROVINCE_FACTOR")
corr_tmin_pop_prec_year_gravity_gam_fit.prov.df <- corr_tmin_pop_prec_year_gravity_gam_fit.prov.df[[1]]

corr_tmin_pop_prec_year_gravity_gam_fit.out.tmin <- get_partial_effects(fit = corr_tmin_pop_prec_year_gravity_gam_fit, var = "tmin")
corr_tmin_pop_prec_year_gravity_gam_fit.tmin.df <- cbind.data.frame(
  tmin = corr_tmin_pop_prec_year_gravity_gam_fit.out.tmin$effects$tmin, y = corr_tmin_pop_prec_year_gravity_gam_fit.out.tmin$effects$y,
  ylower = (corr_tmin_pop_prec_year_gravity_gam_fit.out.tmin$effects$y - 1.96 * corr_tmin_pop_prec_year_gravity_gam_fit.out.tmin$effects$se),
  yupper = (corr_tmin_pop_prec_year_gravity_gam_fit.out.tmin$effects$y + 1.96 * corr_tmin_pop_prec_year_gravity_gam_fit.out.tmin$effects$se)
)

corr_tmin_pop_prec_year_gravity_gam_fit.tmin.df$IsSig <- "Pos"

corr_tmin_pop_prec_year_gravity_gam_fit.out.prec <- get_partial_effects(fit = corr_tmin_pop_prec_year_gravity_gam_fit, var = "prec")
corr_tmin_pop_prec_year_gravity_gam_fit.prec.df <- cbind.data.frame(
  prec = corr_tmin_pop_prec_year_gravity_gam_fit.out.prec$effects$prec, y = corr_tmin_pop_prec_year_gravity_gam_fit.out.prec$effects$y,
  ylower = (corr_tmin_pop_prec_year_gravity_gam_fit.out.prec$effects$y - 1.96 * corr_tmin_pop_prec_year_gravity_gam_fit.out.prec$effects$se),
  yupper = (corr_tmin_pop_prec_year_gravity_gam_fit.out.prec$effects$y + 1.96 * corr_tmin_pop_prec_year_gravity_gam_fit.out.prec$effects$se)
)
corr_tmin_pop_prec_year_gravity_gam_fit.prec.df$IsSig <- "Pos"

corr_tmin_pop_prec_year_gravity_gam_fit.out.pop <- get_partial_effects(fit = corr_tmin_pop_prec_year_gravity_gam_fit, var = "POP_1_2")
corr_tmin_pop_prec_year_gravity_gam_fit.pop.df <- cbind.data.frame(
  POP = corr_tmin_pop_prec_year_gravity_gam_fit.out.pop$effects$POP, y = corr_tmin_pop_prec_year_gravity_gam_fit.out.pop$effects$y,
  ylower = (corr_tmin_pop_prec_year_gravity_gam_fit.out.pop$effects$y - 1.96 * corr_tmin_pop_prec_year_gravity_gam_fit.out.pop$effects$se),
  yupper = (corr_tmin_pop_prec_year_gravity_gam_fit.out.pop$effects$y + 1.96 * corr_tmin_pop_prec_year_gravity_gam_fit.out.pop$effects$se)
)
corr_tmin_pop_prec_year_gravity_gam_fit.pop.df$IsSig <- "Pos"

# and the fixed effects
out <- summary(corr_tmin_pop_prec_year_gravity_gam_fit)
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df <- cbind.data.frame(PROVINCE_FACTOR = c("intercept", sort(as.character(unique(annual.case.pearsons.df2$PROVINCE_FACTOR)))), y = out$p.coeff, ylower = (out$p.coeff - 1.96 * out$p.table[, 2]), yupper = (out$p.coeff + 1.96 * out$p.table[, 2]), pval = out$p.pv)
rownames(corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df) <- c()
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df$IsSig <- "NotSig"
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df$IsSig[corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df$y > 0 & corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df$pval < 0.01] <- "Pos"
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df$IsSig[corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df$y < 0 & corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df$pval < 0.01] <- "Neg"

corr_tmin_pop_prec_year_gravity_gam_fit.year.df$IsSig <- "NotSig"
corr_tmin_pop_prec_year_gravity_gam_fit.year.df$IsSig[corr_tmin_pop_prec_year_gravity_gam_fit.year.df$IsSignificant == "Yes" & corr_tmin_pop_prec_year_gravity_gam_fit.year.df$y > 0] <- "Pos"
corr_tmin_pop_prec_year_gravity_gam_fit.year.df$IsSig[corr_tmin_pop_prec_year_gravity_gam_fit.year.df$IsSignificant == "Yes" & corr_tmin_pop_prec_year_gravity_gam_fit.year.df$y < 0] <- "Neg"

# and plot
colz <- c("Pos" = "red", "Neg" = "blue", "NotSig" = "gray50")
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df <- subset(corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df, PROVINCE_FACTOR != "intercept")

length(which(corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df$y < 0))
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects <- ggplot(data = corr_tmin_pop_prec_year_gravity_gam_fit.fixed.df) +
  theme_bw() +
  coord_flip() +
  theme(panel.grid = element_blank(), axis.title.y = element_blank()) +
  ylab(bquote(atop("Fixed effect of interaction between province and distance", "on pairwise correlation coefficient of annual cases," ~ rho))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = PROVINCE_FACTOR, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_linerange(aes(x = PROVINCE_FACTOR, ymin = ylower, ymax = yupper, color = IsSig), show.legend = F) +
  scale_color_manual(values = colz)
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects
dev.off()
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects.year.effects <- ggplot(data = corr_tmin_pop_prec_year_gravity_gam_fit.year.df) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 300, vjust = 0)) +
  ylab(bquote(atop("Partial effect of year on", "pairwise correlation coefficient of annual cases," ~ rho))) +
  geom_hline(aes(yintercept = 0)) +
  xlab("Year") +
  geom_point(aes(x = YEAR_FACTOR, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_linerange(aes(x = YEAR_FACTOR, ymin = ylower, ymax = yupper, color = IsSig), show.legend = F) +
  scale_color_manual(values = colz)
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects.year.effects

corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects.tmin.effects <-
  ggplot(data = corr_tmin_pop_prec_year_gravity_gam_fit.tmin.df) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylab(bquote(atop("Partial effect of monthly average of daily minimum temperature", "on pairwise correlation coefficient of annual cases," ~ rho))) +
  xlab(bquote("Yearly average of monthly average daily minimum temperature (" * degree ~ "C)")) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(x = tmin, y = y), color = "darkgoldenrod1", show.legend = F, size = 3) +
  geom_ribbon(aes(x = tmin, ymin = ylower, ymax = yupper), fill = "darkgoldenrod1", alpha = .3, show.legend = F)
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects.tmin.effects

corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects.prec.effects <- ggplot(data = corr_tmin_pop_prec_year_gravity_gam_fit.prec.df) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylab(bquote(atop("Partial effect of monthly total precipitation", "on pairwise correlation coefficient of annual cases," ~ rho))) +
  xlab("Monthly total precipitation (mm)") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(x = prec, y = y),
    show.legend = F, size = 3,
    color = "turquoise"
  ) +
  geom_ribbon(aes(x = prec, ymin = ylower, ymax = yupper), fill = "turquoise", alpha = .3, show.legend = F)
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects.prec.effects

corr_tmin_pop_prec_year_gravity_gam_fit.pop.df
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects.pop.effects <- ggplot(data = corr_tmin_pop_prec_year_gravity_gam_fit.pop.df) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylab(bquote(atop("Partial effect of population on", "pairwise correlation coefficient," ~ rho))) +
  xlab("Logarithm of pairwise product of populations") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(x = POP, y = y), color = "forestgreen", show.legend = F, size = 3) +
  geom_ribbon(aes(x = POP, ymin = ylower, ymax = yupper), fill = "forestgreen", alpha = .3, show.legend = F)
corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects.pop.effects




corr_tmin_pop_prec_year_gravity_gam_plot <-
  ggarrange(
    corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects + theme(plot.margin = unit(
      c(0.8, 0.8, 0.8, 0.8),
      "inches"
    )) + theme(
      text = element_text(size = 22),
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.8),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 22),
      legend.text = element_text(size = 22) + geom_text(size = 22)
    ),
    corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects.year.effects + theme(plot.margin = unit(
      c(0.8, 0.8, 0.8, 0.8),
      "inches"
    )) + theme(
      text = element_text(size = 22),
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.8),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 22),
      legend.text = element_text(size = 22) + geom_text(size = 22)
    ),
    corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects.tmin.effects + theme(plot.margin = unit(
      c(0.8, 0.8, 0.8, 0.8),
      "inches"
    )) + theme(
      text = element_text(size = 22),
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.8),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 22),
      legend.text = element_text(size = 22) + geom_text(size = 22)
    ),
    corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects.prec.effects + theme(plot.margin = unit(
      c(0.8, 0.8, 0.8, 0.8),
      "inches"
    )) + theme(
      text = element_text(size = 22),
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.8),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 22),
      legend.text = element_text(size = 22) + geom_text(size = 22)
    ),
    corr_tmin_pop_prec_year_gravity_gam_fit.fixed.effects.pop.effects + theme(plot.margin = unit(
      c(0.8, 0.8, 0.8, 0.8),
      "inches"
    )) + theme(
      text = element_text(size = 22),
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.8),
      plot.subtitle = element_blank(),
      axis.title = element_text(size = 22),
      legend.text = element_text(size = 22) + geom_text(size = 22)
    ),
    nrow = 3, ncol = 2, align = "v"
  )
corr_tmin_pop_prec_year_gravity_gam_plot
ggsave(corr_tmin_pop_prec_year_gravity_gam_plot,
  file = file.path(
    peru.province.wavelet.out.dir,
    "corr_tmin_pop_prec_year_gravity_gam_plot.pdf"
  ),
  h = 24, w = 24
) # Plot for paper of effects



# BIC: best fit gravity model ----
corr_tmax_pop_prec_icen_year_gravity_gam_fit <- correlation_gravity_gams_models[[3]]
corr_tmax_pop_prec_icen_year_gravity_gam_fit.year.df <- get_partial_effects(fit = corr_tmax_pop_prec_icen_year_gravity_gam_fit, var = "YEAR_FACTOR")
corr_tmax_pop_prec_icen_year_gravity_gam_fit.year.df <- corr_tmax_pop_prec_icen_year_gravity_gam_fit.year.df[[1]]
corr_tmax_pop_prec_icen_year_gravity_gam_fit.prov.df <- get_partial_effects(fit = corr_tmax_pop_prec_icen_year_gravity_gam_fit, var = "PROVINCE_FACTOR")
corr_tmax_pop_prec_icen_year_gravity_gam_fit.prov.df <- corr_tmax_pop_prec_icen_year_gravity_gam_fit.prov.df[[1]]

corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.tmax <- get_partial_effects(fit = corr_tmax_pop_prec_icen_year_gravity_gam_fit, var = "tmax")
corr_tmax_pop_prec_icen_year_gravity_gam_fit.tmax.df <- cbind.data.frame(
  tmax = corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.tmax$effects$tmax, y = corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.tmax$effects$y,
  ylower = (corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.tmax$effects$y - 1.96 * corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.tmax$effects$se),
  yupper = (corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.tmax$effects$y + 1.96 * corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.tmax$effects$se)
)

corr_tmax_pop_prec_icen_year_gravity_gam_fit.tmax.df$IsSig <- "Pos"
# corr_tmax_pop_prec_icen_year_gravity_gam_fit.tmax.df$tmax_C <- 10^(corr_tmax_pop_prec_icen_year_gravity_gam_fit.tmax.df$tmax)

corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.prec <- get_partial_effects(fit = corr_tmax_pop_prec_icen_year_gravity_gam_fit, var = "prec")
corr_tmax_pop_prec_icen_year_gravity_gam_fit.prec.df <- cbind.data.frame(
  prec = corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.prec$effects$prec, y = corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.prec$effects$y,
  ylower = (corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.prec$effects$y - 1.96 * corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.prec$effects$se),
  yupper = (corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.prec$effects$y + 1.96 * corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.prec$effects$se)
)
corr_tmax_pop_prec_icen_year_gravity_gam_fit.prec.df$IsSig <- "Pos"

corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.icen <- get_partial_effects(fit = corr_tmax_pop_prec_icen_year_gravity_gam_fit, var = "icen")
corr_tmax_pop_prec_icen_year_gravity_gam_fit.icen.df <- cbind.data.frame(
  icen = corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.icen$effects$icen, y = corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.icen$effects$y,
  ylower = (corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.icen$effects$y - 1.96 * corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.icen$effects$se),
  yupper = (corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.icen$effects$y + 1.96 * corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.icen$effects$se)
)
corr_tmax_pop_prec_icen_year_gravity_gam_fit.icen.df$IsSig <- "Pos"

corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.pop <- get_partial_effects(fit = corr_tmax_pop_prec_icen_year_gravity_gam_fit, var = "POP_1_2")
corr_tmax_pop_prec_icen_year_gravity_gam_fit.pop.df <- cbind.data.frame(
  POP = corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.pop$effects$POP, y = corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.pop$effects$y,
  ylower = (corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.pop$effects$y - 1.96 * corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.pop$effects$se),
  yupper = (corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.pop$effects$y + 1.96 * corr_tmax_pop_prec_icen_year_gravity_gam_fit.out.pop$effects$se)
)
corr_tmax_pop_prec_icen_year_gravity_gam_fit.pop.df$IsSig <- "Pos"

# and the fixed effects
out <- summary(corr_tmax_pop_prec_icen_year_gravity_gam_fit)
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df <- cbind.data.frame(PROVINCE_FACTOR = c("intercept", sort(as.character(unique(annual.case.pearsons.df2$PROVINCE_FACTOR)))), y = out$p.coeff, ylower = (out$p.coeff - 1.96 * out$p.table[, 2]), yupper = (out$p.coeff + 1.96 * out$p.table[, 2]), pval = out$p.pv)
rownames(corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df) <- c()
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df$IsSig <- "NotSig"
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df$IsSig[corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df$y > 0 & corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df$pval < 0.01] <- "Pos"
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df$IsSig[corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df$y < 0 & corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df$pval < 0.01] <- "Neg"

corr_tmax_pop_prec_icen_year_gravity_gam_fit.year.df$IsSig <- "NotSig"
corr_tmax_pop_prec_icen_year_gravity_gam_fit.year.df$IsSig[corr_tmax_pop_prec_icen_year_gravity_gam_fit.year.df$IsSignificant == "Yes" & corr_tmax_pop_prec_icen_year_gravity_gam_fit.year.df$y > 0] <- "Pos"
corr_tmax_pop_prec_icen_year_gravity_gam_fit.year.df$IsSig[corr_tmax_pop_prec_icen_year_gravity_gam_fit.year.df$IsSignificant == "Yes" & corr_tmax_pop_prec_icen_year_gravity_gam_fit.year.df$y < 0] <- "Neg"

# and plot
colz <- c("Pos" = "red", "Neg" = "blue", "NotSig" = "gray50")
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df <- subset(corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df, PROVINCE_FACTOR != "intercept")
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.effects <- ggplot(data = corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.df) +
  theme_bw() +
  coord_flip() +
  theme(panel.grid = element_blank(), axis.title.y = element_blank()) +
  ylab(bquote(atop("fixed effect of province and geographic distance", "interaction on pairwise correlation coefficient," ~ rho))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = PROVINCE_FACTOR, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_linerange(aes(x = PROVINCE_FACTOR, ymin = ylower, ymax = yupper, color = IsSig), show.legend = F) +
  scale_color_manual(values = colz)
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.effects

corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.effects.year.effects <- ggplot(data = corr_tmax_pop_prec_icen_year_gravity_gam_fit.year.df) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(), axis.text.x = element_text(angle = 300, vjust = 0)) +
  ylab(bquote(atop("partial effect of year on", "pairwise correlation coefficient," ~ rho))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = YEAR_FACTOR, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_linerange(aes(x = YEAR_FACTOR, ymin = ylower, ymax = yupper, color = IsSig), show.legend = F) +
  scale_color_manual(values = colz)
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.effects.year.effects

corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.effects.tmax.effects <- ggplot(data = corr_tmax_pop_prec_icen_year_gravity_gam_fit.tmax.df) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylab(bquote(atop("partial effect of temperature on", "pairwise correlation coefficient," ~ rho))) +
  xlab(bquote("mean biweekly temperature ("^0 ~ "C)")) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(x = tmax, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_ribbon(aes(x = tmax, ymin = ylower, ymax = yupper, fill = IsSig), alpha = .3, show.legend = F) +
  scale_color_manual(values = colz) +
  scale_fill_manual(values = colz)
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.effects.tmax.effects

corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.effects.prec.effects <- ggplot(data = corr_tmax_pop_prec_icen_year_gravity_gam_fit.prec.df) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylab(bquote(atop("partial effect of precipitation on", "pairwise correlation coefficient," ~ rho))) +
  xlab("mean total annual precipitation (mm)") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(x = prec, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_ribbon(aes(x = prec, ymin = ylower, ymax = yupper, fill = IsSig), alpha = .3, show.legend = F) +
  scale_color_manual(values = colz) +
  scale_fill_manual(values = colz)
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.effects.prec.effects


corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.effects.icen.effects <-
  ggplot(data = corr_tmax_pop_prec_icen_year_gravity_gam_fit.icen.df) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylab(bquote(atop("partial effect of ICEN on", "pairwise correlation coefficient," ~ rho))) +
  xlab("ICEN E-index") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(x = icen, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_ribbon(aes(x = icen, ymin = ylower, ymax = yupper, fill = IsSig), alpha = .3, show.legend = F) +
  scale_color_manual(values = colz) +
  scale_fill_manual(values = colz)
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.effects.icen.effects

corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.effects.pop.effects <- ggplot(data = corr_tmax_pop_prec_icen_year_gravity_gam_fit.pop.df) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylab(bquote(atop("partial effect of province population on", "pairwise correlation coefficient," ~ rho))) +
  xlab("mean population size") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(x = POP, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_ribbon(aes(x = POP, ymin = ylower, ymax = yupper, fill = IsSig), alpha = .3, show.legend = F) +
  scale_color_manual(values = colz) +
  scale_fill_manual(values = colz)
corr_tmax_pop_prec_icen_year_gravity_gam_fit.fixed.effects.pop.effects







# Correlation GAMS (checking without gravity)----
# Annual Cases
correlation_gams <- function(data) {
  tmax_pop_prec_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_pop_spi_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP, bs = "tp") + s(spi, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_pop_prec_icen_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )
  tmax_prec_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_prec_icen_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )

  tmax_pop_prec_icen_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_pop_spi_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP, bs = "tp") + s(spi, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_pop_prec_icen_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )
  tmax_prec_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_prec_icen_year_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(prec, bs = "tp")
      + s(YEAR_FACTOR, bs = "re") + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )


  tmax_pop_prec_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_pop_spi_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP, bs = "tp") + s(spi, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_pop_prec_icen_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )
  tmax_prec_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_prec_icen_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )

  tmax_pop_prec_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_pop_spi_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP, bs = "tp") + s(spi, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_pop_prec_icen_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(POP, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )
  tmax_prec_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re"),
    data = data
  )
  tmax_prec_icen_fit <- gam(
    corr ~ dist_km:PROVINCE_FACTOR
      + s(tmax, bs = "tp") + s(prec, bs = "tp")
      + s(PROVINCE_FACTOR, bs = "re") +
      s(icen, bs = "tp"),
    data = data
  )
  model_fits <- list(
    tmax_pop_prec_year_fit, tmax_pop_spi_year_fit, tmax_pop_prec_icen_year_fit,
    tmax_prec_year_fit, tmax_prec_icen_year_fit, tmax_pop_prec_icen_year_fit, tmax_pop_spi_year_fit, tmax_pop_prec_icen_year_fit, tmax_prec_year_fit,
    tmax_prec_icen_year_fit, tmax_pop_prec_fit, tmax_pop_spi_fit, tmax_pop_prec_icen_fit,
    tmax_prec_fit, tmax_prec_icen_fit, tmax_pop_prec_fit, tmax_pop_spi_fit, tmax_pop_prec_icen_fit,
    tmax_prec_fit, tmax_prec_icen_fit
  )

  texreg_model_fits <- lapply(model_fits, function(x) texreg::extract(x))

  model_names <- c(
    "tmax_pop_prec_year_fit", "tmax_pop_spi_year_fit", "tmax_pop_prec_icen_year_fit",
    "tmax_prec_year_fit", "tmax_prec_icen_year_fit", "tmax_pop_prec_icen_year_fit", "tmax_pop_spi_year_fit",
    "tmax_pop_prec_icen_year_fit", "tmax_prec_year_fit",
    "tmax_prec_icen_year_fit", "tmax_pop_prec_fit", "tmax_pop_spi_fit", "tmax_pop_prec_icen_fit",
    "tmax_prec_fit", "tmax_prec_icen_fit", "tmax_pop_prec_fit", "tmax_pop_spi_fit", "tmax_pop_prec_icen_fit",
    "tmax_prec_fit", "tmax_prec_icen_fit"
  )
  aic_vals <- c(
    tmax_pop_prec_year_fit$aic, tmax_pop_spi_year_fit$aic, tmax_pop_prec_icen_year_fit$aic,
    tmax_prec_year_fit$aic, tmax_prec_icen_year_fit$aic, tmax_pop_prec_icen_year_fit$aic, tmax_pop_spi_year_fit$aic, tmax_pop_prec_icen_year_fit$aic, tmax_prec_year_fit$aic,
    tmax_prec_icen_year_fit$aic, tmax_pop_prec_fit$aic, tmax_pop_spi_fit$aic, tmax_pop_prec_icen_fit$aic,
    tmax_prec_fit$aic, tmax_prec_icen_fit$aic, tmax_pop_prec_fit$aic, tmax_pop_spi_fit$aic, tmax_pop_prec_icen_fit$aic,
    tmax_prec_fit$aic, tmax_prec_icen_fit$aic
  )

  aicc_vals <- c(
    AICc(tmax_pop_prec_year_fit), AICc(tmax_pop_spi_year_fit), AICc(tmax_pop_prec_icen_year_fit),
    AICc(tmax_prec_year_fit), AICc(tmax_prec_icen_year_fit),
    AICc(tmax_pop_prec_icen_year_fit), AICc(tmax_pop_spi_year_fit),
    AICc(tmax_pop_prec_icen_year_fit), AICc(tmax_prec_year_fit),
    AICc(tmax_prec_icen_year_fit), AICc(tmax_pop_prec_fit), AICc(tmax_pop_spi_fit), AICc(tmax_pop_prec_icen_fit),
    AICc(tmax_prec_fit), AICc(tmax_prec_icen_fit), AICc(tmax_pop_prec_fit), AICc(tmax_pop_spi_fit), AICc(tmax_pop_prec_icen_fit),
    AICc(tmax_prec_fit), AICc(tmax_prec_icen_fit)
  )

  bic_vals <- c(
    BIC(tmax_pop_prec_year_fit), BIC(tmax_pop_spi_year_fit), BIC(tmax_pop_prec_icen_year_fit),
    BIC(tmax_prec_year_fit), BIC(tmax_prec_icen_year_fit),
    BIC(tmax_pop_prec_icen_year_fit), BIC(tmax_pop_spi_year_fit),
    BIC(tmax_pop_prec_icen_year_fit), BIC(tmax_prec_year_fit),
    BIC(tmax_prec_icen_year_fit), BIC(tmax_pop_prec_fit), BIC(tmax_pop_spi_fit), BIC(tmax_pop_prec_icen_fit),
    BIC(tmax_prec_fit), BIC(tmax_prec_icen_fit), BIC(tmax_pop_prec_fit), BIC(tmax_pop_spi_fit), BIC(tmax_pop_prec_icen_fit),
    BIC(tmax_prec_fit), BIC(tmax_prec_icen_fit)
  )

  gcv_vals <- c(
    tmax_pop_prec_year_fit$gcv.ubre, tmax_pop_spi_year_fit$gcv.ubre, tmax_pop_prec_icen_year_fit$gcv.ubre,
    tmax_prec_year_fit$gcv.ubre, tmax_prec_icen_year_fit$gcv.ubre, tmax_pop_prec_icen_year_fit$gcv.ubre, tmax_pop_spi_year_fit$gcv.ubre, tmax_pop_prec_icen_year_fit$gcv.ubre, tmax_prec_year_fit$gcv.ubre,
    tmax_prec_icen_year_fit$gcv.ubre, tmax_pop_prec_fit$gcv.ubre, tmax_pop_spi_fit$gcv.ubre, tmax_pop_prec_icen_fit$gcv.ubre,
    tmax_prec_fit$gcv.ubre, tmax_prec_icen_fit$gcv.ubre, tmax_pop_prec_fit$gcv.ubre, tmax_pop_spi_fit$gcv.ubre, tmax_pop_prec_icen_fit$gcv.ubre,
    tmax_prec_fit$gcv.ubre, tmax_prec_icen_fit$gcv.ubre
  )
  model_summary <- data.table(
    MODEL = model_names, AIC = aic_vals, BIC = bic_vals, gcv = gcv_vals,
    aicc = aicc_vals,
    MODEL_NUMBER = seq(1, length(model_names), by = 1)
  )
  return(list(model_fits, model_summary))
}
correlation_gams_output <- correlation_gams(annual.case.pearsons.df2)
saveRDS(correlation_gams_output,
  file = file.path(
    peru.province.wavelet.out.dir,
    "correlation_gams_output.RDS"
  )
)
correlation_gams_models <- correlation_gams_output[[1]]
saveRDS(correlation_gams_models,
  file = file.path(
    peru.province.wavelet.out.dir,
    "correlation_gams_models.RDS"
  )
)
correlation_gams_summary <- correlation_gams_output[[2]]
correlation_gams_summary[, MOD_TYPE_NUM := paste0("Non_Gravity_", MODEL_NUMBER)]
correlation_gams_summary[order(gcv, decreasing = FALSE), ]
correlation_gams_summary[order(AIC, decreasing = FALSE), ]
correlation_gams_summary[order(BIC, decreasing = FALSE), ]
correlation_gams_summary[order(aicc, decreasing = FALSE), ]

ovr_correlation_gams_summary <- rbind(correlation_gravity_gams_summary, correlation_gams_summary)
ovr_correlation_gams_summary[order(gcv, decreasing = FALSE), ]
ovr_correlation_gams_summary[order(AIC, decreasing = FALSE), ]
ovr_correlation_gams_summary[order(BIC, decreasing = FALSE), ]
ovr_correlation_gams_summary[order(aicc, decreasing = FALSE), ]

# corr_tmax_pop_prec_year_gam_fit ----
corr_tmin_pop_prec_year_gam_fit <- correlation_gams_models[[6]]
corr_tmin_pop_prec_year_gam_fit.year.df <- get_partial_effects(fit = corr_tmin_pop_prec_year_gam_fit, var = "YEAR_FACTOR")
corr_tmin_pop_prec_year_gam_fit.year.df <- corr_tmin_pop_prec_year_gam_fit.year.df[[1]]
corr_tmin_pop_prec_year_gam_fit.prov.df <- get_partial_effects(fit = corr_tmin_pop_prec_year_gam_fit, var = "PROVINCE_FACTOR")
corr_tmin_pop_prec_year_gam_fit.prov.df <- corr_tmin_pop_prec_year_gam_fit.prov.df[[1]]

corr_tmin_pop_prec_year_gam_fit.out.tmin <- get_partial_effects(fit = corr_tmin_pop_prec_year_gam_fit, var = "tmin")
corr_tmin_pop_prec_year_gam_fit.tmin.df <- cbind.data.frame(
  tmin = corr_tmin_pop_prec_year_gam_fit.out.tmin$effects$tmin, y = corr_tmin_pop_prec_year_gam_fit.out.tmin$effects$y,
  ylower = (corr_tmin_pop_prec_year_gam_fit.out.tmin$effects$y - 1.96 * corr_tmin_pop_prec_year_gam_fit.out.tmin$effects$se),
  yupper = (corr_tmin_pop_prec_year_gam_fit.out.tmin$effects$y + 1.96 * corr_tmin_pop_prec_year_gam_fit.out.tmin$effects$se)
)

# corr_tmin_pop_prec_year_gam_fit.tmin.df <- cbind.data.frame(tmin=corr_tmin_pop_prec_year_gam_fit.outtmin$x, y=corr_tmin_pop_prec_year_gam_fit.outtmin$fit, ylower = (corr_tmin_pop_prec_year_gam_fit.outtmin$fit - 1.96*corr_tmin_pop_prec_year_gam_fit.outtmin$se),  yupper = (corr_tmin_pop_prec_year_gam_fit.outtmin$fit + 1.96*corr_tmin_pop_prec_year_gam_fit.outtmin$se))
corr_tmin_pop_prec_year_gam_fit.tmin.df$IsSig <- "Pos"
# corr_tmin_pop_prec_year_gam_fit.tmin.df$tmin_C <- 10^(corr_tmin_pop_prec_year_gam_fit.tmin.df$tmin)

corr_tmin_pop_prec_year_gam_fit.out.prec <- get_partial_effects(fit = corr_tmin_pop_prec_year_gam_fit, var = "prec")
corr_tmin_pop_prec_year_gam_fit.prec.df <- cbind.data.frame(
  prec = corr_tmin_pop_prec_year_gam_fit.out.prec$effects$prec, y = corr_tmin_pop_prec_year_gam_fit.out.prec$effects$y,
  ylower = (corr_tmin_pop_prec_year_gam_fit.out.prec$effects$y - 1.96 * corr_tmin_pop_prec_year_gam_fit.out.prec$effects$se),
  yupper = (corr_tmin_pop_prec_year_gam_fit.out.prec$effects$y + 1.96 * corr_tmin_pop_prec_year_gam_fit.out.prec$effects$se)
)
corr_tmin_pop_prec_year_gam_fit.prec.df$IsSig <- "Pos"

corr_tmin_pop_prec_year_gam_fit.out.pop <- get_partial_effects(fit = corr_tmin_pop_prec_year_gam_fit, var = "POP")
corr_tmin_pop_prec_year_gam_fit.pop.df <- cbind.data.frame(
  POP = corr_tmin_pop_prec_year_gam_fit.out.pop$effects$POP, y = corr_tmin_pop_prec_year_gam_fit.out.pop$effects$y,
  ylower = (corr_tmin_pop_prec_year_gam_fit.out.pop$effects$y - 1.96 * corr_tmin_pop_prec_year_gam_fit.out.pop$effects$se),
  yupper = (corr_tmin_pop_prec_year_gam_fit.out.pop$effects$y + 1.96 * corr_tmin_pop_prec_year_gam_fit.out.pop$effects$se)
)
corr_tmin_pop_prec_year_gam_fit.pop.df$IsSig <- "Pos"

# and the fixed effects
out <- summary(corr_tmin_pop_prec_year_gam_fit)
corr_tmin_pop_prec_year_gam_fit.fixed.df <- cbind.data.frame(PROVINCE_FACTOR = c("intercept", sort(as.character(unique(annual.case.pearsons.df2$PROVINCE_FACTOR)))), y = out$p.coeff, ylower = (out$p.coeff - 1.96 * out$p.table[, 2]), yupper = (out$p.coeff + 1.96 * out$p.table[, 2]), pval = out$p.pv)
rownames(corr_tmin_pop_prec_year_gam_fit.fixed.df) <- c()
corr_tmin_pop_prec_year_gam_fit.fixed.df$IsSig <- "NotSig"
corr_tmin_pop_prec_year_gam_fit.fixed.df$IsSig[corr_tmin_pop_prec_year_gam_fit.fixed.df$y > 0 & corr_tmin_pop_prec_year_gam_fit.fixed.df$pval < 0.01] <- "Pos"
corr_tmin_pop_prec_year_gam_fit.fixed.df$IsSig[corr_tmin_pop_prec_year_gam_fit.fixed.df$y < 0 & corr_tmin_pop_prec_year_gam_fit.fixed.df$pval < 0.01] <- "Neg"

corr_tmin_pop_prec_year_gam_fit.year.df$IsSig <- "NotSig"
corr_tmin_pop_prec_year_gam_fit.year.df$IsSig[corr_tmin_pop_prec_year_gam_fit.year.df$IsSignificant == "Yes" & corr_tmin_pop_prec_year_gam_fit.year.df$y > 0] <- "Pos"
corr_tmin_pop_prec_year_gam_fit.year.df$IsSig[corr_tmin_pop_prec_year_gam_fit.year.df$IsSignificant == "Yes" & corr_tmin_pop_prec_year_gam_fit.year.df$y < 0] <- "Neg"

# and plot
colz <- c("Pos" = "red", "Neg" = "blue", "NotSig" = "gray50")

corr_tmin_pop_prec_year_gam_fit.fixed.df <- subset(corr_tmin_pop_prec_year_gam_fit.fixed.df, PROVINCE_FACTOR != "intercept")
corr_tmin_pop_prec_year_gam_fit.fixed.df
corr_tmin_pop_prec_year_gam_fit.fixed.effects <- ggplot(data = corr_tmin_pop_prec_year_gam_fit.fixed.df) +
  theme_bw() +
  coord_flip() +
  theme(panel.grid = element_blank(), axis.title.y = element_blank()) +
  ylab(bquote(atop("fixed effect of province and geographic distance", "interaction on pairwise correlation coefficient," ~ rho))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = PROVINCE_FACTOR, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_linerange(aes(x = PROVINCE_FACTOR, ymin = ylower, ymax = yupper, color = IsSig), show.legend = F) +
  scale_color_manual(values = colz)
corr_tmin_pop_prec_year_gam_fit.fixed.effects

corr_tmin_pop_prec_year_gam_fit.fixed.effects.year.effects <- ggplot(data = corr_tmin_pop_prec_year_gam_fit.year.df) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(), axis.text.x = element_text(angle = 300, vjust = 0)) +
  ylab(bquote(atop("partial effect of year on", "pairwise correlation coefficient," ~ rho))) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = YEAR_FACTOR, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_linerange(aes(x = YEAR_FACTOR, ymin = ylower, ymax = yupper, color = IsSig), show.legend = F) +
  scale_color_manual(values = colz)
corr_tmin_pop_prec_year_gam_fit.fixed.effects.year.effects

corr_tmin_pop_prec_year_gam_fit.fixed.effects.tmin.effects <- ggplot(data = corr_tmin_pop_prec_year_gam_fit.tmin.df) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylab(bquote(atop("partial effect of temperature on", "pairwise correlation coefficient," ~ rho))) +
  xlab(bquote("mean biweekly temperature ("^0 ~ "C)")) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(x = tmin, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_ribbon(aes(x = tmin, ymin = ylower, ymax = yupper, fill = IsSig), alpha = .3, show.legend = F) +
  scale_color_manual(values = colz) +
  scale_fill_manual(values = colz)
corr_tmin_pop_prec_year_gam_fit.fixed.effects.tmin.effects

corr_tmin_pop_prec_year_gam_fit.fixed.effects.prec.effects <- ggplot(data = corr_tmin_pop_prec_year_gam_fit.prec.df) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylab(bquote(atop("partial effect of precipitation on", "pairwise correlation coefficient," ~ rho))) +
  xlab("mean total annual precipitation (mm)") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(x = prec, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_ribbon(aes(x = prec, ymin = ylower, ymax = yupper, fill = IsSig), alpha = .3, show.legend = F) +
  scale_color_manual(values = colz) +
  scale_fill_manual(values = colz)
corr_tmin_pop_prec_year_gam_fit.fixed.effects.prec.effects

corr_tmin_pop_prec_year_gam_fit.pop.df
corr_tmin_pop_prec_year_gam_fit.fixed.effects.pop.effects <- ggplot(data = corr_tmin_pop_prec_year_gam_fit.pop.df) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylab(bquote(atop("partial effect of province population on", "pairwise correlation coefficient," ~ rho))) +
  xlab("mean population size") +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(x = POP, y = y, color = IsSig), show.legend = F, size = 3) +
  geom_ribbon(aes(x = POP, ymin = ylower, ymax = yupper, fill = IsSig), alpha = .3, show.legend = F) +
  scale_color_manual(values = colz) +
  scale_fill_manual(values = colz)
corr_tmin_pop_prec_year_gam_fit.fixed.effects.pop.effects

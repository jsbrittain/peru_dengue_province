#All times; plot provinces by latitude
latitude_monthly_dt <- copy(ptl_province_inla_df)
setkeyv(latitude_monthly_dt, c("latitude", "TIME"))
latitude_monthly_dt[, PROV_IND:= NULL]
tmp <- unique(subset(latitude_monthly_dt, select = c("PROVINCE")))
tmp[, PROV_IND:= seq(1, nrow(tmp), by = 1)]
latitude_monthly_dt <- merge(latitude_monthly_dt, tmp, by = "PROVINCE")
setkeyv(latitude_monthly_dt, c("latitude", "TIME"))
latitude_monthly_dt[, SCALED_DIR:= scale(DIR), by = "PROVINCE"]

ptl_province_inla_df[, YEAR_DECIMAL:= YEAR + (MONTH - 1)/12]

ptl_province_inla_df <- merge(ptl_province_inla_df, 
                              unique(subset(latitude_monthly_dt, select = c("LAT_PROV_IND", "PROVINCE"))),
                              by = c("PROVINCE"))

lines_ptl_province_inla_df <- subset(ptl_province_inla_df, select = c("PROVINCE", "REGION",
                                                                      "MONTH",
                                                                      "DIR", "longitude",
                                                                      "latitude"))
ptl_province_inla_df[, SCALED_DIR:= scale(DIR), by = "PROVINCE"]
ptl_province_inla_df[, LOG_DIR:= log(DIR + 0.01)]

#Plot labels ----
latitude_province_names <- unique(subset(ptl_province_inla_df, 
                                         select = c("LAT_PROV_IND", "PROVINCE")))
latitude_province_names <- latitude_province_names[order(LAT_PROV_IND),]
latitude_province_names <- unique(latitude_province_names$PROVINCE)

longitude_province_names <- unique(subset(ptl_province_inla_df, 
                                          select = c("LONG_PROV_IND", "PROVINCE")))
longitude_province_names <- longitude_province_names[order(LONG_PROV_IND),]
longitude_province_names <- unique(longitude_province_names$PROVINCE)



#Plots of Log DIR -----
monthly_longitude_plot_by_year <- ggplot(ptl_province_inla_df) + 
  # coord_cartesian(xlim=c(2010.4, 2022), expand = F)+
  # scale_x_continuous(breaks = seq(2010, 2022, by = 2))+
  geom_tile(aes(x = MONTH, y = LONG_PROV_IND, fill = LOG_CASES))+
  facet_wrap(YEAR ~. , nrow = 4)+
  theme_bw()+
  scale_fill_viridis_c(name = "Log(Cases+1)")+scale_color_viridis_c(name = "Log(Cases+1)")+
  ylab("")+scale_y_discrete(limit = longitude_province_names)+
  scale_x_discrete(limit = month.abb)+
  theme(legend.position = "bottom")+
  xlab("Month")+
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size=18), 
        legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=18),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        # axis.title=element_text(size=18), 
        legend.key.size = unit(1.5, "cm"),
        legend.text=element_text(size=20)+geom_text(size = 20))
monthly_longitude_plot_by_year


monthly_latitude_plot_by_year <- ggplot(ptl_province_inla_df) + 
  # coord_cartesian(xlim=c(2010.4, 2022), expand = F)+
  # scale_x_continuous(breaks = seq(2010, 2022, by = 2))+
  geom_tile(aes(x = MONTH, y = LAT_PROV_IND, fill = LOG_CASES))+
  facet_wrap(YEAR ~. , nrow = 4)+
  theme_bw()+
  scale_fill_viridis_c(name = "Log(Cases+1)")+scale_color_viridis_c(name = "Log(Cases+1)")+
  ylab("")+scale_y_discrete(limit = latitude_province_names)+
  scale_x_discrete(limit = month.abb)+
  theme(legend.position = "bottom")+
  xlab("Month")+
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size=18), 
        legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=18),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        # axis.title=element_text(size=18), 
        legend.key.size = unit(1.5, "cm"),
        legend.text=element_text(size=20)+geom_text(size = 20))

monthly_latitude_plot_by_year


monthly_longitude_plot <- ggplot(ptl_province_inla_df) + 
  coord_cartesian(xlim=c(2010.4, 2022), expand = F)+
  scale_x_continuous(breaks = seq(2010, 2022, by = 2))+
  geom_tile(aes(x = YEAR_DECIMAL, y = LONG_PROV_IND, fill = LOG_CASES))+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_fill_viridis_c(name = "Log(Cases+1)")+scale_color_viridis_c(name = "Log(Cases+1)")+
  xlab("Year")+
  ylab("Province")+scale_y_discrete(limit = longitude_province_names)+
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size=18), 
        legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        # axis.title=element_text(size=18), 
        legend.key.size = unit(2.5, "cm"),
        legend.text=element_text(size=20)+geom_text(size = 20))
monthly_longitude_plot


monthly_latitude_plot <- ggplot(ptl_province_inla_df) + 
  coord_cartesian(xlim=c(2010.4, 2022), expand = F)+
  scale_x_continuous(breaks = seq(2010, 2022, by = 2))+
  geom_tile(aes(x = YEAR_DECIMAL, y = LAT_PROV_IND, fill = LOG_CASES))+
  theme(legend.position = "bottom")+
  scale_fill_viridis_c(name = "Log(Cases+1)")+scale_color_viridis_c(name = "Log(Cases+1)")+
  xlab("Year")+
  ylab("Longitude")+scale_y_discrete(limit = latitude_province_names)+
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size=18), 
        legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        # axis.title=element_text(size=18), 
        legend.key.size = unit(2.5, "cm"),
        legend.text=element_text(size=20)+geom_text(size = 20))
monthly_latitude_plot



#Plotting finished ----
# +
#   scale_x_continuous(breaks = seq(1, max(ptl_pro), by = 1))
monthly_longitude_plot
ptl_province_inla_df
mean_monthly_long_lat_dt <- 
  ptl_province_inla_df[,list(DIR = mean(DIR, na.rm = TRUE),
                             LOG_CASES = mean(LOG_CASES, na.rm = TRUE),
                             tmax = mean(tmax, na.rm = TRUE),
                             tmin = mean(tmin, na.rm = TRUE),
                             prec = mean(prec, na.rm = TRUE),
                             tmax_roll_2 = mean(tmax_roll_2, na.rm = TRUE),
                             tmin_roll_2 = mean(tmin_roll_2, na.rm = TRUE),
                             prec_roll_2 = mean(prec_roll_2, na.rm = TRUE),
                             spi = mean(SPI_6, na.rm = TRUE),
                             icen = mean(E_INDEX, na.rm = TRUE)),
                       by = c("PROVINCE", "REGION",
                              "MONTH","longitude",
                              "latitude", "LAT_PROV_IND", "LONG_PROV_IND")]
mean_monthly_longitude_plot <- ggplot(mean_monthly_long_lat_dt) + 
  geom_tile(aes(x = MONTH, y = LONG_PROV_IND, fill = LOG_CASES))+
  theme_bw()+
  scale_x_discrete(limit = month.abb)+
  scale_fill_viridis_c(name = "Log(Cases+1)")+scale_color_viridis_c(name = "Log(Cases+1)")+
  ylab("Province")+  xlab("Month")  + theme(legend.key.size = unit(2.5, "cm"))+ 
  scale_y_discrete(limit = longitude_province_names)+
  theme(text = element_text(size = 22),
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        axis.title=element_text(size=22), 
        legend.text=element_text(size=22)+geom_text(size = 22),
        legend.position = "none")
mean_monthly_longitude_plot #West to East? #PLOT FOR SLIDES

mean_monthly_latitude_plot <- ggplot(mean_monthly_long_lat_dt) + 
  geom_tile(aes(x = MONTH, y = LAT_PROV_IND, fill = LOG_CASES))+
  theme_bw()+
  scale_fill_viridis_c(name = "Log(Cases+1)")+scale_color_viridis_c(name = "Log(Cases+1)")+
  ylab("Province")+  xlab("Month") +  theme(legend.key.size = unit(2.5, "cm"))+
  scale_y_discrete(limit = latitude_province_names)+
  scale_x_discrete(limit = month.abb)+
  theme(text = element_text(size = 22),
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        axis.title=element_text(size=22), 
        legend.text=element_text(size=22)+geom_text(size = 22),
        legend.position = "none")
mean_monthly_latitude_plot# South to north? #PLOT FOR SLIDES

grid.arrange( mean_monthly_longitude_plot,mean_monthly_latitude_plot,
             nrow = 1)
mean_monthly_plot_grid <- 
  ggarrange(plotlist = list(mean_monthly_longitude_plot,mean_monthly_latitude_plot), 
            nrow = 1, ncol = 2, legend = "bottom",
            common.legend = TRUE) + theme(legend.key.size = unit(2.5, "cm"))

mean_monthly_plot_grid
ggsave(mean_monthly_plot_grid,
       file = file.path(peru.province.out.dir,
                        "mean_monthly_plot_grid_new.pdf"),
       h = 12, w = 26)
ggsave(mean_monthly_plot_grid,
       file = file.path(peru.province.out.dir,
                        "mean_monthly_plot_grid.png"),
       h = 12, w = 26)


cross.corr <- function(df){
  out.df <- cbind.data.frame(YEAR=unique(df$YEAR), corr=cor(x = df$DIR_PROV_IN_Q, y = df$DIR_PROV_OTHER))
  return(out.df)
  
}
cross.corr.cycle <- function(df){
  out.df <- cbind.data.frame(YEAR=unique(df$YEAR), corr=cor(df$annual_cycle_this_prov, df$annual_cycle_other_prov))
  return(out.df)
  
}
cross.corr.multi <- function(df){
  out.df <- cbind.data.frame(YEAR_range=paste0(min(df$YEAR),"-" , max(df$YEAR)), mid_YEAR=(min(df$YEAR) +((max(df$YEAR)-min(df$YEAR))/2)), corr=cor(df$multi_this_prov, df$multi_other_prov))
  return(out.df)
  
}
assess.corr.annual <- function(df2, df1){
  #set aside prior data
  df1.hold = df1
  df2.hold = df2
  
  #first, join the two series for the full length that they match up
  df1 <- dplyr::select(df1, TIME, YEAR, DIR)
  df2 <- dplyr::select(df2, TIME, YEAR, DIR)
  names(df1) <- c("TIME", "YEAR", "DIR_PROV_IN_Q")
  names(df2) <- c("TIME", "YEAR", "DIR_PROV_OTHER")
  df.join <- merge(df1,df2, by = c("TIME", "YEAR"))
  
  #now get the full TIME series correlation
  full.corr <- cor(df.join$DIR_PROV_IN_Q, df.join$DIR_PROV_OTHER)
  
  #then, split both by YEAR and look within
  df.YEAR.split <- dlply(df.join, .(YEAR))
  print(length(df.YEAR.split$DIR_PROV_IN_Q))
  print(length(df.YEAR.split$DIR_PROV_OTHER))
  
  df.YEAR.out <- lapply(df.YEAR.split, cross.corr)
  df.YEAR.out <- data.table::rbindlist(df.YEAR.out)
  #and assess cross corr within each YEAR
  
  # now, add in the identifying details
  df.YEAR.out$PROVINCE = unique(df1.hold$PROVINCE)
  df.YEAR.out$comp_prov <- unique(df2.hold$PROVINCE)
  df.YEAR.out$full_ts_corr <- full.corr
  #and add in distance between these two
  df.YEAR.out$dist_m <- distm(c(unique(df1.hold$longitude), unique(df1.hold$latitude)), c(unique(df2.hold$longitude), unique(df2.hold$latitude)), fun = distHaversine)
  df.YEAR.out$dist_km <- df.YEAR.out$dist_m/1000
  df.YEAR.out$dist_from_PP_m <- distm(c(unique(df1.hold$longitude), unique(df1.hold$latitude)), c(104.8397, 11.58035), fun = distHaversine)
  df.YEAR.out$dist_from_PP_km <- df.YEAR.out$dist_from_PP_m/1000
  
  return(df.YEAR.out)
}

assess.corr.annual.cycle <- function(df2, df1){
  #set aside prior data
  df1.hold = df1
  df2.hold = df2
  
  #first, join the two series for the full length that they match up
  df1 <- dplyr::select(df1, TIME, YEAR, reconstructed_annual_period)
  df2 <- dplyr::select(df2, TIME, YEAR, reconstructed_annual_period)
  names(df1) <- c("TIME", "YEAR", "annual_cycle_this_prov")
  names(df2) <- c("TIME", "YEAR", "annual_cycle_other_prov")
  df.join <- merge(df1,df2, by = c("TIME", "YEAR"))
  
  #now get the full TIME series correlation
  full.corr <- cor(df.join$annual_cycle_this_prov, df.join$annual_cycle_other_prov)
  
  #then, split both by YEAR and look within
  df.YEAR.split <- dlply(df.join, .(YEAR))
  
  df.YEAR.out <- lapply(df.YEAR.split, cross.corr.cycle)
  df.YEAR.out <- data.table::rbindlist(df.YEAR.out)
  #and assess cross corr within each YEAR
  
  # now, add in the identifying details
  df.YEAR.out$PROVINCE = unique(df1.hold$PROVINCE)
  df.YEAR.out$comp_prov <- unique(df2.hold$PROVINCE)
  df.YEAR.out$full_ts_corr <- full.corr
  #and add in distance between these two
  df.YEAR.out$dist_m <- distm(c(unique(df1.hold$longitude), unique(df1.hold$latitude)), c(unique(df2.hold$longitude), unique(df2.hold$latitude)), fun = distHaversine)
  df.YEAR.out$dist_km <- df.YEAR.out$dist_m/1000
  df.YEAR.out$dist_from_PP_m <- distm(c(unique(df1.hold$longitude), unique(df1.hold$latitude)), c(104.8397, 11.58035), fun = distHaversine)
  df.YEAR.out$dist_from_PP_km <- df.YEAR.out$dist_from_PP_m/1000
  
  return(df.YEAR.out)
}
assess.corr.5YEAR <- function(df2, df1){
  #set aside prior data
  df1.hold = df1
  df2.hold = df2
  
  #first, join the two series for the full length that they match up
  df1 <- dplyr::select(df1, TIME, YEAR, reconstructed_multi_period)
  df2 <- dplyr::select(df2, TIME, YEAR, reconstructed_multi_period)
  names(df1) <- c("TIME", "YEAR", "multi_this_prov")
  names(df2) <- c("TIME", "YEAR", "multi_other_prov")
  df.join <- merge(df1,df2, by = c("TIME", "YEAR"))
  
  #now get the full TIME series correlation
  full.corr <- cor(df.join$multi_this_prov, df.join$multi_other_prov)
  
  #then, split both by 5 YEAR moving TIME windows and look within each one
  df.YEAR.split <- list()
  for(i in 1:(length(unique(df.join$YEAR))-4)){
    index.end = i + 4
    YEAR.vect = unique(df.join$YEAR)
    sub.dat = subset(df.join, YEAR>=YEAR.vect[i] & YEAR<= YEAR.vect[index.end])
    df.YEAR.split[[i]] <- sub.dat
  }
  
  
  df.YEAR.out <- lapply(df.YEAR.split, cross.corr.multi)
  df.YEAR.out <- data.table::rbindlist(df.YEAR.out)
  #and assess cross corr within each YEAR
  
  # now, add in the identifying details
  df.YEAR.out$PROVINCE = unique(df1.hold$PROVINCE)
  df.YEAR.out$comp_prov <- unique(df2.hold$PROVINCE)
  df.YEAR.out$full_ts_corr <- full.corr

  df.YEAR.out$dist_m <- distm(c(unique(df1.hold$longitude), unique(df1.hold$latitude)), c(unique(df2.hold$longitude), unique(df2.hold$latitude)), fun = distHaversine)
  df.YEAR.out$dist_km <- df.YEAR.out$dist_m/1000
  df.YEAR.out$dist_from_PP_m <- distm(c(unique(df1.hold$longitude), unique(df1.hold$latitude)), c(104.8397, 11.58035), fun = distHaversine)
  df.YEAR.out$dist_from_PP_km <- df.YEAR.out$dist_from_PP_m/1000
  
  return(df.YEAR.out)
}
assess.cross.corr <- function(PROVINCE1, df.all, cycle_type){
  df.other <- subset(df.all, PROVINCE!=PROVINCE1)
  
  #now split other by province 
  df.other <- arrange(df.other, PROVINCE, TIME)
  df.split <- dlply(df.other, .(PROVINCE))
  
  df.now = subset(df.all, PROVINCE==PROVINCE1)
  df.now <- arrange(df.now, PROVINCE, TIME)
  
  #and apply other function across this list of provinces to compare
  if(cycle_type=="annual_case"){
    out.provs.list <- lapply(df.split, assess.corr.annual, df1 = df.now)  
  }else if(cycle_type=="annual_cycle"){
    out.provs.list <- lapply(df.split, assess.corr.annual.cycle, df1 = df.now)  
  }else if(cycle_type=="multi"){
    out.provs.list <- lapply(df.split, assess.corr.5YEAR, df1 = df.now)  
  }
  
  out.provs.df <- data.table::rbindlist(out.provs.list)
  
  #ggplot(out.provs.df) + geom_line(aes(x=YEAR, y = corr, color = comp_prov)) + geom_line(aes(x=YEAR, y=full_ts_corr, color=comp_prov),size=1) + facet_wrap(~comp_prov)
  return(out.provs.df) 
}

#now, split the provinces and run for all
province.list <- as.list(unique(ptl_province_inla_df$PROVINCE))

#Annual case -----
annual.case.out.pearsons <- lapply(province.list, assess.cross.corr, df.all=ptl_province_inla_df, cycle_type = "annual_case")
annual.case.pearsons.df <- data.table::rbindlist(annual.case.out.pearsons)
annual.case.pearsons.df <- merge(annual.case.pearsons.df, 
                                 unique(subset(ptl_province_inla_df, 
                                               select = c("PROVINCE", "REGION",
                                                          "LONG_PROV_IND", "LAT_PROV_IND"))),
                                 by = "PROVINCE")
annual.case.pearsons.df <- merge(annual.case.pearsons.df,
      unique(subset(monthly_province_peru_cases, select = c("PROVINCE", "PROPN_URBAN_2017"))),
      by = "PROVINCE")
province.mean.case.pearsons.df <- annual.case.pearsons.df[, list(MEAN_CORR = mean(corr, na.rm = TRUE),
                                                            PROPN_URBAN_2017 = mean(PROPN_URBAN_2017)),
                                                     by = c("LAT_PROV_IND", "LONG_PROV_IND",
                                                            "PROVINCE")]
yearly_summary_ptl_province_inla_df <- 
  ptl_province_inla_df[, list(POP = mean(POP),
                              POP_DENSITY = mean(POP_DENSITY),
                       tmax = mean(tmax),
                       tmax_roll_2 = mean(tmax_roll_2),
                       tmin = mean(tmin),
                       tmin_roll_2 = mean(tmin_roll_2),
                       prec = mean(prec),
                       sum_prec = sum(prec),
                       prec_roll_2 = mean(prec_roll_2),
                       spi = mean(SPI_6), 
                       icen = mean(E_INDEX),
                       DIR = mean(DIR),
                       oni = mean(ANOM)), by = c("PROVINCE", "YEAR")]
annual.case.pearsons.df <- merge(annual.case.pearsons.df, yearly_summary_ptl_province_inla_df, by = c("PROVINCE", "YEAR"))
annual.case.pearsons.df[, YEAR_FACTOR:= as.factor(YEAR)]
annual.case.pearsons.df[, PROVINCE_FACTOR:= as.factor(PROVINCE)]
annual.case.pearsons.df2 <- annual.case.pearsons.df[which(!is.na(corr)),]
annual.case.pearsons.df2[, YEAR_FACTOR:= as.factor(YEAR)]
annual.case.pearsons.df2[, PROVINCE_FACTOR:= as.factor(PROVINCE)]
annual.case.pearsons.df2[which(YEAR == 2017), list(corr = mean(corr),
                                                  DIR = mean(DIR)), by = "PROVINCE"]
avg.annual.case.pearsons.df2.by.year <- 
  subset(annual.case.pearsons.df2, select = c("YEAR", "YEAR_FACTOR", "DIR", "corr"))
p1 <- ggplot(avg.annual.case.pearsons.df2.by.year)+
  geom_boxplot(aes(x = YEAR_FACTOR, y = corr, fill = corr))+theme_bw()+scale_fill_viridis()
p2 <- ggplot(avg.annual.case.pearsons.df2.by.year)+
  geom_boxplot(aes(x = YEAR_FACTOR, y = DIR, fill = DIR))+theme_bw()+scale_fill_viridis()
grid.arrange(p1, p2, nrow = 2)
annual.case.pearsons.df2
ptl_province_inla_df[, HIGH_URBAN:= as.factor(ifelse(PROPN_URBAN_2017 >= 0.6, 1, 0))]
summary_ptl_province_inla_df_urban_rural_dir_dt <- 
  ptl_province_inla_df[, list(MEDIAN = median(DIR),
                              MIN = quantile(DIR, 0.25),
                              MAX = quantile(DIR, 0.75)),
                       by = c("YEAR_DECIMAL", "HIGH_URBAN", "MONTH", "YEAR")]


pairwise_average_annual.case.pearsons.df2 <- 
  annual.case.pearsons.df2[, list(median_corr = median(corr, na.rm =TRUE),
                                  dist_km = unique(dist_km)), 
                           by = c("PROVINCE", "comp_prov")]
pairwise_average_annual.case.pearsons.df2

annual.case_corr_betw_dist_and_correlation <- 
  pairwise_average_annual.case.pearsons.df2[, 
                                            list(annual.case_corr_betw_dist_and_correlation = 
                                                   cor(dist_km, median_corr)), by = "PROVINCE"]
annual.case_corr_betw_dist_and_correlation
ptl_province_peru_dt <- merge(ptl_province_peru_dt,annual.case_corr_betw_dist_and_correlation,
                              by.x = "PROVINCIA", by.y = "PROVINCE")
ptl_province_peru_map_annual.case_corr_betw_dist_and_correlation <- 
  ggplot(ptl_province_peru_dt, aes(geometry = geometry))+
  geom_sf(aes(fill = annual.case_corr_betw_dist_and_correlation), alpha = 0.7)+theme_bw()+
  theme(legend.position = "bottom")+xlab("Longitude") + ylab("Latitude")+
  geom_text(data=ptl_province_peru_dt, aes(coords_x, coords_y, group=NULL, label=PROVINCIA), size=4.5) +
  scale_fill_viridis_c(name = "Correlation")+
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size=28),
        axis.text.y = element_text(size=28),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        axis.title=element_text(size=28), 
        legend.text=element_text(size=28)+geom_text(size = 28),
        legend.position = "bottom", legend.key.size = unit(2, 'cm'))
ptl_province_peru_map_annual.case_corr_betw_dist_and_correlation

#Some GAMs ----

yearly_pearsons_dt <- annual.case.pearsons.df[, list(corr = mean(corr, na.rm = TRUE)), by = "YEAR"]
yearly_dirs_dt <- ptl_province_inla_df[, list(DIR = mean(DIR)), by = "YEAR"]
yearly_pearsons_dt <- merge(yearly_pearsons_dt, yearly_dirs_dt, by = "YEAR")
yearly_pearsons_dt[, cor(corr, DIR)] 
yearly_pearsons_dt
ggplot(yearly_pearsons_dt, aes(x = YEAR, y = corr)) + geom_line()
# p1 <- ggplot(yearly_pearsons_dt, aes(x = YEAR, DIR)) + geom_line()+
#   theme_bw()
# 
# 140/2.5
# annual.case.pearsons.df <- merge(annual.case.pearsons.df, 
#       subset(ptl_province_peru_dt, 
#              select = c("PROVINCIA","coords_x", "coords_y")),
#       by.x = "PROVINCE", by.y = "PROVINCIA")
# setnames(annual.case.pearsons.df, c("coords_x", "coords_y"), c("longitude", "latitude"))
# 
# longitude_annual.case.pearsons.df <- copy(annual.case.pearsons.df)
# setkeyv(longitude_annual.case.pearsons.df, c("longitude"))
# tmp <- unique(subset(longitude_annual.case.pearsons.df, select = c("PROVINCE")))
# tmp[, PROV_IND:= seq(1, nrow(tmp), by = 1)]
# longitude_annual.case.pearsons.df <- merge(longitude_annual.case.pearsons.df, tmp, by = "PROVINCE")
# longitude_annual.case.pearsons.df
yearly_mean_pearsons_dt <- annual.case.pearsons.df[, list(corr = mean(corr, na.rm = TRUE)), by = c("YEAR", "PROVINCE")]
yearly_mean_dirs_dt <- ptl_province_inla_df[, list(DIR = mean(DIR)), by = c("YEAR", "PROVINCE")]
yearly_mean_pearsons_dt <- merge(yearly_mean_pearsons_dt, yearly_mean_dirs_dt, by = c("YEAR", "PROVINCE"))
yearly_mean_pearsons_dt


#Higher DIR, Higher correlation between provinces
mean(yearly_mean_pearsons_dt[which(!is.na(corr)), cor(corr, DIR), by = "PROVINCE"]$V1 )


annual.case.pearsons.df[, cor(full_ts_corr, dist_km), by = "PROVINCE"]
annual.case.pearsons.df[, mean(full_ts_corr), by = "PROVINCE"]

latitude_annual.case.pearsons.df
latitude_annual.case.pearsons.df <- 
  merge(annual.case.pearsons.df,
        unique(subset(ptl_province_inla_df, select = c("PROVINCE", "LAT_PROV_IND"))),
        by = "PROVINCE")
latitude_annual.case.pearsons.df[, mean(corr, na.rm = TRUE), by = "YEAR"]






#PAPER CORRELATION PLOTS ----
lat_plot_annual_case <- ggplot(annual.case.pearsons.df) + 
  geom_tile(aes(x = YEAR, y = LAT_PROV_IND, fill = corr))+
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size=14), 
        legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        # axis.title=element_text(size=18), 
        legend.key.size = unit(2.5, "cm"),
        legend.text=element_text(size=18)+geom_text(size = 18)) +
  scale_fill_viridis_c(limits = c(-0.5, 1),
                       name = "Mean Correlation")+
  scale_color_viridis_c(limits = c(0, 1),
                        name = "Mean Correlation")+
  ylab("Province")+
  scale_y_discrete(limit = latitude_province_names)+
  coord_cartesian(xlim=c(2010.0,2021.0), expand = T)+
  scale_x_continuous(breaks = seq(2010.0, 2022.0, by = 1))
lat_plot_annual_case #Higher latitude, consistently greater average correlation with other provinces

#Latitude Multi Correlongion Tile Plot ----
subset(annual.case.pearsons.df, PROVINCE == "Lambayeque")
long_plot_annual_case <- ggplot(annual.case.pearsons.df) + 
  geom_tile(aes(x = YEAR, y = LONG_PROV_IND, fill = corr))+
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size=14), 
        legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        # axis.title=element_text(size=18), 
        legend.key.size = unit(2.5, "cm"),
        legend.text=element_text(size=18)+geom_text(size = 18)) +
  scale_fill_viridis_c(limits = c(-0.5, 1),
                       name = "Mean Correlation")+
  scale_color_viridis_c(limits = c(0, 1),
                        name = "Mean Correlation")+
  ylab("Province")+
  scale_y_discrete(limit = longitude_province_names)+
  coord_cartesian(xlim=c(2010.0,2021.0), expand = T)+
  scale_x_continuous(breaks = seq(2010.0, 2022.0, by = 1))
long_plot_annual_case #Higher longitude, consistently greater average correlongion with other provinces
grid.arrange(long_plot_annual_case,
             lat_plot_annual_case, 
             nrow = 1)


annual_case_corr_long_lat_tiles <- ggarrange(long_plot_annual_case,
                                             lat_plot_annual_case, 
                                             nrow = 1, common.legend = TRUE,legend = "bottom") + theme(legend.key.size = unit(2.5, "cm"))
ggsave(annual_case_corr_long_lat_tiles, file = file.path(peru.province.out.dir,
                                                         "annual_case_corr_long_lat_tiles.pdf"), h = 12, w = 24)
ggsave(annual_case_corr_long_lat_tiles, file = file.path(peru.province.out.dir,
                                                         "annual_case_corr_long_lat_tiles.png"), h = 12, w = 24)


long_tile_plot_annual_case<- ggplot(annual.case.pearsons.df) + 
  geom_tile(aes(x = YEAR, y = LONG_PROV_IND, fill = corr))+
  theme_bw()+
  scale_fill_viridis_c(limits = c(-0.5, 1))+scale_color_viridis_c(limits = c(-0.5, 1))+
  ylab("Longitude")+scale_y_discrete(limit = longitude_province_names)+
  theme(legend.position = "bottom")
long_tile_plot_annual_case 


annual_case_distribution_dt <- 
  latitude_annual.case.pearsons.df[, list(MEDIAN = median(corr, na.rm = TRUE),
                                          CI_U = quantile(corr, na.rm = TRUE, probs = 0.75),
                                          CI_L = quantile(corr, na.rm = TRUE, probs = 0.25)),
                                   by = "YEAR"]

lat_line_plot_annual_case<- ggplot(annual_case_distribution_dt) + 
  geom_line(aes(x = YEAR, y = MEDIAN))+geom_ribbon(aes(x = YEAR, ymin = CI_L, ymax = CI_U),
                                                   alpha = 0.2, fill = "pink")+
  theme_bw()+
  theme(legend.position = "bottom")
lat_line_plot_annual_case 


annual_case_distribution_dt <- 
  annual.case.pearsons.df[, list(MEDIAN = median(corr, na.rm = TRUE),
                                          MAX = quantile(corr, na.rm = TRUE, probs = 0.75),
                                          MIN = min(corr, na.rm = TRUE)),
                                   by = "YEAR"]

line_plot_annual_case_corr<- ggplot(annual_case_distribution_dt) + 
  geom_line(aes(x = YEAR, y = MEDIAN))+geom_ribbon(aes(x = YEAR, ymin = MIN, ymax = MAX),
                                                   alpha = 0.2, fill = "pink")+
  theme_bw()+
  theme(legend.position = "bottom")
line_plot_annual_case_corr


annual.case.pearsons.df
annual_dir_distribution_dt <- 
  ptl_province_inla_df[, list(MEDIAN = median(DIR, na.rm = TRUE),
                                 MAX = quantile(DIR, na.rm = TRUE, probs = c(0.975)),
                                 MIN = min(DIR, na.rm = TRUE, probs = 0.025)),
                          by = "YEAR"]
annual_dir_distribution_dt
line_plot_annual_dir<- ggplot(annual_dir_distribution_dt) + 
  geom_line(aes(x = YEAR, y = MEDIAN))+
  theme_bw()+
  theme(legend.position = "bottom")
line_plot_annual_dir





#Annual cycle ----
annual.cycle.out.pearsons <- lapply(province.list, assess.cross.corr, df.all=prov.split.df, cycle_type = "annual_cycle")
annual.cycle.pearsons.df <- data.table::rbindlist(annual.cycle.out.pearsons)
annual.cycle.pearsons.df <- merge(annual.cycle.pearsons.df,
      unique(subset(ptl_province_inla_df, 
                    select = c("PROVINCE", "LONG_PROV_IND", "LAT_PROV_IND"))),
      by = "PROVINCE")
annual_cycle_distribution_dt <- 
  annual.cycle.pearsons.df[which(!is.na(corr)), list(MEDIAN = median(corr, na.rm = TRUE),
                                 MAX = quantile(corr, 0.75),
                                 MIN = quantile(corr, 0.25)),
                          by = "YEAR"]


line_plot_annual_cycle<- ggplot(annual_cycle_distribution_dt) + 
  geom_line(aes(x = YEAR, y = MEDIAN), color = "turquoise", linewidth = 1.7)+
  geom_ribbon(aes(x = YEAR, ymin = MIN, ymax = MAX),
              alpha = 0.2, fill = "turquoise")+
  theme_bw()+
  theme(legend.position = "bottom")
line_plot_annual_cycle 

avg_annual.cycle.pearsons.df <- 
  annual.cycle.pearsons.df[, list(corr = mean(corr, na.rm = TRUE)), 
                           by = c("PROVINCE", "YEAR", "LAT_PROV_IND", "LONG_PROV_IND")]
avg_annual.cycle.pearsons.df
lat_plot_annual_cycle <- ggplot(avg_annual.cycle.pearsons.df) + 
  geom_tile(aes(x = YEAR, y = LAT_PROV_IND, fill = corr))+
  theme_bw()+
  scale_fill_viridis_c(limits = c(-0.5, 1))+
  scale_color_viridis_c(limits = c(-0.5, 1))+
  ylab("Latitude")+scale_y_discrete(limit = latitude_province_names)+
  theme(legend.position = "bottom")
lat_plot_annual_cycle

annual.cycle.pearsons.df <- merge(annual.cycle.pearsons.df, yearly_summary_ptl_province_inla_df, by = c("PROVINCE", "YEAR"))
annual.cycle.pearsons.df[, YEAR_FACTOR:= as.factor(YEAR)]
annual.cycle.pearsons.df[, PROVINCE_FACTOR:= as.factor(PROVINCE)]
annual.cycle.pearsons.df2 <- annual.cycle.pearsons.df[which(!is.na(corr)),]
annual.cycle.pearsons.df2[, YEAR_FACTOR:= as.factor(YEAR)]
annual.cycle.pearsons.df2[, PROVINCE_FACTOR:= as.factor(PROVINCE)]



yearly_annual_cycle_pearsons_dt <- annual.cycle.pearsons.df[, list(corr = mean(corr, na.rm = TRUE)), by = "YEAR"]
yearly_dirs_dt <- ptl_province_inla_df[, list(DIR = mean(DIR)), by = "YEAR"]
yearly_annual_cycle_pearsons_dt <- merge(yearly_annual_cycle_pearsons_dt, yearly_dirs_dt, by = "YEAR")
yearly_annual_cycle_pearsons_dt
yearly_annual_cycle_pearsons_dt[which(!is.na(corr)), cor(corr, DIR)] 
yearly_annual_cycle_pearsons_dt




multi.cycle.out.pearsons <- lapply(province.list, assess.cross.corr, df.all=prov.split.df, cycle_type = "multi")
multi.cycle.pearsons.df <- data.table::rbindlist(multi.cycle.out.pearsons)
multi.cycle.pearsons.df[which(!is.na(full_ts_corr)), cor(corr, dist_km), by = c("PROVINCE")]
multi.cycle.pearsons.df[, cor(full_ts_corr, dist_km)]
multi.cycle.pearsons.df <- merge(multi.cycle.pearsons.df,
                                  unique(subset(ptl_province_inla_df, 
                                                select = c("PROVINCE", "LONG_PROV_IND", "LAT_PROV_IND",
                                                           "REGION"))),
                                  by = "PROVINCE")

avg_multi.cycle.pearsons.df <- multi.cycle.pearsons.df[, list(corr = mean(corr, na.rm = TRUE)), 
                                                       by = c("PROVINCE", 
                                                              "REGION",
                                                              "YEAR_range",
                                                              "mid_YEAR",
                                                              "LAT_PROV_IND", "LONG_PROV_IND")]

avg_multi.cycle.pearsons.df$min_year <- as.numeric(sapply(strsplit(avg_multi.cycle.pearsons.df$YEAR_range, "-"), '[', 1))
avg_multi.cycle.pearsons.df$max_year <- as.numeric(sapply(strsplit(avg_multi.cycle.pearsons.df$YEAR_range, "-"), '[', 2))
avg_multi.cycle.pearsons.df$max_year <- avg_multi.cycle.pearsons.df$max_year +1
#and get the two other years in the series
avg_multi.cycle.pearsons.df$year_2 <- avg_multi.cycle.pearsons.df$min_year +1
avg_multi.cycle.pearsons.df$year_4 <- avg_multi.cycle.pearsons.df$mid_year +1





#OVERLAPPING MULTI-CYCLE PLOTS ----
lat_plot_multi_cycle_overlapping <- ggplot(data=avg_multi.cycle.pearsons.df)+
  geom_tile(aes(x=min_year, y=LAT_PROV_IND, fill=corr, color=corr), alpha=.5) +
  geom_tile(aes(x=year_2, y=LAT_PROV_IND, fill=corr, color=corr), alpha=.5) +
  geom_tile(aes(x=mid_YEAR, y=LAT_PROV_IND, fill=corr, color=corr), alpha=.5) +
  geom_tile(aes(x=year_4, y=LAT_PROV_IND, fill=corr, color=corr), alpha=.5) +
  geom_tile(aes(x=max_year, y=LAT_PROV_IND, fill=corr, color=corr), alpha=.5) +
  scale_fill_viridis_c()+scale_color_viridis_c()+
  ylab("Province")+scale_y_discrete(limit = latitude_province_names)+
  theme_bw()+xlab("Year")+
  coord_cartesian(xlim=c(2010, 2022), expand = T)+
  scale_x_continuous(breaks = seq(2010, 2022, by = 2))+
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size=14), 
        legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        # axis.title=element_text(size=18), 
        legend.key.size = unit(2, "cm"),
        legend.text=element_text(size=18)+geom_text(size = 18))


long_plot_multi_cycle_overlapping <- ggplot(data=avg_multi.cycle.pearsons.df)+
  geom_tile(aes(x=min_year, y=LONG_PROV_IND, fill=corr, color=corr), alpha=.5) +
  geom_tile(aes(x=year_2, y=LONG_PROV_IND, fill=corr, color=corr), alpha=.5) +
  geom_tile(aes(x=mid_YEAR, y=LONG_PROV_IND, fill=corr, color=corr), alpha=.5) +
  geom_tile(aes(x=year_4, y=LONG_PROV_IND, fill=corr, color=corr), alpha=.5) +
  geom_tile(aes(x=max_year, y=LONG_PROV_IND, fill=corr, color=corr), alpha=.5) +
  scale_fill_viridis_c(name = "Mean correlation")+scale_color_viridis_c(name = "Mean correlation")+
  theme_bw() + theme(legend.position = "none")+
  coord_cartesian(xlim=c(2010, 2022), expand = T)+
  scale_x_continuous(breaks = seq(2010, 2022, by = 2))+
  ylab("Province")+scale_y_discrete(limit = longitude_province_names)+
  theme_bw()+xlab("Year")+
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size=14), 
        legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        # axis.title=element_text(size=18), 
        legend.key.size = unit(2, "cm"),
        legend.text=element_text(size=18)+geom_text(size = 18))




process_correlations_df <- function(data){
  result <- data[, list(Median = median(V1, na.rm = TRUE),
              Q1 = quantile(V1, probs = 0.25, na.rm = TRUE),
              Q3 = quantile(V1, probs = 0.75, na.rm = TRUE))]
  return(result)
}

median_annual_case_corr <- annual.case.pearsons.df[, list(median(corr, na.rm = TRUE)), by = "PROVINCE"]
median_annual_cycle_corr <- annual.cycle.pearsons.df[, list(median(corr, na.rm = TRUE)), by = "PROVINCE"]
median_multi_cycle_corr <- multi.cycle.pearsons.df[, list(median(corr, na.rm = TRUE)), by = "PROVINCE"]

summary_corrs <- ldply(list(median_annual_case_corr, median_annual_cycle_corr, median_multi_cycle_corr),
       function(x) process_correlations_df(x))
summary_corrs <- data.table(summary_corrs)
summary_corrs[, time_series:= c("Raw Incidence", "Reconstructed Annual Cycle",
                                "Reconstructed Multiannual Cycle")]
setcolorder(summary_corrs, c("time_series", "Median", "Q1", "Q3"))
xtable(summary_corrs)
ptl_province_peru_dt$median_annual_case_corr <- median_annual_case_corr$V1
ptl_province_peru_dt$median_annual_cycle_corr <- median_annual_cycle_corr$V1
ptl_province_peru_dt$median_multi_cycle_corr <- median_multi_cycle_corr$V1

ptl_province_peru_map_median_annual_case_corr <- 
  ggplot(ptl_province_peru_dt, aes(geometry = geometry))+
  geom_sf(aes(fill = median_annual_case_corr), alpha = 0.7)+theme_bw()+
  theme(legend.position = "bottom")+xlab("Longitude") + ylab("Latitude")+
  geom_text(data=ptl_province_peru_dt, aes(coords_x, coords_y, group=NULL, label=PROVINCIA), size=4.5) +
  scale_fill_viridis_c(name = "Median correlation with \n other provinces")+
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size=28),
        axis.text.y = element_text(size=28),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        axis.title=element_text(size=28), 
        legend.text=element_text(size=28)+geom_text(size = 28),
        legend.position = "bottom",legend.key.size = unit(2.5, 'cm'))
ptl_province_peru_map_median_annual_case_corr

ptl_province_peru_map_median_annual_cycle_corr <- 
  ggplot(ptl_province_peru_dt, aes(geometry = geometry))+
  geom_sf(aes(fill = median_annual_cycle_corr), alpha = 0.7)+theme_bw()+
  theme(legend.position = "bottom")+xlab("Longitude") + ylab("Latitude")+
  geom_text(data=ptl_province_peru_dt, aes(coords_x, coords_y, group=NULL, label=PROVINCIA), size=4.5) +
  scale_fill_viridis_c(name = "Median correlation with \n other provinces")+
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size=28),
        axis.text.y = element_text(size=28),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        axis.title=element_text(size=28), 
        legend.text=element_text(size=28)+geom_text(size = 28),
        legend.position = "bottom",legend.key.size = unit(2.5, 'cm'))
ptl_province_peru_map_median_annual_cycle_corr

ptl_province_peru_map_median_multi_cycle_corr <- 
  ggplot(ptl_province_peru_dt, aes(geometry = geometry))+
  geom_sf(aes(fill = median_multi_cycle_corr), alpha = 0.7)+theme_bw()+
  theme(legend.position = "bottom")+xlab("Longitude") + ylab("Latitude")+
  geom_text(data=ptl_province_peru_dt, aes(coords_x, coords_y, group=NULL, label=PROVINCIA), size=4.5) +
  scale_fill_viridis_c(name = "Median correlation with \n other provinces")+
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size=28),
        axis.text.y = element_text(size=28),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        axis.title=element_text(size=28), 
        legend.text=element_text(size=28)+geom_text(size = 28),
        legend.position = "bottom",legend.key.size = unit(2.5, 'cm'))
ptl_province_peru_map_median_multi_cycle_corr

#Overlapping multicycles -----
expand.df <- function(df){
  
  df$min_year <- as.numeric(sapply(strsplit(df$YEAR_range, "-"), '[', 1))
  df$max_year <- as.numeric(sapply(strsplit(df$YEAR_range, "-"), '[', 2))
  df$max_year <- df$max_year +1
  
  year_range = unique(df$min_year):unique(df$max_year)
  
  df.1 <- df
  df.2 <- df
  df.3 <- df
  df.4 <- df
  df.5 <- df
  
  df.1$plot_year <- year_range[1]
  df.2$plot_year <- year_range[2]
  df.3$plot_year <- year_range[3]
  df.4$plot_year <- year_range[4]
  df.5$plot_year <- year_range[5]
  
  df.out <- rbind(df.1, df.2, df.3, df.4, df.5)
  return(df.out)  
}

expanded.multi.cycle.pearsons.df <- data.table::rbindlist(lapply(multi.cycle.out.pearsons, expand.df))
expanded.multi.cycle.pearsons.df <- merge(expanded.multi.cycle.pearsons.df,
                                  unique(subset(ptl_province_inla_df, 
                                                select = c("PROVINCE", 
                                                           "LONG_PROV_IND", 
                                                           "LAT_PROV_IND",
                                                           "REGION"))),
                                  by = "PROVINCE")

#Latitude Multi Correlation Tile Plot ----
subset(expanded.multi.cycle.pearsons.df, PROVINCE == "Lambayeque")
unique(expanded.multi.cycle.pearsons.df$mid_YEAR)
lat_plot_multi_cycle <- ggplot(expanded.multi.cycle.pearsons.df) + 
  geom_tile(aes(x = mid_YEAR, y = LAT_PROV_IND, fill = corr))+
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size=14), 
        legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        # axis.title=element_text(size=18), 
        legend.key.size = unit(2.5, "cm"),
        legend.text=element_text(size=18)+geom_text(size = 18)) +
  scale_fill_viridis_c(limits = c(-0.5, 1),
                       name = "Mean Correlation")+
  scale_color_viridis_c(limits = c(0, 1),
                        name = "Mean Correlation")+
  ylab("Province")+
  scale_y_discrete(limit = latitude_province_names)+
  coord_cartesian(xlim=c(2012.0,2019.0), expand = T)+
  scale_x_continuous(breaks = seq(2012.0, 2019.0, by = 1))
lat_plot_multi_cycle #Higher latitude, consistently greater average correlation with other provinces

#Latitude Multi Correlongion Tile Plot ----
subset(expanded.multi.cycle.pearsons.df, PROVINCE == "Lambayeque")
expanded.multi.cycle.pearsons.df
long_plot_multi_cycle <- ggplot(expanded.multi.cycle.pearsons.df) + 
  geom_tile(aes(x = mid_YEAR, y = LONG_PROV_IND, fill = corr))+
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size=14), 
        legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        # axis.title=element_text(size=18), 
        legend.key.size = unit(2.5, "cm"),
        legend.text=element_text(size=18)+geom_text(size = 18)) +
  scale_fill_viridis_c(limits = c(-0.5, 1),
                       name = "Mean Correlation")+
  scale_color_viridis_c(limits = c(0, 1),
                        name = "Mean Correlation")+
  ylab("Province")+
  scale_y_discrete(limit = longitude_province_names)
long_plot_multi_cycle #Higher longitude, consistently greater average correlongion with other provinces
grid.arrange(long_plot_multi_cycle,
             lat_plot_multi_cycle, 
             nrow = 1)


multi_cycle_corr_long_lat_tiles <- ggarrange(long_plot_multi_cycle,
             lat_plot_multi_cycle, 
             nrow = 1, common.legend = TRUE,legend = "bottom") + theme(legend.key.size = unit(2.5, "cm"))
multi_cycle_corr_long_lat_tiles
ggsave(multi_cycle_corr_long_lat_tiles, file = file.path(peru.province.out.dir,
                                                         "multi_cycle_corr_long_lat_tiles.pdf"), h = 12, w = 22)
ggsave(multi_cycle_corr_long_lat_tiles, file = file.path(peru.province.out.dir,
                                                         "multi_cycle_corr_long_lat_tiles.png"), h = 12, w = 22)

tmp <- unique(subset(multi.cycle.pearsons.df, select = c("full_ts_corr", "dist_km")))
ggplot(tmp) +geom_point(aes(x = dist_km, y = full_ts_corr))
tmp2 <- multi.cycle.pearsons.df[which(dist_km < 60 & full_ts_corr < 0), ]
mean(unique(subset(ptl_province_inla_df, PROVINCE %in% tmp2$PROVINCE))$POP_DENSITY)
mean((subset(ptl_province_inla_df)$POP_DENSITY))

yearly_multi_cycle_pearsons_dt <- multi.cycle.pearsons.df[, list(corr = mean(corr, na.rm = TRUE)), by = "YEAR_range"]

yearly_dirs_dt <- ptl_province_inla_df[, list(DIR = mean(DIR)), by = "YEAR"]

latitude_multi.cycle.pearsons.df <- 
  merge(multi.cycle.pearsons.df,
        unique(subset(ptl_province_inla_df, select = c("PROVINCE", "LAT_PROV_IND"))),
        by = "PROVINCE")
latitude_multi.cycle.pearsons.df[, mean(corr, na.rm = TRUE), by = "YEAR_range"]





multi.cycle_distribution_dt



expanded.multi.cycle.pearsons.df
multi.cycle_distribution_dt <- 
  multi.cycle.pearsons.df[, list(MEDIAN = median(corr, na.rm = TRUE),
                                          MIN = quantile(corr, probs = 0.25),
                                          MAX = quantile(corr, probs = 0.75)),
                                   by = "mid_YEAR"]
multi.cycle_distribution_dt
# multi.cycle_distribution_dt[, IND:= seq(1, nrow(multi.cycle_distribution_dt))]
line_plot_multi.cycle <- ggplot(multi.cycle_distribution_dt) + 
  geom_line(aes(x = mid_YEAR, y = MEDIAN), color = "forestgreen")+
  geom_ribbon(aes(x = mid_YEAR, ymin = MIN, ymax = MAX),
                                                   alpha = 0.2, fill = "forestgreen")+
  theme_bw()+
  scale_y_continuous(breaks = seq(-0.5, 0.9, by = 0.2))+
  theme(legend.position = "bottom")
line_plot_multi.cycle




annual_case_distribution_dt <- 
  annual.case.pearsons.df[which(!is.na(corr)), list(MEDIAN = median(corr, na.rm = TRUE),
                                 MAX = quantile(corr, probs = 0.75),
                                 MIN = quantile(corr, probs = 0.25)),
                          by = "YEAR"]
annual_case_distribution_dt
line_plot_annual_case_corr<- ggplot(annual_case_distribution_dt) + 
  geom_line(aes(x = YEAR, y = MEDIAN), color = "darkgoldenrod1",
            linewidth = 1.5)+
  geom_ribbon(aes(x = YEAR, ymin = MIN, ymax = MAX),
                                                   alpha = 0.2, fill = "darkgoldenrod1")+
  theme_bw()+
  theme(legend.position = "bottom")
line_plot_annual_case_corr

line_plot_annual_cycle<- ggplot(annual_cycle_distribution_dt) + 
  geom_line(aes(x = YEAR, y = MEDIAN), color = "aquamarine2", linewidth = 1.5)+
  geom_ribbon(aes(x = YEAR, ymin = MIN, ymax = MAX),
              alpha = 0.2, fill = "aquamarine2")+
  theme_bw()+
  theme(legend.position = "bottom")
line_plot_annual_cycle 


line_plot_multi.cycle <- ggplot(multi.cycle_distribution_dt) + 
  geom_line(aes(x = mid_YEAR, y = MEDIAN), color = "darkorchid3",
            linewidth = 1.5)+
  geom_ribbon(aes(x = mid_YEAR, ymin = MIN, ymax = MAX),
              alpha = 0.2, fill = "darkorchid3")+
  theme_bw()+
  theme(legend.position = "bottom")
line_plot_multi.cycle
#Paper Plots ----
#Multi-Cycle Plots----
line_plot_annual_cycle




lat_plot_annual_cycle #Higher latitude, consistently greater average correlation with other provinces
long_plot_annual_cycle #Lower longitude, consistently greater average correlongion with other provinces

#Multi-Cycle Plots----
line_plot_multi.cycle
lat_plot_multi_cycle #Higher latitude, consistently greater average correlation with other provinces
long_plot_multi_cycle #Lower longitude, consistently greater average correlongion with other provinces

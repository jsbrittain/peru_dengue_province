#Wavelet analysis focusing on 
  # 1) epidemic synchrony
  # 2) phase coherence

#The following script is an implementation of the work by Brook et Al, 2023 (https://doi.org/10.1101/2022.06.08.2227617)



#Set up of list of data.tables split by provinces
prov.split <- dlply(ptl_province_inla_df, .(PROVINCE))



#Plotting labels ----
#Setting up province names for ordering in plots (by latitude and longitude)
latitude_province_names <- unique(subset(ptl_province_inla_df, 
                                         select = c("LAT_PROV_IND", "PROVINCE")))
latitude_province_names <- latitude_province_names[order(LAT_PROV_IND),]
latitude_province_names <- unique(latitude_province_names$PROVINCE)

longitude_province_names <- unique(subset(ptl_province_inla_df, 
                                          select = c("LONG_PROV_IND", "PROVINCE")))
longitude_province_names <- longitude_province_names[order(LONG_PROV_IND),]
longitude_province_names <- unique(longitude_province_names$PROVINCE)





#1) PHASE COHERENCE ----
#Functions for coherence ----
#Multiannual dengue cycles  (2-12 years)
prov.rank.multi <- function(df2, df1, pval.cut){
  #Storing input data.tables
  
  input_df1 <-  df1
  input_df2 <- df2
  
  #Subset to keep only relevant columns
  df1 <- dplyr::select(df1, TIME, DIR)
  df2 <- dplyr::select(df2, TIME, DIR)
  
  
  #Set names such that we have different names for the focal and other province
  names(df1)[names(df1)=="DIR"] <- "DIR_PROV_IN_Q"
  names(df2)[names(df2)=="DIR"] <- "DIR_PROV_OTHER"
  
  df.merge <- merge(df1, df2, by="TIME", all.x = T)
  
  df.merge <- df.merge[complete.cases(df.merge),]
  df.merge <- arrange(df.merge, TIME)
  
  
  #Using a Morlet wavelet with non-dimensional frequency 𝜔_0 = 6
  
  corr.prov <- analyze.coherency(df.merge, my.pair = c("DIR_PROV_IN_Q","DIR_PROV_OTHER"),
                                 loess.span = 0,
                                 dt = 1/12, dj = 1/100,
                                 window.type.t = 1, window.type.s = 1,
                                 window.size.t = (12*5), #examine coherence year-by-year - here 5 years
                                 window.size.s = (1/4), #periods on the order of 
                                 lowerPeriod = 2, #shortest possible period in years (multi)
                                 upperPeriod = 7, #largest possible period (in weeks; here, 7 years)
                                 make.pval = TRUE, n.sim = 100)

  coherence.mat <- corr.prov$Coherence
  pval.mat <- corr.prov$Coherence.pval
  pval.mat <- pval.mat< pval.cut #Is the p-value less than chosen p-value significance cut-off
  
  out.mat <- coherence.mat*pval.mat
  out.mat[out.mat==0] <- NA #Only keeping statistically significant
  
  province_in_q <- sub(" ", "_", unique(input_df2$PROVINCE), fixed = T)
  
  
  #Average over coherence of multiannual cycles of differing lengths
  df.merge$coherence_multi <- colMeans(out.mat, na.rm = T)
  
  tmp_input_df2 <- dplyr::select(df.merge, TIME, coherence_multi)
  
  input_df1 <- merge(input_df1, tmp_input_df2, by="TIME", all.x = T)
  
  input_df1$coherence_prov <- province_in_q
  
  df.out <- dplyr::select(input_df1, TIME, coherence_multi, coherence_prov)
  df.out <- arrange(df.out, TIME)

  #add in cross wavelet power
  power.mat <- corr.prov$Power.xy
  pval.mat <- corr.prov$Power.xy.pval
  pval.mat <- pval.mat< pval.cut
  
  power.mat <- power.mat*pval.mat
  power.mat[power.mat==0] <- NA
  
  df.out$cross_wave_power_multi <- colMeans(power.mat, na.rm=T)
  return(df.out)
}


#Annual dengue cycles (6 months to 2 years)
prov.rank.annual <- function(df2, df1, pval.cut){
  
  #Storing input
  df1a <-  df1
  df2a <- df2
  df1 <- dplyr::select(df1, TIME, DIR)
  df2 <- dplyr::select(df2, TIME, DIR)
  
  names(df1)[names(df1)=="DIR"] <- "DIR_PROV_IN_Q"
  names(df2)[names(df2)=="DIR"] <- "DIR_PROV_OTHER"
  
  df.merge <- merge(df1, df2, by="TIME", all.x = T)
  head(df.merge)
  
  df.merge <- df.merge[complete.cases(df.merge),]
  df.merge <- arrange(df.merge, TIME)
  
  corr.prov <- analyze.coherency(df.merge, my.pair = c("DIR_PROV_IN_Q","DIR_PROV_OTHER"),
                                 loess.span = 0,
                                 dt = 1/12, dj = 1/100,
                                 window.type.t = 1, window.type.s = 1,
                                 window.size.t = 12, #examine coherence year-by-year
                                 window.size.s = (1/4), #periods on the order of 
                                 lowerPeriod = 6/12, #shortest possible period in years (annual)
                                 upperPeriod = 2, #largest possible period (2 years)
                                 make.pval = TRUE, n.sim = 100)
  #is there a statistically significant coherence through TIME?
  coherence.mat <- corr.prov$Coherence
  pval.mat <- corr.prov$Coherence.pval
  pval.mat <- pval.mat< pval.cut
  
  out.mat <- coherence.mat*pval.mat
  out.mat[out.mat==0] <- NA
  
  #Focal province
  province_in_q <- sub(" ", "_", unique(df2a$PROVINCE), fixed = T)
  
  df.merge$coherence_annual <- colMeans(out.mat, na.rm = T)
  
  
  df2add <- dplyr::select(df.merge, TIME, coherence_annual)
  
  df1a <- merge(df1a, df2add, by="TIME", all.x = T)
  
  df1a$coherence_prov <- province_in_q #Focal province
  
  df.out <- dplyr::select(df1a, TIME, coherence_annual, coherence_prov)
  df.out <- arrange(df.out, TIME)

  #add in cross wavelet power
  power.mat <- corr.prov$Power.xy
  pval.mat <- corr.prov$Power.xy.pval
  pval.mat <- pval.mat< pval.cut
  
  power.mat <- power.mat*pval.mat
  power.mat[power.mat==0] <- NA
  
  df.out$cross_wave_power_annual <- colMeans(power.mat, na.rm=T)

  #ggplot(df.out) + geom_line(aes(x=TIME, y=cross_wave_power_annual))
  return(df.out)
  
}

#Coherence of multiannual dengue cycles for all the provinces
get.multi.coherence <- function(dat, dat.all){
  
  #Extract data.table of all other provinces
  dat.all = subset(dat.all, PROVINCE !=unique(dat$PROVINCE))
  df.split <- dlply(dat.all, .(PROVINCE))
  
  dat.prov.coherence.multi <- lapply(X=df.split, FUN=prov.rank.multi, df1=dat)
  dat.prov.coherence.multi <- data.table::rbindlist(dat.prov.coherence.multi)

  #0-1 coding in data.table for significance 
  dat.prov.coherence.multi$sig_notsig_power <- 0
  dat.prov.coherence.multi$sig_notsig_coherence <- 0
  dat.prov.coherence.multi$sig_notsig_power[!is.na(dat.prov.coherence.multi$cross_wave_power_multi)] <- 1
  dat.prov.coherence.multi$sig_notsig_coherence[!is.na(dat.prov.coherence.multi$coherence_multi)] <- 1
  
  prov.coher.sum.multi <- ddply(dat.prov.coherence.multi, .(TIME), summarise, num_sig_cross_wave_power=sum(sig_notsig_power, na.rm = T), num_sig_coherence=sum(sig_notsig_coherence, na.rm = T), N_tot=length(sig_notsig_power))
  prov.coher.sum.multi$proportion_sig_cross_wave_power_multi <- prov.coher.sum.multi$num_sig_cross_wave_power/prov.coher.sum.multi$N_tot
  prov.coher.sum.multi$proportion_sig_coherence_multi <- prov.coher.sum.multi$num_sig_coherence/prov.coher.sum.multi$N_tot
  
  #now attach to the rest of the dataset
  
  prov.add.multi <- dplyr::select(prov.coher.sum.multi, TIME, proportion_sig_cross_wave_power_multi, proportion_sig_coherence_multi)
  
  dat <- merge(dat, prov.add.multi, by ="TIME", all.x = T)
  dat <- arrange(dat, TIME)
  
  return(dat)
}

#Function for running both annual and multiannual phase coherence 
get.both.coherence <- function(dat, dat.all, pval.cut1){
  dat.all = subset(dat.all, PROVINCE !=unique(dat$PROVINCE))
  
  df.split <- dlply(dat.all, .(PROVINCE))
  
  
  dat.prov.coherence.multi <- lapply(X=df.split, FUN=prov.rank.multi, df1=dat, pval.cut=pval.cut1)
  dat.prov.coherence.multi <- data.table::rbindlist(dat.prov.coherence.multi)
  dat.prov.coherence.annual <- lapply(X=df.split, FUN=prov.rank.annual, df1=dat,  pval.cut=pval.cut1)
  dat.prov.coherence.annual <- data.table::rbindlist(dat.prov.coherence.annual)

  names(dat.prov.coherence.annual) <- names(dat.prov.coherence.multi) <- c("TIME", "coherence", "coherence_prov", "cross_wave_power")
  dat.prov.coherence.annual$cycle_type <- "annual"
  dat.prov.coherence.multi$cycle_type <- "multi"
  
  dat.out <- rbind(dat.prov.coherence.annual, dat.prov.coherence.multi)
  dat.out$PROVINCE <- unique(dat$PROVINCE)
  
  return(dat.out)
}

#Set up data.table for binding in each provinces' coherence results
new.coherence.df <- NULL
for(i in 1:length(prov.split)){
  # For loop used to ensure memory requirements do not cause termination
    # of R session
  if(i %% 4 == 0){gc()}
  tmp_coherence.out <- lapply(list(prov.split[[i]]), get.both.coherence, dat.all=ptl_province_inla_df, pval.cut1=0.01)
  tmp_coherence.df <- data.table::rbindlist(tmp_coherence.out)
  new.coherence.df <- rbind(new.coherence.df, tmp_coherence.df)  
}



#Processing coherence results ----
complete.coherence.df <- merge(complete.coherence.df, unique(subset(ptl_province_inla_df, select = c("PROVINCE", "longitude", "latitude"))),
                               by = "PROVINCE")
#Check to ensure that we have same province names as original data.table
unique(complete.coherence.df$coherence_prov)[which(!(unique(complete.coherence.df$coherence_prov) %in% ptl_province_inla_df$PROVINCE))]
complete.coherence.df[which(coherence_prov == "Contralmirante_Villar"),
                      coherence_prov:= "Contralmirante Villar"]
tmp <- unique(subset(ptl_province_inla_df, select = c("PROVINCE", "longitude", "latitude")))
setnames(tmp, c("coherence_prov", "comp_longitude", "comp_latitude"))
complete.coherence.df <- merge(complete.coherence.df, tmp,
                               by = "coherence_prov")
# for(i in 1:nrow(complete.coherence.df)){
#   complete.coherence.df[i, dist_m:= distm(c(longitude, latitude), c(comp_longitude, comp_latitude),fun = distHaversine)]
# }
# complete.coherence.df[, dist_km:= dist_m/1000]


complete.coherence.df <- merge(complete.coherence.df, 
                               subset(ptl_province_inla_df, 
                                      select = c("tmax", "tmax_roll_2",
                                                 "tmin", "tmin_roll_2",
                                                 "prec", "prec_roll_2",
                                                 "SPI_6", "E_INDEX",
                                                 "PROVINCE", "TIME",
                                                 "POP", "YEAR", "MONTH")),
                               by = c("PROVINCE", "TIME"))
complete.coherence.df <- merge(complete.coherence.df,
                               unique(subset(ptl_province_inla_df,
                                             select = c("PROVINCE", 
                                                        "URBAN_ORDER", "PROPN_URBAN_2017"))),
                               by = c("PROVINCE"))

#Significance of coherence values ----
significance_coherence_df <- copy(coherence.df)
significance_coherence_df[which(is.na(coherence)), sig_coherence:= 0]
significance_coherence_df[which(!is.na(coherence)), sig_coherence:= 1]
significance_coherence_df[which(is.na(cross_wave_power)), sig_cross_wave_power:= 0]
significance_coherence_df[which(!is.na(cross_wave_power)), sig_cross_wave_power:= 1]

#GAMs ----
#Set up factors for GAMs
complete.coherence.df[, TIME_FACTOR:= as.factor(TIME)]
complete.coherence.df[, MONTH_FACTOR:= as.factor(MONTH)]
complete.coherence.df[, YEAR_FACTOR:= as.factor(YEAR)]

complete.coherence.df <- complete.coherence.df[which(!is.na(coherence)), ]
complete.coherence.df[, PROVINCE_FACTOR:= as.factor(PROVINCE)]
complete.coherence.df[, HIGH_URBAN:= as.factor(ifelse(PROPN_URBAN_2017 >= 0.6, 1, 0))]




#PLOTTING -----
#Proportion of cyles with significant coherence ----
greatest_propn_coherent_df <- significance_coherence_df[, list(AVG_PAIRWISE_COHERENCE = mean(coherence, na.rm = TRUE),
                                                               PROPN_SIGNIFICANT_COHERENT = 
                                                                 length(which(sig_coherence == 1))/length(sig_coherence)),
                                                        by = c("PROVINCE", "cycle_type")]
greatest_propn_coherent_df <- merge(greatest_propn_coherent_df, 
                                    unique(subset(ptl_province_inla_df,
                                                  select = c("LAT_PROV_IND",
                                                             "LONG_PROV_IND",
                                                             "PROVINCE"))),
                                    by = "PROVINCE")
annual_greatest_propn_coherent_df <- subset(greatest_propn_coherent_df, 
                                            cycle_type == "annual")
annual_greatest_propn_coherent_df <- merge(annual_greatest_propn_coherent_df, 
                                           unique(subset(ptl_province_inla_df, select = c("PROPN_URBAN_2017", "PROVINCE"))),
                                           by = "PROVINCE")
annual_greatest_propn_coherent_df[, cor.test(PROPN_SIGNIFICANT_COHERENT, PROPN_URBAN_2017)]


ptl_province_peru_dt$propn_annual_coherent <- annual_greatest_propn_coherent_df$PROPN_SIGNIFICANT_COHERENT

ptl_province_peru_map_propn_annual_coherent <- 
  ggplot(ptl_province_peru_dt, aes(geometry = geometry))+
  geom_sf(aes(fill = propn_annual_coherent), alpha = 0.7)+theme_bw()+
  theme(legend.position = "bottom")+xlab("Longitude") + ylab("Latitude")+
  geom_text(data=ptl_province_peru_dt, aes(coords_x, coords_y, group=NULL, label=PROVINCIA), size=4.5) +
  scale_fill_viridis_c(name = "Proportion of cycles \nwith significant coherence")+
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
ptl_province_peru_map_propn_annual_coherent
#Paita and Morropon have most frequently significant coherence with other provinces

multi_greatest_propn_coherent_df <- subset(greatest_propn_coherent_df, 
                                           cycle_type == "multi")
multi_greatest_propn_coherent_df <- merge(multi_greatest_propn_coherent_df, 
                                          unique(subset(ptl_province_inla_df, select = c("PROPN_URBAN_2017", "PROVINCE"))),
                                          by = "PROVINCE")
ptl_province_peru_dt$propn_multi_coherent <- multi_greatest_propn_coherent_df$PROPN_SIGNIFICANT_COHERENT
with(ptl_province_peru_dt, cor(propn_annual_coherent, PROPN_URBAN_2017))  #Correlation between proportion of significant coherence values and urbanised 
with(ptl_province_peru_dt, cor(propn_multi_coherent, PROPN_URBAN_2017))  #Stronger correlation between proportion of significant coherence values and urbanised 

ptl_province_peru_map_propn_multi_coherent <- 
  ggplot(ptl_province_peru_dt, aes(geometry = geometry))+
  geom_sf(aes(fill = propn_multi_coherent), alpha = 0.7)+theme_bw()+
  theme(legend.position = "bottom")+xlab("Longitude") + ylab("Latitude")+
  geom_text(data=ptl_province_peru_dt, aes(coords_x, coords_y, group=NULL, label=PROVINCIA), size=4.5) +
  scale_fill_viridis_c(name = "Proportion of cycles \nwith significant coherence")+
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
        legend.position = "bottom", legend.key.size = unit(2.5, 'cm'))
ggarrange(ptl_province_peru_map_propn_annual_coherent,
          ptl_province_peru_map_propn_multi_coherent,
          nrow = 1, common.legend = TRUE,
          legend = "bottom")
propn_sig_coherence_maps <- ggarrange(ptl_province_peru_map_propn_annual_coherent,
                                      ptl_province_peru_map_propn_multi_coherent,
                                      nrow = 1, common.legend = TRUE, legend = "bottom", labels = c("A)", "B)"), hjust = -2.0,
                                      font.label = list(size = 26, face = "bold"))
propn_sig_coherence_maps
ggsave(propn_sig_coherence_maps, file = file.path(peru.province.wavelet.out.dir, "propn_sig_coherence_maps.pdf"), h = 14, w = 22)

#Pairwise average coherence tile plot ----
pairwise_significance_coherence_df <- 
  significance_coherence_df[, list(AVG_PAIRWISE_COHERENCE = mean(coherence, na.rm = TRUE),
                                   PROPN_SIGNIFICANT_COHERENT = 
                                     length(which(sig_coherence == 1))/length(sig_coherence)),
                            by = c("PROVINCE", "coherence_prov", "cycle_type")]
tmp <- unique(subset(ptl_province_inla_df,
                     select = c("LAT_PROV_IND","LONG_PROV_IND","PROVINCE")))
pairwise_significance_coherence_df <- merge(pairwise_significance_coherence_df,
                                            tmp,
                                            by = "PROVINCE")
pairwise_significance_coherence_df <- merge(pairwise_significance_coherence_df, unique(subset(ptl_province_inla_df, select = c("PROVINCE", "longitude", "latitude"))),
                                            by = "PROVINCE")

unique(pairwise_significance_coherence_df$coherence_prov)[which(!(unique(pairwise_significance_coherence_df$coherence_prov) %in% ptl_province_inla_df$PROVINCE))]
pairwise_significance_coherence_df[which(coherence_prov == "Contralmirante_Villar"),
                                   coherence_prov:= "Contralmirante Villar"]
tmp <- unique(subset(ptl_province_inla_df, select = c("PROVINCE", "longitude", "latitude")))
setnames(tmp, c("coherence_prov", "comp_longitude", "comp_latitude"))
pairwise_significance_coherence_df <- merge(pairwise_significance_coherence_df, tmp,
                                            by = "coherence_prov")
pairwise_significance_coherence_df

for(i in 1:nrow(pairwise_significance_coherence_df)){
  pairwise_significance_coherence_df[i, dist_m:= distm(c(longitude, latitude), c(comp_longitude, comp_latitude),fun = distHaversine)]
}
pairwise_significance_coherence_df[, dist_km:= dist_m/1000]


tmp <- unique(subset(ptl_province_inla_df,
                     select = c("LAT_PROV_IND","LONG_PROV_IND","PROVINCE")))
setnames(tmp, c("coherence_LAT_PROV_IND", "coherence_LONG_PROV_IND",
                "coherence_prov"))
pairwise_significance_coherence_df <- merge(pairwise_significance_coherence_df,
                                            tmp,
                                            by = "coherence_prov")
annual_pairwise_significance_coherence_df <- 
  subset(pairwise_significance_coherence_df, cycle_type == "annual")
setkeyv(annual_pairwise_significance_coherence_df, c("LAT_PROV_IND", "coherence_LAT_PROV_IND"))



annual_pairwise_significance_coherence_plot <- ggplot(annual_pairwise_significance_coherence_df) +
  geom_tile(aes(x = LAT_PROV_IND, y = coherence_LAT_PROV_IND, 
                fill = PROPN_SIGNIFICANT_COHERENT,
                color = PROPN_SIGNIFICANT_COHERENT))+
  theme_bw() + labs(x = "Province", y = "Province")+
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_blank(),
        axis.title=element_blank(), 
        legend.text=element_text(size=24)+geom_text(size = 20),
        legend.position = "bottom",legend.key.size = unit(2.0, 'cm'))+
  scale_fill_viridis_c(name = "Proportion of annual cycles \n with significant coherence")+
  scale_color_viridis_c(name = "Proportion of annual cycles \n with significant coherence")+
  scale_x_discrete(limit = latitude_province_names)+
  scale_y_discrete(limit = latitude_province_names)
annual_pairwise_significance_coherence_plot #Proportion of annual cycles that are statistically significant
ggsave(annual_pairwise_significance_coherence_plot, 
       file = file.path(peru.province.wavelet.out.dir, 
                        "annual_pairwise_significance_coherence_plot.pdf"), 
       h = 20, w = 26)




#Relationship between proportion of significant coherence and distance
hist(annual_pairwise_significance_coherence_df[, cor(dist_km, PROPN_SIGNIFICANT_COHERENT), by = "PROVINCE"]$V1)
corr_betw_dist_and_propn_significant_coherent <- 
  annual_pairwise_significance_coherence_df[, list(corr_betw_dist_and_propn_significant_coherent = cor(dist_km, PROPN_SIGNIFICANT_COHERENT)), by = "PROVINCE"]
annual_pairwise_significance_coherence_df[, list(corr_betw_dist_and_propn_significant_coherent = cor(dist_km, PROPN_SIGNIFICANT_COHERENT))]
corr_betw_dist_and_propn_significant_coherent
ptl_province_peru_dt <- merge(ptl_province_peru_dt,corr_betw_dist_and_propn_significant_coherent,
                              by.x = "PROVINCIA", by.y = "PROVINCE")
ptl_province_peru_dt$corr_betw_dist_and_propn_significant_coherent.x <- NULL
ptl_province_peru_dt$corr_betw_dist_and_propn_significant_coherent.y <- NULL
ptl_province_peru_dt$corr_betw_dist_and_propn_significant_coherent <- 
  ptl_province_peru_dt$corr_betw_dist_and_propn_significant_coherent.y

ptl_province_peru_map_corr_betw_dist_and_propn_significant_coherent <- 
  ggplot(ptl_province_peru_dt, aes(geometry = geometry))+
  geom_sf(aes(fill = corr_betw_dist_and_propn_significant_coherent), alpha = 0.7)+theme_bw()+
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
ptl_province_peru_map_corr_betw_dist_and_propn_significant_coherent


#Now, same for multiannual cycles
multi_pairwise_significance_coherence_df <- 
  subset(pairwise_significance_coherence_df, cycle_type == "multi")
setkeyv(multi_pairwise_significance_coherence_df, c("LAT_PROV_IND", "coherence_LAT_PROV_IND"))
multi_pairwise_significance_coherence_df[, list(corr_betw_dist_and_propn_significant_coherent_multi = cor(dist_km, PROPN_SIGNIFICANT_COHERENT)), by = "PROVINCE"]
corr_betw_dist_and_propn_significant_coherent_multi <- 
  multi_pairwise_significance_coherence_df[, list(corr_betw_dist_and_propn_significant_coherent_multi = cor(dist_km, PROPN_SIGNIFICANT_COHERENT)), by = "PROVINCE"]
corr_betw_dist_and_propn_significant_coherent_multi
ptl_province_peru_dt <- merge(ptl_province_peru_dt,corr_betw_dist_and_propn_significant_coherent_multi,
                              by.x = "PROVINCIA", by.y = "PROVINCE")
ptl_province_peru_map_corr_betw_dist_and_propn_significant_coherent_multi <- 
  ggplot(ptl_province_peru_dt, aes(geometry = geometry))+
  geom_sf(aes(fill = corr_betw_dist_and_propn_significant_coherent_multi), alpha = 0.7)+theme_bw()+
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
ptl_province_peru_map_corr_betw_dist_and_propn_significant_coherent_multi


#2) EPIDEMIC SYNCHRONY ----
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
  
  #Storing input data
  df1.hold = df1
  df2.hold = df2
  df1 <- dplyr::select(df1, TIME, YEAR, DIR)
  df2 <- dplyr::select(df2, TIME, YEAR, DIR)
  names(df1) <- c("TIME", "YEAR", "DIR_PROV_IN_Q")
  names(df2) <- c("TIME", "YEAR", "DIR_PROV_OTHER")
  df.join <- merge(df1,df2, by = c("TIME", "YEAR"))
  
  #Full time series correlation (2010-2021)
  full.corr <- cor(df.join$DIR_PROV_IN_Q, df.join$DIR_PROV_OTHER)
  
  #Dividing by year
  df.year.split <- dlply(df.join, .(YEAR))
  print(length(df.year.split$DIR_PROV_IN_Q))
  print(length(df.year.split$DIR_PROV_OTHER))
  
  df.year.out <- lapply(df.year.split, cross.corr)
  df.year.out <- data.table::rbindlist(df.year.out)

  df.year.out$PROVINCE = unique(df1.hold$PROVINCE)
  df.year.out$comp_prov <- unique(df2.hold$PROVINCE)
  df.year.out$full_ts_corr <- full.corr
  
  #Geographic distance between two provinces' centroids
  df.year.out$dist_m <- distm(c(unique(df1.hold$longitude), unique(df1.hold$latitude)), c(unique(df2.hold$longitude), unique(df2.hold$latitude)), fun = distHaversine)
  df.year.out$dist_km <- df.year.out$dist_m/1000
  return(df.year.out)
}

assess.corr.annual.cycle <- function(df2, df1){
  
  #Storing input data
  df1.hold = df1
  df2.hold = df2
  df1 <- dplyr::select(df1, TIME, YEAR, reconstructed_annual_period)
  df2 <- dplyr::select(df2, TIME, YEAR, reconstructed_annual_period)
  names(df1) <- c("TIME", "YEAR", "annual_cycle_this_prov")
  names(df2) <- c("TIME", "YEAR", "annual_cycle_other_prov")
  df.join <- merge(df1,df2, by = c("TIME", "YEAR"))
  
  #now get the full time series correlation
  full.corr <- cor(df.join$annual_cycle_this_prov, df.join$annual_cycle_other_prov)
  
  #then, split both by YEAR and look within
  df.year.split <- dlply(df.join, .(YEAR))
  
  df.year.out <- lapply(df.year.split, cross.corr.cycle)
  df.year.out <- data.table::rbindlist(df.year.out)
  #and assess cross corr within each YEAR
  
  # now, add in the identifying details
  df.year.out$PROVINCE = unique(df1.hold$PROVINCE)
  df.year.out$comp_prov <- unique(df2.hold$PROVINCE)
  df.year.out$full_ts_corr <- full.corr
  df.year.out$dist_m <- distm(c(unique(df1.hold$longitude), unique(df1.hold$latitude)), c(unique(df2.hold$longitude), unique(df2.hold$latitude)), fun = distHaversine)
  df.year.out$dist_km <- df.year.out$dist_m/1000
  return(df.year.out)
}
assess.corr.5YEAR <- function(df2, df1){
  df1.hold = df1
  df2.hold = df2
  
  df1 <- dplyr::select(df1, TIME, YEAR, reconstructed_multi_period)
  df2 <- dplyr::select(df2, TIME, YEAR, reconstructed_multi_period)
  names(df1) <- c("TIME", "YEAR", "multi_this_prov")
  names(df2) <- c("TIME", "YEAR", "multi_other_prov")
  df.join <- merge(df1,df2, by = c("TIME", "YEAR"))
  
  
  #Overall corellation (not stratified by time windows)
  full.corr <- cor(df.join$multi_this_prov, df.join$multi_other_prov)
  
  #5-year moving windows and look within each one
  df.year.split <- list()
  for(i in 1:(length(unique(df.join$YEAR))-4)){
    index.end = i + 4
    year.vect = unique(df.join$YEAR)
    sub.dat = subset(df.join, YEAR>=year.vect[i] & YEAR<= year.vect[index.end])
    df.year.split[[i]] <- sub.dat
  }
  
  
  df.year.out <- lapply(df.year.split, cross.corr.multi)
  df.year.out <- data.table::rbindlist(df.year.out)
  #and assess cross corr within each YEAR
  
  # now, add in the identifying details
  df.year.out$PROVINCE = unique(df1.hold$PROVINCE)
  df.year.out$comp_prov <- unique(df2.hold$PROVINCE)
  df.year.out$full_ts_corr <- full.corr
  
  df.year.out$dist_m <- distm(c(unique(df1.hold$longitude), unique(df1.hold$latitude)), c(unique(df2.hold$longitude), unique(df2.hold$latitude)), fun = distHaversine)
  df.year.out$dist_km <- df.year.out$dist_m/1000
  
  return(df.year.out)
}
assess.cross.corr <- function(PROVINCE1, df.all, cycle_type){
  
  #cycle_type is one of i) raw annual case time series, 
                      # ii) annual cycle from reconstructed dengue cycles
  df.other <- subset(df.all, PROVINCE!=PROVINCE1)
  
  #now split other by province 
  df.other <- arrange(df.other, PROVINCE, TIME)
  df.split <- dlply(df.other, .(PROVINCE))
  
  df.now = subset(df.all, PROVINCE==PROVINCE1)
  df.now <- arrange(df.now, PROVINCE, TIME)
  

  if(cycle_type=="annual_case"){
    out.provs.list <- lapply(df.split, assess.corr.annual, df1 = df.now)  
  }else if(cycle_type=="annual_cycle"){
    out.provs.list <- lapply(df.split, assess.corr.annual.cycle, df1 = df.now)  
  }else if(cycle_type=="multi"){
    out.provs.list <- lapply(df.split, assess.corr.5YEAR, df1 = df.now)  
  }
  
  out.provs.df <- data.table::rbindlist(out.provs.list)
  
  return(out.provs.df) 
}

#List of provinces 
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
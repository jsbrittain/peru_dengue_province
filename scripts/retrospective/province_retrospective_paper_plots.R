# Plots of wavelets + climate-based analyses for paper -----


# Main Text----- Reconstructions-----

recons_urban_plot_grob <- ggarrange(recons_annual_urban_plot + coord_cartesian(xlim = c(2010.4,
    2021.917), expand = F), recons_multi_urban_plot + coord_cartesian(xlim = c(2010.4,
    2021.917), expand = F), common.legend = TRUE, legend = "bottom", hjust = -1.5,
    labels = c("A)", "B)"), font.label = list(size = 25, face = "bold"))
ggsave(recons_urban_plot_grob, file = file.path(peru.province.wavelet.out.dir, "recons_urban_plot_grob.pdf"),
    h = 12, w = 24)
recons_annual_plot_grob <- ggarrange(recons_annual_long_plot + coord_cartesian(xlim = c(2010.4,
    2021.917), expand = F), recons_annual_lat_plot + coord_cartesian(xlim = c(2010.4,
    2021.917), expand = F), common.legend = TRUE, legend = "bottom", hjust = -1.5,
    labels = c("A)", "B)"), font.label = list(size = 25, face = "bold"))
recons_multi_plot_grob <- ggarrange(recons_multi_long_plot + coord_cartesian(xlim = c(2010.4,
    2021.917), expand = F) + scale_fill_viridis_b(), recons_multi_lat_plot + scale_fill_viridis_b() +
    coord_cartesian(xlim = c(2010.4, 2021.917), expand = F), common.legend = TRUE,
    legend = "bottom", hjust = -1.5, labels = c("C)", "D)"), font.label = list(size = 25,
        face = "bold"))

recons_plot_grob <- ggarrange(ggarrange(recons_annual_plot_grob), ggarrange(recons_multi_plot_grob),
    ncol = 1, nrow = 2)
ggsave(recons_plot_grob, file = file.path(peru.province.wavelet.out.dir, "recons_plot_grob.pdf"),
    h = 16, w = 22)


# By year
ggsave(recons_annual_long_plot_by_year, file = file.path(peru.province.wavelet.out.dir,
    "recons_annual_long_plot_by_year.pdf"), h = 14, w = 24)
ggsave(recons_multi_lat_plot_by_year, file = file.path(peru.province.wavelet.out.dir,
    "recons_multi_lat_plot_by_year.pdf"), h = 14, w = 24)


# Wavelet - Coherence
propn_sig_coherence_maps <- ggarrange(ptl_province_peru_map_propn_annual_coherent,
    ptl_province_peru_map_propn_multi_coherent, nrow = 1, common.legend = TRUE, legend = "bottom",
    labels = c("A)", "B)", "C)"), hjust = -2, font.label = list(size = 26, face = "bold"))
propn_sig_coherence_maps
ggsave(propn_sig_coherence_maps, file = file.path(peru.province.wavelet.out.dir,
    "propn_sig_coherence_maps.pdf"), h = 14, w = 22)
# Corresponding tile plot for pairwise relationships
ggsave(annual_pairwise_significance_coherence_plot, file = file.path(peru.province.wavelet.out.dir,
    "annual_pairwise_significance_coherence_plot.pdf"), h = 20, w = 26)



# Wavelet - Synchrony-----
annual_case_corr_long_lat_tiles <- ggarrange(long_plot_annual_case, lat_plot_annual_case,
    nrow = 1, common.legend = TRUE, legend = "bottom") + theme(legend.key.size = unit(2.5,
    "cm"))
annual_case_corr_long_lat_tiles
ggsave(annual_case_corr_long_lat_tiles, file = file.path(peru.province.wavelet.out.dir,
    "annual_case_corr_long_lat_tiles.pdf"), h = 12, w = 24)
ggsave(annual_case_corr_long_lat_tiles, file = file.path(peru.province.wavelet.out.dir,
    "annual_case_corr_long_lat_tiles.png"), h = 12, w = 24)
# Results shown for annual cases as reconstruction only valid within cone

# Annual cycle

# Multi-cycle (non-overlapping)
multi_cycle_corr_plots <- ggarrange(long_plot_multi_cycle, lat_plot_multi_cycle,
    nrow = 1, common.legend = TRUE, legend = "bottom")
multi_cycle_corr_plots
ggsave(multi_cycle_corr_plots, file = file.path(peru.province.wavelet.out.dir, "multi_cycle_corr_plots.pdf"),
    h = 12, w = 24)


# Multi-cylce correlation (overlapping)
multi_cycle_overlapping_corr_plots <- ggarrange(long_plot_multi_cycle_overlapping,
    lat_plot_multi_cycle_overlapping, nrow = 1, common.legend = TRUE, legend = "bottom")
multi_cycle_overlapping_corr_plots
multi_cycle_overlapping_corr_plots
ggsave(multi_cycle_overlapping_corr_plots, file = file.path(peru.province.wavelet.out.dir,
    "multi_cycle_overlapping_corr_plots.pdf"), h = 12, w = 24)
# Clear trend of greater synchrony in west and north
multi_cycle_corr_long_lat_tiles <- ggarrange(long_plot_multi_cycle, lat_plot_multi_cycle,
    nrow = 1, common.legend = TRUE, legend = "bottom") + theme(legend.key.size = unit(2.5,
    "cm"))
multi_cycle_corr_long_lat_tiles
ggsave(multi_cycle_corr_long_lat_tiles, file = file.path(peru.province.out.dir, "multi_cycle_corr_long_lat_tiles.pdf"),
    h = 12, w = 22)
ggsave(multi_cycle_corr_long_lat_tiles, file = file.path(peru.province.out.dir, "multi_cycle_corr_long_lat_tiles.png"),
    h = 12, w = 22)

# ANNUAL CYCLES - COHERENCE

# Supplementary ----- Mean Log DIR ----
mean_monthly_plot_grid <- ggarrange(plotlist = list(mean_monthly_longitude_plot,
    mean_monthly_latitude_plot), nrow = 1, ncol = 2, legend = "bottom", common.legend = TRUE) +
    theme(legend.key.size = unit(2.5, "cm"))

ggsave(mean_monthly_plot_grid, file = file.path(peru.province.out.dir, "mean_monthly_plot_grid.pdf"),
    h = 12, w = 26)
ggsave(mean_monthly_plot_grid, file = file.path(peru.province.out.dir, "mean_monthly_plot_grid.png"),
    h = 12, w = 26)


# Exploratory Log DIR -----
monthly_dir_plots <- ggarrange(monthly_longitude_plot, monthly_latitude_plot, common.legend = TRUE,
    legend = "bottom")
ggsave(monthly_dir_plots, file = file.path(peru.province.out.dir, "monthly_dir_plots.pdf"),
    h = 12, w = 22)

monthly_longitude_plot_by_year
ggsave(monthly_longitude_plot_by_year, file = file.path(peru.province.out.dir, "monthly_longitude_plot_by_year.pdf"),
    h = 14, w = 22)
ggsave(monthly_latitude_plot_by_year, file = file.path(peru.province.out.dir, "monthly_latitude_plot_by_year.pdf"),
    h = 14, w = 22)


# Average wavelet power (annual)
avg_wavelet_power_plots <- ggarrange(longitude_avg_wavelet_power_plot, latitude_avg_wavelet_power_plot,
    common.legend = TRUE, legend = "bottom")
ggsave(avg_wavelet_power_plots, file = file.path(peru.province.out.dir, "avg_wavelet_power_plots.pdf"),
    h = 12, w = 22)
ggsave(avg_wavelet_power_plots, file = file.path(peru.province.out.dir, "avg_wavelet_power_plots.png"),
    h = 12, w = 22)

# Average wavelet power (multiannual)
avg_wavelet_power_plots <- ggarrange(longitude_avg_wavelet_power_plot, latitude_avg_wavelet_power_plot,
    common.legend = TRUE, legend = "bottom")
ggsave(avg_wavelet_power_plots, file = file.path(peru.province.out.dir, "avg_wavelet_power_plots.pdf"),
    h = 12, w = 22)
ggsave(avg_wavelet_power_plots, file = file.path(peru.province.out.dir, "avg_wavelet_power_plots.png"),
    h = 12, w = 22)

# Correlation and Coherence pairwise----
annual_pairwise_significance_coherence_plot
# Map- how coherence depends on distance in each province
average_annual_pairwise_distance_maps <- ggarrange(ptl_province_peru_map_annual.case_corr_betw_dist_and_correlation,
    ptl_province_peru_map_corr_betw_dist_and_propn_significant_coherent, ptl_province_peru_map_corr_betw_dist_and_propn_significant_coherent_multi,
    nrow = 1, common.legend = TRUE, legend = "bottom", labels = c("A)", "B)", "C)"),
    hjust = -2, font.label = list(size = 26, face = "bold"))
average_annual_pairwise_distance_maps
ggsave(average_annual_pairwise_distance_maps, file = file.path(peru.province.wavelet.out.dir,
    "average_annual_pairwise_distance_maps.pdf"), h = 12, w = 22)

# Coherence with climatic ----
annual_recons_coherency_plot_list <- list(tmax_annual_recons_coherency_plot, prec_annual_recons_coherency_plot)
c()

annual_multi_recons_coherency_plot_list <- list(tmax_annual_recons_coherency_plot,
    tmax_multi_recons_coherency_plot, prec_annual_recons_coherency_plot, prec_multi_recons_coherency_plot)
annual_multi_recons_coherency_plot_list
ggarrange(plotlist = annual_multi_recons_coherency_plot_list, nrow = 2, ncol = 2,
    common.legend = TRUE, legend = "bottom")


# Rows: Coherency; Cross-wave and coherency Columns; tmax, prec and icen
tmax_prec_icen_coherency_cross_wave_list <- list(tmax_annual_recons_coherency_plot,
    prec_annual_recons_coherency_plot, tmax_annual_recons_cross_wave_plot, prec_annual_recons_cross_wave_plot)

ggarrange(plotlist = tmax_prec_icen_coherency_cross_wave_list, nrow = 2, ncol = 2,
    common.legend = TRUE, legend = "bottom")


tmax_annual_recons_cross_wave_coherency_grid <- ggarrange(tmax_annual_recons_coherency_plot,
    tmax_annual_recons_cross_wave_plot, nrow = 1, labels = c("A)", "B)"), hjust = -1.5,
    font.label = list(size = 26, face = "bold"))
tmax_annual_recons_cross_wave_coherency_grid
ggsave(tmax_annual_recons_cross_wave_coherency_grid, file = file.path(peru.province.wavelet.out.dir,
    "tmax_annual_recons_cross_wave_coherency_grid.pdf"), h = 10, w = 20)
ggsave(tmax_annual_recons_cross_wave_coherency_grid, file = file.path(peru.province.wavelet.out.dir,
    "tmax_annual_recons_cross_wave_coherency_grid.png"), h = 10, w = 20)

prec_annual_recons_cross_wave_coherency_grid <- ggarrange(prec_annual_recons_coherency_plot,
    prec_annual_recons_cross_wave_plot, nrow = 1, labels = c("A)", "B)"), hjust = -1.5,
    font.label = list(size = 26, face = "bold"))
ggsave(prec_annual_recons_cross_wave_coherency_grid, file = file.path(peru.province.wavelet.out.dir,
    "prec_annual_recons_cross_wave_coherency_grid.pdf"), h = 10, w = 20)
ggsave(prec_annual_recons_cross_wave_coherency_grid, file = file.path(peru.province.wavelet.out.dir,
    "prec_annual_recons_cross_wave_coherency_grid.png"), h = 10, w = 20)

ggsave(icen_annual_recons_cross_wave_coherency_grid, file = file.path(peru.province.wavelet.out.dir,
    "icen_annual_recons_cross_wave_coherency_grid.pdf"), h = 16, w = 18)
ggsave(icen_annual_recons_cross_wave_coherency_grid, file = file.path(peru.province.wavelet.out.dir,
    "icen_annual_recons_cross_wave_coherency_grid.png"), h = 16, w = 18)

multi_recons_cross_wave_grid




# TIME DISTRIBUTION OF CROSS-WAVE POWER -----
dist_cross_wave_tmax_prec_icen_plot <- ggarrange(distribution_cross_power_tmax_annual_plot +
    xlab("") + ylab("") + coord_cartesian(ylim = c(0, 15), expand = T), distribution_cross_power_prec_annual_plot +
    xlab("") + coord_cartesian(ylim = c(0, 15), expand = T), distribution_cross_power_icen_annual_plot +
    ylab("") + scale_y_continuous(breaks = seq(0, 15, by = 5)) + coord_cartesian(ylim = c(0,
    10), expand = T), nrow = 3, hjust = -2.5, labels = c("A)", "B)", "C)"), font.label = list(size = 26,
    face = "bold"))
dist_cross_wave_tmax_prec_icen_plot

ggsave(dist_cross_wave_tmax_prec_icen_plot, file = file.path(peru.province.wavelet.out.dir,
    "dist_cross_wave_tmax_prec_icen_plot.pdf"), h = 20, w = 20)
ggsave(dist_cross_wave_tmax_prec_icen_plot, file = file.path(peru.province.wavelet.out.dir,
    "dist_cross_wave_tmax_prec_icen_plot.png"), h = 20, w = 20)


distribution_cross_power_tmax_multi_plot
distribution_cross_power_prec_multi_plot
distribution_cross_power_icen_multi_plot
dist_cross_wave_tmax_prec_icen_multi_plot <- ggarrange(distribution_cross_power_tmax_multi_plot +
    xlab("") + ylab("") + coord_cartesian(ylim = c(0, 1.5), expand = T) + scale_y_continuous(breaks = seq(0,
    1.5, by = 0.5)), distribution_cross_power_prec_multi_plot + xlab("") + coord_cartesian(ylim = c(0,
    1.5), expand = T) + scale_y_continuous(breaks = seq(0, 1.5, by = 0.5)), distribution_cross_power_icen_multi_plot +
    ylab("") + coord_cartesian(ylim = c(0, 4.5), expand = T) + scale_y_continuous(breaks = seq(0,
    4.5, by = 1.5)), nrow = 3, hjust = -2.5, labels = c("A)", "B)", "C)"), font.label = list(size = 26,
    face = "bold"))
dist_cross_wave_tmax_prec_icen_multi_plot

ggsave(dist_cross_wave_tmax_prec_icen_multi_plot, file = file.path(peru.province.wavelet.out.dir,
    "dist_cross_wave_tmax_prec_icen_multi_plot.pdf"), h = 20, w = 20)
ggsave(dist_cross_wave_tmax_prec_icen_multi_plot, file = file.path(peru.province.wavelet.out.dir,
    "dist_cross_wave_tmax_prec_icen_multi_plot.png"), h = 20, w = 20)






# Annual and multi cross-wave together -----
dist_cross_wave_tmax_prec_icen_plot_for_combined <- ggarrange(distribution_cross_power_tmax_annual_plot +
    xlab("") + ylab("") + coord_cartesian(ylim = c(0, 15), expand = T), distribution_cross_power_prec_annual_plot +
    xlab("") + ylab("Cross-wavelet power") + coord_cartesian(ylim = c(0, 15), expand = T),
    distribution_cross_power_icen_annual_plot + ylab("") + scale_y_continuous(breaks = seq(0,
        15, by = 5)) + coord_cartesian(ylim = c(0, 10), expand = T), nrow = 3, hjust = -3,
    vjust = 2, labels = c("A)", "C)", "E)"), font.label = list(size = 26, face = "bold"))
dist_cross_wave_tmax_prec_icen_multi_plot_for_combined <- ggarrange(distribution_cross_power_tmax_multi_plot +
    xlab("") + ylab("") + coord_cartesian(ylim = c(0, 1.5), expand = T) + scale_y_continuous(breaks = seq(0,
    1.5, by = 0.5)), distribution_cross_power_prec_multi_plot + xlab("") + ylab("Cross-wavelet power") +
    coord_cartesian(ylim = c(0, 1.5), expand = T) + scale_y_continuous(breaks = seq(0,
    1.5, by = 0.5)), distribution_cross_power_icen_multi_plot + ylab("") + coord_cartesian(ylim = c(0,
    4.5), expand = T) + scale_y_continuous(breaks = seq(0, 4.5, by = 1.5)), nrow = 3,
    hjust = -3, vjust = 2, labels = c("B)", "D)", "F)"), font.label = list(size = 26,
        face = "bold"))

dist_cross_wave_tmax_prec_icen_combined_plot <- ggarrange(dist_cross_wave_tmax_prec_icen_plot_for_combined,
    dist_cross_wave_tmax_prec_icen_multi_plot_for_combined + ylab(""), ncol = 2)
dist_cross_wave_tmax_prec_icen_combined_plot
ggsave(dist_cross_wave_tmax_prec_icen_combined_plot, file = file.path(peru.province.wavelet.out.dir,
    "dist_cross_wave_tmax_prec_icen_combined_plot.pdf"), h = 16, w = 22)
ggsave(dist_cross_wave_tmax_prec_icen_combined_plot, file = file.path(peru.province.wavelet.out.dir,
    "dist_cross_wave_tmax_prec_icen_combined_plot.png"), h = 16, w = 22)

# Histogram of coherence values ----
annual_coherence_histogram



# Proportion of cycles that are coheret with other provinces (Map) -----
ptl_province_peru_map_propn_annual_coherent
ptl_province_peru_map_propn_multi_coherent
ggarrange(ptl_province_peru_map_propn_annual_coherent, ptl_province_peru_map_propn_multi_coherent,
    nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()
# Median correlation maps

ptl_province_peru_map_median_annual_case_corr
ptl_province_peru_map_median_annual_cycle_corr
ptl_province_peru_map_median_multi_cycle_corr

median_corr_maps <- ggarrange(ptl_province_peru_map_median_annual_case_corr, ptl_province_peru_map_median_annual_cycle_corr,
    ptl_province_peru_map_median_multi_cycle_corr, common.legend = TRUE, legend = "bottom",
    hjust = -1.5, nrow = 1, labels = c("A)", "B)", "C)"), font.label = list(size = 26,
        face = "bold"))
median_corr_maps
ggsave(median_corr_maps, file = file.path(peru.province.wavelet.out.dir, "median_corr_maps.pdf"),
    h = 14, w = 26)

library(spdep)
library(logger)
library(rgeoboundaries)

# --------------------------------------------------------------------------------------

admin1_region_names <- c("Piura", "Tumbes", "Lambayeque")

# --------------------------------------------------------------------------------------

province.base.dir <- file.path(getwd(), "data")
shapefiles.data.dir <- file.path(province.base.dir, "shapefiles")
province.inla.data.in.dir <- file.path(province.base.dir, "INLA/Input")

dir.create(province.inla.data.in.dir, recursive = TRUE, showWarnings = FALSE)

# Boundaries
district_boundaries <- st_read(file.path(shapefiles.data.dir, "per_admbnda_adm2_ign_20200714.shp"))
admin1_boundaries <- subset(district_boundaries, district_boundaries$ADM1_ES %in% admin1_region_names)
province_neighbour_list <- poly2nb(st_make_valid(admin1_boundaries))

# Making graph for climate-based model
log_info("Making graph for climate-based model")
nb2INLA(
  file.path(province.inla.data.in.dir, "nbr_piura_tumbes_lambayeque.graph"),
  province_neighbour_list
)

log_info("Finished peru_neighbours")

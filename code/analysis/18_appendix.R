# Filename: 18_appendix.R (2019-11-20)

# TO DO: Figures found in the Supplementary Information

# Author(s): Jannes Muenchow  

#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************

# 1. ATTACH PACKAGES AND DATA
# 2. 

#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("lattice")
library("here")
library("sf")
library("sp")
library("RSQLite")
library("tidyverse")
library("tidyr")

# attach data
alpha = readRDS(here("images/17_alpha.rds"))
# get NDVI Rasters
ndvi_rasters = readRDS(here("images/00_ndvi_rasters.rds"))
# get vector data
con = dbConnect(SQLite(), here("data/tables.gpkg"))
coast = st_read(con, "coast")
study_area = st_read(con, "study_area")
dbDisconnect(con)

#**********************************************************
# APPENDIX 2-----------------------------------------------
#**********************************************************

study_area = st_transform(study_area, 32717)
r = raster(as(study_area, "Spatial"), res = 100, 
           vals = 1)
dist = distanceFromPoints(r, st_coordinates(coast)[, 1:2])
# plot(mask(dist, study_area))
r_range = cellStats(dist, range)
classes = cut(dist, seq(r_range[1], r_range[2], length.out = 11))
# make sure that object classes has the same prj, extent, origin
classes = projectRaster(classes, crs = proj4string(ndvi_rasters))
classes = resample(classes, ndvi_rasters)
# set Pacific Ocean values to NA
classes[is.na(ndvi_rasters$ndvi_12)] = NA
coast = st_transform(coast, 4326)
# plot(classes)
# plot(crop(as(coast, "Spatial"), classes), add = TRUE)
sp_1 = spplot(classes, scales = list(draw = TRUE, tck = c(1, 0)))

saveRDS(sp_1, file = here("images/18_sample_stratification.rds"))
saveRDS(dist, file = here("images/18_dist_sea_raster.rds"))

#**********************************************************
# APPENDIX 3-----------------------------------------------
#**********************************************************

invisible(Sys.setlocale("LC_TIME", "C"))
p_1 = xyplot(n_spec ~ obs_date | plot_name, data = alpha, type = c("p", "l"), 
       ylab = "No. of species", xlab = "Observation date",
       strip = strip.custom(bg = "white",
                            par.strip.text = list(cex = 0.7)),
       scales = list(tck = c(1, 0),
                     # ticks on top are suppressed (1 = left/bottom, 0 =
                     # right/top
                     alternating = c(1, 1, 1)))
saveRDS(p_1, file = here("images/18_irrigation_spri.rds"))

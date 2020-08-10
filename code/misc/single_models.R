# Filename: single_models (2019-09-03)

# TO DO: Run each year as a single model

# Author(s): Jannes Muenchow

#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************

# 1. ATTACH PACKAGES AND DATA
# 2. INDVIDUAL MODELS

#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
# attach packages
library("dplyr")
library("gdata")
library("lattice")
library("latticeExtra")
library("mgcv")
library("raster")
library("rasterVis")
library("gridExtra")
library("sperrorest")
library("sf")

# source your own funs
source("code/funs/spcv_helper_funs.R")

# attach raster stack
ndvi = readRDS("images/14_ndvi_rasters.rds")
# attach response (DCA scores)
data = readRDS("images/23_dca_scores.rds")
data$id = as.numeric(data$id)
# attach env_data
env_data = readRDS("images/12_env_data.rds")
env_data$year = gsub(".*_", "", env_data$pnr)
# attaching spatial data
rivers = st_read("data/spatial/vector.gpkg", "rivers")
towns = st_read("data/spatial/vector.gpkg", "towns_jvs")

#**********************************************************
# 1 INDIVIDUAL MODELS--------------------------------------
#**********************************************************

# just for exploratory purposes, not used in the paper!
# add xy-coordinates to dca scores
data = inner_join(data, 
                  dplyr::select(env_data, id, year, x, y, lon, lat, slope, 
                                vcurv),
                  by = c("id", "year"))
data$ndvi = NA
for (i in c(11, 12, 16, 17)) {
  ind = grepl(i, data[, "year"]) 
  data[ind, "ndvi"] = 
    raster::extract(ndvi[[paste0("ndvi_", i)]], 
                    data[ind, c("lon", "lat")])
}
sum(is.na(data$ndvi))  # 0, very good


# have a look at the relationship between DCA1 and NDVI again

# make a plot
(xy_ndvi = xyplot(DCA1 ~ ndvi | year, data = data, pch = 16, as.table = TRUE,
                  panel = function(x, y, ...) {
                    panel.points(x, y, ...)
                    panel.loess(x, y, span = 0.7, col = gray(0.6), lwd = 1.5)
                    panel.lmline(x, y, col = "pink", lwd = 1.5)
                  }))

d_11 = filter(data, year == 2011)
fo = formula(DCA1 ~ s(ndvi))
gam_11 = gam(fo, data = d_11)
summary(gam_11)
spcv_11 = sperrorest(data = d_11, coords = c("x", "y"), 
                     formula = fo,
                     model_fun = gam,
                     pred_fun = predict,
                     smp_fun = partition_kmeans,
                     err_rep = TRUE, err_fold = FALSE,
                     err_fun = err_nrmse,
                     smp_args = list(repetition = 1:100, nfold = 5))
summary(spcv_11$error_rep)[c("test_rmse", "test_nrmse"), "mean"]
# rmse: 1.6; nrmse: 39%

d_12 = filter(data, year == 2012)
gam_12 = gam(fo, data = d_12)
summary(gam_12)
spcv_12 = sperrorest(data = d_12, coords = c("x", "y"), formula = fo,
                     model_fun = gam,
                     pred_fun = predict,
                     smp_fun = partition_kmeans,
                     err_rep = TRUE, err_fold = FALSE,
                     err_fun = err_nrmse,
                     smp_args = list(repetition = 1:100, nfold = 5))
summary(spcv_12$error_rep)[c("test_rmse", "test_nrmse"), "mean"]
# rmse: 0.6; nrmse: 18.3%

d_16 = filter(data, year == 2016)
gam_16 = gam(fo, data = d_16)
summary(gam_16)
spcv_16 = sperrorest(data = d_16, coords = c("x", "y"), formula = fo,
                 model_fun = gam,
                 pred_fun = predict,
                 smp_fun = partition_kmeans,
                 err_rep = TRUE, err_fold = FALSE,
                 err_fun = err_nrmse,
                 smp_args = list(repetition = 1:100, nfold = 5))
summary(spcv_16$error_rep)[c("test_rmse", "test_nrmse"), "mean"]
# rmse: 0.7; nrmse: 18.4%

d_17 = filter(data, year == 2017)
gam_17 = gam(fo, data = d_17)
summary(gam_17)
spcv_17 = sperrorest(data = d_17, coords = c("x", "y"), formula = fo,
                     model_fun = gam,
                     pred_fun = predict,
                     smp_fun = partition_kmeans,
                     err_rep = TRUE, err_fold = FALSE,
                     err_fun = err_nrmse,
                     smp_args = list(repetition = 1:100, nfold = 5))
summary(spcv_17$error_rep)[c("test_rmse", "test_nrmse"), "mean"]
# rmse: 1.4; nrmse: 32.4%

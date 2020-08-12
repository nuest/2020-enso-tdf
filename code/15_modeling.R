# Filename: 15_modeling.R (2019-15-10)
#
# TO DO: model DCA scores as a function of NDVI and year, spatial cv, 
#        predictive mapping
#
# Author(s): Jannes Muenchow, Jonas Brock
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DATA PREPARATION
# 3. DATA EXPLORATION
# 4. MODELING AND SPATIAL CV
# 5. PREDICTIVE MAPPING
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

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
library("RSQLite")
library("here")

# attach your own funs
devtools::load_all()

# attach raster stack
ndvi = readRDS("images/00_ndvi_rasters.rds")
# attach response (DCA scores)
data = readRDS("images/14_dca_scores.rds")
data$id = as.numeric(data$id)
# attach env_data
env_data = readRDS("images/11_env_data.rds")
env_data$year = gsub(".*_", "", env_data$pnr)
# attaching spatial data
con = dbConnect(SQLite(), "data/tables.gpkg")
rivers = st_read(con, "rivers")
towns = st_read(con, "towns_jvs")
dbDisconnect(con)

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

# add xy-coordinates to dca scores
data = inner_join(data, 
                  dplyr::select(env_data, id, year, x, y, lon, lat, slope, 
                                vcurv),
                  by = c("id", "year"))
# extract NDVI to points
data$ndvi = NA
for (i in c(11, 12, 16, 17)) {
  ind = grepl(i, data[, "year"]) 
  data[ind, "ndvi"] = 
    raster::extract(ndvi[[paste0("ndvi_", i)]], 
                    data[ind, c("lon", "lat")])
}
sum(is.na(data$ndvi))  # 0, very good

# check if plots shared the same pixel
group_by(data, year) %>%
  summarize(n = n(), distinct_ndvi = n_distinct(ndvi), )
# excellent, there are no plots which share the same pixel
saveRDS(data, here("images/15_model_input_data.rds"))
# test partition_kmeans_factor, i.e., make sure that if a plot is selected for
# the test dataset, then all years (2011, 2012, 2016, 2017) should go into the
# test dataset
tmp = partition_kmeans_factor(data, coords = c("lon", "lat"),
                              fac = "id", nfold = 5, repetition = 1)

# tmp = partition_kmeans(data, coords = c("lon", "lat"),
#                        nfold = 5, repetition = 1)

dplyr::arrange(data[tmp$`1`$`1`$test, ], year)[, "id"]  # looks good
# plots 1-7 from all years are only available in the test dataset
table(data[tmp$`1`$`2`$test, "id"])  
table(data[tmp$`1`$`2`$train, "id"])
# plots 8-20 from all years are only available in the test dataset
table(data[tmp$`1`$`2`$test, "id"])  
table(data[tmp$`1`$`2`$train, "id"])
# plots 21-33 from all years are only available in the test dataset
table(data[tmp$`1`$`3`$test, "id"])  
table(data[tmp$`1`$`3`$train, "id"])


# now visualize
plot(data[, c("lon", "lat")])
points(data[tmp$`1`$`1`$test, c("lon", "lat")], col = "blue", pch = 16)
points(data[tmp$`1`$`2`$test, c("lon", "lat")], col = "red", pch = 16)
points(data[tmp$`1`$`3`$test, c("lon", "lat")], col = "green", pch = 16)
points(data[tmp$`1`$`4`$test, c("lon", "lat")], col = "orange", pch = 16)
points(data[tmp$`1`$`5`$test, c("lon", "lat")], col = "black", pch = 16)
# looks good

#**********************************************************
# 3 DATA EXPLORATION---------------------------------------
#**********************************************************

# first a visual inspection
# make a plot
(xy_ndvi = xyplot(DCA1 ~ ndvi | year, data = data, pch = 16, as.table = TRUE,
                panel = function(x, y, ...) {
                  panel.points(x, y, ...)
                  panel.loess(x, y, span = 0.7, col = gray(0.6), lwd = 1.5)
                  panel.lmline(x, y, col = "pink", lwd = 1.5)
                }))

(xy_vcurv = xyplot(DCA1 ~ vcurv | year, data = data, pch = 16, as.table = TRUE,
                  panel = function(x, y, ...) {
                    panel.points(x, y, ...)
                    panel.loess(x, y, span = 0.7, col = gray(0.6), lwd = 1.5)
                    panel.lmline(x, y, col = "pink", lwd = 1.5)
                  }))


(xy_slope = xyplot(DCA1 ~ slope | year, data = data, pch = 16, as.table = TRUE,
                   panel = function(x, y, ...) {
                     panel.points(x, y, ...)
                     panel.loess(x, y, span = 0.7, col = gray(0.6), lwd = 1.5)
                     panel.lmline(x, y, col = "pink", lwd = 1.5)
                   }))

#**********************************************************
# 4 MODELING AND SPATIAL CV--------------------------------
#**********************************************************
  
# define a formula
data$year = as.factor(data$year)
m_1 = gam(DCA1 ~ s(ndvi) + year, data = data)
m_2 = gam(DCA1 ~ s(ndvi, by = year) + year, data = data)
AIC(m_1, m_2)  # interaction model way better
summary(m_2)
vis.gam(m_2, plot.type = "perspective")

fo = as.formula(DCA1 ~ s(ndvi, by = year) + year)
# run spatial cv
out = sperrorest(data = data, coords = c("x", "y"), formula = fo,
                 # par_args = list(par_mode = "sequential"),
                 model_fun = gam,
                 # next line in the case of mgcv::gam not necessary but it's 
                 # good to be explicit
                 model_args = list(family = "gaussian"),
                 pred_fun = predict,
                 # next line in the case of mgcv::gam not necessary but it's 
                 # good to be explicit
                 pred_args = list(type = "response"),
                 smp_fun = partition_kmeans,
                 smp_args = list(repetition = 1:100, nfold = 5),
                 err_rep = TRUE, err_fold = FALSE,
                 err_fun = err_nrmse)
summary(out$error_rep)[c("train_rmse", "test_rmse", 
                         "train_nrmse", "test_nrmse"), "mean"]
# that's not really convincing...
# (train_rmse: 0.56)
# test_rmse: 3.4 (with a data range of diff(range(data$DCA1)) = 4.43)
# (train_rmse: 12.7%)
# test_nrmse: 77% (2012: 11.9%)

# use partition_kmeans_factor
out = sperrorest(data = data, coords = c("x", "y"), formula = fo,
                 model_fun = gam,
                 pred_fun = predict,
                 smp_fun = partition_kmeans_factor,
                 smp_args = list(repetition = 1:100, nfold = 5, fac = "id"),
                 err_rep = TRUE, err_fold = FALSE,
                 err_fun = err_nrmse)
summary(out$error_rep)[c("test_rmse", "test_nrmse"), "mean"]
# test_nrmse: 67%
# test_rmse: 2.95

# CONCLUSION - this is not convincing -> overfitting -> restrict k to a
# predefined value

fo = as.formula(DCA1 ~ s(ndvi, by = year, k = 3) + year)
# fo = DCA1 ~ ndvi * year  # GLM with an interaction term
gam_1 = gam(fo, data = data)
summary(gam_1)
out = sperrorest(data = data, coords = c("x", "y"), formula = fo,
                 model_fun = gam,
                 pred_fun = predict,
                 smp_fun = partition_kmeans_factor,
                 smp_args = list(repetition = 1:100, nfold = 5, fac = "id"),
                 err_rep = TRUE, err_fold = FALSE,
                 err_fun = err_nrmse)
summary(out$error_rep)[c("test_rmse", "test_nrmse"), c("mean", "sd")]

# way better, though still just a mediocre result
# test_rmse: ca 1.2
# test_nrmse: ca. 27%

#**********************************************************
# 5 PREDICTIVE MAPPING-------------------------------------
#**********************************************************

data$year = as.factor(data$year)
fo = as.formula(DCA1 ~ s(ndvi, by = year, k = 3) + year)
gam_1 = gam(fo, data = data)

pred = pred_fun(data = data, model = gam_1, year = 2011,
                ndvi_raster = ndvi$ndvi_11)

# # just checking if we retrieve the same result as the raster prediction
# library(sp)
# ndvi_df = data.frame(ndvi = values(ndvi$ndvi_17), coordinates(ndvi$ndvi_17))
# ndvi_df$year = 2017
# ndvi_df$DCA1 = predict.gam(gam_1, newdata = ndvi_df, type = "response")
# # reconvert to a spatial raster dataset
# coords = SpatialPoints(ndvi_df[, c("x","y")])
# ndvi_spdf = SpatialPointsDataFrame(coords, ndvi_df)
# result = rasterFromXYZ(as.data.frame(ndvi_spdf)[, c("x", "y", "DCA1")])
# # plot(result)
# all.equal(pred, result)  # TRUE


plot(pred)
# ok, the values in the west are way too high, why is that
d_11 = filter(data, year == 2011)
fo = as.formula(DCA1 ~ s(ndvi, k = 3))
gam_11 = gam(fo, data = d_11)
summary(gam_11)
summary(predict(gam_11))
summary(d_11$ndvi)  
# that's the problem, we only observe NDVI values between 0.03 and 0.35
# hence, we are predicting outside of the observed range...
y = ndvi$ndvi_11
values(y) = data[data$year == 2011, "year"][1]
s = stack(ndvi$ndvi_11, y)
names(s) = c("ndvi", "year")
pred = predict(s, gam_11, type = "response")
summary(pred)
plot(pred)

# Possible solutions:
# 1. use a GLM for each year (however, this decreases R^2 and makes sperrorest worse)
# 2. just use a GLM for 2011
# 3. mask out all areas > 4 in 2011
# 4. mask out all areas > 4 in 2011 and show 4 plots (instead of one RGB plot)

# for now, use the GAM
pred = lapply(c(2011, 2012, 2016, 2017), function(i) {
  ind = gsub(20, "", i)
  ind = paste0("ndvi_", ind)
  pred_fun(data, gam_1, i, ndvi[[ind]])
}) %>%
  stack
cellStats(pred, summary)

white = rgb(255, 255, 255, maxColorValue = 255)
blue = rgb(0, 0, 146, maxColorValue = 255)
lightblue = rgb(0, 129, 255, maxColorValue = 255)
turquoise = rgb(0, 233, 255, maxColorValue = 255)
green = rgb(142, 255, 11, maxColorValue = 255)
yellow = rgb(245, 255, 8, maxColorValue = 255)
orange = rgb(255, 173, 0, maxColorValue = 255)
lightred = rgb(255, 67, 0, maxColorValue = 255)
red = rgb(170, 0, 0, maxColorValue = 255)
pal = colorRampPalette(c(blue, lightblue, turquoise, green, yellow, 
                         orange, lightred, red))

brks = seq(-3, 4.5, length = 20)
cols = pal(length(brks) - 1)

p = spplot(pred, col.regions = cols, at = brks,
       strip = strip.custom(
         bg = "white", 
         factor.levels = c("2011", "2012", "2016", "2017"),
         par.strip.text = list(cex = 0.8)),
       scales = list(
         alternating = c(1, 0),
         tck = c(1, 0)),
       colorkey = list(width = 1, height = 0.4))
# now add also towns and river to the plot
poly = st_as_sf(rasterToPolygons(pred$layer.1))
poly = st_union(poly)
rivers = st_transform(rivers, st_crs(poly))
rivers = st_intersection(rivers, poly)
rivers = filter(rivers, HYC_DESCRI == "Perennial/Permanent")
plot(poly)
plot(rivers, add = TRUE, col = "blue", lwd = 2)  # ok, excellent
towns = st_transform(towns, 4326)
p = p + 
  latticeExtra::layer(sp.polygons(as(towns, "Spatial"), fill = "black")) + 
  latticeExtra::layer(sp.lines(as(rivers, "Spatial"), lwd = 4, col = "black")) +
  latticeExtra::layer(sp.lines(as(rivers, "Spatial"), lwd = 2, col = "lightblue"))
# save the figure
# png(p, filename = "figures/15_pred_map.png", width = 16, height = 12, 
#     units = "cm",
#     res = 300)
print(p)
# dev.off()
save(p, towns, rivers, file = here("figures/15_pred_map.rda"))

# set values > 4 to NA in the 2011 prediction
tmp = pred$layer.1[pred$layer.1 <= 4, drop = FALSE]
tmp = extend(tmp, pred)
pred$layer.1 = tmp
names(pred) = paste("pred", c(11, 12, 16, 17), sep = "_")
# saveRDS(pred, "images/15_pred_rasters.rds")

#**********************************************************
# 6 MODELING ALPHA DIVERSITY----
#**********************************************************

spri = readRDS(here("images/13_spri.rds"))
ndvi = readRDS("images/00_ndvi_rasters.rds")
dist_sea = readRDS("images/18_dist_sea_raster.rds")
dist_sea = projectRaster(dist_sea, crs = proj4string(ndvi))

# 6.1 little data exploration====
#**********************************************************
# extract NDVI to points
spri$ndvi = NA
for (i in c(11, 12, 16, 17)) {
  ind = grepl(paste0(20, i), spri[, "year"]) 
  spri[ind, "ndvi"] = 
    raster::extract(ndvi[[paste0("ndvi_", i)]], 
                    spri[ind, c("lon", "lat")])
}
sum(is.na(spri$ndvi))  # 0, very good

# data exploration
library("ggplot2")
plot_fun = function(x, y = "spri", data = spri) {
  ggplot(data, aes(x = rlang::ensym(x) %>% rlang::eval_tidy(), 
                   y = rlang::ensym(y) %>% rlang::eval_tidy(), 
                   group = year)) +
    geom_line(aes(color = as.factor(year)), alpha = 0.4) + 
    labs(color = "year") +
    xlab(label = x) +
    ylab(label = y) +
    geom_smooth(aes(color = as.factor(year)), se = FALSE) + 
    theme_bw()
}

plot_fun(x = "pnr")
plot_fun(x = "ndvi")
plot_fun(x = "dist_sea")

# 6.2 Spatial CV====
#**********************************************************

spri$year = as.factor(spri$year)
# spri$year = as.character(spri$year)
fo = as.formula(spri ~ s(dist_sea, by = year, k = 3) + year)
gam_1 = gam(fo, data = spri, family = "poisson")
summary(gam_1)
fo_glm = as.formula(spri ~ dist_sea:year + year)
glm_1 = glm(fo_glm, data = spri, family = "poisson")
summary(glm_1)
out = sperrorest(data = spri, 
                 coords = c("lon", "lat"), 
                 formula = fo_glm,
                 model_fun = glm,
                 model_args = list(family = "poisson"),
                 pred_fun = predict,
                 smp_fun = partition_kmeans,
                 smp_args = list(repetition = 1:100, nfold = 5),
                 # smp_fun = partition_keans_factor,
                 # smp_args = list(repetition = 1:100, nfold = 5, fac = "pnr"),
                 err_fun = err_nrmse)
summary(out$error_rep)[c("test_rmse", "test_nrmse"), c("mean", "sd")]
# nrmse of gam ndvi with s(k = 3): ~40%  # not very convincing, but ok
# nrmse of glm ndvi: ~35%
# glm was not better and the prediction map looked worse

# nrmse of gam dist_sea with s(k = 3): ~36%
# nrmse of glm dist_sea: ~36%

# 6.3 Spatial prediction====
#**********************************************************

spri$year = as.factor(spri$year)
fo_dist = as.formula(spri ~ s(dist_sea, by = year, k = 3) + year)
fo_ndvi = as.formula(spri ~ s(ndvi, by = year, k = 3) + year)

gam_ndvi = gam(fo_ndvi, data = spri, family = "poisson")
gam_dist = gam(fo_dist, data = spri, family = "poisson") 
names(dist_sea) = "dist_sea"

# making two alpha diversity predictions, one using ndvi and a second using
# distance to the sea as predictors
pred = lapply(c(2011, 2012, 2016, 2017), function(i) {
  ind = gsub(20, "", i)
  ind = paste0("ndvi_", ind)
  pred_ndvi = pred_fun(spri, gam_ndvi, i, ndvi[[ind]])
  pred_dist = pred_fun(spri, gam_dist, i, dist_sea)
  list(pred_ndvi, pred_dist)
}) 

pred_ndvi = lapply(pred, `[[`, 1) %>% stack
pred_dist = lapply(pred, `[[`, 2) %>% stack

pal = colorRampPalette(c(white, yellow, orange, lightred, red))
brks = seq(0, 40, length = 20)
cols = pal(length(brks) - 1)
# ok, now we are using the same function for the 2nd, 3rd time -> write a
# function
projection(pred_ndvi) = proj4string(as(towns, "Spatial"))
towns_map = st_crop(towns, st_bbox(pred_ndvi))

spri_ndvi = spplot(pred_ndvi, col.regions = cols, at = brks,
           strip = strip.custom(
             bg = "white", 
             factor.levels = c("2011", "2012", "2016", "2017"),
             par.strip.text = list(cex = 0.8)),
           scales = list(
             alternating = c(1, 0),
             tck = c(1, 0)),
           colorkey = list(width = 1, height = 0.4)) +
  latticeExtra::layer(sp.polygons(as(towns_map, "Spatial"), fill = "black")) + 
  latticeExtra::layer(sp.lines(as(rivers, "Spatial"), lwd = 4, col = "black")) +
  latticeExtra::layer(sp.lines(as(rivers, "Spatial"), lwd = 2, col = "lightblue"))

brks = seq(0, 40, length = 80)
cols = pal(length(brks) - 1)
# make disappear the predicted values in the Pacific Ocean
pred_dist = resample(pred_dist, pred_ndvi)
pred_dist = mask(pred_dist, pred_ndvi)
spri_dist = spplot(pred_dist, col.regions = cols, at = brks,
           strip = strip.custom(
             bg = "white", 
             factor.levels = c("2011", "2012", "2016", "2017"),
             par.strip.text = list(cex = 0.8)),
           scales = list(
             alternating = c(1, 0),
             tck = c(1, 0)),
           colorkey = list(width = 1, height = 0.4)) +
  latticeExtra::layer(sp.polygons(as(towns_map, "Spatial"), fill = "black")) + 
  latticeExtra::layer(sp.lines(as(rivers, "Spatial"), lwd = 4, col = "black")) +
  latticeExtra::layer(sp.lines(as(rivers, "Spatial"), lwd = 2, col = "lightblue"))

# save the figures
save(spri_ndvi, spri_dist, towns_map, rivers,
     file = here("figures/15_spri_preds.rda"))

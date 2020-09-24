# Filename: 00_modis_download.R (2018-02-22)
#
#
# Author(s): Jannes Muenchow, Gregor Didenko
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. (INSTALLATION) SET UP
# 2. DOWNLOAD AND PREPROCESS MODIS
# 3. LOAD MODIS FILES FROM DISK AND CREATE MEAN VALUE RASTERS
# 4. CREATE NDVI AT Plots.RDS

#**********************************************************
# 1 ATTACH PACKAGES AND (INSTALLATION) SET UP--------------
#**********************************************************

# create an Earthdata account and store username and password as described in the file README.Rmd:
# https://urs.earthdata.nasa.gov/home

# optional: download MODIS Reprojection Tool (MRT) from
# https://lpdaac.usgs.gov/tools/modis_reprojection_tool

library("MODIS")
library("dplyr")
library("sf")
library("raster")
library("RSQLite")
library("here")
devtools::load_all()

# Note: Download GDAL from http://www.gisinternals.com/release.php
# specify path to GDAL and paths
# to enable hdf4 support on Arch, run trizen -s gdal-hdf4

MODISoptions(
  # path to GDAL on your computer, only necessary for Windows
  # gdalPath = "C:/Program Files/GDAL",
  # gdalPath = "C:/OSGeo4W64/bin",
  # where to save downloaded MODIS data
  localArcPath = here::here("data/raw_data/modis"),
  # where to save processed (projected, cropped and resampled) data
  outDirPath = here::here("data/processed/modis"),
  checkTools = TRUE,
  MODISserverOrder = c("LAADS"), #c("LPDAAC", "LAADS"),
  dlmethod = "auto",
  save = FALSE,
  ask = FALSE
)

# optional:
# assuming the MRT tools are installed in C:/modis_mrt
# path = Sys.getenv("PATH")
# Sys.setenv("PATH" = paste(path, "C:/modis_mrt/bin", sep = ";"))
# Sys.setenv("MRT_HOME" = "C:/modis_mrt")
# Sys.setenv("MRT_DATA_DIR" = "C:/modist_mrt/data")
# system("mrtmosaic")  # ok, should work
# # system("C:/modis_mrt/bin/mrtmosaic")  
# # check
# MODIS:::checkTools('MRT')

# attach data
download_zenodo()
con = dbConnect(SQLite(), here("data/tables.gpkg"))
dbListTables(con)
sa = sf::read_sf(con, "study_area") %>%
  st_transform(crs = 4326)
dbDisconnect(con)

#**********************************************************
# 2 DOWNLOAD AND PREPROCESS MODIS--------------------------
#**********************************************************

# create file with Earthdata login credentials
file.remove(path.expand("~/.netrc")) # see https://github.com/MatMatt/MODIS/issues/75
MODIS::EarthdataLogin(usr = Sys.getenv("EARTHDATA_USER"), pwd = Sys.getenv("EARTHDATA_PASS"))

# Select the product you are interested in
prods = getProduct()
levels(prods$TOPIC)
filter(prods, TOPIC == "Reflectance")
# I guess using MOD09GQ we could compute the NDVI manually, yes, see also:
#browseURL(paste0("https://lpdaac.usgs.gov/dataset_discovery/modis/",
#                 "modis_products_table/mod09gq"))
filter(prods, TOPIC == "Vegetation Indices")
# MODIS/Terra Vegetation Indices 16-Day L3 Global 250m
product = "MOD13Q1"
# have a look at the product description
#browseURL(paste0("https://lpdaac.usgs.gov/dataset_discovery/modis/",
#                 "modis_products_table/mod13q1"))
# to find out about available bands you already need to have downloaded MODIS
# data (.hdf)
# path = file.path(opts$localArcPath, "MODIS/MOD13Q1.006/2011.03.22",
#                  "MOD13Q1.A2011081.h09v09.006.2015217154833.hdf")
# getSds(path)


# Create dates in Modis format
## years are added to date in paste-function
years = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
date_begin = as.POSIXct(as.Date(paste0("15/03/", years), format = "%d/%m/%Y"))
date_end = as.POSIXct(as.Date(paste0("30/04/", years), format = "%d/%m/%Y"))
dates_modis = transDate(begin = date_begin, end = date_end)

## extra months May and June 2017
# years = 2017
# date_begin = as.POSIXct(as.Date(c("01/05/2017", "01/06/2017"), 
#                                 format = "%d/%m/%Y"))
# date_end = as.POSIXct(as.Date(c("31/05/2017", "30/06/2017"), 
#                               format = "%d/%m/%Y"))
# dates_modis = transDate(begin = date_begin, end = date_end)


# define the extent of the studyarea
b_box = extent(sa)

# download MODQ31, select NDVI and EVI, mosaic the tiles, crop them to our study
# area, project into 4326 and save the processed output in "ndvi_peru"
# SDSstring: "11" means give back the first two bands, here NDVI and EVI, "101"
# would return the first and the third band
for(i in 1:length(dates_modis$begin)) {
  job <- paste("ndvi_peru", format(dates_modis$begin[i], "%y"), sep = "_")
  job_dir <- paste0(opts$outDirPath, job)
  if (dir.exists(job_dir)) {
    cat("Skipping download of ", job, "\n")
  } else {
    cat("\nDownloading data into ", job_dir, "\n")
    MODIS::runGdal(
      # the processed data will be stored in a folder named like job
      job = job,
      # choose the product to download, here MOD13Q1
      product = "MOD13Q1",
      # select only the bands you need, here NDVI, EVI
      SDSstring = "11",
      collection = "006",
      begin = dates_modis$beginDOY[i],
      end = dates_modis$endDOY[i],
      # merge neighoring MODIS scenes, crop them to the extent of our study area
      extent = b_box,
      # reproject to epsg:4326
      outProj = "4326"
    )
  }
}

#**********************************************************
# 3 COMPUTE MEAN MODIS NDVI--------------------------------
#**********************************************************

ndvi_mean_raster_list = list()
ndvi_image_dates = numeric(0)
# Get all Files in Modis output folder
# load ndvi data into raster stack
# rescale to put NDVI/EVI between -1 and 1
# get mean NDVI over all images
# save raster in list				  
for (year in c(10, 11, 12, 13, 14, 15, 16, 17)) {
  # load NDVI modis data from one year into R
  file_path = paste0(opts$outDirPath, paste0("ndvi_peru_", year))
  cat("Computing mean NDVI in ", file_path, "\n")
  
  ndvi_files = grep("NDVI", list.files(file_path), value = TRUE)

  # find out when the images were taken
  dates = extractDate(ndvi_files, asDate = TRUE)$inputLayerDates
  ndvi_image_dates = append(ndvi_image_dates, dates)

  # load ndvi data into raster stack
  ndvi_stack = stack(file.path(file_path, ndvi_files))
  # rescale to put NDVI/EVI between -1 and 1
  ndvi_stack = ndvi_stack * 0.0001
  # get mean NDVI over all images. na.rm because some rasters have missing pixels
  ndvi_mean_raster = mean(ndvi_stack, na.rm = TRUE)
  # save raster in list with name of year
  ndvi_mean_raster_list[[paste("ndvi_", year, sep = "")]] = ndvi_mean_raster
}

## Add April, June, March 2017 to raster list (-> was just for exploring if
## later months (after the vegetation period) would be more related to
## vegetation development, in the end we have taken the mean over the whole
## vegetation period)
file_path = paste0(opts$outDirPath, "ndvi_peru_17")
ndvi_files = grep("NDVI", list.files(file_path), value = TRUE)

# find out when the images were taken
dates = extractDate(ndvi_files, asDate = TRUE)$inputLayerDates
ndvi_image_dates = append(ndvi_image_dates, dates)

## function to load rasterfiles, rescale to -1 and 1 and take mean over all dates
ndvi_mean_raster = function(path, files) {
  # load ndvi data into raster stack
  ndvi_stack = stack(file.path(path, files))
  # rescale to put NDVI/EVI between -1 and 1
  ndvi_stack = ndvi_stack * 0.0001
  # get mean NDVI over all images. na.rm because some rasters have missing pixels
  mean(ndvi_stack, na.rm = TRUE)
  }

# add rasters to raster list
ndvi_mean_raster_list[[c("ndvi_2017_april")]] = 
  ndvi_mean_raster(file_path, ndvi_files[1:3])
ndvi_mean_raster_list[[c("ndvi_2017_may")]] =  
  ndvi_mean_raster(file_path, ndvi_files[4:5])
ndvi_mean_raster_list[[c("ndvi_2017_june")]] =
  ndvi_mean_raster(file_path, ndvi_files[6:7])

# Test plot
ndvi_rasters = brick(ndvi_mean_raster_list)
plot(ndvi_rasters)

# save rasterstack to RDA
saveRDS(ndvi_rasters, here::here("images/00_ndvi_rasters.rds"))
# # save image for report as markdown-document
save(ndvi_rasters, ndvi_image_dates,
     file = here::here("docu/ndvi_dca_plots/ndvi.RData"))

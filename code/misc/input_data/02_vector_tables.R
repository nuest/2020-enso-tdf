# Filename: vector.R (2018-07-03)
#
# TO DO: putting several needed vector data into gpkg
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. GPKG
# 3. NOMINATIM CITY BOUNDARIES
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("sf")
library("raster")
library("RSQLite")
# define directory
old_dir = file.path("~/work/acad/science/projects/ecology/latin_america/peru/", 
                    "transect_piura/old/data/R/R data/shapes")
# read in data
towns = st_read(file.path(old_dir, "towns.shp"))
plot(towns$geometry)  # these were the town outlines used in the Erdkunde paper
towns_2 = st_read(file.path(old_dir, "towns_hannes.shp"))
plot(towns_2$geometry, add = TRUE)  # and these were the ones used in the JVS paper
streets = st_read(file.path(old_dir, "streets.shp"))
arable = st_read(file.path(old_dir, "arable_land.shp"))
mountains = st_read(file.path(old_dir, "mountains.shp"))
sa = st_read(file.path(old_dir, "study_area.shp"))
coast = st_read(file.path(old_dir, "coastline.shp"))
# water lines were downloaded from DIVA GIS
rivers = st_read(file.path(old_dir, "PER_wat/PER_water_lines_dcw.shp"))
rivers = st_transform(rivers, st_crs(sa))
# neighboring countries
tmp = ccodes()
dplyr::filter(tmp,
              NAME %in% c("Peru", "Ecuador", "Brazil", "Colombia", "Bolivia", 
                          "Chile"))
nbs = lapply(c("BOL", "BRA", "CHL", "COL", "ECU"), function(i) {
  getData("GADM", country = i, level = 0) %>%
    st_as_sf
  })
nbs = do.call(rbind, nbs)
plot(nbs$geometry, col = NA)
per = getData("GADM", country = "PER", level = 1) %>% 
  st_as_sf

#**********************************************************
# 2 GPKG---------------------------------------------------
#**********************************************************

# # we already have written one file into the gpkg named towns, delete it
# gdal = link2GI::linkGDAL()  # add GDAL to PATH
# system("ogrinfo", intern = TRUE)  # check
# path = file.path(getwd(), "data/data.gpkg")
# st_layers("data/data.gpkg")
# system(paste("ogrinfo", path))  # this works
# # paste("ogrinfo", "-sql 'drop table towns'", path)  # does not work!!!
# cmd = paste("ogrinfo", '-sql "drop table towns"', path)
# system(cmd, intern = TRUE)
# # maybe more appropriate since it also deletes all metadata & Co. 
# # browseURL("https://lists.osgeo.org/pipermail/gdal-dev/2016-September/045243.html")
# # cmd = paste0("ogrinfo -sql 'DELLAYER:towns'", path)
# # system(cmd, intern = TRUE)

# write all vector files into table.gpkg (already contains non-spatial tables)
con = dbConnect(RSQLite::SQLite(), "data/tables.gpkg")
dbListTables(con)

st_write(towns, con, "towns_erdk")
st_write(towns_2, con, "towns_jvs")
st_write(streets, con, "streets")
st_write(arable, con, "arable", update = TRUE,
         layer_options = "OVERWRITE=YES")
st_write(mountains, con, "mountains")
st_write(sa, con, "study_area")
st_write(rivers, con, "rivers")
st_write(coast, con, "coast")
st_write(per, con, "peru")
st_write(nbs, con, "neighbors", update = TRUE)
# check if all tables (spatial and non-spatial are available)
RSQLite::dbListTables(con)  # excellent
RSQLite::dbDisconnect(con)

#**********************************************************
# 3 NOMINATIM CITY BOUNDARIES------------------------------
#**********************************************************

# trying to get cit outlines using Nominatim
paita = osmdata::getbb("paita peru", format_out = "sf_polygon")
piura = osmdata::getbb("piura peru", format_out = "sf_polygon")
chulu = osmdata::getbb("chulucanas peru", format_out = "sf_polygon")
towns = rbind(paita, piura, chulu)
towns = st_transform(towns, st_crs(streets))
plot(towns$geometry)
plot(streets$geom)
plot(towns$geometry, add = TRUE, col = "red")
# not really convincing, way too big
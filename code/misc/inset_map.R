# Filename: study_area.R (2019-06-03)
#
# TO DO: Overview figure (South America and prediction map)
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. SOUTH AMERICA STUDY AREA
# 3. LOCAL PREDICTION MAP
# 4. GRID FIGURE
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("sf")
library("sp")
library("raster")
library("dplyr")
library("latticeExtra")
library("grid")
library("gridExtra")
library("RSQLite")

# # attach data
cous = c("PER", "ECU", "BOL", "COL", "BRA", "CHL")
cous = lapply(cous, function(x) {
  shp = raster::getData(cou = x, level = 0) %>%
    st_as_sf
})
cous = do.call(rbind, cous)
b_box = st_bbox(cous[1:2, ])
b_box["xmin"] = st_bbox(cous[1, ])$xmin
# add something, otherwise Bolivia will become a gc which cannot be converted
# into a Spatial object
b_box["xmax"] = st_bbox(cous[1, ])$xmax + 0.5
sa = st_crop(cous, b_box)
# check
sa = as(sa, "Spatial")  # ok, it works
# saveRDS(sa, file = "images/sa.rds")

# con = dbConnect(SQLite(), "data/tables.gpkg")
# dbListTables(con)
# peru = st_read(con, "peru")
# nbs = st_read(con, "neighbors")
# 
# sa = rbind(nbs, select(peru, GID_0, NAME_0))
# sa = as(sa, "Spatial")  

#*******************************************************
# 2 SA inset map----------------------------------------
#*******************************************************

poly = st_bbox(c(xmin = -81, xmax = -79, ymax = -4.6, ymin = -5.4), 
               crs = st_crs(4326)) %>% 
  st_as_sfc %>% 
  as(., "Spatial")

# Base plot version

# however, you cannot mix base plots and lattice/ggplots with grid

# pin = par("pin")
# # calculate the x- and y-extent
# dxy = st_bbox(sa) %>% diff(., 2)
# ratio = dxy[1] / dxy[2]
# # set the plot dimensions in inches (pin)
# # png("figures/study_area.png", width = 9, height = 12, units = "cm", res = 300)
# par(pin = c(ratio * pin[2], pin[2]), xaxs = "i", yaxs = "i")
# plot(st_geometry(st_as_sf(sa[1:2, ])), bg = "lightblue", col = "gray",
#      border = "gray")
# plot(st_geometry(sa), col = "gray", border = "gray", add = TRUE)
# axis(1, at = c(-80, -70))
# axis(2)
# box()
# plot(st_geometry(sa[1:2, ]), col = "white", border = gray(0.2), add = TRUE)
# text(-76, -10, "Peru", cex = 1, font = 2)
# text(-78, -0.5, "Ecuador", cex = 1, font = 2)
# plot(poly, add = TRUE, lwd = 2, col = "salmon")
# text(x = -77.5, y = -5.2, "local\nstudy area", font = 3)
# # dev.off()

# spplot version
p_sa = 
  spplot(sa, 1, colorkey = FALSE, col = NA,
         scales = list(tck = c(1, 0),
                       x = list(at = c(-80, -75, -70),
                                labels = c(-80, -75, -70))
         ),
         par.settings = list(panel.background = list(col = "lightblue")),
         sp.layout = list(
           list("sp.polygons", 
                sa[3:6, ],
                col = NA, fill = "lightgray",
                first = FALSE),
           list("sp.polygons", 
                sa[1:2, ],
                col = "darkgray", fill = "white",
                first = FALSE),
           list("sp.text", loc = c(-76, y = -10), txt = "Peru", cex = 1,
                font = 2),
           list("sp.text", loc = c(-78, -0.5), txt = "Ecuador", cex = 1, 
                font = 2),
           list("sp.polygons", poly, fill = "salmon", first = FALSE),
           list("sp.text", loc = c(-77.5, -5.2), txt = "local\nstudy area", 
                font = 3)
         )
  )
p_sa

#**********************************************************
# 3 PREDICTION MAP-----------------------------------------
#**********************************************************

# reproject raster (otherwise shadowtext labeling won't work)
pred_16 = projectRaster(pred$pred_16, crs = st_crs(towns)$proj4string)
# crop towns to the extent of the bbox of the prediction raster
towns = st_crop(towns, st_bbox(pred_16) - c(0, -1100, 0, 1000))
# check
# plot(pred_16)
# plot(towns, add = TRUE)
  
# prediction color scale
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
p_pred = 
  spplot(pred_16, col.regions = cols, at = brks,
         # scales = list(draw = TRUE),
         colorkey = list(space = "bottom", width = 0.8, height = 0.2),
         sp.layout = list(
           # list("sp.points", clus, col = "black", pch = 21, cex = 0.7),
           list("sp.polygons", 
                as(towns, "Spatial"),
                col = "black", fill = "black",
                first = FALSE),
           list("sp.points",
                as(plots, "Spatial"), 
                pch = 21, fill = NA, col = "black", first = FALSE),
           list("SpatialPolygonsRescale", layout.scale.bar(),
                offset = c(480000, 9425000), 
                scale = 10000, fill = c("white", "black"), first = FALSE),
           list("sp.text", c(485000, 9427000), "10 km", cex = 0.75)
         )
  )

# use shadow text
# take care, latticeExtra::layer uses NSE, you need to use the data-argument!!!
# See ?layer and:
# browseURL(paste0("https://procomun.wordpress.com/2013/04/24/", 
#                  "stamen-maps-with-spplot/))
# browseURL(paste0("https://gist.github.com/oscarperpinan/",
#                  "7482848#file-stamenpolywithlayerinfunction4-r"))
# shadowtext by Barry Rowlingson
# browseURL(paste0("http://blog.revolutionanalytics.com/2009/05/",
#                  "make-text-stand-out-with-outlines.html"))
# needs some adjustment for lattice, strwidth and strheight replace by
# stringWidth and stringHeight (gridExtra), see
# browseURL("https://stat.ethz.ch/pipermail/r-help/2004-November/061255.html")
coords = st_centroid(towns) %>% st_coordinates
metro_names = c("Chulu-\ncanas", "Piura", "Paita")

theta = seq(pi / 4, 2 * pi, length.out = 8)
xy = xy.coords(coords)
xo = -25 * convertWidth(stringWidth("A"), unitTo = "native", valueOnly = TRUE)
yo = -25 * convertWidth(stringHeight("A"), unitTo = "native", valueOnly = TRUE)
p_pred_2 = p_pred + 
  latticeExtra::layer(
    for (i in theta) {
      ltext(x = xy$x + cos(i) * xo, y = xy$y + sin(i) * yo, 
            labels = metro_names, col = "black", font = 3, cex = 0.8)
    },
    data = list(xy = xy, metro_names = metro_names, theta = theta,
                xo = xo, yo = yo)
  ) +
  latticeExtra::layer(
    ltext(x = xy$x, y =  xy$y, labels = metro_names, col = "white", cex = 0.8, 
          font = 3),
    data = list(xy = xy, metro_names = metro_names)
  )



#**********************************************************
# 4 GRID FIGURE--------------------------------------------
#**********************************************************

lo = grid.layout(nrow = 3, ncol = 2,
                 # column width
                 widths = c(0.5, 0.5),
                 # row height
                 heights = c(0.5, 0.5))
grid.show.layout(lo)
dev.off()

png(filename = "figures/study_area.png", width = 15, height = 13, units = "cm",
    res = 300)
pushViewport(viewport(layout = lo))
pushViewport(viewport(layout.pos.col = 1:2, layout.pos.row = 1:2))
print(p_sa, newpage = FALSE)
popViewport()
# grid.text("a", x = 0.3, y = 0.9, gp = gpar(font = 2))
pushViewport(viewport(layout.pos.col = 1:2, layout.pos.row = 3))
print(p_pred_2, newpage = FALSE)
popViewport()
pushViewport(viewport(layout.pos.col = 1:2, layout.pos.row = 1:3))
grid.lines(x = c(0.395, 0.05), y = c(0.75, 0.3))
grid.lines(x = c(0.44, 0.95), y = c(0.75, 0.3))
dev.off()


# Filename: 12_study_area_ndvi.R (2019-10-15)

# TO DO: Create study area map including an inset map, a ternary plot and
#        precipitation rain bars

# Author(s): Gregor Didenko, Jannes Muenchow, Patrick Schratz

#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************

# 1. ATTACH PACKAGES AND DATA
# 2. STUDY AREA (NDVI BACKGROUND)
# 3. OVERVIEW MAP
# 4. TERNARY PLOT
# 5. PRECIPITATION BARPLOT
# 6. GRID OF PLOTS

#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packgaes
library("cowplot")
library("data.table")
library("dplyr")
library("ggpubr")
library("gridGraphics")
library("here")
library("osmdata")
library("lattice")
library("png")
library("raster")
library("RCurl")
library("RSQLite")
library("sf")
library("Ternary")
library("magrittr")

# attach data
# get NDVI Rasters
ndvi_rasters = readRDS("images/00_ndvi_rasters.rds")
## get streets
con = dbConnect(SQLite(), "data/tables.gpkg")
dbListTables(con)
# set all extents to raster-extent
streets = st_read(con, "streets") %>%
  st_transform(crs = st_crs(ndvi_rasters)) %>%
  st_crop(st_bbox(ndvi_rasters))
street = streets[1, ]

## get town polygons
towns = st_read(con, "towns_jvs") %>%
  st_transform(crs = st_crs(ndvi_rasters)) %>%
  st_crop(st_bbox(ndvi_rasters))
# get center of towns-polygon
towns_label = st_centroid(towns) %>%
  st_coordinates() %>%
  as.data.frame()
# adjust for better visualibility
towns_label[1, 2] = towns_label[1, 2] + 0.024
towns_label[3, 2] = towns_label[3, 2] - 0.005
towns_label$towns = c("Chulucanas", "Piura", "Paita")

## get rivers
rivers = st_read(con, "rivers") %>%
  st_transform(crs = st_crs(ndvi_rasters)) %>%
  st_crop(st_bbox(ndvi_rasters))
river = rivers[1:8, ]

# get coords of study-sites
site_data = readRDS("images/11_topo.rds")

# get study area
study_area = st_read(con, "study_area") %>%
  st_transform(crs = 4326)
study_area_block = study_area %>%
  st_bbox() %>%
  st_as_sfc()

peru = st_read(con, "peru") %>%
  st_union() %>%
  st_crop(xmin = -82, ymin = -8, xmax = -71, ymax = -1)
# plot(peru)

neighbors = st_read(con, "neighbors") %>%
  dplyr::filter(NAME_0 %in% c("Ecuador", "Brazil", "Colombia")) %>%
  st_crop(xmin = -81.5, ymin = -10, xmax = -55, ymax = 3) %>%
  st_union()
# plot(neighbors)

#************************************************
# 2 NDVI RGB and Study-Site Map ----
#************************************************

# create raster stack of selected years
ndvi_stack = stack(
  ndvi_rasters$ndvi_11, ndvi_rasters$ndvi_12,
  ndvi_rasters$ndvi_16, ndvi_rasters$ndvi_17
)
names(ndvi_stack) = c("ndvi_2011", "ndvi_2012", "ndvi_2016", "ndvi_2017")

# NDVI deltas
ndvi_delta_12_11 = abs(ndvi_rasters$ndvi_12 - ndvi_rasters$ndvi_11)
ndvi_delta_16_11 = abs(ndvi_rasters$ndvi_16 - ndvi_rasters$ndvi_11)
ndvi_delta_17_11 = abs(ndvi_rasters$ndvi_17 - ndvi_rasters$ndvi_11)

d_ndvi_stack_11 = stack(ndvi_delta_12_11, ndvi_delta_16_11, ndvi_delta_17_11)
names(d_ndvi_stack_11) = c("d_12_11", "d_16_11", "d_17_11")

# to adjust ndvi Value to 0-255
coef = 255 / max(values(d_ndvi_stack_11), na.rm = TRUE)

ndvi_raster = d_ndvi_stack_11 * coef

# create town label sf
towns_label_sf = st_as_sf(towns_label, coords = c("X", "Y"))
towns$name = c("Chulucanas", "Piura", "Paita")

# transform study-sites data frame to sf
site_data_sf = st_as_sf(site_data, coords = c("lon", "lat"), crs = 4326)

## offsets for plot ids
nudge_y = c(
  1, 0, 1, 0, -1, 0, 0, -1, 1, 0, # 10
  -1, 1, 0, 1, 0, -1, 1, 1, 0, 0, # 20
  1, -1, -1, 1, 0, 0, 0, 0, 1, -1, # 30
  0, 0, 1, 1, 0, -1, 1, 0, 0, 0, # 40
  0, -1, 1, -1, 0, 1, -1, 0, -1, 0
) * 0.25
nudge_x = c(
  0, 1, 0, 1, 0, 1, 1, 0, 0, -1,
  0, 0, 1, 0, -1, 0, 0, 0, -1, 1,
  0, 0, 0, 0, 1, -1, 1, -1, 0, 0,
  1, 1, 0, 0, 1, 0, 0, 1, -1, 1,
  -1, 0, 0, 0, 1, 0, 0, 1, 0, 1
) * 0.25

map_study_area =
  RStoolbox::ggRGB(ndvi_raster, 2, 3, 1, geom_raster = TRUE, scale = 255) +
  geom_sf(data = river, aes(color = "lightblue"), show.legend = "line") +
  geom_sf(data = street, aes(color = "lightgoldenrod"), show.legend = "line") +
  geom_sf(data = towns, aes(color = "black", fill = "grey40"), show.legend = "line") +
  geom_sf_label(data = towns, aes(label = name), size = 2) +
  geom_sf_text(
    data = site_data_sf, aes(label = pnr),
    size = 2.5, color = "white"
  ) +
  scale_x_continuous(
    breaks = c(-81.2, -81, -80.8, -80.6, -80.4, -80.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = c(-5.05, -5.10, -5.15, -5.20),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    values = c("black", "lightblue", "lightgoldenrod"),
    labels = c("Town", "River", "Street")
  ) +
  labs(x = NULL, y = NULL, color = "Legend") +
  ggpubr::theme_pubr(base_size = 7) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))


#************************************************
# 3 OVERVIEW MAP---------------------------------
#************************************************

# create country labels sf
overview_labels_sf = data.frame(
  "name" = c(
    "Ecuador", "Peru",
    "Brazil", "Study area"
  ),
  "long" = c(-78.5, -76, -72.2, -80),
  "lat" = c(-2.5, -5.5, -6.5, -5.5)
) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)


map_overview = ggplot() +
  geom_sf(data = peru, aes(fill = "lightgrey")) +
  geom_sf(data = study_area_block, aes(fill = "black"), color = "black", size = 2.5) +
  geom_sf_label(
    data = overview_labels_sf, aes(label = name),
    size = 2, nudge_y = 1.5
  ) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(breaks = c(-2, -5, -8)) +
  scale_x_continuous(breaks = c(-78, -74)) +
  scale_fill_manual(
    values = c("black", "lightgrey"),
    labels = c("Study area", "Peru")
  ) +
  theme_pubr(base_size = 7) +
  guides(fill = FALSE, color = FALSE)

#************************************************
# 4 TERNARY PLOT---------------------------------
#************************************************

# get values for study-sites
study_sites_ndvi = 
  raster::extract(d_ndvi_stack_11,
                  matrix(c(site_data$lon, site_data$lat), ncol = 2))

par(bg = NA)
png("figures/12_ternary_foreground.png",
  res = 200, bg = NA, height = 900,
  width = 900
)
TernaryPlot(
  grid.minor.lines = 0,
  alab = "2017 - 2011\n%\n\n",
  blab = "2016 - 2011\n%\n\n",
  clab = "\n%\n2012 - 2011",
  lab.offset = 0.18,
  axis.cex = 1, padding = 0.2, lab.cex = 0.75
)

# Or use points instead of text - unsure which one is better
# TernaryPoints(study_sites_ndvi[, c(3, 2, 1)], pch=16, cex=0.5, col = "white")
# TernaryText(study_sites_ndvi[, c(3, 2, 1)], cex = 0.5, col = "white")
dev.off()

# ref: https://stackoverflow.com/a/11810691/4185785
png("figures/12_ternary_background.png",
  res = 200, bg = NA, height = 1200,
  width = 1200
)
plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), asp = 1, bty = "n", axes = FALSE,
     xlab = "", ylab = "")
sm = 500
x = do.call(c, sapply(
  1:(sm * sqrt(3) / 2) / sm,
  function(i) (i * sm / sqrt(3)):(sm - i * sm / sqrt(3)) / sm
))
y = do.call(c, sapply(
  1:(sm * sqrt(3) / 2) / sm,
  function(i) rep(i, length((i * sm / sqrt(3)):(sm - i * sm / sqrt(3))))
))
d.red = y
d.green = abs(sqrt(3) * x - y) / sqrt(3 + 1)
d.blue = abs(-sqrt(3) * x - y + sqrt(3)) / sqrt(3 + 1)
points(x, y, col = rgb(d.green, d.red, d.blue), pch = 19)
dev.off()

## overlay color triangle with color plot
back = as.raster(readPNG("figures/12_ternary_background.png"))
front = as.raster(readPNG("figures/12_ternary_foreground.png"))

# set plot parameters
par(mar = c(0, 0, 0, 0))
png("figures/12_ternary_full.png",
  res = 200, height = 900,
  width = 900
)
plot(back)
par(new = TRUE)
plot(front, xlim = c(240, 700), ylim = c(215, 750))
dev.off()

#**********************************************************
# 5 PREC BARPLOT-------------------------------------------
#**********************************************************

# remove years 1991-1996, they only exist for piura
# precip %<>% filter(!year %in% 1991:1996)

# 5.1 Download and prepare ONI index=======================
#**********************************************************

# read in the individual sheets with specified range
con = dbConnect(RSQLite::SQLite(), here("data/tables.gpkg"))
dbListTables(con)
precip = RSQLite::dbReadTable(con, "precipitation")
dbDisconnect(con)

url = paste0(
  "https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/",
  "ensostuff/ONI_v5.php"
)
doc = getURL(url)
tab = XML::readHTMLTable(doc)
tab = tab[[4]]

ind = grepl("^1990$", tab$V1)
tab_2 = tab[which(ind):nrow(tab), ]
ind = grepl("Year", tab_2$V1)
months = tab_2[ind, ][1, ] %>%
  unlist() %>%
  as.character()
names(tab_2) = tolower(months)
tab_2 = tab_2[!ind, ]
tab_2[] = lapply(tab_2, function(x) as.numeric(as.character(x)))
tab_2 = dplyr::select(tab_2, year, djf, jfm)
tab_2 = as.data.table(tab_2)
tab_2[djf >= 0.5 & jfm >= 0.5, enso := "EN"]
tab_2[djf <= -0.5 & jfm <= -0.5, enso := "LN"]
tab_2[is.na(enso), enso := "neutral"]
enso = tab_2
# save your result
# saveRDS(tab_2, here("images/13_enso_phases.rds"))

# 5.2 Barplot==============================================
#**********************************************************
# read in preprocessed data if needed
# enso = readRDS(here("images/12_enso_phases.rds"))

precip = filter(precip, year != 2018)
# summarize by year
precip = group_by(precip, station, year) %>%
  summarize(precip = sum(precip, na.rm = TRUE)) %>%
  # mutate(dev = (precip - median(precip)) / median(precip)) %>%
  mutate(ind = precip > median(precip, na.rm = TRUE)) %>%
  mutate(dev = ifelse(ind,
    precip / median(precip, na.rm = TRUE),
    1 / (precip / median(precip, na.rm = TRUE) * -1)
  )) %>%
  # precip - median(precip) can result in 0, precip / 0 = Inf -> change to 0
  mutate(dev = ifelse(is.infinite(dev), 0, dev))
precip$station = factor(precip$station, levels = c("paita", "piura", "chulu"))
levels(precip$station) = c("Paita", "Piura", "Chulucanas")

# add ENSO phases
precip = inner_join(precip, dplyr::select(enso, year, enso), by = "year")
# save your result (needed for reporting in the ms)
# saveRDS(precip, here("images/12_precip.rds"))
precip_2 = filter(precip, year > 1998)

# suggestion by Daniel
# labs = paste0(precip_2$year, " (", precip_2$enso, ")") %>%
#   unique
labs = unique(precip_2$year)
fonts = rep(1, length(labs))
fonts[grepl("201(1|2|6|7)", labs)] = 2
pal = c("pink", "lightblue", "lightgray")

precip_2 %<>% mutate(year = as.factor(as.character(year)))
bp_gg = ggplot(precip_2, aes(year, dev, fill = enso)) +
  geom_bar(stat = "identity", width = 0.5) +
  facet_wrap(~station, ncol = 3) +
  # ggsci::scale_fill_nejm() + # alternative color scale
  scale_fill_manual(values = c("pink", "lightblue", "lightgray")) +
  scale_x_discrete() +
  labs(
    y = "Ratio of precipitation\nto median precipitation",
    x = "Year", fill = ""
  ) +
  ggpubr::theme_pubr(base_size = 7) +
  ggpubr::rotate_x_text(size = 5) +
  theme(
    strip.text.x = element_text(size = 6),
    axis.text.x = element_text(face = c(rep('plain', 12),
                                        rep('bold', 2),
                                        rep('plain', 3),
                                        rep('bold', 2))),
    legend.key.size = unit(0.9, "line"),
    legend.text = element_text(size = 4)
  ) # facet label size


#************************************************
# 6 GRID OF PLOTS--------------------------------
#************************************************

# library(patchwork)
# patchwork does not work because it is not a valid ggplot extension -.-
# https://github.com/thomasp85/patchwork/issues/80

# for interactive use in plot_grid() we need to wrap the base plot into a function
# see also https://github.com/wilkelab/cowplot/issues/133
ternaryPlot = function() {
  par(mar = c(0, 0, 0, 0))
  plot(back)
  par(new = TRUE)
  plot(front, xlim = c(238, 710), ylim = c(215, 750))
}
ternary_grob = echoGrob(ternaryPlot)

bottom_row = plot_grid(bp_gg, labels = "D", label_size = 10)
middle_row = plot_grid(map_overview, ternary_grob, labels = c("B", "C"),
                       label_size = 10)
map_grid = plot_grid(map_study_area, middle_row, bottom_row,
  ncol = 1,
  labels = c("A", NULL, NULL), label_size = 10,
  rel_heights = c(1.5, 1.3, 1.8)
  # rel_heights = c(1.5, 1.3, 1.5)
)

saveRDS(map_grid, here("figures/12_map_grid.rds"))
save_plot(here("figures/12_map_grid.png"), map_grid)

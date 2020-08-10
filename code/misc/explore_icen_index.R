# Filename: 11_precipitation.R (2018-04-27)
#
# TO DO: Analyze precipitation data
#
# Author(s):  Jannes Muenchow, Gregor Didenko
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. PREPROCESSING
# 3. PREC BARPLOT
# 4. CREATE DF WITH PRECIP. SEASON SEP-AUG
# 5. PLOTTING
#
#**********************************************************
# 1 ATTACH PACKAGES AND READ DATA--------------------------
#**********************************************************

# attach packages
library("RSQLite")
library("dplyr")
library("magrittr")
library("sf")
library("raster")
library("ggplot2")
library("lattice")
library("data.table")
library("RCurl")

# read in the individual sheets with specified range
con = dbConnect(RSQLite::SQLite(), "../../../../data/tables.gpkg")
dbListTables(con)
precip = RSQLite::dbReadTable(con, "precipitation")
dbDisconnect(con)

#**********************************************************
# 3 PREC BARPLOT-------------------------------------------
#**********************************************************

# remove years 1991-1996, they only exist for piura 
# precip %<>% filter(!year %in% 1991:1996)

# 3.1 Download and prepare ICEN index======================
#**********************************************************

# ICEN phases
# ic = readr::read_table("http://www.met.igp.gob.pe/datos/icen.txt", skip = 11)
# names(ic) = c("year", "month", "icen")
# ic = mutate(ic, month = as.numeric(as.character(month)))
# saveRDS(ic, "images/00_icen_index.rds")
ic = readRDS("../../../../images/00_icen_index.rds")
ic$enso = cut(ic$icen, breaks = c(-10, -0.99, 0.41, 10),
              labels = c("LN", "neutral", "EN"))
# now aggregate to years!!
# running mean over three months, hence 1 corresponds to djf and 2 to jfm
tab_2 = filter(ic, month %in% c(1, 2))
tab_2 = reshape2::dcast(tab_2, year ~ month)
tab_2 = as.data.table(tab_2)
names(tab_2) = c("year", "djf", "jfm")
tab_2[djf == "EN" & jfm == "EN", enso := "EN"]
tab_2[djf <= "LN" & jfm == "LN", enso := "LN"]
tab_2[is.na(enso), enso := "neutral"]
# using ICEN not really convincing as it defines all years between 1999 to 2017
# as either neutral or EN
# we could also go with a majority vote... but I guess still no LN would appear
# in any case we should present this stuff to the reviewer
filter(ic, year %in% 1997:2017) %>%
  group_by(year, enso) %>%
  summarize(n()) %>%
  filter(enso == "LN") %>%
  as.data.frame
# only 2007 would be categorized as LN...

# 3.2 Barplot==============================================
#**********************************************************
# read in preprocessed data if needed
# enso = readRDS("images/13_enso_phases.rds")

precip = filter(precip, year != 2018)
precip = group_by(precip, station, year) %>%
  summarize(precip = sum(precip, na.rm = TRUE)) %>%
  # mutate(dev = (precip - median(precip)) / median(precip)) %>%
  mutate(ind = precip > median(precip, na.rm = TRUE)) %>%
  mutate(dev = ifelse(ind, 
                      precip / median(precip, na.rm = TRUE),
                      1 / (precip / median(precip, na.rm = TRUE) * -1))) %>%
  # precip - median(precip) can result in 0, precip / 0 = Inf -> change to 0
  mutate(dev = ifelse(is.infinite(dev), 0, dev)) 
precip$station = factor(precip$station, levels = c("paita", "piura", "chulu"))
levels(precip$station) = c("Paita", "Piura", "Chulucanas")
# add ENSO phases
precip = inner_join(precip, dplyr::select(tab_2, year, enso), by = "year")
precip_2 = filter(precip, year > 1998)
labs = unique(precip_2$year)
fonts = rep(1, length(labs))
fonts[labs %in% c(2011, 2012, 2016, 2017)] = 2
pal = c("pink", "lightgray")
# Barplot displaying the deviation from the long-term median

# horizontal version
bp = barchart(dev ~ as.factor(year) | station, data = precip_2, horiz = FALSE, 
              origin = 0, as.table = TRUE, group = enso, stack = TRUE,
              layout = c(3, 1), between = list(x = 0.25),
              ylab = list("Ratio of precipitation\nto median precipitation", 
                          cex = 0.8),
              scales = list(y = "same",
                            alternating = c(1, 0),
                            tck = c(1, 0),
                            x = list(at = seq_len(n_distinct(precip_2$year)),
                                     labels = labs,
                                     alternating = c(1, 1, 1),
                                     cex = 0.6, rot = 65),
                            font = fonts),
              strip = strip.custom(bg = "white",
                                   par.strip.text = list(cex = 0.8)),
              par.settings = list(superpose.polygon = 
                                    list(col = pal)),
              # auto.key = list(space = "right", cex = 0.8, columns = 1),
              key = list(points = list(pch = 22, cex = 2, border = "black", 
                                       fill = pal),
                         space = "bottom", columns = 2,
                         text = list(text = c("El Niño", "Neutral"),
                                     cex = 0.8)))


# Filename: 16_varpart.R (2019-10-15)
#
# TO DO: variation partitioning of DCA axes
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. ENV DATA EXPLORATION & TRANSFORMATION
# 3. VARIATION PARTITIONING
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("vegan")
library("dplyr")
library("lattice")

# read in functions
source("code/funs/HighstatLib.R")  # corvif
source("code/funs/varpart_helper_funs.R")

# attach data
ct_londo_11 = readRDS("images/11_ct_londo_11.rds")
ct_londo_12 = readRDS("images/11_ct_londo_12.rds")
ct_londo_16 = readRDS("images/11_ct_londo_16.rds")
ct_londo_17 = readRDS("images/11_ct_londo_17.rds")
# dca = readRDS("images/15_scores_dca.rds")
# env_data = readRDS("images/11_env_data.rds")
soil = readRDS("images/11_soil.rds")
topo = readRDS("images/11_topo.rds")

#**********************************************************
# 2 ENV DATA EXPLORATION & TRANSFORMATION------------------
#**********************************************************

# have a look at the soil variables
tmp = reshape2::melt(soil, id.var = "pnr")
xyplot(value ~ pnr | variable, data = tmp, pch = 16,
       col = "salmon",
       scales = list(relation = "free"),
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y, lwd = 2.5, col = "gray")
       })

soil = mutate(soil,
              log_lf = log(lf_s),
              log_cn = log(cn + 1)) %>%
  dplyr::select(-lf_s, -cn)
# remove collinear variables
corvif(dplyr::select(soil, -pnr, -sand, -log_lf, -p))
soil = dplyr::select(soil, -sand, -log_lf, -p)

# One could also think about stepwise forward variable selection using rda since
# we are now using a two-column response -> however, this is difficult because
# this would make an interannual comparison hard -> so better to use the same
# variables for all years)

# have a look at the topo variables
topo = dplyr::select(topo, -aspect, -ndvi11, -ndvi12, -x, -y, -lat, -lon, -zone,
                     -dist_sea, -landuse, -prec, -hl)
# landuse and prec are not really topographic variables
tmp = reshape2::melt(topo, id.var = "pnr")
xyplot(value ~ pnr | variable, data = tmp, pch = 16,
       col = "salmon",
       scales = list(relation = "free"),
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y, lwd = 2.5, col = "gray")
       })
# remove collinear variables
corvif(dplyr::select(topo, -pnr, -slope, -swi))
topo = dplyr::select(topo, -slope, -swi)


# Look for unimodal relationships 
# However, the effect on varpart is marginal (residuals are even worse using the
# polynomials); what is more, it makes the varpart results insignificant...

# inp = list(ct_londo_11, ct_londo_12, ct_londo_16, ct_londo_17)
# out = lapply(inp, function(x) {
#   explore_dca(x, id = "pnr", choices = 1:2, expl = soil)
# })
# names(out) = c("11", "12", "16", "17")
# 
# out[["11"]][[1]]  # na, ca, ph, skeleton
# out[["11"]][[2]]  # na, mg, ca, skeleton
# out[["12"]][[1]]  # na, mg, ca, skeleton
# out[["12"]][[2]]  # ca, mg
# out[["16"]][[1]]  # na, mg, ca, skeleton
# out[["16"]][[2]]  # nothing
# out[["17"]][[1]]  # na, ca, skeleton
# out[["17"]][[2]]  # nothing
# 
# # add a polynomial function of the second order to na, mg, ca, skeleton
# for (i in c("na", "mg", "ca", "skeleton")) {
#   soil[, c(i, paste0(i, "2"))] = poly(soil[, i], 2)  
# }
# out = lapply(inp, function(x) {
#   explore_dca(x, id = "pnr", choices = 1:2, expl = topo)
# })
# names(out) = c("11", "12", "16", "17")
# 
# out[["11"]][[1]]  # landuse
# out[["11"]][[2]]  # cslope, logcarea
# out[["12"]][[1]]  # logcare
# out[["12"]][[2]]  # nothing
# out[["16"]][[1]]  # landuse
# out[["16"]][[2]]  # alt
# out[["17"]][[1]]  # landuse
# out[["17"]][[2]]  # nothing
# 
# # add a polynomial function of the second order to landuse, logcarea
# for (i in c("logcarea")) {
#   topo[, c(i, paste0(i, "2"))] = poly(topo[, i], 2)  
# }

#**********************************************************
# 3 VARIATION PARTITIONING---------------------------------
#**********************************************************

# Variation partitioning for the four londo cts
inp = list(ct_londo_11, ct_londo_12, ct_londo_16, ct_londo_17)
years = c("2011", "2012", "2016", "2017")

save(inp, years, soil, topo, file = here("images/16_input_varpart_plot.rda"))

png("figures/16_varpart.png", height = 11, width = 11, units = "cm", res = 300)
par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0))
#par(tcl = -0.25)
#par(mgp = c(2, 0.6, 0))
for (i in seq_along(inp)) {
  mod = ordi_varpart(inp[[i]], choices = 1:2, expl_1 = soil, expl_2 = topo,
                     dca = TRUE)
  # showvarparts(2, bg = c("hotpink","skyblue"))
  # plot(mod$vp, bg = c("hotpink","skyblue"))
  labs = mod$vp$part$indfract[, 3]
  labs = round(labs, 2)
  labs[c(1, 3)] = paste(labs[c(1, 3)],
                        c(as.character(mod$a), as.character(mod$c)))
  plot(mod$vp, labels = labs, bg = c("hotpink","skyblue"),
       Xnames = "", cex = 0.8)  
  text(x = 0.5, y = 1.1, labels = years[i], font = 2, cex = 0.8)
  text(x = c(-0.4, 1.1), y = c(0.4, 0.4), labels = c("soil", "topography"),
       cex = 0.8)
}
dev.off()

# TO DO: When plotting the partitions, add signficance stars to the numbers in
# the plot, this way, you can avoid an extra table ->
# 1. plot function
# 2. testable fractions function



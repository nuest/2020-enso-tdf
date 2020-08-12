# Filename: 14_ordination.R (2018-06-08)
#
# TO DO: Find out which ordination yields the "best" and most interpretable 
#        results
#
# Author(s): Jannes Muenchow, Jonas Brock
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DCA
# 3. NMDS
# 4. ISOMAP
# 5. WINNER: DCA LONDO
# 6. DCA VIZ
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("vegan")
library("dplyr")
library("lattice")
library("magrittr")
library("ggplot2")
library("here")

# attach own functions
devtools::load_all()

# attach data
pa = readRDS("images/11_ct_pa.rds")
# Remember, there are 196 obs instead of 2000 because:
# 1: in 2011 there were 3 plots (2, 18, 20) without any vegetation cover
# 2: in 2017 plot 25 was inundated and could not be visited
cover = readRDS("images/11_ct_cover.rds")
londo = readRDS("images/11_ct_londo.rds")
env_data = readRDS("images/11_env_data.rds")

#**********************************************************
# 2 DCA----------------------------------------------------
#**********************************************************

# 2.1 presence-absence=====================================
#**********************************************************

dca = ord_fun(pa, env_data, method = "dca")
# visualize for all years
sc = as.data.frame(scores(dca))
sc %<>% 
  mutate(year = gsub(".*_", "", rownames(sc)),
         id = gsub("_.*", "", rownames(sc)))
           
multi_panel_plot(DCA1, DCA2, by = year, data = sc)

# just checking
# fil = filter(sc, year == 2016)
# plot(sc$DCA1, sc$DCA2)
# points(fil$DCA1, fil$DCA2, pch = 16, col = "black")
# text(fil$DCA1, fil$DCA2, labels = fil$id, pos = 3)



# scores = as.data.frame(scores(dca))
# scores_2011 = scores[1:49,]
# scores_2011$DCA3 = NULL
# scores_2011$DCA4 = NULL
# scores_2011 = add_rownames(scores_2011, "ID")
# scores_2011$ID = gsub("_2011", "", as.character(scores_2011$ID))
# 
# 
# scores_2012 = scores[50:98,]
# scores_2012$DCA3 = NULL
# scores_2012$DCA4 = NULL
# scores_2012 = add_rownames(scores_2012, "ID")
# scores_2012$ID = gsub("_2012", "", as.character(scores_2012$ID))
# 
# 
# scores_2016 = scores[99:147,]
# scores_2016$DCA3 = NULL
# scores_2016$DCA4 = NULL
# scores_2016 = add_rownames(scores_2016, "ID")
# scores_2016$ID = gsub("_2016", "", as.character(scores_2016$ID))
# 
# 
# scores_2017 = scores[148:196,]
# scores_2017$DCA3 = NULL
# scores_2017$DCA4 = NULL
# scores_2017 = add_rownames(scores_2017, "ID")
# scores_2017$ID = gsub("_2017", "", as.character(scores_2017$ID))
# 
# par(mfrow=c(2,2))
# plot(dca, choices=c(1,2), origin=TRUE,
#      display= "sites", main = "Year 2011",
#      cex = 0.8, cols = "lightgrey", pch = 19)
# points(scores_2011$DCA1, scores_2011$DCA2, col = "black", pch = 19, cex = 1)
# text(scores_2011$DCA1, scores_2011$DCA2, labels = scores_2011$ID, cex= 0.5, pos=3)
# plot(fit, col = "black", lwd = 1.5)
# 
# 
# plot(dca, choices=c(1,2), origin=TRUE,
#      display= "sites", main = "Year 2012",
#      cex = 0.8, cols = "lightgrey", pch = 19)
# points(scores_2012$DCA1, scores_2012$DCA2, col = "black", pch = 19, cex = 1)
# text(scores_2012$DCA1, scores_2012$DCA2, labels = scores_2012$ID, cex= 0.5, pos=3)
# plot(fit, col = "black", lwd = 1.5)
# 
# plot(dca, choices=c(1,2), origin=TRUE,
#      display= "sites", main = "Year 2016",
#      cex = 0.8, cols = "lightgrey", pch = 19)
# points(scores_2016$DCA1, scores_2016$DCA2, col = "black", pch = 19, cex = 1)
# text(scores_2016$DCA1, scores_2016$DCA2, labels = scores_2016$ID, cex= 0.5, pos=3)
# plot(fit, col = "black", lwd = 1.5)
# 
# plot(dca, choices=c(1,2), origin=TRUE,
#      display= "sites", main = "Year 2017",
#      cex = 0.8, cols = "lightgrey", pch = 19)
# points(scores_2017$DCA1, scores_2017$DCA2, col = "black", pch = 19, cex = 1)
# text(scores_2017$DCA1, scores_2017$DCA2, labels = scores_2017$ID, cex= 0.5, pos=3)
# plot(fit, col = "black", lwd = 1.5)

# 2.2 Cover================================================
#**********************************************************
dca = ord_fun(cover, env_data, method = "dca")
# visualize each year
sc = as.data.frame(scores(dca))
sc %<>% 
  mutate(year = gsub(".*_", "", rownames(sc)),
         id = gsub("_.*", "", rownames(sc)))

multi_panel_plot(DCA1, DCA2, by = year, data = sc)
# interesting plot order is now reversed, i.e. the lowest DCA1 value corresponds
# to the moistest plots
# just checking
# fil = filter(sc, year == 2012)
# plot(sc$DCA1, sc$DCA2)
# points(fil$DCA1, fil$DCA2, pch = 16, col = "black")
# text(fil$DCA1, fil$DCA2, labels = fil$id, pos = 3)

# 2.3 Londo================================================
#**********************************************************
dca = ord_fun(londo, env_data, method = "dca")
# visualize all years
sc = as.data.frame(scores(dca))
sc %<>% 
  mutate(year = gsub(".*_", "", rownames(sc)),
         id = gsub("_.*", "", rownames(sc)))
multi_panel_plot(DCA1, DCA2, by = year, data = sc)
# interesting, lowest values now correspond to the moistest plots
# just multiply by -1 -> sign is arbitrary in ordinations
# sc$DCA1 = sc$DCA1 * -1
p_1 = multi_panel_plot(DCA1, DCA2, by = year, data = sc)
# png(filename = "figures/14_londo_dca.png", res = 300, height = 17,
#     width = 17, units = "cm")
print(p_1)
# dev.off()

#**********************************************************
# 3 NMDS---------------------------------------------------
#**********************************************************

# 3.1 Presence-absence=====================================
#**********************************************************
nmds = ord_fun(pa, env_data, method = "nmds")
# visualize all years
sc = as.data.frame(scores(nmds))
sc %<>% 
  mutate(year = gsub(".*_", "", rownames(sc)),
         id = gsub("_.*", "", rownames(sc)))
multi_panel_plot(NMDS1, NMDS2, by = year, data = sc)

# 3.2 Cover================================================
#**********************************************************
nmds = ord_fun(cover, env_data, method = "nmds")
# visualize all years
sc = as.data.frame(scores(nmds))
sc %<>% 
  mutate(year = gsub(".*_", "", rownames(sc)),
         id = gsub("_.*", "", rownames(sc)))
multi_panel_plot(NMDS1, NMDS2, by = year, data = sc)

# just checking
# fil = filter(sc, year == 2012)
# plot(sc$NMDS1, sc$NMDS2)
# points(fil$NMDS1, fil$NMDS2, pch = 16, col = "black")
# text(fil$NMDS1, fil$NMDS2, labels = fil$id, pos = 3)


# 3.3 Londo================================================
#**********************************************************
nmds = ord_fun(londo, env_data, method = "nmds")
# visualize all years
sc = as.data.frame(scores(nmds))
sc %<>% 
  mutate(year = gsub(".*_", "", rownames(sc)),
         id = gsub("_.*", "", rownames(sc)))
multi_panel_plot(NMDS1, NMDS2, by = year, data = sc)

# just checking
# fil = filter(sc, year == 2012)
# plot(sc$NMDS1, sc$NMDS2)
# points(fil$NMDS1, fil$NMDS2, pch = 16, col = "black")
# text(fil$NMDS1, fil$NMDS2, labels = fil$id, pos = 3)

#**********************************************************
# 4 ISOMAP-------------------------------------------------
#**********************************************************

# 4.1 Presence-absence=====================================
#**********************************************************

iso = ord_fun(pa, env_data, method = "isomap")
# visualize all years
sc = as.data.frame(iso)
sc %<>% 
  mutate(year = gsub(".*_", "", rownames(sc)),
         id = gsub("_.*", "", rownames(sc)))
multi_panel_plot(Dim1, Dim2, by = year, data = sc)

# 4.2 Cover================================================
#**********************************************************

iso = ord_fun(cover, env_data, method = "isomap")
# visualize all years
sc = as.data.frame(iso)
sc %<>% 
  mutate(year = gsub(".*_", "", rownames(sc)),
         id = gsub("_.*", "", rownames(sc)))
multi_panel_plot(Dim1, Dim2, by = year, data = sc)

# 4.3 Londo================================================
#**********************************************************

iso = ord_fun(pa, env_data, method = "isomap")
# visualize all years
sc = as.data.frame(iso)
sc %<>% 
  mutate(year = gsub(".*_", "", rownames(sc)),
         id = gsub("_.*", "", rownames(sc)))
multi_panel_plot(Dim1, Dim2, by = year, data = sc)

#**********************************************************
# 5 WINNER: DCA LONDO--------------------------------------
#**********************************************************

dca = ord_fun(londo, env_data, method = "dca")
# visualize all years
sc = as.data.frame(scores(dca))
sc %<>% 
  mutate(year = gsub(".*_", "", rownames(sc)),
         id = gsub("_.*", "", rownames(sc)))
multi_panel_plot(DCA1, DCA2, by = year, data = sc)
# interesting, lowest values now correspond to the moistest plots
# just multiply by -1 -> sign is arbitrary in ordinations
# sc$DCA1 = sc$DCA1 * -1
p_1 = multi_panel_plot(DCA1, DCA2, by = year, data = sc)
# png(filename = "figures/14_londo_dca.png", res = 300, height = 17,
#     width = 17, units = "cm")
print(p_1)
# dev.off()
# save(p_1, sc, file = here("figures/14_londo_dca.rda"))
# importance of the first DCA axis (see also 13_descriptive_stats.R) 
# group_by(sc, year) %>%
#   summarize(min = min(DCA1), max = max(DCA1)) %>%
#   group_by(year) %>%
#   mutate(range = diff(c(min, max)))

# species with the most coverage in 2017
c_17 = filter(cover, grepl("_2017", pnr))
ls = lapply(seq_len(nrow(c_17)), function(i) {
  d = c_17[i, ]
  d = dplyr::select(d, -pnr)
  sort(d, decreasing = TRUE)[1:5]
})
  
# find out which species exhibited the highest coverage near the coast
# just consider the first 25 plots 
tmp = plyr::rbind.fill(ls[1:25])
# just keep species occuring frequently
tmp = tmp[, colSums(is.na(tmp)) <= 10]
# sort in accordance with the highest coverage
names(sort(colSums(tmp, na.rm = TRUE), decreasing = TRUE))

# save DCA scores
saveRDS(sc, "images/14_dca_scores.rds")

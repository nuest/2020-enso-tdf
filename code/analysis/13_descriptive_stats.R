# Filename: 13_descriptive_stats.R (2018-06-21)
#
# TO DO: Descriptive stats of biodiversity and plant cover
#
# Author(s): Jannes Muenchow, Jonas Brock
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. VISUALIZE CHANGE IN RICHNESS & COVER
# 3. ALPHA, BETA, GAMMA-DIVERSITY
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("data.table")
library("dplyr")
library("ggplot2")
library("gridExtra")
library("lattice")
library("latticeExtra")
library("here")
library("vegan")
library("magrittr")
# source your own functions
source(here("code/funs/ordi_helper_funs.R"))

# attach cover datasets (d = Deckung, i.e., cover)
d_11 = readRDS(here("images/11_d_11.rds")) %>%
  mutate(year = 2011)
d_12 = readRDS(here("images/11_d_12.rds")) %>%
  mutate(year = 2012)
d_16 = readRDS(here("images/11_d_16.rds")) %>%
  mutate(year = 2016)
d_17 = readRDS(here("images/11_d_17.rds")) %>%
  mutate(year = 2017)
# attach londo cover data
londo = readRDS(here("images/11_ct_londo.rds"))
# envdata
env = readRDS(here("images/11_env_data.rds"))

#**********************************************************
# 2 VISUALIZE CHANGE IN RICHNESS & COVER-------------------
#**********************************************************

# 2.1 Data preparation=====================================
#**********************************************************
d = data.table::rbindlist(list(d_11, d_12, d_16, d_17), use.names = TRUE)
d[, cover := as.numeric(as.character(cover))]
d_2 = d[, .(cover = sum(cover),
            spri = .N),
        by = .(pnr, year)]
# find out about missing ids in groups
tmp = tibble(pnr = rep(1:50, 4),
             year = rep(c(2011, 2012, 2016, 2017), each = 50))
anti_join(tmp, d_2)
d_2 = full_join(d_2, tmp) %>%
  arrange(year, pnr)
# fill NAs with 0
d_2[is.na(d_2)] = 0
d_2 = as.data.table(d_2)
# # use 2011 as a base year
# d_2 = d_2[, .(cover = cover - d_2[year == 2011, cover],
#       spri = spri - d_2[year == 2011, spri],
#       year = year,
#       pnr = pnr)]
# d_2[year == 2011]  # cover and spri should be all 0, perfect
# d_2 = d_2[year != 2011]

# add coordinates
env = dplyr::select(env, id, lon, lat, dist_sea)
env = filter(env, !duplicated(id))
d_2 = inner_join(d_2, env, by = c("pnr" = "id"))
mutate(d_2, dist_sea = round(dist_sea / 1000))

# 2.2 Visualize change in species richness & cover=========
#**********************************************************

# orange color found here:
# browseURL(paste0("https://www.stat.ubc.ca/~jenny/STAT545A/", 
#                  "block16_colorsLatticeQualitative.html"))
# gdURL = paste0("http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/", 
#                "gapminder/data/gapminderCountryColors.txt")
# countryColors = read.delim(file = gdURL, as.is = 3) # protect color
# str(countryColors)
# filter(countryColors, continent = "Africa")[48, "color"]

# lines by group, solution found here:
# browseURL(paste0("https://stackoverflow.com/questions/16066826/", 
#                  "multipanel-smooth-with-grouping-factor-in-lattice"))
my.panel.loess = function(x, y, span = 3/5, degree = 0.5, ...) {
  loess.fit = loess.smooth(x, y, span = span, dgree = degree )
  panel.lines(loess.fit$x, loess.fit$y, ...)
}

pal = rev(c("lightblue",  "#FDD39B", "pink", "lightgray"))
a = 
  xyplot(spri + cover ~ pnr, #| factor(year), 
         data = d_2, groups = factor(year), ylab = "", 
         xlab = "Distance to the sea in km",
         type = "l", col = pal, as.table = TRUE, lwd = 3,
         scales = list(y = "free",
                       alternating = 1,
                       tck = c(1, 0),
                       x = list(at = c(1, 10, 20, 30, 40, 50),
                                labels = round(d_2[c(1, 10, 20, 30, 40, 50),
                                                   "dist_sea"] / 1000))),
         # strip should have customized names, should be white (bg) and the text
         # (par.strip.text) should be a bit smaller
         strip = strip.custom(factor.levels = c("Species richness", "Cover"),
                              bg = "white",
                              par.strip.text = list(cex = 1)),
         panel = function(...) {
           panel.superpose(panel.groups = my.panel.loess, ...)
         },
         #auto.key = list(cex = 1, pch = 16, space = "right"))
         key = list(lines = list(lwd = 2, col = pal),
                    space = "right",
                    text = list(text = c("2011", "2012", "2016", "2017"))
         ))

b = xyplot(spri + cover ~ pnr, data = d_2, groups = factor(year),
            type = "l", alpha = 0.5, col = pal)
# save your result
png("figures/13_spri_cover.png", width = 20, height = 10, units = "cm",
    res = 300)
plot(a + b)
dev.off()

saveRDS(a, here("figures/13_spri.rds"))
saveRDS(b, here("figures/13_cover.rds"))
saveRDS(d_2, here("images/13_spri.rds"))

#**********************************************************
# 3 ALPHA, BETA, GAMMA-DIVERSITY---------------------------
#**********************************************************

# alpha diversity
d_2 = as.data.table(d_2)
alpha = d_2[, .(alpha = mean(spri)), by = .(year)]
# gamma diversity
gamma = d[, .(gamma = length(unique(art))), by = year]
div = inner_join(alpha, gamma, by = "year")

# DCAs, i.e., beta diversity, is actually computed in 14_ordination.R, hence we
# simply copy the corresponding code
env_data = readRDS("images/11_env_data.rds")
dca = ord_fun(londo, env_data, method = "dca")
# visualize all years
sc = as.data.frame(scores(dca))
sc %<>% 
  mutate(year = gsub(".*_", "", rownames(sc)),
         id = gsub("_.*", "", rownames(sc)))
# importance of the first DCA axis 
beta = mutate(sc, year = as.numeric(year)) %>%
  group_by(year) %>%
  summarize(min = min(DCA1), max = max(DCA1)) %>%
  group_by(year) %>%
  mutate(beta = round(diff(c(min, max)), 2))
# join beta diversity to the div table
div = inner_join(div, dplyr::select(beta, year, beta))
setcolorder(div, c("year", "alpha", "beta"))
# add min and maximum alpha diversity
my_range = d_2[, list(min = min(spri), max = max(spri)), by = year]
div = inner_join(my_range, div, by = "year")
# save the table
saveRDS(div, here("images/13_desc_stats_tab.rds"))

# multivariate homogeneity of groups dispersions

# cover = readRDS("images/11_ct_cover.rds")
londo = readRDS("images/11_ct_londo.rds")
londo = tidyr::separate(londo, pnr, into = c("id", "year"), sep = "_")
dis = vegdist(select(londo, -id, -year))
mod = betadisper(dis, group = as.factor(londo$year))
mod = betadisper(dis, group = as.factor(londo$year), type = "centroid")
mod  
anova(mod)
boxplot(mod, xlab = "year")
tukey = TukeyHSD(mod)
tukey
plot(tukey)

# find out about the most abundant species and most frequent species in 2017
tmp = tidyr::separate(londo, pnr, into = c("id", "year")) %>%
  filter(year == 2017) %>%
  select(-id, -year)
# highest cover
colSums(tmp) %>% sort
# highest number of occurences
decostand(tmp, "pa") %>% colSums %>% sort

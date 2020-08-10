# Filename: 12_rda.R (2018-06-01)
#
# Author(s):  Gregor Didenko
#
# Source(s): Jannes Muenchow & "Chapter 6 - Numerical Ecology with R"
#             
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# - ATTACH PACKAGES AND DATA
# - PREPROCESSING
# - RDA WITH env_chem
# - RDA WITH TOPO CHARACTERISTICS
#
#**********************************************************
# ATTACH PACKAGES AND READ DATA--------------------------
#**********************************************************

# load packages
pacman::p_load(vegan, packfor, dplyr, magrittr)

# load data
# Environment data
env_all <- as.data.frame(readRDS("images/06_env_data.rda"))

# Crosstable Londo-scale vegation cover
CTlondo = readRDS("images/02_CTlondo.rda")

# Crosstable Presence/Absence
CTpa = readRDS("images/02_CTpa.rda")


#**********************************************************
# PREPROCESSING--------------------------
#**********************************************************

## CT LONDO:
# set na values to 0
CTlondo[is.na(CTlondo)] = 0

# remove plot nr 25
CTlondo %<>% filter(!grepl("25", pnr))

# Divide datasets into years
CTlondo_2011 = CTlondo %>% filter(grepl("2011", pnr))
CTlondo_2012 = CTlondo %>% filter(grepl("2012", pnr))
CTlondo_2016 = CTlondo %>% filter(grepl("2016", pnr))
CTlondo_2017 = CTlondo %>% filter(grepl("2017", pnr))

#Helling-transformation (and remove Plotnr. column)
vege_hel_2011 <- decostand(CTlondo_2011[, -1], "hellinger")
vege_hel_2012 <- decostand(CTlondo_2012[, -1], "hellinger")
vege_hel_2016 <- decostand(CTlondo_2016[, -1], "hellinger")
vege_hel_2017 <- decostand(CTlondo_2017[, -1], "hellinger")

## ENV-DATA

# Correct Error in CN value and transform to numerical
env_all$cn[env_all$cn == "1.#INF"] = 0
env_all$cn = as.numeric(env_all$cn)

names(env_all)


env_select = env_all %>% select(-x, -y, -zone, -lon, -lat, -prec, -ndvi11, 
                                -ndvi12, -ndvi_11, -ndvi_12, -ndvi_16, -ndvi_17,
                                -Bodenart)

env_select_2011 = env_select %>% filter(grepl("2011", pnr))
env_select_2012 = env_select %>% filter(grepl("2012", pnr))
env_select_2016 = env_select %>% filter(grepl("2016", pnr))
env_select_2017 = env_select %>% filter(grepl("2017", pnr))


#**********************************************************
# 2 RDA WITH env_select--------------------------
#**********************************************************

## RDA 2011 with all explanatory variables

rda_2011 <- rda(vege_hel_2011 ~ ., data = env_select_2011[, -1])
rda_2011
# constrained fraction = amount of variance of Y explained by the explanatory variables

# Global adjusted R^2
rda_2011_rsa = RsquareAdj(rda_2011)$adj.r.squared

#and same result with ordistep (sand has to be excluded according to Blanchet's 2nd stopping criterion = global adjusted R^2 (Blanchet et al., 2008)
rda_2011_pars <- ordistep(rda(vege_hel_2011 ~ 1, data = env_select_2011[, -1]),
                         scope = formula(rda_2011), direction = "forward", pstep = 1000)

#Have a short glance at the results
rda_2011_pars_rsa = (RsquareAdj(rda_2011_pars)$adj.r.squared)
vif.cca(rda_2011_pars)
plot(rda_2011_pars, main = "rda_2011_pars")


## RDA 2012 with all explanatory variables

rda_2012 <- rda(vege_hel_2012 ~ ., data = env_select_2012[, -1])
rda_2012
# constrained fraction = amount of variance of Y explained by the explanatory variables

# Global adjusted R^2
rda_2012_rsa = RsquareAdj(rda_2012)$adj.r.squared

#and same result with ordistep (sand has to be excluded according to Blanchet's 2nd stopping criterion = global adjusted R^2 (Blanchet et al., 2008)
rda_2012_pars <- ordistep(rda(vege_hel_2012 ~ 1, data = env_select_2012[, -1]),
                         scope = formula(rda_2012), direction = "forward", pstep = 1000)

#Have a short glance at the results
rda_2012_pars_rsa = RsquareAdj(rda_2012_pars)$adj.r.squared
vif.cca(rda_2012_pars)
plot(rda_2012_pars, main = "rda_2012_pars")



## RDA 2016 with all explanatory variables

rda_2016 <- rda(vege_hel_2016 ~ ., data = env_select_2016[, -1])
rda_2016
# constrained fraction = amount of variance of Y explained by the explanatory variables

# Global adjusted R^2
rda_2016_rsa = RsquareAdj(rda_2016)$adj.r.squared

#and same result with ordistep (sand has to be excluded according to Blanchet's 2nd stopping criterion = global adjusted R^2 (Blanchet et al., 2008)
rda_2016_pars <- ordistep(rda(vege_hel_2016 ~ 1, data = env_select_2016[, -1]),
                         scope = formula(rda_2016), direction = "forward", pstep = 1000)

#Have a short glance at the results
rda_2016_pars_rsa = RsquareAdj(rda_2016_pars)$adj.r.squared
vif.cca(rda_2016_pars)
plot(rda_2016_pars, main = "rda_2016_pars")




## RDA 2017 with all explanatory variables

rda_2017 <- rda(vege_hel_2017 ~ ., data = env_select_2017[, -1])
rda_2017
# constrained fraction = amount of variance of Y explained by the explanatory variables

# Global adjusted R^2
rda_2017_rsa = RsquareAdj(rda_2017)$adj.r.squared

#and same result with ordistep (sand has to be excluded according to Blanchet's 2nd stopping criterion = global adjusted R^2 (Blanchet et al., 2008)
rda_2017_pars <- ordistep(rda(vege_hel_2017 ~ 1, data = env_select_2017[, -1]),
                         scope = formula(rda_2017), direction = "forward", pstep = 1000)

#Have a short glance at the results
rda_2017_pars_rsa = RsquareAdj(rda_2017_pars)$adj.r.squared
vif.cca(rda_2017_pars)
plot(rda_2017_pars, main = "rda_2017_pars")




#**********************************************************
# RDA WITH env_soil--------------------------
#**********************************************************

env_soil = env_all %>% select(pnr, K_cmolkg, Na_cmolkg, Mg_cmolkg, Ca_cmolkg, 
                              c_anor, c_org, c_cont, n_tot, ph, sand, cn, skeleton)

env_soil_2011 = env_soil %>% filter(grepl("2011", pnr))
env_soil_2012 = env_soil %>% filter(grepl("2012", pnr))
env_soil_2016 = env_soil %>% filter(grepl("2016", pnr))
env_soil_2017 = env_soil %>% filter(grepl("2017", pnr))

rda_soil_2011 <- rda(vege_hel_2011 ~ ., data = env_soil_2011[, -1])
rda_soil_2011
# constrained fraction = amount of variance of Y explained by the explanatory variables

# Global adjusted R^2
rda_soil_2011_rsa = RsquareAdj(rda_soil_2011)$adj.r.squared

#and same result with ordistep (sand has to be excluded according to Blanchet's 2nd stopping criterion = global adjusted R^2 (Blanchet et al., 2008)
rda_soil_2011_pars <- ordistep(rda(vege_hel_2011 ~ 1, data = env_soil_2011[, -1]),
                          scope = formula(rda_soil_2011), direction = "forward", pstep = 1000)

#Have a short glance at the results
rda_soil_2011_pars_rsa = (RsquareAdj(rda_soil_2011_pars)$adj.r.squared)
vif.cca(rda_soil_2011_pars)
plot(rda_soil_2011_pars, main = "rda_soil_2011_pars")


## rda_soil 2012 

rda_soil_2012 <- rda(vege_hel_2012 ~ ., data = env_soil_2012[, -1])
rda_soil_2012
# constrained fraction = amount of variance of Y explained by the explanatory variables

# Global adjusted R^2
rda_soil_2012_rsa = RsquareAdj(rda_soil_2012)$adj.r.squared

#and same result with ordistep (sand has to be excluded according to Blanchet's 2nd stopping criterion = global adjusted R^2 (Blanchet et al., 2008)
rda_soil_2012_pars <- ordistep(rda(vege_hel_2012 ~ 1, data = env_soil_2012[, -1]),
                          scope = formula(rda_soil_2012), direction = "forward", pstep = 1000)

#Have a short glance at the results
rda_soil_2012_pars_rsa = RsquareAdj(rda_soil_2012_pars)$adj.r.squared
vif.cca(rda_soil_2012_pars)
plot(rda_soil_2012_pars, main = "rda_soil_2012_pars")



## rda_soil 2016 

rda_soil_2016 <- rda(vege_hel_2016 ~ ., data = env_soil_2016[, -1])
rda_soil_2016
# constrained fraction = amount of variance of Y explained by the explanatory variables

# Global adjusted R^2
rda_soil_2016_rsa = RsquareAdj(rda_soil_2016)$adj.r.squared

#and same result with ordistep (sand has to be excluded according to Blanchet's 2nd stopping criterion = global adjusted R^2 (Blanchet et al., 2008)
rda_soil_2016_pars <- ordistep(rda(vege_hel_2016 ~ 1, data = env_soil_2016[, -1]),
                          scope = formula(rda_soil_2016), direction = "forward", pstep = 1000)

#Have a short glance at the results
rda_soil_2016_pars_rsa = RsquareAdj(rda_soil_2016_pars)$adj.r.squared
vif.cca(rda_soil_2016_pars)
plot(rda_soil_2016_pars, main = "rda_soil_2016_pars")




## rda_soil 2017 

rda_soil_2017 <- rda(vege_hel_2017 ~ ., data = env_soil_2017[, -1])
rda_soil_2017
# constrained fraction = amount of variance of Y explained by the explanatory variables

# Global adjusted R^2
rda_soil_2017_rsa = RsquareAdj(rda_soil_2017)$adj.r.squared

#and same result with ordistep (sand has to be excluded according to Blanchet's 2nd stopping criterion = global adjusted R^2 (Blanchet et al., 2008)
rda_soil_2017_pars <- ordistep(rda(vege_hel_2017 ~ 1, data = env_soil_2017[, -1]),
                          scope = formula(rda_soil_2017), direction = "forward", pstep = 1000)

#Have a short glance at the results
rda_soil_2017_pars_rsa = RsquareAdj(rda_soil_2017_pars)$adj.r.squared
vif.cca(rda_soil_2017_pars)
plot(rda_soil_2017_pars, main = "rda_soil_2017_pars")




#**********************************************************
# RDA WITH env_topo--------------------------
#**********************************************************

env_topo = env_all %>% select(pnr, dist_sea, aspect, cosasp, cslope, hcurv, hl, 
                              sinasp, slope, swi, vcurv, alt)

env_topo_2011 = env_topo %>% filter(grepl("2011", pnr))
env_topo_2012 = env_topo %>% filter(grepl("2012", pnr))
env_topo_2016 = env_topo %>% filter(grepl("2016", pnr))
env_topo_2017 = env_topo %>% filter(grepl("2017", pnr))

rda_topo_2011 <- rda(vege_hel_2011 ~ ., data = env_topo_2011[, -1])
rda_topo_2011
# constrained fraction = amount of variance of Y explained by the explanatory variables

# Global adjusted R^2
rda_topo_2011_rsa = RsquareAdj(rda_topo_2011)$adj.r.squared

#and same result with ordistep (sand has to be excluded according to Blanchet's 2nd stopping criterion = global adjusted R^2 (Blanchet et al., 2008)
rda_topo_2011_pars <- ordistep(rda(vege_hel_2011 ~ 1, data = env_topo_2011[, -1]),
                               scope = formula(rda_topo_2011), direction = "forward", pstep = 1000)

#Have a short glance at the results
rda_topo_2011_pars_rsa = (RsquareAdj(rda_topo_2011_pars)$adj.r.squared)
vif.cca(rda_topo_2011_pars)
plot(rda_topo_2011_pars, main = "rda_topo_2011_pars")


## rda_topo 2012 

rda_topo_2012 <- rda(vege_hel_2012 ~ ., data = env_topo_2012[, -1])
rda_topo_2012
# constrained fraction = amount of variance of Y explained by the explanatory variables

# Global adjusted R^2
rda_topo_2012_rsa = RsquareAdj(rda_topo_2012)$adj.r.squared

#and same result with ordistep (sand has to be excluded according to Blanchet's 2nd stopping criterion = global adjusted R^2 (Blanchet et al., 2008)
rda_topo_2012_pars <- ordistep(rda(vege_hel_2012 ~ 1, data = env_topo_2012[, -1]),
                               scope = formula(rda_topo_2012), direction = "forward", pstep = 1000)

#Have a short glance at the results
rda_topo_2012_pars_rsa = RsquareAdj(rda_topo_2012_pars)$adj.r.squared
vif.cca(rda_topo_2012_pars)
plot(rda_topo_2012_pars, main = "rda_topo_2012_pars")



## rda_topo 2016 

rda_topo_2016 <- rda(vege_hel_2016 ~ ., data = env_topo_2016[, -1])
rda_topo_2016
# constrained fraction = amount of variance of Y explained by the explanatory variables

# Global adjusted R^2
rda_topo_2016_rsa = RsquareAdj(rda_topo_2016)$adj.r.squared

#and same result with ordistep (sand has to be excluded according to Blanchet's 2nd stopping criterion = global adjusted R^2 (Blanchet et al., 2008)
rda_topo_2016_pars <- ordistep(rda(vege_hel_2016 ~ 1, data = env_topo_2016[, -1]),
                               scope = formula(rda_topo_2016), direction = "forward", pstep = 1000)

#Have a short glance at the results
rda_topo_2016_pars_rsa = RsquareAdj(rda_topo_2016_pars)$adj.r.squared
vif.cca(rda_topo_2016_pars)
plot(rda_topo_2016_pars, main = "rda_topo_2016_pars")




## rda_topo 2017 

rda_topo_2017 <- rda(vege_hel_2017 ~ ., data = env_topo_2017[, -1])
rda_topo_2017
# constrained fraction = amount of variance of Y explained by the explanatory variables

# Global adjusted R^2
rda_topo_2017_rsa = RsquareAdj(rda_topo_2017)$adj.r.squared

#and same result with ordistep (sand has to be excluded according to Blanchet's 2nd stopping criterion = global adjusted R^2 (Blanchet et al., 2008)
rda_topo_2017_pars <- ordistep(rda(vege_hel_2017 ~ 1, data = env_topo_2017[, -1]),
                               scope = formula(rda_topo_2017), direction = "forward", pstep = 1000)

#Have a short glance at the results
rda_topo_2017_pars_rsa = RsquareAdj(rda_topo_2017_pars)$adj.r.squared
vif.cca(rda_topo_2017_pars)
plot(rda_topo_2017_pars, main = "rda_topo_2017_pars")





#**********************************************************
# Plotting--------------------------
#**********************************************************

plot(rda_2017_pars, main = "rda_2017_pars", choices = c(1, 2), 
     display = c("lc", "cn"), type = "none", scaling = "sites")
text(rda_2017_pars, dis="cn", scaling = "sites", col = "blue")
text(rda_2017_pars, labels = c(1:24, 26:50), display = "lc", scaling = "sites")

# Filename: 11_preparation.R (2018-06-07)
#
# TO DO: bring data in a form ready for analysis
#
# Author(s): Jannes Muenchow, Jonas Brock
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES
# 2. VEGETATION DATA PREPROCESSING
# 3. CREATE CROSS-TABLES
# 4. LONDO
# 5. ENIRONMENTAL DATA
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("data.table")
library("tidyverse")
library("magrittr")
library("gdata")
library("reshape2")
library("RSQLite")
library("forcats")
library("here")
devtools::load_all()

# attach data if data/tables.gpkg is not already available, download_zenodo
# downloads the data from Zenodo
download_zenodo()
con = dbConnect(RSQLite::SQLite(), here("data/tables.gpkg"))
dbListTables(con)
lf = dbReadTable(con, "lifeform", as.is = TRUE)
d_11 = dbReadTable(con, "plot_species_matrix_2011", as.is = TRUE)
d_12 = dbReadTable(con, "plot_species_matrix_2012", as.is = TRUE)
d_16 = dbReadTable(con, "plot_species_matrix_2016", as.is = TRUE)
d_17 = dbReadTable(con, "plot_species_matrix_2017", as.is = TRUE)
soil = dbReadTable(con, "soil", as.is = TRUE)
topo = dbReadTable(con, "topography", as.is = TRUE)
plova = dbReadTable(con, "plot_variables", as.is = TRUE)
dbDisconnect(con)

#**********************************************************
# 2 DATA PREPROCESSING-------------------------------------
#**********************************************************

# 2.1 Prepare 2011 data====================================
#**********************************************************

setdiff(1:50, d_11$pnr)
# missing plots (no vegetation found): 2, 18, 20

# classify manually so far uncategorized species
d_11 = as.data.table(d_11)
d_11[art == "Acac_maca", type := "woody"]
d_11[art == "unkn_Poac1", type := "grass"]
d_11[art %in% c("Alte_spec", "Oxal_spec", "unkn_Malv", "Lant_spec", "Teph_spec",
               "unkn_Sola", "Vigna_spec", "Macr_atro", "Pass_foet", 
               "Astr_spe2", "unkn_Poly", "unkn_Aste", "unknown1", "unknown2"),
    type := "herbs"]
# check
d_11[is.na(type)]  # 0, perfect

# WE DON'T NEED PSEUDO PLOTS, THEREFORE SKIP IT
# # find out what plots are missing in 2011 and create pseudo plots
# # with plants from 2016
# x = setdiff(1:50, d_11$pnr)
# 
# # a for loop for an automated creation of pseudo plots
# # Is this really needed? For a Procrustes analysis, yes, but we are not going
# # to do one. For plot-by-plot comparisons this might be helpful as well. But
# # for our joint DCA this is not necessary, so maybe delete before executing the
# # DCA
# for (i in x) {
#   # here, again it is of utmost importance that in fact pnr of 11 is the pnr of
#   # 16
#   new_row = filter(d_16, pnr == i) %>% 
#     filter(cover == max(cover)) %>% 
#     slice(1) %>%
#     mutate(cover = 0.001)
#   d_11 = rbind(d_11, new_row) %>%
#     arrange(pnr)
# }
# # check
# filter(d_11, pnr %in% x)

# Which variable are characterized by false names??
setdiff(d_11$art, d_16$art)
setdiff(d_16$art, d_11$art)

# change the wrong names
d_11$art[d_11$art == "Acac_maca"] = "Acac_macr"

# finally, check if we have duplicates
d_11[, .SD[duplicated(art) | duplicated(art, fromLast = TRUE)], by = pnr]
# this looks like having recorded the same species two times, aggregate
d_11 = d_11[, cover := as.numeric(cover)]
d_11 = d_11[, .(cover = sum(cover)), by = .(pnr, type, art)]

# 2.2 Prepare 2012 data====================================
#**********************************************************

# classify manually so far uncategorized species
d_12 = as.data.table(d_12)
d_12[art == "Acac_maca", type := "woody"]
d_12[art %in% c("unkn_Poac2", "Chlo_spec2", "Chlo_spec1","Cenc_spec", 
                "unkn_Poac3"),
                type := "grass"]
d_12[art %in% c("Astr_spec1", "Sida_spec", "Oxal_spec", "Desm_spec", 
                "Mela_aspe", "Ipom_spec", "Sola_amer", "Lant_spec", "unknown3",
                "unknown4"),
     type := "herbs"]
# check
d_12[is.na(type)]  # 0, perfect

# check plant names for consistency in between years
setdiff(d_12$art, d_11$art)
setdiff(d_11$art, d_12$art)

# change names plants in 2012
d_12$art[d_12$art == "Acac_maca"] = "Acac_macr"
d_12$art[d_12$art == "Astr_spec1"] = "Astr_spec"

# finally, check if we have duplicates
d_12[, .SD[duplicated(art) | duplicated(art, fromLast = TRUE)], by = pnr]
# this looks like having recorded the same species two times, aggregate
d_12 = d_12[, .(cover = sum(cover)), by = .(pnr, type, art)]

# 2.3 Prepare 2016 data====================================
#**********************************************************

# check if there are duplicates
d_16 = as.data.table(d_16)
d_16[, .SD[duplicated(art) | duplicated(art, fromLast = TRUE)], by = pnr]
# ok, this looks like having accidentally written down the same species two
# times
# therefore, delete duplicates
d_16 = d_16[, .SD[!duplicated(art)], by = pnr]

# 2.4 Prepare 2017 data====================================
#**********************************************************

# Remember: Plot 25 has no recordings because it was flooded

# delete _raw + rename Euphorbia serpens into Euph hype from data/db/Transekt_0417.accdb
d_17 = as.data.table(d_17)
# assign a life form to so far unclassified plants
d_17[is.na(type) & !duplicated(art)]
d_17[art %in% c("Senn_spec", "Acac_huar"), type := "woody"]
d_17[art %in% c("Oxal_spec", "Hype_spec", "Lyco_pimp", "Cucu_dips", 
                "Pter_pter"),
     type := "herbs"]
d_17[art == "Ante_spec", type := "grass"]
d_17[is.na(type)]  
# plot 25, this is ok, because it was inundated
# Euph_serp has to be renamed into Euph_hype

# however, we have to remove it, since NAs are not accepted in ordinations
d_17 = d_17[!is.na(art)]

# add the tree lyci_lyci to the dataset as it is a woody species and has been
# already recorded during the previous sample campaign in 2016
d_17 = rbind(d_17, list(49, 1.5, "Lyci_lyci", "woody"))[order(pnr)]

# change wrong names that evolved through different data recorders (Jannes
# vs. Luis)
d_17[pnr == 34 & art == "Park_acul", art := "Park_prae"]
d_17[art == "Acac_huar", art := "Acac_macr"]
d_17[art == "Euph_serp", (c("art", "type")) := .("Euph_hype", "herbs")]

# finally, check if we have duplicates
d_17[, .SD[duplicated(art) | duplicated(art, fromLast = TRUE)], by = pnr]
# this looks like having recorded the same species two times, aggregate
d_17 = d_17[, .(cover = sum(cover)), by = .(pnr, type, art)]

# save your result
saveRDS(d_11, here("images/11_d_11.rds"))
saveRDS(d_12, here("images/11_d_12.rds"))
saveRDS(d_16, here("images/11_d_16.rds"))
saveRDS(d_17, here("images/11_d_17.rds"))

#**********************************************************
# 3 CREATE CROSS TABLES------------------------------------
#**********************************************************

# attach data if needed
# d_11 = readRDS("images/11_d_11.rds")
# d_12 = readRDS("images/11_d_12.rds")
# d_16 = readRDS("images/11_d_16.rds")
# d_17 = readRDS("images/11_d_17.rds")

# 3.1 CT 2011==============================================
#**********************************************************
# CROSS TABLE WITH NORMAL COVER ENTRIES
ct_11 = dcast(d_11, pnr ~ art, value.var = "cover", fill = 0)
ct_11$pnr = paste0(ct_11$pnr, "_2011")

# CROSS TABLE WITH PRESENCE (1) ABSENCE (0)
ct_11_pa = vegan::decostand(dplyr::select(ct_11, -pnr), "pa")
ct_11_pa$pnr = ct_11$pnr

# 3.2 CT 2012==============================================
#**********************************************************
ct_12 = dcast(d_12, pnr ~ art, value.var = "cover", fill = 0)
ct_12$pnr = paste0(ct_12$pnr, "_2012")

# CROSS TABLE WITH PRESENCE (1) ABSENCE (0)
ct_12_pa = vegan::decostand(dplyr::select(ct_12, -pnr), "pa")
ct_12_pa$pnr = ct_12$pnr

# 3.3 CT 2016==============================================
#**********************************************************
ct_16 = dcast(d_16, pnr ~ art, value.var = "cover", fill = 0)
ct_16$pnr = paste0(ct_16$pnr, "_2016")

# CROSS TABLE WITH PRESENCE (1) ABSENCE (0)
ct_16_pa = vegan::decostand(dplyr::select(ct_16, -pnr), "pa")
ct_16_pa$pnr = ct_16$pnr

# 3.4 CT 2017==============================================
#**********************************************************
ct_17 = dcast(d_17, pnr ~ art, value.var = "cover", fill = 0)
ct_17$pnr = paste0(ct_17$pnr, "_2017")

# CROSS TABLE WITH PRESENCE (1) ABSENCE (0)
ct_17_pa = vegan::decostand(dplyr::select(ct_17, -pnr), "pa")
ct_17_pa$pnr = ct_17$pnr

# 3.5 Combine CTs==========================================
#**********************************************************
# presence-absence cross-table
ct_pa = plyr::rbind.fill(ct_11_pa, ct_12_pa, ct_16_pa, ct_17_pa)
ct_pa[is.na(ct_pa)] = 0

# cover cross_table
ct_cover = plyr::rbind.fill(ct_11, ct_12, ct_16, ct_17)
ct_cover[is.na(ct_cover)] = 0

# Remember, there are 196 obs instead of 200 because:
# 1: in 2011 there were 3 plots (2, 18, 20) without any vegetation cover
# 2: in 2017 plot 25 was inundated and could not be visited

# save your result
# saveRDS(ct_pa, "images/11_ct_pa.rds")
# saveRDS(ct_cover, "images/11_ct_cover.rds")

#**********************************************************
# 4 LONDO--------------------------------------------------
#**********************************************************

# ct_cover = readRDS("images/11_ct_cover.rds")

# Londo scale in accordance with Londo (1976) and after Table 3.1 in Leyer &
# Wesche (2008)

# apply londo-transformation
# make sure that 0 stays 0

# this would be the most elegant solution, however, we also need the ct of the
# single years
tmp = ct_cover
ind = names(tmp) != "pnr"
tmp[, ind] =
  cut(unlist(tmp[, ind]),
      c(0, 1e-32, 1, 3, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 100),
      labels = c(0, 0.1, 0.2, 0.4, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      right = FALSE) %>%
  as.character %>%
  as.numeric
# using tmp as it has the right pnr (1_2011, etc.)
# saveRDS(tmp, "images/11_ct_londo.rds")  

convert_to_londo = function(x) {
  ind = names(x) != "pnr"
  x[, ind] = 
    cut(unlist(x[, ind]),
        c(0, 1e-32, 1, 3, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 100),
        labels = c(0, 0.1, 0.2, 0.4, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        right = FALSE) %>%
    as.character %>%
    as.numeric
  x[, "pnr"] = gsub("_.*", "", x[, "pnr"]) %>%
    as.integer
  x
}

ct_londo_11 = convert_to_londo(ct_11)
ct_londo_12 = convert_to_londo(ct_12)
ct_londo_16 = convert_to_londo(ct_16)
ct_londo_17 = convert_to_londo(ct_17)
ct_londo = plyr::rbind.fill(ct_londo_11, ct_londo_12, ct_londo_16, ct_londo_17)
ct_londo[is.na(ct_londo)] = 0
# same result except for pnr?
all.equal(select(ct_londo, -pnr), select(tmp, -pnr))  # TRUE, perfect
# saveRDS(ct_londo_11, "images/11_ct_londo_11.rds")
# saveRDS(ct_londo_12, "images/11_ct_londo_12.rds")
# saveRDS(ct_londo_16, "images/11_ct_londo_16.rds")
# saveRDS(ct_londo_17, "images/11_ct_londo_17.rds")

#**********************************************************
# 5 ADD MISSING LIFEFORMS----------------------------------
#**********************************************************

# 5.1 Add missing lifeforms================================
#**********************************************************

spec = names(ct_londo)
spec = spec[spec != "pnr"]

setdiff(spec, lf$art)
setdiff(lf$art, spec)

spec = spec %>% data.frame(spec = ., stringsAsFactors = FALSE)
lf = full_join(lf, spec, by = c("art" = "spec"))
lf$type = as.factor(lf$type)
levels(lf$type)
lookup = levels(lf$type)
x = c(4, 1, 6, 1, 1, 1, 1, 1, 1, 2, 1, 4, 1, 1, 1, 2, 2, 2, 1, 1, 4, 1, 4, 2, 2,
      1, 1, 2, 4, 4, 4, 4, 1)
lf[is.na(lf$type), "type"] = lookup[x]

# 5.2 Classification scheme================================
#**********************************************************

# collapse factors
levels(lf$type) %<>%
  fct_collapse("herbs" = c("Annual herb", "Perennial herb", "Parasitic shrub"),
               "woody" = c("Woody shrub", "Tree"),
               "grass" = "Grass")
lf = rename(lf, species = art)
# save your result
saveRDS(lf, "images/11_condensed_lifeforms.rds")

#**********************************************************
# 6 ENVIRONMENTAL DATA-------------------------------------
#**********************************************************

# 6.1 Soil variables=======================================
#**********************************************************
# units of soil variables:
# P: g/kg
# sand: %
# skeleton: %
# ph: no unit
# lf_s: electrical conductivity in mikroSiemens
# K (potassium), Na, Mg,  Ca: cmol/kg
# cn: ratio (between carbon and nitrogen)
names(soil) = gsub("_cmolkg|_g_kg", "", names(soil)) %>%
  tolower
# only select the columns you need
soil = soil[, c("pnr", "p", "sand", "skeleton", "ph", "lf_s", "k", "na", "mg", 
                "ca", "cn")]
# Plot 23 was rather heterogeneous which is why we opted to take three soil
# samples, here we take the mean of the values
cols = c("p", "sand", "skeleton", "lf_s", "k", "na", "mg", "ca", "cn")
soil[soil$pnr == 231, cols] =  
  colMeans(soil[soil$pnr %in% c(231, 232, 233), cols])
soil[soil$pnr == 231, "ph"] = 
  log10(mean(10^soil[soil$pnr %in% c(231, 232, 233), "ph"]))
# just keep the first instance of 23 and rename pnr accordingly
soil[soil$pnr == 231, "pnr"]  = 23
# delete 232 and 233
soil = filter(soil, !pnr %in% c(232, 233))

# Hier noch einmal gut nachdenken, ob es wirklich sinnvoll ist ein Inf in 0
# umzuwandeln -> Daten angucken, ggf. 1000 besser -> ASK GREGOR
soil[soil$cn == Inf, "cn"] = 0

# join pnr2 (= plot ID ordered in accordance with distance to see) and make it 
# pnr
soil = inner_join(soil, dplyr::select(topo, pnr, pnr2), by = "pnr") %>%
  select(-pnr, pnr = pnr2) %>%
  arrange(pnr)
# save your result
# saveRDS(soil, "images/11_soil.rds")

# 6.2 Topographic variables================================
#**********************************************************

filter(plova, pnr > 230)
# rename 231 to 23
plova[plova$pnr == 231, "pnr"] = 23
# delete 232 and 233
plova = filter(plova, !pnr %in% c(232, 233))
# join landuse to topo
topo = inner_join(topo, dplyr::select(plova, pnr, landuse), by = "pnr")
# make pnr2 (= plot ID ordered in accordance with distance to see) the unique
# plot ID
topo = select(topo, -pnr, pnr = pnr2) %>%
  arrange(pnr)
# save your result
# saveRDS(topo, "images/11_topo.rds")

# merge topo and soil
# ct_pa = readRDS("images/11_ct_pa.rds")
env_data = inner_join(topo, soil, by = "pnr")
pnr = select(ct_londo, pnr)
pnr = pnr %>%
  mutate(id = gsub("_.*", "", pnr),
         id = as.integer(id))
env_data = inner_join(pnr, env_data,  by = c("id" = "pnr"))
# save your result
# remember 4 plots are missing: 2011: plots 2, 18, 20; 2017: plot 25
# saveRDS(env_data, "images/11_env_data.rds")


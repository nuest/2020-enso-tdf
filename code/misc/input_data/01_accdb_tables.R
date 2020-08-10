# Filename: 00_rodbc.R (2019-09-05)

# TO DO: Read in raw data from accdb, i.e., only possible under Windows

# Author(s): Jannes Muenchow

#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************

# 1. ATTACH PACKAGES AND DATA
# 2. PLOT-SPECIES MATRICES (2011, 2012, 2016, 2017)
# 3. ENVIRONMENTAL DATA
# 4. IRRIGATION-NUTRIENT EXPERIMENT
# 5. SAVE TO GPKG

#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("RODBC")
library("RSQLite")
library("dplyr")
library("data.table")
library("magrittr")
library("lubridate")
library("readxl")
library("reshape2")

# little helper function because we have to do the same thing 4 times
adjust_tab = function(d, cover = "Deckung_%") {
  d %>% 
    # if Arbeitsname does not exist, this only throws a warning message
    dplyr::select(-one_of("Arbeitsname")) %>%
    dplyr::rename(art = "Artkürzel", cover = cover) %>%
    left_join(., lf, by = "art") %>%
    # arrange by plot id (plot number)
    arrange(pnr)
}

#**********************************************************
# 2 PLOT-SPECIES MATRICES (2011, 2012, 2016, 2017)---------
#**********************************************************

# NOTE:
# use 32-bit version of R/Microsoft Office and make sure that you have installed
# Microsoft Database Engine Distributable (32-bit)
# browseURL("https://www.microsoft.com/en-us/download/details.aspx?id=13255")
# remember also that RStudio no longer supports 32-bit R versions, so you have 
# to run RODBC code in the R console directly

# life form data (lf)
con = odbcConnectAccess2007("data/raw_data/Transekt_0316.accdb")
lf = sqlFetch(con, "qurSpecOrigin", as.is = TRUE)
close(con)
lf = dplyr::select(lf, art = "Artkürzel", type = "Life form") %>%
  mutate(type = as.factor(type))

# sample campaign 2011
con = odbcConnectAccess2007("data/raw_data/Transekt_0411.accdb")
sqlTables(con)
data = sqlFetch(con, "qurPrepCross", as.is = TRUE)
topo = sqlFetch(con, "tblSpatial", as.is = TRUE)
close(con)

par(mfrow = c(1, 2))
plot(topo[, c("x", "y")], type = "n")
text(topo[, c("x", "y")], labels = topo$pnr)
plot(topo[, c("x", "y")], type = "n")
text(topo[, c("x", "y")], labels = topo$pnr2)
# so, we have to use pnr2 (ordered by distance to see, and used as pnr in 12, 16
# and 17), not pnr!!!!!

# but before doing that, we have to take care of 231 (we collected three soil
# samples there due to the found edaphic heterogeneity, and named these samples
# 231, 232, 233)
# -> it is actually plot 23 
# using data.table syntax (neat)
data = as.data.table(data)
data[pnr == 231, pnr := 23]
# join pnr2
data = inner_join(data, dplyr::select(topo, pnr, pnr2), by = "pnr")
# delete pnr and rename pnr2 to pnr
data = dplyr::select(data, -pnr, pnr = pnr2)
d_11 = adjust_tab(data, cover = "Deckung")  # warning is ok
setdiff(1:50, d_11$pnr)

# sample campaign 2012
con = odbcConnectAccess2007("data/raw_data/Transekt_0312.accdb")
sqlTables(con)
db_odbc = sqlFetch(con, "qurArtDeckungPlot", as.is = TRUE)
close(con)
db_odbc %<>% mutate(Deckung = as.numeric(Deckung))
d_12 = adjust_tab(db_odbc, cover = "Deckung")

# sample campaign 2016
con = odbcConnectAccess2007("data/raw_data/Transekt_0316.accdb")
sqlTables(con)
db_odbc = sqlFetch(con, "qurArtDeckungPlot", as.is = TRUE)
close(con)
# all.equal(data, as.data.table(db_odbc))  # TRUE
d_16 = adjust_tab(db_odbc)

# sample campaign 2017
con = odbcConnectAccess2007("data/raw_data/Transekt_0417.accdb")
sqlTables(con)$TABLE_NAME
db_ocdb = sqlFetch(con, "qurArtDeckungPlot", as.is = TRUE)
close(con)
db_ocdb[is.na(db_ocdb$Arbeitsname), ]  
# Plot 25 has no recordings -> was flooded
d_17 = adjust_tab(db_ocdb)

#**********************************************************
# 3 ENVIRONMENTAL DATA-------------------------------------
#**********************************************************

con = odbcConnectAccess2007("data/raw_data/Transekt_0411.accdb")
sqlTables(con)$TABLE_NAME
# edaphic variables
soil = sqlFetch(con, "tblSoil", as.is = TRUE)
# topographic variables 
topo = sqlFetch(con, "tblSpatial", as.is = TRUE)
# plot variables
plova = sqlFetch(con, "tblPlova", as.is = TRUE)
close(con)

# rain data from Rodolfo
# read in the individual sheets with specified range
paita = read_excel("data/raw_data/rain_peru.xls", "RainPaita", 
                   range = "A11:M33")
piura = read_excel("data/raw_data/rain_peru.xls", "RainPiura", 
                   range = "A7:M35")
chulu = read_excel("data/raw_data/rain_peru.xls", "RainChulucanas",
                   range = "B9:N31")
# retrieve month names
Sys.setlocale("LC_TIME", "English")
char_months = lubridate::month(ymd(010101) + months(1:12 - 1), label = TRUE, 
                               abbr = TRUE) %>%
  tolower %>%
  as.character
names(paita) = c("year", char_months)
names(piura) = c("year", char_months)
names(chulu) = c("year", char_months)
# reshape
paita = melt(paita, id.vars = "year") %>% mutate(station = "paita") 
piura = melt(piura, id.vars = "year") %>% mutate(station = "piura") 
chulu = melt(chulu, id.vars = "year") %>% mutate(station = "chulu") 

# Combine to one df and rename columns  
precip = rbind(paita, piura, chulu) %>% rename(month = variable, precip = value)
precip$station = as.factor(precip$station)

#**********************************************************
# 4 IRRIGATION-NUTRIENT EXPERIMENT-------------------------
#**********************************************************

channel = odbcConnectAccess2007("data/raw_data/irrigation.accdb")
sqlTables(channel)
# number of indiviudal species per observation date (d)
exp_count = sqlFetch(channel, "qurCount", as.is = TRUE)
# irrigation (rain) input (rain)
exp_irr = sqlFetch(channel, "tblRain", as.is = TRUE)
# cover in % per plot and observation date (pcov)
exp_pcov = sqlFetch(channel, "qurCover", as.is = TRUE)
close(channel)


#**********************************************************
# 5. SAVE TO GPKG------------------------------------------
#**********************************************************

con = dbConnect(RSQLite::SQLite(), "data/tables.gpkg")
dbListTables(con)
dbWriteTable(con, name = "lifeform", value = lf)
dbWriteTable(con, name = "plot_species_matrix_2011", value = d_11)
dbWriteTable(con, name = "plot_species_matrix_2012", value = d_12)
dbWriteTable(con, name = "plot_species_matrix_2016", value = d_16)
dbWriteTable(con, name = "plot_species_matrix_2017", value = d_17)
dbWriteTable(con, name = "topography", value = topo)
dbWriteTable(con, name = "soil", value = soil)
dbWriteTable(con, name = "plot_variables", value = plova)
dbWriteTable(con, name = "precipitation", value = precip)
dbWriteTable(con, name = "experiment_count", value = exp_count)
dbWriteTable(con, name = "experiment_irrigation", value = exp_irr)
dbWriteTable(con, name = "experiment_cover", value = exp_pcov)
dbDisconnect(con)

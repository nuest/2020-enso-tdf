# Filename: 17_irrigation_nutrient_experiment.R (2019-10-15)

# TO DO: Analyze irrigation-nutrient experiment data

# Author(s): Jannes Muenchow

#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************

# 1. ATTACH PACKAGES AND DATA
# 2. DATA PREPARATION
# 3. IRRIGATION PLOT

#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("RSQLite")
library("lattice")
library("dplyr")
library("here")

# attach data
con = dbConnect(SQLite(), "data/tables.gpkg") 
dbListTables(con)
# species counted per observation
count = dbReadTable(con, "experiment_count")  # d
# irrigation/nutrient input data
irr = dbReadTable(con, "experiment_irrigation")  # rain
# cover per plot in %
cover = dbReadTable(con, "experiment_cover")  # pcov
dbDisconnect(con)

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

count = count %>%
  mutate_at(c("plot_name", "abb"), as.factor) %>%
  mutate(obs_date = as.Date(obs_date))

cover = cover %>%
  mutate(obs_date = as.Date(obs_date),
         plot_name = as.factor(plot_name))

names(irr) = tolower(names(irr))

irr = irr %>%
  mutate_at(c("nn", "sn", "prec"), as.numeric) %>%
  mutate(id = as.Date(id))

#**********************************************************
# 3 IRRIGATION PLOT----------------------------------------
#**********************************************************

# monthly precipitation in Piura from December 2012 to May 2013
# group_by(irr, as.factor(format(irr$id, "%b"))) %>%
#   summarize(sum(prec))

# irrigation dataframe (natural precipitation, normal Nino water input and 
# super Nino water input)
prec = data.frame(prec = c(cumsum(irr$prec), cumsum(irr$nn), cumsum(irr$sn)))
prec$date = rep(irr$id, 3) 
prec$enso = as.factor(rep(c("prec", "nn", "sn"), each = nrow(irr)))
# I want to order the levels as follows: prec, nn, sn
prec$enso = factor(prec$enso, levels = c("prec", "nn", "sn"))
# I want to rename the levels
levels(prec$enso) = c("Control", "Normal Niño", "Super Niño")

# aggregate cover data
a = as.data.frame(tapply(cover$cover, 
                         INDEX = list(cover$obs_date, cover$plot_name), 
                         FUN = sum))

# just keep every second column as a and b plots are the same
a = a[, which(1:ncol(a) %% 2 == 0)]
a$date = as.Date(rownames(a))

# Rescale (necessary due to log-scale)
b = unlist(a[, 1:6])
b = b / max(b) * log10(398.5)
b = as.data.frame(matrix(data = b, nrow = 12, ncol = 6))
b$date = a$date

# to plot manually axes labels outside of the panel, we have to disable the
# clipping beforehand
my.setting = trellis.par.get("clip")
str(my.setting)
my.setting$panel = "off"
trellis.par.set("clip", my.setting)
trellis.par.get("clip")

p_1 = xyplot((prec + 1) ~ date | enso, data = prec, xlab = "",
       ylab = list("Water amount (mm) and \n Plant coverage (%)", cex = 0.8),
       layout = c(3, 1), aspect = 1, type = "l",
       # strip should be white (bg) and the text (par.strip.text) should be a
       # bit smaller
       strip = strip.custom(bg = "white",
                            par.strip.text = list(cex = 0.8)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same", log = TRUE),
                     tck = c(0, 0),
                     # ticks on top are suppressed (1 = left/bottom, 0 =
                     # right/top
                     alternating = c(0, 1, 0),
                     draw = FALSE),
       # Create your own legend
       key = list(space = "right", 
                  text = list(c("water input", "fertilized", "untreated"),
                              cex = 0.8),
                  lines = list(lty = c("solid", "solid", "dashed"),
                               col = c("slateblue", "salmon", "lightgray"),
                               lwd = 2, cex = 0.6)),
       panel = function(x, y) {
         panel.lines(x, y, col = "slateblue", lty = "solid", lwd = 2, 
                     axis.label = FALSE)
         if (panel.number() == 1) {
           at = c(log10(1), log10(10), log10(100), log10(1000))
           panel.axis(side = "bottom", at = prec$date[c(1, 63, 122)], 
                      labels = c(),  rot = 0, half = FALSE, outside = TRUE)
           panel.axis(side = "left", half = FALSE, at = at, 
                      labels = c(1, 10, 100, 1000))
           panel.lines(b[, 5] ~ b$date, col = "salmon", lwd = 2)
           panel.lines(b[, 6] ~ b$date, col = "lightgray", lty = "dashed", lwd = 2)
         }
         # Add a label to the x-axis
         if (panel.number() == 2) {
           panel.axis(side = "bottom", at = prec$date[c(1, 63, 122)],
                      labels = c("Dec", "Feb", "Apr"), rot = 0, half = FALSE,
                      outside = TRUE)
           panel.lines(b[, 1] ~ b$date, col = "salmon", lwd = 2)
           panel.lines(b[, 2] ~ b$date, col = "lightgray", lty = "dashed", 
                       lwd = 2)
         }
         if (panel.number() == 3) {
           at = log10(c(1, 10, 100, 1000))
           panel.axis(side = "bottom", at = prec$date[c(1, 63, 122)], 
                      labels = c(), rot = 0, half = FALSE, outside = TRUE)
           panel.axis(side = "right", half = FALSE, at = at, 
                      labels = c(1, 10, 20, 30))
           panel.lines(b[, 3] ~ b$date, col = "salmon", lwd = 2)
           panel.lines(b[, 4] ~ b$date, col = "lightgray", lty = "dashed", 
                       lwd = 2)
         }
       }
)

# png("figures/17_nutrient_irrigation_experiment.png", units = "cm", 
#     width = 14, height = 6,
#     res = 300)
print(p_1)
# dev.off()
save(p_1, prec, b, file = here("figures/17_nutrient_irrigation_experiment.rda"))

#**********************************************************
# 4 DIVERSITY ACROSS TIME----------------------------------
#**********************************************************

alpha = group_by(count, plot_name, obs_date) %>% 
  summarize(n_spec = n_distinct(abb))
# I want to rename the levels
levels(alpha$plot_name) = gsub("NN", "Normal Niño", levels(alpha$plot_name))
levels(alpha$plot_name) = gsub("SN", "Super Niño", levels(alpha$plot_name))
levels(alpha$plot_name) = gsub("test", "Control", levels(alpha$plot_name))
levels(alpha$plot_name) = gsub("1", "fertilized_1", levels(alpha$plot_name))
levels(alpha$plot_name) = gsub("2", "fertilized_2", levels(alpha$plot_name))
levels(alpha$plot_name) = gsub("3", "unfertilized_1", levels(alpha$plot_name))
levels(alpha$plot_name) = gsub("4", "unfertilized_2", levels(alpha$plot_name))

# make sure to use English locale
Sys.setlocale("LC_TIME", "C")
xyplot(n_spec ~ obs_date | plot_name, data = alpha, type = c("p", "l"), 
       ylab = "No. of species", xlab = "Observation date",
       strip = strip.custom(bg = "white",
                            par.strip.text = list(cex = 0.7)),
       scales = list(tck = c(1, 0),
                     # ticks on top are suppressed (1 = left/bottom, 0 =
                     # right/top
                     alternating = c(1, 1, 1)))
saveRDS(alpha, file = here("images/17_alpha.rds"))

# tidy columns
alpha = readRDS(alpha, file = here("images/17_alpha.rds"))
alpha = tidyr::separate(alpha, plot_name, 
                        into = c("phase", "treatment", "repeat"), sep = "_")
# complete missing dates with an observation of 0 species
alpha = alpha %>% 
  tidyr::complete(obs_date, tidyr::nesting(phase, treatment, `repeat`), 
           fill = list(n_spec = 0))
# aggregate to the mean per phase and treatment, i.e., take the mean of the
# repetitions by phase, treatment and obs_date
agg = group_by(alpha, phase, treatment, obs_date) %>% 
  summarize(n_spec = mean(n_spec))
# make sure to use English locale
Sys.setlocale("LC_TIME", "C")
# plot showing mean species richness across time
spri_exp = xyplot(n_spec ~ obs_date | phase, group = treatment, data = agg, 
       type = c("p", "l"), col = c("salmon", "lightgray"), lwd = 2,
       lty = c("solid", "dashed"),
       xlab = "", 
       ylab = list("Number of species", cex = 0.8),
       strip = strip.custom(bg = "white",
                            par.strip.text = list(cex = 0.8)),
       layout = c(3, 1), aspect = 1,
       scales = list(tck = c(1, 0),
                     alternating = c(1, 1, 1)),
       key = list(space = "right", columns = 1,
                  text = list(c("fertilized", "unfertilized"), cex = 0.8),
                  lines = list(col = c("salmon", "lightgray"),
                               lty = c("solid", "dashed"))))
saveRDS(spri_exp, file = here("images/17_irrigation_spri.rds"))

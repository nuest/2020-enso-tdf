#**********************************************************
# 6 DCA VIZ------------------------------------------------
#**********************************************************

library("stringr")
library("vegan")
library("dplyr")
library("lattice")
library("magrittr")
library("ggplot2")

# source own functions
source("code/funs/ordi_helper_funs.R")

londo = readRDS("images/11_ct_londo.rds")
env_data = readRDS("images/11_env_data.rds")
sc = readRDS("images/15_dca_scores.rds")

dca = ord_fun(londo, env_data, method = "dca")
# create dataframe with DCA scores and metadata
scores_df = data.frame(pnr = londo$pnr,
                       # get year of survey
                       year = str_sub(londo$pnr, start = -4),
                       # get plotID from plot number
                       plotID = str_sub(londo$pnr, end = -6),
                       # create grouping variables
                       # group_west = 1:18, group_middle = 19:38, 
                       # group_east = 39:49
                       group = factor(rep(c(1, 2, 3), c(18, 20, 11)),
                                      labels = c("west", "middle", "east")),
                       sc)

# source("code/funs/ordi_helper_funs.R")
# multi_panel_plot(DCA1, DCA2, year, id = plotID, data = scores_df)


# 6.1 Posthoc env fitting==================================
#**********************************************************

# simple test
test = envfit(dca, env_data$x)
test$vectors$r
scores(test, display = "vectors")

# check for compatibilty
str(env_data) # 1, 25, 39 are character type
summary(env_data)
# remove character-type colums (pnr, bodenart, cn) from calculation
sel_columns = unlist(lapply(env_data, class)) != "character"
# calculate envfit for all variables
set.seed(123)
envfit_vectors = sapply(env_data[, sel_columns],
                        function(x){
                          # calculate envfit
                          temp_envfit = envfit(dca, x, perm = 1000)
                          # get correlation and significance with ordination
                          temp_r_p = c(temp_envfit$vectors$r, temp_envfit$vectors$pvals)
                          # use scores - scales the vector arrow coordinates
                          # by the correlation with the ordination configuration =
                          # weak relationship -> shorter arrows
                          temp_scores = scores(temp_envfit, display = "vectors")
                          # output
                          c(temp_scores, temp_r_p)
                        }
)
# create empty dataframe with envfit vector values for plotting
envfit_vectors_df = data.frame(name = colnames(envfit_vectors),
                               DCA1 = envfit_vectors[1, ],
                               DCA2 = envfit_vectors[2, ],
                               R_2 = envfit_vectors[3, ],
                               Pval = envfit_vectors[4, ], row.names = NULL)

# keep only signficiant variables
envfit_vectors_df = envfit_vectors_df[envfit_vectors_df$Pval < 0.05, ]
rm(envfit_vectors, test, sel_columns)


# 6.2 DCA plotting=========================================
#**********************************************************

## Plot with raw sites-data
scores_df %>% ggplot(aes(x = DCA1, y = DCA2)) +
  geom_point(aes(color = group)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(. ~ year) +
  coord_fixed() + theme_bw()

## Plot with raw sites-data and all envfits
scores_df %>% ggplot(aes(x = DCA1, y = DCA2)) +
  geom_point(aes(color = group)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_segment(data = envfit_vectors_df,
               aes(x = 0, y = 0, xend = DCA1, yend = DCA2),
               arrow = arrow(length = unit(0.2, "cm"))) +
  facet_grid(. ~ year) +
  coord_fixed()


## Plot with envfit-vectors selection

# create function to plot dca-scores with envfit vectors for a selection of envfit-variables
plot_fun = function(dca_score, envfit_scores, envfit_select){
  # plotting of dca scores
  ggplot(dca_score, aes(x = DCA1, y = DCA2)) +
    geom_point(aes(color = group)) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    # plotting of envfit vectors
    geom_segment(data = envfit_scores[envfit_scores$name %in% envfit_select, ],
                 aes(x = 0, y = 0, xend = DCA1, yend = DCA2),
                 arrow = arrow(length = unit(0.2, "cm")), alpha = 0.4) +
    geom_text(data = envfit_scores[envfit_scores$name %in% envfit_select, ],
              aes(x = DCA1, y = DCA2, label = name), size = 4) +
    # plot setup
    facet_grid(. ~ year) +
    ggtitle(paste(sel_topo, collapse = ", ")) +
    coord_fixed() + theme_bw()
}

# Plots with envfit-vectors selection
# GREGOR, pls ADJUST accordingly
sel_topo = c("hcurv", "vcurv", "logcarea", "alt")
sel_soil = c("p", "sand", "skeleton", "ph", "lf_s", "k", "na", "mg", "ca", "cn")
sel_loc = c("lon", "lat", "dist_sea", "alt")
sel_env = c("prec", "ndvi_11", "ndvi_12", "ndvi_16", "ndvi_17")

plot_fun(scores_df, envfit_vectors_df, sel_topo)
plot_fun(scores_df, envfit_vectors_df, sel_soil)
plot_fun(scores_df, envfit_vectors_df, sel_loc)
plot_fun(scores_df, envfit_vectors_df, sel_env)


## Plot with grouping by region

# calculate mean score per region and year
scores_df %>%
  group_by(group, year) %>%
  summarise(DCA1 = mean(DCA1), DCA2 = mean(DCA2)) %>%
  ggplot(aes(x = DCA1, y = DCA2)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(aes(color = group)) +
  geom_path(aes(group = group, color = group)) +
  geom_text(aes(label = rep(c(11, 12, 16, 17), 3)), size = 3, nudge_y = 0.06) +
  coord_fixed() + theme_bw() 


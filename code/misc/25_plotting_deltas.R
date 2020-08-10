# Filename: 26_plotting_deltas.R (2018-03-11)
#
# Author(s): Gregor Didenko
#
#**********************************************************
# CONTENTS:-------------------------------------------------
#
# LOAD AND PREPARE DATA
# Calculate deltas 12-11, 16-12, 17-16
# Plotting deltas
# DCA RGB COLOR PLOTS
# NDVI RGB COLOR PLOTS
#
#**********************************************************

#************************************************
# LOAD AND PREPARE DATA ----
#************************************************

pacman::p_load(raster, ggtern, dplyr, reshape2, rasterVis, ggplot2, Ternary)

ndvi_rasters = readRDS("images/14_ndvi_rasters.rds")

pred_rasters = readRDS("images/24_pred_rasters.rds")

# create raster stack of selected years
ndvi_stack = stack(ndvi_rasters$ndvi_11, ndvi_rasters$ndvi_12, ndvi_rasters$ndvi_16, ndvi_rasters$ndvi_17)
names(ndvi_stack) = c("ndvi_2011", "ndvi_2012", "ndvi_2016", "ndvi_2017")



#************************************************
# Calculate deltas 12-11, 16-12, 17-16 ----
#************************************************

# NDVI deltas
ndvi_delta_12_11 = ndvi_rasters$ndvi_12 - ndvi_rasters$ndvi_11
ndvi_delta_16_12 = ndvi_rasters$ndvi_16 - ndvi_rasters$ndvi_12
ndvi_delta_17_16 = ndvi_rasters$ndvi_17 - ndvi_rasters$ndvi_16
d_ndvi_stack = stack(ndvi_delta_12_11, ndvi_delta_16_12, ndvi_delta_17_16)
names(d_ndvi_stack) = c("d_12_11", "d_16_12", "d_17_16")

## Calculate delta with direction
# get min value
min_val = min(values(pred_rasters), na.rm = TRUE)*-1 # -1.730218

# deltas predicted scores with added minimal value to get only change > 0
scores_delta_12_11 = (pred_rasters$pred_12 + min_val) - (pred_rasters$pred_11 + min_val)
scores_delta_16_12 = (pred_rasters$pred_16 + min_val) - (pred_rasters$pred_12 + min_val)
scores_delta_17_16 = (pred_rasters$pred_17 + min_val) - (pred_rasters$pred_16 + min_val)


## New way: calculating absolute difference between values with no direction
raster_diff = function(raster_a, raster_b) {
  output = as.numeric(0)
  # For every value pair in raster_a and raster_b
  for(i in 1:length(raster_a)){
    value_a = values(raster_a)[i]
    value_b = values(raster_b)[i]
    # When value is NA, output NA
    if(is.na(value_a) | is.na(value_b)){
      output[i] = NA
    # calculate absolute difference between raster values
    } else {
      output[i] = abs(diff(c(value_a, value_b)))
    }
  }
  return(output)
}

# DCA-scores deltas
values(scores_delta_12_11) = raster_diff(pred_rasters$pred_12, pred_rasters$pred_11)
values(scores_delta_16_12) = raster_diff(pred_rasters$pred_16, pred_rasters$pred_12)
values(scores_delta_17_16) = raster_diff(pred_rasters$pred_17, pred_rasters$pred_12)

d_pred_stack = stack(scores_delta_12_11, scores_delta_16_12, scores_delta_17_16)
names(d_pred_stack) = c("d_12_11", "d_16_12", "d_17_16")
# saveRDS(d_pred_stack, "images/25_d_pred_stack.rds")

#************************************************
# Plotting deltas ----
#************************************************

rasterVis::gplot(pred_rasters) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  ggtitle("Raw GAM Prediction Scores") +
  coord_equal()

rasterVis::gplot(d_pred_stack) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  scale_fill_gradientn(colours = rev(topo.colors(10))) +
  ggtitle("Delta GAM Prediction Scores") +
  coord_equal()

rasterVis::gplot(d_ndvi_stack) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_gradient2(low = 'red', mid = "white", high = 'green') +
  ggtitle("Delta NDVI") +
  coord_equal()

rasterVis::gplot(ndvi_stack) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  ggtitle("raw NDVI") +
  coord_equal()


#************************************************
# DCA RGB PLOTS  ----
#************************************************

pred_df = as.data.frame(values(d_pred_stack))
summary(pred_df)

## create RGB colors from delta, by transforming the data to a range of 0 to 1
## with 1 beeing the maximum value in all delta-data-sets

# get max value of delta-scores as base for transformation
## transform data to fit into 0 - 1
max_val = max(pred_df ,na.rm = TRUE)
pred_df = pred_df %>% mutate(d2 = d_12_11 / max_val,
                                 d6 = d_16_12 / max_val,
                                 d7 = d_17_16 / max_val)

## create vector of colors from delta predicted DCA scores
# get all NA and set to 0 = black (NA not allowed in rgb function)
sel = is.na(pred_df$d2) | is.na(pred_df$d6) | is.na(pred_df$d7)
pred_df2 = pred_df
summary(pred_df2)
pred_df2[sel, 4:6] = 1

# Which delta years have the highest influence on color (1 = a lot of change)
boxplot(pred_df2[, 4:6])
# boxplot(pred_df2[, 1:3]) # same pattern, transformation worked correct

# create color vector
pred_col = rgb(red = 1-pred_df2$d7,
                 green = 1-pred_df2$d2,
                 blue = 1-pred_df2$d6)

# create empty raster from raster with most NA cells
empty_raster = scores_delta_12_11
coordinates = data.frame(coordinates(empty_raster))
plot(coordinates, col = pred_col, asp = 1)
ggplot(coordinates, aes(x = x, y = y)) + geom_point(colour = pred_col) + 
  coord_equal() + theme_bw()

## Much easier way!
# coeff to set values to 0-255
coef = 255 / max(values(d_pred_stack), na.rm = TRUE)

## 1 = 12-11, 2 = 16-12, 3 = 17-16
#plotRGB(d_pred_stack * coef, 3, 1, 2)
plotRGB(255 - (d_pred_stack * coef), r = 1, g = 2, b = 3, colNA = "black",
        axes = TRUE, main = "test", xlab = "xlab")

RStoolbox::ggRGB(255 - (d_pred_stack * coef), 1, 2, 3) +
  theme(panel.background = element_rect(fill = 'black', colour = 'black'),
        panel.grid.major = element_line(colour = 'black'),
        panel.grid.minor = element_line(colour = 'black'))


#************************************************
# NDVI RGB PLOTS  ----
#************************************************

# To avoid negative Numbers
min(values(d_ndvi_stack), na.rm = TRUE) # 0.6818
d_ndvi_stack2 = d_ndvi_stack + 0.68181

coef = 255 / max(values(d_ndvi_stack2), na.rm = TRUE)

plotRGB(255 - (d_ndvi_stack2 * coef), 1, 2, 3)

RStoolbox::ggRGB(255 - (d_ndvi_stack2 * coef), 1, 2, 3) +
  theme(panel.background = element_rect(fill = 'black', colour = 'black'),
        panel.grid.major = element_line(colour = 'black'),
        panel.grid.minor = element_line(colour = 'black'))



#************************************************
# Calculate deltas 12-11, 16-11, 17-11 ----
#************************************************

# NDVI deltas
ndvi_delta_12_11 = ndvi_rasters$ndvi_12 - ndvi_rasters$ndvi_11
ndvi_delta_16_11 = ndvi_rasters$ndvi_16 - ndvi_rasters$ndvi_11
ndvi_delta_17_11 = ndvi_rasters$ndvi_17 - ndvi_rasters$ndvi_11

d_ndvi_stack_11 = stack(ndvi_delta_12_11, ndvi_delta_16_11, ndvi_delta_17_11)
names(d_ndvi_stack) = c("d_12_11", "d_16_11", "d_17_11")

# DCA-scores deltas
d_pred_stack_11 = stack(pred_rasters$pred_12, pred_rasters$pred_16, pred_rasters$pred_17)
names(d_pred_stack_11) = c("d_12_11", "d_16_11", "d_17_11")

values(d_pred_stack_11$d_12_11) = raster_diff(pred_rasters$pred_12, pred_rasters$pred_11)
values(d_pred_stack_11$d_16_11) = raster_diff(pred_rasters$pred_16, pred_rasters$pred_11)
values(d_pred_stack_11$d_17_11) = raster_diff(pred_rasters$pred_17, pred_rasters$pred_11)


#************************************************
# Plotting deltas ----
#************************************************


rasterVis::gplot(d_pred_stack_11) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  scale_fill_gradientn(colours = rev(topo.colors(10))) +
  ggtitle("Delta GAM Prediction Scores") +
  coord_equal()

rasterVis::gplot(d_ndvi_stack_11) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  scale_fill_gradient2(low = 'red', mid = "white", high = 'green') +
  ggtitle("Delta NDVI") +
  coord_equal()


#************************************************
# RGB COLOR PLOTS  ----
#************************************************

## DCA-Scores
# coeff to set values to 0-255
coef = 255 / max(values(d_pred_stack_11), na.rm = T)

# Inverted colors
plotRGB(255 - (d_pred_stack_11 * coef), 3, 1, 2)


RStoolbox::ggRGB(255 - (d_pred_stack_11 * coef), 2, 3, 1) +
  theme(panel.background = element_rect(fill = 'black', colour = 'black'),
        panel.grid.major = element_line(colour = 'black'),
        panel.grid.minor = element_line(colour = 'black'))

## NDVI
# To avoid negative Numbers
min(values(d_ndvi_stack_11), na.rm = TRUE) # 0.5221
d_ndvi_stack_11b = d_ndvi_stack_11 + 0.5221

coef = 255 / max(values(d_ndvi_stack_11b), na.rm = T)

plotRGB((d_ndvi_stack_11b * coef), 1, 2, 3)

RStoolbox::ggRGB(d_ndvi_stack_11b * coef, 2, 3, 1) +
  theme(panel.background = element_rect(fill = 'black', colour = 'black'),
        panel.grid.major = element_line(colour = 'black'),
        panel.grid.minor = element_line(colour = 'black'))

# save(d_ndvi_stack, d_pred_stack, ndvi_stack,
#    pred_rasters, d_pred_stack_11, d_ndvi_stack_11,
#    #file = "docu/delta_ndvi_dca_plots/25_workspace.rdata")
#    file = "docu/descriptive_statistics_environment/25_workspace.rdata")




#************************************************
# TERNARY PLOTS  ----
#************************************************

## Get triangle with DCA-Scores of sites
scores_df = readRDS("images/23_dca_scores.rds")

# Remove plots that are not in all years
scores_11 = scores_df %>% filter(year == 2011, !id %in% c(2, 18, 20, 25))
scores_12 = scores_df %>% filter(year == 2012, !id %in% c(2, 18, 20, 25))
scores_16 = scores_df %>% filter(year == 2016, !id %in% c(2, 18, 20, 25))
scores_17 = scores_df %>% filter(year == 2017, !id %in% c(2, 18, 20, 25))


vector_diff = function(a, b) {
  output = as.numeric(0)
  # For every value pair a b
  for(i in 1:length(a)){
    value_a = a[i]
    value_b = b[i]
    # When value is NA, output NA
    if(is.na(value_a) | is.na(value_b)){
      output[i] = NA
      # calculate absolute difference between raster values
    } else {
      output[i] = abs(diff(c(value_a, value_b)))
    }
  }
  return(output)
}

scores_delta_111 = data.frame(s12_11 = vector_diff(scores_12$DCA1, scores_11$DCA1),
                             s16_11 = vector_diff(scores_16$DCA1, scores_11$DCA1),
                             s17_11 = vector_diff(scores_17$DCA1, scores_11$DCA1))


# get max value of delta-scores as base for transformation
## transform data to fit into 0 - 1
max = max(c(max(pred_df$d_12_11, na.rm = TRUE),
            max(pred_df$d_16_12, na.rm = TRUE),
            max(pred_df$d_17_16, na.rm = TRUE))
          ,na.rm = TRUE)


library('Ternary')

par(bg=NA)
TernaryPlot(atip='16_11', btip='12_11', ctip='17_11', alab='Green', blab='Red', clab='Blue') 
scores_delta_11 = scores_delta_111 %>% mutate(d2 = (1 - (s12_11 / max)),
                                               d6 = (1 - (s16_11 / max)),
                                               d7 = (1 - (s17_11 / max)))
AddToTernary(points, scores_delta_11[, c(5,4,6)], bg=vapply(scores_delta_11[, c(5,4,6)], function (x) rgb(x[1], x[2], x[3], maxColorValue=1), character(1)), pch=21, cex=1)
dev.copy(png,'ternary.png')
dev.off()


TernaryPlot(atip='16_11', btip='12_11', ctip='17_11', alab='Green', blab='Red', clab='Blue')   
scores_delta_11 = 255 - (scores_delta_111 * coef)
AddToTernary(points, scores_delta_11[, c(2,1,3)], bg=vapply(scores_delta_11[, c(2,1,3)], function (x) rgb(x[1], x[2], x[3], maxColorValue=255), character(1)), pch=21, cex=1.8)
  



#************************************************
# Saving Plots to disk  ----
#************************************************


rasterVis::gplot(pred_rasters) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  ggtitle("Raw GAM Prediction Scores") +
  coord_equal()
ggsave("Raw GAM Prediction Scores.png", path = "figures/25_NDVI_GAM_Maps")

rasterVis::gplot(d_pred_stack) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  scale_fill_gradientn(colours = (topo.colors(10))) +
  ggtitle("Interanual Delta GAM Prediction Scores") +
  coord_equal()
ggsave("Interanual Delta GAM Prediction Scores.png", path = "figures/25_NDVI_GAM_Maps")

rasterVis::gplot(d_ndvi_stack) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_gradient2(low = 'red', mid = "white", high = 'green') +
  ggtitle("Interanual Delta NDVI") +
  coord_equal()
ggsave("Interanual Delta NDVI.png", path = "figures/25_NDVI_GAM_Maps")

rasterVis::gplot(ndvi_stack) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  ggtitle("Raw NDVI") +
  coord_equal()
ggsave("Raw NDVI.png", path = "figures/25_NDVI_GAM_Maps")

rasterVis::gplot(d_pred_stack_11) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  scale_fill_gradientn(colours = (topo.colors(10))) +
  ggtitle("Base 2011 Delta GAM Prediction Scores") +
  coord_equal()
ggsave("Base 2011 Delta GAM Prediction Scores.png", path = "figures/25_NDVI_GAM_Maps")

rasterVis::gplot(d_ndvi_stack_11) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 1) +
  scale_fill_gradient2(low = 'red', mid = "white", high = 'green') +
  ggtitle("Base 2011 Delta NDVI") +
  coord_equal()
ggsave("Base 2011 Delta NDVI.png", path = "figures/25_NDVI_GAM_Maps")

## 1 = 12-11, 2 = 16-12, 3 = 17-16

# loop trough different colorschmemes and save to disk
for( i in list(c(1,2,3), c(1,3,2), c(2,3,1), c(2,1,3), c(3,1,2), c(3,2,1))){
  # rasternames for file-names
  names = c("12_11", "16_12", "17_16")
  
  ## DCA
  # coeff to set values to 0-255
  coef = 255 / max(values(d_pred_stack), na.rm = T)
  RStoolbox::ggRGB(255 - (d_pred_stack * coef), i[1], i[2], i[3]) +
    theme(panel.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major = element_line(colour = 'black'),
          panel.grid.minor = element_line(colour = 'black')) +
    ggtitle(paste0("Interanual R = ", names[i[1]] ,", G = ", names[i[2]] ,
                   ", B = ", names[i[3]] ," Delta GAM Scores")) 
  ggsave(paste0("Interanual GAM R-", names[i[1]] ,
                " G-", names[i[2]] ,
                " B-", names[i[3]] ,".png"), path = "figures/25_NDVI_GAM_Maps",
         width = 8, height = 2.5)

  ## NDVI
  coef = 255 / max(values(d_ndvi_stack2), na.rm = T)
  RStoolbox::ggRGB(255 - (d_ndvi_stack2 * coef),i[1], i[2], i[3]) +
    theme(panel.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major = element_line(colour = 'black'),
          panel.grid.minor = element_line(colour = 'black')) +
    ggtitle(paste0("Interanual R = ", names[i[1]] ,", G = ", names[i[2]] ,
                   ", B = ", names[i[3]] ," Delta NDVI")) 
  ggsave(paste0("Interanual NDVI R-", names[i[1]] ,
                " G-", names[i[2]] ,
                " B-", names[i[3]] ,".png"), path = "figures/25_NDVI_GAM_Maps",
         width = 8, height = 2.5)
  
  # rasternames as file-names
  names = c("12_11", "16_11", "17_11")
  ## DCA-Scores
  # coeff to set values to 0-255
  coef = 255 / max(values(d_pred_stack_11), na.rm = T)
  RStoolbox::ggRGB(255 - (d_pred_stack_11 * coef), i[1], i[2], i[3]) +
    theme(panel.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major = element_line(colour = 'black'),
          panel.grid.minor = element_line(colour = 'black')) +
    ggtitle(paste0("Base 2011 R = ", names[i[1]] ,", G = ", names[i[2]] ,
                   ", B = ", names[i[3]] ," Delta GAM Scores")) 
  ggsave(paste0("Base 2011 GAM R-", names[i[1]] ,
                " G-", names[i[2]] ,
                " B-", names[i[3]] ,".png"), path = "figures/25_NDVI_GAM_Maps",
         width = 8, height = 2.5)
  
  ## NDVI
  coef = 255 / max(values(d_ndvi_stack_11b), na.rm = T)
  RStoolbox::ggRGB(d_ndvi_stack_11b * coef, i[1], i[2], i[3]) +
    theme(panel.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major = element_line(colour = 'black'),
          panel.grid.minor = element_line(colour = 'black')) +
    ggtitle(paste0("Base 2011 R = ", names[i[1]] ,", G = ", names[i[2]] ,
                   ", B = ", names[i[3]] ," Delta NDVI")) 
  ggsave(paste0("Base 2011 NDVI R-", names[i[1]] ,
                " G-", names[i[2]] ,
                " B-", names[i[3]] ,".png"), path = "figures/25_NDVI_GAM_Maps",
         width = 8, height = 2.5)

}

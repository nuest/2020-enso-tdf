library("tidyverse")

precip = readRDS("images/21_precip.rds")
d_pred_stack = readRDS("images/25_d_pred_stack.rds")


#**********************************************************
# CREATE DF WITH PRECIP. SEASON SEP-AUG ---------------
#****************************************<******************

# Heavy Rain can start in December goes to April
# Add rainy season data variable
precip$season = precip$year
# sep-dec from last year, count to season this year
precip[precip$month %in% char_months[9:12], "season"] = precip[precip$month %in% char_months[9:12], "season"] + 1

precip_season = precip %>% group_by(station, season) %>% summarise(precip = sum(precip, na.rm = TRUE))

summary(precip_season)

## Save precipitation object
#saveRDS(precip_season, file = "images/21_precip_seasons.rds")

#**********************************************************
# PLOTTING
#**********************************************************

precip_season %>% 
  filter(season > 2007, season < 2018) %>%
  ggplot(aes(season, precip, color = station)) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2008:2017)


location_weather_stations =
  readr::read_csv("data/spatial/location_weather_stations.csv")

predic_rgb = RStoolbox::ggRGB(d_pred_stack * coef, 3, 1, 2) + 
  geom_point(aes(y = location_weather_stations$Lat,
                 x = location_weather_stations$Long), colour = "white")


rain_chulu = precip_season %>% 
  filter(season %in% c(2011, 2012, 2016, 2017), station == "chulu") %>%
  ggplot(aes(as.factor(season), precip)) +
  geom_col(fill = "blue") + 
  scale_x_discrete(name = "") + 
  scale_y_log10(breaks = c(10, 50, 100, 500, 1000), limits = c(1, 1700))
rain_paita = precip_season %>% 
  filter(season %in% c(2011, 2012, 2016, 2017), station == "paita") %>%
  ggplot(aes(as.factor(season), precip)) +
  geom_col(fill = "blue") + 
  scale_x_discrete(name = "")  + 
  scale_y_log10(breaks = c(10, 50, 100, 500, 1000), limits = c(1, 1700))
rain_piura = precip_season %>% 
  filter(season %in% c(2011, 2012, 2016, 2017), station == "piura") %>%
  ggplot(aes(as.factor(season), precip)) +
  geom_col(fill = "blue") + 
  scale_x_discrete(name = "")  + 
  scale_y_log10(breaks = c(10, 50, 100, 500, 1000), limits = c(1, 1700))

layout <- rbind(c(1,1,1), c(2,3,4))

grid.arrange(predic_rgb, rain_paita, rain_piura, rain_chulu, 
             layout_matrix = layout)


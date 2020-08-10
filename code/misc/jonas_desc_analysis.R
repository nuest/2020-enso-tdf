#**********************************************************
# JONAS STUFF - CAN BE DELETED AFTER TALKING TO ALEX-------
#**********************************************************

#************************************************
# CALCULATE MEAN NUMBER OF PLANTS PER PLOT ----
#************************************************
# mean number of plants per plot
dat11 = base::subset(d11, select = "pnr")
dat12 = base::subset(d12, select = "pnr")
dat16 = base::subset(d16, select = "pnr")
dat17 = base::subset(d17, select = "pnr")

# calculate sum of respective plots
dat11 = dat11 %>%
  summarise(no_rows = length(pnr))

dat12 = dat12 %>%
  summarise(no_rows = length(pnr))

dat16 = dat16 %>%
  summarise(no_rows = length(pnr))

dat17 = dat17 %>%
  summarise(no_rows = length(pnr))

# preparation for calculating the mean
plant_mean = cbind(dat11, dat12, dat16, dat17)
plant_mean = plant_mean[-c(3, 5, 7)]
colnames(plant_mean)[2] = "2011"
colnames(plant_mean)[3] = "2012"
colnames(plant_mean)[4] = "2016"
colnames(plant_mean)[5] = "2017"

# calculate the mean per plot
plant_mean$mean = round((plant_mean$`2011` + plant_mean$`2012` +
  plant_mean$`2016` + plant_mean$`2017`)/4, 0)


# define plot style
plot_syle = theme_bw() + theme(panel.border = element_rect(colour = "black", size=1.4),
                                axis.line = element_line(colour = "black", size = 1.4),
                                axis.text.x = element_text(colour = "black"),
                                axis.text.y = element_text(colour = "black"),
                                axis.title=element_text(size=14,face="bold"))

# setting the style of the plot (without grid!) -> alternative style
# plot_syle = theme_bw() + theme(panel.border = element_rect(colour = "black", size=1.4),
#                                 panel.grid.major = element_blank(),
#                                 panel.grid.minor = element_blank(),
#                                 axis.line = element_line(colour = "black", size = 1.4),
#                                 axis.text.x = element_text(colour = "black"),
#                                 axis.text.y = element_text(colour = "black"),
#                                 axis.title=element_text(size=14,face="bold"))



# contents of the plot
plot1 = ggplot(data=plant_mean, aes(x=pnr, y=plant_mean$`2011`, group=1)) +
  geom_line(colour = "cyan3", size = 1.2) + 
  #ylim(0,30)+ 
  scale_x_discrete(name = "Number of plot", breaks = c(5,10,15,20,25,30,35,40,45,50)) + 
  scale_y_continuous(name = "Mean number of plants", limits = c(0,35), breaks = c(0,5,10,15,20,25,30,35)) + 
  ggtitle("Year 2011") + 
  plot_syle

plot2 = ggplot(data=plant_mean, aes(x=pnr, y=plant_mean$`2012`, group=1)) +
  geom_line(colour = "cyan3", size = 1.2) + 
  #ylim(0,30)+
  scale_x_discrete(name = "Number of plot", breaks = c(5,10,15,20,25,30,35,40,45,50)) + 
  scale_y_continuous(name = "Mean number of plants", limits = c(0,35), breaks = c(0,5,10,15,20,25,30,35)) + 
  ggtitle("Year 2012") +
  plot_syle

plot3 = ggplot(data=plant_mean, aes(x=pnr, y=plant_mean$`2016`, group=1)) +
  geom_line(colour = "cyan3", size = 1.2) + 
  #ylim(0,30)+
  scale_x_discrete(name = "Number of plot", breaks = c(5,10,15,20,25,30,35,40,45,50)) + 
  scale_y_continuous(name = "Mean number of plants", limits = c(0,35), breaks = c(0,5,10,15,20,25,30,35)) + 
  ggtitle("Year 2016") +
  plot_syle

plot4 = ggplot(data=plant_mean, aes(x=pnr, y=plant_mean$`2017`, group=1)) +
  geom_line(colour = "cyan3", size = 1.2) + 
  #ylim(0,30)+
  scale_x_discrete(name = "Number of plot", breaks = c(5,10,15,20,25,30,35,40,45,50)) + 
  scale_y_continuous(name = "Mean number of plants", limits = c(0,35), breaks = c(0,5,10,15,20,25,30,35)) + 
  ggtitle("Year 2017") +
  plot_syle

plot5 = ggplot(data=plant_mean, aes(x=pnr, y=plant_mean$mean, group=1)) +
  geom_line(colour = "cyan3", size = 1.2) + 
  #ylim(0,30)+
  scale_x_discrete(name = "Number of plot", breaks = c(5,10,15,20,25,30,35,40,45,50)) + 
  scale_y_continuous(name = "Mean number of plants", limits = c(0,35), breaks = c(0,5,10,15,20,25,30,35)) + 
  ggtitle("Mean of years") +
  plot_syle


# arrange the plots
ggarrange(ggarrange(plot1, plot2, plot3, plot4, ncol = 4),
          plot5,
          nrow = 2) 





# delete all dataframes
keep(d11, d12, d16, d17, plot_syle, sure = TRUE)



#************************************************
# CALCULATE MEAN COVER PER PLOT ----
#************************************************
# convert to factor variable
d11$pnr = as.factor(d11$pnr)
d12$pnr = as.factor(d12$pnr)
d16$pnr = as.factor(d16$pnr)
d17$pnr = as.factor(d17$pnr)

# mean number of plants per plot
dat11 = base::subset(d11, select = c("pnr", "cover"))
dat12 = base::subset(d12, select = c("pnr", "cover"))
dat16 = base::subset(d16, select = c("pnr", "cover"))
dat17 = base::subset(d17, select = c("pnr", "cover"))


dat11 = dat11 %>%
  summarise(no_rows = sum(cover))

dat12 = dat12 %>%
  summarise(no_rows = sum(cover))

dat16 = dat16 %>%
  summarise(no_rows = sum(cover))

dat17 = dat17 %>%
  summarise(no_rows = sum(cover))


plant_mean = cbind(dat11, dat12, dat16, dat17)
plant_mean = plant_mean[-c(3, 5, 7)]
colnames(plant_mean)[2] = "2011"
colnames(plant_mean)[3] = "2012"
colnames(plant_mean)[4] = "2016"
colnames(plant_mean)[5] = "2017"


plant_mean$mean = round((plant_mean$`2011` + plant_mean$`2012` +
                            plant_mean$`2016` + plant_mean$`2017`)/4, 0)


plot1 = ggplot(data=plant_mean, aes(x=pnr, y=plant_mean$`2011`, group=1)) +
  geom_line(colour = "cyan3", size = 1.2) + 
  #ylim(0,30)+ 
  scale_x_discrete(name = "Number of plot", breaks = c(5,10,15,20,25,30,35,40,45,50)) + 
  scale_y_continuous(name = "Vegetation cover (%)", limits = c(0,150), breaks = c(0,15,30,45,60,75,90,105,120,135,150)) + 
  ggtitle("Year 2011") + 
  plot_syle

plot2 = ggplot(data=plant_mean, aes(x=pnr, y=plant_mean$`2012`, group=1)) +
  geom_line(colour = "cyan3", size = 1.2) + 
  #ylim(0,30)+
  scale_x_discrete(name = "Number of plot", breaks = c(5,10,15,20,25,30,35,40,45,50)) + 
  scale_y_continuous(name = "Vegetation cover (%)", limits = c(0,150), breaks = c(0,15,30,45,60,75,90,105,120,135,150)) +
  ggtitle("Year 2012") +
  plot_syle

plot3 = ggplot(data=plant_mean, aes(x=pnr, y=plant_mean$`2016`, group=1)) +
  geom_line(colour = "cyan3", size = 1.2) + 
  #ylim(0,30)+
  scale_x_discrete(name = "Number of plot", breaks = c(5,10,15,20,25,30,35,40,45,50)) + 
  scale_y_continuous(name = "Vegetation cover (%)", limits = c(0,150), breaks = c(0,15,30,45,60,75,90,105,120,135,150)) +
  ggtitle("Year 2016") +
  plot_syle

plot4 = ggplot(data=plant_mean, aes(x=pnr, y=plant_mean$`2017`, group=1)) +
  geom_line(colour = "cyan3", size = 1.2) + 
  #ylim(0,30)+
  scale_x_discrete(name = "Number of plot", breaks = c(5,10,15,20,25,30,35,40,45,50)) + 
  scale_y_continuous(name = "Vegetation cover (%)", limits = c(0,150), breaks = c(0,15,30,45,60,75,90,105,120,135,150)) +
  ggtitle("Year 2017") +
  plot_syle

plot5 = ggplot(data=plant_mean, aes(x=pnr, y=plant_mean$mean, group=1)) +
  geom_line(colour = "cyan3", size = 1.2) + 
  #ylim(0,30)+
  scale_x_discrete(name = "Number of plot", breaks = c(5,10,15,20,25,30,35,40,45,50)) + 
  scale_y_continuous(name = "Vegetation cover (%)", limits = c(0,150), breaks = c(0,15,30,45,60,75,90,105,120,135,150)) +
  ggtitle("Mean of years") +
  plot_syle


ggarrange(ggarrange(plot1, plot2, plot3, plot4, ncol = 4),
          plot5,
          nrow = 2)


keep(d11, d12, d16, d17, plot_syle, sure = TRUE)


#************************************************
# CALCULATE BETA DIVERSITY ----
#************************************************
# calculate beta-diversity
scores = readRDS("images/05_scores_dca.rda")

# partitioning the scores to the respective years
scores_2011 = scores[scores$year == 2011, ]
scores_2012 = scores[scores$year == 2012, ]
scores_2016 = scores[scores$year == 2016, ]
scores_2017 = scores[scores$year == 2017, ]

# with the existing dca
summary(scores_2011$DCA1)
summary(scores_2012$DCA1)
summary(scores_2016$DCA1)
summary(scores_2017$DCA1)

# test whether this ranges are changing for yearly dca´s
spec_data = readRDS("images/02_CTlondo.rda")

# delete plot number 25 due to inundation in 2017 (allready deleted in env_data)
spec_data = spec_data[-c(25, 75, 125, 175),]
spec_data[is.na(spec_data)] = 0

# create individual dataframes for DCA
input_2011 = spec_data[1:49,]
input_2012 = spec_data[50:98,]
input_2016 = spec_data[99:147,]
input_2017 = spec_data[148:196,]

# apply 4 dca´s
dca_2011 = input_2011 %>% select(-pnr) %>% decorana(iweigh = TRUE)
scores_2011_i = as.data.frame(scores(dca_2011))
scores_2011_i$year = "2011 (own DCA)"

dca_2012 = input_2012 %>% select(-pnr) %>% decorana(iweigh = TRUE)
scores_2012_i = as.data.frame(scores(dca_2012))
scores_2012_i$year = "2012 (own DCA)"

dca_2016 = input_2016 %>% select(-pnr) %>% decorana(iweigh = TRUE)
scores_2016_i = as.data.frame(scores(dca_2016))
scores_2016_i$year = "2016 (own DCA)"

dca_2017 = input_2017 %>% select(-pnr) %>% decorana(iweigh = TRUE)
scores_2017_i = as.data.frame(scores(dca_2017))
scores_2017_i$year = "2017 (own DCA)"

# show the summary (ranges)
summary(scores_2011_i$DCA1)
summary(scores_2012_i$DCA1)
summary(scores_2016_i$DCA1)
summary(scores_2017_i$DCA1)

boxplot = ggplot(data = scores_2011, aes(x = year, y=DCA1)) + 
  geom_boxplot() + 
  geom_boxplot(data = scores_2011_i, aes(x = year, y = DCA1)) + 
  geom_boxplot(data = scores_2012, aes(x = year, y = DCA1)) +
  geom_boxplot(data = scores_2012_i, aes(x = year, y = DCA1)) +
  geom_boxplot(data = scores_2016, aes(x = year, y = DCA1)) +
  geom_boxplot(data = scores_2016_i, aes(x = year, y = DCA1)) +
  geom_boxplot(data = scores_2017, aes(x = year, y = DCA1)) +
  geom_boxplot(data = scores_2017_i, aes(x = year, y = DCA1)) + 
  plot_syle

boxplot

keep(d11, d12, d16, d17, plot_syle, sure = TRUE)


#************************************************
# CALCULATE GAMMA DIVERSITY ----
#************************************************
d11$art = as.factor(d11$art)
d12$art = as.factor(d12$art)
d16$art = as.factor(d16$art)
d17$art = as.factor(d17$art)

# caclulate Gamma diversity
n = c(nlevels(d11$art), nlevels(d12$art), nlevels(d16$art), nlevels(d17$art))
y = c(2011, 2012, 2016, 2017)
gamma = data.frame(y, n)

# plot the gamma diversity
plot = ggplot(data=gamma, aes(x=y, y=n, group=1)) +
  geom_line(colour = "cyan3", size = 1.2) + 
  geom_point() +
  scale_x_continuous(name = "Year", breaks = c(2011, 2012, 2016, 2017)) + 
  scale_y_continuous(name = "Number of species", limits = c(0,100), breaks = c(0,10,20,30,40,50,60,70,80,90,100)) + 
  ggtitle("Gamma diversity") + 
  plot_syle
plot

keep(d11, d12, d16, d17, plot_syle, sure = TRUE)

#************************************************
# VEGETATION FORMS PER YEAR ----
#************************************************
d11$type = as.factor(d11$type)
d12$type = as.factor(d12$type)
d16$type = as.factor(d16$type)
d17$type = as.factor(d17$type)

d11_t = d11 %>% 
  group_by(type) %>%
  summarise(no_rows = length(type))
d11_t = d11_t[-c(4), ] 

d12_t = d12 %>% 
  group_by(type) %>%
  summarise(no_rows = length(type))
d12_t = d12_t[-c(4), ]

d16_t = d16 %>% 
  group_by(type) %>%
  summarise(no_rows = length(type))

d17_t = d17 %>% 
  group_by(type) %>%
  summarise(no_rows = length(type))
d17_t = d17_t[-c(4), ]

# bind the data in order to make a nice plot
types = cbind(d11_t, d12_t, d16_t, d17_t)
types = types[-c(3,5,7)]
colnames(types) = c("type", "2011", "2012", "2016", "2017")


# Grouped plot
plot1 = ggplot(types, aes(y=types$`2011`, x=types$type)) + 
  geom_bar(position="dodge", stat="identity", fill = "blue") + 
  scale_y_continuous(name = "Vegetation cover (%)", limits = c(0,550), breaks = c(0,50,100,150,200,250,300,350,400,450,500,550)) +
  xlab("Vegetation type") + 
  ggtitle("Year 2011") +
  plot_syle

plot2 = ggplot(types, aes(y=types$`2012`, x=types$type)) + 
  geom_bar(position="dodge", stat="identity", fill = "blue") + 
  scale_y_continuous(name = "Vegetation cover (%)", limits = c(0,550), breaks = c(0,50,100,150,200,250,300,350,400,450,500,550)) +
  xlab("Vegetation type") + 
  ggtitle("Year 2012") +
  plot_syle

plot3 = ggplot(types, aes(y=types$`2016`, x=types$type)) + 
  geom_bar(position="dodge", stat="identity", fill = "blue") + 
  scale_y_continuous(name = "Vegetation cover (%)", limits = c(0,550), breaks = c(0,50,100,150,200,250,300,350,400,450,500,550)) +
  xlab("Vegetation type") + 
  ggtitle("Year 2016") +
  plot_syle

plot4 = ggplot(types, aes(y=types$`2017`, x=types$type)) + 
  geom_bar(position="dodge", stat="identity", fill = "blue") + 
  scale_y_continuous(name = "Vegetation cover (%)", limits = c(0,550), breaks = c(0,50,100,150,200,250,300,350,400,450,500,550)) +
  xlab("Vegetation type") + 
  ggtitle("Year 2017") +
  plot_syle

ggarrange(plot1, plot2, plot3, plot4, ncol = 4)




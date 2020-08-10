# Filename: 11_consistency.R
#
# TO DO: test for consistency
#
# Author(s): Jonas Brock & Jannes Muenchow


#Packages and Data-------------------------------------------------------

setwd("C:/Users/Jonas Brock/Desktop/papers/ENSO")


library("RODBC")
library("Hmisc")
library("lattice")
library("dplyr")
library("reshape")
library("data.table")
library("RODBC")

path = "data/db/Transekt_0417.accdb"
con = odbcConnectAccess2007(path)
sqlTables(con)
db_odbc = sqlFetch(con, "qurArtDeckungPlot_Kreuztabelle", as.is = TRUE)
close(con)

# JONAS:
# load data for 2017
data17 <- fread("data/data2017.csv")
all.equal(data17, as.data.table(db_odbc))
setdiff(names(data17), names(db_odbc))  # Euph_hype
setdiff(names(db_odbc), names(data17))  # Euph_serp

# use another db saved in our folder structure
path = "_raw/data_2017/Transekt_0417.accdb"
con = odbcConnectAccess2007(path)
sqlTables(con)
db_odbc = sqlFetch(con, "qurArtDeckungPlot_Kreuztabelle", as.is = TRUE)
close(con)
all.equal(data17, as.data.table(db_odbc))
str(data17[, 1:3])
str(d_17[, 1:3])
# ok, this was the one used, clean up accordingly


# delete "<>" column
data17[, "<>" := NULL]

# JONAS:
#load data for 2016
data16 <- fread("data/data2016.csv")

path = "data/db/Transekt_0316.accdb"
con = odbcConnectAccess2007(path)
sqlTables(con)
db_odbc = sqlFetch(con, "tblArten", as.is = TRUE)
close(con)

all.equal(data16, as.data.table(db_odbc))
setdiff(names(data16), names(db_odbc))
setdiff(names(db_odbc), names(data16))
# ok, family was added to tblArten

path = "data/db/Transekt_0316.accdb"
con = odbcConnectAccess2007(path)
query = paste("SELECT tblArten.*, tblFamilie.Familie",
              "FROM tblFamilie", "INNER JOIN tblArten", 
              "ON tblFamilie.idFamilie = tblArten.fidFamilie")
db_odbc = sqlQuery(con, query = query, as.is = TRUE)
# or just qurSpecOrigin
# db_odbc = sqlFetch(con, "qurSpecOrigin", as.is = TRUE)
close(con)
# just keep the cols of data16
db_odbc = db_odbc[, names(data16)]
all.equal(data16, as.data.table(db_odbc))
setdiff(data16$Gattung, db_odbc$Gattung)  # ""
setdiff(db_odbc$Gattung, data16$Gattung)  # NA
data16$Gattung  # observation 49
db_odbc$Gattung  # observation 49
# ok, this was the table used -> change script to make it reproducible!

#Spezies Aufkommen per plot und dessen ?nderung-----------------------------------

#set all NaN equal 0
data16[is.na(data16)] <- 0
data17[is.na(data17)] <- 0



#bring the data in another format
data16$id <- 1:50
d <- melt(data16, id.vars = "id")
d <- d[d$value != 0, ]
d[order(d$id), ]
d <- subset(d, variable!="pnr")
summarize_16 <- group_by(d, id) %>% # number of species per plot
  summarise("alpha_16" = n())
colnames(d)<- c("id", "variable", "value_16")



data17$id <- 1:50
e <- reshape::melt(data17, id.vars = "id")
e <- e[e$value != 0, ]
e[order(e$id), ]
e <- subset(e, variable!="pnr")
summarize_17 <- group_by(e, id) %>% # number of species per plot
  summarise("alpha_17" = n())
colnames(e)<- c("id", "variable", "value_17")



#merge the data
diff_16_17 <- merge(summarize_16, summarize_17, by= "id", all.x = TRUE, all.y = TRUE)
diff_16_17$diff <- (diff_16_17$alpha_17 - diff_16_17$alpha_16)


plot(diff_16_17$diff, type = "h", xlab = "Plot", col.lab= "black", ylab = "Zu-/ Abnahme",
     col.lab="black", axes = FALSE )
axis(side = 1, at = seq(from = 1, to = 55, by = 1), labels = c(1:55))
axis(side = 2, at = seq(from = -15, to = 10, by = 1) )
title( main = "Zu-/ Abnahme der absoluten Pflanzenarten pro Plot (vgl. 2016 zu 2017)",
       col.main="black", font.main=20)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = 1, equilogs = TRUE)
abline(h = 0)
box()


#Deckungsvergleich zwischen den einzelnen Plots---------------------------------------------------------------------------

mergede <- merge(d,e, by = c("id", "variable"), all=TRUE)
mergede[is.na(mergede)] <- 0

#Summe der Deckung per plot im Jahr 2017
Plot_Sums17 <- as.data.frame(rowSums(data17))
colnames(Plot_Sums17) <- "Sums_17"
Plot_Sums17$id <- c(1:50)
Plot_Sums17$pnr <- c(1:50)
as.numeric(Plot_Sums17$id)
as.numeric(Plot_Sums17$pnr)
Plot_Sums17$Sums_17 <- Plot_Sums17$Sums_17 - Plot_Sums17$id - Plot_Sums17$pnr
Plot_Sums17$pnr <- NULL


#Summe der Deckung per plot im Jahr 2016
Plot_Sums16 <- as.data.frame(rowSums(data16))
colnames(Plot_Sums16) <- "Sums_16"
Plot_Sums16$id <- c(1:50)
Plot_Sums16$pnr <- c(1:50)
as.numeric(Plot_Sums16$id)
as.numeric(Plot_Sums16$pnr)
Plot_Sums16$Sums_16 <- Plot_Sums16$Sums_16 - Plot_Sums16$id - Plot_Sums16$pnr
Plot_Sums16$pnr <- NULL

Plot_Sums_all <- merge(Plot_Sums16,Plot_Sums17, by ="id")
Plot_Sums_all$diff <- (Plot_Sums_all$Sums_17 - Plot_Sums_all$Sums_16)



plot(Plot_Sums_all$diff, type = "h", xlab = "Plot", col.lab= "black", ylab = "Zu-/ Abnahme (%)",
     col.lab="black", axes = FALSE )
axis(side = 1, at = seq(from = 1, to = 55, by = 1), labels = c(1:55))
axis(side = 2, at = seq(from = -30, to = 100, by = 5) )
title( main = "Zu-/ Abnahme der absoluten Vegetationsbedeckung pro Plot (vgl. 2016 zu 2017)",
       col.main="black", font.main=20)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = 1, equilogs = TRUE)
abline(h = 0)
box()




#Frequency Vergleich der Arten zwischen den Jahren---------------------------------------------------------

#f?r das Jahr 2016
fre16 <- fread("data/data2016.csv")
fre16$pnr <- NULL
fre16[!is.na(fre16)] <- 1
fre16[is.na(fre16)] <- 0

barchart(colSums(fre16)  ,data = fre16, main = "Vorkommen der Pflanzen im Jahr 2016",
         xlab = "H?ufigkeit des Vorkommens", xlim= c(0: 50), cex.axis=1.5, cex.names=1.5)



#f?r das Jahr 2017
fre17 <- fread("data/data2017.csv")
fre17[,2] <- NULL
fre17$pnr <- NULL
fre17[!is.na(fre17)] <- 1
fre17[is.na(fre17)] <- 0

barchart(colSums(fre17), data = fre17, main = "Vorkommen der Pflanzen im Jahr 2017",
         xlab = "H?ufigkeit des Vorkommens", xlim= c(0: 50), cex.axis=1.5, cex.names=1.5)


#BVorkommensvergleich 2016 und 2017-------------------------------------------

# 2016
d[order(d$variable), ] # ordnet nach Variable
species16 <- group_by(d, variable) %>% # absolutes Vorkommen der Pflanzenart im gesamten Gebiet
  summarise("species16" = n())

# 2017
e[order(e$variable), ] # ordnet nach Variable
species17 <- group_by(e, variable) %>% # absolutes Vorkommen der Pflanzenart im gesamten Gebiet
  summarise("species17" = n())


#zeige Jannes, dass es Probleme mit den Abk?rzungen gibt
Spec_turn_abs <- merge(species16, species17, by = "variable", all = TRUE)



Spec_turn_abs[is.na(Spec_turn_abs)] <- 0
z <- Spec_turn_abs$species16
y <- Spec_turn_abs$species17
x <- cbind(z,y)
x <- as.data.frame(x)
colnames(x) <- c("16","17")
rownames(x) <- Spec_turn_abs$variable




#Arten die nur im Jahr 2017 vorkamen, aber nicht in 2016. Achtung bias durch falsche Namen!!!!
x_1 <- x[79:90,]


par(cex.axis=0.8)
barplot(t(x_1), beside = TRUE)
axis(side = 2, at = seq(from = 0, to = 25, by = 5) )

title( main = "Vorkommen der Pflanzen (vgl. 2016 zu 2017)",
       col.main="black", font.main=20)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = 2, equilogs = TRUE)
box()



#Vergleich f?r die ersten 20 Arten
x_20 <- x[1:20,]

par(cex.axis=0.8)
barplot(t(x_20), beside = TRUE, col = c("snow1", "snow4"))
axis(side = 2, at = seq(from = 0, to = 25, by = 5) )

title( main = "Vorkommen der Arten (vgl. 2016 zu 2017)",
       col.main="black", font.main=20)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = 2, equilogs = TRUE)

legend("topright",
       legend = c("2016", "2017"),
       fill = c("snow1", "snow4"))
box()



x_40 <- x[21:40,]

par(cex.axis=0.8)
barplot(t(x_40), beside = TRUE, col = c("snow1", "snow4"))
axis(side = 2, at = seq(from = 0, to = 25, by = 5) )

title( main = "Vorkommen der Arten (vgl. 2016 zu 2017)",
       col.main="black", font.main=20)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = 2, equilogs = TRUE)

legend("topright",
       legend = c("2016", "2017"),
       fill = c("snow1", "snow4"))
box()





x_60 <- x[41:60,]

par(cex.axis=0.8)
barplot(t(x_60), beside = TRUE, col = c("snow1", "snow4"))
axis(side = 2, at = seq(from = 0, to = 25, by = 5) )

title( main = "Vorkommen der Arten (vgl. 2016 zu 2017)",
       col.main="black", font.main=20)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = 2, equilogs = TRUE)

legend("topright",
       legend = c("2016", "2017"),
       fill = c("snow1", "snow4"))
box()



x_80 <- x[61:78,]

par(cex.axis=0.8)
barplot(t(x_80), beside = TRUE, col = c("snow1", "snow4"))
axis(side = 2, at = seq(from = 0, to = 25, by = 5) )

title( main = "Vorkommen der Arten (vgl. 2016 zu 2017)",
       col.main="black", font.main=20)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = 2, equilogs = TRUE)

legend("topright",
       legend = c("2016", "2017"),
       fill = c("snow1", "snow4"))
box()



# Plotweiser Vergleich

d <- melt(data16, id.vars = "id") # melt an object into a form suitable for easy casting
d <- d[d$value != 0, ]
plotwise_16 <-d[order(d$id), ] # ordnet nach id
e <- melt(data17, id.vars = "id") # melt an object into a form suitable for easy casting
e <- e[e$value != 0, ]
plotwise_17 <-  e[order(e$id), ] # ordnet nach id


i = 33
Ploti_16 <- plotwise_16[which(plotwise_16$id == i),]
Plot_16 <- as.data.frame(Ploti_16[,2:3])
rownames(Plot_16) <- Ploti_16$variable
Plot_16 <- t(Plot_16)

Ploti_17 <- plotwise_17[which(plotwise_17$id == i),]

Plot_17 <- as.data.frame(Ploti_17[,2:3])
rownames(Plot_17) <- Ploti_17$variable

ggplot(Ploti_16, aes(variable, value))+geom_histogram(stat = "identity")+ coord_flip()
ggplot(Ploti_17, aes(variable, value))+geom_histogram(stat = "identity")+ coord_flip()



















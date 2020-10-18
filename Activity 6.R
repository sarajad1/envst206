#instal.packages("sp","rgdal","dplyr")
library(sp)
library(rgdal)
library(dplyr)
install.packages("dplyr")
library(dplyr)

#read shapefiles, glaciers in 1966
g1966 <- readOGR("/users/sara/documents/ESdata/a06/GNPglaciers/GNPglaciers_1966.shp")

#glaciers in 2015
g2015 <- readOGR("/users/sara/documents/ESdata/a06/GNPglaciers/GNPglaciers_2015.shp")
str(g2015)

#map glaciers filling in polygons with light blue and grey borders
plot(g1966, col="lightblue2", border="grey50")
#data stores for each spatial object
head(g2015@data)
#projection info
g1966@proj4string

#check glacier names
g1966@data$GLACNAME
g2015@data$GLACNAME

#fix glacier names
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse( g2015@data$GLACNAME == "Miche Wabun",
                                      "Miche Wabun Glacier",
                                      as.character(g2015@data$GLACNAME)))
#combining area, working with smaller data frame
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)
gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

#all data tables joined by glacier name
gAll <- full_join(gdf66,gdf15, by="GLACNAME")

#calculating % change in area from 1966 to 2015
gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

#scatterplot of glacier area 1966 vs. % change in area
plot(gAll$area66,gAll$gdiff)

#spplot of glacier area 1966 vs. % change in area
#join data
g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")
spplot(g1966, "gdiff", main="% change in area", col="transparent")

#mean and standard deviation for % lost
mean(gAll$gdiff)
sd(gAll$gdiff)

#map of Boulder glacier 1966-2015
boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]
plot(boulder66, main="Boulder Glacier Size in 1966 & 2015", col="lightblue2")
plot(boulder15, col="royalblue2", add=TRUE)
legend("bottomleft", c("1966", "2015"), fill= c("lightblue2","royalblue2"), bty="n")

#map of Pumpelly glacier 1966-2015
Pumpelly66 <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
Pumpelly15 <- g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]
plot(Pumpelly66, main="Pumpelly Glacier Size in 1966 & 2015", col="lightblue2")
plot(Pumpelly15, col="royalblue2", add=TRUE)
legend("topleft", c("1966", "2015"), fill= c("lightblue2","royalblue2"), bty="n")


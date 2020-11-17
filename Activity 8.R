install.packages(c("raster"))
yes
library(raster)
library(ggplot2)
library(rgdal)

#set up directory
dirR <- "/Users/Sara/documents/ESData/a08/oneida"

#read in sentinel data
rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))

plot(rdatB2/10000)

#stack red green blue
rgbS <- stack(rdatB4,rdatB3,rdatB2)/10000

#view raster
plotRGB(rgbS, scale=2)

#add contrast stretch
plotRGB(rgbS, stretch="lin")

#changing resolution
plotRGB(rgbS, stretch="lin",maxpixels=rgbS@nrows*rgbS@ncols)

#total raster cells
rgbS@nrows*rgbS@ncols

#stack infrared green blue
ngbS <- stack(rdatB8,rdatB3,rdatB2)/10000

#plotting false color
plotRGB(ngbS, stretch="lin")

#calculate NDVI
NDVI <- (rdatB8 - rdatB4) / (rdatB8 + rdatB4)
#plot NDVI
plot(NDVI)


#read in landcover points data
algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)

#plot points and true color
plotRGB(rgbS, stretch="lin",maxpixels=2297430)
plot(algae, add=TRUE, col=rgb(0.5,0.5,0.5,0.5), pch=19)
plot(agri, add=TRUE, col=rgb(0.75,0.5,0.5,0.5), pch=19)
plot(forest, add=TRUE, col=rgb(0.75,0.75,0.25,0.5), pch=19)
plot(water, add=TRUE, col=rgb(0.33,0.75,0.75,0.5), pch=19)
plot(wetlands, add=TRUE, col=rgb(0.33,0.33,0.65,0.5), pch=19)
legend("bottomleft", c("algae","agri","forest","water","wetlands"),
       pch=19, col=c(rgb(0.5,0.5,0.5,0.5),rgb(0.75,0.5,0.5,0.5),rgb(0.75,0.75,0.25,0.5),rgb(0.33,0.75,0.75,0.5),rgb(0.33,0.33,0.65,0.5)),
       bty="n", cex=0.75)

#dataframe with all point coordinates
landExtract <- data.frame(landcID = as.factor(rep(c("algae","water","agri","forest","wetland"),each=120)),
                          x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],forest@coords[,1],wetlands@coords[,1]),
                          y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],forest@coords[,2],wetlands@coords[,2]))

#stack all bands
allbands <- stack(rdatB2, rdatB3, rdatB4, rdatB8)/10000
#adding the raster reflectance values to point coordinates/classes
#extract
ExtractOut <- raster::extract(allbands,landExtract[,2:3])
#rename
colnames(ExtractOut) <- c("B02","B03","B04","B08")
#combining original data w coordinates and raster data
rasterEx <- cbind(landExtract,ExtractOut)
head(rasterEx)

#scatterplot of bands
ggplot(data=rasterEx, aes(x=B02, y=B03, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()

#scatterplot b08 (infrared) vs. b02 (blue)
ggplot(data=rasterEx, aes(x=B08, y=B02, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()
#scatterplot b08 (infrared) vs. b03 (green)
ggplot(data=rasterEx, aes(x=B08, y=B03, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()
#scatterplot b08 (infrared) vs. b04 (red)
ggplot(data=rasterEx, aes(x=B08, y=B04, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()

#extract landcover points from NDVI raster
ExtractOutNDVI <- raster::extract(NDVI,landExtract[,2:3])
#combining original data with NDVI
NDVIraster <- cbind(landExtract,ExtractOutNDVI)
#rename column
names(NDVIraster)[names(NDVIraster) == "ExtractOutNDVI"] <- "NDVI"

#plotting NDVI
ggplot(data=NDVIraster[NDVIraster$landcID==c("agri","forest","wetland"),], aes(x=landcID, y=NDVI, fill=landcID))+
  geom_boxplot(width=.1)+
  geom_violin(alpha=.2)+
  theme_classic()

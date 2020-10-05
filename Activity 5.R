#read data
datW <- read.csv ("/users/sara/documents/ESData/noaa2011124.csv")
#name column as factor
datW$NAME <- as.factor(datW$NAME)
#set up as vector for all names for each level
nameS <- levels(datW$NAME)
nameS
#dataframe with just precipitation, year, sitename
#removed NA
datP <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           PRCP=datW$PRCP))
#total annual precip in mm
precip <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum", na.rm=TRUE)
precip <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE)
#rename columns
colnames(precip) <- c("NAME","year","totalP")
#add x column from aggregate and look at the length of observations in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x

#new dataframe of precip less than 364
pr <- precip[precip$ncount >=364, ]
#look at only livermore, CA and morrisvile, NY precip
ca <- pr [pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

#plot of california precip
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year",
     yaxt = "n")
#fix y axis
axis(2, seq(200,800, by=200), las=2)
#add new york
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")
#adjusting plot range
plot(ca$year, ca$totalP,
     type="b",
     pch= 19,
     ylab = "Annual precipitation (mm)",
     xlab= "Year",
     yaxt = "n",
     ylim=c(0, 1600))
axis(2, seq(0,1600, by=400), las=2 )
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")
#add legend
legend("topleft", #position
       c("California", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness
       bty="n")


#dataframe with just TMAX, year, site name
datT <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMAX=datW$TMAX))
#mean aggregate of maximum
TMAX <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="mean", na.rm=TRUE)
colnames(TMAX) <- c("NAME","year","TMAX")

#add x column from aggregate and look at the length of observations in each year
TMAX$ncount <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="length")$x
#new dataframe of TMAX less than 364
tm <- TMAX[TMAX$ncount >=364, ]

ndt <- tm[tm$NAME == nameS[3], ]
nyt <- tm[tm$NAME == nameS[5], ]
#plotting average annual temperature in Mandan and Morrisville
plot(ndt$year, ndt$TMAX,
     type="b",
     pch= 19,
     ylab = "Average Annual Temperature (C)",
     xlab= "Year",
     yaxt = "n",
     ylim = c(8,15))
axis(2, seq(8,15, by=2), las=2)
points(nyt$year, nyt$TMAX,
       type = "b",
       pch = 19,
       col="tomato3")
legend("bottomright", #position
       c("Mandan, ND", "Morrisville, NY"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness
       bty="n") 


#installing packages
install.packages("ggplot2")
library(ggplot2)

#gg2plot for annual precip
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path ()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation")+
  theme_classic()+
  scale_color_manual(values = c("orchid2", "seagreen3", "sienna1","royalblue2","gold2"))

ggplot(data = datW, aes(x=NAME, y=TMIN))+
  geom_violin(fill=rgb(0.933,0.953,0.98))+
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+
  theme_classic()

#looking at Mormon Flat, AZ 1974
sub <- datW[datW$NAME == nameS[4] & datW$ year == 1974, ]
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

#ggplot with date
ggplot (data=sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximum temperature (C)")

#ggplot barplot
ggplot(data=sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

#looking at Aberdeen, WA 1974
subx <- datW[datW$NAME == nameS[1] & datW$ year == 1974, ]
subx$DATE <- as.Date(subx$DATE,"%Y-%m-%d")

#Aberdeen 1974 max temp
#ggplot with max temp
ggplot (data=subx, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximum temperature (C)")
#ggplot with prcp
ggplot(data=subx, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

#looking at min temp in Livermore, CA

#adjusting dataframe with just TMIN, year, site name
datC <- na.omit(data.frame(NAME= datW$NAME,
                           year=datW$year,
                           TMIN=datW$TMIN))
TMIN <- aggregate(datC$TMIN, by=list(datC$NAME,datC$year), FUN="mean", na.rm=TRUE)
colnames(TMIN) <- c("NAME","year","TMIN")
TMIN$ncount <- aggregate(datC$TMIN, by=list(datC$NAME,datC$year), FUN="length")$x
tmi <- TMIN[TMIN$ncount >=364, ]
catm <- tmi[tmi$NAME == nameS[2] & tmi$year >=2000,]

#plot of california precip
plot(catm$year, catm$TMIN,
     type = "b",
     pch = 19,
     ylab = "Minimum Temperature (C)",
     xlab = "Year",
     yaxt = "n")
axis(2, seq(8,10, by=0.5), las=2)


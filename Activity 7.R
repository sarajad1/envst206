#read Florida CO2 pollutant data
CoPOL <- read.csv("/users/sara/downloads/ad_viz_plotval_data.csv")

#set up site names as factor
CoPOL$Site.Name <- as.factor(CoPOL$Site.Name)

#mean of Air quality index across all sites
AQImean <- aggregate(CoPOL$DAILY_AQI_VALUE, by=list(CoPOL$Site.Name),FUN="mean",na.rm=TRUE)

#change column names
colnames(AQImean) <- c("NAME","Average AQI")
AQImean

#standard deviation of air quality index across all sites
AQIsd <- aggregate(CoPOL$DAILY_AQI_VALUE, by=list(CoPOL$Site.Name),FUN="sd",na.rm=TRUE)

#change column names
colnames(AQIsd) <- c("NAME","Standard Deviation")

plot(AQImean,
     type = "b",
     pch = 19,
     ylab = "Average AQI",
     xlab = "Site Name")



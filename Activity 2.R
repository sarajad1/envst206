#activity 2

heights <- c(3,2,3)


datW #data frame

datW <- read.csv("/users/sara/documents/ESData/noaa2011124.csv")

#converting precip to cm
datW$PRCP_cm <- datW$PRCP/10 

#mean of daily precip
mean(datW$PRCP_cm, na.rm=TRUE)
